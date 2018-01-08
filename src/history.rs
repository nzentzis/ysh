use std::sync::{Mutex, Arc, Weak};
use std::time::SystemTime;

use data::*;
use environment::{global, BindingProxy};

pub struct HistoryProxy {
}

impl BindingProxy for HistoryProxy {
    fn get(&self) -> Value {
        let values = DB.snapshot()
            .into_iter()
            .map(|entry| {
                Value::map(vec![
                    (Value::atom_hash("cmd"), entry.command.structure.into_obj()),
                ])
            });
        Value::list(values)
    }

    fn set(&self, val: Value) {
        unimplemented!()
    }
}

pub enum OrderingKey {
    /// Order by date of first use
    FirstUsed,

    /// Order by date of most recent use
    LastUsed,

    /// Order by the score of each match
    MatchScore,

    /// Order by the length of each match's string representation
    Length,

    /// Order by the structural complexity of each match
    Complexity
}

/// A search query for the history database
pub struct Query<T> {
    query: T,
    order_by: OrderingKey,
    ascending: bool
}

impl<T> Query<T> {
    /// Generate a new query from the given value
    pub fn new(q: T) -> Self {
        Query {
            query: q,
            order_by: OrderingKey::MatchScore,
            ascending: false
        }
    }

    /// Adjust the key used to order the query results
    pub fn order_by(&mut self, k: OrderingKey) -> &mut Self {
        self.order_by = k;
        self
    }

    /// Sort the results in ascending order
    pub fn ascending(&mut self) -> &mut Self {
        self.ascending = true;
        self
    }

    /// Sort the results in descending order
    pub fn descending(&mut self) -> &mut Self {
        self.ascending = false;
        self
    }
}

/// A command which may be used by more than one entry in the history database
///
/// For example, if you run `ls` twice, the database would contain two entries,
/// one for each, but only one *command*, since both entries reference same
/// actual input.
pub struct Command {
    pub structure: Pipeline,
}

impl Command {
    /// Perform text matching
    ///
    /// Return `None` if the command doesn't match or `Some(score)` if it does.
    fn match_text(&self, t: &str) -> Option<u32> {
        None
    }

    /// Perform structural matching
    ///
    /// Return `None` if the command doesn't match or `Some(score)` if it does.
    fn match_obj(&self, v: &Value) -> Option<u32> {
        None
    }
}

pub struct Entry {
    pub command: Arc<Command>,
    pub when: SystemTime,
}

/// The history database -- stores command history and allows querying it
pub struct Database {
    values: Mutex<Vec<Arc<Entry>>>,
    commands: Mutex<Vec<Weak<Command>>>
}

fn value_complexity(v: &Value) -> u32 {
    match v.data {
        ValueData::List(ref xs) => 1+xs.iter().map(|v| value_complexity(&v))
                                       .max().unwrap_or(0),
        ValueData::Map(ref m) => 1+m.values().map(|v| value_complexity(&v))
                                    .max().unwrap_or(0),
        _ => 1
    }
}

/// Count the depth and complexity of a pipeline structure
fn complexity_metric(v: &Pipeline) -> usize {
    (v.elements.len() * v.elements.iter()
                         .map(|e| e.xform.0.iter()
                                   .map(|v| value_complexity(&v))
                                   .sum::<u32>() as usize)
                         .sum::<usize>()) +
      2*v.terminals.len()
}

impl Database {
    pub fn new() -> Self {
        Database {
            values: Mutex::new(Vec::new()),
            commands: Mutex::new(Vec::new())
        }
    }

    /// Remove commands which have no active users
    fn clean_cmds(&self) {
        let mut cmds = self.commands.lock().unwrap();
        cmds.retain(|c| c.upgrade().is_some());
    }

    /// Sort query results
    fn order_query_results<T>(&self, q: &Query<T>,
                              mut commands: Vec<(u32, Arc<Command>)>)
            -> Vec<Arc<Command>> {
        let vals = self.values.lock().unwrap();

        // figure out how to sort the entries
        match q.order_by {
            OrderingKey::MatchScore => {
                commands.sort_unstable_by_key(|&(s,_)| s);
                commands.into_iter().map(|t| t.1).collect()
            },
            OrderingKey::FirstUsed => {
                let cmd_ptrs = commands.into_iter().map(|i| i.1).collect::<Vec<_>>();
                let mut entries = vals.iter()
                                  .filter(|v| cmd_ptrs.iter()
                                                      .any(|x| Arc::ptr_eq(
                                                              x,&v.command)))
                                  .collect::<Vec<_>>();
                entries.sort_by_key(|e| e.when);
                entries.into_iter().map(|x| x.command.to_owned()).collect::<Vec<_>>()
            },
            OrderingKey::LastUsed => {
                let cmd_ptrs = commands.into_iter().map(|i| i.1).collect::<Vec<_>>();
                let mut entries = vals.iter()
                                  .filter(|v| cmd_ptrs.iter()
                                                      .any(|x| Arc::ptr_eq(
                                                              x,&v.command)))
                                  .collect::<Vec<_>>();
                entries.sort_by_key(|e| e.when);
                entries.reverse();
                entries.into_iter().map(|x| x.command.to_owned()).collect::<Vec<_>>()
            },
            OrderingKey::Complexity => {
                commands.sort_unstable_by_key(
                    |&(_,ref c)| complexity_metric(&c.structure));
                commands.into_iter().map(|t| t.1).collect()
            },
            OrderingKey::Length => {
                unimplemented!()
            },
        }
    }

    /// Run a textual query over the representations of all history entries
    ///
    /// This takes the string *representations* as generated by
    /// `Value::into_str` of each history entry and search through them for the
    /// passed query pattern.
    pub fn query_text<S: AsRef<str>>(&self, query: Query<S>) -> Vec<Arc<Command>> {
        // match commands
        let commands = {
            let q = query.query.as_ref();
            let cmds = self.commands.lock().unwrap();
            cmds.iter()
                .filter_map(|x| x.upgrade()
                                 .and_then(|c| c.match_text(&q)
                                                .map(|s| (s,c))))
                .collect::<Vec<_>>()
        };

        self.order_query_results(&query, commands)
    }

    /// Run a structural query over all history entries
    ///
    /// This takes the structure of the passed value and searches the database
    /// for entries which embed the given structure. One structure embeds
    /// another if they're equal or if they're both lists and the embedding
    /// structure is a prefix of the embedded one.
    ///
    /// For this function, equality tests compare the objects using the standard
    /// equality test, with one exception: the symbol "_" matches any value.
    /// This allows for wild-card matching, where `( 1 2)` matches both
    /// `(+ 1 2)` and `(- 1 2)`
    pub fn query_structural(&self, query: Query<Value>) -> Vec<Arc<Command>> {
        // match commands
        let commands = {
            let q = &query.query;
            let cmds = self.commands.lock().unwrap();
            cmds.iter()
                .filter_map(|x| x.upgrade()
                                 .and_then(|c| c.match_obj(&q)
                                                .map(|s| (s,c))))
                .collect::<Vec<_>>()
        };

        self.order_query_results(&query, commands)
    }

    /// Get the length of the history in entries
    pub fn len(&self) -> usize {
        self.values.lock().unwrap().len()
    }

    /// Get the value of the Nth item back in history
    pub fn get(&self, i: usize) -> Pipeline {
        let v = self.values.lock().unwrap();
        v[(v.len()-1)-i].command.structure.to_owned()
    }

    /// Get a snapshot of all historical entries
    pub fn snapshot(&self) -> Vec<Arc<Entry>> {
        let v = self.values.lock().unwrap();
        v.to_owned()
    }

    /// Add a value to the history database
    ///
    /// The new entry will occur at the present time.
    pub fn record(&self, val: &Pipeline) {
        // find a command to reuse if possible
        let cmd = {
            let mut cmd = None;
            let mut l = self.commands.lock().unwrap();
            for c in l.iter().filter_map(|x| x.upgrade()) {
                if &c.structure == val {
                    cmd = Some(c);
                    break;
                }
            }

            cmd.unwrap_or_else(|| {
                let a = Arc::new(Command { structure: val.to_owned() });
                l.push(Arc::downgrade(&a));
                a
            })
        };

        // construct an entry
        let entry = Arc::new(Entry {
            command: cmd,
            when: SystemTime::now()
        });

        let mut l = self.values.lock().unwrap();
        l.push(entry);
    }
}

lazy_static! {
    static ref DB: Database = Database::new();
}

/// Get a reference to the history database
pub fn db() -> &'static Database {
    &DB
}
