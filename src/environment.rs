use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use data::*;

/// An environment handle which allows mutable access
/// 
/// While this exists, the environment cannot be accessed.
pub struct ExclusiveEnvironment<'a> {
    map: &'a mut HashMap<String, Value>
}

impl<'a> ExclusiveEnvironment<'a> {
    /// Add a mapping to the environment
    pub fn set<S: AsRef<str>>(&mut self, key: S, val: Value) {
        self.map.insert(String::from(key.as_ref()), val);
    }

    /// Remove a mapping from the environment
    /// 
    /// If it's present, return its previous value.
    pub fn unset<S: AsRef<str>>(&mut self, key: S) -> Option<Value> {
        self.map.remove(key.as_ref())
    }
}

#[derive(Clone)]
/// Implements a partially mutable environment which can be copied and accessed
/// from multiple threads. This is intended for use to implement local bindings;
/// global names should use the `GlobalEnvironment` structure.
/// 
/// This type implements a *partially* mutable environment, which means that its
/// entries can be mutated, but the actual values inside them are immutable. By
/// preventing value mutation, the actual value structures can be shared across
/// multiple threads.
/// 
/// Environments can be cloned, which is useful in function closures or
/// subshells. Their copies allow them to reference the enclosing environment
/// without wasting memory on redundant copies of the environment.
pub struct Environment {
    mappings: Arc<HashMap<String, Value>>
}

impl Environment {
    /// Generate a new empty environment
    pub fn new() -> Self {
        Environment { mappings: Arc::new(HashMap::new()) }
    }

    /// Get a temporary exclusive handle that can be mutated in place.
    pub fn exclusive(&mut self) -> ExclusiveEnvironment {
        let r = Arc::make_mut(&mut self.mappings);
        ExclusiveEnvironment { map: r }
    }

    /// Get a value from the environment
    /// 
    /// If not present, attempt to retrieve it from the global environment
    /// instead
    pub fn get<K: AsRef<str>>(&self, key: K) -> Option<Value> {
        self.mappings.get(key.as_ref())
            .map(|x| x.clone())
            .or_else(|| global().get(key))
    }
}

/// A trait for things which can be inserted as value proxies
///
/// Binding proxies allow code to intercept user attempts to access or overwrite
/// the value of a given binding. This is useful for components like history or
/// environment variables.
pub trait BindingProxy {
    /// Get the proxy's current value
    fn get(&self) -> Value;

    /// Called when the user attempts to bind a new value
    fn set(&self, val: Value);

    /// Called when the user attempts to unbind a value
    /// 
    /// By default, always fails.
    fn unset(&self) -> Option<Value> { None }
}

#[derive(Clone)]
enum GlobalMappingValue {
    Literal(Value),
    Proxy(Arc<BindingProxy + Send + Sync>)
}

struct GlobalMapping {
    value: GlobalMappingValue,
    mutable: bool
}

/// Implements the global environment where function definitions and other
/// global bindings are stored. This is shared across threads and there's only
/// one copy per process.
pub struct GlobalEnvironment {
    mappings: RwLock<HashMap<String, GlobalMapping>>
}

lazy_static! {
    static ref ENV: GlobalEnvironment = GlobalEnvironment::new();
    static ref EMPTY: Environment = Environment::new();
}

impl GlobalEnvironment {
    fn new() -> Self {
        GlobalEnvironment { mappings: RwLock::new(HashMap::new()) }
    }

    pub fn listing(&self) -> Vec<(String, Value)> {
        let l = self.mappings.read().unwrap();
        (&*l).into_iter()
         .filter_map(|(k,v)|
                     if let GlobalMappingValue::Literal(ref v) = v.value {
                         Some((k.to_owned(), v.to_owned()))
                     } else {
                         None
                     })
         .collect::<Vec<_>>()
    }

    pub fn get<K: AsRef<str>>(&self, key: K) -> Option<Value> {
        let r = self.mappings.read().unwrap();
        r.get(key.as_ref())
         .map(|r| r.value.clone())
         .map(|v| match v {
             GlobalMappingValue::Literal(v) => v,
             GlobalMappingValue::Proxy(ref p) => p.get()
         })
    }

    fn set_key<K: AsRef<str>>(&self,
                              key: K,
                              val: GlobalMappingValue,
                              mutable: bool) {
        use std::collections::hash_map::Entry;

        let mut w = self.mappings.write().unwrap();
        let entry = w.entry(key.as_ref().to_owned());

        match entry {
            Entry::Occupied(mut e) => {
                let existing_val = e.get_mut();

                // use a proxy if present
                if let GlobalMappingValue::Proxy(ref a) = existing_val.value {
                    // setting proxy to proxy is invalid
                    if let GlobalMappingValue::Literal(v) = val { a.set(v); }
                    else { panic!("cannot write proxy var into proxy"); }
                } else if existing_val.mutable {
                    existing_val.value = val;
                    existing_val.mutable = mutable;
                }
            },
            Entry::Vacant(v) => {
                v.insert(GlobalMapping { value: val, mutable });
            }
        }
    }

    /// Remove a mapping from the environment
    /// 
    /// If it's present, return its previous value.
    pub fn unset<S: AsRef<str>>(&self, key: S) -> Option<Value> {
        let mut w = self.mappings.write().unwrap();
        // make sure the content is mutable before removing it, otherwise
        // (undef undef) would break the interpreter
        if let Some(entry) = w.get(key.as_ref()) {
            if !entry.mutable { return None; }

            // use proxy if present
            if let GlobalMappingValue::Proxy(ref p) = entry.value {
                return p.unset();
            }
        }

        w.remove(key.as_ref()).and_then(|m| match m.value {
            GlobalMappingValue::Literal(v) => Some(v),
            GlobalMappingValue::Proxy(_)   => None,
        })
    }

    pub fn set<K: AsRef<str>>(&self, key: K, val: Value) {
        let id = Identifier::new(key.as_ref());
        self.set_key(key, GlobalMappingValue::Literal(val.rename(id)), true);
    }

    pub fn set_immut<K: AsRef<str>>(&self, key: K, val: Value) {
        let id = Identifier::new(key.as_ref());
        self.set_key(key, GlobalMappingValue::Literal(val.rename(id)), false);
    }

    pub fn set_proxy<K, P>(&self, key: K, prox: P)
            where K: AsRef<str>, P: BindingProxy+Send+Sync+'static {
        self.set_key(key, GlobalMappingValue::Proxy(Arc::new(prox)), false);
    }
}

/// Get a reference to the process-wide global environment
pub fn global() -> &'static GlobalEnvironment { &ENV }

/// Get an empty environment
pub fn empty() -> Environment { EMPTY.clone() }

/// Query the global environment for an object and execute it with the given
/// args and an empty lexical environment
pub fn run_fn<S: AsRef<str>>(key: S, args: &[Value]) -> Option<Eval<Value>> {
    ENV.get(key)
       .map(|v| v.execute(&empty(), args))
}
