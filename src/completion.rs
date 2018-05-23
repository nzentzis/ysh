use std::collections::HashSet;
use std::sync::{Arc, Weak};
use std::iter::FromIterator;
use std::path::Path;

use environment;
use environment::Environment;
use data::*;
use numeric::Number;
use evaluate::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum EntryType {
    SystemCommand,
    FunctionBinding,
    VariableBinding,
    OtherForm,
    File,
}

impl ValueConvert for EntryType {
    fn into_obj(&self) -> Value {
        match self {
            &EntryType::SystemCommand   => Value::atom("command"),
            &EntryType::FunctionBinding => Value::atom("function"),
            &EntryType::VariableBinding => Value::atom("variable"),
            &EntryType::OtherForm       => Value::atom("other"),
            &EntryType::File            => Value::atom("file"),
        }
    }

    fn from_obj(val: &Value) -> Result<Self, EvalError> {
        let atom = match val.data {
            ValueData::Atom(ref a) => a,
            _ => return Err(EvalError::TypeError(String::from(
                        "cannot convert object to completion entry type")))
        };

        match atom.as_str() {
            "command"   => Ok(EntryType::SystemCommand),
            "function"  => Ok(EntryType::FunctionBinding),
            "variable"  => Ok(EntryType::VariableBinding),
            "other"     => Ok(EntryType::OtherForm),
            "file"      => Ok(EntryType::File),
            _           => Err(EvalError::TypeError(String::from(
                        "unknown entry type")))
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Entry {
    /// The type of this entry
    pub what: EntryType,

    /// The text to insert if chosen
    pub text: String,

    /// A short line of text describing the entry
    pub docs: Option<String>,

    /// A weighting factor used when ordering results
    ///
    /// Results are (by default) ordered by weight, then by the presence of
    /// docs, then text.
    pub weight: i32
}

impl ValueConvert for Entry {
    fn into_obj(&self) -> Value {
        Value::atom_map(vec![("type", self.what.into_obj()),
                             ("text", Value::str(self.text.as_str())),
                             ("docs", self.docs.as_ref().map(|x| Value::str(x.as_ref()))
                                          .unwrap_or(Value::empty())),
                             ("weight", Value::from(Number::int(self.weight as i64)))])
    }

    fn from_obj(v: &Value) -> EvalRes<Self> {
        if let ValueData::Map(ref h) = v.data {
            let typ = h.get(&Value::atom("type").hash().wait().unwrap().unwrap())
                .ok_or(EvalError::TypeError(format!(
                            "missing required key :type")))?;
            let text = h.get(&Value::atom("text").hash().wait().unwrap().unwrap())
                .ok_or(EvalError::TypeError(format!(
                            "missing required key :text")))?;
            let docs = h.get(&Value::atom("docs").hash().wait().unwrap().unwrap())
                .ok_or(EvalError::TypeError(format!(
                            "missing required key :docs")))?;
            let weight = h.get(&Value::atom("weight").hash().wait().unwrap().unwrap())
                .ok_or(EvalError::TypeError(format!(
                            "missing required key :weight")))?;
            
            let typ = EntryType::from_obj(typ)?;
            let text = text.into_str().wait()?;
            let docs = docs.first().wait()?;
            let weight = weight.into_num().wait()?
                .ok_or(EvalError::TypeError(String::from(
                            "weight must be numeric")))?.round();

            let docs = if let Some(d) = docs {Some(d.into_str().wait()?)}
                       else {None};

            Ok(Entry {
                what: typ,
                text,
                docs,
                weight: weight as i32
            })
        } else {
            Err(EvalError::TypeError(format!(
                        "cannot read non-map value as entry")))
        }
    }
}

fn file_completer(seed: &str) -> EvalRes<Vec<Entry>> {
    use std::os::unix::prelude::PermissionsExt;

    let paths = if let Some(p) = environment::global().get("path") {p}
                else {return Ok(Vec::new())};
    let paths = paths.into_seq().wait()?
                     .into_iter()
                     .map(|x| x.into_str().wait())
                     .collect::<EvalRes<Vec<String>>>()?;
    
    // TODO: handle resolution by path, e.g. "/usr/bin/abc<TAB>"
    //       this might be handled by the file completer.
    let mut res = Vec::new();
    for p in paths {
        let entries = if let Ok(r) = Path::new(&p).read_dir() {r}
                      else {continue;};
        for entry in entries {
            let e = entry?;
            let meta = e.metadata()?;

            if meta.is_file() && (meta.permissions().mode() & 0o111 != 0) {
                if let Ok(s) = e.file_name().into_string() {
                    if s.starts_with(seed) {
                        res.push(Entry {
                            what: EntryType::SystemCommand,
                            text: s,
                            docs: None, // TODO: check manpages for docs
                            weight: 1
                        });
                    }
                }
            }
        }
    }

    Ok(res)
}

/// Default completion generator
///
/// This is bound to the `shell/generate-completions` symbol by default, and is
/// used to generate completions for the default editing discipline.
fn default_completer(seed: &str, entries: HashSet<EntryType>) -> Vec<Entry> {
    let mut results = Vec::new();
    let mut used_names = HashSet::new();

    // run completion on bindings
    if entries.contains(&EntryType::FunctionBinding) ||
        entries.contains(&EntryType::VariableBinding) {
        for (name, value) in environment::global().listing() {
            if !name.starts_with(seed) {continue;}
            if used_names.contains(&name) {continue;}
            
            let is_func = value.is_executable();
            if (entries.contains(&EntryType::FunctionBinding) == is_func) ||
                (entries.contains(&EntryType::VariableBinding) == !is_func) {
                used_names.insert(name.clone());
                results.push(Entry {
                    what: if is_func {EntryType::FunctionBinding}
                          else {EntryType::VariableBinding},
                    text: name,
                    docs: value.doc.and_then(|x| x.short_desc
                                                .as_ref()
                                                .map(|d| d.as_ref().to_owned())),
                    weight: 1
                });
            }
        }
    }

    // TODO: handle files

    // look through $PATH and list them
    if entries.contains(&EntryType::SystemCommand) {
        for e in file_completer(seed).unwrap_or_else(|_| Vec::new()) {
            // ignore shadowed names
            if used_names.contains(&e.text) {continue;}

            used_names.insert(e.text.clone());
            results.push(e);
        }
    }

    // sort the output
    results.sort_unstable_by_key(|e| (e.weight, e.docs.is_some(), e.text.clone()));
    results
}

/// Wraps the default completer in a Lisp-compatible form
fn default_complete_wrapper(_: &Environment, args: &[Value]) -> EvalRes<Value> {
    if args.len() != 2 {
        Err(EvalError::Arity {got: args.len(), expected: 2})
    } else {
        let seed = args[0].into_str().wait()?;
        let types = args[1].into_seq().wait()?
                           .into_iter()
                           .map(|x| EntryType::from_obj(&x))
                           .collect::<EvalRes<Vec<_>>>()?;

        let res = default_completer(&seed, HashSet::from_iter(types))
                 .into_iter()
                 .map(|x| x.into_obj());
        Ok(Value::list(res))
    }
}

/// Look up the active completion function in `shell/generate-completions` and
/// run it to build a completion list.
fn run_completer(seed: &str, entries: HashSet<EntryType>) -> Vec<Entry> {
    let results = environment::run_fn(&"shell/generate-completions",
                                      &[Value::str(seed), Value::list(
                                          entries.iter()
                                                 .map(|x| x.into_obj()))])
                 .map(|x| x.wait());

    if let Some(Ok(r)) = results {
        let l = r.into_seq()
                 .wait()
                 .and_then(|s| s.into_iter()
                                .map(|x| Entry::from_obj(&x))
                                .collect::<Result<Vec<_>, _>>());
        if let Ok(r) = l { return r; }
    }

    Vec::new()
}

#[derive(Clone)]
/// A result set from the completion engine
///
/// Allows caching the results of one call to the completion function and
/// reusing them for further queries.
pub struct CompletionSet {
    entries: Vec<Arc<Entry>>,
    seed: String,
    wanted: HashSet<EntryType>,
    marked: Weak<Entry>
}

impl CompletionSet {
    /// Infer appropriate completion settings from current parse result and call
    /// `complete` with them.
    pub fn complete_smart(seed: &str,
                          partial: &::reader::ParseOutput<Pipeline>) -> Self {
        use reader::{ParseOutput, ParseContext};

        // TODO: use partial tree state to figure out whether they're typing a
        //       command or shell form, and use that to enable/disable filename
        //       completion

        // TODO: implementing seeding properly here requires the reader to give
        //       back a partial parse tree to work from

        let s = match partial.context {
            ParseContext::Symbol(_) => {
                vec![EntryType::FunctionBinding,
                     EntryType::VariableBinding,
                     EntryType::SystemCommand,
                     EntryType::File]
            },
            ParseContext::Value => {
                vec![EntryType::FunctionBinding,
                     EntryType::VariableBinding,
                     EntryType::SystemCommand,
                     EntryType::File]
            },
            ParseContext::File => {
                vec![EntryType::VariableBinding, EntryType::File]
            },
            _ => {
                vec![EntryType::FunctionBinding,
                     EntryType::VariableBinding,
                     EntryType::SystemCommand,
                     EntryType::File,
                     EntryType::OtherForm]
            },
        };
        CompletionSet::complete(&seed, HashSet::from_iter(s))
    }

    /// Utility function for calling `complete` with every entry type allowed
    pub fn complete_any(seed: &str) -> Self {
        CompletionSet::complete(seed, HashSet::from_iter(
                vec![EntryType::File,
                     EntryType::FunctionBinding,
                     EntryType::SystemCommand,
                     EntryType::VariableBinding,
                     EntryType::OtherForm]))
    }

    /// Generate completions for the provided seed and goal set
    ///
    /// The `seed` parameter specifies the seed text to use when generating
    /// completions, and the `wanted` set lists the eligible entry types to
    /// return in the current context.
    pub fn complete(seed: &str, wanted: HashSet<EntryType>) -> Self {
        let res = run_completer(seed, wanted.clone());
        CompletionSet {
            entries: res.into_iter().map(Arc::new).collect(),
            seed: seed.to_owned(),
            wanted,
            marked: Weak::new()
        }
    }

    /// Update the result set using more seed characters
    ///
    /// This will reuse the cached result set if the original seed is a prefix
    /// of the new one.
    pub fn update(&mut self, seed: &str) {
        if seed.starts_with(&self.seed) {
            // refinement
            self.entries.retain(|e| e.text.starts_with(seed));
            self.seed = seed.to_owned();
        } else {
            // update
            // TODO: retain marks if they're still valid
            let res = run_completer(seed, self.wanted.clone());
            self.entries = res.into_iter().map(Arc::new).collect();
            self.seed = seed.to_owned();
            self.marked = Weak::new();
        }
    }

    /// Mark the item at a given index
    ///
    /// The marked item is preserved across completion set updates as long as
    /// it is still included in the updated set.
    ///
    /// # Panics
    ///
    /// This function will panic if the given index is not within the completion
    /// set.
    pub fn mark_at(&mut self, idx: usize) {
        self.marked = Arc::downgrade(&self.entries[idx]);
    }

    /// Get the index of the marked item
    ///
    /// If no item is marked, return `None`.
    pub fn marked_idx(&self) -> Option<usize> {
        self.marked.upgrade()
            .and_then(|ptr| self.entries.iter()
                                        .position(|e| Arc::ptr_eq(e, &ptr)))
    }

    /// Select the next item in the list, or the first if none is selected
    pub fn mark_next(&mut self) {
        if let Some(i) = self.marked_idx() {
            if i < self.entries.len()-1 {
                self.mark_at(i+1);
            }
        } else {
            self.mark_at(0);
        }
    }

    /// Select the previous item in the list, or the last if none is selected
    pub fn mark_prev(&mut self) {
        if let Some(i) = self.marked_idx() {
            if i > 0 {
                self.mark_at(i-1);
            }
        } else {
            let l = self.entries.len()-1;
            self.mark_at(l);
        }
    }

    /// Get the marked item if possible
    pub fn marked(&self) -> Option<Arc<Entry>> {
        self.marked.upgrade()
    }

    /// Get the number of entries
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Access the list of entries
    pub fn entries(&self) -> &[Arc<Entry>] {
        self.entries.as_slice()
    }

    /// Convert into list of entries
    pub fn into_entries(self) -> Vec<Arc<Entry>> {self.entries}
}

lazy_static! {
    // TODO: add long-form docs here when the support is ready
    static ref DOC_GEN_COMPLETIONS: Documentation = Documentation::new()
        .form(&["seed", "entry-types"]);
}

/// Install the completion functions
pub fn initialize() {
    let env = environment::global();

    env.set("shell/generate-completions", Value::from(
            Executable::native(default_complete_wrapper)));
}

#[cfg(test)]
mod tests {
}
