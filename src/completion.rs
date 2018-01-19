use std::collections::HashSet;
use std::sync::{Arc, Weak};
use std::iter::FromIterator;
use std::path::Path;

use environment;
use environment::Environment;
use data::*;
use numeric::Number;

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

    fn from_obj(val: &Value) -> Eval<Self> {
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

    fn from_obj(v: &Value) -> Eval<Self> {
        if let ValueData::Map(ref h) = v.data {
            let typ = h.get(&Value::atom("type").hash().unwrap().unwrap())
                .ok_or(EvalError::TypeError(format!(
                            "missing required key :type")))?;
            let text = h.get(&Value::atom("text").hash().unwrap().unwrap())
                .ok_or(EvalError::TypeError(format!(
                            "missing required key :text")))?;
            let docs = h.get(&Value::atom("docs").hash().unwrap().unwrap())
                .ok_or(EvalError::TypeError(format!(
                            "missing required key :docs")))?;
            let weight = h.get(&Value::atom("weight").hash().unwrap().unwrap())
                .ok_or(EvalError::TypeError(format!(
                            "missing required key :weight")))?;
            
            let typ = EntryType::from_obj(typ)?;
            let text = text.into_str()?;
            let docs = docs.first()?;
            let weight = weight.into_num()?
                .ok_or(EvalError::TypeError(String::from(
                            "weight must be numeric")))?.round();

            let docs = if let Some(d) = docs {Some(d.into_str()?)}
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

fn file_completer(seed: &str) -> Eval<Vec<Entry>> {
    use std::os::unix::prelude::PermissionsExt;

    let paths = if let Some(p) = environment::global().get("path") {p}
                else {return Ok(Vec::new())};
    let paths = paths.into_seq()?
                     .into_iter()
                     .map(|x| x.into_str())
                     .collect::<Eval<Vec<String>>>()?;
    
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
                    println!("{}", s);
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
                    docs: value.doc.and_then(|x| x.short_desc)
                                   .map(|x| x.to_owned()),
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
fn default_complete_wrapper(_: &Environment, args: &[Value]) -> Eval<Value> {
    if args.len() != 2 {
        return Err(EvalError::Arity {
            got: args.len(),
            expected: 2
        });
    }

    let seed = args[0].into_str()?;
    let types = args[1].into_seq()?
                       .into_iter()
                       .map(|x| EntryType::from_obj(&x))
                       .collect::<Eval<Vec<_>>>()?;

    let res = default_completer(&seed, HashSet::from_iter(types))
             .into_iter()
             .map(|x| x.into_obj());
    Ok(Value::list(res))
}

/// Look up the active completion function in `shell/generate-completions` and
/// run it to build a completion list.
fn run_completer(seed: &str, entries: HashSet<EntryType>) -> Vec<Entry> {
    let results = environment::run_fn(&"shell/generate-completions",
                                      &[Value::str(seed), Value::list(
                                          entries.iter()
                                                 .map(|x| x.into_obj()))]);

    if let Some(Ok(r)) = results {
        let l = r.into_seq()
                 .and_then(|s| s.into_iter()
                                .map(|x| Entry::from_obj(&x))
                                .collect::<Eval<Vec<_>>>());
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
    /// Generate completions for the provided seed and goal set
    ///
    /// The `seed` parameter specifies the seed text to use when generating
    /// completions, and the `wanted` set lists the eligible entry types to
    /// return in the current context.
    pub fn complete(seed: &str, wanted: HashSet<EntryType>) -> Self {
        unimplemented!()
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
        }
        unimplemented!()
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

    /// Get the marked item if possible
    pub fn marked(&self) -> Option<Arc<Entry>> {
        self.marked.upgrade()
    }
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
