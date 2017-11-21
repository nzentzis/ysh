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

struct GlobalMapping {
    value: Value,
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

    pub fn get<K: AsRef<str>>(&self, key: K) -> Option<Value> {
        let r = self.mappings.read().unwrap();
        r.get(key.as_ref())
         .map(|r| r.value.clone())
    }

    fn set_key<K: AsRef<str>>(&self, key: K, val: Value, mutable: bool) {
        use std::collections::hash_map::Entry;

        let mapping = GlobalMapping { value: val, mutable };

        let mut w = self.mappings.write().unwrap();
        let entry = w.entry(key.as_ref().to_owned());

        match entry {
            Entry::Occupied(mut e) => {
                let m = e.get().mutable;
                if m { e.insert(mapping); }
            },
            Entry::Vacant(v) => { v.insert(mapping); }
        }
    }

    pub fn set<K: AsRef<str>>(&self, key: K, val: Value) {
        self.set_key(key, val, true);
    }

    pub fn set_immut<K: AsRef<str>>(&self, key: K, val: Value) {
        self.set_key(key, val, false);
    }
}

/// Get a reference to the process-wide global environment
pub fn global() -> &'static GlobalEnvironment { &ENV }

/// Get an empty environment
pub fn empty() -> Environment { EMPTY.clone() }
