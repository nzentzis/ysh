use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use data::*;

/// An environment handle which allows mutable access
/// 
/// While this exists, the environment cannot be accessed.
pub struct ExclusiveEnvironment<'a> {
    map: &'a mut HashMap<String, Arc<Value>>
}

impl<'a> ExclusiveEnvironment<'a> {
    /// Add a mapping to the environment
    pub fn set<S: AsRef<str>>(&mut self, key: S, val: Value) {
        self.map.insert(String::from(key.as_ref()), Arc::new(val));
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
    mappings: Arc<HashMap<String, Arc<Value>>>
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
    pub fn get<K: AsRef<str>>(&self, key: K) -> Option<Arc<Value>> {
        self.mappings.get(key.as_ref()).map(Arc::clone)
            .or_else(|| global().get(key))
    }
}

/// Implements the global environment where function definitions and other
/// global bindings are stored. This is shared across threads and there's only
/// one copy per process.
pub struct GlobalEnvironment {
    mappings: RwLock<HashMap<String, Arc<Value>>>
}

lazy_static! {
    static ref ENV: GlobalEnvironment = GlobalEnvironment::new();
    static ref EMPTY: Environment = Environment::new();
}

impl GlobalEnvironment {
    fn new() -> Self {
        GlobalEnvironment { mappings: RwLock::new(HashMap::new()) }
    }

    pub fn get<K: AsRef<str>>(&self, key: K) -> Option<Arc<Value>> {
        let r = self.mappings.read().unwrap();
        r.get(key.as_ref()).map(Arc::clone)
    }

    pub fn set<K: AsRef<str>>(&self, key: K, val: Value) {
        let mut w = self.mappings.write().unwrap();
        w.insert(key.as_ref().to_owned(), Arc::new(val));
    }
}

/// Get a reference to the process-wide global environment
pub fn global() -> &'static GlobalEnvironment { &ENV }

/// Get an empty environment
pub fn empty() -> Environment { EMPTY.clone() }
