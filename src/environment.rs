use std::collections::HashMap;
use std::sync::Arc;

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
/// from multiple threads.
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
    pub fn get<K: AsRef<str>>(&self, key: K) -> Option<Arc<Value>> {
        self.get_ref(key).map(Arc::clone)
    }

    /// Get a *reference* to a value in the environment
    /// 
    /// This is more efficient than `get` since it doesn't take extra copies of
    /// the resulting `Arc`.
    pub fn get_ref<K: AsRef<str>>(&self, key: K) -> Option<&Arc<Value>> {
        self.mappings.get(key.as_ref())
    }
}
