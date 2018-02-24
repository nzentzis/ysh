use std::collections::HashMap;
use std::sync::{Arc, Weak, Mutex};

use termion::*;

enum Binding {
    Permanent(Box<Fn() + Send + 'static>),
    Scoped(BindingInfo)
}

/// Key binding structure which can execute actions based on dynamic bindings
pub struct Keymap {
    binds: Mutex<HashMap<event::Key, Binding>>,
}

impl Keymap {
    /// Generate a new empty keymap
    pub fn new() -> Self {
        Keymap {
            binds: Mutex::new(HashMap::new())
        }
    }

    /// Check for relevant bindings and return whether anything was run
    pub fn invoke(&self, k: &event::Key) -> bool {
        let mut w = self.binds.lock().unwrap();
        if let Some(f) = w.get(&k) {
            match *f {
                Binding::Permanent(ref f) => f(),
                Binding::Scoped(ref f) => {
                    if let Some(r) = f.0.upgrade() {
                        (*r)();
                        return true;
                    }
                }
            }
        } else {
            return false;
        }
        w.remove(&k);
        false
    }

    /// Register a new permanent binding
    pub fn bind_permanent<F>(&self, key: event::Key, f: F)
            where F: Fn() + Send + 'static {
        let mut w = self.binds.lock().unwrap();
        w.insert(key, Binding::Permanent(Box::new(f)));
    }

    /// Register a new scoped binding
    pub fn bind<'a, F>(&'a self, key: event::Key, f: F) -> ScopedBinding
            where F: Fn() + Send + 'static {
        let f: Arc<Box<Fn() + Send + 'static>> = Arc::new(Box::new(f));

        let mut w = self.binds.lock().unwrap();
        w.insert(key, Binding::Scoped(BindingInfo(Arc::downgrade(&f))));

        ScopedBinding {
            func: f
        }
    }
}

struct BindingInfo(Weak<Box<Fn() + Send + 'static>>);

/// Handle for a scoped binding, which auto-deletes when its binding object is
/// dropped.
///
/// When the `ScopedBinding` is dropped, the binding will be automatically
/// removed so the closure will no longer be called.
///
/// The binding will never be called if the keymap is dropped. This can be
/// detected using the `is_unlinked` method on the `ScopedBinding`. Note that
/// even in this circumstance, the binding will not be dropped until the
/// `ScopedBinding` is.
pub struct ScopedBinding {
    func: Arc<Box<Fn() + Send + 'static>>,
}
