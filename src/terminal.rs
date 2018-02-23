//! Wrapper object for controlling access to the terminal. Allows management of
//! FD usage and terminal group information through a type-safe interface.

use std::sync::{Condvar, Mutex, Arc};
use std::io::prelude::*;
use std::marker::PhantomData;
use std::io;

use termion;

lazy_static! {
    static ref TERM: Arc<Terminal> = Arc::new(Terminal {
        available: Mutex::new(true),
        avail_cond: Condvar::new(),
        is_tty: termion::is_tty(&io::stdin())
    });
}

struct Terminal {
    available: Mutex<bool>,
    avail_cond: Condvar,
    is_tty: bool
}

/// Check whether the terminal is a TTY
pub fn is_tty() -> bool {
    TERM.is_tty
}

/// Acquire handle allowing exclusive access to the terminal
///
/// No other component may access the terminal until the resulting guard
/// object is dropped.
pub fn acquire() -> TerminalGuard {
    let mut guard = TERM.available.lock().unwrap();
    loop {
        if *guard {
            *guard = false;
            return TerminalGuard {};
        }
        guard = TERM.avail_cond.wait(guard).unwrap();
    }
}

// TODO: allow hijacking the terminal from existing code to support user code
//       launching foreground tasks

pub struct TerminalGuard {
}

impl TerminalGuard {
    /// Utility function for releasing the guard
    ///
    /// Just drops it, but `guard.release()` is more readable than
    /// `let _ = guard;`
    pub fn release(self) {}

    /// Generate a stdout guard for the terminal
    pub fn stdout(&self) -> StdoutGuard {
        StdoutGuard {
            chan: io::stdout(),
            life: PhantomData
        }
    }

    /// Generate a stdin guard for the terminal
    pub fn stdin(&self) -> StdinGuard {
        StdinGuard {
            chan: io::stdin(),
            life: PhantomData
        }
    }

    /// Generate a stderr guard for the terminal
    pub fn stderr(&self) -> StderrGuard {
        StderrGuard {
            chan: io::stderr(),
            life: PhantomData
        }
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        let mut guard = TERM.available.lock().unwrap();
        *guard = true;
        TERM.avail_cond.notify_one();
    }
}

pub struct StdoutGuard<'a> {
    chan: io::Stdout,
    life: PhantomData<&'a ()>
}

pub struct StdinGuard<'a> {
    chan: io::Stdin,
    life: PhantomData<&'a ()>
}

pub struct StderrGuard<'a> {
    chan: io::Stderr,
    life: PhantomData<&'a ()>
}

impl<'a> Write for StdoutGuard<'a> {
    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        self.chan.write(data)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.chan.flush()
    }
}

impl<'a> Write for StderrGuard<'a> {
    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        self.chan.write(data)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.chan.flush()
    }
}

impl<'a> Read for StdinGuard<'a> {
    fn read(&mut self, data: &mut [u8]) -> io::Result<usize> {
        self.chan.read(data)
    }
}
