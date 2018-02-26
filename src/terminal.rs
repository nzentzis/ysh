//! Wrapper object for controlling access to the terminal. Allows management of
//! FD usage and terminal group information through a type-safe interface.

use std::sync::{Condvar, Mutex, Arc};
use std::io::prelude::*;
use std::marker::PhantomData;
use std::os::unix::prelude::*;
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

    /// Convert this guard into owned (stdin, stdout, stderr) channels
    ///
    /// This is less efficient than the lifetime-bounded versions, but allows
    /// transferring ownership of the resulting guards outside the lifetime of
    /// the `TerminalGuard`.
    pub fn into_owned_channels(self) -> (OwnedStdinGuard, OwnedStdoutGuard, OwnedStderrGuard) {
        let a = Arc::new(self);
        
        (OwnedStdinGuard {
            chan: io::stdin(),
            guard: Arc::clone(&a)
        },
        OwnedStdoutGuard {
            chan: io::stdout(),
            guard: Arc::clone(&a)
        },
        OwnedStderrGuard {
            chan: io::stderr(),
            guard: a
        })
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

/// Owned version of `StdoutGuard` that carries the lock around with it
pub struct OwnedStdoutGuard {
    chan: io::Stdout,
    guard: Arc<TerminalGuard>,
}

impl Write for OwnedStdoutGuard {
    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        self.chan.write(data)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.chan.flush()
    }
}

impl OwnedStdoutGuard {
    pub fn as_raw(self) -> FdGuard<Self> {
        let fd = self.chan.as_raw_fd();
        FdGuard{guard: self, fd}
    }
}

/// Owned version of `StdinGuard` that carries the lock around with it
pub struct OwnedStdinGuard {
    chan: io::Stdin,
    guard: Arc<TerminalGuard>,
}

impl Read for OwnedStdinGuard {
    fn read(&mut self, data: &mut [u8]) -> io::Result<usize> {
        self.chan.read(data)
    }
}

impl OwnedStdinGuard {
    pub fn as_raw(self) -> FdGuard<Self> {
        let fd = self.chan.as_raw_fd();
        FdGuard{guard: self, fd}
    }
}

/// Owned version of `StderrGuard` that carries the lock around with it
pub struct OwnedStderrGuard {
    chan: io::Stderr,
    guard: Arc<TerminalGuard>,
}

impl Write for OwnedStderrGuard {
    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        self.chan.write(data)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.chan.flush()
    }
}

impl OwnedStderrGuard {
    pub fn as_raw(self) -> FdGuard<Self> {
        let fd = self.chan.as_raw_fd();
        FdGuard{guard: self, fd}
    }
}

/// Result of converting an otherwise owned guard into a `RawFd`
///
/// This does not permit writing or reading, but must be maintained for as long
/// as the produced FD is used. Multiple concurrent accessors to an FD will have
/// weird effects.
pub struct FdGuard<T> {
    guard: T,
    fd: RawFd
}

impl<T> FdGuard<T> {
    pub fn fd(&self) -> RawFd { self.fd }
}
