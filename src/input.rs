use std::io::prelude::*;
use std::io;

use termion::*;
use termion::raw::IntoRawMode;

use data::*;
use parse::pipeline;

/// Abstraction for the shell's input and output streams
/// 
/// The terminal is responsible for displaying output and accepting input from
/// the user. It's also responsible for history search, syntax highlighting, and
/// autocompletion.
/// 
/// If the shell's I/O isn't connected to a pty, it'll also detect that and
/// disable most of the fancy UI features.
pub struct Terminal {
    /// Restores the terminal to its normal state when dropped
    restorer: Option<raw::RawTerminal<io::Stdout>>,

    /// I/O streams
    input: io::Stdin,
}

impl Terminal {
    /// Create a new terminal object
    /// 
    /// This generates a new terminal object and connects it to the current PTY.
    /// If there's not a PTY connected, it generates a terminal with completion,
    /// highlighting, and other user-friendly features disabled.
    /// 
    /// # Errors
    /// This function will fail with an error if it's connected to a PTY and
    /// cannot open it in raw mode.
    pub fn new() -> io::Result<Terminal> {
        let output = io::stdout();

        if !is_tty(&output) {
            // disable fancy formatting
            return Ok(Terminal {
                restorer: None,
                input: io::stdin()
            });
        }

        let raw = output.into_raw_mode()?;

        Ok(Terminal {
            restorer: Some(raw),
            input: io::stdin()
        })
    }

    /// Redraw the command-line prompt
    fn redraw_prompt(&mut self) {
    }

    pub fn read(&mut self) -> io::Result<Pipeline> {
        self.redraw_prompt();

        panic!()
    }
}
