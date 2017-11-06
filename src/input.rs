use std::collections::HashMap;
use std::io::prelude::*;
use std::io;

use termion::*;
use termion::raw::IntoRawMode;
use termion::input::TermRead;

use data::*;
use parse::pipeline;
use editor::LineEditor;
use editor::basic;

/// Key binding structure which can execute actions based on dynamic bindings
pub struct Keymap {
    binds: HashMap<event::Key, Box<Fn()>>
}

impl Keymap {
    /// Generate a new empty keymap
    pub fn new() -> Self {
        Keymap { binds: HashMap::new() }
    }

    /// Check for relevant bindings and return whether anything was run
    pub fn invoke(&self, k: &event::Key) -> bool {
        if let Some(f) = self.binds.get(&k) {
            f();
            true
        } else {
            false
        }
    }
}

// TODO: implement basic terminal support
/// Simple terminal which just reads a line
struct BasicTerminal {
}

impl BasicTerminal {
    fn new() -> io::Result<BasicTerminal> {
        Ok(BasicTerminal {})
    }

    fn read(&mut self) -> io::Result<Pipeline> {
        unimplemented!()
    }
}

/// Nicer terminal which supports line editing and completion
struct FancyTerminal {
    /// Output handle - restores the terminal to its normal state when dropped
    output: raw::RawTerminal<io::Stdout>,

    /// I/O streams
    input: input::Events<io::Stdin>,

    /// Active keyboard bindings and editing discipline
    keymap: Keymap,
    editor: LineEditor
}

impl FancyTerminal {
    /// Generate a new terminal. Fail if the term isn't a PTY.
    fn new() -> io::Result<FancyTerminal> {
        let raw = io::stdout().into_raw_mode()?;

        let mut term = FancyTerminal {
            output: raw,
            input: io::stdin().events(),
            keymap: Keymap::new(),
            editor: LineEditor::new(basic::Editor::new())
        };

        Ok(term)
    }

    /// Redraw the command-line prompt
    fn redraw_prompt(&mut self) -> io::Result<()> {
        // TODO: prompt customization
        let s = self.editor.buf().as_string();
        write!(self.output, "\r{}$ {}\r{}",
               clear::CurrentLine, s,
               cursor::Right(2+s.len() as u16))?;
        self.output.flush()
    }

    /// Handle a keyboard event for the entry prompt
    fn handle_key(&mut self, key: &event::Key) {
        // don't process a key if it's bound
        if self.keymap.invoke(key) {
            return;
        }

        // pass it to the line editor
        self.editor.handle_key(key);
    }

    /// Read a pipeline from the input terminal
    fn read(&mut self) -> io::Result<Pipeline> {
        self.redraw_prompt()?;

        while let Some(evt) = self.input.next() {
            match evt? {
                event::Event::Key(key) => self.handle_key(&key),
                event::Event::Mouse(_) => {}, // TODO: handle mouse events
                event::Event::Unsupported(_) => {}, // TODO: handle more events
            }

            if let Some(r) = self.editor.done() {
                return Ok(pipeline(r.as_bytes()).to_result().unwrap());
            }

            self.redraw_prompt()?;
        }

        panic!()
    }
}

/// Hold both interactive and basic terminals
enum InternalTerminal {
    Interactive(FancyTerminal),
    Basic(BasicTerminal)
}

/// Abstraction for the shell's input and output streams
/// 
/// The terminal is responsible for displaying output and accepting input from
/// the user. It's also responsible for history search, syntax highlighting, and
/// autocompletion.
/// 
/// If the shell's I/O isn't connected to a pty, it'll also detect that and
/// disable most of the fancy UI features.
pub struct Terminal(InternalTerminal);

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
            BasicTerminal::new()
                          .map(InternalTerminal::Basic)
                          .map(Terminal)
        } else {
            FancyTerminal::new()
                          .map(InternalTerminal::Interactive)
                          .map(Terminal)
        }

    }

    /// Read a pipeline from the input terminal
    pub fn read(&mut self) -> io::Result<Pipeline> {
        match &mut self.0 {
            &mut InternalTerminal::Basic(ref mut t)         => t.read(),
            &mut InternalTerminal::Interactive(ref mut t)   => t.read(),
        }
    }
}
