use std::collections::HashMap;
use std::io::prelude::*;
use std::io;

use nom::*;

use termion::*;
use termion::raw::IntoRawMode;
use termion::input::TermRead;

use data::*;
use reader::read_pipeline;
use editor::{LineEditor, EditingDiscipline};
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

struct ActiveEditor<'a> {
    /// Output handle - restores the terminal to its normal state when dropped
    output: raw::RawTerminal<io::Stdout>,

    /// I/O streams
    input: input::Events<io::Stdin>,

    /// Line editor
    editor: LineEditor<Box<EditingDiscipline>>,

    /// Parent terminal
    parent: &'a mut FancyTerminal
}

impl<'a> ActiveEditor<'a> {
    fn new(term: &'a mut FancyTerminal) -> io::Result<Self> {
        let raw = io::stdout().into_raw_mode()?;

        Ok(ActiveEditor {
            output: raw,
            input: io::stdin().events(),
            editor: LineEditor::new(Box::new(basic::Editor::new())),
            parent: term
        })
    }

    fn read(&mut self) -> io::Result<Pipeline> {
        self.redraw_prompt()?;

        while let Some(evt) = self.input.next() {
            match evt? {
                event::Event::Key(key) => self.handle_key(&key),
                event::Event::Mouse(_) => {}, // TODO: handle mouse events
                event::Event::Unsupported(_) => {}, // TODO: handle more events
            }

            if let Some(r) = self.editor.done() {
                write!(self.output, "\r\n")?;
                self.output.flush()?;

                // if the line's blank, just reset
                if r.as_bytes().iter().all(|x| *x == b' ') {
                    self.redraw_prompt()?;
                } else {
                    let mut s = io::Cursor::new(r);

                    // need to add \r here since we're still in raw mode
                    match read_pipeline(&mut s) {
                        Ok(r) => return Ok(r),
                        Err(e) => {
                            println!("ysh: {}\r", e);
                        },
                    }
                    self.output.flush()?;
                }

                self.editor = LineEditor::new(Box::new(basic::Editor::new()));
            }

            self.redraw_prompt()?;
        }

        panic!()
    }

    /// Redraw the command-line prompt
    fn redraw_prompt(&mut self) -> io::Result<()> {
        // TODO: prompt customization
        let s = self.editor.buf().as_string();
        write!(self.output, "\r{}$ {}\r{}",
               clear::CurrentLine, s,
               cursor::Right(2+self.editor.buf().cursor() as u16))?;
        self.output.flush()
    }

    /// Handle a keyboard event for the entry prompt
    fn handle_key(&mut self, key: &event::Key) {
        // don't process a key if it's bound
        if self.parent.keymap.invoke(key) {
            return;
        }

        // pass it to the line editor
        self.editor.handle_key(key);
    }
}

/// Nicer terminal which supports line editing and completion
struct FancyTerminal {
    /// Active keyboard bindings and editing discipline
    keymap: Keymap,
}

impl FancyTerminal {
    /// Generate a new terminal. Fail if the term isn't a PTY.
    fn new() -> io::Result<FancyTerminal> {

        let term = FancyTerminal { keymap: Keymap::new(), };

        Ok(term)
    }

    /// Read a pipeline from the input terminal
    fn read(&mut self) -> io::Result<Pipeline> {
        ActiveEditor::new(self)?.read()
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
