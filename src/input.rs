use std::collections::HashMap;
use std::io::prelude::*;
use std::sync::Arc;
use std::io;

use termion::*;
use termion::raw::IntoRawMode;
use termion::input::TermRead;

use stream::ReadWrapper;
use data::*;
use reader::read_pipeline;
use editor::{LineEditor, EditingDiscipline};
use editor::basic;
use completion::{CompletionSet, Entry};
use terminal;

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
    output: raw::RawTerminal<terminal::StdoutGuard<'a>>,

    /// I/O streams
    input: input::Events<terminal::StdinGuard<'a>>,

    /// Line editor
    editor: LineEditor<Box<EditingDiscipline>>,

    /// Parent terminal
    parent: &'a FancyTerminal,
    
    /// Active completion set, if any exists
    completions: Option<CompletionSet>,
}

// TODO: refactor completions out of ActiveEditor somehow
impl<'a> ActiveEditor<'a> {
    fn new(term: &'a mut FancyTerminal) -> io::Result<Self> {
        Ok(ActiveEditor {
            output: term.guard.stdout().into_raw_mode()?,
            input: term.guard.stdin().events(),
            editor: LineEditor::new(Box::new(basic::Editor::new())),
            parent: term,
            completions: None
        })
    }

    fn read(&mut self) -> io::Result<Pipeline> {
        self.redraw()?;

        while let Some(evt) = self.input.next() {
            match evt? {
                event::Event::Key(event::Key::Char('\t')) => {
                    // TODO: make this not hard-coded
                    self.trigger_completion()?;
                },
                event::Event::Key(key) => self.handle_key(&key),
                event::Event::Mouse(_) => {}, // TODO: handle mouse events
                event::Event::Unsupported(_) => {}, // TODO: handle more events
            }

            if let Some(r) = self.editor.done() {
                write!(self.output, "\r\n")?;
                self.output.flush()?;

                // if the line's blank, just reset
                if r.as_bytes().iter().all(|x| *x == b' ') {
                    self.redraw()?;
                } else {
                    let mut s = io::Cursor::new(r);

                    // need to add \r here since we're still in raw mode
                    match read_pipeline(&mut ReadWrapper::new(&mut s)) {
                        Ok(r) => return Ok(r),
                        Err(e) => {
                            println!("ysh: {}\r", e);
                        },
                    }
                    self.output.flush()?;
                }

                self.editor = LineEditor::new(Box::new(basic::Editor::new()));
            }

            self.redraw()?;
        }

        panic!()
    }

    /// Call this to fail the active completion
    ///
    /// It will display an error message and return to normal line-editing mode.
    ///
    /// # Panics
    ///
    /// This will panic if no completion is active
    fn fail_completion(&mut self, msg: &str) -> io::Result<()> {
        if let Some(c) = self.completions.take() {
            unimplemented!()
        } else {
            panic!("attempted to fail completion without active completion")
        }
    }

    /// Call this to abort the active completion
    ///
    /// Intermediate data will be discarded and the buffer will be unaffected
    ///
    /// # Panics
    ///
    /// This will panic if no completion is active
    fn abort_completion(&mut self) -> io::Result<()> {
        if let Some(c) = self.completions.take() {
            unimplemented!()
        } else {
            panic!("attempted to abort completion without active completion")
        }
    }

    /// Call this to commit the active completion
    ///
    /// The given entry's content will be inserted into the line editor
    fn commit_completion(&mut self, entry: Arc<Entry>) -> io::Result<()> {
        self.editor.buf_mut().insert(&entry.text);
        self.completions = None;
        Ok(())
    }

    /// Start interactive completion
    ///
    /// This will start running the interactive completion interface, using the
    /// system's current completion bindings.
    ///
    /// This may later be replaced by a more general terminal user interface
    /// framework.
    fn trigger_completion(&mut self) -> io::Result<()> {
        let res = CompletionSet::complete_any(&self.editor.buf().as_string());

        // handle the zero and one-completion cases
        if res.len() == 0 {
            self.fail_completion("No candidates available")?;
        } else if res.len() == 1 {
            // just insert the entry
            self.commit_completion(res.into_entries().swap_remove(0))?;
        } else {
            self.completions = Some(res);
        }
        self.redraw()
    }

    /// Redraw the command-line interface
    fn redraw(&mut self) -> io::Result<()> {
        // TODO: prompt customization
        
        if let Some(compl) = self.completions.as_ref() {
            let s = self.editor.buf().as_string();
            let mark = compl.marked_idx();
            let marked_entry = compl.marked();

            // TODO: insert greyed-out version of marked completion
            // draw prompt
            write!(self.output, "\r{}$ {}{}\r", clear::CurrentLine, s,
                   clear::AfterCursor)?;

            // draw completion list, capped to fixed size
            let nlines = if let Some(mark) = mark {
                // omit last line if mark is beyond range, so we have room to
                // draw the marked entry
                if mark > 10 { 9 }
                else { 10 }
            } else {
                compl.len().min(10)
            };
            for comp in compl.entries().into_iter().take(nlines) {
                if marked_entry.as_ref()
                               .map(|e| Arc::ptr_eq(e, comp))
                               .unwrap_or(false) {
                    // marked - highlight it
                    write!(self.output, "\n{}{}{}\r",
                           style::Invert, comp.text, style::NoInvert)?;
                } else {
                    write!(self.output, "\n{}\r", comp.text)?;
                }
            }

            // move back to start
            write!(self.output, "{}\r{}",
                   cursor::Up(nlines as u16),
                   cursor::Right(2+self.editor.buf().cursor() as u16))?;
        } else {
            let s = self.editor.buf().as_string();
            write!(self.output, "\r{}$ {}\r{}",
                   clear::CurrentLine, s,
                   cursor::Right(2+self.editor.buf().cursor() as u16))?;
        }
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
        
        if let Some(compl) = self.completions.as_mut() {
            compl.update(&self.editor.buf().as_string());
        }
    }
}

/// Nicer terminal which supports line editing and completion
struct FancyTerminal {
    /// Active keyboard bindings and editing discipline
    keymap: Keymap,

    /// Terminal guard
    guard: terminal::TerminalGuard,
}

impl FancyTerminal {
    /// Generate a new terminal. Fail if the term isn't a PTY.
    fn new() -> io::Result<FancyTerminal> {

        let term = FancyTerminal {
            keymap: Keymap::new(),
            guard: terminal::acquire()
        };

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
        if !terminal::is_tty() {
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
