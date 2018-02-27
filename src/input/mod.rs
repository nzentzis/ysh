mod render;
mod keybind;

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

use self::keybind::Keymap;

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

enum EditorState<W: Write> {
    Completion {
        set: CompletionSet,
        render: render::CompleteRenderer<W>
    },
    Editing {
        render: render::LineRenderer<W>
    },
}

impl<W: Write> EditorState<W> {
    fn output(&mut self) -> &mut raw::RawTerminal<W> {
        match *self {
            EditorState::Completion {ref mut render, ..} => render.output(),
            EditorState::Editing {ref mut render} => render.output()
        }
    }
}

struct ActiveEditor<'a> {
    /// Active mode
    mode: Option<EditorState<terminal::StdoutGuard<'a>>>,

    /// I/O streams
    input: input::Events<terminal::StdinGuard<'a>>,

    /// Line editor
    editor: LineEditor<Box<EditingDiscipline>>,

    /// Parent terminal
    parent: &'a FancyTerminal,
}

// TODO: this is really nasty - clean it up
impl<'a> ActiveEditor<'a> {
    fn new(term: &'a mut FancyTerminal, guard: &'a mut terminal::TerminalGuard)
            -> io::Result<Self> {
        Ok(ActiveEditor {
            mode: Some(EditorState::Editing {
                render: render::LineRenderer::new(guard
                                                 .stdout()
                                                 .into_raw_mode()?)
            }),
            input: guard.stdin().events(),
            editor: LineEditor::new(Box::new(basic::Editor::new())),
            parent: term,
        })
    }

    /// Switch back to editing mode, regardless of what mode we're in now
    fn reset_mode(&mut self) -> io::Result<()> {
        match self.mode.take().unwrap() {
            EditorState::Completion {render, ..} => {
                let (t,r) = render.end();
                self.mode = Some(EditorState::Editing {
                    render: render::LineRenderer::new(t)
                });
                r
            }
            EditorState::Editing {render, ..} => {
                self.mode = Some(EditorState::Editing {render});
                Ok(())
            }
        }
    }

    /// Convert the mode back to line, then to another target mode
    ///
    /// Upon conversion failure, do nothing and return an error.
    fn convert_mode<F>(&mut self, f: F) -> io::Result<()>
            where F: FnOnce(raw::RawTerminal<terminal::StdoutGuard<'a>>)
                       -> Result<EditorState<terminal::StdoutGuard<'a>>,
                                 (raw::RawTerminal<terminal::StdoutGuard<'a>>,
                                  io::Error)> {
        self.reset_mode()?;
        if let Some(EditorState::Editing{render}) = self.mode.take() {
            match f(render.into_output()) {
                Ok(r) => self.mode = Some(r),
                Err((r,e)) => {
                    self.mode = Some(EditorState::Editing {
                        render: render::LineRenderer::new(r)
                    });
                    return Err(e);
                }
            }
            Ok(())
        } else {
            panic!("invalid editing mode state")
        }
    }

    fn read(&mut self) -> io::Result<Pipeline> {
        self.redraw()?;

        while let Some(evt) = self.input.next() {
            match evt? {
                event::Event::Key(key) => self.handle_key(&key),
                event::Event::Mouse(_) => {}, // TODO: handle mouse events
                event::Event::Unsupported(_) => {}, // TODO: handle more events
            }

            if let Some(r) = self.editor.done() {
                {
                    let mut output = self.mode.as_mut().unwrap().output();
                    write!(output, "\r\n")?;
                    output.flush()?;
                }

                // if the line's blank, just reset
                if r.as_bytes().iter().all(|x| *x == b' ') {
                    self.reset_mode()?;
                    self.redraw()?;
                } else {
                    self.reset_mode()?;
                    let mut s = io::Cursor::new(r);

                    // need to add \r here since we're still in raw mode
                    match read_pipeline(&mut ReadWrapper::new(&mut s)) {
                        Ok(r) => return Ok(r),
                        Err(e) => {
                            println!("ysh: {}\r", e);
                        },
                    }
                    {
                        self.mode.as_mut().unwrap().output().flush()?;
                    }
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
        self.reset_mode()
    }

    /// Call this to abort the active completion
    ///
    /// Intermediate data will be discarded and the buffer will be unaffected
    ///
    /// # Panics
    ///
    /// This will panic if no completion is active
    fn abort_completion(&mut self) -> io::Result<()> {
        self.reset_mode()
    }

    /// Call this to commit the active completion
    ///
    /// The given entry's content will be inserted into the line editor
    fn commit_completion(&mut self, entry: Arc<Entry>) -> io::Result<()> {
        // remove seed from stuff to insert so we don't replicate input
        let to_insert = entry.text.trim_left_matches(&self.editor.buf().as_string());

        self.editor.buf_mut().insert(&to_insert);
        self.reset_mode()
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
            self.convert_mode(move |strm| {
                let render = render::CompleteRenderer::new(strm, &res);
                Ok(EditorState::Completion {
                    render,
                    set: res
                })
            })?;

            let esc = self.parent.keymap.bind(event::Key::Esc, move || {
            });
        }
        self.redraw()
    }

    /// Redraw the command-line interface
    fn redraw(&mut self) -> io::Result<()> {
        match self.mode.as_mut().unwrap() {
            &mut EditorState::Editing {ref mut render} =>
                render.render(&self.editor),
            &mut EditorState::Completion {ref mut render, ref mut set} => {
                render.update(&set);
                render.render(&self.editor)
            }
        }
    }

    /// Try to handle internal key bindings
    fn handle_internal_key(&mut self, key: &event::Key) -> bool {
        enum Action {
            ComplAbort,
            ComplCommit(Arc<Entry>), // (entry, consume_key)
        }
        let mut action = None;
        let mut consume = false;

        if let Some(EditorState::Completion {ref mut set, ..}) = self.mode {
            // handle completion bindings
            match *key {
                event::Key::Char('\t') => {
                    set.mark_next();
                    return true;
                },
                event::Key::Up => {
                    set.mark_prev();
                    return true;
                },
                event::Key::Down => {
                    set.mark_next();
                    return true;
                },
                event::Key::Esc => {
                    // abort
                    action = Some(Action::ComplAbort);
                    consume = true;
                },
                event::Key::Right => {
                    if self.editor.buf().at_end() {
                        // finish the completion
                        action = Some(if let Some(marked) = set.marked() {
                            consume = true;
                            Action::ComplCommit(marked)
                        } else {
                            consume = true;
                            Action::ComplAbort
                        });
                    }
                },
                event::Key::Char('\n') => {
                    if let Some(m) = set.marked() {
                        // don't consume char so it can be immediately run
                        action = Some(Action::ComplCommit(m));
                    }
                }
                _ => {}
            }
        }
        
        match action {
            Some(Action::ComplAbort) => {
                self.abort_completion();
            },
            Some(Action::ComplCommit(c)) => {
                self.commit_completion(c);
                self.redraw();
            },
            _ => {}
        }
        consume
    }

    /// Handle a keyboard event for the entry prompt
    fn handle_key(&mut self, key: &event::Key) {
        // don't process a key if it's bound
        if self.handle_internal_key(key) || self.parent.keymap.invoke(key) {
            return;
        }

        // TODO: move this so it's not hard-coded
        if *key == event::Key::Char('\t') {
            self.trigger_completion().unwrap();
            return;
        }

        // pass it to the line editor
        self.editor.handle_key(key);

        if let Some(EditorState::Completion {ref mut set, ..}) = self.mode {
            set.update(&self.editor.buf().as_string());
        }
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

        let term = FancyTerminal {
            keymap: Keymap::new(),
        };

        Ok(term)
    }

    /// Read a pipeline from the input terminal
    fn read(&mut self) -> io::Result<Pipeline> {
        let mut guard = terminal::acquire();
        let r = {
            ActiveEditor::new(self, &mut guard)?.read()
        };
        r
    }
}

/// Hold both interactive and basic terminals
enum InternalTerminal {
    Interactive(FancyTerminal),
    Basic(BasicTerminal)
}

/// Abstraction for the shell's terminal UI
/// 
/// This handles line editing, completions, history search, and so on.
/// 
/// If the shell's I/O isn't connected to a pty, it'll detect that and disable
/// most of the fancy UI features.
pub struct Terminal {
    term: InternalTerminal
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
        let term = if !terminal::is_tty() {
            BasicTerminal::new()
                          .map(InternalTerminal::Basic)
        } else {
            FancyTerminal::new()
                          .map(InternalTerminal::Interactive)
        }?;

        Ok(Terminal {
            term
        })

    }

    /// Read a pipeline from the input terminal
    pub fn read(&mut self) -> io::Result<Pipeline> {
        match &mut self.term {
            &mut InternalTerminal::Basic(ref mut t)         => t.read(),
            &mut InternalTerminal::Interactive(ref mut t)   => t.read(),
        }
    }
}
