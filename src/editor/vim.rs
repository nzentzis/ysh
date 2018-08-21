// This is a terrible placeholder implementation of Vim bindings, and it needs
// a lot of work before it's up to a proper standard.
//
// However, it should be good enough that you don't have to toss out all your
// muscle memory.

use termion::event::Key;

use history;
use editor::*;

#[derive(Copy, Clone, PartialEq, Eq)]
enum Dir { Forward, Backward }

#[derive(Copy, Clone, PartialEq, Eq)]
enum Operator {
    Delete,
    Change,
    Yank
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum TextObject {
    Word {
        hard: bool, // word or WORD
    },
    SquareBrackets,
    Parens,
    Angles,
    DoubleQuote,
    SingleQuote,
    TickQuote
}

impl TextObject {
    /// Compute the character span for the given text object
    fn compute(self, buf: &EditBuffer) -> (usize, usize) {
        match self {
            TextObject::Word {hard} => {
                unimplemented!()
            },
            TextObject::SquareBrackets => {
                unimplemented!()
            },
            TextObject::Parens => {
                unimplemented!()
            },
            TextObject::Angles => {
                unimplemented!()
            },
            TextObject::DoubleQuote => {
                unimplemented!()
            },
            TextObject::SingleQuote => {
                unimplemented!()
            },
            TextObject::TickQuote => {
                unimplemented!()
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Motion {
    Word {
        dir: Dir,
        hard: bool, // whether it's word or WORD
    },
    WordEnd {
        hard: bool,
    },
    Left,
    Right,
    Up,
    Down,
    StartNonblank,
    Start,
    End,
    Until {
        dir: Dir,
        over: bool
    },
    AroundObj,
    InsideObj,
    Pair, // the % motion
}

impl Motion {
    /// Evaluate the motion, returning a (start_cursor, end_cursor) pair
    ///
    /// This must not be used with the `AroundObj` or `InsideObj` motions.
    ///
    /// # Panics
    /// This will panic if called on the `AroundObj` or `InsideObj` motions.
    fn eval(self, buf: &EditBuffer) -> (usize, usize) {
        let chars = buf.view();

        match self {
            Motion::Word {dir, hard} => {
                let mut pos = buf.cursor();
                if pos >= chars.len() { return (pos, pos); }

                // whether we started in a word or not. this doesn't matter if
                // hard is set, but otherwise we need to track it so we can
                // jump properly over both "foo" and ")])"
                let orig_in_word = chars[pos].is_ascii_alphanumeric() ||
                                   chars[pos] == '_';

                while pos < chars.len() {
                    let c = chars[pos];

                    let is_end = if hard {
                        c.is_ascii_whitespace()
                    } else {
                        if orig_in_word {!(c.is_ascii_alphanumeric() || c == '_')}
                        else {c.is_ascii_alphanumeric() || c == '_'}
                    };

                    if is_end {break;}
                    match dir {
                        Dir::Forward => {
                            pos += 1;
                        }
                        Dir::Backward => {
                            if pos == 0 { break; }
                            pos -= 1;
                        }
                    }
                }

                (buf.cursor(), pos)
            },
            Motion::WordEnd {hard } => {
                unimplemented!()
            },
            Motion::Left => {
                (buf.cursor(), buf.cursor().saturating_sub(1))
            },
            Motion::Right => {
                (buf.cursor(), buf.cursor().saturating_add(1))
            },
            Motion::Up => {
                unimplemented!()
            },
            Motion::Down => {
                unimplemented!()
            },
            Motion::StartNonblank => {
                let view = buf.view();
                let mut pos = 0;
                while pos < view.len() {
                    if !view[pos].is_whitespace() { break; }
                    pos += 1;
                }
                (buf.cursor(), pos)
            },
            Motion::Start => (buf.cursor(), 0),
            Motion::End => (buf.cursor(), buf.len()),
            Motion::Until { dir, over } => {
                unimplemented!()
            },
            Motion::AroundObj => panic!("cannot evaluate AroundObj motion"),
            Motion::InsideObj => panic!("cannot evaluate InsideObj motion"),
            Motion::Pair => {
                unimplemented!()
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum State {
    /// Insert mode
    Insert,

    /// Normal mode
    Normal,

    /// The user invoked an operator which is now waiting for a motion to act on
    OpMotion(Operator),

    /// The user invoked an operator with a motion which is waiting for a param
    OpMotionDigraph(Operator, Motion),

    /// Waiting for the param of a motion
    Motion(Motion),

    // TODO: register specification with '
    // TODO: macros
}

pub struct Editor {
    history_idx: Option<usize>,
    mode: State,
}

impl Editor {
    pub fn new() -> Self {
        Editor {
            history_idx: None,
            mode: State::Insert
        }
    }

    fn handle_fallback(&mut self, buf: &mut EditBuffer, key: &Key) -> bool {
        match key {
            Key::Left => {
                buf.move_cursor(-1);
                true
            },
            Key::Right => {
                buf.move_cursor(1);
                true
            },
            Key::Up => {
                let h_len = history::db().len();
                if h_len > 0 && self.history_idx.map(|x| x < (h_len-1)).unwrap_or(true) {
                    self.history_idx = Some(self.history_idx
                                                .map(|x| x+1)
                                                .unwrap_or(0));
                    let s = history::db().get(self.history_idx.unwrap());
                    buf.replace(&s.into_repr());
                    true
                } else {
                    false
                }
            },
            Key::Down => {
                if let Some(i) = self.history_idx {
                    if i == 0 {
                        self.history_idx = None;
                        buf.clear();
                        true
                    } else {
                        self.history_idx = Some(i-1);
                        let s = history::db().get(self.history_idx.unwrap());
                        buf.replace(&s.into_repr());
                        true
                    }
                } else {
                    false
                }
            },
            Key::Home => {
                buf.set_cursor(0);
                true
            },
            Key::End => {
                let end = buf.view().len();
                buf.set_cursor(end+1);
                true
            },
            Key::Esc => {
                self.mode = State::Normal;
                true
            }
            _ => false
        }
    }

    fn handle_normal_key(&mut self, buf: &mut EditBuffer, key: &Key) -> bool {
        match key {
            Key::Char(c) => {
                false
            },
            Key::Esc => {
                self.mode = State::Normal;
                true
            },
            _ => self.handle_fallback(buf, key)
        }
    }

    fn handle_insert_key(&mut self, buf: &mut EditBuffer, key: &Key) -> bool {
        match key {
            Key::Delete => {
                buf.delete_forward(1);
                true
            },
            Key::Backspace => {
                buf.delete(1);
                true
            },
            Key::Char(c) => {
                buf.push(*c);
                true
            },
            _ => self.handle_fallback(buf, key)
        }
    }
}

impl EditingDiscipline for Editor {
    fn handle_key(&mut self, buf: &mut EditBuffer, key: &Key) -> bool {
        match self.mode {
            State::Insert => self.handle_insert_key(buf, key),
            State::Normal => self.handle_normal_key(buf, key),
            _ => unimplemented!()
        }
    }
}
