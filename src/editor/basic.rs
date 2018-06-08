use termion::event::Key;

use history;
use editor::*;

pub struct Editor {
    history_idx: Option<usize>
}

impl Editor {
    pub fn new() -> Self {
        Editor {
            history_idx: None
        }
    }
}

impl EditingDiscipline for Editor {
    fn handle_key(&mut self, buf: &mut EditBuffer, key: &Key) -> bool {
        match key {
            &Key::Left => {
                buf.move_cursor(-1);
                true
            },
            &Key::Right => {
                buf.move_cursor(1);
                true
            },
            &Key::Up => {
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
            &Key::Down => {
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
            &Key::Home => {
                buf.set_cursor(0);
                true
            },
            &Key::End => {
                let end = buf.view().len();
                buf.set_cursor(end+1);
                true
            },
            &Key::Delete => {
                buf.delete_forward(1);
                true
            },
            &Key::Backspace => {
                buf.delete(1);
                true
            },
            &Key::Char(c) => {
                buf.push(c);
                true
            },
            _ => false
        }
    }
}
