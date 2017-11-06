use termion::event::Key;

use editor::*;

pub struct Editor {
}

impl Editor {
    pub fn new() -> Self {
        Editor {}
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
