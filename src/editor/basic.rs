use input::Keymap;
use editor::LineEditor;

pub struct Editor {
}

impl Editor {
    pub fn new() -> Self {
        Editor {}
    }
}

impl LineEditor for Editor {
    fn init_bindings(&mut self, map: &mut Keymap) {
    }
}
