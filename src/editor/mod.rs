pub mod basic;

use input::Keymap;

/// Basic trait for line-editing disciplines
pub trait LineEditor {
    /// Populate the given keymap with bindings for this line-editing discipline
    fn init_bindings(&mut self, map: &mut Keymap);
}

/// Line editing structure for holding the current input buffer, cursor
/// position, and highlighting regions
pub struct EditBuffer {
    buf: Vec<char>,
    cursor: usize
}

impl EditBuffer {
}
