pub mod basic;

use std::iter::FromIterator;
use std::boxed::Box;

use termion::event::Key;

/// Basic trait for line-editing disciplines
pub trait EditingDiscipline {
    /// Process a keyboard input
    /// 
    /// Returns whether the discipline handled the key event
    fn handle_key(&mut self, buf: &mut EditBuffer, key: &Key) -> bool;
}

impl<'a> EditingDiscipline for Box<EditingDiscipline> {
    fn handle_key(&mut self, buf: &mut EditBuffer, key: &Key) -> bool {
        use std::ops::DerefMut;
        let mut r: &mut EditingDiscipline = self.deref_mut();
        r.deref_mut().handle_key(buf, key)
    }
}

/// An interactive line editor
/// 
/// Manages editing for an underlying buffer, and allows swapping out the active
/// editing discipline at runtime.
pub struct LineEditor<D: EditingDiscipline> {
    discipline: D,
    buffer: EditBuffer,
    is_done: bool
}

impl<D: EditingDiscipline> LineEditor<D> {
    /// Create a new line editor with the given discipline
    pub fn new(disc: D) -> Self {
        LineEditor {
            discipline: disc,
            buffer: EditBuffer::new(),
            is_done: false
        }
    }

    /// Switch the active editing discipline
    pub fn set_discipline(&mut self, disc: D) {
        self.discipline = disc;
    }

    /// Get access to the underlying edit buffer
    pub fn buf(&self) -> &EditBuffer { &self.buffer }

    /// Get mutable access to the underlying edit buffer
    pub fn buf_mut(&mut self) -> &mut EditBuffer { &mut self.buffer }

    /// Extract and return the input result if it's complete
    pub fn done(&mut self) -> Option<String> {
        if self.is_done {
            let r = self.buffer.as_string();
            self.buffer.clear();
            Some(r)
        } else {
            None
        }
    }

    /// Process a key event
    /// 
    /// Returns whether the line editor handled that key
    pub fn handle_key(&mut self, key: &Key) -> bool {
        if let &Key::Char(c) = key {
            if c == '\n' {
                self.is_done = true;
                return true;
            }
        }
        self.discipline.handle_key(&mut self.buffer, key)
    }
}

/// Line editing structure for holding the current input buffer, cursor
/// position, and highlighting regions
/// 
/// Editing buffers support the following operations:
/// * Filtering the entire buffer contents through an arbitrary function
/// * Moving the cursor to arbitrary locations
/// * Inserting or deleting text at the cursor position
/// * Retrieving the buffer contents as a `String`
/// * Viewing the buffer contents as a character slice
/// * Creating and deleting highlight regions
/// 
/// At any given time, the cursor position will point to a location in the edit
/// buffer. Keep in mind that these locations are *not* the same as character
/// indices:
/// 
///     char index: 0 1 2 3 4 5
///                 h e l l o !
///     cursor:    0 1 2 3 4 5 6
/// 
/// In other words, cursor indices refer to locations *between* characters,
/// while character indices refer to the characters themselves. This means that
/// cursor index 0 will always be valid, even in an empty edit buffer.
pub struct EditBuffer {
    buf: Vec<char>,
    cursor: usize
}

impl EditBuffer {
    /// Create a new, empty EditBuffer
    pub fn new() -> EditBuffer {
        EditBuffer {
            buf: Vec::with_capacity(128),
            cursor: 0,
        }
    }

    /// Filter the buffer contents through a function
    /// 
    /// This operation is O(n) at the moment since the internal storage must be
    /// duplicated, but later updates may improve this performance. Cursor
    /// position will be preserved, but all highlight regions will be cleared.
    pub fn filter_buf<F>(&mut self, func: F) where F: Fn(String) -> String {
        let s = func(self.as_string());
        let old_cursor = self.cursor;
        self.buf.clear();
        self.buf.extend(s.chars());
        self.set_cursor(old_cursor);
    }

    /// Check whether the cursor is at the end of the buffer
    pub fn at_end(&self) -> bool {
        self.cursor == self.buf.len()
    }

    /// Move the cursor relative to its current location
    /// 
    /// If the given offset would take the cursor outside the valid text region,
    /// the motion will be clamped to the end of the valid region. This is an
    /// O(1) operation.
    pub fn move_cursor(&mut self, offset: isize) {
        let new_pos =
            if offset < 0 { self.cursor.saturating_sub((-offset) as usize) }
            else { self.cursor.saturating_add(offset as usize) };
        self.set_cursor(new_pos);
    }

    /// Move the cursor to an absolute position
    ///
    /// If the position is out of bounds, it will be clamped to the bounds of
    /// the valid region. This is an O(1) operation.
    pub fn set_cursor(&mut self, pos: usize) {
        self.cursor =
            if pos > self.buf.len() { self.buf.len() }
            else { pos };
    }

    /// Get the current cursor position
    pub fn cursor(&self) -> usize { self.cursor }

    /// Insert a string at the cursor position
    /// 
    /// Highlighting regions will be preserved or modified (any region which
    /// contains the cursor position at which text is inserted will be expanded,
    /// and those outside it will be shifted) after this operation.
    /// 
    /// After insertion, the cursor will be moved to the end of the inserted
    /// string.
    pub fn insert<S: AsRef<str>>(&mut self, text: S) {
        let text_len = text.as_ref().chars().count();

        // handle the three cases: cursor at start, cursor in middle, and cursor
        // at end
        if self.cursor == 0 { // beginning
            // just prepend
            println!("at start");
            let mut v = Vec::with_capacity(self.buf.len() + text_len);
            v.extend(text.as_ref().chars());
            v.append(&mut self.buf);
            self.buf = v;
        } else if self.at_end() { // end
            // append into our vector
            println!("at end");
            self.buf.reserve_exact(text_len);
            self.buf.extend(text.as_ref().chars());
        } else { // middle
            // split our vector in half, append to the first, then reapply the
            // second part
            println!("at middle");
            let mut later = self.buf.split_off(self.cursor);
            self.buf.reserve_exact(later.len() + text_len);
            self.buf.extend(text.as_ref().chars());
            self.buf.append(&mut later);
        }
        self.cursor += text_len;
    }

    /// Insert one character at the cursor position
    /// 
    /// Uses the same semantics as `insert`, but can be implemented more
    /// efficiently than converting the char to a string.
    pub fn push(&mut self, c: char) {
        self.buf.insert(self.cursor, c);
        self.cursor += 1;
    }

    /// Delete a number of characters before the current cursor position and
    /// return them.
    /// 
    /// Highlighting regions will be preserved or modified according to the
    /// following rules:
    /// * Regions surrounding the deleted section will be shrunk
    /// * Regions whose start index is in the deleted section will have it moved
    ///   to the end of the deleted section.
    /// * Regions whose end index is in the deleted section will have it moved
    ///   to the start of the deleted section.
    /// * Regions outside the deleted section will have their indices shifted to
    ///   accomodate it.
    /// 
    /// After deletion, the cursor will be moved to the start of the deleted
    /// region. If there are less than `num` chars before the cursor, then all
    /// chars before the cursor will be deleted.
    pub fn delete(&mut self, num: usize) -> String {
        // cap the number of characters to delete
        let num = num.min(self.cursor);

        // avoid useless deletions at start of string
        if num == 0 { return String::new(); }

        let drain_iter = self.buf.drain((self.cursor - num)..self.cursor);
        let res = String::from_iter(drain_iter);

        self.cursor -= num;
        res
    }

    /// Delete a number of characters after the current cursor position and
    /// return them.
    /// 
    /// Highlighting regions will be preserved or modified according to the
    /// following rules:
    /// * Regions surrounding the deleted section will be shrunk
    /// * Regions whose start index is in the deleted section will have it moved
    ///   to the end of the deleted section.
    /// * Regions whose end index is in the deleted section will have it moved
    ///   to the start of the deleted section.
    /// * Regions outside the deleted section will have their indices shifted to
    ///   accomodate it.
    /// 
    /// After deletion, the cursor will be moved to the start of the deleted
    /// region. If there are less than `num` chars after the cursor, then all
    /// chars after the cursor will be deleted.
    pub fn delete_forward(&mut self, num: usize) -> String {
        // cap the number of characters to delete
        let num = num.min((self.buf.len()+1) - self.cursor);

        // avoid useless deletions at end of string
        if num == 0 { return String::new(); }

        let drain_iter = self.buf.drain(self.cursor..(self.cursor + num));
        let res = String::from_iter(drain_iter);

        res
    }

    /// Clear all editing state and reset the buffer to empty
    /// 
    /// Highlight regions and text will be deleted, and the cursor will be reset
    /// to location 0.
    pub fn clear(&mut self) {
        self.buf.truncate(0);
        self.cursor = 0;
    }

    /// Get a copy of the buffer contents
    /// 
    /// Note that this operation may allocate memory. For a zero-cost version
    /// which works in many cases, use the `view` method.
    pub fn as_string(&self) -> String {
        String::from_iter(self.buf.iter())
    }

    /// Get a view into the buffer contents
    pub fn view(&self) -> &[char] { self.buf.as_slice() }

    /// Get a mutable view into the buffer contents
    pub fn view_mut(&mut self) -> &mut [char] { self.buf.as_mut_slice() }
}

impl<T> From<T> for EditBuffer where T: AsRef<str> {
    fn from(s: T) -> EditBuffer {
        EditBuffer {
            buf: Vec::from_iter(s.as_ref().chars()),
            cursor: 0
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_buffer_cursor() {
        let mut b = EditBuffer::from("hello world");
        assert_eq!(b.cursor(), 0);
        b.set_cursor(500);
        assert_eq!(b.cursor(), 11);
        b.move_cursor(10);
        assert_eq!(b.cursor(), 11);
        b.move_cursor(-10);
        assert_eq!(b.cursor(), 1);
        b.move_cursor(-13);
        assert_eq!(b.cursor(), 0);
        b.move_cursor(4);
        assert_eq!(b.cursor(), 4);
    }

    #[test]
    fn test_buffer_insert() {
        let mut b = EditBuffer::new();

        // at start
        assert_eq!(&b.as_string(), "");
        assert_eq!(b.cursor(), 0);
        b.insert("he");
        assert_eq!(&b.as_string(), "he");
        assert_eq!(b.cursor(), 2);
        b.insert("llo ");
        assert_eq!(&b.as_string(), "hello ");
        assert_eq!(b.cursor(), 6);
        b.push('w');
        assert_eq!(&b.as_string(), "hello w");
        assert_eq!(b.cursor(), 7);
        b.insert("orld");
        assert_eq!(&b.as_string(), "hello world");
        assert_eq!(b.cursor(), 11);

        // in middle
        b.move_cursor(-6);
        b.insert(" test");
        assert_eq!(&b.as_string(), "hello test world");
        assert_eq!(b.cursor(), 10);
    }

    #[test]
    fn test_editing() {
        let mut b = EditBuffer::new();
        assert_eq!(&b.as_string(), "");
        assert_eq!(b.cursor(), 0);

        b.insert("a");
        assert_eq!(&b.as_string(), "a");
        assert_eq!(b.cursor(), 1);

        b.move_cursor(-2);
        assert_eq!(&b.as_string(), "a");
        assert_eq!(b.cursor(), 0);

        b.insert("b");
        assert_eq!(&b.as_string(), "ba");
        assert_eq!(b.cursor(), 1);

        b.move_cursor(10);
        assert_eq!(&b.as_string(), "ba");
        assert_eq!(b.cursor(), 2);

        b.insert("c");
        assert_eq!(&b.as_string(), "bac");
        assert_eq!(b.cursor(), 3);
    }
}
