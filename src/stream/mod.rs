pub mod poly;

pub use self::poly::*;

use std::str;
use std::io;
use std::io::prelude::*;

/// Reads a UTF-8 character from the given stream
/// 
/// # Errors
/// If this encounters an I/O error, it will fail with that result. If it finds
/// an EOF before the end of the character, it will ignore the read bytes and
/// return an `io::ErrorKind::UnexpectedEof`.
/// 
/// If the bytes aren't valid UTF-8, this will return an error of kind
/// `io::ErrorKind::InvalidData`.
fn read_utf8<R: Read>(strm: &mut R) -> io::Result<char> {
    let mut buf = [0u8;4];

    strm.read_exact(&mut buf[..1])?;

    let num_ones = {
        let mut n = 0;
        let b = buf[0];
        for i in 0..3 {
            // check that the bit N from the left is one
            if (b >> (7 - i)) & 1 == 0 { break }

            n += 1;
        }

        // double check that it's valid
        if (b >> (7 - n)) & 1 != 0 {
            return Err(io::Error::new(io::ErrorKind::InvalidData,
                       "encountered invalid UTF-8 sequence"));
        }

        n
    };

    // get enough data
    let needed = if num_ones == 0 { 0 } else { num_ones-1 };
    strm.read_exact(&mut buf[1..needed+1])?;

    if let Ok(s) = str::from_utf8(&buf[..needed+1]) {
        if let Some(r) = s.chars().next() { Ok(r) }
        else { Err(io::Error::new(io::ErrorKind::InvalidData,
                       "encountered invalid UTF-8 sequence")) }
    } else {
        Err(io::Error::new(io::ErrorKind::InvalidData,
                       "encountered invalid UTF-8 sequence"))
    }
}

/// Basic readable char-stream trait
pub trait CharStream {
    /// Get the next character from the stream without consuming it
    ///
    /// At EOF, fail with an error `UnexpectedEOF`
    fn peek(&mut self) -> io::Result<char>;

    /// Consume a character from the input stream
    ///
    /// At EOF, fail with an error `UnexpectedEOF`
    fn next(&mut self) -> io::Result<char>;

    /// Push a character back into the stream
    fn push(&mut self, c: char);
}

pub struct ReadWrapper<'a, R: Read + 'a> {
    peek: Vec<char>, // index 0 is the read point, new data goes at end
    strm: &'a mut R
}

impl<'a, R: Read> ReadWrapper<'a, R> {
    /// Build a new peek stream
    pub fn new(strm: &'a mut R) -> Self {
        ReadWrapper { peek: Vec::new(), strm }
    }

    /// Read a new character into the peek buffer
    fn make_avail(&mut self) -> io::Result<()> {
        let c = read_utf8(&mut self.strm)?;
        self.peek.push(c);
        Ok(())
    }
}

impl<'a, R: Read> CharStream for ReadWrapper<'a, R> {
    /// Get the next character from the input stream
    fn peek(&mut self) -> io::Result<char> {
        if self.peek.is_empty() { self.make_avail()?; }
        Ok(self.peek[0])
    }

    /// Consume a character from the input stream
    fn next(&mut self) -> io::Result<char> {
        if self.peek.is_empty() { read_utf8(&mut self.strm) }
        else { Ok(self.peek.remove(0)) }
    }

    /// Push a value back into the stream
    fn push(&mut self, c: char) {
        self.peek.insert(0, c);
    }
}
