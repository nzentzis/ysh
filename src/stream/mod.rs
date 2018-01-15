pub mod poly;

pub use self::poly::*;

use std::str;
use std::io;
use std::io::prelude::*;
use std::sync::{Arc, Mutex};
use std::cell::RefCell;

/// Reads a UTF-8 character from the given stream
/// 
/// # Errors
/// If this encounters an I/O error, it will fail with that result. If it finds
/// an EOF before the end of the character, it will ignore the read bytes and
/// return an `io::ErrorKind::UnexpectedEof`.
/// 
/// If the bytes aren't valid UTF-8, this will return an error of kind
/// `io::ErrorKind::InvalidData`.
fn read_utf8<R: Read + ?Sized>(strm: &mut R) -> io::Result<char> {
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
    fn peek(&self) -> Result<char, StreamError>;

    /// Consume a character from the input stream
    ///
    /// At EOF, fail with an error `UnexpectedEOF`
    fn next(&self) -> Result<char, StreamError>;

    /// Push a character back into the stream
    fn push(&self, c: char);
}

pub struct ReadWrapper<'a, R: Read + 'a> {
    peek: RefCell<Vec<char>>, // index 0 is the read point, new data goes at end
    strm: RefCell<&'a mut R>
}

impl<'a, R: Read> ReadWrapper<'a, R> {
    /// Build a new peek stream
    pub fn new(strm: &'a mut R) -> Self {
        ReadWrapper {
            peek: RefCell::new(Vec::new()),
            strm: RefCell::new(strm)
        }
    }

    /// Read a new character into the peek buffer
    fn make_avail(&self) -> io::Result<()> {
        let mut strm = self.strm.borrow_mut();
        let mut peek = self.peek.borrow_mut();

        let c = read_utf8(&mut *strm)?;
        peek.push(c);
        Ok(())
    }
}

impl<'a, R: Read> CharStream for ReadWrapper<'a, R> {
    /// Get the next character from the input stream
    fn peek(&self) -> Result<char, StreamError> {
        let is_empty = self.peek.borrow().is_empty();
        if is_empty { self.make_avail()?; }
        Ok(self.peek.borrow()[0])
    }

    /// Consume a character from the input stream
    fn next(&self) -> Result<char, StreamError> {
        let mut peek = self.peek.borrow_mut();
        if peek.is_empty() {
            let mut strm = self.strm.borrow_mut();
            Ok(read_utf8(&mut *strm)?)
        } else {
            Ok(peek.remove(0))
        }
    }

    /// Push a value back into the stream
    fn push(&self, c: char) {
        self.peek.borrow_mut().insert(0, c);
    }
}

trait ReadWrite : Read + Write {
}

impl<T: Read+Write> ReadWrite for T {}

#[derive(Clone)]
/// Opaque stream wrapper which can contain either `Read` or `Write` streams
enum StreamWrap {
    R(Arc<Mutex<Box<Read + Send>>>),
    W(Arc<Mutex<Box<Write + Send>>>),
    RW(Arc<Mutex<Box<ReadWrite + Send>>>)
}

impl StreamWrap {
    fn get_rw_mode(&self) -> (bool,bool) {
        match self {
            &StreamWrap::R(_)  => (true,false),
            &StreamWrap::W(_)  => (false,true),
            &StreamWrap::RW(_) => (true,true),
        }
    }
}

#[derive(Debug)]
pub enum StreamError {
    /// Failed due to IO error
    IO(io::Error),

    /// Operation denied due to stream mode
    ModeDenied,
}

impl ::std::fmt::Display for StreamError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            &StreamError::IO(ref e) => write!(f, "I/O error: {}", e),
            &StreamError::ModeDenied =>
                write!(f, "Stream does not support requested operation"),
        }
    }
}

impl ::std::error::Error for StreamError {
    fn description(&self) -> &str {
        match self {
            &StreamError::IO(_) => "I/O error",
            &StreamError::ModeDenied => "unsupported stream operation",
        }
    }
}

impl StreamError {
    /// Return the contained `io::Error`, if any
    pub fn io_err(self) -> Option<io::Error> {
        if let StreamError::IO(e) = self {Some(e)}
        else {None}
    }

    /// Check whether this error was an IO error of kind `UnexpectedEof`
    pub fn is_eof(&self) -> bool {
        if let &StreamError::IO(ref e) = self {
            e.kind() == io::ErrorKind::UnexpectedEof
        } else {
            false
        }
    }
}

impl From<io::Error> for StreamError {
    fn from(e: io::Error) -> StreamError { StreamError::IO(e) }
}

#[derive(Clone)]
pub struct StreamWrapper {
    strm: StreamWrap,
    buf: Arc<Mutex<Vec<char>>>
}

impl StreamWrapper {
    /// Return whether this stream can be read from
    pub fn is_readable(&self) -> bool { self.strm.get_rw_mode().0 }

    /// Return whether this stream can be written to
    pub fn is_writable(&self) -> bool { self.strm.get_rw_mode().1 }

    /// Create a read-only stream wrapper
    pub fn new_read<S: Read + Send + 'static>(r: S) -> Self {
        StreamWrapper {
            strm: StreamWrap::R(Arc::new(Mutex::new(Box::new(r)))),
            buf: Arc::new(Mutex::new(Vec::new()))
        }
    }

    /// Create a write-only stream wrapper
    pub fn new_write<S: Write + Send + 'static>(r: S) -> Self {
        StreamWrapper {
            strm: StreamWrap::W(Arc::new(Mutex::new(Box::new(r)))),
            buf: Arc::new(Mutex::new(Vec::new()))
        }
    }

    /// Create a read-only stream wrapper
    pub fn new_rw<S: Read + Write + Send + 'static>(r: S) -> Self {
        StreamWrapper {
            strm: StreamWrap::RW(Arc::new(Mutex::new(Box::new(r)))),
            buf: Arc::new(Mutex::new(Vec::new()))
        }
    }

    fn read_char(&self) -> Result<char, StreamError> {
        match self.strm {
            StreamWrap::R(ref s) => {
                let mut f = s.lock().unwrap();
                Ok(read_utf8(f.as_mut())?)
            },
            StreamWrap::RW(ref s) => {
                let mut f = s.lock().unwrap();
                Ok(read_utf8(f.as_mut())?)
            },
            _ => Err(StreamError::ModeDenied)
        }
    }
}

impl CharStream for StreamWrapper {
    fn peek(&self) -> Result<char, StreamError> {
        let mut buf = self.buf.lock().unwrap();
        if buf.is_empty() {
            let c = self.read_char()?;
            buf.push(c);
            Ok(c)
        } else {
            Ok(buf[0])
        }
    }

    fn next(&self) -> Result<char, StreamError> {
        let mut buf = self.buf.lock().unwrap();
        if buf.is_empty() { self.read_char() }
        else { Ok(buf.remove(0)) }
    }

    fn push(&self, c: char) {
        self.buf.lock().unwrap().insert(0, c);
    }
}
