use std::io;
use std::io::prelude::*;
use std::os::unix::prelude::*;
use std::sync::Arc;

use span::*;
use data::*;


#[derive(Clone, Copy, Debug)]
/// Options structure to use when generating a new polymorphic stream structure.
pub struct StreamOptions {
    /// specific character to use as a delimiter
    delimiter: Option<char>,
    
    /// split lines
    lines: bool,
    
    /// split fields
    fields: bool,
}

impl StreamOptions {
    /// Generate a new options structure
    pub fn new() -> Self {
        StreamOptions {
            delimiter: None,
            lines: true,
            fields: true
        }
    }

    /// Generate a new options structure representing "basic" mode; no parsing
    /// at all.
    pub fn basic() -> Self {
        StreamOptions {
            delimiter: None,
            lines: false,
            fields: false
        }
    }

    /// Set whether to split lines when parsing the stream
    pub fn lines(&mut self, enable: bool) -> &mut Self {
        self.lines = enable;
        self
    }

    /// Set the delimiter to use
    pub fn delimiter(&mut self, delim: char) -> &mut Self {
        self.delimiter = Some(delim);
        self
    }
}

/// An underlying polystream object accessible via the public PolyStream
/// interface.
/// 
/// This is wrapped in an Arc so the iterated objects can just reference the
/// content of the stream instead of making copies.
struct InnerStream {
    opts: StreamOptions,
    stream: LazyReadStream
}

impl InnerStream {
    /// Get the next line from this stream
    fn next_line(&self) -> PolyLine {
        unimplemented!()
    }
}

pub struct PolyStream {
    inner: Arc<InnerStream>
}

impl PolyStream {
    /// Create a stream to parse and process the given input stream
    /// 
    /// This requires that the PolyStream be the sole owner of the given file
    /// descriptor.
    /// 
    /// It's not suitable for pulling data from stdin.
    pub fn from_fd(fd: RawFd, opts: StreamOptions) -> io::Result<Self> {
        Ok(PolyStream {
            inner: Arc::new(InnerStream {
                opts,
                stream: LazyReadStream::new(fd)?
            })
        })
    }

    /// Open a stream from stdin
    pub fn from_stdin(opts: StreamOptions) -> io::Result<Self> {
        Ok(PolyStream {
            inner: Arc::new(InnerStream {
                opts,
                stream: LazyReadStream::stdin()?
            })
        })
    }
}

struct LineIterator {
    stream: Arc<InnerStream>
}

impl Iterator for LineIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        unimplemented!()
    }
}

impl ValueLike for PolyStream {
    // simple stuff
    fn is_executable(&self) -> bool {false}
    fn execute(&self, args: Vec<Value>) -> Result<Value, Vec<Value>> {Err(args)}

    fn into_seq(&self) -> Vec<Value> { self.into_iter().collect() }

    fn into_iter(&self) -> ValueIteratorBox {
        unimplemented!()
    }

    fn into_str(&self) -> String {
        unimplemented!()
    }

    fn into_args(&self) -> Vec<String> {
        unimplemented!()
    }

    fn first(&self) -> Option<&Value> {
        unimplemented!()
    }
}

/// A line from a polymorphic stream. Handles field conversion and acts as a
/// list-like object.
pub struct PolyLine {
    stream: Arc<InnerStream>,
    start: StreamPoint,
    end: StreamPoint
}

impl PolyLine {
}
