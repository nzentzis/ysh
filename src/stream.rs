use std::io;
use std::io::prelude::*;
use std::os::unix::prelude::*;

use std::fs::File;

use data::*;

/// Input stream that lazily reads chunks from an underlying file descriptor
enum LazyReadStream {
    FromFile(File),
    FromChannel(io::Stdin),
}

impl LazyReadStream {
    /// Create a read stream for the given raw FD
    /// 
    /// This must be granted exclusive ownership of the passed file descriptor.
    fn new(f: RawFd) -> Self {
        LazyReadStream::FromFile(unsafe { File::from_raw_fd(f) })
    }

    /// Create a read stream for stdin
    fn stdin() -> Self {
        LazyReadStream::FromChannel(io::stdin())
    }
}

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

pub struct PolyStream {
    opts: StreamOptions,
    stream: LazyReadStream
}

impl PolyStream {
    /// Create a stream to parse and process the given input stream
    /// 
    /// This requires that the PolyStream be the sole owner of the given file
    /// descriptor.
    /// 
    /// It's not suitable for pulling data from stdin.
    pub fn from_fd(fd: RawFd, opts: StreamOptions) -> Self {
        PolyStream {
            opts,
            stream: LazyReadStream::new(fd)
        }
    }

    /// Open a stream from stdin
    pub fn from_stdin(opts: StreamOptions) -> Self {
        PolyStream {
            opts,
            stream: LazyReadStream::stdin()
        }
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
