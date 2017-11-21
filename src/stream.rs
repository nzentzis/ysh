use std::io;
use std::io::prelude::*;
use std::os::unix::prelude::*;
use std::sync::Arc;
use std::boxed::Box;

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

#[derive(Clone)]
/// An underlying polystream object accessible via the public PolyStream
/// interface.
/// 
/// This is wrapped in an Arc so the iterated objects can just reference the
/// content of the stream instead of making copies.
struct InnerStream {
    opts: StreamOptions,
    stream: LazyReadStream,

    /// The current frontmost span
    /// 
    /// If `None`, nothing has been read yet
    front: Option<Span>
}

impl InnerStream {
    /// Get the next line from this stream
    /// 
    /// Returns `None` when no further lines are available
    fn next_line(&mut self) -> io::Result<Option<PolyLine>> {
        let front = {
            if self.front.is_none() {
                self.front = Some(self.stream.read(100000)?);
            }
            self.front.as_ref().unwrap().to_owned()
        };

        let avail = front.clone();
        let avail_len = front.real_len();
        for (i,b) in avail.bytes().enumerate() {
            if b == b'\n' {
                // split here
                let new_span = avail.subspan(..i);

                // TODO: check that idx+1 is valid?
                if i+1 < avail_len {
                    self.front = Some(front.subspan(i+1..));
                } else {
                    self.front = Some(self.stream.read(512)?);
                }
                return Ok(Some(PolyLine::new_from(new_span, self.opts.clone())));
            }
        }

        if front.is_frozen() {
            return Ok(None);
        }

        // get more data and recurse
        self.stream.extend(self.front.as_mut().unwrap(), 512)?;
        self.next_line()
    }
}

pub struct PolyStream {
    inner: InnerStream
}

impl PolyStream {
    /// Create a stream to parse and process the given input stream
    /// 
    /// This requires that the PolyStream be the sole owner of the given file
    /// descriptor.
    /// 
    /// It's not suitable for pulling data from stdin.
    pub fn from_fd(fd: RawFd, opts: StreamOptions) -> io::Result<Self> {
        let mut strm = LazyReadStream::new(fd)?;
        Ok(PolyStream {
            inner: InnerStream {
                opts, front: None,
                stream: strm,
            }
        })
    }

    /// Open a stream from stdin
    pub fn from_stdin(opts: StreamOptions) -> io::Result<Self> {
        let mut strm = LazyReadStream::stdin()?;
        Ok(PolyStream {
            inner: InnerStream {
                opts, front: None,
                stream: strm
            }
        })
    }
}

struct LineIterator {
    stream: InnerStream
}

impl Iterator for LineIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        self.stream.next_line()
                   .expect("I/O read error") // TODO: handle
                   .map(Value::new)
    }
}

impl ValueLike for PolyStream {
    fn into_seq(&self) -> Vec<Value> { self.into_iter().collect() }

    fn into_iter(&self) -> ValueIteratorBox {
        Box::new(LineIterator { stream: self.inner.clone() })
    }

    fn into_str(&self) -> String {
        unimplemented!()
    }

    fn into_args(&self) -> Vec<String> {
        unimplemented!()
    }

    fn first(&self) -> Option<&ValueLike> {
        unimplemented!()
    }
}

#[derive(Clone)]
/// A line from a polymorphic stream. Handles field conversion and acts as a
/// list-like object.
pub struct PolyLine {
    opts: StreamOptions,
    data: Span,
    fields: Vec<()>
}

impl PolyLine {
    fn new_from(span: Span, opts: StreamOptions) -> PolyLine {
        PolyLine {
            opts,
            data: span,
            fields: Vec::new()
        }
    }
}

impl ValueLike for PolyLine {
    fn into_seq(&self) -> Vec<Value> { self.into_iter().collect() }

    fn into_iter(&self) -> ValueIteratorBox {
        unimplemented!()
    }

    fn into_str(&self) -> String {
        let x = self.data.copy(..);
        String::from_utf8_lossy(x.as_slice()).into_owned()
    }

    fn into_args(&self) -> Vec<String> {
        unimplemented!()
    }

    fn first(&self) -> Option<&ValueLike> {
        unimplemented!()
    }
}
