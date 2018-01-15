use std::io;
use std::os::unix::prelude::*;
use std::boxed::Box;

use span::*;
use data::*;
use numeric::*;
use super::{CharStream, StreamError};

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
        let strm = LazyReadStream::new(fd)?;
        Ok(PolyStream {
            inner: InnerStream {
                opts, front: None,
                stream: strm,
            }
        })
    }

    /// Open a stream from stdin
    pub fn from_stdin(opts: StreamOptions) -> io::Result<Self> {
        let strm = LazyReadStream::stdin()?;
        Ok(PolyStream {
            inner: InnerStream {
                opts, front: None,
                stream: strm
            }
        })
    }
}

impl CharStream for PolyStream {
    fn peek(&self) -> Result<char, StreamError> {
        unimplemented!()
    }

    fn next(&self) -> Result<char, StreamError> {
        unimplemented!()
    }

    fn push(&self, c: char) {
        unimplemented!()
    }
}

struct LineIterator {
    stream: InnerStream
}

impl Iterator for LineIterator {
    type Item = Eval<Value>;

    fn next(&mut self) -> Option<Eval<Value>> {
        match self.stream.next_line() {
            Ok(r) => r.map(Value::new).map(Ok),
            Err(e) => Some(Err(EvalError::IO(e)))
        }
    }
}

impl ValueLike for PolyStream {
    fn into_seq(&self) -> Eval<Vec<Value>> { self.into_iter().collect() }

    fn into_iter(&self) -> ValueIteratorBox {
        Box::new(LineIterator { stream: self.inner.clone() })
    }

    fn evaluate(&self, _env: &::environment::Environment) -> EvalResult {
        Ok(Value::new(PolyStream { inner: self.inner.clone() }))
    }

    fn into_str(&self) -> Eval<String> {
        Ok(String::from("<polystream>"))
    }

    fn into_args(&self) -> Eval<Vec<String>> {
        unimplemented!()
    }

    fn first(&self) -> Eval<Option<Value>> {
        unimplemented!()
    }
}

#[derive(Clone)]
/// A line from a polymorphic stream. Handles field conversion and acts as a
/// list-like object.
pub struct PolyLine {
    opts: StreamOptions,
    data: Span,
    fields: Vec<PolyField>
}

impl PolyLine {
    fn new_from(span: Span, opts: StreamOptions) -> PolyLine {
        #[derive(Debug, PartialEq, Eq, Copy, Clone)]
        enum FieldSplitState {
            Start, // start state (space-separated only)
            LeftBlanks,
            ReadData,
            RightBlanks,
            CheckSep, // separator check state (space-separated only)
        }

        let mut field_regions = Vec::new();

        let sep = opts.delimiter.unwrap_or(' ');
        if sep == ' ' {
            let mut state = FieldSplitState::Start;
            let mut start_idx = 0;
            let mut lspace = 0;
            let mut rspace = 0;
            let mut length = 0;

            let mut bias = false; // whether to bias to left alignment

            // split on spaces
            let mut last_idx = 0;
            for (i,c) in span.chars().enumerate() {
                last_idx = i;
                match state {
                    FieldSplitState::Start => {
                        if c == ' ' {
                            // if the first char is a space, bias left
                            bias = true;
                            lspace += 1;
                            state = FieldSplitState::LeftBlanks;
                        } else {
                            state = FieldSplitState::ReadData;
                            length += 1;
                        }
                    },
                    FieldSplitState::LeftBlanks => {
                        if c == ' ' {
                            lspace += 1;
                        } else {
                            length += 1;
                            state = FieldSplitState::ReadData;
                        }
                    },
                    FieldSplitState::ReadData => {
                        if c == ' ' {
                            state = FieldSplitState::CheckSep;
                        } else { // grab more body
                            length += 1;
                        }
                    },
                    FieldSplitState::CheckSep => {
                        if c != ' ' {
                            // end field
                            field_regions.push((start_idx,
                                                lspace,
                                                length,
                                                rspace,
                                                i-1));
                            start_idx = i;
                            lspace = 0; rspace = 0; length = 1;
                            state = FieldSplitState::ReadData;
                        } else {
                            // if biased left, break immediately, but if we're
                            // biasing toward right alignment then we have to
                            // consume spaces
                            if bias {
                                field_regions.push((start_idx,
                                                    lspace,
                                                    length,
                                                    rspace,
                                                    i-1));
                                start_idx = i+1;
                                lspace = 0; rspace = 0; length = 0;
                                state = FieldSplitState::LeftBlanks;
                            } else {
                                state = FieldSplitState::RightBlanks;
                                rspace += 1;
                                // rspace should be 1 less than normal since it
                                // needs to account for the next delimiter
                                // but the extra space is already dropped when
                                // transitioning to this state.
                            }
                        }
                    },
                    FieldSplitState::RightBlanks => {
                        if c == ' ' {
                            rspace += 1;
                        } else {
                            // end field
                            field_regions.push((start_idx,
                                                lspace,
                                                length,
                                                rspace,
                                                i-1));
                            start_idx = i;
                            lspace = 0; length = 0; rspace = 0;
                            state = FieldSplitState::ReadData;
                        }
                    },
                }
            }
            let end_idx = match state {
                FieldSplitState::CheckSep => last_idx,
                _ => last_idx+1,
            };
            field_regions.push((start_idx, lspace, length, rspace, end_idx));
        } else {
            let mut state = FieldSplitState::LeftBlanks;
            let mut start_idx = 0;
            let mut end_idx;
            let mut lspace = 0;
            let mut rspace = 0;
            let mut length = 0;

            // split on delimiter
            let mut last_idx = 0;
            for (i,c) in span.chars().enumerate() {
                last_idx = i;
                match state {
                    FieldSplitState::LeftBlanks => {
                        if c == sep {
                            state = FieldSplitState::LeftBlanks;
                            end_idx = i;
                        } else if c != ' ' {
                            start_idx = i;
                            length = 1;
                            state = FieldSplitState::ReadData;
                            continue;
                        } else {
                            lspace += 1;
                            continue;
                        }
                    },
                    FieldSplitState::ReadData => {
                        if c == sep {
                            state = FieldSplitState::LeftBlanks;
                            end_idx = i;
                        } else if c == ' ' {
                            state = FieldSplitState::RightBlanks;
                            rspace += 1;
                            continue;
                        } else {
                            length += 1;
                            continue;
                        }
                    },
                    FieldSplitState::RightBlanks => {
                        if c == sep {
                            state = FieldSplitState::LeftBlanks;
                            end_idx = i;
                        } else if c == ' ' {
                            rspace += 1;
                            continue;
                        } else {
                            // more body data - go back and adjust
                            length += rspace + 1;
                            rspace = 0;
                            state = FieldSplitState::ReadData;
                            continue;
                        }
                    },
                    _ => panic!("invalid FSM state for separator mode")
                }

                // end of field
                field_regions.push((start_idx, lspace, length, rspace, end_idx));

                // set up for next
                start_idx = i;
                length = 0;
                lspace = 0;
                rspace = 0;
            }

            field_regions.push((start_idx, lspace, length, rspace, last_idx+1));
        }

        let fields: Vec<_> = field_regions.into_iter()
            .map(|(start,l,_len,r,end)| {
                let data =
                    if (end-r) > span.real_len() { span.subspan(start+l..) }
                    else { span.subspan(start+l..end-r) };
                PolyField {
                    data,
                    info: FieldInfo {
                        start: start,
                        length: end-start,
                        pad: ' ',
                        align: if l > 0 { Alignment::Left }
                               else { Alignment::Right }
                    }
                }
            })
            .collect();

        PolyLine {
            opts,
            data: span,
            fields
        }
    }
}

impl ValueLike for PolyLine {
    fn into_iter(&self) -> ValueIteratorBox {
        Box::new(self.fields.clone().into_iter().map(Value::new).map(Ok))
    }

    fn evaluate(&self, _env: &::environment::Environment) -> EvalResult {
        Ok(Value::new(self.clone()))
    }

    fn into_str(&self) -> Eval<String> {
        let x = self.data.copy(..);
        Ok(String::from_utf8_lossy(x.as_slice()).into_owned())
    }

    fn first(&self) -> Eval<Option<Value>> {
        unimplemented!()
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Alignment {
    Left, // left align
    Right, // right align
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct FieldInfo {
    align: Alignment,
    pad: char,
    start: usize, // field start column
    length: usize // length in columns
}

#[derive(Clone)]
/// A field from a polymorphic stream. Handles conversion to/from other types
/// and acts as a (number/string)-like object
/// 
/// This object is *unfrozen*, meaning that it can be used as any type to which
/// it has a defined conversion. Once the conversion happens, the result will be
/// frozen and type-checked normally.
/// 
/// In other words, for two polyfields `x` and `y` the forms `(+ x y)` and
/// `(str/strip x)` will both succeed, but `(+ (chars x) y)` would fail since
/// the result of `(str/strip x)` would be a string rather than a polymorphic
/// object.
pub struct PolyField {
    data: Span,
    info: FieldInfo
}

impl ValueLike for PolyField {
    fn into_iter(&self) -> ValueIteratorBox {
        Box::new(Value::new(self.to_owned()).into_iter())
    }

    fn evaluate(&self, _env: &::environment::Environment) -> EvalResult {
        Ok(Value::new(self.to_owned()))
    }

    fn into_str(&self) -> Eval<String> {
        let x = self.data.copy(..);
        Ok(String::from_utf8_lossy(x.as_slice()).into_owned())
    }

    fn into_num(&self) -> Eval<Option<Number>> {
        use reader::parse_number;

        // try parsing as a number
        let data = self.data.copy(..);
        let n = parse_number(data.as_slice());

        if let Ok(n) = n {
            Ok(Some(n))
        } else {
            Ok(None)
        }
    }

    fn first(&self) -> Eval<Option<Value>> {
        Ok(Some(Value::new(self.to_owned())))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    fn mk_stream(data: &'static [u8]) -> LazyReadStream {
        LazyReadStream::from_reader(Cursor::new(data)).unwrap()
    }

    #[test]
    fn line_simple() {
        let l_data = "1234 abcd 1.24 6-7i ";
        let mut strm = mk_stream(l_data.as_bytes());

        let mut s1 = strm.read(26).unwrap();
        while !s1.is_frozen() { strm.extend(&mut s1, 8).unwrap(); }

        let opts = StreamOptions::new();
        let l = PolyLine::new_from(s1, opts);

        assert_eq!(l.into_str().unwrap(), l_data);
        assert_eq!(l.into_iter()
                    .map(|x| x.and_then(|v| v.into_str()))
                    .collect::<Eval<Vec<_>>>().unwrap(),
                    vec!["1234", "abcd", "1.24", "6-7i"]);
    }

    #[test]
    fn line_left_padded() {
        let l_data = "    1234    abcd    1.24    6-7i ";
        let mut strm = mk_stream(l_data.as_bytes());

        let mut s1 = strm.read(26).unwrap();
        while !s1.is_frozen() { strm.extend(&mut s1, 8).unwrap(); }

        let opts = StreamOptions::new();
        let l = PolyLine::new_from(s1, opts);

        assert_eq!(l.into_str().unwrap(), l_data);
        assert_eq!(l.into_iter()
                    .map(|x| x.and_then(|v| v.into_str()))
                    .collect::<Eval<Vec<_>>>().unwrap(),
                    vec!["1234", "abcd", "1.24", "6-7i"]);
    }

    #[test]
    fn line_right_padded() {
        let l_data = "1234    abcd    1.24    6-7i";
        let mut strm = mk_stream(l_data.as_bytes());

        let mut s1 = strm.read(26).unwrap();
        while !s1.is_frozen() { strm.extend(&mut s1, 8).unwrap(); }

        let opts = StreamOptions::new();
        let l = PolyLine::new_from(s1, opts);

        assert_eq!(l.into_str().unwrap(), l_data);
        assert_eq!(l.into_iter()
                    .map(|x| x.and_then(|v| v.into_str()))
                    .collect::<Eval<Vec<_>>>().unwrap(),
                    vec!["1234", "abcd", "1.24", "6-7i"]);
    }

    #[test]
    fn separated_simple() {
        let l_data = "1234|abcd|1.24|6-7i";
        let mut strm = mk_stream(l_data.as_bytes());

        let mut s1 = strm.read(26).unwrap();
        while !s1.is_frozen() { strm.extend(&mut s1, 8).unwrap(); }

        let mut opts = StreamOptions::new();
        opts.delimiter('|');
        let l = PolyLine::new_from(s1, opts);

        assert_eq!(l.into_str().unwrap(), l_data);
        assert_eq!(l.into_iter()
                    .map(|x| x.and_then(|v| v.into_str()))
                    .collect::<Eval<Vec<_>>>().unwrap(),
                    vec!["1234", "abcd", "1.24", "6-7i"]);
    }
}

