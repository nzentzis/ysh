use std::io;
use std::io::prelude::*;
use std::os::unix::prelude::*;

use std::io::ErrorKind;
use std::sync::{Arc, Weak, Mutex, RwLock};
use std::cell::Cell;
use std::cmp::Ordering;
use std::ops::{Range, RangeFrom, RangeTo, RangeFull, Index};

use std::fs::File;

const MAX_CHUNK: usize = 8192;

/// Chunk of an input stream
/// 
/// This is the innermost structure which holds data. Everything else references
/// chunks, but the chunks themselves are independent of one another. They need
/// to be as small as possible, and weak references take extra space.
struct Chunk {
    /// Offset in bytes from the stream start to the start of this chunk
    /// 
    /// Data always extends towards higher offsets from this index
    start: usize,

    /// Content bytes
    data: Box<[u8]>
}

impl Chunk {
    /// Generate a new chunk read from the given stream
    /// 
    /// Uses the passed values for prev and start, and increments the offset
    /// value with however much it read from the stream.
    fn new_from_read<R: Read>(input: &mut R,
                              offset: &mut usize) -> io::Result<Self> {
        let mut buf = vec![0; MAX_CHUNK];
        let r = loop {
            match input.read(&mut buf) {
                Ok(r) => break r,
                Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => return Err(e)
            }
        };

        let start = *offset;
        *offset += r;
        buf.truncate(r);
        buf.shrink_to_fit();
        Ok(Chunk { start, data: buf.into_boxed_slice() })
    }
}

/// Stream chunk reference
/// 
/// Since multiple copies of the stream need to strongly reference the subsets
/// of the chunk chain in front of them but the chunks themselves don't need to
/// reference each other, the stream references go through this structure.
struct StreamChunk {
    /// Next stream chunk
    next: Mutex<Option<Arc<StreamChunk>>>,

    /// Data for this chunk
    chunk: Arc<Chunk>
}

impl StreamChunk {
    fn new_from_read<R: Read>(input: &mut R, offset: &mut usize)
            -> io::Result<Self> {
        let chunk = Chunk::new_from_read(input, offset)?;
        Ok(StreamChunk {
            next: Mutex::new(None),
            chunk: Arc::new(chunk)
        })
    }

    /// Add another stream chunk to this one if it's the head of the chain
    fn extend(&mut self, other: &Arc<StreamChunk>) {
        let mut n = self.next.lock().unwrap();
        if n.is_none() {
            *n = Some(Arc::clone(other));
        }
    }

    /// Advance to the next chunk, reading from a stream if necessary
    /// 
    /// For chunks which are in the past, this won't read at all; it'll just
    /// advance to the next chunk.
    fn advance(&self, source: &mut ReadSource) -> io::Result<Arc<StreamChunk>> {
        let mut w = self.next.lock().unwrap();
        let present = w.is_some();
        if present { Ok(Arc::clone(w.as_ref().unwrap())) }
        else {
            let new = Arc::new(source.read_chunk()?);
            *w = Some(Arc::clone(&new));
            Ok(new)
        }
    }
}

pub enum ReadSource {
    FromFile {
        f: File,
        offs: usize
    },
    FromChannel {
        f: io::Stdin,
        offs: usize
    },
}

impl ReadSource {
    /// Create a read stream for the given raw FD
    /// 
    /// This must be granted exclusive ownership of the passed file descriptor.
    pub fn new(f: RawFd) -> Self {
        ReadSource::FromFile{
            f: unsafe { File::from_raw_fd(f) },
            offs: 0
        }
    }

    /// Create a read stream for stdin
    pub fn stdin() -> Self {
        ReadSource::FromChannel {
            f: io::stdin(),
            offs: 0
        }
    }

    /// Read a chunk from the stream with a given "previous" pointer
    fn read_chunk(&mut self) -> io::Result<StreamChunk> {
        match self {
            &mut ReadSource::FromFile {ref mut f, ref mut offs} =>
                StreamChunk::new_from_read(f, offs),
            &mut ReadSource::FromChannel {ref mut f, ref mut offs} =>
                StreamChunk::new_from_read(f, offs)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
/// A location value in a lazy stream. Used to store start and end points
/// of lines and fields.
/// 
/// For lines/fields which haven't yet seen their terminal character, the
/// Unknown value denotes a point which is somewhere further into the stream and
/// whose location isn't yet known.
pub enum StreamPoint {
    Past(usize),
    Future
}

impl PartialOrd for StreamPoint {
    fn partial_cmp(&self, other: &StreamPoint) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StreamPoint {
    fn cmp(&self, other: &StreamPoint) -> Ordering {
        match (self,other) {
            (&StreamPoint::Future,  &StreamPoint::Future)  => Ordering::Equal,
            (&StreamPoint::Past(_), &StreamPoint::Future)  => Ordering::Less,
            (&StreamPoint::Future,  &StreamPoint::Past(_)) => Ordering::Greater,
            (&StreamPoint::Past(a), &StreamPoint::Past(b)) => a.cmp(&b),
        }
    }
}

/// Iterator over the bytes in a span
pub struct Bytes<'a> {
    span: &'a Span,

    // which chunk in the span we're referencing
    chunk: Arc<Chunk>,
    chunk_idx: usize,

    // position in the current chunk
    local_pos: usize,

    // absolute stream position
    pos: usize
}

impl<'a> Iterator for Bytes<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        // advance if the current chunk is empty
        if self.local_pos == self.chunk.data.len() {
            self.chunk_idx += 1;
            if self.chunk_idx == self.span.chunks.len() {
                // make sure we take the same path if called again
                self.chunk_idx -= 1;

                // out of data!
                return None;
            }

            self.chunk = Arc::clone(&self.span.chunks[self.chunk_idx]);
            self.local_pos = 0;
        }

        match self.span.end {
            StreamPoint::Past(s) => if s == self.pos { return None; }
            StreamPoint::Future  => {}
        }

        let res = Some(self.chunk.data[self.local_pos]);
        self.local_pos += 1;
        self.pos += 1;
        res
    }
}

// TODO: more efficient char iterator

/// Iterator that incrementally decodes the characters in a span as UTF-8
/// 
/// Undecodable characters will be replaced with U+FFFD REPLACEMENT CHARACTER.
pub struct Chars<'a> {
    bytes: Bytes<'a>,
    buf: [u8; 4], // incremental buffer for UTF-8 chars
    ptr: usize // current write index into buffer
}

impl<'a> Iterator for Chars<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        use std::str;

        loop {
            let b = self.bytes.next();
            if b.is_none() { break None; }

            self.buf[self.ptr] = b.unwrap();

            match str::from_utf8(&self.buf[..]) {
                Ok(s) => {
                    self.ptr = 0;
                    break Some(s.chars().next().unwrap())
                },
                Err(e) => {
                    let elen = e.error_len();
                    if let Some(len) = elen {
                        // replace the garbage and continue
                        let bad_suffix_len = self.ptr - e.valid_up_to();
                        let to_strip = len - bad_suffix_len;
                        for i in 0..to_strip { self.bytes.next(); }
                        self.ptr = e.valid_up_to();
                        break Some('\u{FFFD}');
                    } else {
                        // we might just need more data
                        self.ptr += 1;
                        continue;
                    }
                }
            }
        }
    }
}

#[derive(Clone)]
/// External reference to a region of chunks
/// 
/// This object will keep the given section of the stream in memory as long as
/// it's alive.
pub struct Span {
    start: StreamPoint, // the first position included in the span
    end: StreamPoint, // the last position included in the span

    // chunks sorted by start index
    chunks: Vec<Arc<Chunk>>
}

impl Span {
    /// Check whether two spans overlap
    pub fn intersects(&self, other: &Span) -> bool {
        let a_start = self.chunks[0].start;
        let a_end = {
            let l = self.chunks.last().unwrap();
            l.start + l.data.len()
        };

        let b_start = other.chunks[0].start;
        let b_end = {
            let l = other.chunks.last().unwrap();
            l.start + l.data.len()
        };

        (a_start > b_start && a_start < b_end) ||
        (a_end > b_start && a_end < b_end) ||
        (b_start > a_start && b_start < a_end) ||
        (b_end > a_start && b_end < a_end)
    }

    /// Merge two contiguous or overlapping spans together
    /// 
    /// Consumes the passed span and adds its referenced region to the region
    /// contained in this span. The input span's chunks must overlap or be
    /// immediately adjacent to those of this one.
    /// 
    /// If the passed span doesn't meet these conditions, the union will fail.
    /// 
    /// The resulting span will begin at the lower of the two start points and
    /// end at the higher of the two start points (where `Future` is higher than
    /// any `Past` point).
    /// 
    /// # Errors
    /// 
    /// On failure, this returns ownership of the other span back to the caller
    pub fn union(&mut self, mut other: Span) -> Result<(), Span> {
        // coordinates are error-prone, so check by traversing the chunk chain
        // instead
         if self.intersects(&other) {
            return Err(other);
        }

        // merge chunk arrays
        let mut new_chunks = Vec::with_capacity(self.chunks.len() +
                                                other.chunks.len());
        {
            let mut i = other.chunks.drain(..);
            let mut j = self.chunks.drain(..);

            let mut a = i.next();
            let mut b = j.next();

            while a.is_some() || b.is_some() {
                if a.is_some() && b.is_some() {
                    let x = a.unwrap();
                    let y = b.unwrap();

                    if Arc::ptr_eq(&x, &y) {
                        new_chunks.push(x);

                        // drop y, advance to next; since they're duplicates we
                        // only need one
                        a = i.next();
                        b = j.next();
                    } else if x.start < y.start {
                        new_chunks.push(x);
                        a = i.next();
                        b = Some(y);
                    } else { // y.start < x.start
                        new_chunks.push(y);
                        a = Some(x);
                        b = j.next();
                    }
                }
            }
        }

        // adjust ourselves
        self.start = other.start.min(self.start);
        self.end = other.end.max(self.end);
        self.chunks = new_chunks;

        Ok(())
    }

    pub fn start_pos(&self) -> usize {
        if let StreamPoint::Past(s) = self.start {
            s
        } else {
            panic!("span start point not in past")
        }
    }

    /// Check whether the span is known to be empty
    /// 
    /// This will return false if the span ends with `Future`.
    pub fn is_empty(&self) -> bool {
        match (self.start, self.end) {
            (StreamPoint::Past(a),StreamPoint::Past(b)) if a == b   => true,
            _                                                       => false
        }
    }

    /// Get the length of this span, if known
    /// 
    /// If the span ends on `Future`, this will return `None` since the actual
    /// length is indeterminate.
    pub fn len(&self) -> Option<usize> {
        match (self.start, self.end) {
            (StreamPoint::Past(a),StreamPoint::Past(b)) => Some(b-a),
            _                                           => None
        }
    }

    /// Get the populated length of this span
    /// 
    /// This will return the amount of data in the span that can be accessed
    /// directly without reading from a stream.
    pub fn real_len(&self) -> usize {
        if let Some(x) = self.len() { return x; }

        let start = self.start_pos();
        let first_start = self.chunks[0].start;
        let chunks_len: usize = self.chunks.iter().map(|c| c.data.len()).sum();
        chunks_len - (start - first_start)
    }

    /// Get a subregion of the span as a slice if possible
    /// 
    /// Since spans may not be stored contiguously in memory, this operation may
    /// not be possible. This function is provded purely to allow other code to
    /// optimize the case where a region is contiguous.
    /// 
    /// # Panics
    /// This will panic if the span cannot possibly contain the given range
    pub fn try_slice(&self, range: Range<usize>) -> Option<&[u8]> {
        // TODO: implement this properly
        None
    }

    /// Freeze the end of the span where it is, even if more data could
    /// otherwise be added
    pub fn freeze(&mut self) {
        self.end = StreamPoint::Past(self.start_pos() + self.real_len());
    }

    /// Check whether the span is frozen
    pub fn is_frozen(&self) -> bool {
        match &self.end {
            &StreamPoint::Future  => false,
            &StreamPoint::Past(_) => true,
        }
    }

    /// Iterate over bytes in the span
    pub fn bytes(&self) -> Bytes {
        Bytes {
            span: self,
            chunk: Arc::clone(&self.chunks[0]),
            chunk_idx: 0,
            local_pos: self.start_pos() - self.chunks[0].start,
            pos: self.start_pos()
        }
    }

    /// Iterate over the characters in the span
    /// 
    /// Invalid characters will be replaced with U+FFFD REPLACEMENT CHARACTER.
    /// If the span ends in the middle of a character, the final invalid
    /// seequence will be dropped.
    pub fn chars(&self) -> Chars {
        let bs = self.bytes();
        Chars {
            bytes: bs,
            buf: [0u8; 4],
            ptr: 0
        }
    }
}

pub trait Subspan<Idx> {
    /// Create a new span from a sub-region of this one
    ///
    /// # Panics
    /// This function will panic if the specified range does not lie within this
    /// span.
    fn subspan(&self, idx: Idx) -> Span;

    /// Copy up to the given range of bytes out of the span
    /// 
    /// This will copy as many bytes as possible into a new `Vec`. If the span
    /// ends in `Future` but might be able to contain the given range, the `Vec`
    /// may not contain the full size of the range given.
    /// 
    /// # Panics
    /// This will panic if the span cannot possibly contain the given range
    fn copy(&self, range: Idx) -> Vec<u8>;
}

impl Subspan<Range<usize>> for Span {
    fn subspan(&self, range: Range<usize>) -> Span {
        if range.start < self.start_pos() {
            panic!("specified subspan out of range");
        }
        if let StreamPoint::Past(x) = self.end {
            if range.end >= x {
                panic!("specified subspan out of range");
            }
        }

        // find the start chunk
        let mut start_chunk = 0;
        loop {
            let c = &self.chunks[start_chunk];
            let end = c.start + c.data.len();
            if range.start >= c.start && range.start < end {
                break;
            }
            start_chunk += 1;
        }

        let mut res: Vec<Arc<Chunk>> = Vec::with_capacity(self.chunks.len());
        for chunk in self.chunks[start_chunk..].iter() {
            res.push(Arc::clone(chunk));

            let chunk_end = chunk.start + chunk.data.len();
            if chunk_end >= range.end {
                break;
            }
        }

        Span {
            start: StreamPoint::Past(range.start),
            end: StreamPoint::Past(range.end),
            chunks: res
        }
    }

    fn copy(&self, range: Range<usize>) -> Vec<u8> {
        // check to make sure we can hold the range
        if range.start < self.start_pos() {
            panic!("range out of span bounds");
        }
        match self.end {
            StreamPoint::Past(l) if range.end > l =>
                panic!("range out of span bounds"),
            _ => {}
        }

        // find the start chunk
        let mut start_chunk = 0;
        loop {
            let c = &self.chunks[start_chunk];
            let end = c.start + c.data.len();
            if range.start >= c.start && range.start < end {
                break;
            }
            start_chunk += 1;
        }
        
        let mut res: Vec<u8> = Vec::with_capacity(range.len());
        for chunk in self.chunks[start_chunk..].iter() {
            let chunk_end = chunk.start + chunk.data.len();
            let start = if chunk.start < range.start {range.start - chunk.start}
                        else {0};
            let end = if chunk_end > range.end { range.end - chunk.start }
                      else {chunk.data.len()};
            res.extend(&((*chunk.data)[start..end]));
        }

        res
    }
}

impl Subspan<RangeTo<usize>> for Span {
    fn subspan(&self, range: RangeTo<usize>) -> Span {
        self.subspan(self.start_pos()..range.end)
    }

    fn copy(&self, range: RangeTo<usize>) -> Vec<u8> {
        self.copy(self.start_pos()..range.end)
    }
}

impl Subspan<RangeFrom<usize>> for Span {
    fn subspan(&self, range: RangeFrom<usize>) -> Span {
        let last_chunk = self.chunks.last().unwrap();
        let end_pos = match self.len() {
            Some(l) => self.start_pos() + l,
            None    => last_chunk.start + last_chunk.data.len()
        };
        let mut s = self.subspan(range.start..end_pos);
        s.end = StreamPoint::Future;
        s
    }

    fn copy(&self, range: RangeFrom<usize>) -> Vec<u8> {
        let last_chunk = self.chunks.last().unwrap();
        let end_pos = match self.len() {
            Some(l) => self.start_pos() + l,
            None    => last_chunk.start + last_chunk.data.len()
        };
        self.copy(range.start..end_pos)
    }
}

impl Subspan<RangeFull> for Span {
    fn subspan(&self, range: RangeFull) -> Span {
        self.clone()
    }

    fn copy(&self, range: RangeFull) -> Vec<u8> {
        let last_chunk = self.chunks.last().unwrap();
        let end_pos = match self.len() {
            Some(l) => self.start_pos() + l,
            None    => last_chunk.start + last_chunk.data.len()
        };
        self.copy(self.start_pos()..end_pos)
    }
}

/// Input stream that lazily reads chunks from an underlying file descriptor
/// 
/// The resulting data is managed in the form of *spans*, logical regions of the
/// file which are reference-counted. When no spans remain active that refer to
/// a given region of the file, the underlying memory will be freed.
/// 
/// This allows users to consume and process large (potentially infinite, even)
/// streams of data normally without worrying about which regions to buffer at
/// any given time.
pub struct LazyReadStream {
    // NOTE: Position is always within the head chunk
    head: Arc<StreamChunk>,
    position: usize,
    source: Arc<Mutex<ReadSource>>
}

impl LazyReadStream {
    /// Create a read stream for the given raw FD
    /// 
    /// This must be granted exclusive ownership of the passed file descriptor.
    pub fn new(f: RawFd) -> io::Result<Self> {
        LazyReadStream::new_from_source(ReadSource::new(f))
    }

    /// Create a read stream for stdin
    pub fn stdin() -> io::Result<Self> {
        LazyReadStream::new_from_source(ReadSource::stdin())
    }

    /// Try to read up to N bytes from the stream into a span
    /// 
    /// This will read at most one new chunk from the underlying stream. If the
    /// stream is already at EOF, this will return an empty `Span`. You can
    /// check for this condition using `Span::is_empty` or `Span::len`.
    pub fn read(&mut self, len: usize) -> io::Result<Span> {
        let mut source = self.source.lock().unwrap();
        let pos = self.position;

        // if the head is empty, we're at EOF
        if self.head.chunk.data.len() == 0 {
            return Ok(Span {
                chunks: vec![Arc::clone(&self.head.chunk)],
                start: StreamPoint::Past(pos),
                end: StreamPoint::Past(pos),
            });
        }

        let desired_end = pos + len;
        let head_end = self.head.chunk.start + self.head.chunk.data.len();

        // it's okay to deliver partial spans
        let span_end = if desired_end < head_end { StreamPoint::Past(desired_end) }
                       else { StreamPoint::Future };
        let read_len = if desired_end < head_end { len } else { head_end - pos };
        let res = Span {
            chunks: vec![Arc::clone(&self.head.chunk)],
            start: StreamPoint::Past(pos),
            end: span_end
        };
        self.position += read_len;

        // are we out of data in the current block? we have to maintain the
        // invariant "there's data between the pointer and the end of the
        // head block".
        //
        // This will always happen if the read length is above the remaining
        // head length
        if self.position == head_end {
            self.head = self.head.advance(&mut *source)?;
        }

        Ok(res)
    }

    /// Extend a span by up to N bytes
    /// 
    /// This works similar to `read` but adds data to the end of an existing
    /// span. If the passed span does not end with `Future`, this is a no-op.
    /// If the underlying stream hits EOF, this will freeze the span and replace
    /// the end with a definite point.
    /// 
    /// # Panics
    /// 
    /// This will panic if the given span's start point begins after the
    /// stream's read pointer or if it ends in `Future` and the end does not
    /// align with the current read pointer.
    pub fn extend(&mut self, span: &mut Span, len: usize) -> io::Result<()> {
        if span.end != StreamPoint::Future { return Ok(()); }
        if span.real_len() + span.start_pos() != self.position {
            panic!("cannot extend a span from the past");
        }

        let s = self.read(len)?;
        let empty = s.is_empty();
        span.union(s)
            .map_err(|_|())
            .expect("tried to extend a span from the past");

        if empty { span.freeze(); }

        Ok(())
    }

    fn new_from_source(mut src: ReadSource) -> io::Result<Self> {
        // pull the first block from the read source
        let block = src.read_chunk()?;

        Ok(LazyReadStream {
            head: Arc::new(block),
            position: 0,
            source: Arc::new(Mutex::new(src))
        })
    }
}

impl Clone for LazyReadStream {
    fn clone(&self) -> Self {
        LazyReadStream {
            head: Arc::clone(&self.head),
            position: self.position,
            source: Arc::clone(&self.source)
        }
    }
}
