use std::io;
use std::io::prelude::*;
use std::os::unix::prelude::*;

use std::io::ErrorKind;
use std::sync::{Arc, Weak, Mutex};
use std::cell::Cell;
use std::cmp::Ordering;
use std::ops::Range;

use std::fs::File;

const MAX_CHUNK: usize = 8192;

/// Chunk of an input stream
/// 
/// This *doesn't* hold a strong reference to previous stream chunks, but it
/// does let you traverse backwards in the stream if the previous elements still
/// exist. This allows for efficient traversal of contiguous ranges.
struct Chunk {
    /// Optional pointer to the last chunk
    prev: Weak<Chunk>,

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
    fn new_from_read<R: Read>(prev: Weak<Chunk>,
                              input: &mut R,
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
        buf.shrink_to_fit();
        Ok(Chunk {
            prev, start,
            data: buf.into_boxed_slice()
        })
    }

    /// Check whether a given chunk is reachable by moving backwards from the
    /// current one
    fn reachable(&self, other: &Chunk) -> bool {
        if self.start == other.start { true }
        else if self.start < other.start { false }
        else {
            // traverse
            let mut ptr = Weak::clone(&self.prev);
            while let Some(arc) = ptr.upgrade() {
                if arc.start == other.start { return true; }
                ptr = Arc::downgrade(&arc);
            }
            false
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
    fn read_chunk(&mut self, prev: Weak<Chunk>) -> io::Result<Chunk> {
        match self {
            &mut ReadSource::FromFile {ref mut f, ref mut offs} =>
                Chunk::new_from_read(prev, f, offs),
            &mut ReadSource::FromChannel {ref mut f, ref mut offs} =>
                Chunk::new_from_read(prev, f, offs)
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
        if (!self.chunks[0].reachable(other.chunks.last().unwrap())) &&
                (!other.chunks[0].reachable(self.chunks.last().unwrap())) {
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

    /// Copy up to the given range of bytes out of the span
    /// 
    /// This will copy as many bytes as possible into a new `Vec`. If the span
    /// ends in `Future` but might be able to contain the given range, the `Vec`
    /// may not contain the full size of the range given.
    /// 
    /// # Panics
    /// This will panic if the span cannot possibly contain the given range
    pub fn copy(&self, range: Range<usize>) -> Vec<u8> {
        // check to make sure we can hold the range
        if range.end <= self.start_pos() {
            panic!("range out of span bounds");
        }
        match self.end {
            StreamPoint::Past(l) if range.end > l =>
                panic!("range out of span bounds"),
            _ => {}
        }

        // find the start chunk
        let start_chunk = self.chunks.iter()
                                     .position(|c| c.start > range.start)
                                     .unwrap() - 1;
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
    // NOTE: Lock ordering: position -> head -> source
    // NOTE: Position is always within the head chunk
    head: Mutex<Arc<Chunk>>,
    position: Mutex<usize>,
    source: Mutex<ReadSource>
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
    pub fn read(&self, len: usize) -> io::Result<Span> {
        let mut pos = self.position.lock().unwrap();
        let mut head = self.head.lock().unwrap();
        let mut source = self.source.lock().unwrap();

        let desired_end = *pos + len;
        let head_end = head.start + head.data.len();

        // it's okay to deliver partial spans
        let span_end = if desired_end < head_end { StreamPoint::Past(desired_end) }
                       else { StreamPoint::Future };
        let read_len = if desired_end < head_end { len } else { head_end - *pos };
        let res = Span {
            chunks: vec![Arc::clone(&*head)],
            start: StreamPoint::Past(*pos),
            end: span_end
        };
        *pos += read_len;

        // are we out of data in the current block? we have to maintain the
        // invariant "there's data between the pointer and the end of the
        // head block".
        //
        // This will always happen if the read length is above the remaining
        // head length
        if *pos == head_end {
            let new_head = Arc::new(
                source.read_chunk(Arc::downgrade(&*head))?);
            *head = new_head;
        }

        Ok(res)
    }

    /// Extend a span by up to N bytes
    /// 
    /// This works similar to `read` but adds data to the end of an existing
    /// span. If the passed span does not end with `Future`, this is a no-op.
    pub fn extend(&self, span: &mut Span, len: usize) -> io::Result<()> {
        if span.end != StreamPoint::Future { return Ok(()); }

        let s = self.read(len)?;
        span.union(s)
            .map_err(|_|())
            .expect("tried to extend a span from the past");
        Ok(())
    }

    fn new_from_source(mut src: ReadSource) -> io::Result<Self> {
        // pull the first block from the read source
        let block = src.read_chunk(Weak::new())?;

        Ok(LazyReadStream {
            head: Mutex::new(Arc::new(block)),
            position: Mutex::new(0),
            source: Mutex::new(src)
        })
    }
}