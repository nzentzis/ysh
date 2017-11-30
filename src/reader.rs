use std::str;
use std::str::FromStr;
use std::io::prelude::*;
use std::io;
use std::sync::RwLock;
use std::collections::HashMap;

use data::*;
use numeric::*;

lazy_static! {
    // read table
    //
    // if an entry is present for an input char, the reader will execute the
    // given function f as (f strm char)
    static ref READ_TABLE: RwLock<HashMap<char, Executable>>
        = RwLock::new(HashMap::new());
}

enum ParseStackElement {
    List(Vec<Value>),
    Quote,
}

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

struct PeekReadChars<'a, R: Read + 'a> {
    peek: Vec<char>, // index 0 is the read point, new data goes at end
    strm: &'a mut R
}

impl<'a, R: Read> PeekReadChars<'a, R> {
    /// Build a new peek stream
    fn new(strm: &'a mut R) -> Self {
        PeekReadChars { peek: Vec::new(), strm }
    }

    /// Read a new character into the peek buffer
    fn make_avail(&mut self) -> io::Result<()> {
        let c = read_utf8(&mut self.strm)?;
        self.peek.push(c);
        Ok(())
    }

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

    /// Terminate the stream and return the peeked chars, if any
    fn end(mut self) -> Vec<char> {
        self.peek.clone()
    }
}

/// Read a numeric value from the given input stream
/// 
/// Upon read failure, push the chars back into the peek stream
fn read_number<R: Read>(strm: &mut PeekReadChars<R>) -> Eval<Number> {
    #[derive(Debug)]
    enum St {
        S,
        IPM, // initial +/-
        IZ, // initial zero
        NUM0, // first group of numerals
        DOT0, // floating-point dot (first)
        NUM1, // second group of numerals
        EXP, // exponent (e|E)
        EPM, // exponent +/-
        EDIG, // exponent digits
        RAT, // rational division
        RNUM1, // second integer of a rational
        CPM, // +/- of a complex number
        CNUM1, // second digit set of a complex number
        CDOT,  // second dot of a complex number
        CNUM2, // third digit set of a complex number
        CEXP1, // second exp of a complex number
        CEPM, // second exp +/- of a complex number
        CEDIG, // second exp digits of a complex number
        CPX, // complex end
        RB,RX, // radix hex/binary
        HEX,BIN, // hex/binary data
    };

    let mut data = String::with_capacity(32);
    let mut s = St::S;

    let mut split_idx = 0; // used for rational and complex splitting

    let r = loop {
        let c = strm.peek().map(Some)
                    .or_else(|e| {
                        if e.kind() == io::ErrorKind::UnexpectedEof { Ok(None) }
                        else { Err(e) }})?;
        match s {
            St::S => {
                let c = if let Some(c) = c { c } else {break None};
                if c == '+' || c == '-' { s = St::IPM; }
                else if c == '0' { s = St::IZ; }
                else if c.is_digit(10) { s = St::NUM0; }
                else if c == '.' { s = St::DOT0 }
                else { break None }
            },
            St::IPM => {
                let c = if let Some(c) = c { c } else { break None };
                if c == '0' { s = St::IZ; }
                else if c.is_digit(10) { s = St::NUM0; }
                else if c == '.' { s = St::DOT0 }
                else { break None }
            },
            St::IZ => {
                let c = if let Some(c) = c { c } else { break Some(()) };
                if c == 'x' { s = St::RX; }
                else if c == 'b' { s = St::RB; }
                else if c == '.' { s = St::DOT0; }
                else if c == 'e' || c == 'E' { s = St::EXP; }
                else if c.is_digit(10) { s = St::NUM0; }
                else { break Some(()) }
            },
            St::NUM0 => {
                let c = if let Some(c) = c { c } else { break Some(()) };
                if c == '.' { s = St::DOT0; }
                else if c == 'e' || c == 'E' { s = St::EXP; }
                else if c.is_digit(10) {} // continue
                else if c == '/' { s = St::RAT; split_idx = data.len(); }
                else if c == '+' || c == '-' {
                    s = St::CPM;
                    split_idx = data.len();
                } else { break Some(()) }
            },
            St::DOT0 => {
                let c = if let Some(c) = c { c } else { break None };
                if c.is_digit(10) { s = St::NUM1; }
                else { break None }
            },
            St::NUM1 => {
                let c = if let Some(c) = c { c } else { break Some(()) };
                if c.is_digit(10) {} // continue
                else if c == 'e' || c == 'E' { s = St::EXP; }
                else if c == '+' || c == '-' {
                    s = St::CPM;
                    split_idx = data.len();
                } else { break Some(()) }
            },
            St::EXP => {
                let c = if let Some(c) = c { c } else { break None };
                if c.is_digit(10) { s = St::EDIG; }
                else if c == '+' || c == '-' { s = St::EPM; }
                else { break None }
            },
            St::EPM => {
                let c = if let Some(c) = c { c } else { break None };
                if c.is_digit(10) { s = St::EDIG; }
                else { break None }
            },
            St::EDIG => {
                let c = if let Some(c) = c { c } else { break Some(()) };
                if c.is_digit(10) {} // continue
                else if c == '+' || c == '-' {
                    s = St::CPM;
                    split_idx = data.len();
                } else { break Some(()) }
            },
            St::RAT => {
                let c = if let Some(c) = c { c } else { break None };
                if c.is_digit(10) { s = St::RNUM1; }
                else { break None }
            },
            St::RNUM1 => {
                let c = if let Some(c) = c { c } else { break Some(()) };
                if c.is_digit(10) {} // continue
                else { break Some(()) }
            },
            St::CPM => {
                let c = if let Some(c) = c { c } else { break None };
                if c.is_digit(10) { s = St::CNUM1; }
                else if c == '.' { s = St::CDOT; }
                else { break None }
            },
            St::CNUM1 => {
                let c = if let Some(c) = c { c } else { break None };
                if c.is_digit(10) {}
                else if c == 'e' || c == 'E' { s = St::CEXP1; }
                else if c == '.' { s = St::CDOT; }
                else if c == 'i' { s = St::CPX; }
                else { break None }
            },
            St::CDOT => {
                let c = if let Some(c) = c { c } else { break None };
                if c.is_digit(10) { s = St::CNUM2; }
                else { break None }
            },
            St::CNUM2 => {
                let c = if let Some(c) = c { c } else { break None };
                if c.is_digit(10) {}
                else if c == 'e' || c == 'E' { s = St::CEXP1; }
                else if c == 'i' { s = St::CPX; }
                else { break None }
            },
            St::CEXP1 => {
                let c = if let Some(c) = c { c } else { break None };
                if c.is_digit(10) { s = St::CEDIG; }
                else if c == '+' || c == '-' { s = St::CEPM; }
                else { break None }
            },
            St::CEPM => {
                let c = if let Some(c) = c { c } else { break None };
                if c.is_digit(10) { s = St::CEDIG; }
                else { break None }
            },
            St::CEDIG => {
                let c = if let Some(c) = c { c } else { break Some(()) };
                if c.is_digit(10) {} // continue
                else if c == 'i' { s = St::CPX; }
                else { break Some(()) }
            },
            St::CPX => {
                break Some(());
            },
            St::RB => {
                let c = if let Some(c) = c { c } else { break None };
                if c == '0' || c == '1' { s = St::BIN; }
                else { break None }
            },
            St::RX => {
                let c = if let Some(c) = c { c } else { break None };
                if c.is_digit(16) { s = St::HEX; }
                else { break None }
            },
            St::HEX => {
                let c = if let Some(c) = c { c } else { break Some(()) };
                if c.is_digit(16) {} // continue
                else { break Some(()) }
            },
            St::BIN => {
                let c = if let Some(c) = c { c } else { break Some(()) };
                if c == '0' || c == '1' {} // continue
                else { break Some(()) }
            }
        }
        data.push(c.unwrap());
        strm.next()?;
    };

    if r.is_some() {
        // figure out which type of value it is and parse it
        match s {
            St::IZ   => Ok(Number::int(0)),
            St::NUM0 => 
                Ok(Number::int(i64::from_str_radix(&data, 10).unwrap())),
            St::NUM1 => Ok(Number::real(f64::from_str(&data).unwrap())),
            St::EDIG => Ok(Number::real(f64::from_str(&data).unwrap())),
            St::HEX =>
                Ok(Number::int(i64::from_str_radix(&data[2..], 16).unwrap())),
            St::BIN =>
                Ok(Number::int(i64::from_str_radix(&data[2..], 2).unwrap())),
            St::RNUM1 => {
                // find number components
                let a = i64::from_str_radix(&data[..split_idx], 10).unwrap();
                let b = i64::from_str_radix(&data[split_idx+1..], 10).unwrap();
                Ok(Number::rational(a,b))
            },
            St::CPX => {
                let max_idx = data.len();
                let a = f64::from_str(&data[..split_idx]).unwrap();
                let b = f64::from_str(&data[split_idx..max_idx-1]).unwrap();
                Ok(Number::complex(a,b))
            },
            _ => unimplemented!()
        }
    } else {
        for c in data.chars() { strm.push(c); }
        return Err(EvalError::InvalidOperation("cannot read number"));
    }
}

fn read_identifier<R: Read>(peek: &mut PeekReadChars<R>) -> Eval<Identifier> {
    let c = peek.next()?;

    let mut s = String::with_capacity(32);
    s.push(c);
    loop {
        let c = peek.peek()?;

        if c != ')' && !c.is_whitespace() {
            s.push(c);
            peek.next()?;
        } else {
            break;
        }
    }
    Ok(Identifier::from(s))
}

/// Read a Lisp form from the given input stream
/// 
/// This returns either an evaluation failure or a (read value, unconsumed char)
/// pair. In most cases the unconsumed character can be ignored.
pub fn read<R: Read>(strm: &mut R) -> Eval<(Value, Vec<char>)> {
    let mut peek = PeekReadChars::new(strm);
    let mut stack = Vec::with_capacity(16);
    let table = READ_TABLE.read().unwrap();
    let empty = ::environment::empty();

    loop {
        // read a character
        let c = loop {
            let c = peek.peek().map(Some)
                   .or_else(|e| {
                       if e.kind() == io::ErrorKind::UnexpectedEof { Ok(None) }
                       else { Err(e) }})?;
            if !c.map(|x| x.is_whitespace()).unwrap_or(false) { break c }

            // skip whitespace
            peek.next()?;
        };

        let c = if let Some(c) = c { c } else {
            // handle EOF
            unimplemented!()
        };

        // use a reader macro if applicable
        let res = if let Some(exec) = table.get(&c) {
            // use a reader macro
            unimplemented!()
        } else {
            if c == '(' {
                stack.push(ParseStackElement::List(Vec::new()));
                peek.next()?;
                continue;
            } else if c == ')' {
                let v =
                    if let ParseStackElement::List(v) = stack.pop().unwrap() {v}
                    else {panic!("impossible parser situation - bad stack state")};
                peek.next()?;
                BasicValue::list(v)
            } else if c == '"' {
                peek.next()?;

                // read a quoted string
                let mut escaped = false;
                let mut s = String::with_capacity(32);
                loop {
                    let c = peek.next()?;
                    if escaped {
                        match c {
                            'n' => s.push('\n'),
                            'r' => s.push('\r'),
                            't' => s.push('\t'),
                            x   => s.push(x),
                        }
                        escaped = false;
                    } else {
                        if c == '\\' {
                            escaped = true;
                        } else if c == '"' {
                            break;
                        } else {
                            s.push(c);
                        }
                    }
                }
                BasicValue::str(s)
            } else if c == '\'' {
                // use the built-in quote macro
                stack.push(ParseStackElement::Quote);
                peek.next()?;
                continue;
            } else if c == '+' || c == '-' || (c >= '0' && c <= '9') || c == '.' {
                // try to read a numeric constant
                let r = read_number(&mut peek);
                if let Ok(r) = r { Value::new(BasicValue::Number(r)) }
                else {
                    // fall back to symbol
                    Value::new(BasicValue::Symbol(read_identifier(&mut peek)?))
                }
            } else if c == '|' {
                // special handling for pipe symbols since literally any char
                // can be inside of a |x> separator
                peek.next()?;

                // read forward to see if there's a terminator
                let fwd = peek.next()?;

                if fwd == '>' {
                    Value::new(BasicValue::Symbol(
                            Identifier::from(format!("|>"))))
                } else {
                    if peek.peek()? == '>' {
                        Value::new(BasicValue::Symbol(
                            Identifier::from(format!("|{}>", fwd))))
                    } else {
                        peek.push(fwd);
                        Value::new(BasicValue::Symbol(
                                Identifier::from(format!("|"))))
                    }
                }
            } else {
                Value::new(BasicValue::Symbol(read_identifier(&mut peek)?))
            }
        };

        // pop off stack
        let mut res = res;
        loop {
            match stack.last_mut() {
                Some(&mut ParseStackElement::List(ref mut v)) => {
                    v.push(res);
                    break;
                },
                Some(&mut ParseStackElement::Quote) => {
                    // quote the element
                    res = BasicValue::list(vec![
                        Value::new(BasicValue::Symbol(Identifier::new("quote"))),
                        res]);
                },
                None => return Ok((res, peek.end()))
            }
            stack.pop();
        }
    }
}

/// Read a pipeline from the given input stream
/// 
/// This just reads forms from the input and constructs a pipeline out of them
pub fn read_pipeline<R: Read>(strm: &mut R) -> Eval<Pipeline> {
    unimplemented!()
}
