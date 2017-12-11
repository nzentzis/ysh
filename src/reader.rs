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
    Lambda,
}

#[derive(Debug)]
pub enum ParseError {
    Evaluation(EvalError),
    Syntax(&'static str),
    UnexpectedEOF,
}

impl ParseError {
    pub fn to_eval(self) -> EvalError {
        match self {
            ParseError::Evaluation(e) => e,
            ParseError::UnexpectedEOF =>
                EvalError::InvalidOperation("unexpected end of file"),
            ParseError::Syntax(_) => EvalError::InvalidOperation("syntax error")
        }
    }
}

impl ::std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            &ParseError::Evaluation(ref e) => write!(f, "Evaluation error: {}", e),
            &ParseError::UnexpectedEOF => write!(f, "Unexpected end of file"),
            &ParseError::Syntax(ref e) => write!(f, "Syntax error: {}", e),
        }
    }
}

impl ::std::error::Error for ParseError {
    fn description(&self) -> &str {
        match self {
            &ParseError::Evaluation(_) => "evaluation error",
            &ParseError::UnexpectedEOF => "unexpected end of file",
            &ParseError::Syntax(_) => "syntax error",
        }
    }
}

impl From<io::Error> for ParseError {
    fn from(e: io::Error) -> ParseError {
        if e.kind() == io::ErrorKind::UnexpectedEof {
            ParseError::UnexpectedEOF
        } else {
            ParseError::Evaluation(EvalError::IO(e))
        }
    }
}

impl From<EvalError> for ParseError {
    fn from(e: EvalError) -> ParseError { ParseError::Evaluation(e) }
}

pub type Parse<T> = Result<T, ParseError>;

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
}

/// Read a numeric value from the given input stream
/// 
/// Upon read failure, push the chars back into the peek stream
fn read_number<R: Read>(strm: &mut PeekReadChars<R>) -> Parse<Number> {
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
                else if c == '+' || c == '-' {
                    s = St::CPM;
                    split_idx = data.len();
                } else if c.is_digit(10) { s = St::NUM0; }
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
            St::HEX => {
                // from_str_radix doesn't handle prefix signs
                let sign = data.chars().next().unwrap();
                let n = if sign == '+' || sign == '-' { &data[3..] }
                        else { &data[2..] };
                let n = i64::from_str_radix(n, 16).unwrap();
                let n = if sign == '-' { -n } else { n };
                Ok(Number::int(n))
            },
            St::BIN => {
                // from_str_radix doesn't handle prefix signs
                let sign = data.chars().next().unwrap();
                let n = if sign == '+' || sign == '-' { &data[3..] }
                        else { &data[2..] };
                let n = i64::from_str_radix(n, 2).unwrap();
                let n = if sign == '-' { -n } else { n };
                Ok(Number::int(n))
            },
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
            _ => panic!("unexpected numeric read state")
        }
    } else {
        for c in data.chars() { strm.push(c); }
        return Err(ParseError::Syntax("cannot read number"));
    }
}

pub fn parse_number(x: &[u8]) -> Parse<Number> {
    use std::io::Cursor;

    read_number(&mut PeekReadChars::new(&mut Cursor::new(x)))
}

fn read_identifier<R: Read>(peek: &mut PeekReadChars<R>) -> Parse<Identifier> {
    let c = peek.next()?;

    let mut s = String::with_capacity(32);
    s.push(c);
    loop {
        let c = peek.peek()
                    .map(Some)
                    .or_else(|e|
                         if e.kind() == io::ErrorKind::UnexpectedEof {Ok(None)}
                         else {Err(e)})?;

        // stop the ident if we hit EOF
        let c = if let Some(c) = c { c } else { break };

        if c != '(' && c != ')' && !c.is_whitespace() {
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
fn internal_read<R: Read>(peek: &mut PeekReadChars<R>,
                          allow_newlines: bool) -> Parse<(Value)> {
    let mut stack = Vec::with_capacity(16);
    let table = READ_TABLE.read().unwrap();

    // maintain state for lambda shorthand
    let mut in_lambda = false;
    let mut lambda_args: i32 = 0; // -1 for a single $, 0 for unset, >0 for $N form

    loop {
        // read a character
        let c = loop {
            let c = peek.peek().map(Some)
                   .or_else(|e| {
                       if e.kind() == io::ErrorKind::UnexpectedEof { Ok(None) }
                       else { Err(e) }})?;
            if !c.map(|x| x.is_whitespace() &&
                          !(x == '\n' && !allow_newlines))
                 .unwrap_or(false) { break c }

            // skip whitespace
            peek.next()?;
        };

        let c = if let Some(c) = c { c } else {
            // handle EOF
            return Err(ParseError::UnexpectedEOF);
        };

        // if we got here, we're not allowing newlines - treat this just like
        // EOF
        if c == '\n' {
            return Err(ParseError::UnexpectedEOF);
        }

        // use a reader macro if applicable
        let res = if let Some(_exec) = table.get(&c) {
            // use a reader macro
            unimplemented!()
        } else {
            if c == '(' {
                stack.push(ParseStackElement::List(Vec::new()));
                peek.next()?;
                continue;
            } else if c == ')' {
                let v =
                    if let Some(ParseStackElement::List(v)) = stack.pop() {v}
                    else {
                        return Err(ParseError::Syntax("unexpected ')'"))
                    };
                peek.next()?;
                Value::list(v)
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
                Value::str(s)
            } else if c == '\'' {
                // use the built-in quote macro
                stack.push(ParseStackElement::Quote);
                peek.next()?;
                continue;
            } else if c == '$' {
                peek.next()?;
                let c = peek.peek().map(Some)
                            .or_else(|e|
                                 if e.kind() == io::ErrorKind::UnexpectedEof {
                                     Ok(None)
                                 } else {Err(e)})?;
                if let Some(c) = c {
                    if c == '(' {
                        if in_lambda {
                            return Err(ParseError::Syntax(
                                "lambda functions cannot be nested"));
                        }
                        in_lambda = true;
                        lambda_args = 0;
                        stack.push(ParseStackElement::Lambda);
                        stack.push(ParseStackElement::List(Vec::new()));
                        peek.next()?;
                        continue;
                    }
                }
                peek.push('$');

                let id = read_identifier(peek)?;
                if id.as_ref() == "$" {
                    if in_lambda {
                        if lambda_args > 1 {
                            return Err(ParseError::Syntax(
                                    "mixing $ and $N forms is not allowed"));
                        }
                        lambda_args = -1;
                    }
                } else if id.as_ref().len() == 2 &&
                          id.as_ref().chars().next() == Some('$') {
                    let c = id.as_ref().chars().skip(1).next().unwrap();
                    if c.is_digit(10) && in_lambda {
                        if lambda_args < 0 {
                            return Err(ParseError::Syntax(
                                    "mixing $ and $N forms is not allowed"));
                        }
                        let n = c.to_digit(10).unwrap() as i32;
                        if n > lambda_args {
                            lambda_args = n;
                        }
                    }
                }

                Value::from(id)
            } else if c == '+' || c == '-' || (c >= '0' && c <= '9') || c == '.' {
                // try to read a numeric constant
                let r = read_number(peek);
                if let Ok(r) = r { Value::from(r) }
                else {
                    // fall back to symbol
                    Value::from(read_identifier(peek)?)
                }
            } else if c == '|' {
                // special handling for pipe symbols since literally any char
                // can be inside of a |x> separator
                peek.next()?;

                // read forward to see if there's a terminator
                let fwd = peek.next()?;

                if fwd == '>' {
                    Value::from(Identifier::from(format!("|>")))
                } else {
                    if peek.peek()? == '>' {
                        peek.next()?;
                        Value::from(Identifier::from(format!("|{}>", fwd)))
                    } else {
                        peek.push(fwd);
                        Value::from(Identifier::from(format!("|")))
                    }
                }
            } else {
                let id = read_identifier(peek)?;
                if id.0.as_str() == "true" {
                    Value::from(true)
                } else if id.0.as_str() == "false" {
                    Value::from(false)
                } else {
                    Value::from(id)
                }
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
                    res = Value::list(vec![
                        Value::from(Identifier::new("quote")),
                        res]);
                },
                Some(&mut ParseStackElement::Lambda) => {
                    // construct an appropriate lambda function
                    let args = {
                        if lambda_args == -1 {
                            vec![Value::from(Identifier::new("$"))]
                        } else {
                            (0..lambda_args).into_iter()
                                            .map(|x| format!("${}", x+1))
                                            .map(Identifier::new)
                                            .map(Value::from)
                                            .collect::<Vec<_>>()
                        }
                    };
                    res = Value::list(vec![
                        Value::from(Identifier::new("fn")),
                        Value::list(args),
                        res
                    ]);
                    
                    // reset state
                    in_lambda = false;
                    lambda_args = 0;
                },
                None => return Ok((res))
            }
            stack.pop();
        }
    }
}

/// Read a Lisp form from the given input stream
/// 
/// This returns either an evaluation failure or a (read value, unconsumed char)
/// pair. In most cases the unconsumed character can be ignored.
pub fn read<R: Read>(strm: &mut R) -> Parse<Value> {
    let mut peek = PeekReadChars::new(strm);
    internal_read(&mut peek, true)
}

/// Read a pipeline from the given input stream
/// 
/// This just reads forms from the input and processes them into a pipeline.
/// Reader macros run *before* this sees the forms.
pub fn read_pipeline<R: Read>(strm: &mut R) -> Parse<Pipeline> {
    let mut pipeline = Pipeline {
        elements: Vec::new(),
        terminals: Vec::new()
    };
    let mut peek = PeekReadChars::new(strm);

    fn get_pipe(id: &Identifier) -> Option<PipeMode> {
        let s: &str = id.as_ref();
        if s == "|" { Some(PipeMode::Pipe) }
        else if s == "|>" { Some(PipeMode::PipeText) }
        else if s.chars().count() == 3 {
            let mut chars = s.chars();
            let f = chars.next().unwrap();
            let s = chars.next().unwrap();
            let l = chars.next().unwrap();
            if f == '|' && l == '>' { Some(PipeMode::DelimitedPipe(s)) }
            else { None }
        } else { None }
    }

    // pipeline read state
    #[derive(Debug)]
    enum PRS {
        S, // start state
        F, // reading form
        Pipe, // pipe separator
        // redirects (append/replace) (file/var)
        OutReplaceFile, OutAppendFile, OutReplaceVar, OutAppendVar,
        InFile, InVar, // input redirects (file/var)
        TerminalDone,
    }

    let mut s = PRS::S;
    let mut cur_component = Vec::new();
    let mut out_done = false;
    let mut in_done = false;

    loop {
        let tok = internal_read(&mut peek, false).map(Some)
                 .or_else(|e| match e {
                     ParseError::UnexpectedEOF => Ok(None),
                     e => Err(e)
                 })?;
        println!("({:?}){:?}", s, tok);

        match s {
            PRS::S => {
                let tok = if let Some(tok) = tok { tok }
                          else { return Err(ParseError::UnexpectedEOF) };
                if let Some(sym) = tok.get_symbol()? {
                    // check if it's a pipe separator
                    if get_pipe(&sym).is_some() {
                        return Err(ParseError::Syntax(
                                "invalid pipeline"));
                    } else {
                        s = PRS::F;
                        cur_component.push(tok);
                    }
                } else {
                    s = PRS::F;
                    cur_component.push(tok);
                }
            },
            PRS::F => {
                let tok = if let Some(tok) = tok { tok } else {
                    // if EOF, terminate the pipeline
                    let part = PipelineComponent {
                        xform: Transformer(cur_component.drain(..).collect()),
                        link: None
                    };
                    pipeline.elements.push(part);
                    return Ok(pipeline);
                };

                if let Some(sym) = tok.get_symbol()? {
                    // check if it's a pipe separator
                    let mut pipe_mode = None;
                    let next_state =
                        if let Some(mode) = get_pipe(&sym) {
                            pipe_mode = Some(mode);
                            Some(PRS::Pipe) }
                        else if sym.as_ref() == ">" {Some(PRS::OutReplaceFile)}
                        else if sym.as_ref() == ">>" {Some(PRS::OutAppendFile)}
                        else if sym.as_ref() == ">=" {Some(PRS::OutReplaceVar)}
                        else if sym.as_ref() == ">>=" {Some(PRS::OutAppendVar)}
                        else if sym.as_ref() == "<" {Some(PRS::InFile)}
                        else if sym.as_ref() == "<=" {Some(PRS::InVar)}
                        else { cur_component.push(tok); None };

                    if let Some(next) = next_state {
                        // finish the component
                        let part = PipelineComponent {
                            xform: Transformer(cur_component.drain(..).collect()),
                            link: pipe_mode
                        };
                        pipeline.elements.push(part);
                        s = next;
                    }
                } else {
                    s = PRS::F;
                    cur_component.push(tok);
                }
            },
            PRS::Pipe => {
                let tok = if let Some(tok) = tok { tok }
                          else { return Err(ParseError::UnexpectedEOF) };
                if let Some(sym) = tok.get_symbol()? {
                    // check if it's a pipe separator
                    if get_pipe(&sym).is_some() {
                        return Err(ParseError::Syntax("invalid pipeline"));
                    } else {
                        s = PRS::F;
                        cur_component.push(tok);
                    }
                } else {
                    s = PRS::F;
                    cur_component.push(tok);
                }
            },
            PRS::OutReplaceFile => {
                let tok = if let Some(tok) = tok { tok }
                          else { return Err(ParseError::UnexpectedEOF) };
                
                // evaluate the token and try converting to a string
                let res = tok.evaluate(&::environment::empty())?;
                let t = res.into_str()?;
                pipeline.terminals.push(TerminalMode::ReplaceFile(t));
                if in_done { return Ok(pipeline); }
                else { s = PRS::TerminalDone; out_done = true; }
            },
            PRS::OutAppendFile => {
                let tok = if let Some(tok) = tok { tok }
                          else { return Err(ParseError::UnexpectedEOF) };
                
                // evaluate the token and try converting to a string
                let res = tok.evaluate(&::environment::empty())?;
                let t = res.into_str()?;
                pipeline.terminals.push(TerminalMode::AppendFile(t));
                if in_done { return Ok(pipeline); }
                else { s = PRS::TerminalDone; out_done = true; }
            },
            PRS::OutReplaceVar => {
                let tok = if let Some(tok) = tok { tok }
                          else { return Err(ParseError::UnexpectedEOF) };
                
                if let Some(sym) = tok.get_symbol()? {
                    pipeline.terminals.push(TerminalMode::SetVariable(sym));
                    if in_done { return Ok(pipeline); }
                    else { s = PRS::TerminalDone; out_done = true; }
                } else {
                    return Err(ParseError::Syntax(
                            "expected symbol as output target"));
                }
            },
            PRS::OutAppendVar => {
                let tok = if let Some(tok) = tok { tok }
                          else { return Err(ParseError::UnexpectedEOF) };
                
                if let Some(sym) = tok.get_symbol()? {
                    pipeline.terminals.push(TerminalMode::AppendVariable(sym));
                    if in_done { return Ok(pipeline); }
                    else { s = PRS::TerminalDone; out_done = true; }
                } else {
                    return Err(ParseError::Syntax(
                            "expected symbol as output target"));
                }
            },
            PRS::InFile => {
                let tok = if let Some(tok) = tok { tok }
                          else { return Err(ParseError::UnexpectedEOF) };
                
                // evaluate the token and try converting to a string
                let res = tok.evaluate(&::environment::empty())?;
                let t = res.into_str()?;
                pipeline.terminals.push(TerminalMode::InputFile(t));
                if out_done { return Ok(pipeline); }
                else { s = PRS::TerminalDone; in_done = true; }
            },
            PRS::InVar => {
                let tok = if let Some(tok) = tok { tok }
                          else { return Err(ParseError::UnexpectedEOF) };
                
                if let Some(sym) = tok.get_symbol()? {
                    pipeline.terminals.push(TerminalMode::InputVar(sym));
                    if out_done { return Ok(pipeline); }
                    else { s = PRS::TerminalDone; in_done = true; }
                } else {
                    return Err(ParseError::Syntax(
                            "expected symbol as output target"));
                }
            },
            PRS::TerminalDone => {
                let tok = if let Some(tok) = tok { tok }
                          else { return Ok(pipeline); };

                if let Some(sym) = tok.get_symbol()? {
                    let (out,st) =
                        if sym.as_ref() == ">" {(true,PRS::OutReplaceFile)}
                        else if sym.as_ref() == ">>" {(true,PRS::OutAppendFile)}
                        else if sym.as_ref() == ">=" {(true,PRS::OutReplaceVar)}
                        else if sym.as_ref() == ">>=" {(true,PRS::OutAppendVar)}
                        else if sym.as_ref() == "<" {(false,PRS::InFile)}
                        else if sym.as_ref() == "<=" {(false,PRS::InVar)}
                        else {
                            return Err(ParseError::Syntax(
                                    "unexpected symbol in output position")); 
                        };
                    if out {
                        if out_done { return Err(ParseError::Syntax(
                                    "cannot specify two output targets")); }
                        s = st;
                    } else {
                        if in_done { return Err(ParseError::Syntax(
                                    "cannot specify two input targets")); }
                        s = st;
                    }
                } else {
                    return Err(ParseError::Syntax(
                            "expected terminal or end of input"));
                }
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn numbers() {
        let cases = vec![
            (&b"0"[..], Number::int(0)),
            (&b"0x1a"[..], Number::int(0x1a)),
            (&b"0x1A"[..], Number::int(0x1a)),
            (&b"-0x1A"[..], Number::int(-0x1a)),
            (&b"+0x2A"[..], Number::int(0x2a)),
            (&b"0x02"[..], Number::int(2)),
            (&b"0b1101"[..], Number::int(13)),
            (&b"0b0100"[..], Number::int(4)),
            (&b"-0b0100"[..], Number::int(-4)),
            (&b"14"[..], Number::int(14)),
            (&b"027"[..], Number::int(27)),

            (&b"0.2"[..], Number::real(0.2)),
            (&b"-0.2"[..], Number::real(-0.2)),
            (&b"+0.2"[..], Number::real(0.2)),
            (&b"0.2e7"[..], Number::real(2000000.0)),
            (&b"0.2e+7"[..], Number::real(2000000.0)),
            (&b"0.2e-7"[..], Number::real(0.2e-7)),
            (&b"-0.2e-7"[..], Number::real(-0.2e-7)),

            (&b"1/2"[..], Number::rational(1, 2)),
            (&b"-1/2"[..], Number::rational(-1, 2)),
            (&b"-02/7"[..], Number::rational(-2, 7)),

            (&b"0+2i"[..], Number::complex(0.,2.)),
            (&b"1-2i"[..], Number::complex(1.,-2.)),
            (&b"+1+2i"[..], Number::complex(1.,2.)),
            (&b"+1.27e-0+2i"[..], Number::complex(1.27,2.)),
            (&b"+1.27e-0+2.2i"[..], Number::complex(1.27,2.2)),
            (&b"+1.27e-0+2.2e-1i"[..], Number::complex(1.27,0.22)),
        ];

        for (i,o) in cases {
            let r = read_number(&mut PeekReadChars::new(&mut Cursor::new(i)));
            assert_eq!(r.unwrap(), o);
        }
    }

    #[test]
    fn identifiers() {
        let cases = vec![
            (&b"ident"[..], "ident"),
            (&b"ident foo"[..], "ident"),
        ];

        for (i,o) in cases {
            let r = read_identifier(
                &mut PeekReadChars::new(&mut Cursor::new(&i[..])));
            assert_eq!(r.unwrap(), Identifier::new(o));
        }
    }

    #[test]
    fn forms() {
        let cases = vec![
            (&b"ident foo"[..], Value::from(Identifier::new("ident"))),
            (&b"12/4"[..], Value::from(Number::rational(12,4))),
            (&b"+"[..], Value::from(Identifier::new("+"))),
            (&b"\"st\\nr\\t\\\" t\\rest\""[..], Value::str("st\nr\t\" t\rest")),
            (&b"()"[..], Value::list(vec![])),
            (&b"(1 2)"[..], Value::list(vec![Value::from(Number::int(1)),
                                             Value::from(Number::int(2))])),
            (&b"|  "[..], Value::from(Identifier::new("|"))),
            (&b"|> "[..], Value::from(Identifier::new("|>"))),
            (&b"| > "[..], Value::from(Identifier::new("| >"))),
            (&b"||> "[..], Value::from(Identifier::new("||>"))),
            (&b"(true false)"[..], Value::list(vec![Value::from(true), Value::from(false)])),
            (&b"$(+ x $)"[..], Value::list(vec![
                Value::from(Identifier::new("fn")),
                Value::list(vec![Value::from(Identifier::new("$"))]),
                Value::list(vec![
                    Value::from(Identifier::new("+")),
                    Value::from(Identifier::new("x")),
                    Value::from(Identifier::new("$"))])])),
            (&b"$(+ $1    $3)"[..], Value::list(vec![
                Value::from(Identifier::new("fn")),
                Value::list(vec![Value::from(Identifier::new("$1")),
                                 Value::from(Identifier::new("$2")),
                                 Value::from(Identifier::new("$3"))]),
                Value::list(vec![
                    Value::from(Identifier::new("+")),
                    Value::from(Identifier::new("$1")),
                    Value::from(Identifier::new("$3"))])])),
            (&b"'2"[..], Value::list(vec![Value::from(Identifier::new("quote")),
                                          Value::from(Number::int(2))]))
        ];

        for (i,o) in cases {
            let r = internal_read(
                &mut PeekReadChars::new(&mut Cursor::new(&i[..])),
                false);
            assert_eq!(r.unwrap(), o);
        }
    }

    #[test]
    fn pipelines() {
        let s = &b"1 bar | baz bax |> foo |~> bax > bar < foo"[..];
        let r = read_pipeline(&mut Cursor::new(&s[..])).unwrap();
        assert_eq!(r, Pipeline {
            elements: vec![
                PipelineComponent {
                    xform: Transformer(vec![Value::from(Identifier::new("foo")),
                                            Value::from(Identifier::new("bar"))]),
                    link: Some(PipeMode::Pipe)
                },
                PipelineComponent {
                    xform: Transformer(vec![Value::from(Identifier::new("baz")),
                                            Value::from(Identifier::new("bax"))]),
                    link: Some(PipeMode::PipeText)
                },
                PipelineComponent {
                    xform: Transformer(vec![Value::from(Identifier::new("foo"))]),
                    link: Some(PipeMode::DelimitedPipe('~'))
                },
                PipelineComponent {
                    xform: Transformer(vec![Value::from(Identifier::new("bax"))]),
                    link: None
                },
            ],
            terminals: vec![TerminalMode::ReplaceFile(String::from("bar")),
                            TerminalMode::InputFile(String::from("foo"))]
        });
    }
}
