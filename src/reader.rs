use std::str;
use std::str::FromStr;
use std::io;
use std::sync::RwLock;
use std::collections::HashMap;

use data::*;
use numeric::*;
use stream::{CharStream, ReadWrapper, StreamError};

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
    Map(HashMap<ValueHash, Value>, Option<ValueHash>),
    Quote,
    Lambda,
}

#[derive(Debug)]
pub enum ParseError {
    Evaluation(EvalError),
    Syntax(&'static str),
    Stream(StreamError),
    UnexpectedEOF,
}

impl ParseError {
    pub fn to_eval(self) -> EvalError {
        match self {
            ParseError::Evaluation(e) => e,
            ParseError::UnexpectedEOF =>
                EvalError::InvalidOperation("unexpected end of file"),
            ParseError::Stream(s) => EvalError::from(s),
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
            &ParseError::Stream(ref e) => write!(f, "Stream error: {}", e),
        }
    }
}

impl ::std::error::Error for ParseError {
    fn description(&self) -> &str {
        match self {
            &ParseError::Evaluation(_) => "evaluation error",
            &ParseError::UnexpectedEOF => "unexpected end of file",
            &ParseError::Syntax(_) => "syntax error",
            &ParseError::Stream(_) => "stream operation failed",
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

impl From<StreamError> for ParseError {
    fn from(e: StreamError) -> ParseError { ParseError::Stream(e) }
}

pub type Parse<T> = Result<T, ParseError>;

pub enum ParseContext {
    /// String contents, starting with the given partial contents
    StringContent(String),

    /// Symbol, starting with the given partial contents
    Symbol(String),

    /// Some other value-like element (number, boolean, etc)
    Value,

    /// Stringifiable object representing a file
    File,

    /// Other element
    Other
}

pub struct ParseOutput<T> {
    pub result: Parse<T>,
    pub context: ParseContext
}

impl<T> ParseOutput<T> {
    fn err(ctx: ParseContext, e: ParseError) -> Self {
        ParseOutput {
            result: Err(e),
            context: ctx
        }
    }

    fn generic_err(e: ParseError) -> Self {
        ParseOutput {
            result: Err(e),
            context: ParseContext::Other
        }
    }

    fn ok(ctx: ParseContext, res: T) -> Self {
        ParseOutput {
            result: Ok(res),
            context: ctx
        }
    }

    /// Map the error of one `ParseOutput` into another value type
    ///
    /// # Panics
    ///
    /// This will panic if called on a non-error object
    fn map_err<O>(self) -> ParseOutput<O> {
        let e = if let Err(e) = self.result {e}
                else {panic!("tried to map_err non-error result object")};

        ParseOutput {
            result: Err(e),
            context: self.context
        }
    }

    /// Map the success value through a function
    fn map<R, F: Fn(T) -> R>(self, f: F) -> ParseOutput<R> {
        ParseOutput {
            result: self.result.map(f),
            context: self.context
        }
    }

    fn or_else<F>(self, f: F) -> ParseOutput<T> where
            F: Fn(ParseContext, ParseError) -> ParseOutput<T> {
        match self.result {
            Ok(r) => ParseOutput {
                result: Ok(r),
                context: self.context
            },
            Err(e) => f(self.context, e)
        }
    }
}

/// Read a numeric value from the given input stream
/// 
/// Upon read failure, push the chars back into the peek stream
fn read_number<R: CharStream>(strm: &R) -> Parse<Number> {
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
                    .or_else(|e| if e.is_eof() { Ok(None) } else { Err(e) })?;
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

pub fn parse_number(x: &[u8]) -> ParseOutput<Number> {
    use std::io::Cursor;

    match read_number(&mut ReadWrapper::new(&mut Cursor::new(x))) {
        Ok(r)  => ParseOutput::ok(ParseContext::Other, r),
        Err(e) => ParseOutput::err(ParseContext::Other, e),
    }
}

fn valid_identifier_char(c: char) -> bool {
    c != '(' && c != ')' && c != '{' && c != '}' && !c.is_whitespace()
}

fn read_identifier<R: CharStream>(peek: &R) -> ParseOutput<Identifier> {
    let c = match peek.next() {
        Ok(r) => r,
        Err(e) => return ParseOutput::err(
            ParseContext::Symbol(String::from("")),
            ParseError::from(e))
    };

    let mut s = String::with_capacity(32);
    s.push(c);
    loop {
        let c = peek.peek()
                    .map(Some)
                    .or_else(|e| if e.is_eof() {Ok(None)} else {Err(e)});
        let c = match c {
            Ok(r) => r,
            Err(e) => return ParseOutput::err(ParseContext::Symbol(s),
                                              ParseError::from(e))
        };

        // stop the ident if we hit EOF
        let c = if let Some(c) = c { c } else { break };

        if valid_identifier_char(c) {
            s.push(c);
            match peek.next() {
                Ok(_) => {},
                Err(e) => return ParseOutput::err(ParseContext::Symbol(s),
                                                  ParseError::from(e))
            }
        } else {
            break;
        }
    }
    ParseOutput::ok(ParseContext::Symbol(s.clone()),
                    Identifier::from(s))
}

/// Reader configuration structure
struct ReadOptions {
    /// Automatically insert closing parens to make a complete structure
    autoclose: bool,

    /// Allow newlines
    newlines: bool
}

impl ReadOptions {
    /// Generate options suitable for normal use
    fn default() -> Self {
        ReadOptions {
            autoclose: false,
            newlines: true
        }
    }

    /// Generate options suitable for use in a pipeline
    fn pipeline() -> Self {
        ReadOptions {
            autoclose: false,
            newlines: false
        }
    }
}

/// Read a Lisp form from the given input stream
/// 
/// This returns either an evaluation failure or a (read value, unconsumed char)
/// pair. In most cases the unconsumed character can be ignored.
fn internal_read<R: CharStream>(peek: &R, conf: ReadOptions) -> ParseOutput<Value> {
    let mut stack = Vec::with_capacity(16);
    let table = READ_TABLE.read().unwrap();

    // maintain state for lambda shorthand
    let mut in_lambda = false;
    let mut lambda_args: i32 = 0; // -1 for a single $, 0 for unset, >0 for $N form

    let result = loop {
        // read a character
        let c = loop {
            let c = peek.peek().map(Some)
                   .or_else(|e| if e.is_eof() { Ok(None) } else { Err(e) });
            let c = match c {
                Ok(r) => r,
                Err(e) => return ParseOutput::err(ParseContext::Other,
                                                  ParseError::Stream(e))
            };
            if !c.map(|x| x.is_whitespace() &&
                          !(x == '\n' && !conf.newlines))
                 .unwrap_or(false) { break c }

            // skip whitespace
            match peek.next() {
                Ok(_) => {},
                Err(e) => return ParseOutput::err(ParseContext::Other,
                                                  ParseError::Stream(e))
            }
        };

        let c = if let Some(c) = c { c } else {
            // handle EOF
            break ParseOutput::err(ParseContext::Other,
                                   ParseError::UnexpectedEOF);
        };

        // if we got here, we're not allowing newlines - treat this just like
        // EOF
        if c == '\n' {
            break ParseOutput::err(ParseContext::Other,
                                   ParseError::UnexpectedEOF);
        }

        // use a reader macro if applicable
        let res = if let Some(_exec) = table.get(&c) {
            // use a reader macro
            unimplemented!()
        } else {
            if c == '(' {
                stack.push(ParseStackElement::List(Vec::new()));
                match peek.next() {
                    Ok(_) => {},
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::Stream(e))
                }
                continue;
            } else if c == ')' {
                let v =
                    if let Some(ParseStackElement::List(v)) = stack.pop() {v}
                    else { return ParseOutput::err(
                            ParseContext::Other,
                            ParseError::Syntax("unexpected ')'")) };
                match peek.next() {
                    Ok(_) => {},
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::Stream(e))
                }
                Value::list(v)
            } else if c == '{' {
                stack.push(ParseStackElement::Map(HashMap::new(), None));
                match peek.next() {
                    Ok(_) => {},
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::Stream(e))
                }
                continue;
            } else if c == '}' {
                let h =
                    if let Some(ParseStackElement::Map(h, None)) = stack.pop() {h}
                    else { return ParseOutput::err(
                            ParseContext::Other,
                            ParseError::Syntax("unexpected '}'")) };
                match peek.next() {
                    Ok(_) => {},
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::Stream(e))
                }
                Value::from(ValueData::Map(h))
            } else if c == '"' {
                match peek.next() {
                    Ok(_) => {},
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::Stream(e))
                }

                // read a quoted string
                let mut escaped = false;
                let mut s = String::with_capacity(32);
                loop {
                    let c = match peek.next() {
                        Ok(c) => c,
                        Err(e) => return ParseOutput::err(ParseContext::Other,
                                                          ParseError::Stream(e))
                    };
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
                match peek.next() {
                    Ok(_) => {},
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::Stream(e))
                }
                continue;
            } else if c == '$' {
                match peek.next() {
                    Ok(_) => {},
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::Stream(e))
                }
                let c = peek.peek().map(Some)
                            .or_else(|e| if e.is_eof() {Ok(None)} else {Err(e)});
                let c = match c {
                    Ok(c) => c,
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::Stream(e))
                };
                if let Some(c) = c {
                    if c == '(' {
                        if in_lambda {
                            return ParseOutput::err(
                                ParseContext::Other,
                                ParseError::Syntax(
                                    "lambda functions cannot be nested"));
                        }
                        in_lambda = true;
                        lambda_args = 0;
                        stack.push(ParseStackElement::Lambda);
                        stack.push(ParseStackElement::List(Vec::new()));
                        match peek.next() {
                            Ok(_) => {},
                            Err(e) => return ParseOutput::err(ParseContext::Other,
                                                              ParseError::Stream(e))
                        }
                        continue;
                    }
                }
                peek.push('$');

                let id = read_identifier(peek);
                if id.result.is_err() { return id.map_err(); }
                let id = id.result.unwrap();

                if id.as_ref() == "$" {
                    if in_lambda {
                        if lambda_args > 1 {
                            return ParseOutput::err(ParseContext::Other,
                                                    ParseError::Syntax(
                                        "mixing $ and $N forms is not allowed"));
                        }
                        lambda_args = -1;
                    }
                } else if id.as_ref().len() == 2 &&
                          id.as_ref().chars().next() == Some('$') {
                    let c = id.as_ref().chars().skip(1).next().unwrap();
                    if c.is_digit(10) && in_lambda {
                        if lambda_args < 0 {
                            return ParseOutput::err(ParseContext::Other,
                                ParseError::Syntax(
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
                    let r = read_identifier(peek);
                    if r.result.is_err() { return r.map_err(); }
                    Value::from(r.result.unwrap())
                }
            } else if c == '|' {
                // special handling for pipe symbols since literally any char
                // can be inside of a |x> separator
                match peek.next() {
                    Ok(_) => {},
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::Stream(e))
                }

                // read forward to see if there's a terminator
                let fwd = match peek.next() {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::Stream(e))
                };

                if fwd == '>' {
                    Value::from(Identifier::from(format!("|>")))
                } else {
                    let p = match peek.peek() {
                        Ok(r) => r,
                        Err(e) => return ParseOutput::err(ParseContext::Other,
                                                          ParseError::Stream(e))
                    };
                    if p == '>' {
                        match peek.next() {
                            Ok(_) => {},
                            Err(e) => return ParseOutput::err(ParseContext::Other,
                                                              ParseError::Stream(e))
                        }
                        Value::from(Identifier::from(format!("|{}>", fwd)))
                    } else {
                        peek.push(fwd);
                        Value::from(Identifier::from(format!("|")))
                    }
                }
            } else if c == ':' { // read atom if followed by something
                let c = match peek.next() {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::Stream(e))
                };
                let id = read_identifier(peek);
                if id.result.is_err() {return id.map_err();}
                let id = id.result.unwrap();

                if valid_identifier_char(c) {
                    Value::atom(id.0.as_ref())
                } else {
                    Value::from(id)
                }
            } else {
                let id = read_identifier(peek);
                if id.result.is_err() {return id.map_err();}
                let id = id.result.unwrap();

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
                Some(&mut ParseStackElement::Map(ref mut m, ref mut s)) => {
                    if let Some(k) = s.take() {
                        m.insert(k, res);
                    } else {
                        let h = match res.hash() {
                            Ok(r) => r,
                            Err(e) => return ParseOutput::err(
                                ParseContext::Other, ParseError::from(e))
                        };
                        if let Some(h) = h { *s = Some(h); }
                        else { return ParseOutput::err(
                                ParseContext::Other,
                                ParseError::from(
                                    EvalError::TypeError(String::from(
                                            "unhashable key in map literal"))));
                        }
                    }
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
                None => return ParseOutput::ok(ParseContext::Other, res)
            }
            stack.pop();
        }
    };

    if let Err(ParseError::UnexpectedEOF) = result.result {
        // handle autoclose
        if conf.autoclose {
            let mut res = None;
            while let Some(obj) = stack.pop() {
                match obj {
                    ParseStackElement::List(mut v) => {
                        if let Some(res) = res {
                            v.push(res);
                        }
                        res = Some(Value::list(v));
                    },
                    ParseStackElement::Map(mut m, mut s) => {
                        if let Some(res) = res {
                            if let Some(k) = s.take() {
                                m.insert(k, res);
                            }
                        }
                        res = Some(Value::map(m));
                    },
                    ParseStackElement::Quote => {
                        continue;
                    },
                    ParseStackElement::Lambda => {
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
                        res = Some(Value::list(vec![
                            Value::from(Identifier::new("fn")),
                            Value::list(args),
                            res.unwrap()
                        ]));
                    },
                }
            }
            if let Some(res) = res {
                return ParseOutput::ok(ParseContext::Other, res);
            }
        }
    }
    result
}

/// Read a Lisp form from the given input stream
/// 
/// This returns either an evaluation failure or a (read value, unconsumed char)
/// pair. In most cases the unconsumed character can be ignored.
pub fn read<R: CharStream>(strm: &R) -> ParseOutput<Value> {
    internal_read(strm, ReadOptions::default())
}

/// Read a Lisp form, autoclosing if needed
pub fn read_autoclose<R: CharStream>(strm: &mut R) -> ParseOutput<Value> {
    let mut opts = ReadOptions::default();
    opts.autoclose = true;
    internal_read(strm, opts)
}

/// Read a pipeline from the given input stream
/// 
/// This just reads forms from the input and processes them into a pipeline.
/// Reader macros run *before* this sees the forms.
pub fn read_pipeline<R: CharStream>(strm: &mut R) -> ParseOutput<Pipeline> {
    let mut pipeline = Pipeline {
        elements: Vec::new(),
        terminals: Vec::new()
    };

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
        let tok = internal_read(strm, ReadOptions::pipeline()).map(Some)
                 .or_else(|c,e| match e {
                     ParseError::UnexpectedEOF => ParseOutput::ok(
                         ParseContext::Other, None),
                     e => ParseOutput::err(c, e)
                 });
        if tok.result.is_err() { return tok.map_err(); }
        let tok = tok.result.unwrap();

        match s {
            PRS::S => {
                let tok = if let Some(tok) = tok { tok }
                          else { return ParseOutput::err(
                                  ParseContext::Value,
                                  ParseError::UnexpectedEOF) };
                let sym = match tok.get_symbol() {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::generic_err(ParseError::from(e))
                };
                if let Some(sym) = sym {
                    // check if it's a pipe separator
                    if get_pipe(&sym).is_some() {
                        return ParseOutput::err(
                            ParseContext::Symbol(sym.as_ref().to_owned()),
                            ParseError::Syntax("invalid pipeline"));
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
                    return ParseOutput::ok(ParseContext::Value, pipeline);
                };

                let sym = match tok.get_symbol() {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(ParseContext::Value,
                                                      ParseError::from(e))
                };
                if let Some(sym) = sym {
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
                          else { return ParseOutput::generic_err(
                                  ParseError::UnexpectedEOF) };
                let sym = match tok.get_symbol() {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::from(e))
                };

                if let Some(sym) = sym {
                    // check if it's a pipe separator
                    if get_pipe(&sym).is_some() {
                        return ParseOutput::err(
                            ParseContext::Other,
                            ParseError::Syntax("invalid pipeline"));
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
                          else { return ParseOutput::err(
                                  ParseContext::File,
                                  ParseError::UnexpectedEOF) };
                
                // evaluate the token and try converting to a string
                let res = match tok.evaluate(&::environment::empty()) {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(ParseContext::File,
                                                      ParseError::from(e))
                };
                let t = match res.into_str() {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(ParseContext::File,
                                                      ParseError::from(e))
                };
                pipeline.terminals.push(TerminalMode::ReplaceFile(t));
                if in_done { return ParseOutput::ok(ParseContext::File,
                                                    pipeline); }
                else { s = PRS::TerminalDone; out_done = true; }
            },
            PRS::OutAppendFile => {
                let tok = if let Some(tok) = tok { tok }
                          else { return ParseOutput::err(
                                  ParseContext::File,
                                  ParseError::UnexpectedEOF) };
                
                // evaluate the token and try converting to a string
                let res = match tok.evaluate(&::environment::empty()) {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(ParseContext::File,
                                                      ParseError::from(e))
                };
                let t = match res.into_str() {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(ParseContext::File,
                                                      ParseError::from(e))
                };
                pipeline.terminals.push(TerminalMode::AppendFile(t));
                if in_done { return ParseOutput::ok(ParseContext::File,
                                                    pipeline); }
                else { s = PRS::TerminalDone; out_done = true; }
            },
            PRS::OutReplaceVar => {
                let tok = if let Some(tok) = tok { tok }
                          else { return ParseOutput::err(
                                  ParseContext::Symbol(String::from("")),
                                  ParseError::UnexpectedEOF) };
                
                let sym = match tok.get_symbol() {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(
                        ParseContext::Symbol(String::from("")),
                        ParseError::from(e))
                };
                if let Some(sym) = sym {
                    pipeline.terminals.push(
                        TerminalMode::SetVariable(sym.clone()));
                    if in_done {
                        return ParseOutput::ok(
                            ParseContext::Symbol(sym.as_ref().to_owned()),
                            pipeline);
                    } else { s = PRS::TerminalDone; out_done = true; }
                } else {
                    return ParseOutput::err(
                        ParseContext::Symbol(String::from("")),
                        ParseError::Syntax("expected symbol as output target"));
                }
            },
            PRS::OutAppendVar => {
                let tok = if let Some(tok) = tok { tok }
                          else { return ParseOutput::err(
                                  ParseContext::Symbol(String::from("")),
                                  ParseError::UnexpectedEOF) };
                let sym = match tok.get_symbol() {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(
                        ParseContext::Symbol(String::from("")),
                        ParseError::from(e))
                };
                
                if let Some(sym) = sym {
                    pipeline.terminals.push(
                        TerminalMode::AppendVariable(sym.clone()));
                    if in_done {
                        return ParseOutput::ok(
                            ParseContext::Symbol(sym.as_ref().to_owned()),
                            pipeline);
                    } else { s = PRS::TerminalDone; out_done = true; }
                } else {
                    return ParseOutput::err(
                        ParseContext::Symbol(String::from("")),
                        ParseError::Syntax("expected symbol as output target"));
                }
            },
            PRS::InFile => {
                let tok = if let Some(tok) = tok { tok }
                          else { return ParseOutput::err(
                                  ParseContext::File,
                                  ParseError::UnexpectedEOF) };
                
                // evaluate the token and try converting to a string
                let res = match tok.evaluate(&::environment::empty()) {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(ParseContext::File,
                                                      ParseError::from(e))
                };
                let t = match res.into_str() {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(ParseContext::File,
                                                      ParseError::from(e))
                };
                pipeline.terminals.push(TerminalMode::InputFile(t));
                if out_done { return ParseOutput::ok(ParseContext::File,
                                                     pipeline); }
                else { s = PRS::TerminalDone; in_done = true; }
            },
            PRS::InVar => {
                let tok = if let Some(tok) = tok { tok }
                          else { return ParseOutput::err(
                                  ParseContext::Symbol(String::from("")),
                                  ParseError::UnexpectedEOF) };
                
                let sym = match tok.get_symbol() {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(
                        ParseContext::Symbol(String::from("")),
                        ParseError::from(e))
                };
                if let Some(sym) = sym {
                    pipeline.terminals.push(TerminalMode::InputVar(sym));
                    if out_done { return ParseOutput::ok(
                            ParseContext::Symbol(String::from("")),
                            pipeline); }
                    else { s = PRS::TerminalDone; in_done = true; }
                } else {
                    return ParseOutput::err(
                        ParseContext::Symbol(String::from("")),
                        ParseError::Syntax("expected symbol as output target"));
                }
            },
            PRS::TerminalDone => {
                let tok = if let Some(tok) = tok { tok }
                          else { return ParseOutput::ok(ParseContext::Other,
                                                        pipeline); };

                let sym = match tok.get_symbol() {
                    Ok(r) => r,
                    Err(e) => return ParseOutput::err(ParseContext::Other,
                                                      ParseError::from(e))
                };
                if let Some(sym) = sym {
                    let (out,st) =
                        if sym.as_ref() == ">" {(true,PRS::OutReplaceFile)}
                        else if sym.as_ref() == ">>" {(true,PRS::OutAppendFile)}
                        else if sym.as_ref() == ">=" {(true,PRS::OutReplaceVar)}
                        else if sym.as_ref() == ">>=" {(true,PRS::OutAppendVar)}
                        else if sym.as_ref() == "<" {(false,PRS::InFile)}
                        else if sym.as_ref() == "<=" {(false,PRS::InVar)}
                        else {
                            return ParseOutput::err(
                                ParseContext::Other,
                                ParseError::Syntax(
                                    "unexpected symbol in output position")); 
                        };
                    if out {
                        if out_done {
                            return ParseOutput::err(
                                ParseContext::Other,
                                ParseError::Syntax(
                                    "cannot specify two output targets"));
                        }
                        s = st;
                    } else {
                        if in_done {
                            return ParseOutput::err(
                                ParseContext::Other,
                                ParseError::Syntax(
                                    "cannot specify two input targets"));
                        }
                        s = st;
                    }
                } else {
                    return ParseOutput::err(
                        ParseContext::Other,
                        ParseError::Syntax("expected terminal or end of input"));
                }
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;
    use stream::ReadWrapper;

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
            let r = read_number(&mut ReadWrapper::new(&mut Cursor::new(i)));
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
                &mut ReadWrapper::new(&mut Cursor::new(&i[..])));
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
                &mut ReadWrapper::new(&mut Cursor::new(&i[..])),
                ReadOptions::default());
            assert_eq!(r.unwrap(), o);
        }
    }

    #[test]
    fn pipelines() {
        let s = &b"foo bar | baz bax |> foo |~> bax > bar < foo"[..];
        let r = read_pipeline(&mut ReadWrapper::new(&mut Cursor::new(&s[..])))
               .unwrap();
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
