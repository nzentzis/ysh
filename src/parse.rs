use std::str;
use std::sync::Arc;

use nom::*;
use data::*;

/// Match any valid UTF-8 code point
fn any_utf8(input: &[u8]) -> IResult<&[u8], char> {
    if input.is_empty() {
        IResult::Incomplete(Needed::Size(1))
    } else {
        // count the prefixing ones
        let mut num_ones = 0;
        let b = input[0];
        for i in 0..3 {
            // check that the bit N from the left is one
            if (b >> (7 - i)) & 1 == 0 { break }

            num_ones += 1;
        }

        // double check that it's valid
        if (b >> (7 - num_ones)) & 1 != 0 {
            return IResult::Error(ErrorKind::Custom(1));
        }

        // ensure that we have enough data
        let needed = if num_ones == 0 { 1 } else { num_ones };
        if input.len() < needed {
            return IResult::Incomplete(Needed::Size(needed - input.len()));
        }

        if let Ok(s) = str::from_utf8(&input[0..needed]) {
            if let Some(r) = s.chars().next() {
                IResult::Done(&input[needed..], r)
            } else {
                IResult::Error(ErrorKind::Custom(3))
            }
        } else {
            IResult::Error(ErrorKind::Custom(2))
        }
    }
}

/// over_chars!(&str -> IResult<&str, O>) => &[u8] -> IResult<&[u8], O>
/// 
/// Transform a parser over strings to a parser over bytes.
macro_rules! over_chars(
    ($i:expr, $submacro:ident!( $($args:tt)* )) => ({
        let data_: &[u8] = $i;
        let r_: IResult<&[u8], _> = match str::from_utf8(data_) {
            Ok(s_) => match $submacro!(s_, $($args)*) {
                IResult::Done(x_, r_) => {
                    // take the ending sequence
                    let begin_ = data_.len() - x_.as_bytes().len();
                    IResult::Done(&data_[begin_..], r_)
                },
                IResult::Incomplete(e_) => IResult::Incomplete(e_),
                IResult::Error(e_) => IResult::Error(e_),
            },
            Err(e_) => match e_.error_len() {
                None    => IResult::Incomplete(Needed::Size(1)),
                Some(l) => {
                    // try to decode a subsequence
                    let s_ = str::from_utf8(&data_[..e_.valid_up_to()]).unwrap();
                    match $submacro!(s_, $($args)*) {
                        IResult::Done(x_, r_) => {
                            // take the ending sequence
                            let begin_ = e_.valid_up_to() - x_.as_bytes().len();
                            IResult::Done(&data_[begin_..], r_)
                        },
                        IResult::Incomplete(_) =>
                            // this is an error, since decoding further would
                            // fail
                            IResult::Error(ErrorKind::Custom(4)),
                        IResult::Error(e) => IResult::Error(e)
                    }
                }
            }
        };
        r_
    });
    ($i:expr, $f: expr) => (
        over_chars!($i, call!($f));
    );
);

/// Parse a filename or filepath
/// 
/// The key difference here is that everything else can be escaped
fn filename(input: &[u8]) -> IResult<&[u8], String> {
    unimplemented!()
}

// match any valid identifier char. identifier chars are any non-whitespace
// character other than |, >, or parentheses
fn is_valid_ident_char(c: char) -> bool {
    (c.is_alphanumeric() ||
     !(c == '|' || c == '>' || c == ')' || c == '(')) &&
        !c.is_whitespace()
}

/// Parse an identifier
named!(ident<Identifier>, map!(over_chars!(take_while1_s!(is_valid_ident_char)),
                               |s| Identifier::new(s)));

named!(pipe_elem<PipeMode>, alt!(
        value!(PipeMode::PipeText, tag!(b"|>")) |
        do_parse!(
            tag!(b"|") >>
            c: any_utf8 >>
            tag!(b">") >>
            (PipeMode::DelimitedPipe(c))) |
        value!(PipeMode::Pipe, tag!(b"|"))));

named!(terminal_mode<TerminalMode>, alt!(
        do_parse!(tag!(b">") >> f: filename >> (TerminalMode::ReplaceFile(f))) |
        do_parse!(tag!(b">>") >> f: filename >> (TerminalMode::AppendFile(f))) |
        do_parse!(tag!(b">=") >> v: ident >> (TerminalMode::SetVariable(v))) |
        do_parse!(tag!(b">>=") >> v: ident >> (TerminalMode::AppendVariable(v))) |
        do_parse!(tag!(b"<") >> f: filename >> (TerminalMode::InputFile(f))) |
        do_parse!(tag!(b"<=") >> v: ident >> (TerminalMode::InputVar(v)))
        ));

#[cfg(test)]
mod tests {
    use super::*;
    use nom::IResult;

    #[test]
    fn test_pipe_elem() {
        assert_eq!(pipe_elem(b"| foo"), IResult::Done(&b" foo"[..], PipeMode::Pipe));
        assert_eq!(pipe_elem(b"|> foo"), IResult::Done(&b" foo"[..], PipeMode::PipeText));
        assert_eq!(pipe_elem(b"|=> foo"),
                   IResult::Done(&b" foo"[..], PipeMode::DelimitedPipe('=')));
        assert_eq!(pipe_elem("|±> foo".as_bytes()),
                   IResult::Done(&b" foo"[..], PipeMode::DelimitedPipe('±')));

        // test failures
        assert_eq!(pipe_elem("-> foo".as_bytes()), IResult::Error(ErrorKind::Alt));
    }

    #[test]
    fn test_identifier() {
        assert_eq!(ident(b"identifier"),
            IResult::Done(&b""[..], Identifier::new("identifier")));
        assert_eq!(ident(b"te/st | bar"),
            IResult::Done(&b" | bar"[..], Identifier::new("te/st")));
        assert_eq!(ident("te/s±t | bar".as_bytes()),
            IResult::Done(&b" | bar"[..], Identifier::new("te/s±t")));
    }
}
