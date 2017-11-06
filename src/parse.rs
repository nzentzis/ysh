use std::str;

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

// match any valid identifier char. identifier chars are any non-whitespace
// character other than |, >, or parentheses
fn is_valid_ident_char(c: char) -> bool {
    (c.is_alphanumeric() ||
     !(c == '|' || c == '>' || c == ')' || c == '(' || c == '"')) &&
        !c.is_whitespace()
}

/// Parse an identifier
named!(ident<Identifier>, map!(over_chars!(take_while1_s!(is_valid_ident_char)),
                               |s| Identifier::new(s)));

/// A quoted string which supports backslash escapes
named!(quoted_string<String>, map!(over_chars!(
        delimited!(tag_s!("\""), take_until_s!("\""), tag_s!("\""))),
        |x| x.to_owned()));

/// Parse a general identifier (i.e. a normal identifier or quoted string)
named!(gen_ident<String>, alt_complete!(
            quoted_string |
            map!(over_chars!(take_while1_s!(is_valid_ident_char)),
                 |x| x.to_owned())));

named!(pipe_elem<PipeMode>, ws!(alt_complete!(
        value!(PipeMode::PipeText, tag!(b"|>")) |
        do_parse!(
            tag!(b"|") >>
            c: any_utf8 >>
            tag!(b">") >>
            (PipeMode::DelimitedPipe(c))) |
        value!(PipeMode::Pipe, tag!(b"|")))));

named!(terminal_mode<TerminalMode>, ws!(alt_complete!(
        do_parse!(tag!(b">=") >> v: ident >> (TerminalMode::SetVariable(v))) |
        do_parse!(tag!(b">>=") >> v: ident >> (TerminalMode::AppendVariable(v))) |
        do_parse!(tag!(b">") >> f: gen_ident >> (TerminalMode::ReplaceFile(f))) |
        do_parse!(tag!(b">>") >> f: gen_ident >> (TerminalMode::AppendFile(f))) |
        do_parse!(tag!(b"<=") >> v: ident >> (TerminalMode::InputVar(v))) |
        do_parse!(tag!(b"<") >> f: gen_ident >> (TerminalMode::InputFile(f)))
        )));

named!(paren_list<Vec<Value>>,
       delimited!(tag!(b"("),
                  separated_nonempty_list_complete!(space, value),
                  tag!(b")")));

named!(value<Value>, alt_complete!(
        map!(alt!(value!(true, tag!(b"true")) |
                  value!(false, tag!(b"false"))),
             Value::Boolean) |
        map!(quoted_string, Value::Str) |
        map!(gen_ident, |s| {
            if s.chars().all(is_valid_ident_char) {
                Value::Symbol(Identifier::from(s))
            } else {
                Value::Str(s)
            }
        }) |
        map!(paren_list, Value::List)
        )
       );

named!(transformer<Transformer>,
        // basic transformer
        map!(separated_nonempty_list_complete!(space, value), Transformer));

named!(pub pipeline<Pipeline>,
       ws!(do_parse!(
           base: many0!(complete!(do_parse!(xform: transformer >> link: pipe_elem >>
                                  (PipelineComponent {xform, link: Some(link)})))) >>
           last: map!(transformer, |x| PipelineComponent {xform: x, link: None}) >>
           terms: separated_list_complete!(space, terminal_mode) >>
           ({
               let mut base = base;
               base.push(last);
               Pipeline { elements: base, terminals: terms } }))));

#[cfg(test)]
mod tests {
    use super::*;
    use nom::IResult;

    #[test]
    fn test_pipe_elem() {
        assert_eq!(pipe_elem(b"| foo"), IResult::Done(&b"foo"[..], PipeMode::Pipe));
        assert_eq!(pipe_elem(b"|> foo"), IResult::Done(&b"foo"[..], PipeMode::PipeText));
        assert_eq!(pipe_elem(b"|=> foo"),
                   IResult::Done(&b"foo"[..], PipeMode::DelimitedPipe('=')));
        assert_eq!(pipe_elem("|±> foo".as_bytes()),
                   IResult::Done(&b"foo"[..], PipeMode::DelimitedPipe('±')));

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

    #[test]
    fn test_terminal_modes() {
        assert_eq!(terminal_mode(b">foo"),
                   IResult::Done(&b""[..], TerminalMode::ReplaceFile(String::from("foo"))));
        assert_eq!(terminal_mode(b">>foo"),
                   IResult::Done(&b""[..], TerminalMode::AppendFile(String::from("foo"))));
        assert_eq!(terminal_mode(b">=ident"),
                   IResult::Done(&b""[..], TerminalMode::SetVariable(Identifier::new("ident"))));
        assert_eq!(terminal_mode(b">>=ident"),
                   IResult::Done(&b""[..], TerminalMode::AppendVariable(Identifier::new("ident"))));
        assert_eq!(terminal_mode(b"<file"),
                   IResult::Done(&b""[..], TerminalMode::InputFile(String::from("file"))));
        assert_eq!(terminal_mode(b"<=name"),
                   IResult::Done(&b""[..], TerminalMode::InputVar(Identifier::new("name"))));
    }

    #[test]
    fn test_values() {
        // booleans
        assert_eq!(value(b"truee"),
                   IResult::Done(&b"e"[..], Value::Boolean(true)));
        assert_eq!(value(b"false "),
                   IResult::Done(&b" "[..], Value::Boolean(false)));

        // strings
        assert_eq!(value(b"\"foo bar baz\""),
                   IResult::Done(&b""[..],
                                 Value::Str(String::from("foo bar baz"))));
        assert_eq!(value(b"\"foo\""),
                   IResult::Done(&b""[..], Value::Str(String::from("foo"))));
        //assert_eq!(value(b"\"foo"), IResult::Error(ErrorKind::Alt));

        // symbols
        assert_eq!(value(b"symbol!"),
                   IResult::Done(&b""[..],
                                 Value::Symbol(Identifier::new("symbol!"))));
        assert_eq!(value("add-±".as_bytes()),
                   IResult::Done(&b""[..],
                                 Value::Symbol(Identifier::new("add-±"))));
        assert_eq!(value("+".as_bytes()),
                   IResult::Done(&b""[..], Value::Symbol(Identifier::new("+"))));

        // homogeneous lists
        assert_eq!(value("(true false)".as_bytes()),
                   IResult::Done(&b""[..], Value::List(
                           vec![Value::Boolean(true), Value::Boolean(false)])));
        assert_eq!(value("(\"foo\" \"test\")".as_bytes()),
                   IResult::Done(&b""[..], Value::List(
                           vec![Value::Str(String::from("foo")),
                                Value::Str(String::from("test"))])));
        assert_eq!(value("(\"foo\" \"test\")".as_bytes()),
                   IResult::Done(&b""[..], Value::List(
                           vec![Value::Str(String::from("foo")),
                                Value::Str(String::from("test"))])));
        assert_eq!(value("(+ test)".as_bytes()),
                   IResult::Done(&b""[..], Value::List(
                           vec![Value::Symbol(Identifier::new("+")),
                                Value::Symbol(Identifier::new("test"))])));

        // mixed list
        assert_eq!(value("(+ true \"test\")".as_bytes()),
                   IResult::Done(&b""[..], Value::List(
                           vec![Value::Symbol(Identifier::new("+")),
                                Value::Boolean(true),
                                Value::Str(String::from("test"))])));
    }

    #[test]
    fn test_transformer() {
        assert_eq!(transformer("python main.py".as_bytes()), IResult::Done(&b""[..],
                   Transformer(vec![
                        Value::Symbol(Identifier::new("python")),
                        Value::Symbol(Identifier::new("main.py"))])));
        assert_eq!(transformer("(+ true \"hello\")".as_bytes()), IResult::Done(&b""[..],
                   Transformer(vec![Value::List(vec![
                        Value::Symbol(Identifier::new("+")),
                        Value::Boolean(true),
                        Value::Str(String::from("hello"))])])));
    }

    #[test]
    fn test_pipeline() {
        assert_eq!(pipeline("cat foo.txt | wc -l".as_bytes()), IResult::Done(&b""[..],
                    Pipeline {
                        elements: vec![
                            PipelineComponent {
                                xform: Transformer(vec![
                                    Value::Symbol(Identifier::new("cat")),
                                    Value::Symbol(Identifier::new("foo.txt"))]),
                                link: Some(PipeMode::Pipe)},
                            PipelineComponent {
                                xform: Transformer(vec![
                                    Value::Symbol(Identifier::new("wc")),
                                    Value::Symbol(Identifier::new("-l"))]),
                                link: None }],
                        terminals: vec![]}));

        assert_eq!(pipeline("cat foo.txt | wc -l > test".as_bytes()),
            IResult::Done(&b""[..],
                    Pipeline {
                        elements: vec![
                            PipelineComponent {
                                xform: Transformer(vec![
                                    Value::Symbol(Identifier::new("cat")),
                                    Value::Symbol(Identifier::new("foo.txt"))]),
                                link: Some(PipeMode::Pipe)},
                            PipelineComponent {
                                xform: Transformer(vec![
                                    Value::Symbol(Identifier::new("wc")),
                                    Value::Symbol(Identifier::new("-l"))]),
                                link: None }],
                        terminals: vec![TerminalMode::ReplaceFile(String::from("test"))]}));
    }
}
