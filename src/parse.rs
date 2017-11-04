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

named!(pipe_elem<PipeMode>, alt!(
        value!(PipeMode::PipeText, tag!(b"|>")) |
        do_parse!(
            tag!(b"|") >>
            c: any_utf8 >>
            tag!(b">") >>
            (PipeMode::DelimitedPipe(c))) |
        value!(PipeMode::Pipe, tag!(b"|"))));

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
    }
}
