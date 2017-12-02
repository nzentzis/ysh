use std::str;
use std::str::FromStr;

use nom::*;

use data::*;
use numeric::*;

named!(integer<i64>,
       map_res!(recognize!(do_parse!(opt!(one_of!("+-")) >> digit >> ())),
       |x| i64::from_str_radix(str::from_utf8(x).unwrap(), 10)));

named!(real<f64>,
       map_res!(recognize!(do_parse!(digit >> tag!(".") >>
                                     opt!(digit) >>
                                     opt!(do_parse!(tag_no_case!(b"e") >>
                                                    opt!(one_of!("+-")) >>
                                                    digit >> ())) >>
                                     ())),
                |x| f64::from_str(str::from_utf8(x).unwrap())));

// TODO: replace subparser calls with something else, since they don't handle
// signs properly (e.g. -2 + +3 i -> -2+3i or -3/-2 -> 3/2)
/// Parse a numeric value
named!(pub numeric_value<Value>,
       map!(alt_complete!(
               ws!(do_parse!(n:integer >> tag!("/") >> m:integer >>
                         (Number::rational(n,m)))) |
               ws!(do_parse!(a:real >> tag!("+") >> b:real >> tag!("i") >>
                         (Number::complex(a,b)))) |
               map!(real, Number::real) |
               map!(integer, Number::int)
               ),
            |x| Value::new(Value::Number(x)))
       );

#[cfg(test)]
mod tests {
    use super::*;
    use nom::IResult;

    #[test]
    fn test_numerics() {
        assert_eq!(numeric_value(b"12"),
            IResult::Done(&b""[..], Value::Number(Number::Integer(12))));
        assert_eq!(numeric_value(b"-12"),
            IResult::Done(&b""[..], Value::Number(Number::Integer(-12))));
        assert_eq!(numeric_value(b"+12"),
            IResult::Done(&b""[..], Value::Number(Number::Integer(12))));
        assert_eq!(numeric_value(b"12"),
            IResult::Done(&b""[..], Value::Number(Number::Integer(12))));
        assert_eq!(numeric_value(b"12/3"),
            IResult::Done(&b""[..], Value::Number(Number::Rational {
                num: 12, denom: 3 })));
        assert_eq!(numeric_value(b"2.2e3"),
            IResult::Done(&b""[..], Value::Number(Number::Real(2.2e3))));
        assert_eq!(numeric_value(b"2.2qq"),
            IResult::Done(&b"qq"[..], Value::Number(Number::Real(2.2))));
        assert_eq!(numeric_value(b"4. + 7.i"),
            IResult::Done(&b""[..], Value::Number(Number::Complex{a:4.,b:7.})));
    }
}
