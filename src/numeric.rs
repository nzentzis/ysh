use std::fmt;
use std::ops;

#[derive(Clone, PartialEq)]
/// A general numeric type
/// 
/// This implements a basic version of the Scheme numerical tower
pub enum Number {
    Integer(i64),
    Rational { // always stored in reduced form
        num: i64,
        denom: i64
    },
    Real(f64),
    Complex {a: f64, b: f64}
}

impl Number {
    /// Generate a new integer
    pub fn int(x: i64) -> Self {
        Number::Integer(x)
    }

    /// Generate a new rational from a/b
    pub fn rational(a: i64, b: i64) -> Self {
        Number::Rational {num: a, denom: b}
    }

    /// Generate a new real from a floating-point number
    pub fn real(x: f64) -> Self {
        Number::Real(x)
    }

    /// Generate a new complex number a+bi from real and imaginary components
    pub fn complex(a: f64, b: f64) -> Self {
        Number::Complex {a, b}
    }

    /// Cast up to a higher type to match another number
    /// 
    /// If this is a higher type than the other, then return it unmodified.
    /// Warning: this may reduce a precise value to an approximate one.
    fn cast_to_match(self, other: &Self) -> Self {
        match (self, other) {
            (Number::Integer(n), &Number::Integer(_)) =>
                Number::Integer(n),
            (Number::Integer(n), &Number::Rational{..}) =>
                Number::rational(n,1),
            (Number::Integer(n), &Number::Real(_)) =>
                Number::real(n as f64),
            (Number::Integer(n), &Number::Complex{..}) =>
                Number::complex(n as f64, 0.),
            (Number::Rational{num, denom}, &Number::Integer(_)) =>
                Number::rational(num, denom),
            (Number::Rational{num, denom}, &Number::Rational{..}) =>
                Number::rational(num, denom),
            (Number::Rational{num, denom}, &Number::Real(_)) =>
                Number::real(num as f64 / denom as f64),
            (Number::Rational{num, denom}, &Number::Complex{..}) =>
                Number::complex(num as f64 / denom as f64, 0.),
            (Number::Real(n), &Number::Integer(_)) =>
                Number::real(n),
            (Number::Real(n), &Number::Rational{..}) =>
                Number::real(n),
            (Number::Real(n), &Number::Real(_)) =>
                Number::real(n),
            (Number::Real(n), &Number::Complex{..}) =>
                Number::complex(n, 0.),
            (Number::Complex{a,b}, _) => Number::Complex{a,b}
        }
    }
}

impl fmt::Debug for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Number::Integer(n) => write!(f, "{}", n),
            &Number::Rational{num, denom} => write!(f, "{}/{}", num, denom),
            &Number::Real(x) => write!(f, "{}", x),
            &Number::Complex{a,b} => write!(f, "{} + {}i", a, b),
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Number::Integer(n) => write!(f, "{}", n),
            &Number::Rational{num, denom} => write!(f, "{}/{}", num, denom),
            &Number::Real(x) => write!(f, "{}", x),
            &Number::Complex{a,b} => write!(f, "{} + {}i", a, b),
        }
    }
}

impl ops::Add for Number {
    type Output = Number;

    fn add(self, mut rhs: Number) -> Number {
        let mut a = self.cast_to_match(&rhs);
        rhs = rhs.cast_to_match(&a);

        match (a,rhs) {
            (Number::Integer(a), Number::Integer(b)) =>
                Number::Integer(a+b),
            (Number::Rational{num,denom}, Number::Rational{num:a,denom:b}) => {
                unimplemented!()
            },
            (Number::Real(a), Number::Real(b)) =>
                Number::Real(a+b),
            (Number::Complex{a,b}, Number::Complex{a:x,b:y}) =>
                Number::Complex {a: a+x, b: b+y},
            _ => panic!("failed to cast numeric values")
        }
    }
}
