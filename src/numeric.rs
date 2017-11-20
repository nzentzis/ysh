use std::fmt;

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
