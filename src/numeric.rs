use std::fmt;
use std::ops;
use std::borrow::Borrow;

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
    fn cast_to_match(&self, other: &Self) -> Self {
        match (self, other) {
            (&Number::Integer(n), &Number::Integer(_)) =>
                Number::Integer(n),
            (&Number::Integer(n), &Number::Rational{..}) =>
                Number::rational(n,1),
            (&Number::Integer(n), &Number::Real(_)) =>
                Number::real(n as f64),
            (&Number::Integer(n), &Number::Complex{..}) =>
                Number::complex(n as f64, 0.),
            (&Number::Rational{num, denom}, &Number::Integer(_)) =>
                Number::rational(num, denom),
            (&Number::Rational{num, denom}, &Number::Rational{..}) =>
                Number::rational(num, denom),
            (&Number::Rational{num, denom}, &Number::Real(_)) =>
                Number::real(num as f64 / denom as f64),
            (&Number::Rational{num, denom}, &Number::Complex{..}) =>
                Number::complex(num as f64 / denom as f64, 0.),
            (&Number::Real(n), &Number::Integer(_)) =>
                Number::real(n),
            (&Number::Real(n), &Number::Rational{..}) =>
                Number::real(n),
            (&Number::Real(n), &Number::Real(_)) =>
                Number::real(n),
            (&Number::Real(n), &Number::Complex{..}) =>
                Number::complex(n, 0.),
            (&Number::Complex{a,b}, _) => Number::Complex{a,b}
        }
    }

    /// Round to the nearest integer, sacrificing precision if needed
    pub fn round(&self) -> i64 {
        match self {
            &Number::Integer(i)     => i,
            &Number::Rational{num,denom} => num/denom,
            &Number::Real(f)        => f.round() as i64,
            &Number::Complex{a, b}  => a.round() as i64
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

/// Compute the GCD of two integers using the Euclidean algorithm
fn gcd(a: i64, b: i64) -> i64 {
    let (mut k1, mut k2) = if a < b { (b, a) } else { (a, b) };

    loop {
        let q = k2 / k1;
        let r = k2 % k1;

        if r == 0 { break k1 }

        k2 = k1;
        k1 = r;
    }
}

/// Compute the LCM of two integers
fn lcm(a: i64, b: i64) -> i64 {
    let m = (a * b).abs();
    m / gcd(a,b)
}

impl<A> ops::Add<A> for Number where A: Borrow<Number> {
    type Output = Number;

    fn add(self, rhs: A) -> Number {
        let rhs = rhs.borrow();
        let mut a = self.cast_to_match(rhs);
        let rhs = rhs.cast_to_match(&a);

        match (a,rhs) {
            (Number::Integer(a), Number::Integer(b)) =>
                Number::Integer(a+b),
            (Number::Rational{num,denom}, Number::Rational{num:a,denom:b}) => {
                let d = lcm(denom, b);
                let n_x = (d/denom)*num;
                let n_y = (d/b)*a;
                let n = n_x + n_y;
                Number::Rational {
                    num: n,
                    denom: d
                }
            },
            (Number::Real(a), Number::Real(b)) =>
                Number::Real(a+b),
            (Number::Complex{a,b}, Number::Complex{a:x,b:y}) =>
                Number::Complex {a: a+x, b: b+y},
            _ => panic!("failed to cast numeric values")
        }
    }
}

impl<A> ops::AddAssign<A> for Number where A: Borrow<Number> {
    fn add_assign(&mut self, rhs: A) {
        let rhs = rhs.borrow();
        *self = self.cast_to_match(rhs);
        let rhs = rhs.cast_to_match(&self);

        match (self,rhs) {
            (&mut Number::Integer(ref mut a), Number::Integer(b)) => { *a += b },
            (&mut Number::Rational{ref mut num,ref mut denom},
             Number::Rational{num:a,denom:b}) => {
                let d = lcm(*denom, b);
                let n_x = (d / *denom) * (*num);
                let n_y = (d/b)*a;
                let n = n_x + n_y;

                *num = n;
                *denom = d;
            },
            (&mut Number::Real(ref mut a), Number::Real(b)) => { *a += b; },
            (&mut Number::Complex{ref mut a,ref mut b}, Number::Complex{a:x,b:y}) => {
                *a += x;
                *b += y;
            },
            _ => panic!("failed to cast numeric values")
        }
    }
}

impl<A> ops::Sub<A> for Number where A: Borrow<Number> {
    type Output = Number;

    fn sub(self, rhs: A) -> Number {
        let rhs = rhs.borrow();
        let mut a = self.cast_to_match(rhs);
        let rhs = rhs.cast_to_match(&a);

        match (a,rhs) {
            (Number::Integer(a), Number::Integer(b)) =>
                Number::Integer(a-b),
            (Number::Rational{num,denom}, Number::Rational{num:a,denom:b}) => {
                let d = lcm(denom, b);
                let n_x = (d/denom)*num;
                let n_y = (d/b)*a;
                let n = n_x - n_y;
                Number::Rational {
                    num: n,
                    denom: d
                }
            },
            (Number::Real(a), Number::Real(b)) =>
                Number::Real(a-b),
            (Number::Complex{a,b}, Number::Complex{a:x,b:y}) =>
                Number::Complex {a: a-x, b: b-y},
            _ => panic!("failed to cast numeric values")
        }
    }
}

impl<A> ops::SubAssign<A> for Number where A: Borrow<Number> {
    fn sub_assign(&mut self, rhs: A) {
        let rhs = rhs.borrow();
        *self = self.cast_to_match(rhs);
        let rhs = rhs.cast_to_match(&self);

        match (self,rhs) {
            (&mut Number::Integer(ref mut a), Number::Integer(b)) => { *a -= b },
            (&mut Number::Rational{ref mut num,ref mut denom},
             Number::Rational{num:a,denom:b}) => {
                let d = lcm(*denom, b);
                let n_x = (d / *denom) * (*num);
                let n_y = (d/b)*a;
                let n = n_x - n_y;

                *num = n;
                *denom = d;
            },
            (&mut Number::Real(ref mut a), Number::Real(b)) => { *a -= b; },
            (&mut Number::Complex{ref mut a,ref mut b}, Number::Complex{a:x,b:y}) => {
                *a -= x;
                *b -= y;
            },
            _ => panic!("failed to cast numeric values")
        }
    }
}

impl ops::Neg for Number {
    type Output = Number;

    fn neg(self) -> Number {
        match self {
            Number::Integer(x) => Number::Integer(-x),
            Number::Rational{num,denom} => Number::Rational{num:-num,denom},
            Number::Real(x) => Number::Real(-x),
            Number::Complex{a,b} => Number::Complex{a:-a,b:-b}
        }
    }
}
