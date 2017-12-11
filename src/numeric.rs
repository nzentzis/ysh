use std::fmt;
use std::ops;
use std::cmp;
use std::borrow::Borrow;

#[derive(Clone, Copy)]
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

    /// Check whether the given number is an integer
    pub fn is_integer(&self) -> bool {
        match self {
            &Number::Integer(_) => true,
            _ => false
        }
    }

    /// Check whether the given number is an rational
    pub fn is_rational(&self) -> bool {
        match self {
            &Number::Rational {..} => true,
            _ => false
        }
    }

    /// Check whether the given number is an real
    pub fn is_real(&self) -> bool {
        match self {
            &Number::Real(_) => true,
            _ => false
        }
    }

    /// Check whether the given number is an complex
    pub fn is_complex(&self) -> bool {
        match self {
            &Number::Complex {..} => true,
            _ => false
        }
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
            &Number::Complex{a,..}  => a.round() as i64
        }
    }

    /// Simplify the number to a lower form
    /// 
    /// This will only perform simplification if it can be done without losing
    /// information.
    pub fn simplify(self) -> Number {
        match self {
            Number::Integer(i) => Number::Integer(i),
            Number::Rational{num,denom} => {
                if num % denom == 0 { Number::Integer(num/denom) }
                else {
                    let g = gcd(num,denom);
                    Number::Rational{num: num/g, denom: denom/g}
                }
            },
            Number::Real(f) => if f.fract() == 0. {
                Number::Integer(f.trunc() as i64)
            } else {
                Number::Real(f)
            },
            Number::Complex{a,b} => if b == 0. { Number::Real(a).simplify() }
                                    else { Number::Complex{a,b} }
        }
    }

    /// Simplify the number to a lower form in place
    /// 
    /// This has the same semantics as the `simplify` function
    pub fn simplify_inplace(&mut self) {
        match self {
            &mut Number::Rational{ref mut num, ref mut denom}
                if *num % *denom != 0 => {
                    let g = gcd(*num,*denom);
                    *num /= g;
                    *denom /= g;
                },
            &mut Number::Rational{num, denom} => {
                *self = Number::Integer(num / denom);
            },
            &mut Number::Real(f) =>
                if f.fract() == 0. {
                    *self = Number::Integer(f.trunc() as i64)
                },
            &mut Number::Complex{a, b} =>
                if b == 0. { *self = Number::Real(a).simplify() },
            _ => {}
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
        let a = self.cast_to_match(rhs);
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
        }.simplify()
    }
}

impl<A> ops::AddAssign<A> for Number where A: Borrow<Number> {
    fn add_assign(&mut self, rhs: A) {
        let rhs = rhs.borrow();
        *self = self.cast_to_match(rhs);
        let rhs = rhs.cast_to_match(&self);

        *self = *self + rhs;
        self.simplify_inplace();
    }
}

impl<A> ops::Sub<A> for Number where A: Borrow<Number> {
    type Output = Number;

    fn sub(self, rhs: A) -> Number {
        let rhs = rhs.borrow();
        let a = self.cast_to_match(rhs);
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
        }.simplify()
    }
}

impl<A> ops::SubAssign<A> for Number where A: Borrow<Number> {
    fn sub_assign(&mut self, rhs: A) {
        let rhs = rhs.borrow();
        *self = self.cast_to_match(rhs);
        let rhs = rhs.cast_to_match(&self);

        *self = *self - rhs;
        self.simplify_inplace();
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

impl<A> ops::Mul<A> for Number where A: Borrow<Number> {
    type Output = Number;

    fn mul(self, rhs: A) -> Number {
        let rhs = rhs.borrow();
        let a = self.cast_to_match(rhs);
        let rhs = rhs.cast_to_match(&a);

        match (a, rhs) {
            (Number::Integer(a), Number::Integer(b)) => Number::Integer(a*b),
            (Number::Rational{num:a,denom:b}, Number::Rational{num:c,denom:d})=>
                Number::Rational {num: a*c, denom: b*d},
            (Number::Real(a), Number::Real(b)) => Number::Real(a*b),
            (Number::Complex{a,b}, Number::Complex{a:c,b:d}) =>
                Number::Complex{a:a*c - b*d, b:a*d + c*b},
            _ => panic!("failed to cast numeric values")
        }.simplify()
    }
}

impl<A> ops::MulAssign<A> for Number where A: Borrow<Number> {
    fn mul_assign(&mut self, rhs: A) {
        let rhs = rhs.borrow();
        *self = self.cast_to_match(rhs);
        let rhs = rhs.cast_to_match(&self);

        *self = *self * rhs;
        self.simplify_inplace();
    }
}

impl<A> ops::Div<A> for Number where A: Borrow<Number> {
    type Output = Number;

    fn div(self, rhs: A) -> Number {
        let rhs = rhs.borrow();
        let a = self.cast_to_match(rhs);
        let rhs = rhs.cast_to_match(&a);

        match (a, rhs) {
            (Number::Integer(a), Number::Integer(b)) =>
                Number::Rational {num: a, denom: b},
            (Number::Rational{num:a,denom:b}, Number::Rational{num:c,denom:d})=>
                Number::Rational {num: a*d, denom: b*c},
            (Number::Real(a), Number::Real(b)) => Number::Real(a/b),
            //(Number::Complex{a,b}, Number::Complex{a:c,b:d}) =>
            //    unimplemented!("complex division not yet implemented"),
            _ => panic!("failed to cast numeric values")
        }.simplify()
    }
}

impl<A> PartialEq<A> for Number where A: Borrow<Number> {
    fn eq(&self, other: &A) -> bool {
        // upcast for testing
        let rhs = other.borrow();
        let a = self.cast_to_match(rhs);
        let rhs = rhs.cast_to_match(&a);

        match (a,rhs) {
            (Number::Integer(a), Number::Integer(b)) => a == b,
            (Number::Rational{num:a,denom:b}, Number::Rational{num:c,denom:d})=>{
                let l = lcm(b,d);
                (a*l) == (c*l)
            },
            (Number::Real(a), Number::Real(b)) => a == b,
            (Number::Complex{a,b}, Number::Complex{a:c,b:d}) => (a,b) == (c,d),
            _ => panic!("failed to cast numeric values")
        }
    }
}

impl<A> PartialOrd<A> for Number where A: Borrow<Number> {
    fn partial_cmp(&self, other: &A) -> Option<cmp::Ordering> {
        let rhs = other.borrow();

        // cannot assign ordering to complex numbers
        if rhs.is_complex() || self.is_complex() { return None; }

        // upcast for testing
        let a = self.cast_to_match(rhs);
        let rhs = rhs.cast_to_match(&a);

        match (a, rhs) {
            (Number::Integer(n), Number::Integer(m)) => Some(n.cmp(&m)),
            (Number::Rational{num:a,denom:b}, Number::Rational{num:c,denom:d})=>{
                let l = lcm(b,d);
                Some((a*(l/b)).cmp(&(c*(l/d))))
            },
            (Number::Real(n), Number::Real(m))=> n.partial_cmp(&m),
            _ => panic!("invalid attempt to compare complex numbers")
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_props() {
        let n1 = Number::int(1);
        let n2 = Number::rational(1, 2);
        let n3 = Number::real(1.5);
        let n4 = Number::complex(1., -1.);

        assert_eq!((n1.is_integer(),
                    n1.is_rational(),
                    n1.is_real(),
                    n1.is_complex()),
                   (true, false, false, false));
        assert_eq!((n2.is_integer(),
                    n2.is_rational(),
                    n2.is_real(),
                    n2.is_complex()),
                   (false, true, false, false));
        assert_eq!((n3.is_integer(),
                    n3.is_rational(),
                    n3.is_real(),
                    n3.is_complex()),
                   (false, false, true, false));
        assert_eq!((n4.is_integer(),
                    n4.is_rational(),
                    n4.is_real(),
                    n4.is_complex()),
                   (false, false, false, true));
    }

    #[test]
    fn addition() {
        let n1 = Number::int(1);
        let n2 = Number::rational(1, 2);
        let n3 = Number::real(1.5);
        let n4 = Number::complex(1., -1.);

        assert_eq!(n1+n1, Number::int(2));
        assert_eq!(n2+n2, Number::int(1));
        assert_eq!(n1+n2, Number::real(1.5));
        assert_eq!(n3+n3, Number::real(3.));
        assert_eq!(n1+n3, Number::real(2.5));
        assert_eq!(n4+n4, Number::complex(2., -2.));
        assert_eq!(n4+Number::complex(0.,1.), Number::int(1));

        let mut m = n1.clone();
        m += n1;
        assert_eq!(m, Number::int(2));

        m = n2.clone();
        m += n2;
        assert_eq!(m, Number::int(1));

        m = n1.clone();
        m += n2;
        assert_eq!(m, Number::real(1.5));

        m = n3.clone();
        m += n3;
        assert_eq!(m, Number::real(3.));

        m = n1.clone();
        m += n3;
        assert_eq!(m, Number::real(2.5));

        m = n4.clone();
        m += n4;
        assert_eq!(m, Number::complex(2., -2.));

        m = n4.clone();
        m += Number::complex(0., 1.);
        assert_eq!(m, Number::int(1));

    }

    #[test]
    fn subtraction() {
        let n1 = Number::int(1);
        let n2 = Number::rational(1, 2);
        let n3 = Number::real(1.5);
        let n4 = Number::complex(1., -1.);

        assert_eq!(n1-n1, Number::int(0));
        assert_eq!(n2-n2, Number::int(0));
        assert_eq!(n1-n2, Number::rational(1,2));
        assert_eq!(n3-n2, Number::int(1));
        assert_eq!(n4-n4, Number::int(0));
        assert_eq!(n4-Number::complex(0.,-1.), Number::int(1));

        let mut m = n1.clone();
        m -= n1;
        assert_eq!(m, Number::int(0));

        m = n2.clone();
        m -= n2;
        assert_eq!(m, Number::int(0));

        m = n1.clone();
        m -= n2;
        assert_eq!(m, Number::rational(1,2));

        m = n3.clone();
        m -= n2;
        assert_eq!(m, Number::int(1));

        m = n4.clone();
        m -= n4;
        assert_eq!(m, Number::int(0));

        m = n4.clone();
        m -= Number::complex(0., -1.);
        assert_eq!(m, Number::int(1));
    }

    #[test]
    fn negate() {
        assert_eq!(-Number::int(1), Number::int(-1));
        assert_eq!(-Number::real(1.), Number::real(-1.));
        assert_eq!(-Number::rational(1,2), Number::rational(-1,2));
        assert_eq!(-Number::complex(1.,2.), Number::complex(-1.,-2.));
    }

    #[test]
    fn multiply() {
        let n1 = Number::int(1);
        let n2 = Number::rational(1, 2);
        let n3 = Number::real(1.5);
        let n4 = Number::complex(1., -1.);

        assert_eq!(n1*n2, n2);
        assert_eq!(n2*n2, Number::rational(1,4));
        assert_eq!(n2*n3, Number::real(0.75));
        assert_eq!(n4*n4, Number::complex(0., -2.));

        let mut m = n1.clone();
        m *= n2; assert_eq!(m, n2);
        m = n2; m *= n2; assert_eq!(m, Number::rational(1,4));
        m = n2; m *= n3; assert_eq!(m, Number::real(0.75));
        m = n4; m *= n4; assert_eq!(m, Number::complex(0., -2.));
    }

    #[test]
    fn divide() {
        assert_eq!(Number::int(1)/Number::int(2), Number::rational(1,2));
        assert_eq!(Number::rational(1,2)/Number::rational(1,4), Number::int(2));
        assert_eq!(Number::real(2.)/Number::real(0.75), Number::real(2. + (2./3.)));
    }

    #[test]
    fn ord() {
        let i1 = Number::int(1);
        let i2 = Number::int(2);
        let r1 = Number::rational(2, 9);
        let r2 = Number::rational(2, 7);
        let f1 = Number::real(2.9);
        let f2 = Number::real(2.7);

        assert_eq!(i1.partial_cmp(&i2), Some(cmp::Ordering::Less));
        assert_eq!(r1.partial_cmp(&r2), Some(cmp::Ordering::Less));
        assert_eq!(f1.partial_cmp(&f2), Some(cmp::Ordering::Greater));
    }
}
