use std::fmt;
use std::slice;
use std::ops::{Add,Sub,Mul,Div,Deref,DerefMut,Drop};
use std::hash::{Hash, Hasher};
use std::convert::From;
use std::os::raw::c_void;
extern crate libc;
mod rmath;
mod rRinternal;
use rRinternal::SEXP;

const  NILSXP     : u32 = 0;
// const  SYMSXP     : u32 = 1;
// const  LISTSXP    : u32 = 2;	   
// const  CLOSXP     : u32 = 3;
// const  ENVSXP     : u32 = 4;
// const  PROMSXP    : u32 = 5;
// const  LANGSXP    : u32 = 6;
// const  SPECIALSXP : u32 = 7;
// const  BUILTINSXP : u32 = 8;
const  CHARSXP	  : u32 = 9;
const  LGLSXP	  : u32 = 10;
const  INTSXP	  : u32 = 13;
const  REALSXP	  : u32 = 14;
const  CPLXSXP	  : u32 = 15;
const  STRSXP	  : u32 = 16;
// const  DOTSXP	  : u32 = 17;
// const  ANYSXP	  : u32 = 18;
const  VECSXP	  : u32 = 19;
// const  EXPRSXP	  : u32 = 20;
// const  BCODESXP   : u32 = 21;
// const  EXTPTRSXP  : u32 = 22;
// const  WEAKREFSXP : u32 = 23;
const  RAWSXP     : u32 = 24;
// const  S4SXP      : u32 = 25;
// const  NEWSXP     : u32 = 30;
// const  FREESXP    : u32 = 31;
// const  FUNSXP     : u32 = 99;

#[derive(Debug)]
pub enum Rtype {
        NILSXP,     
        SYMSXP,
        LISTSXP    ,
        CLOSXP     ,
        ENVSXP     ,
        PROMSXP    ,
        LANGSXP    ,
        SPECIALSXP ,
        BUILTINSXP ,
        CHARSXP	   ,
        LGLSXP	   ,
        INTSXP	   ,
        REALSXP	  ,
        CPLXSXP	  ,
        STRSXP	  ,
        DOTSXP	  ,
        ANYSXP	  ,
        VECSXP	  ,
        EXPRSXP	  ,
        BCODESXP   ,
        EXTPTRSXP  ,
        WEAKREFSXP ,
        RAWSXP     ,
        S4SXP      ,
        NEWSXP     ,
        FREESXP    ,
        FUNSXP     ,
        OTHERS
}


pub fn rtype(f: SEXP) -> Rtype {
        match unsafe { rRinternal::TYPEOF(f) as u32 } {
                NILSXP =>  Rtype::NILSXP,
                VECSXP => Rtype::VECSXP,
                LGLSXP => Rtype::LGLSXP,
                INTSXP => Rtype::INTSXP,
                REALSXP => Rtype::REALSXP,
                CPLXSXP => Rtype::CPLXSXP,
                STRSXP => Rtype::STRSXP,
                RAWSXP => Rtype::RAWSXP,
                CHARSXP => Rtype::CHARSXP,
                _ => Rtype::OTHERS
        }
}


#[derive(Debug)]
pub struct RUnexpectedType{ rtype: Rtype,}
pub struct RLengthMismtach;


pub trait Robject {
        type T;
        fn to_sexp(&self) -> rRinternal::SEXP ;
        fn t(&self) -> Rtype;
       
}

pub trait Rvector {
        type T;
        fn l(&self) -> usize;
        fn slice_to(&self, t: usize) -> Result<Self::T,RLengthMismtach> ;
        fn slice_from(&self, t: usize) -> Result<Self::T,RLengthMismtach> ;
        fn slice_include(&self, t1:usize, t2: usize) -> Result<Self::T,RLengthMismtach> ;
}        


//////////////////////////////////////////////////////////////////////
// NULL TYPE
//////////////////////////////////////////////////////////////////////

#[derive(Debug,Clone)]
pub struct Rnull;
impl Rnull {
        pub  fn create() -> Rnull {
                return Rnull;
        }
        pub fn to_sexp() -> SEXP {
                unsafe {
                        return rRinternal::R_NilValue;
                }
        }
}


impl Robject for Rnull {
        type T = Rnull;
        fn t(&self) -> Rtype {
               return Rtype::NILSXP;
        }
        fn to_sexp(&self) ->  rRinternal::SEXP {
                unsafe {
                        return rRinternal::R_NilValue;
                }
        }
}



pub trait Wrap<T,R: Robject> {
    fn from(T) -> R ;
}

//////////////////////////////////////////////////////////////////////
// Numeric Type
//////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct Rnumeric{
        _r: rRinternal::SEXP,
        _d: *mut f64,
        _l: usize,
}

impl Rnumeric {
        pub fn with_length(l : usize) -> Rnumeric {
                let r  = unsafe { rRinternal::Rf_allocVector(REALSXP as u32,l as isize) };
                return Rnumeric { _r: r, _d: unsafe{ rRinternal::REAL(r) }, _l: unsafe{ rRinternal::Rf_xlength(r) as usize } };
        }

        pub fn with_length_default(l : usize, f: f64) -> Rnumeric {
                let r  = unsafe { rRinternal::Rf_allocVector(REALSXP as u32,l as isize) };
                let mut p =  Rnumeric { _r: r, _d: unsafe{ rRinternal::REAL(r) }, _l: unsafe{ rRinternal::Rf_xlength(r) as usize}};
                for i in 0 .. p.l() {
                        p[i] = f;
                }
                return p;
        }

        pub fn with_sexp(r: rRinternal::SEXP) ->  Result<Rnumeric, RUnexpectedType>{
                match  rtype(r) {
                        Rtype::REALSXP  => {
                                unsafe { rRinternal::R_PreserveObject(r) };
                                let x = Rnumeric {
                                        _r: r,
                                        _d: unsafe{ rRinternal::REAL(r) },
                                        _l: unsafe{ rRinternal::Rf_xlength(r) as usize},
                                };
                                Ok(x)
                        },
                        _ => Err(RUnexpectedType { rtype:rtype(r) })
                }
        }
}

impl Robject for Rnumeric {
        type T = Rnumeric;
        fn t(&self) -> Rtype {
                return Rtype::REALSXP;  
        }
        fn to_sexp(&self) ->  rRinternal::SEXP {
                return self._r;
        }
}

impl Rvector for Rnumeric {
        type T = Rnumeric;
        fn l(&self) -> usize {
                return self._l as usize;
        }
        fn slice_to(&self, t: usize) -> Result<Self::T,RLengthMismtach> {
                if t > self._l {
                        Err(RLengthMismtach)
                } else {
                        let  mut l = Rnumeric::with_length( t +1);
                        for i in 0 .. t {
                                l[i] = self[i];
                                }
                        Ok(l)
                }
        }
        fn slice_from(&self, t: usize) -> Result<Self::T,RLengthMismtach> {
                if   t > self._l {
                        Err(RLengthMismtach)
                } else {
                        let  mut l = Rnumeric::with_length( self._l - t +1);
                        for i in t .. self._l {
                                l[ i-t]  = self[i];
                        }
                        Ok(l)
                }
        }
        fn slice_include(&self, t1: usize, t2:usize) -> Result<Self::T,RLengthMismtach> {
                if  t1> self._l || t2> self._l || t1 > t2 {
                        Err(RLengthMismtach)
                } else {
                        let  mut l = Rnumeric::with_length( t2-t1+1);
                        if t1 == t2 {
                                l[ 0 ] = self[t1];
                        }else {
                                for i in t1 .. t2 {
                                        l[ t1 - i ]  = self[i];
                                }
                        }
                        Ok(l)
                }
        }
}

impl Drop for Rnumeric {
        fn drop(&mut self){
                println!("Dropping {}", self);
                unsafe { rRinternal::R_ReleaseObject(self._r) };
        }
}
impl From<f64> for Rnumeric {
        fn from(f: f64) -> Self {
                let r  = unsafe { rRinternal::Rf_ScalarReal(f) };
                return Rnumeric { _r: r, _d: unsafe{ rRinternal::REAL(r) }, _l: unsafe{ rRinternal::Rf_xlength(r) as usize } };
        }
}
impl From<Vec<f64>> for Rnumeric {
        fn from(f: Vec<f64>) -> Self {
                let mut r  = Rnumeric::with_length(f.len());
                for i in 1 .. r.l() {
                        r[i] = f[i];
                }
                return r;
        }
}


impl fmt::Display for Rnumeric {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "Rnumeric[ r:{:?}, d:{:?}, l:{} ]", self._r as * const c_void,self._d ,self._l)
        }
}
impl Deref for Rnumeric {
        type Target = [f64];
        fn deref(&self) -> &[f64] {
                return unsafe { slice::from_raw_parts( rRinternal::REAL(self._r), self._l as usize) };
        }
}
impl DerefMut for Rnumeric {
        fn  deref_mut(&mut self) -> &mut [f64] {
                return unsafe { slice::from_raw_parts_mut( rRinternal::REAL(self._r), self._l as usize)  };
        }
}
impl Add<f64> for Rnumeric {
    type Output = Rnumeric;

        fn add(mut self, other: f64) -> Rnumeric {
                for i in 0 .. self.l() {
                        self[i] = self[i] + other ;
                }
                return self;
        }
}
impl Add<Rnumeric> for f64 {
    type Output = Rnumeric;

        fn add( self,mut other: Rnumeric) -> Rnumeric {
                for i in 0 .. other.l() {
                        other[i] = other[i] + self ;
                }
                return other;
        }
}

impl Sub<f64> for Rnumeric {
    type Output = Rnumeric;

        fn sub(mut self, other: f64) -> Rnumeric {
                for i in 0 .. self.l() {
                        self[i] = self[i] - other ;
                }
                return self;
        }
}
impl Sub<Rnumeric> for f64 {
    type Output = Rnumeric;

        fn sub( self,mut other: Rnumeric) -> Rnumeric {
                for i in 0 .. other.l() {
                        other[i] = self - other[i] ;
                }
                return other;
        }
}

impl Mul<f64> for Rnumeric {
    type Output = Rnumeric;

        fn mul(mut self, other: f64) -> Rnumeric {
                for i in 0 .. self.l() {
                        self[i] = self[i] * other ;
                }
                return self;
        }
}

impl Mul<Rnumeric> for f64 {
    type Output = Rnumeric;

        fn mul( self,mut other: Rnumeric) -> Rnumeric {
                for i in 0 .. other.l() {
                        other[i] = other[i] * self ;
                }
                return other;
        }
}


impl Div<f64> for Rnumeric {
    type Output = Rnumeric;

        fn div(mut self, other: f64) -> Rnumeric {
                for i in 0 .. self.l() {
                        self[i] = self[i] / other ;
                }
                return self;
        }
}

// See http://stackoverflow.com/questions/30218886/how-to-implement-iterator-and-intoiterator-for-a-simple-struct
impl IntoIterator for Rnumeric {
        type Item = f64;
        type IntoIter = RnumericIntoIterator;
        
        fn into_iter(self) -> Self::IntoIter {
                RnumericIntoIterator { o: self, index:0 }
        }
}


pub struct RnumericIntoIterator {
        o:   Rnumeric,
        index: usize,
}

impl Iterator for RnumericIntoIterator {
        type Item = f64;
        fn next(&mut self) -> Option<f64> {
                let result = if self.index < self.o._l {
                        // Some( unsafe { *self.o.offset(self.index as isize) })
                        Some( self.o[ self.index ])
                } else {
                        None
                };
                self.index += 1;
                result
        }
}

        
// Borrowed Iterator
impl<'a> IntoIterator for &'a Rnumeric {
        type Item = f64;
        type IntoIter = RnumericIterator<'a>;
        
        fn into_iter(self) -> Self::IntoIter {
                RnumericIterator { o: self, index:0 }
        }
}

pub struct RnumericIterator<'a> {
        o:   &'a Rnumeric,
        index: usize,
}

impl<'a> Iterator for RnumericIterator<'a> {
        type Item = f64;
        fn next(&mut self) -> Option<f64> {
                let result = if self.index < self.o._l {
                        // Some( unsafe { *self.o.offset(self.index as isize) })
                        Some( self.o[ self.index ])
                } else {
                        None
                };
                self.index += 1;
                result
        }
}

impl Hash for Rnumeric
{
        fn hash<H: Hasher>(&self, state: &mut H) {
                self._d.hash(state);
        }
}


impl Clone for Rnumeric
{
        fn clone(&self) -> Self {
                let r = unsafe { rRinternal::Rf_duplicate(self._r) };
                let l = Rnumeric{ _r: r,
                        _d: unsafe{ rRinternal::REAL(r) },
                        _l: unsafe{ rRinternal::Rf_xlength(r) as usize }};
                l
        }

        fn clone_from(&mut self, rhs: &Self) {
                self._r = unsafe { rRinternal::Rf_duplicate(rhs._r) };
                self._d = unsafe { rRinternal::REAL(self._r) };
                self._l = unsafe { rRinternal::Rf_xlength(rhs._r) as usize };
        }
}
        
// Taken from (and works for up to 20K elements
// https://danielkeep.github.io/tlborm/book/blk-counting.html
macro_rules! replace_expr {
    ($_t:tt $sub:expr) => {$sub};
}

macro_rules! count_tts {
    ($($tts:tt)*) => {<[()]>::len(&[$(replace_expr!($tts ())),*])};
}


macro_rules! rtypedvec {
        ($t:ident; $( $x:expr ),* ) => {
                {
                        let n: usize = count_tts!($($x)*);
                        let mut p = $t::with_length( n );
                        let mut i = 0;
                        $(
                          p[i] = $x;
                          i = i +1;
                        )*
                        p
                }
        };
       ( $t:ident; $elem:expr; $n:expr) => {
                {
                        let p = $t::with_length_default($n,$elem);
                        p
                }
        };
        ($t:ident; $n:expr;) => {
                {
                        let p = $t::with_length($n);
                        p
                }
        };
} 

macro_rules! realvec {
        (_;$n:expr) => {  rtypedvec![Rnumeric;$n;] };
        ($n:expr;$elem:expr) => {  rtypedvec![Rnumeric;$n;$elem] };
        ( $( $x:expr ),* ) => {  rtypedvec![Rnumeric;$($x),*] };
}



macro_rules! rrange {
        ($t:ident; => $x:expr) => {
                $t.slice_to($x);
        };
        ($t:ident; $x:expr => ) => {
                        $t.slice_from($x);
        };
        ($t:ident; $x:expr => $y:expr) => {
                $t.slice_include($x,$y);
        };
} 



#[no_mangle]
pub extern fn ex0(p0 : SEXP) ->  SEXP {
        return p0;
}

#[no_mangle]
pub extern fn exa(_: SEXP) -> SEXP {
        let p1 = realvec![_;10]; // numeric(10)
        return p1.to_sexp();
}

#[no_mangle]
pub extern fn exb(_: SEXP) -> SEXP {
        let  p3 = realvec![10.1,12.1,13.0,14.0]; //c(10.1,12.1,13.0,14.0)
        return p3.to_sexp();
}

#[no_mangle]
pub extern fn exc(_: SEXP) -> SEXP {
        let p4 = realvec![10.1]; // c(10.1)
        return p4.to_sexp();
}
#[no_mangle]
pub extern fn exd(_: SEXP) -> SEXP {
        let mut p2 = realvec![12.0;10]; // rep(12,10)
        p2[2] = 13.0;
        return p2.to_sexp();
}

#[no_mangle]
pub extern fn exe(p0: SEXP) -> SEXP {
        let mut p2 = realvec![2.0;3]; // rep(12,10)
        p2[2] = 1.0;
        p2 = 2.0-3.0*p2;
        for x in &p2{
                println!("{}",x);
        }
        p2.to_sexp()
        
}


#[no_mangle]
pub extern fn ex2(p0 : SEXP) ->  SEXP {
        match Rnumeric::with_sexp(p0) {
                Ok(mut p) => {
                        println!("{}",p);
                        // return p.to_sexp();
                        p[0] = 10.2;
                        return  Rnumeric::from(p[0] as f64).to_sexp();
                },
                Err(RUnexpectedType { rtype:t}) => {
                        println!("Found Wrong Type: {:?}",t);
                        return Rnull::to_sexp();
                }
        }
}


#[no_mangle]
pub extern fn ex3(_ : SEXP) ->  SEXP {
        let p = Rnumeric::from(4.56 as f64);
        let s = rrange![p; => 10];
        println!("{}",p);
        return p.to_sexp();
}
        

#[no_mangle]
pub extern fn hello_rust( p1:  *mut f64, p2:  *mut f64)  {
   unsafe {
    	        println!("We will compute with {} and {}", *p1,*p2);
    	        let  y = rmath::R_pow(*p1,*p2);
                *p1 = 30.0;
    	        println!("Hello, world! At the nice age of {}", y);
       }
}

#[no_mangle]
pub extern fn ex1(p0 : *const i32, p1 : *const i32) -> rRinternal::SEXP{
        let sxp : rRinternal::SEXP;
        unsafe {
                sxp = rRinternal::Rf_allocVector( *p0 as rRinternal::SEXPTYPE,  *p1 as rRinternal::R_xlen_t);
        }
        return sxp;
}
