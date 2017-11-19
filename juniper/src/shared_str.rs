use std::sync::Arc;
use std::{fmt, cmp, borrow, hash, ops, convert};

#[derive(Clone)]
pub struct SharedStr {
    string: Arc<String>,
    start: usize,
    end: usize,
}

impl SharedStr {
    pub fn clone_from_str(s: &str) -> SharedStr {
        SharedStr {
            string: Arc::new(s.to_owned()),
            start: 0,
            end: s.len(),
        }
    }

    pub fn as_str(&self) -> &str {
        &self.string[self.start..self.end]
    }

    pub fn to_string(&self) -> String {
        self.as_str().to_owned()
    }

    pub fn get_range(&self, start: usize, end: usize) -> SharedStr {
        assert!(self.start + start <= self.string.len());
        assert!(self.start + end <= self.string.len());
        assert!(end - start <= self.end - self.start);
        SharedStr {
            string: self.string.clone(),
            start: self.start + start,
            end: self.start + end,
        }
    }
}

impl borrow::Borrow<str> for SharedStr {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl convert::AsRef<str> for SharedStr {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Debug for SharedStr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}", self.as_str())
    }
}

impl fmt::Display for SharedStr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.as_str())
    }
}

impl cmp::PartialEq for SharedStr {
    fn eq(&self, rhs: &SharedStr) -> bool {
        cmp::PartialEq::eq(self.as_str(), rhs.as_str())
    }
}

impl cmp::Eq for SharedStr {}

impl<'a> cmp::PartialEq<&'a str> for SharedStr {
    fn eq(&self, rhs: &&'a str) -> bool { cmp::PartialEq::eq(self.as_str(), *rhs) }
}

impl<'a> cmp::PartialEq<SharedStr> for &'a str {
    fn eq(&self, rhs: &SharedStr) -> bool { cmp::PartialEq::eq(*self, rhs.as_str()) }
}

impl cmp::PartialEq<String> for SharedStr {
    fn eq(&self, rhs: &String) -> bool { cmp::PartialEq::eq(self.as_str(), rhs.as_str()) }
}

impl cmp::PartialEq<SharedStr> for String {
    fn eq(&self, rhs: &SharedStr) -> bool { cmp::PartialEq::eq(self.as_str(), rhs.as_str()) }
}

impl hash::Hash for SharedStr {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        hash::Hash::hash(self.as_str(), state)
    }
}

impl ops::Deref for SharedStr {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}
