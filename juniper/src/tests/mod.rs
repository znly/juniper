//! Library tests and fixtures

use futures::{Async, Future};

pub mod model;
mod schema;
#[cfg(test)]
mod query_tests;
#[cfg(test)]
mod introspection_tests;
#[cfg(test)]
mod type_info_tests;

#[cfg(test)]
pub fn get_immediate<F: Future<Item=T, Error=E>, T, E>(mut f: F) -> Result<T, E> {
    match f.poll() {
        Ok(Async::Ready(v)) => Ok(v),
        Ok(Async::NotReady) => panic!("Expected future to be synchronous, but it is not ready"),
        Err(e) => Err(e),
    }
}
