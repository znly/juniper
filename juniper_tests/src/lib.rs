#[macro_use]
extern crate juniper;
#[macro_use]
extern crate juniper_codegen;
extern crate serde_json;

#[cfg(test)]
extern crate fnv;
#[cfg(test)]
extern crate futures;

mod codegen;

#[cfg(test)]
use futures::{Future, Async};

#[cfg(test)]
pub fn get_immediate<F: Future<Item=T, Error=E>, T, E>(mut f: F) -> Result<T, E> {
    match f.poll() {
        Ok(Async::Ready(v)) => Ok(v),
        Ok(Async::NotReady) => panic!("Expected future to be synchronous, but it is not ready"),
        Err(e) => Err(e),
    }
}
