/* Hastur's main file
Copyright (C) 2018 Reed Koser
This file is NOT part of GNU Make.
GNU Make and Hastur are free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option) any
later version.

Hastur is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.  */

//! This is Hastur, named after the god of shepherds from the Cthulhu mythos.
//! The primary purpose of this crate is to provide a way to evaluate statements
//! in GNU Make syntax.
#![warn(missing_docs)]

#[macro_use]
extern crate failure;
extern crate fxhash;
#[cfg(test)]
#[macro_use]
extern crate lazy_static;

use failure::Fail;
use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};

/// Where we include the bindgen-generated bindings
mod native {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    #![allow(unused)]
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

mod variable;
mod eval;

pub use variable::Variable;

static MAKE_INITIALIZED: AtomicBool = AtomicBool::new(false);

/// Something went wrong initializing make
#[derive(Debug)]
pub enum MakeInitializationError {
    /// Someone else has already initialized Make
    AlreadyInitialized,
}

impl fmt::Display for MakeInitializationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl Fail for MakeInitializationError {}

/// This is the beating heart of Hastur. Only one instance can be created
pub struct Make {}

impl Make {
    /// Initialize all the global Make state, returning the unique handle to all
    /// Make state.
    pub fn new() -> Result<Make, MakeInitializationError> {
        // compare_and_swap returns the previous value, so if the CAS returns
        // true somebody else already initialized the library
        if MAKE_INITIALIZED.compare_and_swap(false, true, Ordering::AcqRel) {
            Err(MakeInitializationError::AlreadyInitialized)
        } else {
            unsafe {
                let mut hastur_config = native::hastur_config {
                    stdout: 0,
                    stderr: 1,
                    use_old_builtin_rules: false,
                };
                native::hastur_make_init(&mut hastur_config);
            }
            Ok(Make {})
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Mutex, MutexGuard};
    use Make;
    lazy_static! {
        static ref GLOBAL_TEST_MAKE: Mutex<Make> = Mutex::new(Make::new().unwrap());
    }

    pub fn get_test_make() -> MutexGuard<'static, Make> {
        GLOBAL_TEST_MAKE
            .lock()
            .expect("we can acquire the global make instance")
    }
}
