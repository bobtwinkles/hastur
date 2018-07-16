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

use failure::Fail;
use native::{gmk_eval, gmk_floc, RUST_CODE_FILE_NAME};
use std::ffi::{CStr, CString, NulError};
use std::fmt;
use Make;

/// Evaluating a makefile snippet can fail for various reasons.
#[derive(Debug)]
pub enum EvaluationFailure {
    /// Converting the string from a Rust string to a C string failed.
    ConversionFail(NulError),
}

impl fmt::Display for EvaluationFailure {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl Fail for EvaluationFailure {}

impl Make {
    /// For when you have a C string already. You probably want [Make::eval_str]
    /// instead.
    pub fn eval_cstr(&mut self, snippet: impl AsRef<CStr>) -> Result<(), EvaluationFailure> {
        let snippet = snippet.as_ref();
        unsafe {
            let file_loc = gmk_floc {
                filenm: RUST_CODE_FILE_NAME,
                lineno: 0,
            };

            gmk_eval(snippet.as_ptr(), &file_loc);
        }

        Ok(())
    }

    pub fn eval_str(&mut self, snippet: impl Into<Vec<u8>>) -> Result<(), EvaluationFailure> {
        let c_str = CString::new(snippet).map_err(EvaluationFailure::ConversionFail)?;
        self.eval_cstr(c_str)
    }
}
