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
use native::hastur_lookup_variable_for_file;
use native::variable;
use std::ffi::{CStr, CString, NulError};
use std::fmt;
use std::marker::PhantomData;
use std::ptr;
use Make;

/// A Make variable.
pub struct Variable<'make> {
    /// Pointer to the actual variable
    native_var: *mut variable,

    /// how we hold a "lock" on the global make state
    _make_phantom: PhantomData<&'make variable>,
}

/// Looking up a variable failed
#[derive(Debug)]
pub enum VarLookupError {
    /// Failed to convert the variable name due to a null byte
    ConvertVarName(NulError),
    /// Failed to convert the file name due to a null byte
    ConvertFileName(NulError),
    /// Looking up the variable failed, probably because it doesn't exist
    LookupFailed,
}

impl fmt::Display for VarLookupError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl Fail for VarLookupError {}

impl Make {
    /// Get a variable from this instance of Make
    pub fn lookup_variable_for_file(
        &self,
        var_name: impl AsRef<str>,
        file_name: Option<impl AsRef<str>>,
    ) -> Result<Variable, VarLookupError> {
        let var_name = var_name.as_ref();
        let native_var = do_variable_lookup(var_name, file_name)?;
        if native_var.is_null() {
            return Err(VarLookupError::LookupFailed);
        }
        Ok(Variable {
            native_var,
            _make_phantom: PhantomData,
        })
    }
}

fn do_variable_lookup(
    var_name: &str,
    file_name: Option<impl AsRef<str>>,
) -> Result<*mut variable, VarLookupError> {
    let file_name = if let Some(file_name) = file_name {
        match CString::new(file_name.as_ref()) {
            Ok(o) => o.as_ptr(),
            Err(e) => return Err(VarLookupError::ConvertFileName(e)),
        }
    } else {
        ptr::null()
    };
    Ok(unsafe {
        hastur_lookup_variable_for_file(
            var_name.as_ptr() as (*const i8),
            var_name.len() as u32,
            file_name,
        )
    })
}

impl<'make> Variable<'make> {
    /// Get the contents of this variable
    pub fn get_contents(&self) -> Option<&CStr> {
        let value_ptr = unsafe { *self.native_var };
        if value_ptr.value.is_null() {
            None
        } else {
            Some(unsafe { CStr::from_ptr((*self.native_var).value) })
        }
    }
}

#[cfg(test)]
mod tests {
    use tests::get_test_make;

    #[test]
    fn test_round_trip() {
        let mut make = get_test_make();
        make.eval_str("A_VARIABLE := some value").unwrap();
        let var = make
            .lookup_variable_for_file("A_VARIABLE", None as Option<String>)
            .unwrap();
        let var_contents = var.get_contents().unwrap().to_str().unwrap();
        assert_eq!(var_contents, "some value");
    }

    #[test]
    fn test_recursive_expand() {
        let mut make = get_test_make();
        make.eval_str("A_VARIABLE = $(ANTOHER_VARIABLE)").unwrap();
        let var = make
            .lookup_variable_for_file("A_VARIABLE", None as Option<String>)
            .unwrap();
        let var_contents = var.get_contents().unwrap().to_str().unwrap();
        assert_eq!(var_contents, "$(ANTOHER_VARIABLE)");
    }
}
