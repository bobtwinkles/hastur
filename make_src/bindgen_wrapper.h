/* Aggregates all the things that Hastur might need to reexport
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
#if !defined(HASTUR)
# define HASTUR
#endif

#include <stdio.h>

#include "makeint.h"

// Headers containing APIs we want to export
#include "job.h"
#include "dep.h"
#include "variable.h"
#include "filedef.h"
#include "gnumake.h"

extern const char * RUST_CODE_FILE_NAME;

#include "hastur.h"
