/* hastur-internal APIs
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

#ifndef _HASTUR_H_
#define _HASTUR_H_
#include <stdbool.h>

#define HASTUR_OK 0
#define HASTUR_CWD_FAILED -1

struct hastur_config {
  int stdout;
  int stderr;
  bool use_old_builtin_rules;
};

/* Part of the public API. Returns an error code on failure */
int hastur_make_init(struct hastur_config *);
struct variable *hastur_lookup_variable_for_file(const char *data,
                                                 unsigned int length,
                                                 const char *file_name);

/* Things that we reexport from main.c */
void initialize_stopchar_map(void);
void initialize_global_hash_tables(void);
struct variable *define_makeflags(int all, int makefile);

#endif /* _HASTUR_H_ */
