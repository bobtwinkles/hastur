/* The C side of Hastur
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
#include "bindgen_wrapper.h"

#include "commands.h"

#include "hastur.h"

// String constant on the C side because it's annoying to do in Rust
const char * RUST_CODE_FILE_NAME = "<from Rust code>";

struct output hastur_output;

// Most of this function is stolen from main.c:main
int hastur_make_init(struct hastur_config *config) {
  PATH_VAR(current_directory);
  initialize_stopchar_map();
  initialize_global_hash_tables();
#ifdef WINDOWS32
  if (getcwd_fs(current_directory, GET_PATH_MAX) != 0)
#else
  if (getcwd(current_directory, GET_PATH_MAX) != 0)
#endif
    return HASTUR_CWD_FAILED;

  hastur_output.err = config->stdout;
  hastur_output.out = config->stderr;
  // Don't synchronize output. Maybe we should expose this, but you're not
  // actually trying to *run* the make job through Hastur, right?
  // This controls whether or not Make buffers output (the -o switch)
  hastur_output.syncout = 0;

  /* Initialize the special variables.  */
  define_variable_cname(".VARIABLES", "", o_default, 0)->special = 1;
  define_variable_cname(".RECIPEPREFIX", "", o_default, 0)->special = 1;
  define_variable_cname(".SHELLFLAGS", "-c", o_default, 0);
  define_variable_cname(".LOADED", "", o_default, 0);

  /* Set up .FEATURES
     Use a separate variable because define_variable_cname() is a macro and
     some compilers (MSVC) don't like conditionals in macros.  */
  {
    const char *features = "target-specific order-only second-expansion"
                           " else-if shortest-stem undefine oneshell"
#ifndef NO_ARCHIVES
                           " archives"
#endif
#ifdef MAKE_JOBSERVER
                           " jobserver"
#endif
#ifndef NO_OUTPUT_SYNC
                           " output-sync"
#endif
#ifdef MAKE_SYMLINKS
                           " check-symlink"
#endif
#ifdef HAVE_GUILE
                           " guile"
#endif
#ifdef MAKE_LOAD
                           " load"
#endif
        ;

    define_variable_cname(".FEATURES", features, o_default, 0);
  }

  // guile_gmake_setup(NILF);

  // XXX this is not implemented yet
  // NOTE: we do not automatically import environment variables
  // there should be a Rust-side API for setting things like that

  OUTPUT_SET(&hastur_output);

  define_variable_cname("CURDIR", current_directory, o_file, 0);

  /* Define the initial list of suffixes for old-style rules.  */
  set_default_suffixes();

  /* Define the file rules for the built-in suffix rules.  These will later
     be converted into pattern rules.  We used to do this in
     install_default_implicit_rules, but since that happens after reading
     makefiles, it results in the built-in pattern rules taking precedence
     over makefile-specified suffix rules, which is wrong.  */
  install_default_suffix_rules();

  /* Define some internal and special variables.  */
  define_automatic_variables();

  /* Set up the MAKEFLAGS and MFLAGS variables for makefiles to see.
     Initialize it to be exported but allow the makefile to reset it.  */
  define_makeflags(0, 0)->export = v_export;

  /* Define the default variables.  */
  define_default_variables();

  default_file = enter_file(strcache_add(".DEFAULT"));

  default_goal_var = define_variable_cname(".DEFAULT_GOAL", "", o_file, 0);

  return HASTUR_OK;
}

struct variable *hastur_lookup_variable_for_file(const char *data,
                                                 unsigned int length,
                                                 const char * file_name) {
  struct variable *tr;
  struct variable_set_list *savev;
  const floc *savef;

  // Save context
  savev = current_variable_set_list;
  savef = reading_file;

  // Make sure we're in the right context to do the lookup
  if (file_name) {
    struct file * f = lookup_file(file_name);
    if (!f) {
      tr = NULL;
    }
    // We're in a file-specific context
    current_variable_set_list = f->variables;
    if (f->cmds && f->cmds->fileinfo.filenm)
      reading_file = &f->cmds->fileinfo;
    else
      reading_file = 0;
  } else {
    // Assume the global setlist is current
    // XXX: don't just assume, actually set it
    // current_variable_set_list = &global_setlist;
  }

  // Do the lookup
  tr = lookup_variable(data, length);

  // Restore global state
  current_variable_set_list = savev;
  reading_file = savef;

  return tr;
}
