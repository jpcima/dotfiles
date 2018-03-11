;;;;
;;;; ecl-readline.lisp
;;;;
;;;; Copyright (C) 2008,2010 Jason Aquilina <jfa7600@yahoo.com.au>
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation; either version 2
;;;; of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;;;

;;;;
;;;; Readline support for ECL
;;;;
;;;; Ideas and code stolen from:
;;;;    CL-READLINE by Nikodemus Siivola <nikodemus@random-state.net>
;;;;    SB-READLINE by Miles Egan <miles@caddr.com>
;;;;

(in-package :ecl-readline)

(ffi:clines "#include <stdlib.h>")
(ffi:clines "#include <string.h>")
(ffi:clines "#include <stdio.h>")
(ffi:clines "#include <readline/readline.h>")

(ffi:clines "static char** g_completions = NULL;")
(ffi:clines "static int g_index = 0;");

(ffi:clines
"
static char* symbol_completer(const char* str, int target)
{
  static char** ptr  = NULL;   /* pointer into completions array */
  static int length = 0;       /* cached length of str         */

  if (target == 0) {
    ptr = g_completions;
    length = strlen(str);

    while (*ptr && strncmp(str, *ptr, length) != 0)
      ptr++;

    return *ptr ? strdup(*ptr) : NULL;
  }

  if (*ptr && strncmp(str, *ptr, length) == 0)
      return strdup(*ptr++);

  return NULL;
}
")

(defun rl-add-completion (completion)
  (ffi:c-inline (completion) (:cstring) :void
		"g_completions[g_index++] = strdup(#0)" :one-liner t))

(defun rl-allocate-completions (length)
  (ffi:c-inline (length) (:int) :void
		"
                 g_index = 0;
                 g_completions = malloc((#0 + 1) * sizeof(char*));
                 g_completions[#0] = NULL;
                "))

(defun rl-delete-completions ()
  (ffi:c-inline () () :void
		"
		 if (g_completions) {
                   char** p = g_completions;
                   while (*p)
                     free(*p++);

                   free(g_completions);
                   g_completions = NULL;
                 }
                "))

(defun rl-use-symbol-completer ()
  (ffi:c-inline () () :void
		"rl_completion_entry_function = symbol_completer" :one-liner t))

;;;; End of File


