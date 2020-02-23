/* utils.c -- Utility functions.
   Copyright © 2017 Mathieu Lirzin <mthl@gnu.org>

   This file is part of GNU Mcron.

   GNU Mcron is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   GNU Mcron is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Mcron.  If not, see <http://www.gnu.org/licenses/>.  */

#include "utils.h"
#include <stdbool.h>
#include <stdio.h>

void
wrap_env_path (const char *envar, const char *dir)
{
  const char *path = getenv (envar);
  if (path == NULL)
    setenv (envar, dir, true);
  else
    {
      char *new_path;
      int ret = asprintf (&new_path, "%s:%s", dir, path);
      if (ret >= 0)
        setenv (envar, new_path, true);
      else
        {
          perror (envar);
          exit (EXIT_FAILURE);
        }
      free (new_path);
    }
}

void
assq_symbol_set_x (SCM *alst, const char *symbol, SCM val)
{
  *alst = scm_assq_set_x (*alst, scm_from_utf8_symbol (symbol), val);
}
