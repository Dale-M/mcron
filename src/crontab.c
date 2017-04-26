/* crontab.c -- edit users' crontab files
   Copyright © 2003, 2014 Dale Mellor <dale_mellor@users.sourceforge.net>
   Copyright © 2015, 2016, 2017 Mathieu Lirzin <mthl@gnu.org>

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

/* Forward declarations.  */
static void inner_main (void *closure, int argc, char *argv[]);

int
main (int argc, char *argv[])
{
  /* Set Guile load paths to ensure that Mcron modules will be found after
     installation.  In a build environment let the 'pre-inst-env' script set
     the correct paths.  */
  if (getenv ("MCRON_UNINSTALLED") == NULL)
    {
      wrap_env_path ("GUILE_LOAD_PATH", PACKAGE_LOAD_PATH);
      wrap_env_path ("GUILE_LOAD_COMPILED_PATH", PACKAGE_LOAD_COMPILED_PATH);
    }

  scm_boot_guile (argc, argv, inner_main, NULL);

  return EXIT_SUCCESS;
}

/* Launch the Mcron Guile main program.  */
static void
inner_main (void *closure, int argc, char *argv[])
{
  scm_set_current_module (scm_c_resolve_module ("mcron scripts crontab"));
  scm_call_0 (scm_variable_ref (scm_c_lookup ("main")));
}
