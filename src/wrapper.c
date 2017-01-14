/* wrapper.c -- C code booting Guile
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

/* This C code represents a thin wrapper around the Guile code of Mcron.  It
   is needed because the crontab personality requires SUID which is not
   permitted for executable scripts.  */

#include <libguile.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Forward declarations.  */
static void wrap_env_path (const char *envar, const char *dir);
static void inner_main (void *closure, int argc, char **argv);
static void react_to_terminal_signal (int sig);
static SCM set_cron_signals (void);

int
main (int argc, char **argv)
{
  /* Set Guile load paths to ensure that Mcron modules will be found after
     installation.  In a build environment let the 'pre-inst-env' script set
     the correct paths.  */
  if (getenv ("MCRON_UNINSTALLED") == NULL)
    {
      wrap_env_path ("GUILE_LOAD_PATH", PACKAGE_LOAD_PATH);
      wrap_env_path ("GUILE_LOAD_COMPILED_PATH", PACKAGE_LOAD_COMPILED_PATH);
    }

  scm_boot_guile (argc, argv, inner_main, 0);

  return EXIT_SUCCESS;
}

/* Append DIR in front of ENVAR environment variable value.  If ENVAR is not
   defined, then define it with DIR.  Bail out if something went wrong.  */
static void
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

/* Launch the Mcron Guile main program.  */
static void
inner_main (void *closure, int argc, char **argv)
{
  scm_set_current_module (scm_c_resolve_module ("mcron scripts " PROGRAM));
  /* Register the procedures to be called from Guile.  */
  scm_c_define_gsubr ("c-set-cron-signals", 0, 0, 0, set_cron_signals);
  /* Call main procedure.  */
  scm_call_0 (scm_variable_ref (scm_c_lookup ("main")));
}

/* Set up all the signal handlers as required by the cron personality.  This
   is necessary to perform the signal processing in C because the sigaction
   function won't work when called from Guile.  */
static SCM
set_cron_signals ()
{
  static struct sigaction sa;

  memset (&sa, 0, sizeof (sa));
  sa.sa_handler = react_to_terminal_signal;
  sigaction (SIGTERM, &sa, 0);
  sigaction (SIGINT,  &sa, 0);
  sigaction (SIGQUIT, &sa, 0);
  sigaction (SIGHUP,  &sa, 0);

  return SCM_BOOL_T;
}

/* Handle signal SIG and exit.  All signals that mcron handles will produce
   the same behavior so we don't need to use SIG in the implementation.  */
static void
react_to_terminal_signal (int sig)
{
  scm_c_eval_string ("(delete-run-file)");
  exit (EXIT_FAILURE);
}
