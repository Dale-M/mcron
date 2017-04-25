/* mcron.c -- run jobs at scheduled times
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
#include <argp.h>
#include <libguile.h>

/* Forward declarations.  */
static void inner_main (void *closure, int argc, char *argv[]);
static SCM parse_args (int argc, char *argv[]);
static error_t parse_opt (int key, char *arg, struct argp_state *state);

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
  SCM config = parse_args (argc, argv);
  scm_set_current_module (scm_c_resolve_module ("mcron scripts mcron"));
  scm_call_1 (scm_variable_ref (scm_c_lookup ("main")), config);
}

/* Handle command line arguments.  */
static SCM
parse_args (int argc, char *argv[])
{
  static struct argp_option options[] = {
    {"schedule", 's', "N", 0,
     "Display the next N jobs that will be run"},
    {"daemon", 'd', 0, 0,
     "Run as a daemon process"},
    {"stdin", 'i', "FORMAT", 0,
     "Format of data passed as standard input or file arguments (default guile)"},
    {0, 0, 0, 0, 0}
  };

  static struct argp argp = {
    .options = options,
    .parser = parse_opt,
    .args_doc = "[FILE...]",
    .doc = "Run an mcron process according to the specifications in the "
    "FILE... (`-' for standard input), or use all the files in "
    "~/.config/cron (or the deprecated ~/.cron) with .guile or "
    ".vixie extensions."
  };

  SCM config = SCM_EOL;
  argp_program_version = PACKAGE_STRING;
  argp_program_bug_address = PACKAGE_BUGREPORT;
  argp_parse (&argp, argc, argv, 0, NULL, &config);

  return config;
}

static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  SCM *config = state->input;

  switch (key)
    {
    case 's':
      *config = scm_assq_set_x (*config, scm_from_utf8_symbol ("schedule"),
                                scm_from_int (atoi (arg)));
      break;
    case 'd':
      *config = scm_assq_set_x (*config, scm_from_utf8_symbol ("daemon"),
                                SCM_BOOL_T);
      break;
    case 'i':
      if (strncmp (arg, "vixie", 6) == 0)
        *config = scm_assq_set_x (*config, scm_from_utf8_symbol ("vixie"),
                                  SCM_BOOL_T);
      break;
    case ARGP_KEY_NO_ARGS:
      *config = scm_assq_set_x (*config, scm_from_utf8_symbol ("files"),
                                SCM_EOL);
      break;
    case ARGP_KEY_ARGS:
      {
        SCM lst = SCM_EOL;
        int filec = state->argc - state->next;
        char **filev = state->argv + state->next;

        for (int i = filec - 1; i >= 0; i--)
          lst = scm_cons (scm_from_locale_string (filev[i]), lst);

        *config = scm_assq_set_x (*config, scm_from_utf8_symbol ("files"),
                                  lst);
        break;
      }
    case ARGP_KEY_ARG:
    default:
      return ARGP_ERR_UNKNOWN;
    }
  return 0;
}
