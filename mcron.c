/* mcron - run jobs at scheduled times

   Copyright (C) 2015, 2016 Mathieu Lirzin
   Copyright (C) 2003, 2014 Dale Mellor

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

/* This C code represents the thinnest possible wrapper around the Guile code
   which constitutes all the functionality of the mcron program.  There are
   two plus one reasons why we need to do this, and one very unfortunate
   consequence.

   * Firstly, SUID does not work on an executable script.  In the end, it is
     the execution of the translator, in our case guile, which determines the
     effective user, and it is not wise to make the system guile installation
     SUID root!

   * Secondly, executable scripts show up in ugly ways in listings of the
     system process table.  Guile in particular, with its multi-line
     #! ...\ \n -s ...!#
     idiosyncracies shows up in process listings in a way that is difficult
     to determine what program is actually running.

   * A third reason for the C wrapper which might be mentioned is that a
     security-conscious system administrator can choose to only install a
     binary, thus removing the possibility of a user studying a guile script
     and working out ways of hacking it to his own ends, or worse still
     finding a way to modify it to his own ends.

   * Unfortunately, running the guile script from inside a C program means
     that the sigaction function does not work.  Instead, it is necessary to
     perform the signal processing in C.  */

#include <libguile.h>
#include <signal.h>
#include <string.h>

/* Forward declarations.  */
void inner_main (void *closure, int argc, char **argv);
void react_to_terminal_signal (int sig);
SCM set_cron_signals (void);

int
main (int argc, char **argv)
{
  setenv ("GUILE_LOAD_PATH", GUILE_LOAD_PATH, 1);
  scm_boot_guile (argc, argv, inner_main, 0);

  return 0;
}

/* The effective main function (i.e. the one that actually does some work).
   We register the function above with the guile system, and then execute the
   mcron guile program.  */

void
inner_main (void *closure, int argc, char **argv)
{
  scm_set_current_module (scm_c_resolve_module ("mcron main"));
  scm_c_define_gsubr ("c-set-cron-signals", 0, 0, 0, set_cron_signals);
  scm_c_eval_string ("(main)");
}

/* This is a function designed to be callable from scheme, and sets up all the
   signal handlers required by the cron personality.  */

SCM
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

/* This is a function designed to be installed as a signal handler, for
   signals which are supposed to initiate shutdown of this program.  It calls
   the scheme procedure (see mcron.scm for details) to do all the work, and
   then exits.  */

void
react_to_terminal_signal (int sig)
{
  scm_c_eval_string ("(delete-run-file)");
  exit (1);
}
