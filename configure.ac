## Process this file with autoconf to produce a configure script.
#
# Copyright © 2003, 2005, 2012, 2014 Dale Mellor <mcron-lsfnyl@rdmp.org>
# Copyright © 2015, 2016, 2017, 2018, 2020 Mathieu Lirzin <mthl@gnu.org>
# Copyright © 2018 宋文武 <iyzsong@member.fsf.org>
#
# This file is part of GNU Mcron.
#
# GNU Mcron is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# GNU Mcron is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Mcron.  If not, see <http://www.gnu.org/licenses/>.

AC_PREREQ([2.69])
AC_INIT([GNU Mcron],[1.2.1],[bug-mcron@gnu.org])
AC_CONFIG_SRCDIR([src/mcron/scripts/mcron.scm])
AC_CONFIG_AUX_DIR([build-aux])
AC_REQUIRE_AUX_FILE([test-driver.scm])

dnl We're fine with GNU make constructs, hence '-Wno-portability'.
AM_INIT_AUTOMAKE([1.11 gnu silent-rules subdir-objects color-tests
                  -Wall -Wno-override -Wno-portability std-options])

AM_SILENT_RULES([yes])		# Enables silent rules by default.

AC_CANONICAL_HOST

dnl We require pkg.m4 (from pkg-config) and guile.m4 (from Guile.)
dnl Make sure they are available when generating the configure script.
m4_pattern_forbid([^PKG_PROG])
m4_pattern_forbid([^PKG_CHECK])
m4_pattern_forbid([^GUILE_P])
m4_pattern_allow([^GUILE_PKG_ERRORS])
m4_pattern_allow([^GUILE_PKG])
m4_pattern_allow([^GUILE_PROGS])

# Check for latest Guile development files.
GUILE_PKG([3.0 2.2 2.0])

# Checks for programs.
GUILE_PROGS

AM_MISSING_PROG(HELP2MAN, help2man, $missing_dir)

# Let users choose the Mail Transfert Agent (MTA) of their choice.  Default to
# a non-absolute program name to make it a loose dependency resolved at
# runtime.
AC_ARG_WITH([sendmail],
  [AS_HELP_STRING([--with-sendmail=COMMAND],
    [command to read an email message from standard input, and send it])],
  [SENDMAIL="$withval"],
  [SENDMAIL="sendmail -t"])
AC_SUBST([SENDMAIL])

AC_ARG_ENABLE([multi-user],
  [AS_HELP_STRING([--disable-multi-user],
    [Don't Install legacy cron and crontab programs])],
  [enable_multi_user="$enableval"],
  [enable_multi_user="yes"])
AM_CONDITIONAL([MULTI_USER], [test "x$enable_multi_user" = xyes])

# Configure the various files that mcron uses at runtime.

AC_MSG_CHECKING([which spool directory to use])
AC_ARG_WITH(spool-dir,
            AS_HELP_STRING([--with-spool-dir],[the crontab spool directory (/var/cron/tabs)]),
            CONFIG_SPOOL_DIR=$withval,
            CONFIG_SPOOL_DIR=[/var/cron/tabs])
AC_MSG_RESULT($CONFIG_SPOOL_DIR)
AC_SUBST(CONFIG_SPOOL_DIR)

AC_MSG_CHECKING([name of socket])
AC_ARG_WITH(socket-file,
            AS_HELP_STRING([--with-socket-file],[unix pathname for cron socket (/var/cron/socket)]),
            CONFIG_SOCKET_FILE=$withval,
            CONFIG_SOCKET_FILE=[/var/cron/socket])
AC_MSG_RESULT($CONFIG_SOCKET_FILE)
AC_SUBST(CONFIG_SOCKET_FILE)

AC_MSG_CHECKING([name of allow file])
AC_ARG_WITH(allow-file,
            AS_HELP_STRING([--with-allow-file],[the file of allowed users (/var/cron/allow)]),
            CONFIG_ALLOW_FILE=$withval,
            CONFIG_ALLOW_FILE=[/var/cron/allow])
AC_MSG_RESULT($CONFIG_ALLOW_FILE)
AC_SUBST(CONFIG_ALLOW_FILE)

AC_MSG_CHECKING([name of deny file])
AC_ARG_WITH(deny-file,
            AS_HELP_STRING([--with-deny-file],[the file of barred users (/var/cron/deny)]),
            CONFIG_DENY_FILE=$withval,
            CONFIG_DENY_FILE=[/var/cron/deny])
AC_MSG_RESULT($CONFIG_DENY_FILE)
AC_SUBST(CONFIG_DENY_FILE)

AC_MSG_CHECKING([name of PID file])
AC_ARG_WITH(pid-file,
            AS_HELP_STRING([--with-pid-file],[the file to record cron's PID (/var/run/cron.pid)]),
            CONFIG_PID_FILE=$withval,
            CONFIG_PID_FILE=[/var/run/cron.pid])
AC_MSG_RESULT($CONFIG_PID_FILE)
AC_SUBST(CONFIG_PID_FILE)

AC_MSG_CHECKING([directory to hold temporary files])
AC_ARG_WITH(tmp-dir,
            AS_HELP_STRING([--with-tmp-dir],[directory to hold temporary files (/tmp)]),
            CONFIG_TMP_DIR=$withval,
            CONFIG_TMP_DIR=[/tmp])
AC_MSG_RESULT($CONFIG_TMP_DIR)
AC_SUBST(CONFIG_TMP_DIR)

# Include the Maintainer's Makefile fragment, if it's here.
MAINT_MAKEFILE=/dev/null
AS_IF([test -r "$srcdir/maint.mk"],
      [MAINT_MAKEFILE="$srcdir/maint.mk"])
AC_SUBST_FILE([MAINT_MAKEFILE])

AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
                [chmod +x pre-inst-env])
AC_CONFIG_FILES([doc/config.texi
                 Makefile
                 src/mcron/config.scm])
AC_OUTPUT
