#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([mcron],[1.0.2],[dale_mellor@users.sourceforge.net])
AM_INIT_AUTOMAKE


AC_MSG_CHECKING([whether debugging is requested])
AC_ARG_ENABLE(debug,
              AS_HELP_STRING(--enable-debug,enable debugging and traceback on error),
              CONFIG_DEBUG=$enableval,
              CONFIG_DEBUG=no)
AC_MSG_RESULT($CONFIG_DEBUG)
if test "$CONFIG_DEBUG" = "no"; then
   CONFIG_DEBUG="#f"
else
   CONFIG_DEBUG="#t"
fi
AC_SUBST(CONFIG_DEBUG)


AC_PROG_AWK
AC_PROG_EGREP
AC_PROG_CC
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR


# Checks for programs.
  
AC_CHECK_PROGS(SED, sed)
if test "x$ac_cv_prog_SED" = "x"; then
   AC_MSG_ERROR(sed not found)
fi
AC_CHECK_PROGS(HEAD, head)
if test "x$ac_cv_prog_HEAD" = "x"; then
   AC_MSG_ERROR(head not found)
fi
AC_CHECK_PROGS(ED, ed)
if test "x$ac_cv_prog_ED" = "x"; then
   AC_MSG_ERROR(ed not found)
fi
AC_CHECK_PROGS(WHICH, which)
if test "x$ac_cv_prog_WHICH" = "x"; then
    AC_MSG_ERROR(which not found)
fi
AC_CHECK_PROGS(CP, cp)
if test "x$ac_cv_prog_CP" = "x"; then
    AC_MSG_ERROR(cp not found)
fi


# Check the Guile version.
        
AC_MSG_CHECKING(for guile version >= 1.6.4)
if [$GUILE --version | $HEAD -1 | $AWK '{print $2}' | \
     $EGREP -q '^(1\.(6\.([4-9]|[1-3][^.])|[7-9]|[1-5][^.])|[2-9])']; then 
   AC_MSG_RESULT(OK)
else
   AC_MSG_ERROR([Sorry, Guile 1.6.4 or greater is needed to run mcron])
fi

                                                           
# Now find a sendmail or equivalent.

AC_CHECK_PROGS(SENDMAIL, sendmail)
if test "x$ac_cv_prog_SENDMAIL" != "x"; then
   AC_MSG_CHECKING(sendmail path and arguments)
   ac_cv_prog_SENDMAIL="`$ac_cv_prog_WHICH sendmail` -FCronDaemon -odi -oem "
dnl  -or0s"
   AC_MSG_RESULT($ac_cv_prog_SENDMAIL)

else
   AC_CHECK_PROGS(SENDMAIL, mail)
   if test "x$ac_cv_prog_SENDMAIL" != "x"; then
      AC_MSG_CHECKING(mail path)
      ac_cv_prog_SENDMAIL="`$ac_cv_prog_WHICH sendmail` -d "
      AC_MSG_RESULT($ac_cv_prog_SENDMAIL)
   else
      AC_MSG_RESULT(No mail program found)
   fi
fi
SENDMAIL=$ac_cv_prog_SENDMAIL


# Configure the various files that mcron uses at runtime.

AC_MSG_CHECKING([which spool directory to use])
AC_ARG_WITH(spool-dir,
            AS_HELP_STRING(--with-spool-dir,the crontab spool directory (/var/cron/tabs)),
              CONFIG_SPOOL_DIR=$withval,
              CONFIG_SPOOL_DIR=[/var/cron/tabs])
AC_MSG_RESULT($CONFIG_SPOOL_DIR)
AC_SUBST(CONFIG_SPOOL_DIR)

AC_MSG_CHECKING([name of socket])
AC_ARG_WITH(socket-file,
            AS_HELP_STRING(--with-socket-file,unix pathname for cron socket (/var/cron/socket)),
              CONFIG_SOCKET_FILE=$withval,
              CONFIG_SOCKET_FILE=[/var/cron/socket])
AC_MSG_RESULT($CONFIG_SOCKET_FILE)
AC_SUBST(CONFIG_SOCKET_FILE)

AC_MSG_CHECKING([name of allow file])
AC_ARG_WITH(allow-file,
            AS_HELP_STRING(--with-allow-file,the file of allowed users (/var/cron/allow)),
              CONFIG_ALLOW_FILE=$withval,
              CONFIG_ALLOW_FILE=[/var/cron/allow])
AC_MSG_RESULT($CONFIG_ALLOW_FILE)
AC_SUBST(CONFIG_ALLOW_FILE)

AC_MSG_CHECKING([name of deny file])
AC_ARG_WITH(deny-file,
            AS_HELP_STRING(--with-deny-file,the file of barred users (/var/cron/deny)),
              CONFIG_DENY_FILE=$withval,
              CONFIG_DENY_FILE=[/var/cron/deny])
AC_MSG_RESULT($CONFIG_DENY_FILE)
AC_SUBST(CONFIG_DENY_FILE)

AC_MSG_CHECKING([name of PID file])
AC_ARG_WITH(pid-file,
            AS_HELP_STRING(--with-pid-file,the file to record cron's PID (/var/run/cron.pid)),
              CONFIG_PID_FILE=$withval,
              CONFIG_PID_FILE=[/var/run/cron.pid])
AC_MSG_RESULT($CONFIG_PID_FILE)
AC_SUBST(CONFIG_PID_FILE)

AC_MSG_CHECKING([directory to hold temporary files])
AC_ARG_WITH(tmp-dir,
            AS_HELP_STRING(--with-tmp-dir,directory to hold temporary files (/tmp)),
              CONFIG_TMP_DIR=$withval,
              CONFIG_TMP_DIR=[/tmp])
AC_MSG_RESULT($CONFIG_TMP_DIR)
AC_SUBST(CONFIG_TMP_DIR)



        
# This is to support `make DESTDIR=...'
                        
real_program_prefix=`echo $program_prefix | sed s/NONE//`
AC_SUBST(real_program_prefix)

        
AC_CONFIG_FILES(makefile config.scm mcron.texinfo)
AC_OUTPUT
