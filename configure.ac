#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.57)
AC_INIT(mcron, 0.99.1, dale_mellor@users.sourceforge.net)
AM_INIT_AUTOMAKE


AC_MSG_CHECKING([whether debugging is requested])
AC_ARG_ENABLE(debug,
              AC_HELP_STRING([--enable-debug],
                             [enable debugging and traceback on error]),
              CONFIG_DEBUG=$enableval,
              CONFIG_DEBUG=no)
AC_MSG_RESULT($CONFIG_DEBUG)
if test "$CONFIG_DEBUG" = "no"; then
   CONFIG_DEBUG="#f"
else
   CONFIG_DEBUG="#t"
fi
AC_SUBST(CONFIG_DEBUG)


AC_MSG_CHECKING([whether the (broken) Vixie programs are requested])
AC_ARG_ENABLE(vixie,
              AC_HELP_STRING([--enable-vixie],
                            [enable installation of cron and crontab programs]),
                             CONFIG_ENABLE_VIXIE=$enableval,
                             CONFIG_ENABLE_VIXIE=no)
AC_MSG_RESULT($CONFIG_ENABLE_VIXIE)
AC_SUBST(CONFIG_ENABLE_VIXIE)


# We substitute the following commands into the makefile by configure, because
# automake is confused by the ifeq command.
  
IFEQ_VIXIE="ifeq (\$(CONFIG_ENABLE_VIXIE), yes)"
AC_SUBST(IFEQ_VIXIE)
ENDIF="endif"
AC_SUBST(ENDIF)

        
AC_PROG_CC
GUILE_PROGS
GUILE_FLAGS
                       
# Checks for programs.
AC_CHECK_PROGS(ED, ed)
if test "x$ac_cv_prog_ED" = "x"; then
   AC_MSG_ERROR(ed not found)
fi
AC_CHECK_PROGS(WHICH, which)
if test "x$ac_cv_prog_WHICH" = "x"; then
    AC_MSG_ERROR(which not found)
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

                
real_program_prefix=`echo $program_prefix | sed s/NONE//`
AC_SUBST(real_program_prefix)

        
AC_CONFIG_FILES(makefile config.scm)
AC_OUTPUT
