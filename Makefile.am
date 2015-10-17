## Process this file with automake to produce Makefile.in.

# Copyright (C) 2003 Dale Mellor
# Copyright (C) 2015, 2016 Mathieu Lirzin
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

bin_PROGRAMS = mcron
mcron_SOURCES = mcron.c
mcron_CFLAGS = @GUILE_CFLAGS@
mcron_DEPENDENCIES = $(GOBJECTS) # Build Guile modules before linking.
mcron_LDADD = @GUILE_LIBS@

MODULES =				\
  scm/mcron/environment.scm		\
  scm/mcron/job-specifier.scm		\
  scm/mcron/main.scm			\
  scm/mcron/redirect.scm		\
  scm/mcron/vixie-specification.scm	\
  scm/mcron/vixie-time.scm

GEN_MODULES =				\
  scm/mcron/config.scm			\
  scm/mcron/core.scm

CP = @CP@
# XXX: Prevent the 'configure' script to delete the 'core.*' files.
scm/mcron/core.scm: scm/mcron/mcron-core.scm
	$(CP) $< $@

GOBJECTS =				\
  $(GEN_MODULES:%.scm=%.go)		\
  $(MODULES:%.scm=%.go)

mcronmodule_DATA =			\
  $(GOBJECTS)				\
  $(GEN_MODULES)

dist_mcronmodule_DATA = 		\
  $(MODULES)				\
  scm/mcron/crontab.scm			\
  scm/mcron/mcron-core.scm

# Unset 'GUILE_LOAD_COMPILED_PATH' altogether while compiling.  Otherwise, if
# $GUILE_LOAD_COMPILED_PATH contains $(mcronmoduledir), we may find .go files
# in there that are newer than the local .scm files (for instance because the
# user ran 'make install' recently).  When that happens, we end up loading
# those previously-installed .go files, which may be stale, thereby breaking
# the whole thing.
#
# XXX: Use the C locale for when Guile lacks
# <http://git.sv.gnu.org/cgit/guile.git/commit/?h=stable-2.0&id=e2c6bf3866d1186c60bacfbd4fe5037087ee5e3f>.
.scm.go:
	$(AM_V_GUILEC)$(MKDIR_P) `dirname "$@"` ;			\
	unset GUILE_LOAD_COMPILED_PATH ;				\
	LC_ALL=C							\
	$(top_builddir)/pre-inst-env $(GUILD) compile			\
	  --load-path="$(top_builddir)/scm"				\
	  --load-path="$(top_srcdir)/scm"				\
	  --warn=format --warn=unbound-variable --warn=arity-mismatch	\
	  --target="$(host)" --output="$@" "$<"

SUFFIXES = .go
noinst_SCRIPTS = pre-inst-env

dist-hook: gen-ChangeLog

gen_start_date = 2015-06-26
.PHONY: gen-ChangeLog
gen-ChangeLog:
	$(AM_V_GEN)if test -d $(srcdir)/.git; then			\
	  log_fix="$(srcdir)/build-aux/git-log-fix";			\
	  test -e "$$log_fix"						\
	    && amend_git_log="--amend=$$log_fix"			\
	    || amend_git_log=;						\
	  $(top_srcdir)/build-aux/gitlog-to-changelog			\
	    $$amend_git_log --since=$(gen_start_date) > $(distdir)/cl-t && \
	    { rm -f $(distdir)/ChangeLog &&				\
	      mv $(distdir)/cl-t $(distdir)/ChangeLog; }		\
	fi

#full program prefix
fpp = $(DESTDIR)$(bindir)/@real_program_prefix@

install-exec-hook:
	@if [ "x@NO_VIXIE_CLOBBER@" != "xyes"   -a   "`id -u`" -eq "0" ]; then \
       rm -f $(fpp)cron$(EXEEXT) > /dev/null 2>&1; \
       $(INSTALL) --mode='u=rwx' mcron$(EXEEXT) $(fpp)cron$(EXEEXT); \
       rm -f $(fpp)crontab$(EXEEXT) > /dev/null 2>&1; \
       $(INSTALL) --mode='u=rwxs,og=rx' mcron$(EXEEXT) $(fpp)crontab$(EXEEXT); \
       $(INSTALL) -d --mode='u=rwx' $(DESTDIR)/var/cron; \
       $(INSTALL) -d --mode='u=rwx,og=rx' $(DESTDIR)/var/run; \
       $(INSTALL) -d --mode='u=rwx,og=rx' $(DESTDIR)@GUILE_SITE@; \
       $(INSTALL) -d --mode='u=rwx,og=rx' $(DESTDIR)@GUILE_SITE@/mcron; \
    elif [ "x@NO_VIXIE_CLOBBER@" = "xyes" ]; then \
       echo "Not installing Vixie-style programs"; \
    else \
       echo "+++ WARNING: NON-ROOT INSTALL: ONLY mcron WILL BE INSTALLED, NOT ANY OF THE VIXIE REPLACEMENT PROGRAMS"; \
    fi

uninstall-hook:
	if [ "`id -u`" -eq "0" ]; then \
        rm -f $(fpp){cron,crontab}$(EXEEXT); \
    fi

EXTRA_DIST = BUGS
CLEANFILES = $(GOBJECTS)
DISTCLEANFILES = $(GEN_MODULES)

## --------------- ##
## Documentation.  ##
## --------------- ##

info_TEXINFOS = doc/mcron.texi
doc_mcron_TEXINFOS = doc/fdl.texi
nodist_doc_mcron_TEXINFOS = doc/config.texi
dist_man_MANS = doc/mcron.1

# XXX: Allow the inclusion of 'doc/fdl.texi' and 'doc/config.texi' inside
# 'doc/mcron.texi' for 'dvi' and 'pdf' targets.
TEXI2DVI = texi2dvi -I doc

# Not part of formal package building, but a rule for manual use to get the
# elemental man page.  Will only work once the mcron program is installed.
doc/mcron.1: mcron.c
	-$(AM_V_HELP2MAN)$(MKDIR_P) `dirname "$@"` ; 			\
	$(top_builddir)/pre-inst-env $(HELP2MAN)			\
	  -n 'a program to run tasks at regular (or not) intervals'	\
	  ./mcron > $@

## -------------- ##
## Silent rules.  ##
## -------------- ##

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC  " $@;

AM_V_HELP2MAN = $(AM_V_HELP2MAN_$(V))
AM_V_HELP2MAN_ = $(AM_V_HELP2MAN_$(AM_DEFAULT_VERBOSITY))
AM_V_HELP2MAN_0 = @echo "  HELP2MAN" $@;
