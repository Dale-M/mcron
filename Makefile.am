## Process this file with automake to produce Makefile.in.
# Copyright © 2003 Dale Mellor <dale_mellor@users.sourceforge.net>
# Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
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

bin_PROGRAMS = bin/mcron bin/crontab
sbin_PROGRAMS = bin/cron

AM_CPPFLAGS = -DPACKAGE_LOAD_PATH=\"$(moduledir)\"
AM_CFLAGS = @GUILE_CFLAGS@
LDADD = @GUILE_LIBS@

bin_mcron_SOURCES = src/wrapper.c
bin_mcron_CPPFLAGS = $(AM_CPPFLAGS) -DPROGRAM="\"mcron\""
bin_mcron_DEPENDENCIES = $(modules:%.scm=%.go)

bin_cron_SOURCES = src/wrapper.c
bin_cron_CPPFLAGS = $(AM_CPPFLAGS) -DPROGRAM="\"cron\""
bin_cron_DEPENDENCIES = $(modules:%.scm=%.go)

bin_crontab_SOURCES = src/wrapper.c
bin_crontab_CPPFLAGS = $(AM_CPPFLAGS) -DPROGRAM="\"crontab\""
bin_crontab_DEPENDENCIES = $(modules:%.scm=%.go)

dist_mcronmodule_DATA = \
  src/mcron/base.scm \
  src/mcron/environment.scm \
  src/mcron/job-specifier.scm \
  src/mcron/redirect.scm \
  src/mcron/utils.scm \
  src/mcron/vixie-specification.scm \
  src/mcron/vixie-time.scm

mcronmodule_DATA = \
  $(dist_mcronmodule_DATA:%.scm=%.go) \
  src/mcron/config.scm \
  src/mcron/config.go

mcronscriptdir = $(mcronmoduledir)/scripts
dist_mcronscript_DATA = \
  src/mcron/scripts/cron.scm \
  src/mcron/scripts/crontab.scm \
  src/mcron/scripts/mcron.scm

mcronscript_DATA = $(dist_mcronscript_DATA:%.scm=%.go)

modules = \
  $(dist_mcronmodule_DATA) \
  $(dist_mcronscript_DATA) \
  src/mcron/config.scm

TEST_EXTENSIONS = .scm
AM_TESTS_ENVIRONMENT = env GUILE_AUTO_COMPILE='0'

SCM_LOG_DRIVER = \
  $(builddir)/pre-inst-env $(GUILE) \
  $(srcdir)/build-aux/test-driver.scm

TESTS = \
  tests/environment.scm \
  tests/job-specifier.scm

# Unset 'GUILE_LOAD_COMPILED_PATH' altogether while compiling.  Otherwise, if
# $GUILE_LOAD_COMPILED_PATH contains $(mcronmoduledir), we may find .go files
# in there that are newer than the local .scm files (for instance because the
# user ran 'make install' recently).  When that happens, we end up loading
# those previously-installed .go files, which may be stale, thereby breaking
# the whole thing.  Set GUILE_AUTO_COMPILE to 0 to avoid auto-compiling guild
# as a consequence of the previous hack.
#
# XXX: Use the C locale for when Guile lacks
# <http://git.sv.gnu.org/cgit/guile.git/commit/?h=stable-2.0&id=e2c6bf3866d1186c60bacfbd4fe5037087ee5e3f>.
.scm.go:
	$(guilec_verbose)$(MKDIR_P) `dirname "$@"` ;			\
	export GUILE_AUTO_COMPILE=0 ; unset GUILE_LOAD_COMPILED_PATH ;	\
	LC_ALL=C							\
	$(top_builddir)/pre-inst-env $(GUILD) compile			\
	  --load-path="$(top_builddir)/src"				\
	  --load-path="$(top_srcdir)/src"				\
	  --warn=format --warn=unbound-variable --warn=arity-mismatch	\
	  --target="$(host)" --output="$@" "$<" $(devnull_verbose)

SUFFIXES = .go
noinst_SCRIPTS = pre-inst-env

BUILT_SOURCES= .version
.version:
	$(AM_V_GEN)echo $(VERSION) > $@-t && mv $@-t $@

dist-hook: gen-ChangeLog
	$(AM_V_GEN)echo $(VERSION) > $(distdir)/.tarball-version

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

EXTRA_DIST = \
  .version \
  $(TESTS)

MAINTAINERCLEANFILES = $(dist_man_MANS)
DISTCLEANFILES = src/config.scm
CLEANFILES = \
  $(modules:%.scm=%.go) \
  src/mcron/config.go

## --------------- ##
## Documentation.  ##
## --------------- ##

info_TEXINFOS = doc/mcron.texi
doc_mcron_TEXINFOS = doc/fdl.texi
nodist_doc_mcron_TEXINFOS = doc/config.texi
dist_man_MANS = \
  $(srcdir)/doc/mcron.1 \
  $(srcdir)/doc/crontab.1 \
  $(srcdir)/doc/cron.8

# XXX: Allow the inclusion of 'doc/fdl.texi' and 'doc/config.texi' inside
# 'doc/mcron.texi' for 'dvi' and 'pdf' targets.
TEXI2DVI = texi2dvi -I doc

# The 'case' ensures the man pages are only generated if the corresponding
# source script (the first prerequisite) has been changed.  The second
# prerequisites is solely meant to force these docs to be made only after
# executables have been compiled.
gen_man = \
  case '$?' in \
    *$<*) $(AM_V_P) && set -x || echo "  HELP2MAN $@"; \
          LANGUAGE= $(top_builddir)/pre-inst-env $(HELP2MAN) \
          -s $$man_section -S GNU -p $(PACKAGE_TARNAME) -o $@ $$prog;; \
    *)    : ;; \
  esac

$(srcdir)/doc/mcron.1: src/mcron/scripts/mcron.scm bin/mcron
	-@prog="mcron"; man_section=1; $(gen_man)

$(srcdir)/doc/crontab.1: src/mcron/scripts/crontab.scm bin/crontab
	-@prog="crontab"; man_section=1; $(gen_man)

$(srcdir)/doc/cron.8: src/mcron/scripts/cron.scm bin/cron
	-@prog="cron"; man_section=8; $(gen_man)

## -------------- ##
## Silent rules.  ##
## -------------- ##

guilec_verbose = $(guilec_verbose_@AM_V@)
guilec_verbose_ = $(guilec_verbose_@AM_DEFAULT_V@)
guilec_verbose_0 = @echo "  GUILEC  " $@;

devnull_verbose = $(devnull_verbose_@AM_V@)
devnull_verbose_ = $(devnull_verbose_@AM_DEFAULT_V@)
devnull_verbose_0 = >/dev/null
