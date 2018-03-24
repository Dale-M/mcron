## Process this file with automake to produce Makefile.in.
# Copyright © 2003 Dale Mellor <dale_mellor@users.sourceforge.net>
# Copyright © 2015, 2016, 2017, 2018 Mathieu Lirzin <mthl@gnu.org>
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

## ---------- ##
## Programs.  ##
## ---------- ##

bin_PROGRAMS = bin/mcron

if MULTI_USER
bin_PROGRAMS += bin/crontab
sbin_PROGRAMS = bin/cron
else
noinst_PROGRAMS = bin/cron bin/crontab
endif

AM_CPPFLAGS = \
  -DPACKAGE_LOAD_PATH=\"$(guilesitedir)\" \
  -DPACKAGE_LOAD_COMPILED_PATH=\"$(guilesitegodir)\" \
  -D_GNU_SOURCE

AM_CFLAGS = @GUILE_CFLAGS@
LDADD = @GUILE_LIBS@ src/libmcron.a

bin_mcron_SOURCES = src/mcron.c
bin_mcron_DEPENDENCIES = $(compiled_modules) $(noinst_LIBRARIES)

bin_cron_SOURCES = src/cron.c
bin_cron_DEPENDENCIES = $(compiled_modules) $(noinst_LIBRARIES)

bin_crontab_SOURCES = src/crontab.c
bin_crontab_DEPENDENCIES = $(compiled_modules) $(noinst_LIBRARIES)

# wrapper to be used in the build environment and for running tests.
noinst_SCRIPTS = pre-inst-env

# local library.
noinst_LIBRARIES = src/libmcron.a
src_libmcron_a_SOURCES = \
  src/utils.c \
  src/utils.h

## --------------- ##
## Guile modules.  ##
## --------------- ##

# Root directory used for installing Guile modules.
guilesitedir = $(datarootdir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
# Root directory used for installing Guile compiled modules.
guilesitegodir = $(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

pkgmoduledir = $(guilesitedir)/$(PACKAGE)
pkgmodule_DATA = src/mcron/config.scm
dist_pkgmodule_DATA = \
  src/mcron/base.scm \
  src/mcron/environment.scm \
  src/mcron/job-specifier.scm \
  src/mcron/redirect.scm \
  src/mcron/utils.scm \
  src/mcron/vixie-specification.scm \
  src/mcron/vixie-time.scm

# Alias for 'src/mcron/base.scm' kept for backward compatibility.
dist_pkgmodule_DATA += src/mcron/core.scm

pkgmodulegodir = $(guilesitegodir)/$(PACKAGE)
pkgmodulego_DATA = \
  $(dist_pkgmodule_DATA:.scm=.go) \
  src/mcron/config.go

pkgscriptdir = $(pkgmoduledir)/scripts
dist_pkgscript_DATA = \
  src/mcron/scripts/cron.scm \
  src/mcron/scripts/crontab.scm \
  src/mcron/scripts/mcron.scm

pkgscriptgodir = $(pkgmodulegodir)/scripts
pkgscriptgo_DATA = $(dist_pkgscript_DATA:.scm=.go)

compiled_modules = \
  $(pkgmodulego_DATA) \
  $(pkgscriptgo_DATA)

CLEANFILES = $(compiled_modules)
DISTCLEANFILES = src/mcron/config.scm

# Unset 'GUILE_LOAD_COMPILED_PATH' altogether while compiling.  Otherwise, if
# $GUILE_LOAD_COMPILED_PATH contains $(pkgmoduledir), we may find .go files
# in there that are newer than the local .scm files (for instance because the
# user ran 'make install' recently).  When that happens, we end up loading
# those previously-installed .go files, which may be stale, thereby breaking
# the whole thing.  Set GUILE_AUTO_COMPILE to 0 to avoid auto-compiling guild
# as a consequence of the previous hack.
#
# XXX: Use the C locale for when Guile lacks
# <http://git.sv.gnu.org/cgit/guile.git/commit/?h=stable-2.0&id=e2c6bf3866d1186c60bacfbd4fe5037087ee5e3f>.
.scm.go:
	$(guilec_verbose)$(MKDIR_P) `dirname "$@"`; \
	export GUILE_AUTO_COMPILE=0; unset GUILE_LOAD_COMPILED_PATH; \
	LC_ALL=C \
	$(top_builddir)/pre-inst-env $(GUILD) compile \
	  --load-path="$(builddir)/src" \
	  --load-path="$(srcdir)/src" \
	  --warn=format --warn=unbound-variable --warn=arity-mismatch \
	  --target="$(host)" --output="$@" "$<" $(devnull_verbose)

## ------------ ##
## Test suite.  ##
## ------------ ##

TEST_EXTENSIONS = .scm .sh
AM_TESTS_ENVIRONMENT = env GUILE_AUTO_COMPILE='0'

SH_LOG_COMPILER = ./pre-inst-env $(SHELL)

SCM_LOG_DRIVER = \
  $(builddir)/pre-inst-env $(GUILE) \
  $(srcdir)/build-aux/test-driver.scm

TESTS = \
  tests/basic.sh \
  tests/schedule.sh \
  tests/base.scm \
  tests/environment.scm \
  tests/job-specifier.scm \
  tests/utils.scm

## -------------- ##
## Distribution.  ##
## -------------- ##

dist-hook: gen-ChangeLog
	$(AM_V_GEN)echo $(VERSION) > $(distdir)/.tarball-version

# Generate the ChangeLog file from Git commit logs.
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

EXTRA_DIST = \
  bootstrap \
  build-aux/guix.scm \
  HACKING \
  tests/init.sh \
  $(TESTS)

## -------------- ##
## Installation.  ##
## -------------- ##

# Sed command for Transforming program names.
transform_exe = s/$(EXEEXT)$$//;$(transform);s/$$/$(EXEEXT)/

if MULTI_USER
install-exec-hook:
	tcrontab=`echo crontab$(EXEEXT) | sed '$(transform_exe)'`; \
	chmod u+s $(DESTDIR)$(bindir)/$${tcrontab}
endif

installcheck-local:
## Check that only expected programs are installed and configured
	tmcron=`echo mcron$(EXEEXT) | sed '$(transform_exe)'`; \
	test -e $(DESTDIR)$(bindir)/$${tmcron}
if MULTI_USER
	tcrontab=`echo crontab$(EXEEXT) | sed '$(transform_exe)'`; \
	test -u $(DESTDIR)$(bindir)/$${tcrontab}
	tcron=`echo cron$(EXEEXT) | sed '$(transform_exe)'`; \
	test -e $(DESTDIR)$(sbindir)/$${tcron}
else !MULTI_USER
	tcrontab=`echo crontab$(EXEEXT) | sed '$(transform_exe)'`; \
	test ! -u $(DESTDIR)$(bindir)/$${tcrontab}
	tcron=`echo cron$(EXEEXT) | sed '$(transform_exe)'`; \
	test ! -f $(DESTDIR)$(sbindir)/$${tcron}
endif !MULTI_USER

## --------------- ##
## Documentation.  ##
## --------------- ##

info_TEXINFOS = doc/mcron.texi
doc_mcron_TEXINFOS = doc/fdl.texi
nodist_doc_mcron_TEXINFOS = doc/config.texi
dist_man_MANS = $(srcdir)/doc/mcron.1
extra_mans = \
  $(srcdir)/doc/crontab.1 \
  $(srcdir)/doc/cron.8

if MULTI_USER
dist_man_MANS += $(extra_mans)
else
# Build, distribute, but do not install the extra man pages.
all-local: $(extra_mans)
EXTRA_DIST += $(extra_mans)
endif

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

MAINTAINERCLEANFILES = $(dist_man_MANS) $(extra_mans)

## -------------- ##
## Silent rules.  ##
## -------------- ##

guilec_verbose = $(guilec_verbose_@AM_V@)
guilec_verbose_ = $(guilec_verbose_@AM_DEFAULT_V@)
guilec_verbose_0 = @echo "  GUILEC  " $@;

devnull_verbose = $(devnull_verbose_@AM_V@)
devnull_verbose_ = $(devnull_verbose_@AM_DEFAULT_V@)
devnull_verbose_0 = >/dev/null
