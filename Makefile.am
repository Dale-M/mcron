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

bin_SCRIPTS = bin/mcron
noinst_SCRIPTS = 

if MULTI_USER
bin_SCRIPTS += bin/crontab
sbin_SCRIPTS = bin/cron
else
noinst_SCRIPTS += bin/cron bin/crontab
endif

# wrapper to be used in the build environment and for running tests.
noinst_SCRIPTS += pre-inst-env

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

CLEANFILES = $(compiled_modules) bin/crontab bin/cron bin/mcron
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


bin/% : src/%.in Makefile
	-@sed	-e 's,%PREFIX%,${prefix},g'				\
		-e 's,%modsrcdir%,${guilesitedir},g'			\
		-e 's,%modbuilddir%,${guilesitegodir},g'		\
		-e 's,%localstatedir%,${localstatedir},g'		\
		-e 's,%pkglibdir%,${pkglibdir},g'			\
		-e 's,%sysconfdir%,${sysconfdir},g'			\
		-e 's,%localedir%,${localedir},g'			\
		-e 's,%VERSION%,@VERSION@,g'				\
		-e 's,%PACKAGE_BUGREPORT%,@PACKAGE_BUGREPORT@,g'	\
		-e 's,%PACKAGE_NAME%,@PACKAGE_NAME@,g'			\
		-e 's,%PACKAGE_URL%,@PACKAGE_URL@,g'			\
		-e 's,%GUILE%,$(GUILE),g'				\
	   $< > $@;
	-@chmod a+x $@


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
  tests/schedule-2.sh \
  tests/base.scm \
  tests/environment.scm \
  tests/job-specifier.scm \
  tests/utils.scm \
  tests/vixie-time.scm

## -------------- ##
## Distribution.  ##
## -------------- ##

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

install-exec-hook:
if MULTI_USER
	tcrontab=`echo crontab | sed '$(transform_exe)'`; \
	chmod u+s $(DESTDIR)$(bindir)/$${tcrontab}
	tcron=`echo cron | sed '$(transform_exe)'`; \
	chmod u+s $(DESTDIR)$(sbindir)/$${tcron}
endif
	tmcron=`echo mcron | sed '$(transform_exe)'`;

installcheck-local:
## Check that only expected programs are installed and configured
	tmcron=`echo mcron | sed '$(transform_exe)'`; \
	test -e $(DESTDIR)$(bindir)/$${tmcron}
if MULTI_USER
	tcrontab=`echo crontab | sed '$(transform_exe)'`; \
	test -u $(DESTDIR)$(bindir)/$${tcrontab}
	tcron=`echo cron | sed '$(transform_exe)'`; \
	test -e $(DESTDIR)$(sbindir)/$${tcron}
else !MULTI_USER
	tcrontab=`echo crontab | sed '$(transform_exe)'`; \
	test ! -u $(DESTDIR)$(bindir)/$${tcrontab}
	tcron=`echo cron | sed '$(transform_exe)'`; \
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
	-@prog="bin/mcron"; man_section=1; $(gen_man)

$(srcdir)/doc/crontab.1: src/mcron/scripts/crontab.scm bin/crontab
	-@prog="bin/crontab"; man_section=1;	 $(gen_man)

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

## ------------- ##
## Maintenance.  ##
## ------------- ##

@MAINT_MAKEFILE@
