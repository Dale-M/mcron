## Maintainer-only Makefile fragment
# Copyright © 2018 Mathieu Lirzin <mthl@gnu.org>
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

# Rebuild Makefile.in if this file is modifed.
Makefile.in: maint.mk

## -------------------- ##
##  Third-party files.  ##
## ---------------------##

WGET = wget

# Git repositories on Savannah.
git_sv_host = git.savannah.gnu.org

# Some repositories we sync files from.
sv_git_am = 'https://$(git_sv_host)/gitweb/?p=automake.git;a=blob_plain;hb=HEAD;f='
sv_git_gl = 'https://$(git_sv_host)/gitweb/?p=gnulib.git;a=blob_plain;hb=HEAD;f='

# Files that we fetch and which we compare against.
# Note that the 'lib/COPYING' file must still be synced by hand.
fetchfiles = \
  $(sv_git_am)contrib/test-driver.scm \
  $(sv_git_gl)build-aux/do-release-commit-and-tag \
  $(sv_git_gl)build-aux/gitlog-to-changelog \
  ${sv_git_gl}build-aux/gnu-web-doc-update \
  $(sv_git_gl)build-aux/gnupload

# Fetch the latest versions of few scripts and files we care about.
# A retrieval failure or a copying failure usually mean serious problems,
# so we'll just bail out if 'wget' or 'cp' fail.
fetch:
	$(AM_V_at)rm -rf Fetchdir
	$(AM_V_at)mkdir Fetchdir
	$(AM_V_GEN)set -e; \
	if $(AM_V_P); then wget_opts=; else wget_opts=-nv; fi; \
	for url in $(fetchfiles); do \
	   file=`printf '%s\n' "$$url" | sed 's|^.*/||; s|^.*=||'`; \
	   $(WGET) $$wget_opts "$$url" -O Fetchdir/$$file || exit 1; \
	   if cmp Fetchdir/$$file $(srcdir)/build-aux/$$file >/dev/null; then \
	     : Nothing to do; \
	   else \
	     echo "$@: updating file $$file"; \
	     cp Fetchdir/$$file $(srcdir)/build-aux/$$file || exit 1; \
	   fi; \
	done
	$(AM_V_at)rm -rf Fetchdir
.PHONY: fetch
