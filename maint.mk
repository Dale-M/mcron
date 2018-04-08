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

# If it's not already specified, derive the GPG key ID from
# the signed tag we've just applied to mark this release.
gpg_key_ID = \
  $$(cd $(srcdir) \
     && git cat-file tag v$(VERSION) \
        | gpgv --status-fd 1 --keyring /dev/null - - 2>/dev/null \
        | awk '/^\[GNUPG:\] ERRSIG / {print $$3; exit}')

# Use alpha.gnu.org for alpha and beta releases.
# Use ftp.gnu.org for stable releases.
gnu_ftp_host-alpha = alpha.gnu.org
gnu_ftp_host-beta = alpha.gnu.org
gnu_ftp_host-stable = ftp.gnu.org
gnu_rel_host = $(gnu_ftp_host-$(release-type))

noteworthy_changes = * Noteworthy changes in release ?.? (????-??-??) [?]

.PHONY: release
release:
	cd $(srcdir) && rm -rf autom4te.cache && ./bootstrap && ./configure
	$(AM_V_at)$(MAKE) Makefile
	$(AM_V_at)$(srcdir)/build-aux/announce-gen \
	    --mail-headers='To: ??? Mail-Followup-To: $(PACKAGE_BUGREPORT)' \
	    --release-type=$(release-type) \
	    --package=$(PACKAGE) \
	    --prev=`cat .prev-version` \
	    --curr=$(VERSION) \
	    --gpg-key-id=$(gpg_key_ID) \
	    --srcdir=$(srcdir) \
	    --news=$(srcdir)/NEWS \
	    --bootstrap-tools=autoconf,automake,help2man \
	    --no-print-checksums \
	    --url-dir=https://ftp.gnu.org/gnu/$(PACKAGE) \
	  > ~/announce-$(PACKAGE)-$(VERSION)
	$(AM_V_at)echo $(VERSION) > .prev-version
	$(AM_V_at)perl -pi \
	  -e '$$. == 3 and print "$(noteworthy_changes)\n\n\n"' \
	  $(srcdir)/NEWS
	$(AM_V_at)msg=`printf '%s\n' 'maint: Post-release administrivia' '' \
	    '* NEWS: Add header line for next release.' \
	    '* .prev-version: Record previous version.'` || exit 1; \
	git commit -m "$$msg" -a

.PHONY: upload
upload:
	$(srcdir)/build-aux/gnupload $(GNUPLOADFLAGS) \
	  --to $(gnu_rel_host):$(PACKAGE) \
	  $(DIST_ARCHIVES)

.PHONY: web-manual
web-manual:
	$(AM_V_at)cd '$(srcdir)/doc'; \
	  $(SHELL) ../build-aux/gendocs.sh \
	     -o '$(abs_builddir)/doc/manual' \
	     --email $(PACKAGE_BUGREPORT) $(PACKAGE) \
	    "$(PACKAGE_STRING) Reference Manual"
	$(AM_V_at)echo " *** Upload the doc/manual directory to web-cvs."

.PHONY: web-manual-update
web-manual-update:
	$(AM_V_GEN)cd $(srcdir) \
	  && build-aux/gnu-web-doc-update -C $(abs_builddir)
