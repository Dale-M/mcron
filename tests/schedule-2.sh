# schedule-2.sh -- Check mcron schedule output
# Copyright © 2020  Dale Mellor <mcron-lsfnyl@rdmp.org>
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

source "${srcdir}/tests/init.sh"

# Use UTC and SOURCE_DATE_EPOCH to get reproducible result.

SOURCE_DATE_EPOCH=1
export SOURCE_DATE_EPOCH

TZ=UTC0
export TZ

# Use current working directory to store mcron files
XDG_CONFIG_HOME=`pwd`
export XDG_CONFIG_HOME

LC_ALL=C
export LC_ALL

mkdir cron
cat > cron/foo.guile <<EOF
(job '(next-second) '(display "foo\n"))
EOF

cat > expected <<EOF
Thu Jan  1 00:00:01 1970 +0000
(display foo
)

Thu Jan  1 00:00:02 1970 +0000
(display foo
)

Thu Jan  1 00:00:03 1970 +0000
(display foo
)

Thu Jan  1 00:00:04 1970 +0000
(display foo
)

Thu Jan  1 00:00:05 1970 +0000
(display foo
)

Thu Jan  1 00:00:06 1970 +0000
(display foo
)

Thu Jan  1 00:00:07 1970 +0000
(display foo
)

Thu Jan  1 00:00:08 1970 +0000
(display foo
)

EOF

mcron -s cron/foo.guile > output
diff expected output \
    || skip_ 'The -s option is not fully functional; 
this will be fixed with a future version of GNU Guile.'

Exit 0
