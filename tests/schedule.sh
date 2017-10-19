# schedule.sh -- Check mcron schedule output
# Copyright © 2017 Mathieu Lirzin <mthl@gnu.org>
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

SOURCE_DATE_EPOCH=1
export SOURCE_DATE_EPOCH

# Use current working directory to store mcron files
XDG_CONFIG_HOME=`pwd`
export XDG_CONFIG_HOME

mkdir cron
cat > cron/foo.guile <<EOF
(job '(next-second) '(display "foo\n"))
EOF

cat > cron/bar.guile <<EOF
(job '(next-second) '(display "bar\n"))
EOF

cat > expected <<EOF
Thu Jan  1 01:00:01 1970 +0100
(display foo
)

Thu Jan  1 01:00:01 1970 +0100
(display bar
)

Thu Jan  1 01:00:02 1970 +0100
(display foo
)

Thu Jan  1 01:00:02 1970 +0100
(display bar
)

Thu Jan  1 01:00:03 1970 +0100
(display foo
)

Thu Jan  1 01:00:03 1970 +0100
(display bar
)

Thu Jan  1 01:00:04 1970 +0100
(display foo
)

Thu Jan  1 01:00:04 1970 +0100
(display bar
)

Thu Jan  1 01:00:05 1970 +0100
(display foo
)

Thu Jan  1 01:00:05 1970 +0100
(display bar
)

Thu Jan  1 01:00:06 1970 +0100
(display foo
)

Thu Jan  1 01:00:06 1970 +0100
(display bar
)

Thu Jan  1 01:00:07 1970 +0100
(display foo
)

Thu Jan  1 01:00:07 1970 +0100
(display bar
)

Thu Jan  1 01:00:08 1970 +0100
(display foo
)

Thu Jan  1 01:00:08 1970 +0100
(display bar
)

Thu Jan  1 01:00:09 1970 +0100
(display foo
)

Thu Jan  1 01:00:09 1970 +0100
(display bar
)

Thu Jan  1 01:00:10 1970 +0100
(display foo
)

Thu Jan  1 01:00:10 1970 +0100
(display bar
)

EOF

mcron --schedule=10 > output
diff expected output || fail_ "schedule output is not correct"

Exit 0
