# basic.sh -- basic tests for mcron
# Copyright © 2017 Mathieu Lirzin <mthl@gnu.org>
# Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

# Use current working directory to store mcron files
XDG_CONFIG_HOME=`pwd`
export XDG_CONFIG_HOME

mkdir cron
cat > cron/foo.guile <<EOF
(job '(next-second) '(display "foo\n"))
EOF

mcron --schedule=1 cron/foo.guile > "output$$"
grep -e "foo" "output$$" || fail_ "'foo.guile' job is not scheduled"

mcron --schedule=1 > "output$$"
grep -e "foo" "output$$" || fail_ "'foo.guile' job is not scheduled"

mcron --date-format="~½" cron/foo.guile \
    && fail_ "mcron --date-format unexpected pass"

mcron --log-format="~½" cron/foo.guile \
    && fail_ "mcron --log-format unexpected pass"

cron --date-format="~½" cron/foo.guile \
    && fail_ "cron --date-format unexpected pass"

cron --log-format="~½" cron/foo.guile \
    && fail_ "cron --log-format unexpected pass"

Exit 0
