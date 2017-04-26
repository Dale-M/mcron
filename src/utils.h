/* utils.h -- Utility functions.
   Copyright © 2017 Mathieu Lirzin <mthl@gnu.org>

   This file is part of GNU Mcron.

   GNU Mcron is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   GNU Mcron is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Mcron.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef MCRON_UTILS_H
#define MCRON_UTILS_H

#include <libguile.h>

/**
   Append DIR in front of ENVAR environment variable value.  If ENVAR is not
   defined, then define it with DIR.  Bail out if something went wrong.  */
extern void wrap_env_path (const char *envar, const char *dir);

/**
   In the association list pointed by ALST, set SYMBOL to VAL.  */
extern void assq_symbol_set_x (SCM *alst, const char *symbol, SCM val);

#endif /* MCRON_UTILS_H */
