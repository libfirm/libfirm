/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief    Data type for unique names.
 * @author   Goetz Lindenmaier
 * @brief    Declarations for identifiers in the firm library
 */
#ifndef FIRM_IDENT_H
#define FIRM_IDENT_H

#include <stddef.h>
#include "firm_types.h"
#include "begin.h"

/**
 * @defgroup ir_ident  Identifiers
 * @{
 */

/**
 *  Store a string and create an ident.
 *
 *  Stores a string in the ident module and returns a handle for the string.
 *
 *  Copies the string. @p str must be zero terminated
 *
 * @param str   the string which shall be stored
 * @return id   a handle for the generated ident
 */
FIRM_API ident *new_id_from_str(const char *str);

/** Store a string and create an ident.
 *
 * Stores a string in the ident module and returns a handle for the string.
 * Copies the string. This version takes non-zero-terminated strings.
 *
 * @param str   the string (or whatever) which shall be stored
 * @param len   the length of the data in bytes
 * @return id   a handle for the generated ident
 */
FIRM_API ident *new_id_from_chars(const char *str, size_t len);

/**
 * Returns a string represented by an ident.
 *
 * Returns the string represented by id. This string is
 * NULL terminated. The string may not be changed.
 *
 * @param id   the ident
 * @return cp   a string
 */
FIRM_API const char *get_id_str(ident *id);

/**
 * Returns the length of the string represented by an ident.
 *
 * @param id   the ident
 * @return len   the length of the string
 */
FIRM_API size_t get_id_strlen(ident *id);

/**
 * Test if @p prefix is a prefix of ident @p id.
 *
 * @param prefix   the prefix
 * @param id       the ident
 * @returns        1 if @p prefix is prefix of @p id, 0 otherwise
 */
FIRM_API int id_is_prefix(ident *prefix, ident *id);

/**
 * Test if @p suffix is a suffix of ident @p id.
 *
 * @param suffix   the suffix
 * @param id       the ident
 * @returns        1 if @p suffix is suffix of @p id, 0 otherwise
 */
FIRM_API int id_is_suffix(ident *suffix, ident *id);

/**
 * Test if identifier contains a given character.
 *
 * @param id    the ident
 * @param c     the character
 * @returns     1 if character is contained, 0 otherwise
 */
FIRM_API int id_contains_char(ident *id, char c);

/**
 * helper function for creating unique idents. It contains an internal counter
 * and replaces a "%u" inside the tag with the counter.
 */
FIRM_API ident *id_unique(const char *tag);

/** Computes a definite name for this entity by concatenating
   the name of the owner type and the name of the entity with
   a separating "_". */
FIRM_API ident *id_mangle_entity(const ir_entity *ent);

/** mangle underscore: Returns a new ident that represents first_scnd. */
FIRM_API ident *id_mangle_u(ident *first, ident* scnd);

/** mangle dot: Returns a new ident that represents first.scnd. */
FIRM_API ident *id_mangle_dot(ident *first, ident* scnd);

/** mangle: Returns a new ident that represents firstscnd. */
FIRM_API ident *id_mangle(ident *first, ident* scnd);

/** Returns a new ident that represents 'prefixscndsuffix'. */
FIRM_API ident *id_mangle3(const char *prefix, ident *middle,
                           const char *suffix);

/** @} */

#include "end.h"

#endif
