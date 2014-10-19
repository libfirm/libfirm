/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
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
 * helper function for creating unique idents. It contains an internal counter
 * and replaces a "%u" inside the tag with the counter.
 */
FIRM_API ident *id_unique(const char *tag);

/** Returns a new ident that represents 'prefixscndsuffix'. */
FIRM_API ident *id_mangle3(const char *prefix, ident *middle,
                           const char *suffix);

/** @} */

#include "end.h"

#endif
