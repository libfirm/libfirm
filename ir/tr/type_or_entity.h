/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*/

/**
 * @file type_or_entity.h
 *
 * Provides a datatype to treat types and entities as the same.
 *
 * @author Goetz Lindenmaier
 */

/* $Id$ */


# ifndef _TYPE_OR_ENTITY_H_
# define _TYPE_OR_ENTITY_H_

/** A datatype to treat types and entities as the same. */
typedef union {
  struct type   *typ;     /**< points to a type */
  struct entity *ent;     /**< points to an entity */
} type_or_ent;


# endif /* _TYPE_OR_ENTITY_H_ */
