/*
 * Project:     libFIRM
 * File name:   ir/tr/type_or_entity.h
 * Purpose:     Provides a datatype to treat types and entities as the same.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file type_or_entity.h
 *
 * Provides a datatype to treat types and entities as the same.
 *
 * @author Goetz Lindenmaier
 */

# ifndef _TYPE_OR_ENTITY_H_
# define _TYPE_OR_ENTITY_H_

/** A datatype to treat types and entities as the same. */
typedef union {
  struct type   *typ;     /**< points to a type */
  struct entity *ent;     /**< points to an entity */
} type_or_ent;


# endif /* _TYPE_OR_ENTITY_H_ */
