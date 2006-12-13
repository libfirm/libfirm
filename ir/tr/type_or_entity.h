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

#include "firm_types.h"

/** A data type to treat types and entities as the same. */
typedef union {
  ir_type   *typ;   /**< points to a type */
  ir_entity *ent;   /**< points to an entity */
} type_or_ent;


# endif /* _TYPE_OR_ENTITY_H_ */
