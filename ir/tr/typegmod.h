
/* $Id$ */

# ifndef _TYPEGMOD_H_
# define _TYPEGMOD_H_

# include "type.h"

/**
 *
 *  file typegmod.h
 *   (C) 2001 by Universitaet Karlsruhe
 *   Goetz Lindenmaier
 *  This module supplies routines that support changing the type graph.
 */

/**
 *
 *   - replaces one type by the other.
 *   @param The old type that shall be replaced by the new type.
 *   Old type is replaced by new_type.  All references to old_type
 *   now point to new_type.  The memory for the old type is destroyed,
 *   but still used.  Therefore it is not freed.
 *   All referenced to this memory will be lost after a certain while.
 *   An exception is the list of types in irp (irprog.h).
 *   In the future there might be a routine to recover the memory, but
 *   this will be at considerable runtime cost.
 *
 */
INLINE void exchange_types(type *old_type, type *new_type);

/**
 *
 *   - skip id types until a useful type is reached.
 *   @param A type of arbitrary kind.
 *   tp if it is not an id type.
 *   If tp is an id type retruns the real type it stands for.
 *
 */
INLINE type *skip_tid(type *tp);

# endif /*_TYPEGMOD_H_ */
