
# ifndef _TYPEGMOD_H_
# define _TYPEGMOD_H_

# include "type.h"

/****h* libfirm/typegmod
 *
 * NAME
 *  file typegmod.h
 * COPYRIGHT
 *   (C) 2001 by Universitaet Karlsruhe
 * AUTHORS
 *   Goetz Lindenmaier
 * NOTES
 *  This module supplies routines that support changing the type graph.
 *****
 */

/****f* tpop/exchange_types
 *
 * NAME
 *   exchange_types -- replaces one type by the other.
 * SYNOPSIS
 *   void exchange_types(type *old_type, type *new_type);
 * INPUTS
 *   The old type that shall be replaced by the new type.
 * SIDE EFFECTS
 *   Old type is replaced by new_type.  All references to old_type
 *   now point to new_type.  The memory for the old type is destroyed,
 *   but still used.  Therefore it is not freed.
 *   All referenced to this memory will be lost after a certain while.
 *   An exception is the list of types in irp (irprog.h).
 *   In the future there might be a routine to recover the memory, but
 *   this will be at considerable runtime cost.
 ***
 */
inline void exchange_types(type *old_type, type *new_type);

/****f* tpop/skip_tid
 *
 * NAME
 *   skip_tid -- skip id types until a useful type is reached.
 * SYNOPSIS
 *   type *skip_tid(type *tp)
 * INPUTS
 *   A type of arbitrary kind.
 * RETURNS
 *   tp if it is not an id type.
 *   If tp is an id type retruns the real type it stands for.
 ***
 */
inline type *skip_tid(type *tp);

# endif /*_TYPEGMOD_H_ */
