
# ifndef _TYPEGMOD_T_H_
# define _TYPEGMOD_T_H_

# include "typegmod.h"

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

# endif /*_TYPEGMOD_T_H_ */
