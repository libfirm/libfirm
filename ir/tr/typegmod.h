/*
 * Project:     libFIRM
 * File name:   ir/tr/typegmod.h
 * Purpose:     Functionality to modify the type graph.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# ifndef _TYPEGMOD_H_
# define _TYPEGMOD_H_

# include "type.h"

/**
 *
 * @file typegmod.h
 *  This module supplies routines that support changing the type graph.
 */

/** Replaces one type by the other.
 *
 *  Old type is replaced by new_type.  All references to old_type
 *  now point to new_type.  The memory for the old type is destroyed,
 *  but still used.  Therefore it is not freed.
 *  All referenced to this memory will be lost after a certain while.
 *  An exception is the list of types in irp (irprog.h).
 *  In the future there might be a routine to recover the memory, but
 *  this will be at considerable runtime cost.
 *
 *  @param old_type  - The old type that shall be replaced by the new type.
 *  @param new_type  - The new type that will replace old_type.
 *
 */
void exchange_types(type *old_type, type *new_type);

/** Skip id types until a useful type is reached.
 *
 *  @param tp - A type of arbitrary kind.
 *
 *  @return
 *    tp if it is not an id type.
 *    If tp is an id type returns the real type it stands for.
 */
type *skip_tid(type *tp);

# endif /*_TYPEGMOD_H_ */
