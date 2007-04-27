/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @file    typegmod.h
 * @brief   Functionality to modify the type graph.
 * @author  Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#ifndef FIRM_TR_TYPEGMOD_H
#define FIRM_TR_TYPEGMOD_H

#include "firm_types.h"

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
void exchange_types(ir_type *old_type, ir_type *new_type);

/** Skip id types until a useful type is reached.
 *
 *  @param tp - A type of arbitrary kind.
 *
 *  @return
 *    tp if it is not an id type.
 *    If tp is an id type returns the real type it stands for.
 */
ir_type *skip_tid(ir_type *tp);

#endif /*FIRM_TR_TYPEGMOD_H */
