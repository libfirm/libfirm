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
 * @file    mangle.h
 * @brief   Methods to manipulate names.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#ifndef FIRM_TR_MANGLE_H
#define FIRM_TR_MANGLE_H

#include "ident.h"
#include "entity.h"

/** initializes the name mangling code */
void   firm_init_mangle (void);

/** Computes a definite name for this entity by concatenating
   the name of the owner type and the name of the entity with
   a separating "_". */
ident *mangle_entity (ir_entity *ent);

/** mangle underscore: Returns a new ident that represents first_scnd. */
ident *mangle_u (ident *first, ident* scnd);

/** mangle dot: Returns a new ident that represents first.scnd. */
ident *mangle_dot (ident *first, ident* scnd);

/** mangle: Returns a new ident that represents firstscnd. */
ident *mangle   (ident *first, ident* scnd);

/** returns a mangled name for a Win32 function using it's calling convention */
ident *decorate_win32_c_fkt(ir_entity *ent, ident *id);

#endif /* FIRM_TR_MANGLE_H */
