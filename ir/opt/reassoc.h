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

/*
 * Project:     libFIRM
 * File name:   ir/opt/reassoc.h
 * Purpose:     Reassociation
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 */

/**
 * @file reassoc.h
 *
 * Reassociation optimization.
 * Uses the "firm.opt.reassoc" debug space
 *
 * @author Michael Beck
 */
#ifndef _REASSOC_H_
#define _REASSOC_H_

#include "irgraph.h"

/** Reassociation.
 *
 * Applies Reassociation rules to integer expressions.
 * Beware: Works only if integer overflow might be ignored, as for C, Java
 * and for address expression.
 * Works only if Constant folding is activated.
 *
 * Uses loop information to detect loop-invariant (ie contant
 * inside the loop) values.
 *
 * See Muchnik 12.3.1 Algebraic Simplification and Reassociation of
 * Addressing Expressions.
 *
 *
 */
void optimize_reassociation(ir_graph *irg);

#endif /* _REASSOC_H_ */
