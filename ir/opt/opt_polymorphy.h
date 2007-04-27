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
 * File name:   ir/opt/opt_polymorphy.h
 * Purpose:     Optimize polymorphic Sel and Load nodes.
 * Author:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 */

/** @file
 *
 *  This file subsumes optimization code from cgana.
 */
#ifndef _OPT_POLYMORPHY_H_
#define _OPT_POLYMORPHY_H_

#include "firm_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Transform  Sel(Alloc)[method]
 * to SymC[method] under the following conditions:
 *
 * - opt_dyn_meth_dispatch must be set
 * - the method is not overwritten OR
 * - the dynamic type is known
 */
ir_node *transform_node_Sel(ir_node *node);

/** Transform  Load(Sel(Alloc)[constant static entity])
 *  to Const[constant static entity value].
 *
 *  This function returns a node replacing the Proj(Load)[Value].
 *  If this is actually called in transform_node, we must build
 *  a tuple, or replace the Projs of the load.
 *  Therefore we call this optimization in ldstopt.
 */
ir_node *transform_node_Load(ir_node *n);

#ifdef __cplusplus
}
#endif

#endif /* _OPT_POLYMORPHY_H_ */
