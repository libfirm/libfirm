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
 * @file    absgraph.h
 * @author  Sebastian Hack
 * @date    20.04.2007
 * @version $Id$
 * @summary
 *
 * An abstract graph "interface". Currently
 * only used by the DFS facility.
 *
 * This is just that we can do some graph algos
 * on the CFG, dominance tree, etc.
 */
#ifndef FIRM_ANA_ABSGRAPH_H
#define FIRM_ANA_ABSGRAPH_H

#include "obst.h"

typedef struct _absgraph_t {
	void *(*get_root)(void *self);
	void (*grow_succs)(void *self, void *node, struct obstack *obst);
} absgraph_t;

extern const absgraph_t absgraph_irg_cfg_succ;
extern const absgraph_t absgraph_irg_cfg_pred;

#endif /* FIRM_ANA_ABSGRAPH_H */
