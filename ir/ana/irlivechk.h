/*
 * Copyright (C) 1995-2007 Inria Rhone-Alpes.  All right reserved.
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
 * @file
 * @author  Sebastian Hack
 * @date    22.04.2007
 * @brief
 *
 * Live in/end checks whose only precomputation concerns the structure of the CFG.
 * Hence, nothing has to be updated if the program is modified unless the CFG is touched.
 * See .c file for more comments.
 */

#ifndef FIRM_ANA_IRLIVECHK_H
#define FIRM_ANA_IRLIVECHK_H

#include "irgraph.h"
#include "irnode.h"
#include <stdbool.h>

typedef enum {
	lv_chk_state_in  = 1,
	lv_chk_state_end = 2,
	lv_chk_state_out = 4,
	lv_chk_state_through = lv_chk_state_in | lv_chk_state_out | lv_chk_state_end,
} lv_chk_state_t;

typedef struct lv_chk_t lv_chk_t;

/**
 * Filter out some nodes for which we never need liveness.
 *
 * @param irn  the node t check
 * @return 0 if no liveness info is needed, 1 else
 */
static inline bool is_liveness_node(const ir_node *irn)
{
	switch (get_irn_opcode(irn)) {
	case iro_Block:
	case iro_Bad:
	case iro_End:
	case iro_Anchor:
	case iro_NoMem:
		return 0;
	default:
		return 1;
	}
}

/**
 * Make a new liveness check environment.
 * @param irg The graph.
 * @return    The environment.
 */
extern lv_chk_t *lv_chk_new(ir_graph *irg);

/**
 * Free liveness check information.
 * @param lv The liveness check information.
 */
extern void lv_chk_free(lv_chk_t *lv);


/**
 * Return liveness information for a node concerning a block.
 * @param lv   The liveness environment.
 * @param bl   The block to investigate.
 * @param irn  The node to check for.
 * @return     A bitmask of <code>lv_chk_state_t</code>.
 */
extern unsigned lv_chk_bl_xxx(lv_chk_t *lv, const ir_node *bl, const ir_node *irn);

#define lv_chk_bl_in(lv, bl, irn)  ((lv_chk_bl_xxx((lv), (bl), (irn)) & lv_chk_state_in)  != 0)
#define lv_chk_bl_end(lv, bl, irn) ((lv_chk_bl_xxx((lv), (bl), (irn)) & lv_chk_state_end) != 0)
#define lv_chk_bl_out(lv, bl, irn) ((lv_chk_bl_xxx((lv), (bl), (irn)) & lv_chk_state_out) != 0)

#endif /* FIRM_ANA_IRLIVECHK_H */
