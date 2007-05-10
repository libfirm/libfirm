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
 * @file    irlivechk.h
 * @author  Sebastian Hack
 * @date    22.04.2007
 * @version $Id: $
 * @summary
 *
 * Live in/end checks whose only precomputation concerns the structure of the CFG.
 * Hence, nothing has to be updated if the program is modified unless the CFG is touched.
 * See .c file for more comments.
 *
 * Copyright (C) 2007 Universitaet Karlsruhe
 * Released under the GPL
 */
#ifndef FIRM_ANA_IRLIVECHK_H
#define FIRM_ANA_IRLIVECHK_H

#include "irgraph.h"
#include "irnode.h"

typedef struct _lv_chk_t lv_chk_t;

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
 * Check, if a node is live end of a given block.
 * @param lv   The liveness environment.
 * @param bl   The block to investigate.
 * @param irn  The node to check for.
 * @return     1, if @p what is live end at @p bl, 0 else.
 */
extern int lv_chk_bl_end(const lv_chk_t *lv, const ir_node *bl, const ir_node *irn);

/**
 * Check, if a node is live out of a given block.
 * @param lv   The liveness environment.
 * @param bl   The block to investigate.
 * @param irn  The node to check for.
 * @return     1, if @p what is live out at @p bl, 0 else.
 */
extern int lv_chk_bl_out(const lv_chk_t *lv, const ir_node *bl, const ir_node *irn);

/**
 * Check, if a node is live in of a given block.
 * @param lv   The liveness environment.
 * @param bl   The block to investigate.
 * @param irn  The node to check for.
 * @return     1, if @p what is live in at @p bl, 0 else.
 */
extern int lv_chk_bl_in(const lv_chk_t *lv, const ir_node *bl, const ir_node *irn);

#endif /* FIRM_ANA_IRLIVECHK_H */
