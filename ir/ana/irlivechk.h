/**
 * @file   irlivechk.h
 * @date   22.04.2007
 * @author Sebastian Hack
 *
 * Live in/end checks whose only precomputation concerns the structure of the CFG.
 * Hence, nothing has to be updated if the program is modified unless the CFG is touched.
 * See .c file for more comments.
 *
 * Copyright (C) 2007 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _IRLIVECHK_H
#define _IRLIVECHK_H

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

#endif /* _IRLIVECHK_H */
