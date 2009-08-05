/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief       additional backend node infos
 * @author      Matthias Braun
 * @version     $Id$
 */
#ifndef FIRM_BE_BEINFO_H
#define FIRM_BE_BEINFO_H

#include "be_types.h"
#include "irnode_t.h"

/**
 * The schedule structure which is present at each ir node.
 *
 * Currently, only basic blocks are scheduled. The list head of
 * every block schedule list is the Block list.
 */
struct sched_info_t {
	ir_node          *next;
	ir_node          *prev;
	sched_timestep_t  time_step;    /**< If a is after b in a schedule, its time step is larger than b's. */
};

struct reg_out_info_t {
	const arch_register_t     *reg;
	const arch_register_req_t *req;
};

struct backend_info_t {
	sched_info_t                sched_info;
	const arch_register_req_t **in_reqs;
	reg_out_info_t             *out_infos;
	arch_irn_flags_t            flags;
};

static inline backend_info_t *be_get_info(const ir_node *node)
{
	backend_info_t *info = node->backend_info;
	return info;
}

void be_info_init(void);
void be_info_free(void);
void be_info_init_irg(ir_graph *irg);
void be_info_new_node(ir_node *node);
void be_info_duplicate(const ir_node *old_node, ir_node *new_node);
int be_info_initialized(const ir_graph *irg);

int be_info_equal(const ir_node *node1, const ir_node *node2);

#endif
