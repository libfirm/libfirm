/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       additional backend node infos
 * @author      Matthias Braun
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
	/** Additional register pressure for the first 4 regclasses */
	be_add_pressure_t           add_pressure[4];
	const arch_register_req_t **in_reqs;
	reg_out_info_t             *out_infos;
	arch_irn_flags_t            flags;
#ifndef NDEBUG
	char const                 *orig_node;
#endif
};

static inline backend_info_t *be_get_info(const ir_node *node)
{
	assert(!is_Proj(node));
	return (backend_info_t*) node->backend_info;
}

void be_info_init(void);
void be_info_free(void);
void be_info_init_irg(ir_graph *irg);
void be_info_new_node(ir_graph *irg, ir_node *node);

void be_info_init_irn(ir_node *node, arch_irn_flags_t flags, arch_register_req_t const **in_reqs, unsigned n_res);

int attrs_equal_be_node(const ir_node *node1, const ir_node *node2);

#endif
