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
 * @file
 * @brief    This file implements the creation of the architecture specific firm
 *           opcodes and the corresponding node constructors for the MIPS
 *           assembler irg.
 * @author   Matthias Braun, Mehdi
 * @version  $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irop.h"
#include "firm_common_t.h"
#include "irvrfy_t.h"
#include "irprintf.h"
#include "xmalloc.h"

#include "../bearch_t.h"

#include "mips_nodes_attr.h"
#include "mips_new_nodes.h"
#include "gen_mips_regalloc_if.h"



/***********************************************************************************
 *      _                                   _       _             __
 *     | |                                 (_)     | |           / _|
 *   __| |_   _ _ __ ___  _ __   ___ _ __   _ _ __ | |_ ___ _ __| |_ __ _  ___ ___
 *  / _` | | | | '_ ` _ \| '_ \ / _ \ '__| | | '_ \| __/ _ \ '__|  _/ _` |/ __/ _ \
 * | (_| | |_| | | | | | | |_) |  __/ |    | | | | | ||  __/ |  | || (_| | (_|  __/
 *  \__,_|\__,_|_| |_| |_| .__/ \___|_|    |_|_| |_|\__\___|_|  |_| \__,_|\___\___|
 *                       | |
 *                       |_|
 ***********************************************************************************/

/**
 * Dumps the register requirements for either in or out.
 */
static void dump_reg_req(FILE *F, ir_node *n, const arch_register_req_t **reqs,
                         int inout)
{
	const mips_attr_t *attr = get_mips_attr_const(n);
	char *dir = inout ? "out" : "in";
	int   max = inout ? ARR_LEN(attr->slots) : get_irn_arity(n);
	char  buf[1024];
	int   i;

	memset(buf, 0, sizeof(buf));

	if (reqs) {
		for (i = 0; i < max; i++) {
			fprintf(F, "%sreq #%d =", dir, i);

			if (reqs[i]->type == arch_register_req_type_none) {
				fprintf(F, " n/a");
			}

			if (reqs[i]->type & arch_register_req_type_normal) {
				fprintf(F, " %s", reqs[i]->cls->name);
			}

			if (reqs[i]->type & arch_register_req_type_limited) {
				fprintf(F, " %s",
				        arch_register_req_format(buf, sizeof(buf), reqs[i], n));
			}

			if (reqs[i]->type & arch_register_req_type_should_be_same) {
				ir_fprintf(F, " same as %+F", get_irn_n(n, reqs[i]->other_same[0]));
				if (reqs[i]->other_same[1] != -1)
					ir_fprintf(F, " or %+F", get_irn_n(n, reqs[i]->other_same[1]));
			}

			if (reqs[i]->type & arch_register_req_type_should_be_different) {
				ir_fprintf(F, " different from %+F", get_irn_n(n, reqs[i]->other_different));
			}

			fprintf(F, "\n");
		}

		fprintf(F, "\n");
	} else {
		fprintf(F, "%sreq = N/A\n", dir);
	}
}


/**
 * Dumper interface for dumping mips nodes in vcg.
 * @param n        the node to dump
 * @param F        the output file
 * @param reason   indicates which kind of information should be dumped
 * @return 0 on success or != 0 on failure
 */
static int mips_dump_node(ir_node *n, FILE *F, dump_reason_t reason) {
	int          bad  = 0;
	int          i;
	const mips_attr_t *attr = get_mips_attr_const(n);
	const arch_register_req_t **reqs;
	const arch_register_t     **slots;

	switch (reason) {
		case dump_node_opcode_txt:
			fprintf(F, "%s", get_irn_opname(n));
			break;

		case dump_node_mode_txt:
			break;

		case dump_node_nodeattr_txt:

			if(is_mips_Immediate(n)) {
				const mips_immediate_attr_t *attr
					= get_mips_immediate_attr_const(n);
				switch(attr->imm_type) {
				case MIPS_IMM_CONST:
					fprintf(F, " %ld ", attr->val);
					break;
				case MIPS_IMM_SYMCONST_LO:
					fprintf(F, " lo(%s", get_entity_ld_name(attr->entity));
					if(attr->val != 0) {
						fprintf(F, "%+ld", attr->val);
					}
					fprintf(F, ") ");
					break;
				case MIPS_IMM_SYMCONST_HI:
					fprintf(F, " hi(%s", get_entity_ld_name(attr->entity));
					if(attr->val != 0) {
						fprintf(F, "%+ld", attr->val);
					}
					fprintf(F, ") ");
					break;
				default:
					fprintf(F, " INVALID ");
					break;
				}
			}
			break;

		case dump_node_info_txt:
			attr = get_mips_attr(n);
			fprintf(F, "=== mips attr begin ===\n");

			/* dump IN requirements */
			if (get_irn_arity(n) > 0) {
				reqs = get_mips_in_req_all(n);
				dump_reg_req(F, n, reqs, 0);
			}

			/* dump OUT requirements */
			if (ARR_LEN(attr->slots) > 0) {
				reqs = get_mips_out_req_all(n);
				dump_reg_req(F, n, reqs, 1);
			}

			/* dump assigned registers */
			slots = get_mips_slots(n);
			if (slots && ARR_LEN(attr->slots) > 0) {
				for (i = 0; i < ARR_LEN(attr->slots); i++) {
					if (slots[i]) {
						fprintf(F, "reg #%d = %s\n", i, slots[i]->name);
					}
					else {
						fprintf(F, "reg #%d = n/a\n", i);
					}
				}
			}
			fprintf(F, "\n");

			/* dump n_res */
			fprintf(F, "n_res = %d\n", ARR_LEN(attr->slots));

			/* dump flags */
			fprintf(F, "flags =");
			if (attr->flags == arch_irn_flags_none) {
				fprintf(F, " none");
			}
			else {
				if (attr->flags & arch_irn_flags_dont_spill) {
					fprintf(F, " unspillable");
				}
				if (attr->flags & arch_irn_flags_rematerializable) {
					fprintf(F, " remat");
				}
				if (attr->flags & arch_irn_flags_ignore) {
					fprintf(F, " ignore");
				}
			}
			fprintf(F, " (%d)\n", attr->flags);

			fprintf(F, "=== mips attr end ===\n");
			/* end of: case dump_node_info_txt */
			break;
	}


	return bad;
}



/***************************************************************************************************
 *        _   _                   _       __        _                    _   _               _
 *       | | | |                 | |     / /       | |                  | | | |             | |
 *   __ _| |_| |_ _ __   ___  ___| |_   / /_ _  ___| |_   _ __ ___   ___| |_| |__   ___   __| |___
 *  / _` | __| __| '__| / __|/ _ \ __| / / _` |/ _ \ __| | '_ ` _ \ / _ \ __| '_ \ / _ \ / _` / __|
 * | (_| | |_| |_| |    \__ \  __/ |_ / / (_| |  __/ |_  | | | | | |  __/ |_| | | | (_) | (_| \__ \
 *  \__,_|\__|\__|_|    |___/\___|\__/_/ \__, |\___|\__| |_| |_| |_|\___|\__|_| |_|\___/ \__,_|___/
 *                                        __/ |
 *                                       |___/
 ***************************************************************************************************/

mips_attr_t *get_mips_attr(ir_node *node)
{
	assert(is_mips_irn(node) && "need mips node to get attributes");
	return (mips_attr_t *) get_irn_generic_attr(node);
}

const mips_attr_t *get_mips_attr_const(const ir_node *node)
{
	assert(is_mips_irn(node) && "need mips node to get attributes");
	return get_irn_generic_attr_const(node);
}

const mips_immediate_attr_t *get_mips_immediate_attr_const(const ir_node *node)
{
	assert(is_mips_irn(node) && "need mips node to get attributes");
	return get_irn_generic_attr_const(node);
}

const mips_load_store_attr_t *get_mips_load_store_attr_const(
		const ir_node *node)
{
	assert(is_mips_irn(node) && "need mips node to get attributes");
	return get_irn_generic_attr_const(node);
}

/**
 * Returns the argument register requirements of a mips node.
 */
const arch_register_req_t **get_mips_in_req_all(const ir_node *node)
{
	const mips_attr_t *attr = get_mips_attr_const(node);
	return attr->in_req;
}

/**
 * Returns the result register requirements of an mips node.
 */
const arch_register_req_t **get_mips_out_req_all(const ir_node *node)
{
	const mips_attr_t *attr = get_mips_attr_const(node);
	return attr->out_req;
}

/**
 * Returns the argument register requirement at position pos of an mips node.
 */
const arch_register_req_t *get_mips_in_req(const ir_node *node, int pos)
{
	const mips_attr_t *attr = get_mips_attr_const(node);
	return attr->in_req[pos];
}

/**
 * Returns the result register requirement at position pos of an mips node.
 */
const arch_register_req_t *get_mips_out_req(const ir_node *node, int pos)
{
	const mips_attr_t *attr = get_mips_attr_const(node);
	return attr->out_req[pos];
}

/**
 * Sets the OUT register requirements at position pos.
 */
void set_mips_req_out(ir_node *node, const arch_register_req_t *req, int pos)
{
	mips_attr_t *attr   = get_mips_attr(node);
	attr->out_req[pos] = req;
}

/**
 * Sets the IN register requirements at position pos.
 */
void set_mips_req_in(ir_node *node, const arch_register_req_t *req, int pos)
{
	mips_attr_t *attr  = get_mips_attr(node);
	attr->in_req[pos] = req;
}

/**
 * Returns the register flag of an mips node.
 */
arch_irn_flags_t get_mips_flags(const ir_node *node)
{
	const mips_attr_t *attr = get_mips_attr_const(node);
	return attr->flags;
}

/**
 * Sets the register flag of an mips node.
 */
void set_mips_flags(ir_node *node, arch_irn_flags_t flags)
{
	mips_attr_t *attr = get_mips_attr(node);
	attr->flags      = flags;
}

/**
 * Returns the result register slots of an mips node.
 */
const arch_register_t **get_mips_slots(const ir_node *node)
{
	const mips_attr_t *attr = get_mips_attr_const(node);
	return attr->slots;
}

/**
 * Returns the name of the OUT register at position pos.
 */
const char *get_mips_out_reg_name(const ir_node *node, int pos)
{
	const mips_attr_t *attr = get_mips_attr_const(node);

	assert(is_mips_irn(node) && "Not an mips node.");
	assert(pos < ARR_LEN(attr->slots) && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return arch_register_get_name(attr->slots[pos]);
}

/**
 * Returns the index of the OUT register at position pos within its register class.
 */
int get_mips_out_regnr(const ir_node *node, int pos)
{
	const mips_attr_t *attr = get_mips_attr_const(node);

	assert(is_mips_irn(node) && "Not an mips node.");
	assert(pos < ARR_LEN(attr->slots) && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return arch_register_get_index(attr->slots[pos]);
}

/**
 * Returns the OUT register at position pos.
 */
const arch_register_t *get_mips_out_reg(const ir_node *node, int pos)
{
	const mips_attr_t *attr = get_mips_attr_const(node);

	assert(is_mips_irn(node) && "Not an mips node.");
	assert(pos < ARR_LEN(attr->slots) && "Invalid OUT position.");
	assert(attr->slots[pos]  && "No register assigned");

	return attr->slots[pos];
}

/**
 * Initializes the nodes attributes.
 */
static void init_mips_attributes(ir_node *node, arch_irn_flags_t flags,
                                 const arch_register_req_t **in_reqs,
                                 const arch_register_req_t **out_reqs,
                                 const be_execution_unit_t ***execution_units,
                                 int n_res, unsigned latency)
{
	ir_graph       *irg  = get_irn_irg(node);
	struct obstack *obst = get_irg_obstack(irg);
	mips_attr_t    *attr = get_mips_attr(node);
	(void) execution_units;
	(void) latency;

	attr->flags   = flags;
	attr->out_req = out_reqs;
	attr->in_req  = in_reqs;

	attr->slots = NEW_ARR_D(const arch_register_t*, obst, n_res);
	memset(attr->slots, 0, n_res * sizeof(attr->slots[0]));
}

static void init_mips_immediate_attributes(ir_node *node,
                                           mips_immediate_type_t type,
                                           ir_entity *entity, long val)
{
	mips_immediate_attr_t *attr = get_irn_generic_attr(node);

	attr->imm_type = type;
	attr->entity   = entity;
	attr->val      = val;
}

static void init_mips_load_store_attributes(ir_node *node, ir_entity *entity,
                                            long offset)
{
	mips_load_store_attr_t *attr = get_irn_generic_attr(node);
	attr->stack_entity = entity;
	attr->offset       = offset;
}

static int mips_compare_nodes_attr(ir_node *node_a, ir_node *node_b)
{
	const mips_attr_t *a = get_mips_attr_const(node_a);
	const mips_attr_t *b = get_mips_attr_const(node_b);

	if(a->flags != b->flags)
		return 1;
	if(ARR_LEN(a->slots) != ARR_LEN(b->slots))
		return 1;
	if(a->switch_default_pn != b->switch_default_pn)
		return 1;

	return 0;
}

static int mips_compare_immediate_attr(ir_node *node_a, ir_node *node_b)
{
	const mips_immediate_attr_t *a = get_mips_immediate_attr_const(node_a);
	const mips_immediate_attr_t *b = get_mips_immediate_attr_const(node_b);

	if(a->attr.flags != b->attr.flags)
		return 1;
	if(a->val != b->val)
		return 1;

	return 0;
}

static int mips_compare_load_store_attr(ir_node *node_a, ir_node *node_b)
{
	const mips_load_store_attr_t *a = get_mips_load_store_attr_const(node_a);
	const mips_load_store_attr_t *b = get_mips_load_store_attr_const(node_b);

	if(mips_compare_nodes_attr(node_a, node_b))
		return 1;
	if(a->stack_entity != b->stack_entity)
		return 1;
	if(a->offset != b->offset)
		return 1;

	return 0;
}

static void mips_copy_attr(const ir_node *old_node , ir_node *new_node)
{
	ir_graph          *irg      = get_irn_irg(new_node);
	struct obstack    *obst     = get_irg_obstack(irg);
	const mips_attr_t *attr_old = get_mips_attr_const(old_node);
	mips_attr_t       *attr_new = get_mips_attr(new_node);

	/* copy the attributes */
	memcpy(attr_new, attr_old, get_op_attr_size(get_irn_op(old_node)));

	/* copy register assignments */
	attr_new->slots = DUP_ARR_D(arch_register_t*, obst, attr_old->slots);
}

/***************************************************************************************
 *                  _                            _                   _
 *                 | |                          | |                 | |
 *  _ __   ___   __| | ___    ___ ___  _ __  ___| |_ _ __ _   _  ___| |_ ___  _ __ ___
 * | '_ \ / _ \ / _` |/ _ \  / __/ _ \| '_ \/ __| __| '__| | | |/ __| __/ _ \| '__/ __|
 * | | | | (_) | (_| |  __/ | (_| (_) | | | \__ \ |_| |  | |_| | (__| || (_) | |  \__ \
 * |_| |_|\___/ \__,_|\___|  \___\___/|_| |_|___/\__|_|   \__,_|\___|\__\___/|_|  |___/
 *
 ***************************************************************************************/

/* Include the generated constructor functions */
#include "gen_mips_new_nodes.c.inl"
