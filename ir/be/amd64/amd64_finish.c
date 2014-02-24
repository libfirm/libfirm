/*
 * Copyright (C) 1995-2012 University of Karlsruhe.  All right reserved.
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
 * @brief   This file implements functions to finalize the irg for emit.
 */
#include "amd64_finish.h"
#include "amd64_new_nodes.h"
#include "amd64_nodes_attr.h"
#include "bearch.h"
#include "benode.h"
#include "besched.h"
#include "debug.h"
#include "error.h"
#include "gen_amd64_new_nodes.h"
#include "irgwalk.h"
#include "util.h"
#include "irgmod.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * Returns the index of the first "same" register.
 */
static unsigned get_first_same(arch_register_req_t const *const req)
{
	unsigned const other = req->other_same;
	for (unsigned i = 0; i != 32; ++i) {
		if (other & (1U << i))
			return i;
	}
	panic("same position not found");
}

static bool try_swap_inputs(ir_node *node)
{
	/* commutative operation, just switch the inputs */
	if (is_amd64_Add(node) || is_amd64_And(node) || is_amd64_Or(node)
	 || is_amd64_Xor(node) || is_amd64_IMul(node)) {
		/* TODO: support Cmp input swapping */
		ir_node *in0 = get_irn_n(node, 0);
		ir_node *in1 = get_irn_n(node, 1);
		set_irn_n(node, 0, in1);
		set_irn_n(node, 1, in0);
		return true;
	}
	return false;
}

/**
  * Transforms a Sub to a Neg + Add, which subsequently allows swapping
  * of the inputs. The swapping is also (implicitly) done here.
  */
static void transform_sub_to_neg_add(ir_node *node,
                                     const arch_register_t *out_reg)
{
	assert(is_amd64_Sub(node));

	ir_node  *block = get_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	const amd64_binop_addr_attr_t *attr = get_amd64_binop_addr_attr(node);

	ir_node *in1  = get_irn_n(node, 0);
	ir_node *in2  = get_irn_n(node, 1);

	ir_node *neg  = new_bd_amd64_Neg(dbgi, block, in2, attr->base.insn_mode);
	arch_set_irn_register(neg, out_reg);

	sched_add_before(node, neg);

	ir_node *in[2]   = { neg, in1 };
	ir_node *add     = new_bd_amd64_Add(dbgi, block, ARRAY_SIZE(in), in, attr);
	ir_node *add_res = new_r_Proj(add, mode_Lu, pn_amd64_Add_res);

	arch_set_irn_register(add_res, out_reg);

	/* exchange the add and the sub */
	edges_reroute(node, add);
	sched_replace(node, add);

	kill_node(node);
}

static ir_node *amd64_turn_back_am(ir_node *node)
{
	dbg_info          *dbgi  = get_irn_dbg_info(node);
	ir_node           *block = get_nodes_block(node);
	amd64_addr_attr_t *attr  = get_amd64_addr_attr(node);

	amd64_addr_t new_addr = attr->addr;
	ir_node *load_in[3];
	int      load_arity = 0;
	if (attr->addr.base_input != NO_INPUT
	 && attr->addr.base_input != RIP_INPUT) {
		new_addr.base_input = load_arity;
		load_in[load_arity++] = get_irn_n(node, attr->addr.base_input);
	}
	if (attr->addr.index_input != NO_INPUT) {
		new_addr.index_input = load_arity;
		load_in[load_arity++] = get_irn_n(node, attr->addr.index_input);
	}
	assert(attr->addr.mem_input != NO_INPUT);
	new_addr.mem_input = load_arity;
	load_in[load_arity++] = get_irn_n(node, attr->addr.mem_input);

	ir_node *load = new_bd_amd64_Mov(dbgi, block, load_arity, load_in,
	                                 attr->insn_mode, AMD64_OP_ADDR, new_addr);
	ir_node *load_res = new_r_Proj(load, mode_Lu, pn_amd64_Mov_res);

	/* change operation */
	const amd64_binop_addr_attr_t *binop_attr
		= (const amd64_binop_addr_attr_t*)attr;
	ir_node *new_in[2];
	new_in[0] = get_irn_n(node, binop_attr->u.reg_input);
	new_in[1] = load_res;
	set_irn_in(node, ARRAY_SIZE(new_in), new_in);
	attr->base.op_mode     = AMD64_OP_REG_REG;
	attr->addr.base_input  = NO_INPUT;
	attr->addr.index_input = NO_INPUT;

	/* rewire mem-proj */
	foreach_out_edge(node, edge) {
		ir_node *out = get_edge_src_irn(edge);
		if (get_irn_mode(out) == mode_M) {
			set_Proj_pred(out, load);
			set_Proj_proj(out, pn_amd64_Mov_M);
			break;
		}
	}

	if (sched_is_scheduled(node))
		sched_add_before(node, load);
	return load_res;
}

/**
 * Insert copies for all amd64 nodes where the should_be_same requirement is
 * not fulfilled.
 */
static void assure_should_be_same_requirements(ir_node *const node)
{
	unsigned const n_res = arch_get_irn_n_outs(node);

	/* Check all OUT requirements, if there is a should_be_same. */
	for (unsigned i = 0; i < n_res; ++i) {
		arch_register_req_t const *const req
			= arch_get_irn_register_req_out(node, i);
		if (!arch_register_req_is(req, should_be_same))
			continue;
		unsigned               const same_pos = get_first_same(req);
		ir_node               *const in_node  = get_irn_n(node, same_pos);
		arch_register_t const *const in_reg   = arch_get_irn_register(in_node);
		arch_register_t const *const out_reg
			= arch_get_irn_register_out(node, i);
		if (in_reg == out_reg)
			continue;

		/* test if any other input is using the out register */
		for (int i2 = 0, arity = get_irn_arity(node); i2 < arity; ++i2) {
			const arch_register_t *reg = arch_get_irn_register_in(node, i2);
			if (reg == out_reg && (unsigned)i2 != same_pos) {
				if (!is_amd64_irn(node))
					panic("Can't fulfill should_be_same on non-amd64 node");
				/* see what role this register has */
				const amd64_attr_t *attr = get_amd64_attr_const(node);
				if (attr->op_mode == AMD64_OP_ADDR
				 || attr->op_mode == AMD64_OP_REG
				 || attr->op_mode == AMD64_OP_REG_IMM) {
					panic("unexpected op_mode");
				} else if (attr->op_mode == AMD64_OP_REG_REG) {
swap:;
					bool res = try_swap_inputs(node);
					if (res)
						return;

					if (is_amd64_Sub(node)) {
						transform_sub_to_neg_add(node, out_reg);
						return;
					}
					panic("couldn't swap inputs of %+F", node);
				} else {
					assert(attr->op_mode == AMD64_OP_ADDR_REG);
					/* extract load into an own instruction */
					ir_node *res = amd64_turn_back_am(node);
					arch_set_irn_register(res, out_reg);
					goto swap;
				}
			}
		}

		ir_node *const block = get_nodes_block(node);
		ir_node *const copy  = be_new_Copy(block, in_node);

		/* Destination is the out register. */
		arch_set_irn_register(copy, out_reg);
		/* Insert copy before the node into the schedule. */
		sched_add_before(node, copy);
		/* Set copy as in. */
		set_irn_n(node, same_pos, copy);

		DBG((dbg, LEVEL_1, "created copy %+F for should be same argument at input %d of %+F\n", copy, same_pos, node));
	}
}

/**
 * Block walker: finishes a block.
 */
static void amd64_finish_irg_walker(ir_node *const block, void *const env)
{
	(void) env;

	/* Insert should_be_same copies. */
	for (ir_node *irn = sched_first(block), *next; !sched_is_end(irn);
	     irn = next) {
		next = sched_next(irn);
		if (is_amd64_irn(irn)) {
			assure_should_be_same_requirements(irn);
		}
	}
}

/**
 * Add Copy nodes for not fulfilled should_be_same constraints.
 */
void amd64_finish_irg(ir_graph *const irg)
{
	irg_block_walk_graph(irg, 0, amd64_finish_irg_walker, 0);
}

void amd64_init_finish(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.finish");
}
