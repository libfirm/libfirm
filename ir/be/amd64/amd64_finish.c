/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief   This file implements functions to finalize the irg for emit.
 */
#include "amd64_finish.h"
#include "amd64_new_nodes.h"
#include "amd64_nodes_attr.h"
#include "amd64_transform.h"
#include "bearch.h"
#include "bearch_amd64_t.h"
#include "benode.h"
#include "besched.h"
#include "debug.h"
#include "panic.h"
#include "gen_amd64_new_nodes.h"
#include "gen_amd64_regalloc_if.h"
#include "irgwalk.h"
#include "util.h"
#include "irgmod.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/**
 * Returns the index of the first "same" register.
 */
static unsigned get_first_same(arch_register_req_t const *const req)
{
	unsigned const other = req->should_be_same;
	for (unsigned i = 0; i != 32; ++i) {
		if (other & (1U << i))
			return i;
	}
	panic("same position not found");
}

static bool is_commutative(const ir_node *node)
{
	return arch_get_irn_flags(node) & amd64_arch_irn_flag_commutative_binop;
}

static bool try_swap_inputs(ir_node *node)
{
	/* commutative operation, just switch the inputs */
	if (is_commutative(node)) {
		assert(get_amd64_attr_const(node)->op_mode == AMD64_OP_REG_REG);
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
	ir_node  *block = get_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);

	ir_node *in1 = get_irn_n(node, 0);
	ir_node *in2 = get_irn_n(node, 1);

	const arch_register_t *in2_reg = arch_get_irn_register(in2);

	const amd64_binop_addr_attr_t *attr = get_amd64_binop_addr_attr(node);
	ir_node                       *add;
	unsigned                       pos;
	if (is_amd64_subs(node)) {
		int bits = get_insn_mode_bits(attr->base.insn_mode);
		ir_tarval *tv = get_mode_one(amd64_mode_xmm);
		tv = tarval_shl_unsigned(tv, bits - 1);
		ir_entity *sign_bit_const = create_float_const_entity(tv);

		amd64_binop_addr_attr_t xor_attr;
		memset(&xor_attr, 0, sizeof(xor_attr));
		xor_attr.base.insn_mode             = INSN_MODE_64;
		xor_attr.base.base.op_mode          = AMD64_OP_REG_ADDR;
		init_lconst_addr(&xor_attr.base.addr, sign_bit_const);

		ir_node *xor_in[] = { in2 };
		ir_node *const xor = new_bd_amd64_xorp(dbgi, block, ARRAY_SIZE(xor_in), xor_in, amd64_xmm_reqs, &xor_attr);
		sched_add_before(node, xor);
		ir_node *const neg = be_new_Proj_reg(xor, pn_amd64_xorp_res, in2_reg);

		ir_node *in[] = { neg, in1 };
		add = new_bd_amd64_adds(dbgi, block, ARRAY_SIZE(in), in, amd64_xmm_xmm_reqs, attr);
		pos = pn_amd64_adds_res;
	} else {
		assert(is_amd64_sub(node));
		ir_node *neg = new_bd_amd64_neg(dbgi, block, in2, attr->base.insn_mode);
		sched_add_before(node, neg);
		ir_node *const neg_res = be_new_Proj_reg(neg, pn_amd64_neg_res, out_reg);

		ir_node *in[] = { neg_res, in1 };
		add = new_bd_amd64_add(dbgi, block, ARRAY_SIZE(in), in, amd64_reg_reg_reqs, attr);
		pos = pn_amd64_add_res;
	}
	arch_set_irn_register_out(add, pos, out_reg);

	/* exchange the add and the sub */
	sched_replace(node, add);
	exchange(node, add);
}

static void amd64_turn_back_am(ir_node *const node, arch_register_t const *const out_reg)
{
	dbg_info          *dbgi  = get_irn_dbg_info(node);
	ir_node           *block = get_nodes_block(node);
	amd64_addr_attr_t *attr  = get_amd64_addr_attr(node);

	amd64_addr_t new_addr = attr->addr;
	ir_node *load_in[3];
	int      load_arity = 0;
	if (attr->addr.base_input != NO_INPUT &&
	    attr->addr.base_input != RIP_INPUT) {
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

	ir_node *const load     = new_bd_amd64_mov_gp(dbgi, block, load_arity, load_in, gp_am_reqs[load_arity - 1], attr->insn_mode, AMD64_OP_ADDR, new_addr);
	ir_node *const load_res = be_new_Proj_reg(load, pn_amd64_mov_gp_res, out_reg);

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
			set_Proj_num(out, pn_amd64_mov_gp_M);
			break;
		}
	}

	if (sched_is_scheduled(node))
		sched_add_before(node, load);
}

/**
 * Insert copies for all amd64 nodes where the should_be_same requirement is
 * not fulfilled.
 */
static void assure_should_be_same_requirements(ir_node *const node)
{
	/* Check all OUT requirements, if there is a should_be_same. */
	be_foreach_out(node, i) {
		arch_register_req_t const *const req
			= arch_get_irn_register_req_out(node, i);
		if (req->should_be_same == 0)
			continue;
		unsigned               const same_pos = get_first_same(req);
		ir_node               *const in_node  = get_irn_n(node, same_pos);
		arch_register_t const *const in_reg   = arch_get_irn_register(in_node);
		arch_register_t const *const out_reg
			= arch_get_irn_register_out(node, i);
		if (in_reg == out_reg)
			continue;

		/* test if any other input is using the out register */
		foreach_irn_in(node, i2, in) {
			arch_register_t const *const reg = arch_get_irn_register(in);
			if (reg == out_reg && (unsigned)i2 != same_pos) {
				if (!is_amd64_irn(node))
					panic("cannot fulfill should_be_same on non-amd64 node");
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

					if (is_amd64_sub(node) || is_amd64_subs(node)) {
						transform_sub_to_neg_add(node, out_reg);
						return;
					}
					panic("couldn't swap inputs of %+F", node);
				} else {
					assert(attr->op_mode == AMD64_OP_REG_ADDR);
					/* extract load into an own instruction */
					amd64_turn_back_am(node, out_reg);
					goto swap;
				}
			}
		}

		ir_node *const copy = be_new_Copy_before_reg(in_node, node, out_reg);
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

	/* Insert copies for should_be_same constraints. */
	sched_foreach_safe(block, irn) {
		if (is_amd64_irn(irn))
			assure_should_be_same_requirements(irn);
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
