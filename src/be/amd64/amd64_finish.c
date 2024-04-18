/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief   This file implements functions to finalize the irg for emit.
 */
#include "amd64_finish.h"

#include "amd64_bearch_t.h"
#include "amd64_new_nodes.h"
#include "amd64_nodes_attr.h"
#include "amd64_transform.h"
#include "be2addr.h"
#include "bearch.h"
#include "benode.h"
#include "besched.h"
#include "gen_amd64_new_nodes.h"
#include "gen_amd64_regalloc_if.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "panic.h"
#include "util.h"

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
		unsigned bits = x86_bytes_from_size(attr->base.base.size) * 8;
		ir_tarval *tv = get_mode_one(amd64_mode_xmm);
		tv = tarval_shl_unsigned(tv, bits - 1);
		ir_entity *sign_bit_const = create_float_const_entity(tv);

		amd64_binop_addr_attr_t xor_attr = {
			.base = {
				.base = {
					.op_mode = AMD64_OP_REG_ADDR,
					.size    = X86_SIZE_64,
				},
			},
		};
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
		ir_node *neg = new_bd_amd64_neg(dbgi, block, in2, attr->base.base.size);
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

	x86_addr_t new_addr = attr->addr;
	ir_node *load_in[3];
	int      load_arity = 0;
	x86_addr_variant_t variant = attr->addr.variant;
	if (x86_addr_variant_has_base(variant)) {
		int base_input = load_arity++;
		new_addr.base_input = base_input;
		load_in[base_input] = get_irn_n(node, attr->addr.base_input);
	}
	if (x86_addr_variant_has_index(variant)) {
		int index_input = load_arity++;
		new_addr.index_input = index_input;
		load_in[index_input] = get_irn_n(node, attr->addr.index_input);
	}
	int mem_input = load_arity++;
	new_addr.mem_input = mem_input;
	load_in[mem_input] = get_irn_n(node, attr->addr.mem_input);
	assert(get_irn_mode(load_in[mem_input]) == mode_M);

	ir_node *const load     = new_bd_amd64_mov_gp(dbgi, block, load_arity, load_in, gp_am_reqs[load_arity - 1], attr->base.size, AMD64_OP_ADDR, new_addr);
	ir_node *const load_res = be_new_Proj_reg(load, pn_amd64_mov_gp_res, out_reg);

	/* change operation */
	const amd64_binop_addr_attr_t *binop_attr
		= (const amd64_binop_addr_attr_t*)attr;
	ir_node *new_in[2];
	new_in[0] = get_irn_n(node, binop_attr->u.reg_input);
	new_in[1] = load_res;
	set_irn_in(node, ARRAY_SIZE(new_in), new_in);
	attr->base.op_mode = AMD64_OP_REG_REG;
	attr->addr = (x86_addr_t) {
		.base_input = 0,
		.variant    = X86_ADDR_REG,
	};

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

static bool amd64_handle_2addr(ir_node *const node, arch_register_req_t const *const req, arch_register_t const *const out_reg)
{
	(void)req;

	amd64_attr_t const *const attr = get_amd64_attr_const(node);
	if (attr->op_mode == AMD64_OP_REG_ADDR) {
		x86_addr_t const *const addr = &get_amd64_addr_attr_const(node)->addr;
		if ((x86_addr_variant_has_base( addr->variant) && arch_get_irn_register_in(node, addr->base_input)  == out_reg) ||
		    (x86_addr_variant_has_index(addr->variant) && arch_get_irn_register_in(node, addr->index_input) == out_reg)) {
			amd64_turn_back_am(node, out_reg);
			goto swap;
		 }
	} else if (attr->op_mode == AMD64_OP_REG_REG) {
		if (arch_get_irn_register_in(node, 1) == out_reg) {
swap:
			if (try_swap_inputs(node)) {
				return true;
			} else if (is_amd64_sub(node) || is_amd64_subs(node)) {
				transform_sub_to_neg_add(node, out_reg);
				return true;
			}
		}
	}

	return false;
}

/**
 * Add Copy nodes for not fulfilled should_be_same constraints.
 */
void amd64_finish_irg(ir_graph *const irg)
{
	be_handle_2addr(irg, &amd64_handle_2addr);
}
