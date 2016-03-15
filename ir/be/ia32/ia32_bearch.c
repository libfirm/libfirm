/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This is the main ia32 firm backend driver.
 * @author      Christian Wuerdig
 */
#include "ia32_bearch_t.h"

#include "be_t.h"
#include "beflags.h"
#include "begnuas.h"
#include "bemodule.h"
#include "bera.h"
#include "besched.h"
#include "bespillslots.h"
#include "bestack.h"
#include "beutil.h"
#include "bevarargs.h"
#include "gen_ia32_regalloc_if.h"
#include "ia32_architecture.h"
#include "ia32_emitter.h"
#include "ia32_encode.h"
#include "ia32_finish.h"
#include "ia32_fpu.h"
#include "ia32_new_nodes.h"
#include "ia32_optimize.h"
#include "ia32_transform.h"
#include "ident_t.h"
#include "instrument.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "iropt_t.h"
#include "irtools.h"
#include "lc_opts_enum.h"
#include "lower_alloc.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "lower_mode_b.h"
#include "lower_softfloat.h"
#include "lowering.h"
#include "panic.h"
#include "x86_x87.h"

pmap *ia32_tv_ent; /**< A map of entities that store const tarvals */

ir_mode *ia32_mode_fpcw;
ir_mode *ia32_mode_flags;
ir_mode *ia32_mode_8h;
ir_mode *ia32_mode_gp;
ir_mode *ia32_mode_float64;
ir_mode *ia32_mode_float32;

/** The current omit-fp state */
static ir_type   *omit_fp_between_type;
static ir_type   *between_type;
static ir_entity *old_bp_ent;
static ir_entity *ret_addr_ent;
static ir_entity *omit_fp_ret_addr_ent;
static bool       return_small_struct_in_regs;

typedef ir_node *(*create_const_node_func) (dbg_info *dbgi, ir_node *block);

/**
 * Used to create per-graph unique pseudo nodes.
 */
static inline ir_node *create_const(ir_graph *irg, ir_node **place,
                                    create_const_node_func func,
                                    const arch_register_t* reg)
{
	if (*place != NULL)
		return *place;

	ir_node *block = get_irg_start_block(irg);
	ir_node *res   = func(NULL, block);
	arch_set_irn_register(res, reg);
	*place = res;
	/* We need a keep edge on our cached nodes, so that following firm
	 * irgwalks will not miss them. */
	keep_alive(res);
	return res;
}

ir_node *ia32_new_NoReg_gp(ir_graph *irg)
{
	ia32_irg_data_t *irg_data = ia32_get_irg_data(irg);
	return create_const(irg, &irg_data->noreg_gp, new_bd_ia32_NoReg_GP,
	                    &ia32_registers[REG_GP_NOREG]);
}

ir_node *ia32_new_NoReg_fp(ir_graph *irg)
{
	ia32_irg_data_t *irg_data = ia32_get_irg_data(irg);
	return create_const(irg, &irg_data->noreg_fp, new_bd_ia32_NoReg_FP,
	                    &ia32_registers[REG_FP_NOREG]);
}

ir_node *ia32_new_NoReg_xmm(ir_graph *irg)
{
	ia32_irg_data_t *irg_data = ia32_get_irg_data(irg);
	return create_const(irg, &irg_data->noreg_xmm, new_bd_ia32_NoReg_XMM,
	                    &ia32_registers[REG_XMM_NOREG]);
}

ir_node *ia32_new_Fpu_truncate(ir_graph *irg)
{
	ia32_irg_data_t *irg_data = ia32_get_irg_data(irg);
	return create_const(irg, &irg_data->fpu_trunc_mode, new_bd_ia32_ChangeCW,
                        &ia32_registers[REG_FPCW]);
}

/**
 * Returns the admissible noreg register node for input register pos of node irn.
 */
static ir_node *ia32_get_admissible_noreg(ir_node *irn, int pos)
{
	ir_graph                  *irg = get_irn_irg(irn);
	const arch_register_req_t *req = arch_get_irn_register_req_in(irn, pos);
	if (req->cls == &ia32_reg_classes[CLASS_ia32_gp])
		return ia32_new_NoReg_gp(irg);

	if (ia32_cg_config.use_sse2) {
		return ia32_new_NoReg_xmm(irg);
	} else {
		return ia32_new_NoReg_fp(irg);
	}
}

static ir_entity *ia32_get_frame_entity(const ir_node *node)
{
	if (!is_ia32_irn(node))
		return NULL;
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	if (attr->am_imm.kind == X86_IMM_FRAMEENT) {
		assert(get_ia32_frame_use(node) != IA32_FRAME_USE_NONE);
		return attr->am_imm.entity;
	}
	assert(get_ia32_frame_use(node) == IA32_FRAME_USE_NONE);
	return NULL;
}

static void ia32_set_frame_entity(ir_node *node, ir_entity *entity,
                                  const ir_type *type)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	attr->am_imm = (x86_imm32_t) {
		.kind   = X86_IMM_FRAMEENT,
		.entity = entity,
		.offset = attr->am_imm.offset,
	};
	assert(get_ia32_frame_use(node) != IA32_FRAME_USE_NONE);

	/* set ls_mode based on entity unless we explicitly requested
	 * a certain mode */
	if (get_ia32_frame_use(node) != IA32_FRAME_USE_AUTO
	    || is_ia32_Cmp(node) || is_ia32_Conv_I2I(node))
		return;
	ir_mode *mode = get_type_mode(type);
	/** we 8bit stores have a special register requirement, so we can't simply
	 * change the ls_mode to 8bit here. The "hack" in
	 * ia32_collect_frame_entity_nodes() should take care that it never happens
	 */
	assert(!is_ia32_Store(node) || get_mode_size_bits(mode) > 8);
	set_ia32_ls_mode(node, mode);
}

static void ia32_set_frame_offset(ir_node *node, int bias)
{
	ia32_attr_t *const attr = get_ia32_attr(node);
	assert(attr->am_imm.kind == X86_IMM_FRAMEENT);

	/* Pop nodes modify the stack pointer before calculating the
	 * destination address, fix this here */
	if (is_ia32_PopMem(node)) {
		ir_node *base = get_irn_n(node, n_ia32_PopMem_base);
		if (arch_get_irn_register(base) == &ia32_registers[REG_ESP]) {
			ir_mode *mode = get_ia32_ls_mode(node);
			bias -= get_mode_size_bytes(mode);
		}
	}

#ifndef NDEBUG
	attr->old_frame_ent = attr->am_imm.entity;
#endif
	/* This is just a simple 32bit value now */
	attr->am_imm.offset += bias;
	attr->am_imm.entity = NULL;
	attr->am_imm.kind   = X86_IMM_VALUE;
}

int ia32_get_sp_bias(const ir_node *node)
{
	if (is_ia32_Call(node))
		return -(int)get_ia32_call_attr_const(node)->pop;

	if (is_ia32_Push(node)) {
		ir_mode *ls_mode = get_ia32_ls_mode(node);
		return get_mode_size_bytes(ls_mode);
	}

	if (is_ia32_Pop(node) || is_ia32_PopMem(node)) {
		ir_mode *ls_mode = get_ia32_ls_mode(node);
		return -get_mode_size_bytes(ls_mode);
	}

	if (is_ia32_Leave(node) || is_ia32_CopyEbpEsp(node))
		return SP_BIAS_RESET;

	return 0;
}

/**
 * Build the between type and entities if not already built.
 */
static void ia32_build_between_type(void)
{
	if (between_type == NULL) {
		ir_type *old_bp_type   = new_type_primitive(ia32_mode_gp);
		ir_type *ret_addr_type = new_type_primitive(ia32_mode_gp);

		between_type = new_type_struct(NEW_IDENT("ia32_between_type"));
		old_bp_ent   = new_entity(between_type, NEW_IDENT("old_bp"), old_bp_type);
		ret_addr_ent = new_entity(between_type, NEW_IDENT("ret_addr"), ret_addr_type);

		set_entity_offset(old_bp_ent, 0);
		set_entity_offset(ret_addr_ent, get_type_size(old_bp_type));
		set_type_size(between_type, get_type_size(old_bp_type) + get_type_size(ret_addr_type));
		set_type_state(between_type, layout_fixed);

		omit_fp_between_type = new_type_struct(NEW_IDENT("ia32_between_type_omit_fp"));
		omit_fp_ret_addr_ent = new_entity(omit_fp_between_type, NEW_IDENT("ret_addr"), ret_addr_type);

		set_entity_offset(omit_fp_ret_addr_ent, 0);
		set_type_size(omit_fp_between_type, get_type_size(ret_addr_type));
		set_type_state(omit_fp_between_type, layout_fixed);
	}
}

/**
 * Return the stack entity that contains the return address.
 */
ir_entity *ia32_get_return_address_entity(ir_graph *irg)
{
	ia32_build_between_type();
	return ia32_get_irg_data(irg)->omit_fp ? omit_fp_ret_addr_ent
	                                       : ret_addr_ent;
}

/**
 * Return the stack entity that contains the frame address.
 */
ir_entity *ia32_get_frame_address_entity(ir_graph *irg)
{
	ia32_build_between_type();
	return ia32_get_irg_data(irg)->omit_fp ? NULL : old_bp_ent;
}

/**
 * Get the estimated cycle count for @p irn.
 *
 * @param irn  The node.
 *
 * @return     The estimated cycle count for this operation
 */
static unsigned ia32_get_op_estimated_cost(ir_node const *const irn)
{
	if (!is_ia32_irn(irn))
		return 1;

	if (is_ia32_CopyB_i(irn)) {
		unsigned const size = get_ia32_copyb_size(irn);
		return 20 + size * 4 / 3;
	}

	unsigned cost = get_ia32_latency(irn);

	/* in case of address mode operations add additional cycles */
	if (get_ia32_op_type(irn) != ia32_Normal) {
		if (get_ia32_frame_use(irn) != IA32_FRAME_USE_NONE || (
		      is_ia32_NoReg_GP(get_irn_n(irn, n_ia32_base)) &&
		      is_ia32_NoReg_GP(get_irn_n(irn, n_ia32_index))
		    )) {
			/* Stack access, assume it is cached. */
			cost += 5;
		} else {
			/* Access probably elsewhere. */
			cost += 20;
		}
	}

	return cost;
}

static ir_mode *get_spill_mode(const ir_node *value)
{
	/* determine a sensible spill mode and try to make it small */
	const ir_node *skipped = skip_Proj_const(value);
	if (is_ia32_fld(skipped) || is_ia32_Load(skipped))
		return get_ia32_ls_mode(skipped);

	return get_irn_mode(value);
}

/**
 * Check if irn can load its operand at position i from memory (source addressmode).
 * @param irn    The irn to be checked
 * @param i      The operands position
 * @return whether operand can be loaded
 */
static bool ia32_possible_memory_operand(const ir_node *irn, unsigned int i)
{
	if (!is_ia32_irn(irn)                    || /* must be an ia32 irn */
	    get_ia32_op_type(irn) != ia32_Normal || /* must not already be a addressmode irn */
	    get_ia32_frame_use(irn) != IA32_FRAME_USE_NONE) /* must not already use frame */
		return false;

	switch (get_ia32_am_support(irn)) {
	case ia32_am_none:
		return false;

	case ia32_am_unary:
		if (i != n_ia32_unary_op)
			return false;
		break;

	case ia32_am_binary:
		switch (i) {
		case n_ia32_binary_left: {
			if (!is_ia32_commutative(irn))
				return false;

			/* we can't swap left/right for limited registers
			 * (As this (currently) breaks constraint handling copies) */
			arch_register_req_t const *const req = arch_get_irn_register_req_in(irn, n_ia32_binary_left);
			if (req->limited != NULL)
				return false;
			break;
		}

		case n_ia32_binary_right:
			break;

		default:
			return false;
		}
		break;

	default:
		panic("unknown AM type");
	}

	/* HACK: must not already use "real" memory.
	 * This can happen for Call and Div */
	if (!is_NoMem(get_irn_n(irn, n_ia32_mem)))
		return false;

	ir_node       *const op   = get_irn_n(irn, i);
	ir_node const *const load = get_Proj_pred(op);
	ir_mode const *const mode = get_ia32_ls_mode(load);
	if (mode_is_float(mode)) {
		if (mode != ia32_mode_float64 && mode != mode_F)
			return false;
		/* Don't do reload folding for x87 nodes for now, as we can't predict
		 * yet wether the spillslot must be widened to 80bit for which no AM
		 * operations exist. */
		if (is_ia32_fld(load))
			return false;
	}

	return true;
}

static void ia32_perform_memory_operand(ir_node *irn, unsigned int i)
{
	if (!ia32_possible_memory_operand(irn, i))
		return;

	ir_node *op           = get_irn_n(irn, i);
	ir_node *load         = get_Proj_pred(op);
	ir_mode *load_mode    = get_ia32_ls_mode(load);
	ir_node *spill        = get_irn_n(load, n_ia32_mem);
	ir_mode *dest_op_mode = get_ia32_ls_mode(irn);
	if (get_mode_size_bits(load_mode) <= get_mode_size_bits(dest_op_mode))
		set_ia32_ls_mode(irn, load_mode);
	set_ia32_op_type(irn, ia32_AddrModeS);
	set_ia32_frame_use(irn, IA32_FRAME_USE_AUTO);

	if (i == n_ia32_binary_left                    &&
	    get_ia32_am_support(irn) == ia32_am_binary &&
	    /* immediates are only allowed on the right side */
	    !is_ia32_Immediate(get_irn_n(irn, n_ia32_binary_right))) {
		ia32_swap_left_right(irn);
		i = n_ia32_binary_right;
	}

	assert(is_NoMem(get_irn_n(irn, n_ia32_mem)));

	set_irn_n(irn, n_ia32_base, get_irg_frame(get_irn_irg(irn)));
	set_irn_n(irn, n_ia32_mem,  spill);
	set_irn_n(irn, i,           ia32_get_admissible_noreg(irn, i));
	set_ia32_is_reload(irn);

	/* kill the reload */
	assert(get_irn_n_edges(op) == 0);
	assert(get_irn_n_edges(load) == 1);
	sched_remove(load);
	kill_node(op);
	kill_node(load);
}

static bool gprof;

static ir_node *ia32_turn_back_dest_am(ir_node *node)
{
	typedef ir_node *construct_binop_func(
		dbg_info *db, ir_node *block,
		ir_node *base, ir_node *index, ir_node *mem,
		ir_node *op1, ir_node *op2);

	ir_mode *ls_mode = get_ia32_ls_mode(node);
	bool     is_8bit = get_mode_size_bits(ls_mode) == 8;

	construct_binop_func *func;
	switch (get_ia32_irn_opcode(node)) {
	case iro_ia32_AddMem: func = is_8bit ? new_bd_ia32_Add_8bit : new_bd_ia32_Add; break;
	case iro_ia32_AndMem: func = is_8bit ? new_bd_ia32_And_8bit : new_bd_ia32_And; break;
	case iro_ia32_OrMem:  func = is_8bit ? new_bd_ia32_Or_8bit  : new_bd_ia32_Or;  break;
	case iro_ia32_SubMem: func = is_8bit ? new_bd_ia32_Sub_8bit : new_bd_ia32_Sub; break;
	case iro_ia32_XorMem: func = is_8bit ? new_bd_ia32_Xor_8bit : new_bd_ia32_Xor; break;
	default: panic("cannot turn back DestAM for %+F", node);
	}

	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = get_nodes_block(node);
	ir_node  *const base  = get_irn_n(node, n_ia32_base);
	ir_node  *const idx   = get_irn_n(node, n_ia32_index);
	ir_node  *const mem   = get_irn_n(node, n_ia32_mem);
	ir_node  *const load  = new_bd_ia32_Load(dbgi, block, base, idx, mem);
	ia32_copy_am_attrs(load, node);
	if (is_ia32_is_reload(node))
		set_ia32_is_reload(load);
	sched_add_before(node, load);
	ir_node *const load_res = be_new_Proj(load, pn_ia32_Load_res);
	ir_node *const load_mem = be_new_Proj(load, pn_ia32_Load_M);

	ir_graph *const irg      = get_irn_irg(node);
	ir_node  *const noreg    = ia32_new_NoReg_gp(irg);
	ir_node  *const nomem    = get_irg_no_mem(irg);
	ir_node  *const operand  = get_irn_n(node, n_ia32_binary_left);
	ir_node  *const new_node = func(dbgi, block, noreg, noreg, nomem, load_res, operand);
	set_ia32_ls_mode(new_node, ls_mode);
	set_irn_mode(new_node, mode_T);

	arch_set_irn_register_out(new_node, pn_ia32_flags, &ia32_registers[REG_EFLAGS]);

	ir_node *const res_proj = be_new_Proj(new_node, pn_ia32_res);
	ir_node *const store    = is_8bit ? new_bd_ia32_Store_8bit(dbgi, block, base, idx, load_mem, res_proj)
	                                  : new_bd_ia32_Store(dbgi, block, base, idx, load_mem, res_proj);
	ia32_copy_am_attrs(store, node);
	set_ia32_op_type(store, ia32_AddrModeD);
	sched_add_after(node, store);

	ir_node *const mem_proj = get_Proj_for_pn(node, pn_ia32_M);
	set_Proj_pred(mem_proj, store);
	set_Proj_num(mem_proj, pn_ia32_Store_M);

	sched_replace(node, new_node);
	exchange(node, new_node);
	return new_node;
}

ir_node *ia32_turn_back_am(ir_node *node)
{
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_graph *irg      = get_irn_irg(node);
	ir_node  *block    = get_nodes_block(node);
	ir_node  *base     = get_irn_n(node, n_ia32_base);
	ir_node  *idx      = get_irn_n(node, n_ia32_index);
	ir_node  *mem      = get_irn_n(node, n_ia32_mem);
	ir_node  *load     = new_bd_ia32_Load(dbgi, block, base, idx, mem);
	ir_node  *load_res = be_new_Proj(load, pn_ia32_Load_res);

	ia32_copy_am_attrs(load, node);
	if (is_ia32_is_reload(node))
		set_ia32_is_reload(load);
	set_irn_n(node, n_ia32_mem, get_irg_no_mem(irg));

	switch (get_ia32_am_support(node)) {
	case ia32_am_unary:
		set_irn_n(node, n_ia32_unary_op, load_res);
		break;

	case ia32_am_binary:
		if (is_ia32_Immediate(get_irn_n(node, n_ia32_binary_right))) {
			set_irn_n(node, n_ia32_binary_left, load_res);
		} else {
			set_irn_n(node, n_ia32_binary_right, load_res);
		}
		break;

	default:
		panic("unknown AM type");
	}
	ir_node *noreg = ia32_new_NoReg_gp(irg);
	set_irn_n(node, n_ia32_base,  noreg);
	set_irn_n(node, n_ia32_index, noreg);
	ia32_attr_t *const attr = get_ia32_attr(node);
	attr->am_imm = (x86_imm32_t) { .kind = X86_IMM_VALUE, .offset = 0 };
	attr->frame_use = IA32_FRAME_USE_NONE;
	set_ia32_am_scale(node, 0);

	/* rewire mem-proj */
	if (get_irn_mode(node) == mode_T) {
		foreach_out_edge(node, edge) {
			ir_node *out = get_edge_src_irn(edge);
			if (get_irn_mode(out) == mode_M) {
				set_Proj_pred(out, load);
				set_Proj_num(out, pn_ia32_Load_M);
				break;
			}
		}
	}

	set_ia32_op_type(node, ia32_Normal);
	if (sched_is_scheduled(node))
		sched_add_before(node, load);

	return load_res;
}

static ir_node *flags_remat(ir_node *node, ir_node *after)
{
	/* we should turn back address modes when rematerializing nodes */
	ir_node *const block = get_block(after);

	ia32_op_type_t type = get_ia32_op_type(node);
	switch (type) {
	case ia32_AddrModeS:
		ia32_turn_back_am(node);
		break;

	case ia32_AddrModeD:
		node = ia32_turn_back_dest_am(node);
		break;

	default:
		assert(type == ia32_Normal);
		break;
	}

	ir_node *copy = exact_copy(node);
	set_nodes_block(copy, block);
	sched_add_after(after, copy);
	return copy;
}

COMPILETIME_ASSERT((int)(n_ia32_Sub_minuend)    == (int)(n_ia32_Cmp_left) &&
                   (int)(n_ia32_Sub_subtrahend) == (int)(n_ia32_Cmp_right),
                   Cmp_and_Sub_operand_numbers_equal)

static bool ia32_try_replace_flags(ir_node *consumers, ir_node *flags, ir_node *available)
{
	if (!is_ia32_Sub(flags) && !is_ia32_Cmp(flags))
		return false;
	unsigned pn;
	if (is_ia32_Sub(available)) {
		pn = pn_ia32_Sub_flags;
	} else if (is_ia32_Cmp(available)) {
		pn = pn_ia32_Cmp_eflags;
	} else {
		return false;
	}
	/* Assuming CSE would have found the more obvious case */
	ir_node *const flags_left  = get_irn_n(flags,     n_ia32_binary_left);
	ir_node *const avail_right = get_irn_n(available, n_ia32_binary_right);
	if (flags_left != avail_right)
		return false;
	ir_node *const avail_left  = get_irn_n(available, n_ia32_binary_left);
	ir_node *const flags_right = get_irn_n(flags,     n_ia32_binary_right);
	if (avail_left != flags_right)
		return false;

	/* We can use available if we reverse the consumers' condition codes. */
	arch_set_irn_register_out(available, pn, &ia32_registers[REG_EFLAGS]);
	ir_node *const proj      = get_irn_mode(available) == mode_T ? be_get_or_make_Proj_for_pn(available, pn) : available;
	ir_mode *const flag_mode = ia32_reg_classes[CLASS_ia32_flags].mode;
	for (ir_node *c = consumers; c != NULL; c = get_irn_link(c)) {
		x86_condition_code_t cc = get_ia32_condcode(c);
		set_ia32_condcode(c, x86_invert_condition_code(cc));

		foreach_irn_in(c, i, in) {
			if (get_irn_mode(in) == flag_mode)
				set_irn_n(c, i, proj);
		}
	}
	return true;
}

static void remat_simplifier(ir_node *node, void *env)
{
	(void)env;

	/* A Sub with unused result is a Cmp. */
	if (is_ia32_Sub(node) && get_irn_mode(node) == mode_T) {
		ir_node *projs[] = { [pn_ia32_Sub_M] = NULL };
		foreach_out_edge(node, out) {
			ir_node *const proj = get_edge_src_irn(out);
			unsigned const num  = get_Proj_num(proj);
			assert(num < ARRAY_SIZE(projs));
			assert(!projs[num] && "duplicate Proj");
			projs[num] = proj;
		}

		ir_node       *res_keep = NULL;
		ir_node *const sub_res  = projs[pn_ia32_Sub_res];
		if (sub_res) {
			foreach_out_edge(sub_res, out) {
				ir_node *const user = get_edge_src_irn(out);
				if (be_is_Keep(user)) {
					assert(!res_keep && "Proj has two be_Keep");
					res_keep = user;
				} else {
					return;
				}
			}
		}

		dbg_info *const dbgi    = get_irn_dbg_info(node);
		ir_node  *const block   = get_nodes_block(node);
		ir_node  *const base    = get_irn_n(node, n_ia32_Sub_base);
		ir_node  *const idx     = get_irn_n(node, n_ia32_Sub_index);
		ir_node  *const mem     = get_irn_n(node, n_ia32_Sub_mem);
		ir_node  *const minu    = get_irn_n(node, n_ia32_Sub_minuend);
		ir_node  *const subt    = get_irn_n(node, n_ia32_Sub_subtrahend);
		ir_mode  *const ls_mode = get_ia32_ls_mode(node);
		bool            is_8bit = get_mode_size_bits(ls_mode) == 8;
		ir_node        *cmp     = is_8bit ? new_bd_ia32_Cmp_8bit(dbgi, block, base, idx, mem, minu, subt, false)
		                                  : new_bd_ia32_Cmp(dbgi, block, base, idx, mem, minu, subt, false);
		arch_set_irn_register(cmp, &ia32_registers[REG_EFLAGS]);
		ia32_copy_am_attrs(cmp, node);

		sched_replace(node, cmp);

		if (get_ia32_op_type(node) == ia32_AddrModeS) {
			set_ia32_op_type(cmp, ia32_AddrModeS);
			set_irn_mode(cmp, mode_T);

			ir_node *const sub_mem = projs[pn_ia32_Sub_M];
			if (sub_mem) {
				ir_node *const proj_M = be_new_Proj(cmp, pn_ia32_Cmp_M);
				exchange(sub_mem, proj_M);
			}

			cmp = be_new_Proj(cmp, pn_ia32_Cmp_eflags);
		} else {
			assert(get_ia32_op_type(node) == ia32_Normal);
		}

		exchange(projs[pn_ia32_Sub_flags], cmp);

		if (res_keep) {
			sched_remove(res_keep);
			remove_keep_alive(res_keep);
			kill_node(res_keep);
		}
		kill_node(node);
	}
}

static void simplify_remat_nodes(ir_graph *irg)
{
	irg_walk_graph(irg, remat_simplifier, NULL, NULL);
	remove_End_Bads_and_doublets(get_irg_end(irg));
}

static ir_node *ia32_new_spill(ir_node *value, ir_node *after)
{
	ir_graph *irg   = get_irn_irg(value);
	ir_node  *block = get_block(after);
	ir_node  *frame = get_irg_frame(irg);
	ir_mode  *mode  = get_spill_mode(value);
	ir_node  *noreg = ia32_new_NoReg_gp(irg);
	ir_node  *nomem = get_irg_no_mem(irg);

	ir_node *res;
	ir_node *store;
	if (mode_is_float(mode)) {
		if (ia32_cg_config.use_sse2) {
			store = new_bd_ia32_xStore(NULL, block, frame, noreg, nomem, value);
			res   = be_new_Proj(store, pn_ia32_xStore_M);
		} else {
			store = new_bd_ia32_fst(NULL, block, frame, noreg, nomem, value, mode);
			res   = be_new_Proj(store, pn_ia32_fst_M);
		}
	} else if (get_mode_size_bits(mode) == 128) {
		/* Spill 128 bit SSE registers */
		store = new_bd_ia32_xxStore(NULL, block, frame, noreg, nomem, value);
		res   = be_new_Proj(store, pn_ia32_xxStore_M);
	} else {
		store = get_mode_size_bits(mode) == 8
			? new_bd_ia32_Store_8bit(NULL, block, frame, noreg, nomem, value)
			: new_bd_ia32_Store     (NULL, block, frame, noreg, nomem, value);
		res   = be_new_Proj(store, pn_ia32_Store_M);
	}
	set_ia32_op_type(store, ia32_AddrModeD);
	set_ia32_ls_mode(store, mode);
	set_ia32_frame_use(store, IA32_FRAME_USE_AUTO);
	set_ia32_is_spill(store);
	sched_add_after(after, store);

	return res;
}

static ir_node *ia32_new_reload(ir_node *value, ir_node *spill, ir_node *before)
{
	ir_graph *irg       = get_irn_irg(before);
	ir_node  *block     = get_block(before);
	ir_mode  *spillmode = get_spill_mode(value);
	ir_node  *noreg     = ia32_new_NoReg_gp(irg);
	ir_node  *frame     = get_irg_frame(irg);

	ir_node *load;
	if (mode_is_float(spillmode)) {
		if (ia32_cg_config.use_sse2) {
			load = new_bd_ia32_xLoad(NULL, block, frame, noreg, spill, spillmode);
		} else {
			load = new_bd_ia32_fld(NULL, block, frame, noreg, spill, spillmode);
		}
	} else if (get_mode_size_bits(spillmode) == 128) {
		/* Reload 128 bit SSE registers */
		load = new_bd_ia32_xxLoad(NULL, block, frame, noreg, spill);
	} else {
		load = new_bd_ia32_Load(NULL, block, frame, noreg, spill);
	}
	set_ia32_op_type(load, ia32_AddrModeS);
	set_ia32_ls_mode(load, spillmode);
	set_ia32_frame_use(load, IA32_FRAME_USE_AUTO);
	set_ia32_is_reload(load);
	arch_add_irn_flags(load, arch_irn_flag_reload);
	sched_add_before(before, load);

	return be_new_Proj(load, pn_ia32_res);
}

static ir_node *create_push(ir_node *node, ir_node *schedpoint, ir_node *sp,
                            ir_node *mem, ir_entity *ent, ir_mode *mode)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *noreg = ia32_new_NoReg_gp(irg);
	ir_node  *frame = get_irg_frame(irg);

	ir_node *const push = new_bd_ia32_Push(dbgi, block, frame, noreg, mem,
	                                       noreg, sp, mode);
	ia32_attr_t *const attr = get_ia32_attr(push);
	attr->am_imm = (x86_imm32_t) {
		.kind   = X86_IMM_FRAMEENT,
		.entity = ent,
	};
	set_ia32_frame_use(push, IA32_FRAME_USE_AUTO);
	set_ia32_op_type(push, ia32_AddrModeS);
	set_ia32_is_spill(push);

	sched_add_before(schedpoint, push);
	return push;
}

static ir_node *create_pop(ir_node *node, ir_node *schedpoint, ir_node *sp,
                           ir_entity *ent, ir_mode *mode)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *noreg = ia32_new_NoReg_gp(irg);
	ir_node  *frame = get_irg_frame(irg);
	ir_node  *pop   = new_bd_ia32_PopMem(dbgi, block, frame, noreg,
	                                     get_irg_no_mem(irg), sp);
	ia32_attr_t *const attr = get_ia32_attr(pop);
	attr->am_imm = (x86_imm32_t) {
		.kind   = X86_IMM_FRAMEENT,
		.entity = ent,
	};
	set_ia32_frame_use(pop, IA32_FRAME_USE_AUTO);
	set_ia32_op_type(pop, ia32_AddrModeD);
	set_ia32_ls_mode(pop, mode);
	set_ia32_is_reload(pop);
	sched_add_before(schedpoint, pop);
	return pop;
}

static ir_node *create_spproj(ir_node *const pred, unsigned const pos)
{
	return be_new_Proj_reg(pred, pos, &ia32_registers[REG_ESP]);
}

/**
 * Transform MemPerm, currently we do this the ugly way and produce
 * push/pop into/from memory cascades. This is possible without using
 * any registers.
 */
static void transform_MemPerm(ir_node *node)
{
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *sp    = be_get_Start_proj(irg, &ia32_registers[REG_ESP]);
	int       arity = be_get_MemPerm_entity_arity(node);
	ir_node **pops  = ALLOCAN(ir_node*, arity);

	/* create Pushs */
	for (int i = 0; i < arity; ++i) {
		ir_entity *inent = be_get_MemPerm_in_entity(node, i);
		ir_entity *outent = be_get_MemPerm_out_entity(node, i);
		ir_type *enttype = get_entity_type(inent);
		unsigned entsize = get_type_size(enttype);
		unsigned entsize2 = get_type_size(get_entity_type(outent));
		ir_node *mem = get_irn_n(node, i);

		/* work around cases where entities have different sizes */
		if (entsize2 < entsize)
			entsize = entsize2;

		int offset = 0;
		do {
			ir_mode *mode;
			if (entsize%2 == 1) {
				mode = mode_Bu;
			} else if (entsize % 4 == 2) {
				mode = mode_Hu;
			} else {
				assert(entsize%4 == 0);
				mode = ia32_mode_gp;
			}

			ir_node *push = create_push(node, node, sp, mem, inent, mode);
			sp = create_spproj(push, pn_ia32_Push_stack);
			add_ia32_am_offs_int(push, offset);

			unsigned size = get_mode_size_bytes(mode);
			offset  += size;
			entsize -= size;
		} while(entsize > 0);
		set_irn_n(node, i, new_r_Bad(irg, mode_X));
	}

	/* create pops */
	for (int i = arity; i-- > 0; ) {
		ir_entity *inent = be_get_MemPerm_in_entity(node, i);
		ir_entity *outent = be_get_MemPerm_out_entity(node, i);
		ir_type *enttype = get_entity_type(outent);
		unsigned entsize = get_type_size(enttype);
		unsigned entsize2 = get_type_size(get_entity_type(inent));

		/* work around cases where entities have different sizes */
		if (entsize2 < entsize)
			entsize = entsize2;

		int      offset = entsize;
		ir_node *pop;
		do {
			ir_mode *mode;
			if (entsize%2 == 1) {
				mode = mode_Bu;
			} else if (entsize%4 == 2) {
				mode = mode_Hu;
			} else {
				assert(entsize%4 == 0);
				mode = ia32_mode_gp;
			}
			pop = create_pop(node, node, sp, outent, mode);
			sp  = create_spproj(pop, pn_ia32_PopMem_stack);

			unsigned size = get_mode_size_bytes(mode);
			offset  -= size;
			entsize -= size;
			add_ia32_am_offs_int(pop, offset);
		} while(entsize > 0);
		pops[i] = pop;
	}

	ir_node *const keep = be_new_Keep_one(sp);
	sched_replace(node, keep);

	/* exchange memprojs */
	foreach_out_edge_safe(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		unsigned p = get_Proj_num(proj);

		assert(p < (unsigned)arity);

		set_Proj_pred(proj, pops[p]);
		set_Proj_num(proj, pn_ia32_PopMem_M);
	}

	/* remove memperm */
	kill_node(node);
}

/**
 * Block-Walker: Calls the transform functions Spill and Reload.
 */
static void ia32_after_ra_walker(ir_node *block, void *env)
{
	(void)env;

	/* beware: the schedule is changed here */
	sched_foreach_reverse_safe(block, node) {
		if (be_is_MemPerm(node)) {
			transform_MemPerm(node);
		}
	}
}

/**
 * Collects nodes that need frame entities assigned.
 */
static void ia32_collect_frame_entity_nodes(ir_node *node, void *data)
{
	be_fec_env_t *env = (be_fec_env_t*)data;
	/* Disable coalescing for "returns twice" calls: In case of setjmp/longjmp
	 * our control flow graph isn't completely correct: There are no backedges
	 * from longjmp to the setjmp => coalescing would produce wrong results. */
	if (is_ia32_Call(node)) {
		const ia32_call_attr_t    *attrs = get_ia32_call_attr_const(node);
		const ir_type             *type  = attrs->call_tp;
		mtp_additional_properties  mtp
			= get_method_additional_properties(type);
		if (mtp & mtp_property_returns_twice)
			be_forbid_coalescing(env);
	}

	if (!is_ia32_irn(node) || get_ia32_op_type(node) != ia32_AddrModeS)
		return;
	ia32_attr_t const *const attr = get_ia32_attr_const(node);
	if (attr->am_imm.kind != X86_IMM_FRAMEENT) {
		assert(get_ia32_frame_use(node) == IA32_FRAME_USE_NONE);
		return;
	}
	if (attr->am_imm.entity != NULL)
		return;

	ir_type const *type;
	switch (get_ia32_frame_use(node)) {
	case IA32_FRAME_USE_NONE:
		panic("X86_IMM_FRAMEENT but IA32_FRAME_USE_NONE");
	case IA32_FRAME_USE_32BIT:
		type = get_type_for_mode(ia32_mode_gp);
		goto request_entity;
	case IA32_FRAME_USE_64BIT:
		type = get_type_for_mode(mode_Ls);
		goto request_entity;
	case IA32_FRAME_USE_AUTO: {
		ir_mode *mode = get_ia32_ls_mode(node);
		/* stupid hack: in some situations (like reloads folded into ConvI2I
		 * with 8bit mode, an 8bit entity and reload+spill would suffice, but
		 * an 8bit store has special register requirements on ia32 which we may
		 * not be able to fulfill anymore at this point, so extend the spillslot
		 * size to 16bit :-( */
		if (get_mode_size_bits(mode) == 8) {
			type = get_type_for_mode(mode_Hu);
		} else if (mode == x86_mode_E) {
			type = x86_type_E; /* make sure we have the right alignment
			                      entity size (different from mode size) */
		} else {
			type = get_type_for_mode(mode);
		}
		goto request_entity;
	}
	}
	panic("invalid frame use type");
request_entity:
	be_load_needs_frame_entity(env, node, type);
}

static int determine_ebp_input(ir_node *ret)
{
	const arch_register_t *bp = &ia32_registers[REG_EBP];
	foreach_irn_in(ret, i, input) {
		if (arch_get_irn_register(input) == bp)
			return i;
	}
	panic("no ebp input found at %+F", ret);
}

static void introduce_epilogue(ir_node *const ret, bool const omit_fp)
{
	ir_node        *curr_sp;
	ir_node  *const first_sp = get_irn_n(ret, n_ia32_Return_stack);
	ir_node  *const block    = get_nodes_block(ret);
	ir_graph *const irg      = get_irn_irg(ret);
	if (!omit_fp) {
		arch_register_t const *const sp = &ia32_registers[REG_ESP];
		arch_register_t const *const bp = &ia32_registers[REG_EBP];

		ir_node  *restore;
		int const n_ebp    = determine_ebp_input(ret);
		ir_node  *curr_bp  = get_irn_n(ret, n_ebp);
		ir_node  *curr_mem = get_irn_n(ret, n_ia32_Return_mem);
		if (ia32_cg_config.use_leave) {
			restore  = new_bd_ia32_Leave(NULL, block, curr_mem, curr_bp);
			curr_bp  = be_new_Proj_reg(restore, pn_ia32_Leave_frame, bp);
			curr_sp  = be_new_Proj_reg(restore, pn_ia32_Leave_stack, sp);
			curr_mem = be_new_Proj(restore, pn_ia32_Leave_M);
		} else {
			/* Copy ebp to esp. */
			curr_sp = new_bd_ia32_CopyEbpEsp(NULL, block, curr_bp);
			arch_set_irn_register(curr_sp, sp);
			sched_add_before(ret, curr_sp);

			/* Pop ebp. */
			restore  = new_bd_ia32_Pop_ebp(NULL, block, curr_mem, curr_sp);
			curr_bp  = be_new_Proj_reg(restore, pn_ia32_Pop_res,   bp);
			curr_sp  = be_new_Proj_reg(restore, pn_ia32_Pop_stack, sp);
			curr_mem = be_new_Proj(restore, pn_ia32_Pop_M);
		}
		sched_add_before(ret, restore);
		set_irn_n(ret, n_ia32_Return_mem, curr_mem);
		set_irn_n(ret, n_ebp,             curr_bp);
	} else {
		ir_type *const frame_type = get_irg_frame_type(irg);
		unsigned const frame_size = get_type_size(frame_type);
		curr_sp = ia32_new_IncSP(block, first_sp, -(int)frame_size, 0);
		sched_add_before(ret, curr_sp);
	}
	set_irn_n(ret, n_ia32_Return_stack, curr_sp);

	/* Keep verifier happy. */
	if (get_irn_n_edges(first_sp) == 0 && is_Proj(first_sp))
		kill_node(first_sp);
}

static void introduce_prologue(ir_graph *const irg, bool omit_fp)
{
	const arch_register_t *sp         = &ia32_registers[REG_ESP];
	const arch_register_t *bp         = &ia32_registers[REG_EBP];
	ir_node               *start      = get_irg_start(irg);
	ir_node               *block      = get_nodes_block(start);
	ir_type               *frame_type = get_irg_frame_type(irg);
	unsigned               frame_size = get_type_size(frame_type);
	ir_node               *initial_sp = be_get_Start_proj(irg, sp);

	if (!omit_fp) {
		/* push ebp */
		ir_node *const mem        = get_irg_initial_mem(irg);
		ir_node *const noreg      = ia32_new_NoReg_gp(irg);
		ir_node *const initial_bp = be_get_Start_proj(irg, bp);
		ir_node *const push       = new_bd_ia32_Push(NULL, block, noreg, noreg, mem, initial_bp, initial_sp, ia32_mode_gp);
		sched_add_after(start, push);
		ir_node *const curr_mem   = be_new_Proj(push, pn_ia32_Push_M);
		edges_reroute_except(mem, curr_mem, push);
		ir_node *const curr_sp    = be_new_Proj_reg(push, pn_ia32_Push_stack, sp);

		/* move esp to ebp */
		ir_node *const curr_bp = be_new_Copy(block, curr_sp);
		sched_add_after(push, curr_bp);
		arch_copy_irn_out_info(curr_bp, 0, initial_bp);
		edges_reroute_except(initial_bp, curr_bp, push);

		ir_node *incsp = ia32_new_IncSP(block, curr_sp, frame_size, 0);
		edges_reroute_except(initial_sp, incsp, push);
		sched_add_after(curr_bp, incsp);

		/* make sure the initial IncSP is really used by someone */
		be_keep_if_unused(incsp);

		be_stack_layout_t *const layout = be_get_irg_stack_layout(irg);
		layout->initial_bias = -4;
	} else {
		ir_node *const incsp = ia32_new_IncSP(block, initial_sp, frame_size, 0);
		edges_reroute_except(initial_sp, incsp, incsp);
		sched_add_after(start, incsp);
	}
}

/**
 * Put the prologue code at the beginning, epilogue code before each return
 */
static void introduce_prologue_epilogue(ir_graph *const irg, bool omit_fp)
{
	/* introduce epilogue for every return node */
	foreach_irn_in(get_irg_end_block(irg), i, ret) {
		assert(is_ia32_Return(ret));
		introduce_epilogue(ret, omit_fp);
	}

	introduce_prologue(irg, omit_fp);
}

static x87_attr_t *ia32_get_x87_attr(ir_node *const node)
{
	ia32_x87_attr_t *const attr = get_ia32_x87_attr(node);
	return &attr->x87;
}

static void ia32_before_emit(ir_graph *irg)
{
	/*
	 * Last touchups for the graph before emit: x87 simulation to replace the
	 * virtual with real x87 instructions, creating a block schedule and
	 * peephole optimizations.
	 */
	bool          omit_fp = ia32_get_irg_data(irg)->omit_fp;
	be_fec_env_t *fec_env = be_new_frame_entity_coalescer(irg);

	/* create and coalesce frame entities */
	irg_walk_graph(irg, NULL, ia32_collect_frame_entity_nodes, fec_env);
	be_assign_entities(fec_env, ia32_set_frame_entity, omit_fp);
	be_free_frame_entity_coalescer(fec_env);

	irg_block_walk_graph(irg, NULL, ia32_after_ra_walker, NULL);

	introduce_prologue_epilogue(irg, omit_fp);

	/* fix stack entity offsets */
	be_fix_stack_nodes(irg, &ia32_registers[REG_ESP]);
	be_birg_from_irg(irg)->non_ssa_regs = NULL;
	be_abi_fix_stack_bias(irg, ia32_get_sp_bias, ia32_set_frame_offset,
	                      ia32_get_frame_entity);

	/* fix 2-address code constraints */
	ia32_finish_irg(irg);
	be_dump(DUMP_RA, irg, "2addr");

	/* we might have to rewrite x87 virtual registers */
	if (ia32_get_irg_data(irg)->do_x87_sim) {
		x86_prepare_x87_callbacks_ia32();
		const x87_simulator_config_t config = {
			.regclass      = &ia32_reg_classes[CLASS_ia32_fp],
			.new_bd_fdup   = new_bd_ia32_fdup,
			.new_bd_fxch   = new_bd_ia32_fxch,
			.new_bd_fpop   = new_bd_ia32_fpop,
			.new_bd_ffreep = ia32_cg_config.use_ffreep ? new_bd_ia32_ffreep
			                                           : NULL,
			.get_x87_attr  = ia32_get_x87_attr,
		};
		x86_x87_simulate_graph(irg, &config);
	}
	be_dump(DUMP_RA, irg, "x87");

	/* do peephole optimizations */
	ia32_peephole_optimization(irg);

	be_remove_dead_nodes_from_schedule(irg);
}

/**
 * Prepare a graph and perform code selection.
 */
static void ia32_select_instructions(ir_graph *irg)
{
	if (gprof) {
		/* Linux gprof implementation needs base pointer */
		be_options.omit_fp = 0;

		static ir_entity *mcount = NULL;
		if (mcount == NULL) {
			ir_type *tp = new_type_method(0, 0);
			ident   *id = new_id_from_str("mcount");
			mcount = new_global_entity(get_glob_type(), id, tp,
			                           ir_visibility_external,
			                           IR_LINKAGE_DEFAULT);
		}
		instrument_initcall(irg, mcount);
	}
	ia32_adjust_pic(irg);

	be_timer_push(T_CODEGEN);
	ia32_transform_graph(irg);
	be_timer_pop(T_CODEGEN);

	be_dump(DUMP_BE, irg, "code-selection");

	/* do local optimizations (mainly CSE) */
	optimize_graph_df(irg);

	/* optimize address mode */
	ia32_optimize_graph(irg);

	be_dump(DUMP_BE, irg, "opt");

	ir_type *frame_type = get_irg_frame_type(irg);
	if (get_type_state(frame_type) == layout_undefined)
		default_layout_compound_type(frame_type);

	/* do code placement, to optimize the position of constants */
	place_code(irg);

	/* backend code expects that outedges are always enabled */
	assure_edges(irg);

	be_dump(DUMP_BE, irg, "place");
}

/**
 * Check if Mux(sel, mux_true, mux_false) would represent a Max or Min operation
 */
static bool mux_is_float_min_max(ir_node *sel, ir_node *mux_true,
                                 ir_node *mux_false)
{
	if (!is_Cmp(sel))
		return false;

	ir_node *cmp_l = get_Cmp_left(sel);
	ir_node *cmp_r = get_Cmp_right(sel);
	if (!mode_is_float(get_irn_mode(cmp_l)))
		return false;

	/* check for min/max. They're defined as (C-Semantik):
	 *  min(a, b) = a < b ? a : b
	 *  or min(a, b) = a <= b ? a : b
	 *  max(a, b) = a > b ? a : b
	 *  or max(a, b) = a >= b ? a : b
	 * (Note we only handle float min/max here) */
	ir_relation relation = get_Cmp_relation(sel);
	switch (relation) {
	case ir_relation_greater_equal:
	case ir_relation_greater:
		/* this is a max */
		if (cmp_l == mux_true && cmp_r == mux_false)
			return true;
		break;
	case ir_relation_less_equal:
	case ir_relation_less:
		/* this is a min */
		if (cmp_l == mux_true && cmp_r == mux_false)
			return true;
		break;
	case ir_relation_unordered_greater_equal:
	case ir_relation_unordered_greater:
		/* this is a min */
		if (cmp_l == mux_false && cmp_r == mux_true)
			return true;
		break;
	case ir_relation_unordered_less_equal:
	case ir_relation_unordered_less:
		/* this is a max */
		if (cmp_l == mux_false && cmp_r == mux_true)
			return true;
		break;

	default:
		break;
	}

	return false;
}

static bool mux_is_set(ir_node *sel, ir_node *mux_true, ir_node *mux_false)
{
	(void)sel;
	ir_mode *mode = get_irn_mode(mux_true);
	if (!mode_is_int(mode) && !mode_is_reference(mode)
	    && mode != mode_b)
		return false;

	/* we can create a set plus up two 3 instructions for any combination
	 * of constants */
	if (is_Const(mux_true) && is_Const(mux_false))
		return true;

	return false;
}

static bool mux_is_float_const_const(ir_node *sel, ir_node *mux_true,
                                     ir_node *mux_false)
{
	(void)sel;
	if (!mode_is_float(get_irn_mode(mux_true)))
		return false;

	return is_Const(mux_true) && is_Const(mux_false);
}

static bool mux_is_doz(ir_node *sel, ir_node *mux_true, ir_node *mux_false)
{
	if (!is_Cmp(sel))
		return false;

	ir_mode *mode = get_irn_mode(mux_true);
	if (mode_is_signed(mode) || mode_is_float(mode))
		return false;

	ir_relation relation  = get_Cmp_relation(sel);
	ir_node    *cmp_left  = get_Cmp_left(sel);
	ir_node    *cmp_right = get_Cmp_right(sel);

	/* "move" zero constant to false input */
	if (is_Const(mux_true) && is_Const_null(mux_true)) {
		ir_node *tmp = mux_false;
		mux_false = mux_true;
		mux_true  = tmp;
		relation  = get_negated_relation(relation);
	}
	if (!is_Const(mux_false) || !is_Const_null(mux_false))
		return false;
	if (!is_Sub(mux_true))
		return false;
	ir_node *sub_left  = get_Sub_left(mux_true);
	ir_node *sub_right = get_Sub_right(mux_true);

	/* Mux(a >=u b, 0, a-b) */
	if ((relation & ir_relation_greater)
	    && sub_left == cmp_left && sub_right == cmp_right)
		return true;
	/* Mux(a <=u b, 0, b-a) */
	if ((relation & ir_relation_less)
	    && sub_left == cmp_right && sub_right == cmp_left)
		return true;

	return false;
}

static int ia32_is_mux_allowed(ir_node *sel, ir_node *mux_false,
                               ir_node *mux_true)
{
	/* middleend can handle some things */
	if (ir_is_optimizable_mux(sel, mux_false, mux_true))
		return true;
	/* we can handle Set for all modes and compares */
	if (mux_is_set(sel, mux_true, mux_false))
		return true;
	/* SSE has own min/max operations */
	if (ia32_cg_config.use_sse2
			&& mux_is_float_min_max(sel, mux_true, mux_false))
		return true;
	/* we can handle Mux(?, Const[f], Const[f]) */
	if (mux_is_float_const_const(sel, mux_true, mux_false))
		return true;

	/* no support for 64bit inputs to cmov */
	ir_mode *mode = get_irn_mode(mux_true);
	if (get_mode_size_bits(mode) > 32)
		return false;
	/* we can handle Abs for all modes and compares (except 64bit) */
	if (ir_mux_is_abs(sel, mux_false, mux_true) != 0)
		return true;
	/* we can't handle MuxF yet */
	if (mode_is_float(mode))
		return false;

	if (mux_is_doz(sel, mux_true, mux_false))
		return true;

	/* Check Cmp before the node */
	if (is_Cmp(sel)) {
		ir_mode *cmp_mode = get_irn_mode(get_Cmp_left(sel));

		/* we can't handle 64bit compares */
		if (get_mode_size_bits(cmp_mode) > 32)
			return false;

		/* we can't handle float compares */
		if (mode_is_float(cmp_mode))
			return false;
	}

	/* did we disable cmov generation? */
	if (!ia32_cg_config.use_cmov)
		return false;

	/* we can use a cmov */
	return true;
}

static const ir_settings_arch_dep_t ia32_arch_dep = {
	.also_use_subs        = true,
	.maximum_shifts       = 4,
	.highest_shift_amount = 63,
	.evaluate             = ia32_evaluate_insn,
	.allow_mulhs          = true,
	.allow_mulhu          = true,
	.max_bits_for_mulh    = 32,
};
static backend_params ia32_backend_params = {
	.byte_order_big_endian          = false,
	.pic_supported                  = true,
	.unaligned_memaccess_supported  = true,
	.thread_local_storage_supported = true,
	.modulo_shift                   = 32,
	.dep_param                      = &ia32_arch_dep,
	.allow_ifconv                   = ia32_is_mux_allowed,
	.machine_size                   = 32,
	.mode_float_arithmetic          = NULL,  /* will be set later */
	.type_long_long                 = NULL,  /* will be set later */
	.type_unsigned_long_long        = NULL,  /* will be set later */
	.type_long_double               = NULL,  /* will be set later */
	.stack_param_align              = 4,
	.float_int_overflow             = ir_overflow_indefinite,
	.vararg                         = {
		.va_list_type = NULL, /* will be set later */
		.lower_va_arg = be_default_lower_va_arg,
	},
};

/**
 * Initializes the backend ISA.
 */
static void ia32_init(void)
{
	ia32_setup_cg_config();

	x86_set_be_asm_constraint_support(&ia32_asm_constraints);

	ia32_mode_fpcw = new_non_arithmetic_mode("fpcw", 16);
	ia32_mode_flags = new_non_arithmetic_mode("flags", 32);

	ia32_mode_8h = new_non_arithmetic_mode("8h", 8);
	ia32_mode_gp = new_int_mode("gp", irma_twos_complement, 32, 0, 32);
	ia32_mode_float64 = new_float_mode("fp64", irma_ieee754, 11, 52,
	                                   ir_overflow_indefinite);
	ia32_mode_float32 = new_float_mode("fp32", irma_ieee754, 8, 23,
	                                   ir_overflow_indefinite);

	ir_mode *mode_long_long
		= new_int_mode("long long", irma_twos_complement, 64, 1, 64);
	ir_type *type_long_long = new_type_primitive(mode_long_long);
	ir_mode *mode_unsigned_long_long
		= new_int_mode("unsigned long long", irma_twos_complement, 64, 0, 64);
	ir_type *type_unsigned_long_long
		= new_type_primitive(mode_unsigned_long_long);

	ia32_backend_params.type_long_long          = type_long_long;
	ia32_backend_params.type_unsigned_long_long = type_unsigned_long_long;

	// va_list is a void pointer
	ir_type *type_va_list = new_type_pointer(new_type_primitive(mode_ANY));
	ia32_backend_params.vararg.va_list_type = type_va_list;

	if (ia32_cg_config.use_sse2 || ia32_cg_config.use_softfloat) {
		ia32_backend_params.mode_float_arithmetic = NULL;
		ia32_backend_params.type_long_double = NULL;
	} else {
		x86_init_x87_type();
		ia32_backend_params.mode_float_arithmetic = x86_mode_E;
		ia32_backend_params.type_long_double      = x86_type_E;
	}

	ia32_register_init();
	obstack_init(&opcodes_obst);
	ia32_create_opcodes();
	ia32_cconv_init();
}

static void ia32_finish(void)
{
	if (between_type != NULL) {
		free_type(between_type);
		between_type = NULL;
	}
	ia32_free_opcodes();
	obstack_free(&opcodes_obst, NULL);
}

static void ia32_mark_remat(ir_node *node)
{
	if (is_ia32_irn(node))
		set_ia32_is_remat(node);
}

static const regalloc_if_t ia32_regalloc_if = {
	.spill_cost             = 7,
	.reload_cost            = 5,
	.mark_remat             = ia32_mark_remat,
	.new_spill              = ia32_new_spill,
	.new_reload             = ia32_new_reload,
	.perform_memory_operand = ia32_perform_memory_operand,
};

static bool lower_for_emit(ir_graph *const irg, unsigned *const sp_is_non_ssa)
{
	if (!be_step_first(irg))
		return false;

	struct obstack *obst = be_get_be_obst(irg);
	be_birg_from_irg(irg)->isa_link = OALLOCZ(obst, ia32_irg_data_t);

	be_birg_from_irg(irg)->non_ssa_regs = sp_is_non_ssa;
	ia32_select_instructions(irg);

	be_step_schedule(irg);

	be_timer_push(T_RA_PREPARATION);
	ia32_setup_fpu_mode(irg);
	be_sched_fix_flags(irg, &ia32_reg_classes[CLASS_ia32_flags],
					   &flags_remat, NULL, &ia32_try_replace_flags);
	simplify_remat_nodes(irg);
	be_timer_pop(T_RA_PREPARATION);

	be_step_regalloc(irg, &ia32_regalloc_if);

	ia32_before_emit(irg);
	return true;
}

static void ia32_generate_code(FILE *output, const char *cup_name)
{
	ia32_tv_ent = pmap_create();

	be_begin(output, cup_name);
	unsigned *const sp_is_non_ssa = rbitset_alloca(N_IA32_REGISTERS);
	rbitset_set(sp_is_non_ssa, REG_ESP);

	foreach_irp_irg(i, irg) {
		if (!lower_for_emit(irg, sp_is_non_ssa))
			continue;

		be_timer_push(T_EMIT);
		ia32_emit_function(irg);
		be_timer_pop(T_EMIT);

		be_step_last(irg);
	}

	ia32_emit_thunks();

	be_finish();
	pmap_destroy(ia32_tv_ent);
}

static ir_jit_function_t *ia32_jit_compile(ir_jit_segment_t *const segment,
                                           ir_graph *const irg)
{
	unsigned *const sp_is_non_ssa = rbitset_alloca(N_IA32_REGISTERS);
	rbitset_set(sp_is_non_ssa, REG_ESP);

	if (!lower_for_emit(irg, sp_is_non_ssa))
		return NULL;

	be_timer_push(T_EMIT);
	ir_jit_function_t *const res = ia32_emit_jit(segment, irg);
	be_timer_pop(T_EMIT);

	be_step_last(irg);
	return res;
}

static int ia32_is_valid_clobber(const char *clobber)
{
	return x86_parse_clobber(ia32_additional_clobber_names, clobber) != NULL;
}

static bool is_float(ir_type const *const type)
{
	return is_atomic_type(type) && mode_is_float(get_type_mode(type));
}

static ir_mode *iu4_iu4_modes[2];
static aggregate_spec_t const iu4_iu4_spec = {
	.n_values = 2,
	.modes    = iu4_iu4_modes,
};
static aggregate_spec_t const iu4_spec = {
	.n_values = 1,
	.modes    = iu4_iu4_modes,
};
static ir_mode *iu2_modes[1];
static aggregate_spec_t const iu2_spec = {
	.n_values = 1,
	.modes    = iu2_modes,
};
static ir_mode *iu1_modes[1];
static aggregate_spec_t const iu1_spec = {
	.n_values = 1,
	.modes    = iu1_modes,
};
static ir_mode *float_modes[1];
static aggregate_spec_t const float_spec = {
	.n_values = 1,
	.modes    = float_modes,
};
static ir_mode *double_modes[1];
static aggregate_spec_t const double_spec = {
	.n_values = 1,
	.modes    = double_modes,
};

static void init_aggregate_specs(void)
{
	iu4_iu4_modes[0] = mode_Iu;
	iu4_iu4_modes[1] = mode_Iu;
	iu2_modes[0]     = mode_Hu;
	iu1_modes[0]     = mode_Bu;
	float_modes[0]   = mode_F;
	double_modes[0]  = mode_D;
}

static aggregate_spec_t const *decide_compound_ret(ir_type const *type)
{
	unsigned size = get_type_size(type);
	if (is_Array_type(type)) {
		/* This is used for returning complex float numbers */
		if (size == 8 && has_array_size(type) && get_array_size_int(type) == 2
		 && is_float(get_array_element_type(type))) {
			return &iu4_iu4_spec;
		}
		return &no_values_aggregate_spec;
	}

	assert(is_compound_type(type));

	/* return_small_struct_in_regs is used on OS X */
	if (return_small_struct_in_regs && size <= 8) {
		if (get_compound_n_members(type) == 1) {
			ir_entity *const member      = get_compound_member(type, 0);
			ir_type   *const member_type = get_entity_type(member);
			if (is_float(member_type)) {
				unsigned member_size = get_type_size(member_type);
				if (member_size == 4)
					return &float_spec;
				if (member_size == 8)
					return &double_spec;
			}
		}

		switch (size) {
		case 1: return &iu1_spec;
		case 2: return &iu2_spec;
		case 4: return &iu4_spec;
		case 8: return &iu4_iu4_spec;
		}
	}

	return &no_values_aggregate_spec;
}

static void ia32_lower_for_target(void)
{
	ir_mode *mode_gp = ia32_reg_classes[CLASS_ia32_gp].mode;

	/* lower compound param handling
	 * Note: we lower compound arguments ourself, since on ia32 we don't
	 * have hidden parameters but know where to find the structs on the stack.
	 * (This also forces us to always allocate space for the compound arguments
	 *  on the callframe and we can't just use an arbitrary position on the
	 *  stackframe) */
	init_aggregate_specs();
	compound_call_lowering_flags lower_call_flags
		= LF_RETURN_HIDDEN | LF_DONT_LOWER_ARGUMENTS;
	lower_calls_with_compounds(lower_call_flags, decide_compound_ret);
	be_after_irp_transform("lower-calls");

	/* replace floating point operations by function calls */
	if (ia32_cg_config.use_softfloat) {
		lower_floating_point();
		be_after_irp_transform("lower-fp");
	}

	ir_builtin_kind supported[32];
	size_t          s = 0;
	supported[s++] = ir_bk_trap;
	supported[s++] = ir_bk_debugbreak;
	supported[s++] = ir_bk_return_address;
	supported[s++] = ir_bk_frame_address;
	supported[s++] = ir_bk_prefetch;
	supported[s++] = ir_bk_ffs;
	supported[s++] = ir_bk_clz;
	supported[s++] = ir_bk_ctz;
	supported[s++] = ir_bk_parity;
	supported[s++] = ir_bk_bswap;
	supported[s++] = ir_bk_outport;
	supported[s++] = ir_bk_inport;
	supported[s++] = ir_bk_saturating_increment;
	supported[s++] = ir_bk_va_start;
	if (ia32_cg_config.use_popcnt)
		supported[s++] = ir_bk_popcount;
	if (ia32_cg_config.use_cmpxchg)
		supported[s++] = ir_bk_compare_swap;
	assert(s < ARRAY_SIZE(supported));
	lower_builtins(s, supported);
	be_after_irp_transform("lower-builtins");

	foreach_irp_irg(i, irg) {
		/* break up switches with wide ranges */
		lower_switch(irg, 4, 256, mode_gp);
		be_after_transform(irg, "lower-switch");
	}

	ia32_lower64();
	be_after_irp_transform("lower-64");

	foreach_irp_irg(i, irg) {
		/* lower for mode_b stuff */
		ir_lower_mode_b(irg, ia32_mode_gp);
		be_after_transform(irg, "lower-modeb");
		lower_alloc(irg, ia32_cg_config.po2_stack_alignment);
		be_after_transform(irg, "lower-alloc");
	}

	foreach_irp_irg(i, irg) {
		/* Turn all small CopyBs into loads/stores, keep medium-sized CopyBs,
		 * so we can generate rep movs later, and turn all big CopyBs into
		 * memcpy calls. */
		lower_CopyB(irg, 64, 8193, true);
		be_after_transform(irg, "lower-copyb");
	}
}

/**
 * Returns the libFirm configuration parameter for this backend.
 */
static const backend_params *ia32_get_libfirm_params(void)
{
	return &ia32_backend_params;
}

static const lc_opt_table_entry_t ia32_options[] = {
	LC_OPT_ENT_BOOL("gprof", "Create gprof profiling code", &gprof),
	LC_OPT_ENT_BOOL("struct_in_reg",
					"Return small structs in integer registers",
					&return_small_struct_in_regs),
	LC_OPT_LAST
};

static arch_isa_if_t const ia32_isa_if = {
	.n_registers           = N_IA32_REGISTERS,
	.registers             = ia32_registers,
	.n_register_classes    = N_IA32_CLASSES,
	.register_classes      = ia32_reg_classes,
	.init                  = ia32_init,
	.finish                = ia32_finish,
	.get_params            = ia32_get_libfirm_params,
	.generate_code         = ia32_generate_code,
	.jit_compile           = ia32_jit_compile,
	.emit_function         = ia32_emit_jit_function,
	.lower_for_target      = ia32_lower_for_target,
	.is_valid_clobber      = ia32_is_valid_clobber,
	.get_op_estimated_cost = ia32_get_op_estimated_cost,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_ia32)
void be_init_arch_ia32(void)
{
	lc_opt_entry_t *be_grp   = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ia32_grp = lc_opt_get_grp(be_grp, "ia32");

	lc_opt_add_table(ia32_grp, ia32_options);
	be_register_isa_if("ia32", &ia32_isa_if);

	ia32_init_emitter();
	ia32_init_finish();
	ia32_init_optimize();
	ia32_init_transform();
	x86_init_x87();
	ia32_init_architecture();
}
