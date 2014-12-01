/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    The main amd64 backend driver file.
 */
#include "bearch.h"
#include "beflags.h"
#include "begnuas.h"
#include "beirg.h"
#include "belistsched.h"
#include "belower.h"
#include "bemodule.h"
#include "benode.h"
#include "besched.h"
#include "bespillslots.h"
#include "bespillutil.h"
#include "bestack.h"
#include "be_t.h"
#include "debug.h"
#include "panic.h"
#include "irarch_t.h"
#include "ircons_t.h"
#include "irdump.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "iropt_t.h"
#include "irprog_t.h"
#include "lower_alloc.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "lower_mode_b.h"
#include "util.h"

#include "bearch_amd64_t.h"

#include "amd64_finish.h"
#include "amd64_new_nodes.h"
#include "gen_amd64_regalloc_if.h"
#include "amd64_transform.h"
#include "amd64_emitter.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

ir_mode *amd64_mode_E;
ir_type *amd64_type_E;
ir_mode *amd64_mode_xmm;

static ir_entity *amd64_get_frame_entity(const ir_node *node)
{
	if (!is_amd64_irn(node))
		return NULL;
	if (!amd64_has_addr_attr(node))
		return NULL;
	const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
	ir_entity *entity = attr->addr.immediate.entity;
	if (entity == NULL)
		return NULL;
	ir_type *owner = get_entity_owner(entity);
	if (is_frame_type(owner))
		return entity;
	ir_graph *irg = get_irn_irg(node);
	be_stack_layout_t *layout = be_get_irg_stack_layout(irg);
	if (owner == layout->arg_type)
		return entity;
	return NULL;
}

static int get_insn_mode_bytes(amd64_insn_mode_t insn_mode)
{
	switch (insn_mode) {
	case INSN_MODE_8:   return 1;
	case INSN_MODE_16:  return 2;
	case INSN_MODE_32:  return 4;
	case INSN_MODE_64:  return 8;
	case INSN_MODE_128: return 16;
	}
	panic("bad insn mode");
}

/**
 * This function is called by the generic backend to correct offsets for
 * nodes accessing the stack.
 */
static void amd64_set_frame_offset(ir_node *node, int offset)
{
	if (!is_amd64_irn(node))
		return;
	amd64_addr_attr_t *attr = get_amd64_addr_attr(node);
	attr->addr.immediate.offset += offset;
	if (is_amd64_PopAM(node)) {
		ir_graph          *irg    = get_irn_irg(node);
		be_stack_layout_t *layout = be_get_irg_stack_layout(irg);
		if (layout->sp_relative)
			attr->addr.immediate.offset -= get_insn_mode_bytes(attr->insn_mode);
	}
}

static int amd64_get_sp_bias(const ir_node *node)
{
	if (is_amd64_PushAM(node)) {
		const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
		return get_insn_mode_bytes(attr->insn_mode);
	} else if (is_amd64_PushRbp(node)) {
		/* 64-bit register size */
		return AMD64_REGISTER_SIZE;
	} else if (is_amd64_PopAM(node)) {
		const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
		return -get_insn_mode_bytes(attr->insn_mode);
	} else if (is_amd64_Leave(node)) {
		return SP_BIAS_RESET;
	}

	return 0;
}

/* fill register allocator interface */

static const arch_irn_ops_t amd64_irn_ops = {
	.get_op_estimated_cost  = NULL,
	.perform_memory_operand = NULL,
};

static void amd64_before_ra(ir_graph *irg)
{
	be_sched_fix_flags(irg, &amd64_reg_classes[CLASS_amd64_flags], NULL, NULL, NULL);
}

static const arch_register_req_t amd64_requirement_gp = {
	.cls             = &amd64_reg_classes[CLASS_amd64_gp],
	.limited         = NULL,
	.type            = arch_register_req_type_normal,
	.other_same      = 0,
	.other_different = 0,
	.width           = 1,
};

static const unsigned amd64_limited_gp_rsp [] = { (1 << REG_GP_RSP) };
static const arch_register_req_t amd64_single_reg_req_gp_rsp = {
	.type            = arch_register_req_type_limited,
	.cls             = &amd64_reg_classes[CLASS_amd64_gp],
	.limited         = amd64_limited_gp_rsp,
	.other_same      = 0,
	.other_different = 0,
	.width           = 1,
};

static const arch_register_req_t *am_pushpop_base_reqs[] = {
	&amd64_single_reg_req_gp_rsp,
	&amd64_requirement_gp,
	&arch_no_requirement,
};

static ir_node *create_push(ir_node *node, ir_node *schedpoint, ir_node *sp,
                            ir_node *mem, ir_entity *ent)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *frame = get_irg_frame(irg);

	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.base_input       = 1;
	addr.index_input      = NO_INPUT;
	addr.immediate.entity = ent;
	ir_node *in[] = { sp, frame, mem };
	ir_node *push = new_bd_amd64_PushAM(dbgi, block, ARRAY_SIZE(in), in,
	                                    INSN_MODE_64, addr);
	arch_set_irn_register_reqs_in(push, am_pushpop_base_reqs);
	sched_add_before(schedpoint, push);
	return push;
}

static ir_node *create_pop(ir_node *node, ir_node *schedpoint, ir_node *sp, ir_entity *ent)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *frame = get_irg_frame(irg);

	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.base_input  = 1;
	addr.index_input = NO_INPUT;
	addr.immediate.entity = ent;
	ir_node *in[] = { sp, frame, get_irg_no_mem(irg) };

	ir_node *pop = new_bd_amd64_PopAM(dbgi, block, ARRAY_SIZE(in), in,
	                                  INSN_MODE_64, addr);
	arch_set_irn_register_reqs_in(pop, am_pushpop_base_reqs);
	sched_add_before(schedpoint, pop);

	return pop;
}

static ir_node* create_spproj(ir_node *pred, int pos)
{
	const arch_register_t *spreg = &amd64_registers[REG_RSP];
	ir_mode               *spmode = spreg->cls->mode;
	ir_node               *sp     = new_r_Proj(pred, spmode, pos);
	arch_set_irn_register(sp, spreg);
	return sp;
}

/**
 * Transform MemPerm, currently we do this the ugly way and produce
 * push/pop into/from memory cascades. This is possible without using
 * any registers.
 */
static void transform_MemPerm(ir_node *node)
{
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *sp    = be_get_initial_reg_value(irg, &amd64_registers[REG_RSP]);
	int       arity = be_get_MemPerm_entity_arity(node);
	ir_node **pops  = ALLOCAN(ir_node*, arity);
	ir_node  *in[1];
	ir_node  *keep;
	int       i;

	/* create Pushs */
	for (i = 0; i < arity; ++i) {
		ir_entity *inent = be_get_MemPerm_in_entity(node, i);
		ir_entity *outent = be_get_MemPerm_out_entity(node, i);
		ir_type *enttype = get_entity_type(inent);
		unsigned entsize = get_type_size_bytes(enttype);
		unsigned entsize2 = get_type_size_bytes(get_entity_type(outent));
		ir_node *mem = get_irn_n(node, i);
		ir_node *push;

		/* work around cases where entities have different sizes */
		if (entsize2 < entsize)
			entsize = entsize2;
		/* spillslot should be 64bit size */
		assert(entsize == 8);

		push = create_push(node, node, sp, mem, inent);
		sp = create_spproj(push, pn_amd64_PushAM_stack);
		set_irn_n(node, i, new_r_Bad(irg, mode_X));
	}

	/* create pops */
	for (i = arity; i-- > 0; ) {
		ir_entity *inent = be_get_MemPerm_in_entity(node, i);
		ir_entity *outent = be_get_MemPerm_out_entity(node, i);
		ir_type *enttype = get_entity_type(outent);
		unsigned entsize = get_type_size_bytes(enttype);
		unsigned entsize2 = get_type_size_bytes(get_entity_type(inent));

		/* work around cases where entities have different sizes */
		if (entsize2 < entsize)
			entsize = entsize2;
		assert(entsize == 8);

		ir_node *pop = create_pop(node, node, sp, outent);
		sp = create_spproj(pop, pn_amd64_PopAM_stack);
		pops[i] = pop;
	}

	in[0] = sp;
	keep  = be_new_Keep(block, 1, in);
	sched_replace(node, keep);

	/* exchange memprojs */
	foreach_out_edge_safe(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		int p = get_Proj_num(proj);

		assert(p < arity);

		set_Proj_pred(proj, pops[p]);
		set_Proj_num(proj, pn_amd64_PopAM_M);
	}

	/* remove memperm */
	kill_node(node);
}

static void amd64_after_ra_walker(ir_node *block, void *data)
{
	(void) data;

	sched_foreach_reverse_safe(block, node) {
		if (be_is_MemPerm(node)) {
			transform_MemPerm(node);
		}
	}
}

/**
 * rewrite unsigned long -> float/double conversion
 * x86_64 only has a signed conversion
 */
static void rewrite_unsigned_float_Conv(ir_node *node)
{
	ir_graph *irg        = get_irn_irg(node);
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node *lower_block = get_nodes_block(node);
	ir_mode *dest_mode   = get_irn_mode(node);

	part_block(node);

	ir_node *block       = get_nodes_block(node);
	ir_node *unsigned_x  = get_Conv_op(node);
	ir_mode *mode_u      = get_irn_mode(unsigned_x);
	ir_mode *mode_s      = find_signed_mode(mode_u);
	ir_node *signed_x    = new_rd_Conv(dbgi, block, unsigned_x, mode_s);
	ir_node *zero        = new_r_Const_null(irg, mode_s);
	collect_new_start_block_node(zero);
	ir_node *cmp         = new_rd_Cmp(dbgi, block, signed_x, zero,
	                                 ir_relation_less);
	ir_node *cond        = new_rd_Cond(dbgi, block, cmp);
	ir_node *proj_true   = new_r_Proj(cond, mode_X, pn_Cond_true);
	ir_node *proj_false  = new_r_Proj(cond, mode_X, pn_Cond_false);
	ir_node *in_true[1]  = { proj_true };
	ir_node *in_false[1] = { proj_false };

	/* true block: Do some arithmetic to use the signed conversion */
	ir_node *true_block  = new_r_Block(irg, ARRAY_SIZE(in_true), in_true);
	ir_node *true_jmp    = new_r_Jmp(true_block);
	ir_node *one         = new_r_Const_one(irg, mode_u);
	collect_new_start_block_node(one);
	ir_node *and         = new_r_And(true_block, unsigned_x, one, mode_u);
	ir_node *shr         = new_r_Shr(true_block, unsigned_x, one, mode_u);
	ir_node *or          = new_r_Or(true_block, and, shr, mode_u);
	ir_node *or_signed   = new_rd_Conv(dbgi, true_block, or, mode_s);
	ir_node *half        = new_rd_Conv(dbgi, true_block, or_signed, dest_mode);
	ir_node *true_res    = new_r_Add(true_block, half, half, dest_mode);

	/* false block: Simply convert to floating point */
	ir_node *false_block = new_r_Block(irg, ARRAY_SIZE(in_false), in_false);
	ir_node *false_jmp   = new_r_Jmp(false_block);
	ir_node *false_res   = new_rd_Conv(dbgi, false_block, signed_x, dest_mode);

	/* lower block */
	ir_node *lower_in[2] = { true_jmp, false_jmp };
	ir_node *phi_in[2]   = { true_res, false_res };

	set_irn_in(lower_block, ARRAY_SIZE(lower_in), lower_in);
	ir_node *phi = new_r_Phi(lower_block, ARRAY_SIZE(phi_in), phi_in,
	                         dest_mode);
	collect_new_phi_node(phi);
	exchange(node, phi);
}

/* Creates a 64-bit constant with only the sign bit set,
 * i.e. returns 0x8000000000000000
 */
static ir_node *create_sign_bit_const(ir_graph *irg)
{
	ir_tarval *sign_tv = create_sign_tv(mode_Ls);
	return new_r_Const(irg, sign_tv);
}

/* rewrite float/double -> unsigned long conversion
 * x86_64 only has a signed conversion so we rewrite to the following:
 *
 * if (x >= 9223372036854775808.) {
 *   converted ^= (int)(x-9223372036854775808.) ^ 0x8000000000000000;
 * } else {
 *   converted = (int)x;
 * }
 * return (unsigned)converted;

 */
static void rewrite_float_unsigned_Conv(ir_node *node)
{
	ir_graph *irg        = get_irn_irg(node);
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node *lower_block = get_nodes_block(node);
	ir_mode *dest_mode   = get_irn_mode(node);

	part_block(node);

	ir_node   *block    = get_nodes_block(node);
	ir_node   *fp_x     = get_Conv_op(node);
	ir_mode   *src_mode = get_irn_mode(fp_x);
	double     d_const  = 9223372036854775808.;
	ir_tarval *tv       = new_tarval_from_double(d_const, src_mode);
	ir_node   *fp_const = new_r_Const(irg, tv);
	collect_new_start_block_node(fp_const);

	/* Test if the sign bit is needed */
	ir_node *cmp         = new_rd_Cmp(dbgi, block, fp_x, fp_const,
	                                 ir_relation_greater_equal);
	ir_node *cond        = new_rd_Cond(dbgi, block, cmp);
	ir_node *proj_true   = new_r_Proj(cond, mode_X, pn_Cond_true);
	ir_node *proj_false  = new_r_Proj(cond, mode_X, pn_Cond_false);
	ir_node *in_true[1]  = { proj_true };
	ir_node *in_false[1] = { proj_false };

	/* true block: Do some arithmetic to use the signed conversion */
	ir_node *true_block  = new_r_Block(irg, ARRAY_SIZE(in_true), in_true);
	ir_node *true_jmp    = new_r_Jmp(true_block);
	ir_node *sub         = new_r_Sub(true_block, fp_const, fp_x, src_mode);
	ir_node *sub_conv    = new_rd_Conv(dbgi, true_block, sub, mode_Ls);
	ir_node *sign_bit    = create_sign_bit_const(irg);
	collect_new_start_block_node(sign_bit);
	ir_node *xor         = new_r_Eor(true_block, sub_conv, sign_bit, mode_Ls);
	ir_node *true_res    = new_rd_Conv(dbgi, true_block, xor, dest_mode);

	/* false block: Simply convert */
	ir_node *false_block  = new_r_Block(irg, ARRAY_SIZE(in_false), in_false);
	ir_node *false_jmp    = new_r_Jmp(false_block);
	ir_node *false_signed = new_rd_Conv(dbgi, false_block, fp_x, mode_Ls);
	ir_node *false_res    = new_rd_Conv(dbgi, false_block, false_signed,
	                                    dest_mode);

	/* lower block */
	ir_node *lower_in[2] = { true_jmp, false_jmp };
	ir_node *phi_in[2]   = { true_res, false_res };

	set_irn_in(lower_block, ARRAY_SIZE(lower_in), lower_in);
	ir_node *phi = new_r_Phi(lower_block, ARRAY_SIZE(phi_in), phi_in,
	                         dest_mode);
	collect_new_phi_node(phi);
	exchange(node, phi);
}

static bool amd64_rewrite_Conv(ir_node *node)
{
	ir_mode *to_mode    = get_irn_mode(node);
	ir_node *op         = get_Conv_op(node);
	ir_mode *from_mode  = get_irn_mode(op);
	bool     to_float   = mode_is_float(to_mode);
	bool     from_float = mode_is_float(from_mode);

	if (to_float && !from_float && !mode_is_signed(from_mode)
	    && get_mode_size_bits(from_mode) == 64) {
		rewrite_unsigned_float_Conv(node);
		return true;
	} else if (from_float && !to_float && !mode_is_signed(to_mode)
	           && get_mode_size_bits(to_mode) == 64) {
		rewrite_float_unsigned_Conv(node);
		return true;
	}

	return false;
}

static void amd64_intrinsics_walker(ir_node *node, void *data)
{
	bool *changed = (bool*)data;
	if (is_Conv(node)) {
		if (amd64_rewrite_Conv(node))
			*changed = true;
	}
}

static void amd64_handle_intrinsics(ir_graph *irg)
{
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);
	collect_phiprojs_and_start_block_nodes(irg);
	bool changed = false;
	irg_walk_graph(irg, amd64_intrinsics_walker, NULL, &changed);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	if (changed) {
		confirm_irg_properties(irg,
		        IR_GRAPH_PROPERTY_NO_BADS
		        | IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
		        | IR_GRAPH_PROPERTY_MANY_RETURNS
		        | IR_GRAPH_PROPERTY_ONE_RETURN);
	}
}

static void amd64_set_frame_entity(ir_node *node, ir_entity *entity,
                                   const ir_type *type)
{
	(void)type;
	assert(is_amd64_Store(node) || is_amd64_Mov(node)
	    || is_amd64_Movs(node) || is_amd64_xMovs(node)
	    || is_amd64_xStores(node));
	amd64_addr_attr_t *attr = get_amd64_addr_attr(node);
	attr->addr.immediate.entity = entity;
}

static bool is_frame_load(const ir_node *node)
{
	return is_amd64_Mov(node) || is_amd64_Movs(node) || is_amd64_xMovs(node);
}

/**
 * Collects nodes that need frame entities assigned.
 */
static void amd64_collect_frame_entity_nodes(ir_node *node, void *data)
{
	/* we are only interested to report Load nodes */
	if (!is_frame_load(node))
		return;

	const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
	if (attr->needs_frame_ent) {
		be_fec_env_t  *env   = (be_fec_env_t*)data;
		const ir_mode *mode  = mode_Lu; /* TODO: improve */
		const ir_type *type  = get_type_for_mode(mode);
		be_load_needs_frame_entity(env, node, type);
	}
}

static int determine_rbp_input(ir_node *ret)
{
	const arch_register_t *bp = &amd64_registers[REG_RSP];
	foreach_irn_in(ret, i, input) {
		if (arch_get_irn_register(input) == bp)
			return i;
	}
    panic("no rbp input found at %+F", ret);
}

static void introduce_epilogue(ir_node *ret)
{
	const arch_register_t *sp         = &amd64_registers[REG_RSP];
	const arch_register_t *bp         = &amd64_registers[REG_RBP];
	ir_graph              *irg        = get_irn_irg(ret);
	ir_node               *start      = get_irg_start(irg);
	ir_node               *block      = get_nodes_block(start);
	ir_type               *frame_type = get_irg_frame_type(irg);
	unsigned               frame_size = get_type_size_bytes(frame_type);
	be_stack_layout_t     *layout     = be_get_irg_stack_layout(irg);
	ir_node               *first_sp   = get_irn_n(ret, n_amd64_Return_stack);
	ir_node               *curr_sp    = first_sp;
	ir_mode               *mode_gp    = mode_Lu;

	if (!layout->sp_relative) {
		int n_rbp = determine_rbp_input(ret);
		ir_node *curr_bp = get_irn_n(ret, n_rbp);

		ir_node *leave = new_bd_amd64_Leave(NULL, block, curr_bp);
		curr_bp        = new_r_Proj(leave, mode_gp, pn_amd64_Leave_frame);
		curr_sp        = new_r_Proj(leave, mode_gp, pn_amd64_Leave_stack);
		arch_set_irn_register(curr_bp, bp);
		arch_set_irn_register(curr_sp, sp);
		sched_add_before(ret, leave);

		set_irn_n(ret, n_rbp, curr_bp);
	} else {
		if (frame_size > 0) {
			ir_node *incsp = amd64_new_IncSP(block, curr_sp,
			                                 -(int)frame_size, 0);
			sched_add_before(ret, incsp);
			curr_sp = incsp;
		}
	}
	set_irn_n(ret, n_amd64_Return_stack, curr_sp);

	/* keep verifier happy... */
	if (get_irn_n_edges(first_sp) == 0 && is_Proj(first_sp)) {
		kill_node(first_sp);
	}
}

static void introduce_prologue_epilogue(ir_graph *irg)
{
	const arch_register_t *sp         = &amd64_registers[REG_RSP];
	const arch_register_t *bp         = &amd64_registers[REG_RBP];
	ir_node               *start      = get_irg_start(irg);
	ir_node               *block      = get_nodes_block(start);
	ir_type               *frame_type = get_irg_frame_type(irg);
	unsigned               frame_size = get_type_size_bytes(frame_type);
	be_stack_layout_t     *layout     = be_get_irg_stack_layout(irg);
	ir_node               *initial_sp = be_get_initial_reg_value(irg, sp);
	ir_mode               *mode_gp    = mode_Lu;

	if (is_Deleted(start))
		return;

	if (!layout->sp_relative) {
		/* push rbp */
		ir_node *push = new_bd_amd64_PushRbp(NULL, block, initial_sp);
		ir_node *curr_sp = new_r_Proj(push, mode_gp, pn_amd64_PushRbp_stack);

		arch_set_irn_register(curr_sp, sp);
		sched_add_after(start, push);

		/* move rsp to rbp */
		ir_node *const curr_bp = be_new_Copy(block, curr_sp);
		sched_add_after(push, curr_bp);
		be_set_constr_single_reg_out(curr_bp, 0,
		                             bp, arch_register_req_type_ignore);
		curr_sp = be_new_CopyKeep_single(block, curr_sp, curr_bp);
		sched_add_after(curr_bp, curr_sp);
		be_set_constr_single_reg_out(curr_sp, 0,
		                             sp, arch_register_req_type_produces_sp);

		ir_node *incsp = amd64_new_IncSP(block, curr_sp, frame_size, 0);
		sched_add_after(curr_sp, incsp);

		/* make sure the initial IncSP is really used by someone */
		if (get_irn_n_edges(incsp) <= 1) {
			ir_node *in[] = { incsp };
			ir_node *keep = be_new_Keep(block, 1, in);
			sched_add_after(incsp, keep);
		}

		layout->initial_bias = -8;
	} else {
		if (frame_size > 0) {
			ir_node *const incsp = amd64_new_IncSP(block, initial_sp,
			                                       frame_size, 0);
			sched_add_after(start, incsp);
		}
	}

	/* introduce epilogue for every return node */
	foreach_irn_in(get_irg_end_block(irg), i, ret) {
		assert(is_amd64_Return(ret));
		introduce_epilogue(ret);
	}
}

/**
 * Called immediatly before emit phase.
 */
static void amd64_finish_graph(ir_graph *irg)
{
	be_stack_layout_t *stack_layout = be_get_irg_stack_layout(irg);
	bool               at_begin     = stack_layout->sp_relative;
	be_fec_env_t      *fec_env      = be_new_frame_entity_coalescer(irg);

	/* create and coalesce frame entities */
	irg_walk_graph(irg, NULL, amd64_collect_frame_entity_nodes, fec_env);
	be_assign_entities(fec_env, amd64_set_frame_entity, at_begin);
	be_free_frame_entity_coalescer(fec_env);

	irg_block_walk_graph(irg, NULL, amd64_after_ra_walker, NULL);

	introduce_prologue_epilogue(irg);

	/* fix stack entity offsets */
	be_fix_stack_nodes(irg, &amd64_registers[REG_RSP]);
	be_abi_fix_stack_bias(irg, amd64_get_sp_bias, amd64_set_frame_offset,
	                      amd64_get_frame_entity);

	/* Fix 2-address code constraints. */
	amd64_finish_irg(irg);

	/* emit code */
	amd64_emit_function(irg);
}

extern const arch_isa_if_t amd64_isa_if;
static amd64_isa_t amd64_isa_template = {
	.base = {
		.impl               = &amd64_isa_if,
		.n_registers        = N_AMD64_REGISTERS,
		.registers          = amd64_registers,
		.n_register_classes = N_AMD64_CLASSES,
		.register_classes   = amd64_reg_classes,
		.spill_cost         = 7,
		.reload_cost        = 5,
	},
};

static void amd64_finish(void)
{
	amd64_free_opcodes();
}

static arch_env_t *amd64_begin_codegeneration(void)
{
	amd64_isa_t *isa = XMALLOC(amd64_isa_t);
	*isa             = amd64_isa_template;
	isa->constants   = pmap_create();

	return &isa->base;
}

/**
 * Closes the output file and frees the ISA structure.
 */
static void amd64_end_codegeneration(void *self)
{
	amd64_isa_t *isa = (amd64_isa_t*)self;
	pmap_destroy(isa->constants);
	free(isa);
}

/**
 * prepare graph and perform code selection.
 */
static void amd64_prepare_graph(ir_graph *irg)
{
	be_timer_push(T_CODEGEN);
	amd64_transform_graph(irg);
	be_timer_pop(T_CODEGEN);

	be_dump(DUMP_BE, irg, "code-selection");
}

static void amd64_lower_for_target(void)
{
	/* lower compound param handling */
	lower_calls_with_compounds(LF_RETURN_HIDDEN);
	be_after_irp_transform("lower-calls");

	foreach_irp_irg(i, irg) {
		lower_switch(irg, 4, 256, mode_Iu);
		be_after_transform(irg, "lower-switch");
	}

	foreach_irp_irg(i, irg) {
		/* lower for mode_b stuff */
		ir_lower_mode_b(irg, mode_Lu);
		be_after_transform(irg, "lower-modeb");
		lower_alloc(irg, AMD64_PO2_STACK_ALIGNMENT);
		be_after_transform(irg, "lower-alloc");
	}

	foreach_irp_irg(i, irg) {
		/* Turn all small CopyBs into loads/stores, and turn all bigger
		 * CopyBs into memcpy calls, because we cannot handle CopyB nodes
		 * during code generation yet.
		 * TODO:  Adapt this once custom CopyB handling is implemented. */
		lower_CopyB(irg, 64, 65, true);
		be_after_transform(irg, "lower-copyb");
	}

	ir_builtin_kind supported[1];
	size_t  s = 0;
	supported[s++] = ir_bk_saturating_increment;

	assert(s <= ARRAY_SIZE(supported));
	lower_builtins(s, supported);
	be_after_irp_transform("lower-builtins");
}

static int amd64_is_mux_allowed(ir_node *sel, ir_node *mux_false,
                                ir_node *mux_true)
{
	/* optimizable by middleend */
	if (ir_is_optimizable_mux(sel, mux_false, mux_true))
		return true;
	return false;
}

static const ir_settings_arch_dep_t amd64_arch_dep = {
	.also_use_subs        = true,
	.maximum_shifts       = 4,
	.highest_shift_amount = 63,
	.evaluate             = NULL,
	.allow_mulhs          = true,
	.allow_mulhu          = true,
	.max_bits_for_mulh    = 32,
};

static backend_params amd64_backend_params = {
	.byte_order_big_endian         = false,
	.pic_supported                 = false,
	.unaligned_memaccess_supported = true,
	.modulo_shift                  = 32,
	.dep_param                     = &amd64_arch_dep,
	.allow_ifconv                  = amd64_is_mux_allowed,
	.machine_size                  = 64,
	.mode_float_arithmetic         = NULL,  /* will be set later */
	.type_long_long                = NULL,  /* will be set later */
	.type_unsigned_long_long       = NULL,  /* will be set later */
	.type_long_double              = NULL,  /* will be set later */
	.stack_param_align             = 8,
	.float_int_overflow            = ir_overflow_indefinite
};

static const backend_params *amd64_get_backend_params(void) {
	return &amd64_backend_params;
}

static int amd64_is_valid_clobber(const char *clobber)
{
	(void) clobber;
	return 0;
}

static void amd64_init_types(void)
{
	amd64_mode_E = new_float_mode("E", irma_x86_extended_float, 15, 64,
	                              ir_overflow_indefinite);
	amd64_type_E = new_type_primitive(amd64_mode_E);
	set_type_size_bytes(amd64_type_E, 16);
	set_type_alignment_bytes(amd64_type_E, 16);

	/* use an int128 mode for xmm registers for now, so that firm allows us to
	 * create constants with the xmm mode... */
	amd64_mode_xmm = new_int_mode("x86_xmm", irma_twos_complement, 128, 0, 0);

	amd64_backend_params.type_long_double = amd64_type_E;
}

static void amd64_init(void)
{
	amd64_init_types();
	amd64_register_init();
	amd64_create_opcodes(&amd64_irn_ops);
	amd64_cconv_init();
}

const arch_isa_if_t amd64_isa_if = {
	.init                 = amd64_init,
	.finish               = amd64_finish,
	.get_params           = amd64_get_backend_params,
	.lower_for_target     = amd64_lower_for_target,
	.is_valid_clobber     = amd64_is_valid_clobber,
	.begin_codegeneration = amd64_begin_codegeneration,
	.end_codegeneration   = amd64_end_codegeneration,
	.new_spill            = amd64_new_spill,
	.new_reload           = amd64_new_reload,
	.handle_intrinsics    = amd64_handle_intrinsics,
	.prepare_graph        = amd64_prepare_graph,
	.before_ra            = amd64_before_ra,
	.emit                 = amd64_finish_graph,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_amd64)
void be_init_arch_amd64(void)
{
	be_register_isa_if("amd64", &amd64_isa_if);
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.cg");

	amd64_init_finish();
	amd64_init_transform();
}
