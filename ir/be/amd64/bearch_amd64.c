/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    The main amd64 backend driver file.
 */
#include "amd64_emitter.h"
#include "amd64_finish.h"
#include "amd64_new_nodes.h"
#include "amd64_transform.h"
#include "amd64_varargs.h"
#include "bearch_amd64_t.h"
#include "beflags.h"
#include "beirg.h"
#include "bemodule.h"
#include "bera.h"
#include "besched.h"
#include "bespillslots.h"
#include "bestack.h"
#include "beutil.h"
#include "debug.h"
#include "gen_amd64_regalloc_if.h"
#include "irarch_t.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "iropt_t.h"
#include "irtools.h"
#include "lower_alloc.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "lower_mode_b.h"
#include "lowering.h"
#include "panic.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static bool use_x87_long_double;

pmap *amd64_constants;

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
	case INSN_MODE_8:       return 1;
	case INSN_MODE_16:      return 2;
	case INSN_MODE_32:      return 4;
	case INSN_MODE_64:      return 8;
	case INSN_MODE_128:     return 16;
	case INSN_MODE_INVALID: break;
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
	if (is_amd64_pop_am(node)) {
		ir_graph          *irg    = get_irn_irg(node);
		be_stack_layout_t *layout = be_get_irg_stack_layout(irg);
		if (layout->sp_relative)
			attr->addr.immediate.offset -= get_insn_mode_bytes(attr->insn_mode);
	}
	attr->addr.immediate.kind = X86_IMM_VALUE;
	attr->addr.immediate.entity = NULL;
}

static int amd64_get_sp_bias(const ir_node *node)
{
	if (is_amd64_push_am(node)) {
		const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
		return get_insn_mode_bytes(attr->insn_mode);
	} else if (is_amd64_push_reg(node)) {
		/* 64-bit register size */
		return AMD64_REGISTER_SIZE;
	} else if (is_amd64_pop_am(node)) {
		const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
		return -get_insn_mode_bytes(attr->insn_mode);
	} else if (is_amd64_leave(node)) {
		return SP_BIAS_RESET;
	}

	return 0;
}

static ir_node *create_push(ir_node *node, ir_node *schedpoint, ir_node *sp,
                            ir_node *mem, ir_entity *ent,
                            amd64_insn_mode_t insn_mode)
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
	ir_node *const push = new_bd_amd64_push_am(dbgi, block, ARRAY_SIZE(in), in, rsp_reg_mem_reqs, insn_mode, addr);
	sched_add_before(schedpoint, push);
	return push;
}

static ir_node *create_pop(ir_node *node, ir_node *schedpoint, ir_node *sp,
                           ir_entity *ent, amd64_insn_mode_t insn_mode)
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

	ir_node *const pop = new_bd_amd64_pop_am(dbgi, block, ARRAY_SIZE(in), in, rsp_reg_mem_reqs, insn_mode, addr);
	sched_add_before(schedpoint, pop);

	return pop;
}

static ir_node* create_spproj(ir_node *pred, int pos)
{
	return be_new_Proj_reg(pred, pos, &amd64_registers[REG_RSP]);
}

/**
 * Transform MemPerm, currently we do this the ugly way and produce
 * push/pop into/from memory cascades. This is possible without using
 * any registers.
 */
static void transform_MemPerm(ir_node *node)
{
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *sp    = be_get_Start_proj(irg, &amd64_registers[REG_RSP]);
	int       arity = be_get_MemPerm_entity_arity(node);
	ir_node **pops  = ALLOCAN(ir_node*, arity);
	int       i;

	/* create Pushs */
	for (i = 0; i < arity; ++i) {
		ir_entity *inent = be_get_MemPerm_in_entity(node, i);
		ir_entity *outent = be_get_MemPerm_out_entity(node, i);
		ir_type *enttype = get_entity_type(inent);
		unsigned entsize = get_type_size_bytes(enttype);
		unsigned entsize2 = get_type_size_bytes(get_entity_type(outent));
		ir_node *mem = get_irn_n(node, i);

		/* work around cases where entities have different sizes */
		if (entsize2 < entsize)
			entsize = entsize2;

		int offset = 0;
		do {
			amd64_insn_mode_t insn_mode;
			if (entsize%2 == 1) {
				insn_mode = INSN_MODE_8;
			} else if (entsize % 4 == 2) {
				insn_mode = INSN_MODE_16;
			} else if (entsize % 8 == 4) {
				insn_mode = INSN_MODE_32;
			} else {
				assert(entsize%8 == 0);
				insn_mode = INSN_MODE_64;
			}

			ir_node *push = create_push(node, node, sp, mem, inent, insn_mode);
			sp = create_spproj(push, pn_amd64_push_am_stack);
			get_amd64_addr_attr(push)->addr.immediate.offset = offset;

			unsigned size = get_insn_mode_bytes(insn_mode);
			offset  += size;
			entsize -= size;
		} while(entsize > 0);
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

		int      offset = entsize;
		ir_node *pop;
		do {
			amd64_insn_mode_t insn_mode;
			if (entsize%2 == 1) {
				insn_mode = INSN_MODE_8;
			} else if (entsize % 4 == 2) {
				insn_mode = INSN_MODE_16;
			} else if (entsize % 8 == 4) {
				insn_mode = INSN_MODE_32;
			} else {
				assert(entsize%8 == 0);
				insn_mode = INSN_MODE_64;
			}

			pop = create_pop(node, node, sp, outent, insn_mode);
			sp  = create_spproj(pop, pn_amd64_pop_am_stack);

			unsigned size = get_insn_mode_bytes(insn_mode);
			offset  -= size;
			entsize -= size;
			get_amd64_addr_attr(pop)->addr.immediate.offset = offset;
		} while(entsize > 0);
		pops[i] = pop;
	}

	ir_node *const keep = be_new_Keep_one(sp);
	sched_replace(node, keep);

	/* exchange memprojs */
	foreach_out_edge_safe(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		int p = get_Proj_num(proj);

		assert(p < arity);

		set_Proj_pred(proj, pops[p]);
		set_Proj_num(proj, pn_amd64_pop_am_M);
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
 * x86_64 only has a signed conversion so we do some crazy sse construction
 * instead (first seen this pattern in llvm): We split the 64bit value into
 * 2 32bit vals and place them into the mantissa parts of apropriately choosen
 * float values and later add the 2 floats together. In pseudo code:
 *
 * a = (vector unsigned long, unsigned long) x;
 * b = (vector unsigned, unsigned, unsigned, unsigned)
 *       upper half of 0x1p+52, upper half of 0x1p+84, 0, 0
 * c = repack (a[0], b[0], a[1], b[1])
 * d = (vector double) 0x1p+52, 0x1p+84
 * e = c - d
 * f = e[0] + e[1]
 */
static void rewrite_unsigned_float_Conv(ir_node *node)
{
	ir_graph  *irg    = get_irn_irg(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *block  = get_nodes_block(node);
	ir_node   *in     = get_Conv_op(node);
	ir_node   *in_xmm = new_r_Conv(block, in, amd64_mode_xmm);
	ir_tarval *magic0
		= new_integer_tarval_from_str("4530000043300000", 16, 0, 16,
		                              amd64_mode_xmm);
	ir_node   *const0 = new_r_Const(irg, magic0);
	collect_new_start_block_node(const0);
	ir_node   *punpck = new_bd_amd64_l_punpckldq(dbgi, block, in_xmm, const0);
	ir_tarval *magic1
		= new_integer_tarval_from_str("45300000000000004330000000000000", 32,
		                              0, 16, amd64_mode_xmm);
	ir_node   *const1 = new_r_Const(irg, magic1);
	collect_new_start_block_node(const1);
	ir_node   *subpd  = new_bd_amd64_l_subpd(dbgi, block, punpck, const1);
	ir_node   *haddpd = new_bd_amd64_l_haddpd(dbgi, block, subpd, subpd);
	ir_mode   *mode   = get_irn_mode(node);
	ir_node   *conv   = new_r_Conv(block, haddpd, mode);
	exchange(node, conv);
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
	    && get_mode_size_bits(from_mode) == 64
	    && to_mode != x86_mode_E) {
		rewrite_unsigned_float_Conv(node);
		return true;
	} else if (from_float && !to_float && !mode_is_signed(to_mode)
	           && get_mode_size_bits(to_mode) == 64
	           && from_mode != x86_mode_E) {
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
	amd64_addr_attr_t *attr = get_amd64_addr_attr(node);
	attr->addr.immediate.entity = entity;
}

static bool is_frame_load(const ir_node *node)
{
	return is_amd64_mov_gp(node) || is_amd64_movs(node)
	    || is_amd64_movs_xmm(node) || is_amd64_movdqu(node)
	    || is_amd64_fld(node);
}

/**
 * Collects nodes that need frame entities assigned.
 */
static void amd64_collect_frame_entity_nodes(ir_node *node, void *data)
{
	be_fec_env_t  *env  = (be_fec_env_t*)data;

	/* Disable coalescing for "returns twice" calls: In case of setjmp/longjmp
	 * our control flow graph isn't completely correct: There are no backedges
	 * from longjmp to the setjmp => coalescing would produce wrong results. */
	if (is_amd64_call(node)) {
		const amd64_call_addr_attr_t    *attrs = get_amd64_call_addr_attr_const(node);
		const ir_type                   *type  = attrs->call_tp;
		const mtp_additional_properties  mtp
			= get_method_additional_properties(type);
		if (mtp & mtp_property_returns_twice)
			be_forbid_coalescing(env);
	}

	/* we are only interested to report Load nodes */
	if (!is_frame_load(node))
		return;

	const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
	x86_imm32_t       const *imm  = &attr->addr.immediate;
	if (imm->kind == X86_IMM_FRAMEOFFSET && imm->entity == NULL) {
		/* TODO: do not hardcode node names here */
		const ir_mode *mode = is_amd64_movdqu(node) ? amd64_mode_xmm
		                                            : mode_Lu;
		const ir_type *type = get_type_for_mode(mode);
		be_load_needs_frame_entity(env, node, type);
	}
}

static int determine_rbp_input(ir_node *ret)
{
	arch_register_t const *const bp = &amd64_registers[REG_RBP];
	foreach_irn_in(ret, i, input) {
		if (arch_get_irn_register(input) == bp)
			return i;
	}
    panic("no rbp input found at %+F", ret);
}

/**
 * prepare graph and perform code selection.
 */
static void amd64_select_instructions(ir_graph *irg)
{
	amd64_adjust_pic(irg);

	be_timer_push(T_CODEGEN);
	amd64_transform_graph(irg);
	be_timer_pop(T_CODEGEN);

	be_dump(DUMP_BE, irg, "code-selection");

	optimize_graph_df(irg);

	be_dump(DUMP_BE, irg, "opt");
}

static void introduce_epilogue(ir_node *ret)
{
	ir_graph          *irg        = get_irn_irg(ret);
	ir_node           *block      = get_nodes_block(ret);
	ir_type           *frame_type = get_irg_frame_type(irg);
	unsigned           frame_size = get_type_size_bytes(frame_type);
	be_stack_layout_t *layout     = be_get_irg_stack_layout(irg);
	ir_node           *first_sp   = get_irn_n(ret, n_amd64_ret_stack);
	ir_node           *curr_sp    = first_sp;

	if (!layout->sp_relative) {
		int      const n_rbp    = determine_rbp_input(ret);
		ir_node       *curr_bp  = get_irn_n(ret, n_rbp);
		ir_node       *curr_mem = get_irn_n(ret, n_amd64_ret_mem);
		ir_node *const leave    = new_bd_amd64_leave(NULL, block, curr_bp, curr_mem);
		curr_mem = be_new_Proj(leave, pn_amd64_leave_M);
		curr_bp = be_new_Proj_reg(leave, pn_amd64_leave_frame, &amd64_registers[REG_RBP]);
		curr_sp = be_new_Proj_reg(leave, pn_amd64_leave_stack, &amd64_registers[REG_RSP]);
		sched_add_before(ret, leave);

		set_irn_n(ret, n_amd64_ret_mem, curr_mem);
		set_irn_n(ret, n_rbp,           curr_bp);
	} else {
		if (frame_size > 0) {
			ir_node *incsp = amd64_new_IncSP(block, curr_sp,
			                                 -(int)frame_size, 0);
			sched_add_before(ret, incsp);
			curr_sp = incsp;
		}
	}
	set_irn_n(ret, n_amd64_ret_stack, curr_sp);

	/* keep verifier happy... */
	if (get_irn_n_edges(first_sp) == 0 && is_Proj(first_sp)) {
		kill_node(first_sp);
	}
}

static void introduce_prologue(ir_graph *const irg)
{
	const arch_register_t *sp         = &amd64_registers[REG_RSP];
	const arch_register_t *bp         = &amd64_registers[REG_RBP];
	ir_node               *start      = get_irg_start(irg);
	ir_node               *block      = get_nodes_block(start);
	ir_type               *frame_type = get_irg_frame_type(irg);
	unsigned               frame_size = get_type_size_bytes(frame_type);
	be_stack_layout_t     *layout     = be_get_irg_stack_layout(irg);
	ir_node               *initial_sp = be_get_Start_proj(irg, sp);

	if (!layout->sp_relative) {
		/* push rbp */
		ir_node *const mem        = get_irg_initial_mem(irg);
		ir_node *const initial_bp = be_get_Start_proj(irg, bp);
		ir_node *const push       = new_bd_amd64_push_reg(NULL, block, initial_sp, mem, initial_bp);
		sched_add_after(start, push);
		ir_node *const curr_mem   = be_new_Proj(push, pn_amd64_push_reg_M);
		edges_reroute_except(mem, curr_mem, push);
		ir_node *const curr_sp    = be_new_Proj_reg(push, pn_amd64_push_reg_stack, sp);

		/* move rsp to rbp */
		ir_node *const curr_bp = be_new_Copy(block, curr_sp);
		sched_add_after(push, curr_bp);
		arch_copy_irn_out_info(curr_bp, 0, initial_bp);
		edges_reroute_except(initial_bp, curr_bp, push);

		ir_node *incsp = amd64_new_IncSP(block, curr_sp, frame_size, 0);
		sched_add_after(curr_bp, incsp);
		edges_reroute_except(initial_sp, incsp, push);

		/* make sure the initial IncSP is really used by someone */
		be_keep_if_unused(incsp);

		layout->initial_bias = -8;
	} else {
		if (frame_size > 0) {
			ir_node *const incsp = amd64_new_IncSP(block, initial_sp,
			                                       frame_size, 0);
			sched_add_after(start, incsp);
			edges_reroute_except(initial_sp, incsp, incsp);
		}
	}
}

static void introduce_prologue_epilogue(ir_graph *irg)
{
	/* introduce epilogue for every return node */
	foreach_irn_in(get_irg_end_block(irg), i, ret) {
		assert(is_amd64_ret(ret));
		introduce_epilogue(ret);
	}

	introduce_prologue(irg);
}

/**
 * Called immediatly before emit phase.
 */
static void amd64_finish_and_emit(ir_graph *irg)
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
	be_birg_from_irg(irg)->non_ssa_regs = NULL;
	be_abi_fix_stack_bias(irg, amd64_get_sp_bias, amd64_set_frame_offset,
	                      amd64_get_frame_entity);

	/* Fix 2-address code constraints. */
	amd64_finish_irg(irg);

	amd64_simulate_graph_x87(irg);

	/* emit code */
	be_timer_push(T_EMIT);
	amd64_emit_function(irg);
	be_timer_pop(T_EMIT);
}

static void amd64_finish(void)
{
	amd64_free_opcodes();
}

static const regalloc_if_t amd64_regalloc_if = {
	.spill_cost  = 7,
	.reload_cost = 5,
	.new_spill   = amd64_new_spill,
	.new_reload  = amd64_new_reload,
};

static void amd64_generate_code(FILE *output, const char *cup_name)
{
	amd64_constants = pmap_create();
	be_begin(output, cup_name);
	unsigned *const sp_is_non_ssa = rbitset_malloc(N_AMD64_REGISTERS);
	rbitset_set(sp_is_non_ssa, REG_RSP);

	foreach_irp_irg(i, irg) {
		if (!be_step_first(irg))
			continue;

		be_birg_from_irg(irg)->non_ssa_regs = sp_is_non_ssa;
		amd64_select_instructions(irg);

		be_step_schedule(irg);

		be_timer_push(T_RA_PREPARATION);
		be_sched_fix_flags(irg, &amd64_reg_classes[CLASS_amd64_flags], NULL,
						   NULL, NULL);
		be_timer_pop(T_RA_PREPARATION);

		be_step_regalloc(irg, &amd64_regalloc_if);

		amd64_finish_and_emit(irg);

		be_step_last(irg);
	}

	be_finish();
	pmap_destroy(amd64_constants);
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

	ir_builtin_kind supported[6];
	size_t  s = 0;
	supported[s++] = ir_bk_ffs;
	supported[s++] = ir_bk_clz;
	supported[s++] = ir_bk_ctz;
	supported[s++] = ir_bk_compare_swap;
	supported[s++] = ir_bk_saturating_increment;
	supported[s++] = ir_bk_va_start;

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
	.float_int_overflow            = ir_overflow_indefinite,
	.vararg                        = {
		.va_list_type = NULL,  /* Will be set later */
		.lower_va_arg = amd64_lower_va_arg,
	},
};

static const backend_params *amd64_get_backend_params(void) {
	return &amd64_backend_params;
}

static int amd64_is_valid_clobber(const char *clobber)
{
	return x86_parse_clobber(amd64_additional_clobber_names, clobber) != NULL;
}

static void amd64_init_types(void)
{
	ir_mode *const ptr_mode = new_reference_mode("p64", irma_twos_complement, 64, 64);
	set_modeP(ptr_mode);

	/* use an int128 mode for xmm registers for now, so that firm allows us to
	 * create constants with the xmm mode... */
	amd64_mode_xmm = new_int_mode("x86_xmm", irma_twos_complement, 128, 0, 0);

	x86_init_x87_type();
	if (use_x87_long_double) {
		amd64_backend_params.type_long_double = x86_type_E;
	}
	amd64_backend_params.vararg.va_list_type = amd64_build_va_list_type();
}

static void amd64_init(void)
{
	amd64_init_types();
	amd64_register_init();
	amd64_create_opcodes();
	amd64_cconv_init();
	x86_set_be_asm_constraint_support(&amd64_asm_constraints);
}

static unsigned amd64_get_op_estimated_cost(const ir_node *node)
{
	(void)node;/* TODO */
	return 1;
}

static arch_isa_if_t const amd64_isa_if = {
	.n_registers           = N_AMD64_REGISTERS,
	.registers             = amd64_registers,
	.n_register_classes    = N_AMD64_CLASSES,
	.register_classes      = amd64_reg_classes,
	.init                  = amd64_init,
	.finish                = amd64_finish,
	.get_params            = amd64_get_backend_params,
	.generate_code         = amd64_generate_code,
	.lower_for_target      = amd64_lower_for_target,
	.is_valid_clobber      = amd64_is_valid_clobber,
	.handle_intrinsics     = amd64_handle_intrinsics,
	.get_op_estimated_cost = amd64_get_op_estimated_cost,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_amd64)
void be_init_arch_amd64(void)
{
	be_register_isa_if("amd64", &amd64_isa_if);
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.cg");

	static const lc_opt_table_entry_t options[] = {
		LC_OPT_ENT_BOOL("x64abi", "Use x64 ABI (otherwise system V)",
						&amd64_use_x64_abi),
		LC_OPT_ENT_BOOL("x87", "Use x87 for long double", &use_x87_long_double),
		LC_OPT_LAST
	};
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *x86_64_grp = lc_opt_get_grp(be_grp, "x86_64");
	lc_opt_add_table(x86_64_grp, options);

	amd64_init_finish();
	amd64_init_transform();
}
