/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    The main amd64 backend driver file.
 */
#include "amd64_abi.h"
#include "amd64_architecture.h"
#include "amd64_bearch_t.h"
#include "amd64_emitter.h"
#include "amd64_finish.h"
#include "amd64_new_nodes.h"
#include "amd64_optimize.h"
#include "amd64_transform.h"
#include "amd64_varargs.h"
#include "beflags.h"
#include "beirg.h"
#include "bemodule.h"
#include "bera.h"
#include "besched.h"
#include "bespillslots.h"
#include "bestack.h"
#include "beutil.h"
#include "gen_amd64_regalloc_if.h"
#include "irarch.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgwalk.h"
#include "iropt_t.h"
#include "irtools.h"
#include "isas.h"
#include "lower_alloc.h"
#include "lower_builtins.h"
#include "lower_calls.h"
#include "lower_mode_b.h"
#include "lowering.h"
#include "panic.h"
#include "platform_t.h"
#include "target_t.h"

pmap *amd64_constants;

ir_mode *amd64_mode_xmm;

static ir_node *create_push(ir_node *node, ir_node *schedpoint, ir_node *sp,
                            ir_node *mem, ir_entity *ent, x86_insn_size_t size)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *frame = get_irg_frame(irg);

	x86_addr_t addr = {
		.immediate = {
			.kind   = X86_IMM_FRAMEENT,
			.entity = ent,
		},
		.variant    = X86_ADDR_BASE,
		.base_input = 1,
	};
	ir_node *in[] = { sp, frame, mem };
	ir_node *const push = new_bd_amd64_push_am(dbgi, block, ARRAY_SIZE(in), in, rsp_reg_mem_reqs, size, addr);
	sched_add_before(schedpoint, push);
	return push;
}

static ir_node *create_pop(ir_node *node, ir_node *schedpoint, ir_node *sp,
                           ir_entity *ent, x86_insn_size_t size)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = get_nodes_block(node);
	ir_graph *irg   = get_irn_irg(node);
	ir_node  *frame = get_irg_frame(irg);

	x86_addr_t addr = {
		.immediate = {
			.kind   = X86_IMM_FRAMEENT,
			.entity = ent,
		},
		.variant     = X86_ADDR_BASE,
		.base_input  = 1,
	};
	ir_node *in[] = { sp, frame, get_irg_no_mem(irg) };

	ir_node *const pop = new_bd_amd64_pop_am(dbgi, block, ARRAY_SIZE(in), in, rsp_reg_mem_reqs, size, addr);
	sched_add_before(schedpoint, pop);

	return pop;
}

static ir_node* create_spproj(ir_node *pred, int pos)
{
	return be_new_Proj_reg(pred, pos, &amd64_registers[REG_RSP]);
}

static x86_insn_size_t entsize2insnsize(unsigned const entsize)
{
	return
		entsize % 2 == 1 ? X86_SIZE_8  :
		entsize % 4 == 2 ? X86_SIZE_16 :
		entsize % 8 == 4 ? X86_SIZE_32 :
		(assert(entsize % 8 == 0), X86_SIZE_64);
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

	/* create Pushs */
	for (int i = 0; i < arity; ++i) {
		ir_entity *inent = be_get_MemPerm_in_entity(node, i);
		ir_entity *outent = be_get_MemPerm_out_entity(node, i);
		assert(inent->kind == IR_ENTITY_SPILLSLOT);
		assert(outent->kind == IR_ENTITY_SPILLSLOT);
		unsigned entsize = inent->attr.spillslot.size;
		unsigned entsize2 = outent->attr.spillslot.size;
		ir_node *mem = get_irn_n(node, i);

		/* work around cases where entities have different sizes */
		if (entsize2 < entsize)
			entsize = entsize2;

		int offset = 0;
		do {
			x86_insn_size_t const size = entsize2insnsize(entsize);
			ir_node        *const push = create_push(node, node, sp, mem, inent, size);
			sp = create_spproj(push, pn_amd64_push_am_stack);
			get_amd64_addr_attr(push)->addr.immediate.offset = offset;

			unsigned bytes = x86_bytes_from_size(size);
			offset  += bytes;
			entsize -= bytes;
		} while(entsize > 0);
		set_irn_n(node, i, new_r_Bad(irg, mode_X));
	}

	/* create pops */
	for (int i = arity; i-- > 0; ) {
		ir_entity *inent = be_get_MemPerm_in_entity(node, i);
		ir_entity *outent = be_get_MemPerm_out_entity(node, i);
		assert(inent->kind == IR_ENTITY_SPILLSLOT);
		assert(outent->kind == IR_ENTITY_SPILLSLOT);
		unsigned entsize = outent->attr.spillslot.size;
		unsigned entsize2 = inent->attr.spillslot.size;

		/* work around cases where entities have different sizes */
		if (entsize2 < entsize)
			entsize = entsize2;

		int      offset = entsize;
		ir_node *pop;
		do {
			x86_insn_size_t const size = entsize2insnsize(entsize);
			pop = create_pop(node, node, sp, outent, size);
			sp  = create_spproj(pop, pn_amd64_pop_am_stack);

			unsigned bytes = x86_bytes_from_size(size);
			offset  -= bytes;
			entsize -= bytes;
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
	ir_node   *in_xmm = new_rd_Conv(dbgi, block, in, amd64_mode_xmm);
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
	ir_node   *conv   = new_rd_Conv(dbgi, block, haddpd, mode);
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
	ir_node *sub         = new_r_Sub(true_block, fp_x, fp_const);
	ir_node *sub_conv    = new_rd_Conv(dbgi, true_block, sub, mode_Ls);
	ir_node *sign_bit    = create_sign_bit_const(irg);
	collect_new_start_block_node(sign_bit);
	ir_node *xor         = new_r_Eor(true_block, sub_conv, sign_bit);
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

static bool is_flt(ir_mode *const mode)
{
	return mode_is_float(mode) && mode != x86_mode_E;
}

static bool is_u64(ir_mode *const mode)
{
	return !mode_is_float(mode) && !mode_is_signed(mode) && get_mode_size_bits(mode) == 64;
}

static bool amd64_rewrite_Conv(ir_node *const node)
{
	ir_mode *const to_mode    = get_irn_mode(node);
	ir_node *const op         = get_Conv_op(node);
	ir_mode *const from_mode  = get_irn_mode(op);
	if (is_u64(from_mode) && is_flt(to_mode)) {
		rewrite_unsigned_float_Conv(node);
		return true;
	} else if (is_flt(from_mode) && is_u64(to_mode)) {
		rewrite_float_unsigned_Conv(node);
		return true;
	} else {
		return false;
	}
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
                                   unsigned size, unsigned po2align)
{
	(void)size;
	(void)po2align;
	amd64_addr_attr_t *attr = get_amd64_addr_attr(node);
	attr->addr.immediate.entity = entity;
}

/**
 * Collects nodes that need frame entities assigned.
 */
static void amd64_collect_frame_entity_nodes(ir_node *node, void *data)
{
	/* we are only interested to report Load nodes */
	if (!is_amd64_irn(node) || !amd64_loads(node))
		return;

	amd64_addr_attr_t const *attr = get_amd64_addr_attr_const(node);
	x86_imm32_t       const *imm  = &attr->addr.immediate;
	if (imm->kind == X86_IMM_FRAMEENT && imm->entity == NULL) {
		be_fec_env_t  *const env      = (be_fec_env_t*)data;
		unsigned size;
		unsigned po2align;
		if (attr->base.size == X86_SIZE_80) {
			size     = 12;
			po2align = 2;
		} else {
			size     = x86_bytes_from_size(attr->base.size);
			po2align = log2_floor(size);
		}
		be_load_needs_frame_entity(env, node, size, po2align);
	}
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

static void introduce_epilogue(ir_node *ret, bool omit_fp)
{
	ir_graph *irg      = get_irn_irg(ret);
	ir_node  *block    = get_nodes_block(ret);
	ir_node  *first_sp = get_irn_n(ret, n_amd64_ret_stack);
	ir_node  *curr_sp  = first_sp;

	if (!omit_fp) {
		int const n_rbp = be_get_input_pos_for_req(ret, &amd64_single_reg_req_gp_rbp);
		assert(n_rbp >= 0);

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
		ir_type *frame_type = get_irg_frame_type(irg);
		unsigned frame_size = get_type_size(frame_type);
		ir_node *incsp = amd64_new_IncSP(block, curr_sp, -(int)frame_size,
		                                 true);
		sched_add_before(ret, incsp);
		curr_sp = incsp;
	}
	set_irn_n(ret, n_amd64_ret_stack, curr_sp);

	/* keep verifier happy... */
	if (get_irn_n_edges(first_sp) == 0 && is_Proj(first_sp)) {
		kill_node(first_sp);
	}
}

static void introduce_prologue(ir_graph *const irg, bool omit_fp)
{
	const arch_register_t *sp         = &amd64_registers[REG_RSP];
	const arch_register_t *bp         = &amd64_registers[REG_RBP];
	ir_node               *start      = get_irg_start(irg);
	ir_node               *block      = get_nodes_block(start);
	ir_type               *frame_type = get_irg_frame_type(irg);
	unsigned               frame_size = get_type_size(frame_type);
	ir_node               *initial_sp = be_get_Start_proj(irg, sp);

	if (!omit_fp) {
		/* push rbp */
		ir_node *const mem        = get_irg_initial_mem(irg);
		ir_node *const initial_bp = be_get_Start_proj(irg, bp);
		ir_node *const push       = new_bd_amd64_push_reg(NULL, block, initial_sp, mem, initial_bp, X86_SIZE_64);
		sched_add_after(start, push);
		ir_node *const curr_mem   = be_new_Proj(push, pn_amd64_push_reg_M);
		edges_reroute_except(mem, curr_mem, push);
		ir_node *const curr_sp    = be_new_Proj_reg(push, pn_amd64_push_reg_stack, sp);

		/* move rsp to rbp */
		ir_node *const curr_bp = be_new_Copy(block, curr_sp);
		sched_add_after(push, curr_bp);
		arch_copy_irn_out_info(curr_bp, 0, initial_bp);
		edges_reroute_except(initial_bp, curr_bp, push);

		ir_node *incsp = amd64_new_IncSP(block, curr_sp, frame_size, false);
		sched_add_after(curr_bp, incsp);
		edges_reroute_except(initial_sp, incsp, push);

		/* make sure the initial IncSP is really used by someone */
		be_keep_if_unused(incsp);
	} else {
		ir_node *const incsp = amd64_new_IncSP(block, initial_sp,
		                                       frame_size, false);
		sched_add_after(start, incsp);
		edges_reroute_except(initial_sp, incsp, incsp);
	}
}

static void introduce_prologue_epilogue(ir_graph *irg, bool omit_fp)
{
	/* introduce epilogue for every return node */
	foreach_irn_in(get_irg_end_block(irg), i, ret) {
		assert(is_amd64_ret(ret));
		introduce_epilogue(ret, omit_fp);
	}

	introduce_prologue(irg, omit_fp);
}

static bool node_has_sp_base(ir_node const *const node,
                             x86_addr_t const *const addr)
{
	if (!x86_addr_variant_has_base(addr->variant))
		return false;
	arch_register_t const *const base_reg
		= arch_get_irn_register_in(node, addr->base_input);
	return base_reg == &amd64_registers[REG_RSP];
}

static void amd64_determine_frameoffset_addr(ir_node *const node, x86_addr_t *const addr, int const sp_offset)
{
	if (addr->immediate.kind == X86_IMM_FRAMEENT) {
		addr->immediate.offset += get_entity_offset(addr->immediate.entity);
		addr->immediate.entity  = NULL;
		addr->immediate.kind    = X86_IMM_FRAMEOFFSET;
	}

	if (addr->immediate.kind == X86_IMM_FRAMEOFFSET) {
		if (node_has_sp_base(node, addr)) {
			addr->immediate.offset += sp_offset;
		} else {
			/* we calculate offsets relative to the SP value at function begin,
			 * but RBP points after the saved old frame pointer */
			addr->immediate.offset += AMD64_REGISTER_SIZE;
		}
		addr->immediate.kind = X86_IMM_VALUE;
	}
}

static void amd64_determine_frameoffset(ir_node *node, int sp_offset)
{
	if (is_amd64_irn(node)) {
		if (amd64_has_addr_attr(get_amd64_attr_const(node)->op_mode)) {
			x86_addr_t *const addr = &get_amd64_addr_attr(node)->addr;
			amd64_determine_frameoffset_addr(node, addr, sp_offset);
		}
	} else if (be_is_Asm(node)) {
		be_asm_attr_t const *const attr = get_be_asm_attr_const(node);
		x86_asm_operand_t   *const ops  = (x86_asm_operand_t*)attr->operands;
		for (size_t i = 0, n = ARR_LEN(ops); i != n; ++i) {
			x86_asm_operand_t *const op = &ops[i];
			if (op->op.kind == BE_ASM_OPERAND_MEMORY)
				amd64_determine_frameoffset_addr(node, &op->u.addr, sp_offset);
		}
	}
}

static void amd64_sp_sim(ir_node *const node, stack_pointer_state_t *state)
{
	/* Pop nodes modify the stack pointer before calculating destination
	 * address, so do this first */
	if (is_amd64_pop_am(node)) {
		const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
		state->offset -= x86_bytes_from_size(attr->base.size);
	}

	amd64_determine_frameoffset(node, state->offset);

	if (is_amd64_push_am(node)) {
		const amd64_addr_attr_t *attr = get_amd64_addr_attr_const(node);
		state->offset       += x86_bytes_from_size(attr->base.size);
	} else if (is_amd64_push_reg(node)) {
		/* 64-bit register size */
		state->offset       += AMD64_REGISTER_SIZE;
	} else if (is_amd64_leave(node)) {
		state->offset        = 0;
		state->align_padding = 0;
	} else if (is_amd64_sub_sp(node)) {
		state->align_padding = 0;
	}
}

int amd64_get_sp_change(ir_node *const node)
{
	if (be_is_IncSP(node))
		return -be_get_IncSP_offset(node);
	stack_pointer_state_t state = {
		.offset    = 160,
		.no_change = true,
	};
	amd64_sp_sim(node, &state);
	int res = 160 - state.offset;
	assert(-16 <= res && res <= 16);
	return res;
}

/**
 * Called immediately before emit phase.
 */
static void amd64_finish_and_emit(ir_graph *irg)
{
	amd64_irg_data_t const *const irg_data = amd64_get_irg_data(irg);
	bool                    const omit_fp  = irg_data->omit_fp;

	/* create and coalesce frame entities */
	be_fec_env_t *fec_env = be_new_frame_entity_coalescer(irg);
	irg_walk_graph(irg, NULL, amd64_collect_frame_entity_nodes, fec_env);
	be_assign_entities(fec_env, amd64_set_frame_entity, omit_fp);
	be_free_frame_entity_coalescer(fec_env);

	ir_type *const frame = get_irg_frame_type(irg);
	be_sort_frame_entities(frame, omit_fp);
	unsigned const misalign = AMD64_REGISTER_SIZE; /* return address on stack */
	int      const begin    = omit_fp ? 0 : -AMD64_REGISTER_SIZE;
	be_layout_frame_type(frame, begin, misalign);

	irg_block_walk_graph(irg, NULL, amd64_after_ra_walker, NULL);

	introduce_prologue_epilogue(irg, omit_fp);

	/* fix stack entity offsets */
	be_fix_stack_nodes(irg, &amd64_registers[REG_RSP]);
	be_birg_from_irg(irg)->non_ssa_regs = NULL;
	unsigned const p2align = AMD64_PO2_STACK_ALIGNMENT;
	be_sim_stack_pointer(irg, misalign, p2align, amd64_sp_sim);

	/* Fix 2-address code constraints. */
	amd64_finish_irg(irg);

	amd64_simulate_graph_x87(irg);

	amd64_peephole_optimization(irg);

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
	unsigned *const sp_is_non_ssa = rbitset_alloca(N_AMD64_REGISTERS);
	rbitset_set(sp_is_non_ssa, REG_RSP);

	foreach_irp_irg(i, irg) {
		if (!be_step_first(irg))
			continue;

		struct obstack *obst = be_get_be_obst(irg);
		be_birg_from_irg(irg)->isa_link = OALLOCZ(obst, amd64_irg_data_t);

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

static const ir_settings_arch_dep_t amd64_arch_dep = {
	.replace_muls         = true,
	.replace_divs         = true,
	.replace_mods         = true,
	.allow_mulhs          = true,
	.allow_mulhu          = true,
	.also_use_subs        = true,
	.maximum_shifts       = 4,
	.highest_shift_amount = 63,
	.evaluate             = NULL,
	.max_bits_for_mulh    = 32,
};

static void amd64_lower_for_target(void)
{
	ir_arch_lower(&amd64_arch_dep);
	be_after_irp_transform("lower_arch-dep");

	amd64_abi_state parameter_state = (amd64_abi_state) {
		.integer_params = 0,
		.sse_params = 0,
	};

	amd64_abi_state result_state = (amd64_abi_state) {
		.integer_params = 0,
		.sse_params = 0,
	};

	/* lower compound param handling */
	lower_calls_with_compounds(LF_RETURN_HIDDEN,
	                           amd64_lower_parameter, &parameter_state,
	                           amd64_lower_result, &result_state,
	                           amd64_reset_abi_state);
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
	lower_builtins(s, supported, amd64_lower_va_arg);
	be_after_irp_transform("lower-builtins");
}

static void amd64_init_types(void)
{
	/* use an int128 mode for xmm registers for now, so that firm allows us to
	 * create constants with the xmm mode... */
	amd64_mode_xmm = new_int_mode("x86_xmm", 128, 0, 0);
	x86_init_x87_type();

	ir_platform.va_list_type = amd64_build_va_list_type();
}

static void amd64_init(void)
{
	amd64_setup_cg_config();
	amd64_init_types();
	amd64_register_init();
	amd64_create_opcodes();
	amd64_cconv_init();
	x86_set_be_asm_constraint_support(&amd64_asm_constraints);

	ir_target.experimental = "the amd64 backend is experimental and unfinished (consider the ia32 backend)";
	ir_target.fast_unaligned_memaccess = true;
	ir_target.float_int_overflow       = ir_overflow_indefinite;
}

static unsigned amd64_get_op_estimated_cost(const ir_node *node)
{
	(void)node;/* TODO */
	return 1;
}

/** we don't have a concept of aliasing registers, so enumerate them
 * manually for the asm nodes. */
static be_register_name_t const amd64_additional_reg_names[] = {
	{ "al", REG_RAX }, { "ah", REG_RAX }, { "ax", REG_RAX }, { "eax", REG_RAX },
	{ "bl", REG_RBX }, { "bh", REG_RBX }, { "bx", REG_RBX }, { "ebx", REG_RBX },
	{ "cl", REG_RCX }, { "ch", REG_RCX }, { "cx", REG_RCX }, { "ecx", REG_RCX },
	{ "dl", REG_RDX }, { "dh", REG_RDX }, { "dx", REG_RDX }, { "edx", REG_RDX },
	{ "sil",  REG_RSI }, { "si",   REG_RSI }, { "esi",  REG_RSI },
	{ "dil",  REG_RDI }, { "di",   REG_RDI }, { "edi",  REG_RDI },
	{ "bpl",  REG_RBP }, { "bp",   REG_RBP }, { "ebp",  REG_RBP },
	{ "spl",  REG_RSP }, { "sp",   REG_RSP }, { "esp",  REG_RSP },
	{ "r8b",  REG_R8  }, { "r8w",  REG_R8  }, { "r8d",  REG_R8  },
	{ "r9b",  REG_R9  }, { "r9w",  REG_R9  }, { "r9d",  REG_R9  },
	{ "r10b", REG_R10 }, { "r10w", REG_R10 }, { "r10d", REG_R10 },
	{ "r11b", REG_R11 }, { "r11w", REG_R11 }, { "r11d", REG_R11 },
	{ "r12b", REG_R12 }, { "r12w", REG_R12 }, { "r12d", REG_R12 },
	{ "r13b", REG_R13 }, { "r13w", REG_R13 }, { "r13d", REG_R13 },
	{ "r14b", REG_R14 }, { "r14w", REG_R14 }, { "r14d", REG_R14 },
	{ "r15b", REG_R15 }, { "r15w", REG_R15 }, { "r15d", REG_R15 },
	{ NULL, ~0u }
};

arch_isa_if_t const amd64_isa_if = {
	.name                  = "amd64",
	.pointer_size          = 8,
	.modulo_shift          = 32,
	.big_endian            = false,
	.po2_biggest_alignment = 4,
	.pic_supported         = true,
	.n_registers           = N_AMD64_REGISTERS,
	.registers             = amd64_registers,
	.n_register_classes    = N_AMD64_CLASSES,
	.register_classes      = amd64_reg_classes,
	.init                  = amd64_init,
	.finish                = amd64_finish,
	.generate_code         = amd64_generate_code,
	.lower_for_target      = amd64_lower_for_target,
	.additional_reg_names  = amd64_additional_reg_names,
	.handle_intrinsics     = amd64_handle_intrinsics,
	.get_op_estimated_cost = amd64_get_op_estimated_cost,
};

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_arch_amd64)
void be_init_arch_amd64(void)
{
	/*static const lc_opt_table_entry_t options[] = {
		LC_OPT_ENT_BOOL("no-red-zone", "gcc compatibility",                &amd64_use_red_zone),
		LC_OPT_ENT_BOOL("fma",         "support FMA3 code generation",     &use_scalar_fma3),
		LC_OPT_LAST
	};
	lc_opt_entry_t *be_grp    = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *amd64_grp = lc_opt_get_grp(be_grp, "amd64");
	lc_opt_add_table(amd64_grp, options);*/

	amd64_init_transform();
	amd64_init_architecture();
}
