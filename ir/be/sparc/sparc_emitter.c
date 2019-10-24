/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   emit assembler for a backend graph
 * @author  Hannes Rapp, Matthias Braun
 */
#include "sparc_emitter.h"

#include "beasm.h"
#include "beblocksched.h"
#include "bediagnostic.h"
#include "beemithlp.h"
#include "beemitter.h"
#include "begnuas.h"
#include "beirg.h"
#include "benode.h"
#include "bepeephole.h"
#include "besched.h"
#include "bestack.h"
#include "beutil.h"
#include "debug.h"
#include "execfreq_t.h"
#include "gen_sparc_emitter.h"
#include "gen_sparc_regalloc_if.h"
#include "heights.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "panic.h"
#include "pmap.h"
#include "sparc_bearch_t.h"
#include "sparc_new_nodes.h"
#include "util.h"
#include <inttypes.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static ir_heights_t *heights;
static unsigned     *delay_slot_fillers;
static pmap         *delay_slots;

static bool emitting_delay_slot;

static void sparc_emit_immediate(int32_t value, ir_entity *entity)
{
	if (entity == NULL) {
		be_emit_irprintf("%"PRId32, value);
	} else {
		if (is_tls_entity(entity)) {
			be_emit_cstring("%tle_lox10(");
		} else {
			be_emit_cstring("%lo(");
		}
		be_gas_emit_entity(entity);
		if (value != 0) {
			be_emit_irprintf("%+"PRId32, value);
		}
		be_emit_char(')');
	}
}

static void sparc_emit_high_immediate(ir_node const *node)
{
	const sparc_attr_t *attr   = get_sparc_attr_const(node);
	ir_entity          *entity = attr->immediate_value_entity;

	if (entity == NULL) {
		uint32_t value = (uint32_t) attr->immediate_value;
		be_emit_irprintf("%%hi(0x%X)", value);
	} else {
		if (is_tls_entity(entity)) {
			be_emit_cstring("%tle_hix22(");
		} else {
			be_emit_cstring("%hi(");
		}
		be_gas_emit_entity(entity);
		if (attr->immediate_value != 0) {
			be_emit_irprintf("%+"PRId32, attr->immediate_value);
		}
		be_emit_char(')');
	}
}

static void sparc_emit_register(const arch_register_t *const reg)
{
	be_emit_char('%');
	be_emit_string(reg->name);
}

static void sparc_emit_source_register(ir_node const *node, int const pos)
{
	const arch_register_t *reg = arch_get_irn_register_in(node, pos);
	sparc_emit_register(reg);
}

static void sparc_emit_dest_register(ir_node const *const node, int const pos)
{
	const arch_register_t *reg = arch_get_irn_register_out(node, pos);
	sparc_emit_register(reg);
}

/**
 * emit SP offset
 */
static void sparc_emit_offset(const ir_node *node, int offset_node_pos)
{
	const sparc_load_store_attr_t *attr = get_sparc_load_store_attr_const(node);

	if (attr->is_reg_reg) {
		assert(!attr->is_frame_entity);
		assert(attr->base.immediate_value == 0);
		assert(attr->base.immediate_value_entity == NULL);
		be_emit_char('+');
		sparc_emit_source_register(node, offset_node_pos);
	} else if (attr->is_frame_entity) {
		int32_t offset = attr->base.immediate_value;
		if (offset != 0) {
			assert(sparc_is_value_imm_encodeable(offset));
			be_emit_irprintf("%+"PRId32, offset);
		}
	} else if (attr->base.immediate_value != 0
	           || attr->base.immediate_value_entity != NULL) {
		be_emit_char('+');
		sparc_emit_immediate(attr->base.immediate_value,
		                     attr->base.immediate_value_entity);
	}
}

/**
 *  Emit load mode
 */
static void sparc_emit_load_mode(ir_node const *const node)
{
	const sparc_load_store_attr_t *attr = get_sparc_load_store_attr_const(node);
	ir_mode *mode      = attr->load_store_mode;
	int      bits      = get_mode_size_bits(mode);
	bool     is_signed = mode_is_signed(mode);

	switch (bits) {
	case   8: be_emit_string(is_signed ? "sb" : "ub"); break;
	case  16: be_emit_string(is_signed ? "sh" : "uh"); break;
	case  32: break;
	case  64: be_emit_char('d'); break;
	case 128: be_emit_char('q'); break;
	default:  panic("invalid load/store mode %+F", mode);
	}
}

/**
 * Emit store mode char
 */
static void sparc_emit_store_mode(ir_node const *const node)
{
	const sparc_load_store_attr_t *attr = get_sparc_load_store_attr_const(node);
	ir_mode *mode      = attr->load_store_mode;
	int      bits      = get_mode_size_bits(mode);

	switch (bits) {
	case   8: be_emit_char('b'); break;
	case  16: be_emit_char('h'); break;
	case  32: break;
	case  64: be_emit_char('d'); break;
	case 128: be_emit_char('q'); break;
	default:  panic("invalid load/store mode %+F", mode);
	}
}

static void emit_fp_suffix(const ir_mode *mode)
{
	assert(mode_is_float(mode));
	switch (get_mode_size_bits(mode)) {
	case  32: be_emit_char('s'); break;
	case  64: be_emit_char('d'); break;
	case 128: be_emit_char('q'); break;
	default:  panic("invalid FP mode");
	}
}

/**
 * returns true if a sparc_call calls a register and not an immediate
 */
static bool is_sparc_reg_call(const ir_node *node)
{
	const sparc_attr_t *attr = get_sparc_attr_const(node);
	return attr->immediate_value_entity == NULL;
}

static int get_sparc_Call_dest_addr_pos(const ir_node *node)
{
	assert(is_sparc_reg_call(node));
	return get_irn_arity(node)-1;
}

static bool is_no_instruction(const ir_node *node)
{
	/* copies are nops if src_reg == dest_reg */
	if (be_is_Copy(node) || be_is_CopyKeep(node)) {
		const arch_register_t *src_reg  = arch_get_irn_register_in(node, 0);
		const arch_register_t *dest_reg = arch_get_irn_register_out(node, 0);

		if (src_reg == dest_reg)
			return true;
	}
	if (be_is_IncSP(node) && be_get_IncSP_offset(node) == 0)
		return true;
	/* Ba is not emitted if it is a simple fallthrough */
	if (is_sparc_Ba(node) && be_is_fallthrough(node))
		return true;

	return be_is_Keep(node) || be_is_Start(node) || be_is_Unknown(node) || is_Phi(node)
		|| be_is_RegSplit(node) || be_is_RegJoin(node);
}

static bool has_delay_slot(const ir_node *node)
{
	if (is_sparc_Ba(node)) {
		return !be_is_fallthrough(node);
	}

	return arch_get_irn_flags(node) & sparc_arch_irn_flag_has_delay_slot;
}

/** returns true if the emitter for this sparc node can produce more than one
 * actual sparc instruction.
 * Usually it is a bad sign if we have to add instructions here. We should
 * rather try to get them lowered down. So we can actually put them into
 * delay slots and make them more accessible to the scheduler.
 */
static bool emits_multiple_instructions(const ir_node *node)
{
	if (has_delay_slot(node))
		return true;

	if (is_sparc_Call(node))
		return arch_get_irn_flags(node) & sparc_arch_irn_flag_aggregate_return;

	if (be_is_Copy(node)) {
		// TODO This repeats most of the logic of emit_be_Copy
		// We should have two nodes to copy the halves of a double register separately
		const arch_register_t *src_reg = arch_get_irn_register_in(node, 0);
		const arch_register_t *dst_reg = arch_get_irn_register_out(node, 0);
		if (src_reg == dst_reg)
			return false;

		arch_register_class_t const *const cls = dst_reg->cls;
		if (cls == &sparc_reg_classes[CLASS_sparc_fp]) {
			ir_mode *mode = get_irn_mode(node);
			unsigned bits = get_mode_size_bits(mode);
			return bits > 32;
		} else {
			return false;
		}
	}

	return is_sparc_SMulh(node) || is_sparc_UMulh(node)
		|| is_sparc_SDiv(node) || is_sparc_UDiv(node)
		|| be_is_MemPerm(node) || be_is_Perm(node)
		|| is_sparc_SubSP(node) || be_is_Asm(node);
}

static bool uses_reg(const ir_node *node, unsigned reg_index, unsigned width)
{
	int arity = get_irn_arity(node);
	for (int i = 0; i < arity; ++i) {
		const arch_register_t     *in_reg = arch_get_irn_register_in(node, i);
		const arch_register_req_t *in_req = arch_get_irn_register_req_in(node, i);
		if (in_reg == NULL)
			continue;
		if (reg_index < (unsigned)in_reg->global_index + in_req->width
			&& reg_index + width > in_reg->global_index)
			return true;
	}
	return false;
}

static bool writes_reg(const ir_node *node, unsigned reg_index, unsigned width)
{
	be_foreach_out(node, o) {
		const arch_register_t *out_reg = arch_get_irn_register_out(node, o);
		if (out_reg == NULL)
			continue;
		const arch_register_req_t *out_req = arch_get_irn_register_req_out(node, o);
		if (reg_index < (unsigned)out_reg->global_index + out_req->width
			&& reg_index + width > out_reg->global_index)
			return true;
	}
	return false;
}

static bool is_legal_delay_slot_filler(const ir_node *node)
{
	if (is_no_instruction(node))
		return false;
	if (emits_multiple_instructions(node))
		return false;
	if (rbitset_is_set(delay_slot_fillers, get_irn_idx(node)))
		return false;
	if (is_sparc_Save(node))
		return false;
	return true;
}

static bool can_move_down_into_delayslot(const ir_node *node, const ir_node *to)
{
	if (!is_legal_delay_slot_filler(node))
		return false;

	if (!be_can_move_down(heights, node, to, sparc_get_frame_entity))
		return false;

	if (is_sparc_Call(to)) {
		ir_node *check;
		/** all inputs are used after the delay slot so, we're fine */
		if (!is_sparc_reg_call(to))
			return true;

		check = get_irn_n(to, get_sparc_Call_dest_addr_pos(to));
		if (skip_Proj(check) == node)
			return false;

		/* the Call also destroys the value of %o7, but since this is
		 * currently marked as ignore register in the backend, it
		 * should never be used by the instruction in the delay slot. */
		if (uses_reg(node, REG_O7, 1))
			return false;
		return true;
	} else if (is_sparc_Return(to)) {
		/* return uses the value of %o7, all other values are not
		 * immediately used */
		if (writes_reg(node, REG_O7, 1))
			return false;
		return true;
	} else {
		/* the node must not use our computed values */
		foreach_irn_in(to, i, in) {
			if (skip_Proj(in) == node)
				return false;
		}
		return true;
	}
}

static bool is_restore(ir_node const *const node)
{
	return is_sparc_Restore(node) || is_sparc_RestoreZero(node);
}

static bool can_move_up_into_delayslot(const ir_node *node, const ir_node *to)
{
	if (!be_can_move_up(heights, node, to))
		return false;

	/* node must not use any results of 'to' */
	foreach_irn_in(node, i, in) {
		ir_node *skipped = skip_Proj(in);
		if (skipped == to)
			return false;
	}

	/* register window cycling effects at Restore aren't correctly represented
	 * in the graph yet so we need this exception here */
	if (is_restore(node)) {
		return false;
	} else if (is_sparc_Call(to)) {
		/* node must not overwrite any of the inputs of the call,
		 * (except for the dest_addr) */
		int dest_addr_pos = is_sparc_reg_call(to)
			? get_sparc_Call_dest_addr_pos(to) : -1;

		int call_arity = get_irn_arity(to);
		for (int i = 0; i < call_arity; ++i) {
			if (i == dest_addr_pos)
				continue;
			const arch_register_t *reg = arch_get_irn_register_in(to, i);
			if (reg == NULL)
				continue;
			const arch_register_req_t *req = arch_get_irn_register_req_in(to, i);
			if (writes_reg(node, reg->global_index, req->width))
				return false;
		}

		/* node must not write to one of the call outputs */
		be_foreach_out(to, o) {
			const arch_register_t *reg = arch_get_irn_register_out(to, o);
			if (reg == NULL)
				continue;
			const arch_register_req_t *req = arch_get_irn_register_req_out(to, o);
			if (writes_reg(node, reg->global_index, req->width))
				return false;
		}
	} else if (is_sparc_SDiv(to) || is_sparc_UDiv(to)) {
		/* node will be inserted between wr and div so it must not overwrite
		 * anything except the wr input */
		for (int i = 0, arity = get_irn_arity(to); i < arity; ++i) {
			assert((unsigned)n_sparc_SDiv_dividend_high == (unsigned)n_sparc_UDiv_dividend_high);
			if (i == n_sparc_SDiv_dividend_high)
				continue;
			const arch_register_t *reg = arch_get_irn_register_in(to, i);
			if (reg == NULL)
				continue;
			const arch_register_req_t *req = arch_get_irn_register_req_in(to, i);
			if (writes_reg(node, reg->global_index, req->width))
				return false;
		}
	}
	return true;
}

static void optimize_fallthrough(ir_node *node)
{
	be_cond_branch_projs_t const projs = be_get_cond_branch_projs(node);

	if (be_is_fallthrough(projs.t)) {
		/* exchange both proj destinations so the second one can be omitted */
		set_Proj_num(projs.t, pn_sparc_Bicc_false);
		set_Proj_num(projs.f, pn_sparc_Bicc_true);

		sparc_jmp_cond_attr_t *attr = get_sparc_jmp_cond_attr(node);
		attr->relation = get_negated_relation(attr->relation);
	}
}

/**
 * search for an instruction that can fill the delay slot of @p node
 */
static ir_node *pick_delay_slot_for(ir_node *node)
{
	static const unsigned PICK_DELAY_SLOT_MAX_DISTANCE = 10;
	assert(has_delay_slot(node));

	if (is_sparc_cond_branch(node))
		optimize_fallthrough(node);

	unsigned tries = 0;
	sched_foreach_reverse_before(node, schedpoint) {
		if (has_delay_slot(schedpoint))
			break;
		if (tries++ >= PICK_DELAY_SLOT_MAX_DISTANCE)
			break;

		if (!can_move_down_into_delayslot(schedpoint, node))
			continue;

		/* found something */
		return schedpoint;
	}

	/* search after the current position */
	tries = 0;
	sched_foreach_after(node, schedpoint) {
		if (has_delay_slot(schedpoint))
			break;
		if (tries++ >= PICK_DELAY_SLOT_MAX_DISTANCE)
			break;
		if (!is_legal_delay_slot_filler(schedpoint))
			continue;
		if (!can_move_up_into_delayslot(schedpoint, node))
			continue;

		/* found something */
		return schedpoint;
	}

	/* look in successor blocks */
	ir_node *block = get_nodes_block(node);
	/* TODO: sort succs by execution frequency */
	foreach_block_succ(block, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		/* we can't easily move up stuff from blocks with multiple predecessors
		 * since the instruction is lacking for the other preds then.
		 * (We also don't have to do any phi translation) */
		if (get_Block_n_cfgpreds(succ) > 1)
			continue;

		tries = 0;
		sched_foreach(succ, schedpoint) {
			if (has_delay_slot(schedpoint))
				break;
			/* restore doesn't model register window switching correctly,
			 * so it appears like we could move it, which is not true */
			if (is_restore(schedpoint))
				continue;
			if (tries++ >= PICK_DELAY_SLOT_MAX_DISTANCE)
				break;
			if (!is_legal_delay_slot_filler(schedpoint))
				continue;
			if (!get_irn_pinned(schedpoint) && !is_memop(schedpoint) && can_move_up_into_delayslot(schedpoint, node)) {
				/* it's fine to move the insn across blocks */
				return schedpoint;
			} else if (is_sparc_cond_branch(node)) {
				ir_node *proj = get_Block_cfgpred(succ, 0);
				unsigned nr   = get_Proj_num(proj);
				if ((nr == pn_sparc_Bicc_true || nr == pn_sparc_fbfcc_true)
					&& be_can_move_up(heights, schedpoint, succ)) {
					/* we can use it with the annul flag */
					sparc_jmp_cond_attr_t *attr = get_sparc_jmp_cond_attr(node);
					attr->annul_delay_slot = true;
					return schedpoint;
				}
			}
		}
	}

	return NULL;
}

void sparc_emitf(ir_node const *const node, char const *fmt, ...)
{
	BE_EMITF(node, fmt, ap, emitting_delay_slot) {
		bool plus = false;
		if (*fmt == '+') {
			plus = true;
			++fmt;
		}

		switch (*fmt++) {
		case 'A': {
			const sparc_jmp_cond_attr_t *attr
				= get_sparc_jmp_cond_attr_const(node);
			if (attr->annul_delay_slot) {
				be_emit_cstring(",a");
			}
			break;
		}

		case 'D':
			if (!is_digit(*fmt))
				goto unknown;
			sparc_emit_dest_register(node, *fmt++ - '0');
			break;

		case 'E': {
			sparc_attr_t const *const attr = get_sparc_attr_const(node);
			be_gas_emit_entity(attr->immediate_value_entity);
			if (attr->immediate_value != 0) {
				be_emit_irprintf(plus ? "%+"PRId32 : "%"PRId32, attr->immediate_value);
			}
			break;
		}

		case 'F': {
			ir_mode *mode;
			switch (*fmt++) {
			case 'D': mode = get_sparc_fp_conv_attr_const(node)->dest_mode; break;
			case 'M': mode = get_sparc_fp_attr_const(node)->fp_mode;        break;
			case 'S': mode = get_sparc_fp_conv_attr_const(node)->src_mode;  break;
			default:  goto unknown;
			}
			emit_fp_suffix(mode);
			break;
		}

		case 'H':
			sparc_emit_high_immediate(node);
			break;

		case 'M':
			switch (*fmt++) {
			case 'L': sparc_emit_load_mode(node);  break;
			case 'S': sparc_emit_store_mode(node); break;
			default:  goto unknown;
			}
			break;

		case 'O': {
			if (!is_digit(*fmt))
				goto unknown;
			unsigned const pos = *fmt++ - '0';
			if (!plus)
				be_emit_char('[');
			sparc_emit_source_register(node, pos);
			sparc_emit_offset(node, pos + 1);
			if (!plus)
				be_emit_char(']');
			break;
		}

		case 'R': {
			arch_register_t const *const reg = va_arg(ap, const arch_register_t*);
			be_emit_char('%');
			be_emit_string(reg->name);
			break;
		}

		case 'S': {
			bool imm = false;
			if (*fmt == 'I') {
				imm = true;
				++fmt;
			}
			if (!is_digit(*fmt))
				goto unknown;
			unsigned const pos = *fmt++ - '0';
			if (imm && arch_get_irn_flags(node) & (arch_irn_flags_t)sparc_arch_irn_flag_immediate_form) {
				const sparc_attr_t *const attr = get_sparc_attr_const(node);
				sparc_emit_immediate(attr->immediate_value,
				                     attr->immediate_value_entity);
			} else {
				sparc_emit_source_register(node, pos);
			}
			break;
		}

		case 'd': {
			int const num = va_arg(ap, int);
			be_emit_irprintf(plus ? "%+d" : "%d", num);
			break;
		}

		case 'X': {
			unsigned const num = va_arg(ap, unsigned);
			be_emit_irprintf("%X", num);
			break;
		}

		default:
unknown:
			panic("unknown format conversion in sparc_emitf()");
		}
	}
}

/**
 * Emits code for stack space management
 */
static void emit_be_IncSP(const ir_node *irn)
{
	int offset = be_get_IncSP_offset(irn);

	if (offset == 0)
		return;

	/* SPARC stack grows downwards */
	char const *const insn = offset > 0 ? offset = -offset, "add" : "sub";
	sparc_emitf(irn, "%s %S0, %d, %D0", insn, offset);
}

static void emit_sparc_asm_operand(ir_node const *const node, char const modifier, unsigned const pos)
{
	be_asm_attr_t       const *const attr = get_be_asm_attr_const(node);
	sparc_asm_operand_t const *const op   = &((sparc_asm_operand_t const*)attr->operands)[pos];
	/* modifiers:
	 *   c: plain immediate
	 *   f: memory reference without surrounding '[]'
	 *   l: label only
	 *   m: see 'f'
	 *   r: immediate, but show '%g0' instead of '0' */
	if (!be_is_valid_asm_operand_kind(node, modifier, pos, op->op.kind, "r", "c", "fm"))
		return;

	switch (op->op.kind) {
	case BE_ASM_OPERAND_INVALID:
		panic("invalid asm operand");

	case BE_ASM_OPERAND_IMMEDIATE: {
		bool const zero_as_g0 = modifier == 'r';
		if (zero_as_g0 && op->immediate_value == 0 && !op->immediate_value_entity)
			sparc_emit_register(&sparc_registers[REG_G0]);
		else
			sparc_emit_immediate(op->immediate_value, op->immediate_value_entity);
		return;
	}

	case BE_ASM_OPERAND_INPUT_VALUE:
		sparc_emit_register(arch_get_irn_register_in(node, op->op.pos));
		return;

	case BE_ASM_OPERAND_LABEL:
		be_emit_cfop_target_pos(node, op->op.pos);
		return;

	case BE_ASM_OPERAND_OUTPUT_VALUE:
		sparc_emit_register(arch_get_irn_register_out(node, op->op.pos));
		return;

	case BE_ASM_OPERAND_MEMORY: {
		bool const address_only = modifier == 'f' || modifier == 'm';
		if (!address_only)
			be_emit_char('[');
		sparc_emit_register(arch_get_irn_register_in(node, op->op.pos));
		if (!address_only)
			be_emit_char(']');
		return;
	}
	}
	panic("invalid asm operand kind");
}

static void emit_jmp(ir_node const *const node, ir_node const *const target)
{
	BE_EMIT_JMP(sparc, node, "ba", target) {
		/* TODO: fill this slot as well */
		emitting_delay_slot = true;
		sparc_emitf(NULL, "nop");
		emitting_delay_slot = false;
	}
}

static void emit_sparc_ASM(const ir_node *node)
{
	ir_node const *const fallthrough = be_emit_asm(node, emit_sparc_asm_operand);
	if (fallthrough)
		emit_jmp(node, fallthrough);
}

/**
 * Emits code for stack space management.
 */
static void emit_sparc_SubSP(const ir_node *irn)
{
	sparc_emitf(irn, "sub %S1, %SI2, %D0");
	sparc_emitf(irn, "add %S1, %u, %D1", SPARC_MIN_STACKSIZE);
}

static void fill_delay_slot(const ir_node *node)
{
	emitting_delay_slot = true;
	const ir_node *filler = pmap_get(ir_node, delay_slots, node);
	if (filler != NULL) {
		assert(!is_no_instruction(filler));
		assert(!emits_multiple_instructions(filler));
		be_emit_node(filler);
	} else {
		sparc_emitf(NULL, "nop");
	}
	emitting_delay_slot = false;
}

static void emit_sparc_Div(const ir_node *node, char const *const insn)
{
	sparc_emitf(node, "wr %S1, 0, %%y");

	/* TODO: we should specify number of delayslots in an architecture
	 * specification */
	unsigned wry_delay_count = 3;
	for (unsigned i = 0; i < wry_delay_count; ++i) {
		if (i == 0) {
			fill_delay_slot(node);
		} else {
			emitting_delay_slot = true;
			sparc_emitf(NULL, "nop");
			emitting_delay_slot = false;
		}
	}

	sparc_emitf(node, "%s %S2, %SI3, %D0", insn);
}

static void emit_sparc_SDiv(const ir_node *node)
{
	emit_sparc_Div(node, "sdiv");
}

static void emit_sparc_UDiv(const ir_node *node)
{
	emit_sparc_Div(node, "udiv");
}

static void emit_sparc_Call(const ir_node *node)
{
	if (is_sparc_reg_call(node)) {
		int dest_addr = get_sparc_Call_dest_addr_pos(node);
		sparc_emitf(node, "call %R", arch_get_irn_register_in(node, dest_addr));
	} else {
		sparc_emitf(node, "call %E, 0");
	}

	fill_delay_slot(node);

	if (arch_get_irn_flags(node) & sparc_arch_irn_flag_aggregate_return) {
		sparc_emitf(NULL, "unimp 8");
	}
}

static void emit_sparc_Cas(const ir_node *node)
{
	/* except for some patched gaisler binutils nobody understands cas
	 * in v8/leon mode, so we encode the cas in binary form */
#if 0
	sparc_emitf(node, "cas [%S0], %S1, %S2");
#else
	const arch_register_t *reg_new
		= arch_get_irn_register_in(node, n_sparc_Cas_new);
	const arch_register_t *reg_ptr
		= arch_get_irn_register_in(node, n_sparc_Cas_ptr);
	const arch_register_t *reg_old
		= arch_get_irn_register_in(node, n_sparc_Cas_old);
	uint32_t cas_asi  = sparc_cg_config.cas_asi;
	uint32_t encoding = 3u<<30 | (reg_new->encoding<<25) | (0x3C << 19)
	       | (reg_ptr->encoding<<14) | (cas_asi<<5) | (reg_old->encoding);
	sparc_emitf(node, ".long 0x%X  /* cas [%S0], %S1, %S2", encoding);
#endif
}

static void emit_be_Perm(const ir_node *irn)
{
	arch_register_req_t const *const req = arch_get_irn_register_req_out(irn, 0);
	if (req->cls == &sparc_reg_classes[CLASS_sparc_fp]) {
		arch_register_t const *r0 = arch_get_irn_register_out(irn, 0);
		arch_register_t const *r1 = arch_get_irn_register_out(irn, 1);
		for (unsigned i = 0, width = req->width; i < width; ++r0, ++r1, ++i) {
			sparc_emitf(irn, "fmovs %R, %%f31", r0);
			sparc_emitf(irn, "fmovs %R, %R", r1, r0);
			sparc_emitf(irn, "fmovs %%f31, %R", r1);
		}
	} else if (req->cls == &sparc_reg_classes[CLASS_sparc_gp]) {
		sparc_emitf(irn, "xor %D1, %D0, %D0");
		sparc_emitf(irn, "xor %D1, %D0, %D1");
		sparc_emitf(irn, "xor %D1, %D0, %D0");
	} else {
		panic("unexpected register class");
	}
}

static unsigned get_aligned_sp_change(const unsigned num_regs)
{
	const unsigned bytes = num_regs * SPARC_REGISTER_SIZE;
	return round_up2(bytes, SPARC_STACK_ALIGNMENT);
}

/* Spill register l0 or both l0 and l1, depending on n_spilled and n_to_spill.*/
static void memperm_emit_spill_registers(const ir_node *node, int n_spilled,
                                         int n_to_spill)
{
	assert(n_spilled < n_to_spill);

	if (n_spilled == 0) {
		/* We always reserve stack space for two registers because during copy
		 * processing we don't know yet if we also need to handle a cycle which
		 * needs two registers.  More complicated code in emit_MemPerm would
		 * prevent wasting SPARC_REGISTER_SIZE bytes of stack space but
		 * it is not worth the worse readability of emit_MemPerm. */

		const unsigned sp_change = get_aligned_sp_change(2);
		sparc_emitf(node, "sub %%sp, %u, %%sp", sp_change);
		sparc_emitf(node, "st %%l0, [%%sp%+d]", SPARC_MIN_STACKSIZE);
	}

	if (n_to_spill == 2) {
		sparc_emitf(node, "st %%l1, [%%sp%+d]", SPARC_MIN_STACKSIZE + SPARC_REGISTER_SIZE);
	}
}

/* Restore register l0 or both l0 and l1, depending on n_spilled. */
static void memperm_emit_restore_registers(const ir_node *node, int n_spilled)
{
	if (n_spilled == 2) {
		sparc_emitf(node, "ld [%%sp%+d], %%l1", SPARC_MIN_STACKSIZE + SPARC_REGISTER_SIZE);
	}

	sparc_emitf(node, "ld [%%sp%+d], %%l0", SPARC_MIN_STACKSIZE);
	const unsigned sp_change = get_aligned_sp_change(2);
	sparc_emitf(node, "add %%sp, %u, %%sp", sp_change);
}

static void memperm_emit_copy(const ir_node *node, ir_entity *in_ent,
                              ir_entity *out_ent, int ent_offset)
{
	ir_graph   *irg     = get_irn_irg(node);
	const char *reg     = sparc_get_irg_data(irg)->omit_fp ? "sp" : "fp";
	const int   off_in  = get_entity_offset(in_ent) + ent_offset;
	const int   off_out = get_entity_offset(out_ent) + ent_offset;

	sparc_emitf(node, "ld [%%%s%+d], %%l0", reg, off_in);
	sparc_emitf(node, "st %%l0, [%%%s%+d]", reg, off_out);
}

static void memperm_emit_swap(const ir_node *node, ir_entity *ent1,
                              ir_entity *ent2, int ent_offset)
{
	ir_graph   *irg  = get_irn_irg(node);
	const char *reg  = sparc_get_irg_data(irg)->omit_fp ? "sp" : "fp";
	const int   off1 = get_entity_offset(ent1) + ent_offset;
	const int   off2 = get_entity_offset(ent2) + ent_offset;

	sparc_emitf(node, "ld [%%%s%+d], %%l0", reg, off1);
	sparc_emitf(node, "ld [%%%s%+d], %%l1", reg, off2);
	sparc_emitf(node, "st %%l0, [%%%s%+d]", reg, off2);
	sparc_emitf(node, "st %%l1, [%%%s%+d]", reg, off1);
}

static int get_index(ir_entity **ents, int n, ir_entity *ent)
{
	for (int i = 0; i < n; ++i) {
		if (ents[i] == ent)
			return i;
	}

	return -1;
}

/*
 * Emit code for a MemPerm node.
 *
 * Analyze MemPerm for copy chains and cyclic swaps and resolve them using
 * loads and stores.
 * This function is conceptually very similar to permute_values in
 * beprefalloc.c.
 */
static void emit_be_MemPerm(const ir_node *node)
{
	int         memperm_arity = be_get_MemPerm_entity_arity(node);
	/* Upper limit for the number of participating entities is twice the
	 * arity, e.g., for a simple copying MemPerm node with one input/output. */
	int         max_size      = 2 * memperm_arity;
	ir_entity **entities      = ALLOCANZ(ir_entity *, max_size);
	/* sourceof contains the input entity for each entity.  If an entity is
	 * never used as an output, its entry in sourceof is a fix point. */
	int        *sourceof      = ALLOCANZ(int,         max_size);
	/* n_users counts how many output entities use this entity as their input.*/
	int        *n_users       = ALLOCANZ(int,         max_size);
	/* n_spilled records the number of spilled registers, either 1 or 2. */
	int         n_spilled     = 0;
	ir_graph   *irg           = get_irn_irg(node);
	bool        omit_fp       = sparc_get_irg_data(irg)->omit_fp;

	int ent_offset = be_get_MemPerm_offset(node);

	for (int i = 0; i < max_size; ++i) {
		sourceof[i] = i;
	}

	int n = 0;
	for (int i = 0; i < memperm_arity; ++i) {
		ir_entity *out  = be_get_MemPerm_out_entity(node, i);
		ir_entity *in   = be_get_MemPerm_in_entity(node, i);

		/* Insert into entities to be able to operate on unique indices. */
		if (get_index(entities, n, out) == -1)
			entities[n++] = out;
		if (get_index(entities, n, in) == -1)
			entities[n++] = in;

		int oidx = get_index(entities, n, out);
		int iidx = get_index(entities, n, in);

		sourceof[oidx] = iidx;
		++n_users[iidx];
	}

	/* First do all the copies. */
	for (int oidx = 0; oidx < n; /* empty */) {
		int iidx = sourceof[oidx];

		/* Nothing to do for fix points.
		 * Also, if entities[oidx] is used as an input by another copy, we
		 * can't overwrite entities[oidx] yet.*/
		if (iidx == oidx || n_users[oidx] > 0) {
			++oidx;
			continue;
		}

		/* We found the end of a 'chain', so do the copy. */
		if (n_spilled == 0) {
			memperm_emit_spill_registers(node, n_spilled, /*n_to_spill=*/1);
			n_spilled = 1;
			if (omit_fp)
				ent_offset += 8;
		}
		memperm_emit_copy(node, entities[iidx], entities[oidx], ent_offset);

		/* Mark as done. */
		sourceof[oidx] = oidx;

		assert(n_users[iidx] > 0);
		--n_users[iidx];

		if (iidx < oidx && n_users[iidx] == 0) {
			oidx = iidx;
		} else {
			++oidx;
		}
	}

	/* The rest are cycles. */
	for (int oidx = 0; oidx < n; /* empty */) {
		int iidx = sourceof[oidx];

		if (iidx == oidx) {
			++oidx;
			continue;
		}

		assert(n_users[iidx] == 1);

		if (n_spilled < 2) {
			memperm_emit_spill_registers(node, n_spilled, /*n_to_spill=*/2);
			n_spilled = 2;
		}
		memperm_emit_swap(node, entities[iidx], entities[oidx], ent_offset);

		int tidx = sourceof[iidx];
		sourceof[iidx] = iidx; /* Mark as done. */

		/* The source of oidx is now the old source of iidx, because we swapped
		 * the two entities. */
		sourceof[oidx] = tidx;
	}

#ifdef DEBUG_libfirm
	/* Only fix points should remain. */
	for (int i = 0; i < max_size; ++i) {
		assert(sourceof[i] == i);
	}
#endif

	assert(n_spilled > 0 && "Useless MemPerm node");
	memperm_emit_restore_registers(node, n_spilled);
}

static void emit_sparc_Return(const ir_node *node)
{
	ir_graph  *irg    = get_irn_irg(node);
	ir_entity *entity = get_irg_entity(irg);
	ir_type   *type   = get_entity_type(entity);

	/* hack: we don't explicitly model register changes because of the
	 * restore node. So we have to do it manually here */
	ir_node const *const delay_slot = pmap_get(ir_node, delay_slots, node);
	char    const *const destreg    = delay_slot && is_restore(delay_slot) ? "%i7" : "%o7";
	char    const *const offset     = get_method_calling_convention(type) & cc_compound_ret ? "12" : "8";
	sparc_emitf(node, "jmp %s+%s", destreg, offset);
	fill_delay_slot(node);
}

static const arch_register_t *map_i_to_o_reg(const arch_register_t *reg)
{
	unsigned idx = reg->global_index;
	if (idx < REG_I0 || idx > REG_I7)
		return reg;
	idx += REG_O0 - REG_I0;
	assert(REG_O0 <= idx && idx <= REG_O7);
	return &sparc_registers[idx];
}

static void emit_sparc_Restore(const ir_node *node)
{
	const arch_register_t *destreg
		= arch_get_irn_register_out(node, pn_sparc_Restore_res);
	sparc_emitf(node, "restore %S2, %SI3, %R", map_i_to_o_reg(destreg));
}

static void emit_sparc_FrameAddr(const ir_node *node)
{
	const sparc_attr_t *attr   = get_sparc_attr_const(node);
	int32_t             offset = attr->immediate_value;

	char const *const insn = offset > 0 ? offset = -offset, "sub" : "add";
	assert(sparc_is_value_imm_encodeable(offset));
	sparc_emitf(node, "%s %S0, %d, %D0", insn, (int)offset);
}

static const char *get_icc_unsigned(ir_relation relation)
{
	switch (relation & (ir_relation_less_equal_greater)) {
	case ir_relation_false:              return "bn";
	case ir_relation_equal:              return "be";
	case ir_relation_less:               return "blu";
	case ir_relation_less_equal:         return "bleu";
	case ir_relation_greater:            return "bgu";
	case ir_relation_greater_equal:      return "bgeu";
	case ir_relation_less_greater:       return "bne";
	case ir_relation_less_equal_greater: return "ba";
	default: panic("Cmp has unsupported relation");
	}
}

static const char *get_icc_signed(ir_relation relation)
{
	switch (relation & (ir_relation_less_equal_greater)) {
	case ir_relation_false:              return "bn";
	case ir_relation_equal:              return "be";
	case ir_relation_less:               return "bl";
	case ir_relation_less_equal:         return "ble";
	case ir_relation_greater:            return "bg";
	case ir_relation_greater_equal:      return "bge";
	case ir_relation_less_greater:       return "bne";
	case ir_relation_less_equal_greater: return "ba";
	default: panic("Cmp has unsupported relation");
	}
}

static const char *get_fcc(ir_relation relation)
{
	switch (relation) {
	case ir_relation_false:                   return "fbn";
	case ir_relation_equal:                   return "fbe";
	case ir_relation_less:                    return "fbl";
	case ir_relation_less_equal:              return "fble";
	case ir_relation_greater:                 return "fbg";
	case ir_relation_greater_equal:           return "fbge";
	case ir_relation_less_greater:            return "fblg";
	case ir_relation_less_equal_greater:      return "fbo";
	case ir_relation_unordered:               return "fbu";
	case ir_relation_unordered_equal:         return "fbue";
	case ir_relation_unordered_less:          return "fbul";
	case ir_relation_unordered_less_equal:    return "fbule";
	case ir_relation_unordered_greater:       return "fbug";
	case ir_relation_unordered_greater_equal: return "fbuge";
	case ir_relation_unordered_less_greater:  return "fbne";
	case ir_relation_true:                    return "fba";
	}
	panic("invalid relation");
}

typedef const char* (*get_cc_func)(ir_relation relation);

static void emit_sparc_branch(const ir_node *node, get_cc_func get_cc)
{
	const sparc_jmp_cond_attr_t *attr = get_sparc_jmp_cond_attr_const(node);
	ir_relation    relation    = attr->relation;

	be_cond_branch_projs_t const projs = be_get_cond_branch_projs(node);

	/* emit the true proj */
	sparc_emitf(node, "%s%A %L", get_cc(relation), projs.t);
	fill_delay_slot(node);

	emit_jmp(node, projs.f);
}

static void emit_sparc_Bicc(const ir_node *node)
{
	const sparc_jmp_cond_attr_t *attr = get_sparc_jmp_cond_attr_const(node);
	bool             is_unsigned = attr->is_unsigned;
	emit_sparc_branch(node, is_unsigned ? get_icc_unsigned : get_icc_signed);
}

static void emit_sparc_fbfcc(const ir_node *node)
{
	/* if the flags producing node was immediately in front of us, emit
	 * a nop */
	ir_node *flags = get_irn_n(node, n_sparc_fbfcc_flags);
	ir_node *prev  = sched_prev(node);
	if (is_Block(prev)) {
		/* TODO: when the flags come from another block, then we have to do
		 * more complicated tests to see whether the flag producing node is
		 * potentially in front of us (could happen for fallthroughs) */
		panic("TODO: fbfcc flags come from other block");
	}
	if (skip_Proj(flags) == prev) {
		sparc_emitf(NULL, "nop");
	}
	emit_sparc_branch(node, get_fcc);
}

static void emit_sparc_Ba(const ir_node *node)
{
	BE_EMIT_JMP(sparc, node, "ba", node) {
		fill_delay_slot(node);
	}
}

static void emit_jumptable_target(ir_entity const *const table,
                                  ir_node const *const proj_x)
{
	(void)table;
	be_emit_cfop_target(proj_x);
}

static void emit_sparc_IJmp(ir_node const *const node)
{
	sparc_emitf(node, "jmp %+O0");
	fill_delay_slot(node);
}

static void emit_sparc_SwitchJmp(const ir_node *node)
{
	const sparc_switch_jmp_attr_t *attr = get_sparc_switch_jmp_attr_const(node);

	sparc_emitf(node, "jmp %S0");
	fill_delay_slot(node);

	be_emit_jump_table(node, &attr->swtch, mode_P, emit_jumptable_target);
}

static void emit_fmov(const ir_node *node, const arch_register_t *src_reg,
                      const arch_register_t *dst_reg)
{
	sparc_emitf(node, "fmovs %R, %R", src_reg, dst_reg);
}

static const arch_register_t *get_next_fp_reg(const arch_register_t *reg)
{
	unsigned idx = reg->global_index;
	assert(reg == &sparc_registers[idx]);
	idx++;
	assert(idx - REG_F0 < N_sparc_fp_REGS);
	return &sparc_registers[idx];
}

static void emit_be_Copy(const ir_node *node)
{
	const arch_register_t *src_reg = arch_get_irn_register_in(node, 0);
	const arch_register_t *dst_reg = arch_get_irn_register_out(node, 0);
	if (src_reg == dst_reg)
		return;

	arch_register_class_t const *const cls = dst_reg->cls;
	if (cls == &sparc_reg_classes[CLASS_sparc_gp]) {
		sparc_emitf(node, "mov %S0, %D0");
	} else if (cls == &sparc_reg_classes[CLASS_sparc_fp]) {
		ir_mode *mode = get_irn_mode(node);
		unsigned bits = get_mode_size_bits(mode);
		int      n    = bits > 32 ? bits > 64 ? 3 : 1 : 0;
		emit_fmov(node, src_reg, dst_reg);
		for (int i = 0; i < n; ++i) {
			src_reg = get_next_fp_reg(src_reg);
			dst_reg = get_next_fp_reg(dst_reg);
			emit_fmov(node, src_reg, dst_reg);
		}
	} else {
		panic("invalid register class");
	}
}

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void sparc_register_emitters(void)
{
	be_init_emitters();

	/* register all emitter functions defined in spec */
	sparc_register_spec_emitters();

	/* custom emitter */
	be_set_emitter(op_be_Asm,          emit_sparc_ASM);
	be_set_emitter(op_be_Copy,         emit_be_Copy);
	be_set_emitter(op_be_CopyKeep,     emit_be_Copy);
	be_set_emitter(op_be_IncSP,        emit_be_IncSP);
	be_set_emitter(op_be_MemPerm,      emit_be_MemPerm);
	be_set_emitter(op_be_Perm,         emit_be_Perm);
	be_set_emitter(op_sparc_Ba,        emit_sparc_Ba);
	be_set_emitter(op_sparc_Bicc,      emit_sparc_Bicc);
	be_set_emitter(op_sparc_Call,      emit_sparc_Call);
	be_set_emitter(op_sparc_Cas,       emit_sparc_Cas);
	be_set_emitter(op_sparc_FrameAddr, emit_sparc_FrameAddr);
	be_set_emitter(op_sparc_IJmp,      emit_sparc_IJmp);
	be_set_emitter(op_sparc_Restore,   emit_sparc_Restore);
	be_set_emitter(op_sparc_Return,    emit_sparc_Return);
	be_set_emitter(op_sparc_SDiv,      emit_sparc_SDiv);
	be_set_emitter(op_sparc_SubSP,     emit_sparc_SubSP);
	be_set_emitter(op_sparc_SwitchJmp, emit_sparc_SwitchJmp);
	be_set_emitter(op_sparc_UDiv,      emit_sparc_UDiv);
	be_set_emitter(op_sparc_fbfcc,     emit_sparc_fbfcc);
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void sparc_emit_block(ir_node *block)
{
	be_gas_begin_block(block);

	sched_foreach(block, node) {
		if (rbitset_is_set(delay_slot_fillers, get_irn_idx(node)))
			continue;
		be_emit_node(node);
	}
}

/**
 * Emits code for function start.
 */
static void sparc_emit_func_prolog(ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);
	be_gas_emit_function_prolog(entity, 4, NULL);
}

/**
 * Emits code for function end
 */
static void sparc_emit_func_epilog(ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);
	be_gas_emit_function_epilog(entity);
}

static int cmp_block_execfreqs(const void *d1, const void *d2)
{
	ir_node **p1 = (ir_node**)d1;
	ir_node **p2 = (ir_node**)d2;
	double freq1 = get_block_execfreq(*p1);
	double freq2 = get_block_execfreq(*p2);
	if (freq1 < freq2)
		return -1;
	if (freq1 > freq2)
		return 1;
	return get_irn_node_nr(*p2)-get_irn_node_nr(*p1);
}

static void pick_delay_slots(size_t n_blocks, ir_node **blocks)
{
	/* create blocklist sorted by execution frequency */
	ir_node **sorted_blocks = XMALLOCN(ir_node*, n_blocks);
	MEMCPY(sorted_blocks, blocks, n_blocks);
	QSORT(sorted_blocks, n_blocks, cmp_block_execfreqs);

	for (size_t i = 0; i < n_blocks; ++i) {
		sched_foreach(sorted_blocks[i], node) {
			if (!has_delay_slot(node))
				continue;
			ir_node *filler = pick_delay_slot_for(node);
			if (filler == NULL)
				continue;
			rbitset_set(delay_slot_fillers, get_irn_idx(filler));
			pmap_insert(delay_slots, node, filler);
		}
	}
	free(sorted_blocks);
}

void sparc_emit_function(ir_graph *irg)
{
	heights            = heights_new(irg);
	delay_slot_fillers = rbitset_malloc(get_irg_last_idx(irg));
	delay_slots        = pmap_create();

	/* register all emitter functions */
	sparc_register_emitters();

	/* create the block schedule. For now, we don't need it earlier. */
	ir_node **block_schedule = be_create_block_schedule(irg);

	sparc_emit_func_prolog(irg);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	be_emit_init_cf_links(block_schedule);

	size_t n_blocks = ARR_LEN(block_schedule);
	pick_delay_slots(n_blocks, block_schedule);

	for (size_t i = 0; i < n_blocks; ++i) {
		sparc_emit_block(block_schedule[i]);
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	/* emit function epilog */
	sparc_emit_func_epilog(irg);

	pmap_destroy(delay_slots);
	free(delay_slot_fillers);
	heights_free(heights);
}

void sparc_init_emitter(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.sparc.emit");
}
