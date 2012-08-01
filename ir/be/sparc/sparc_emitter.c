/*
 * Copyright (C) 1995-2010 University of Karlsruhe.  All right reserved.
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
 * @brief   emit assembler for a backend graph
 * @author  Hannes Rapp, Matthias Braun
 */
#include "config.h"

#include <limits.h>

#include "bitfiddle.h"
#include "xmalloc.h"
#include "tv.h"
#include "iredges.h"
#include "debug.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irop_t.h"
#include "irargs_t.h"
#include "irprog.h"
#include "irargs_t.h"
#include "error.h"
#include "raw_bitset.h"
#include "dbginfo.h"
#include "heights.h"

#include "besched.h"
#include "beblocksched.h"
#include "beirg.h"
#include "begnuas.h"
#include "bedwarf.h"
#include "benode.h"
#include "bestack.h"
#include "bepeephole.h"

#include "sparc_emitter.h"
#include "gen_sparc_emitter.h"
#include "sparc_nodes_attr.h"
#include "sparc_new_nodes.h"
#include "gen_sparc_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static ir_heights_t  *heights;
static const ir_node *delay_slot_filler; /**< this node has been choosen to fill
                                              the next delay slot */

static void sparc_emit_node(const ir_node *node);
static bool emitting_delay_slot;

/**
 * indent before instruction. (Adds additional indentation when emitting
 * delay slots)
 */
static void sparc_emit_indent(void)
{
	be_emit_char('\t');
	if (emitting_delay_slot)
		be_emit_char(' ');
}

static void sparc_emit_immediate(ir_node const *const node)
{
	const sparc_attr_t *attr   = get_sparc_attr_const(node);
	ir_entity          *entity = attr->immediate_value_entity;

	if (entity == NULL) {
		int32_t value = attr->immediate_value;
		assert(sparc_is_value_imm_encodeable(value));
		be_emit_irprintf("%d", value);
	} else {
		if (get_entity_owner(entity) == get_tls_type()) {
			be_emit_cstring("%tle_lox10(");
		} else {
			be_emit_cstring("%lo(");
		}
		be_gas_emit_entity(entity);
		if (attr->immediate_value != 0) {
			be_emit_irprintf("%+d", attr->immediate_value);
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
		if (get_entity_owner(entity) == get_tls_type()) {
			be_emit_cstring("%tle_hix22(");
		} else {
			be_emit_cstring("%hi(");
		}
		be_gas_emit_entity(entity);
		if (attr->immediate_value != 0) {
			be_emit_irprintf("%+d", attr->immediate_value);
		}
		be_emit_char(')');
	}
}

static void sparc_emit_source_register(ir_node const *node, int const pos)
{
	const arch_register_t *reg = arch_get_irn_register_in(node, pos);
	be_emit_char('%');
	be_emit_string(arch_register_get_name(reg));
}

static void sparc_emit_dest_register(ir_node const *const node, int const pos)
{
	const arch_register_t *reg = arch_get_irn_register_out(node, pos);
	be_emit_char('%');
	be_emit_string(arch_register_get_name(reg));
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
			be_emit_irprintf("%+ld", offset);
		}
	} else if (attr->base.immediate_value != 0
			|| attr->base.immediate_value_entity != NULL) {
		be_emit_char('+');
		sparc_emit_immediate(node);
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

static ir_node *get_jump_target(const ir_node *jump)
{
	return (ir_node*)get_irn_link(jump);
}

/**
 * Returns the target label for a control flow node.
 */
static void sparc_emit_cfop_target(const ir_node *node)
{
	ir_node *block = get_jump_target(node);
	be_gas_emit_block_name(block);
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

static bool ba_is_fallthrough(const ir_node *node)
{
	ir_node *block      = get_nodes_block(node);
	ir_node *next_block = (ir_node*)get_irn_link(block);
	return get_irn_link(node) == next_block;
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
	if (is_sparc_Ba(node) && ba_is_fallthrough(node))
		return true;

	return be_is_Keep(node) || be_is_Start(node) || is_Phi(node);
}

static bool has_delay_slot(const ir_node *node)
{
	if (is_sparc_Ba(node)) {
		return !ba_is_fallthrough(node);
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

	if (is_sparc_Call(node)) {
		return arch_get_irn_flags(node) & sparc_arch_irn_flag_aggregate_return;
	}

	return is_sparc_SMulh(node) || is_sparc_UMulh(node)
		|| is_sparc_SDiv(node) || is_sparc_UDiv(node)
		|| be_is_MemPerm(node) || be_is_Perm(node)
		|| is_sparc_SubSP(node);
}

static bool uses_reg(const ir_node *node, const arch_register_t *reg)
{
	int arity = get_irn_arity(node);
	int i;

	for (i = 0; i < arity; ++i) {
		const arch_register_t *in_reg = arch_get_irn_register_in(node, i);
		if (reg == in_reg)
			return true;
	}
	return false;
}

static bool writes_reg(const ir_node *node, const arch_register_t *reg)
{
	unsigned n_outs = arch_get_irn_n_outs(node);
	unsigned o;
	for (o = 0; o < n_outs; ++o) {
		const arch_register_t *out_reg = arch_get_irn_register_out(node, o);
		if (out_reg == reg)
			return true;
	}
	return false;
}

static bool can_move_into_delayslot(const ir_node *node, const ir_node *to)
{
	if (!be_can_move_before(heights, node, to))
		return false;

	if (is_sparc_Call(to)) {
		ir_node *check;
		/** all deps are used after the delay slot so, we're fine */
		if (!is_sparc_reg_call(to))
			return true;

		check = get_irn_n(to, get_sparc_Call_dest_addr_pos(to));
		if (skip_Proj(check) == node)
			return false;

		/* the Call also destroys the value of %o7, but since this is
		 * currently marked as ignore register in the backend, it
		 * should never be used by the instruction in the delay slot. */
		if (uses_reg(node, &sparc_registers[REG_O7]))
			return false;
		return true;
	} else if (is_sparc_Return(to)) {
		/* return uses the value of %o7, all other values are not
		 * immediately used */
		if (writes_reg(node, &sparc_registers[REG_O7]))
			return false;
		return true;
	} else {
		/* the node must not use our computed values */
		int arity = get_irn_arity(to);
		int i;
		for (i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(to, i);
			if (skip_Proj(in) == node)
				return false;
		}
		return true;
	}
}

/**
 * search for an instruction that can fill the delay slot of @p node
 */
static const ir_node *pick_delay_slot_for(const ir_node *node)
{
	const ir_node *schedpoint = node;
	unsigned       tries      = 0;
	/* currently we don't track which registers are still alive, so we can't
	 * pick any other instructions other than the one directly preceding */
	static const unsigned PICK_DELAY_SLOT_MAX_DISTANCE = 10;

	assert(has_delay_slot(node));

	while (sched_has_prev(schedpoint)) {
		schedpoint = sched_prev(schedpoint);

		if (has_delay_slot(schedpoint))
			break;

		/* skip things which don't really result in instructions */
		if (is_no_instruction(schedpoint))
			continue;

		if (tries++ >= PICK_DELAY_SLOT_MAX_DISTANCE)
			break;

		if (emits_multiple_instructions(schedpoint))
			continue;

		if (!can_move_into_delayslot(schedpoint, node))
			continue;

		/* found something */
		return schedpoint;
	}

	return NULL;
}

void sparc_emitf(ir_node const *const node, char const *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	sparc_emit_indent();
	for (;;) {
		char const *start = fmt;

		while (*fmt != '%' && *fmt != '\0')
			++fmt;
		be_emit_string_len(start, fmt - start);
		if (*fmt == '\0')
			break;
		++fmt;

		bool plus = false;
		if (*fmt == '+') {
			plus = true;
			++fmt;
		}

		switch (*fmt++) {
		case '%':
			be_emit_char('%');
			break;

		case 'D':
			if (*fmt < '0' || '9' <= *fmt)
				goto unknown;
			sparc_emit_dest_register(node, *fmt++ - '0');
			break;

		case 'E': {
			sparc_attr_t const *const attr = get_sparc_attr_const(node);
			be_gas_emit_entity(attr->immediate_value_entity);
			if (attr->immediate_value != 0) {
				be_emit_irprintf(plus ? "%+d" : "%d", attr->immediate_value);
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

		case 'L':
			sparc_emit_cfop_target(node);
			break;

		case 'M':
			switch (*fmt++) {
			case 'L': sparc_emit_load_mode(node);  break;
			case 'S': sparc_emit_store_mode(node); break;
			default:  goto unknown;
			}
			break;

		case 'O':
			if (*fmt < '0' || '9' <= *fmt)
				goto unknown;
			sparc_emit_offset(node, *fmt++ - '0');
			break;

		case 'R': {
			arch_register_t const *const reg = va_arg(ap, const arch_register_t*);
			be_emit_char('%');
			be_emit_string(arch_register_get_name(reg));
			break;
		}

		case 'S': {
			bool imm = false;
			if (*fmt == 'I') {
				imm = true;
				++fmt;
			}
			if (*fmt < '0' || '9' <= *fmt)
				goto unknown;
			unsigned const pos = *fmt++ - '0';
			if (imm && arch_get_irn_flags(node) & (arch_irn_flags_t)sparc_arch_irn_flag_immediate_form) {
				sparc_emit_immediate(node);
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

		case 's': {
			char const *const str = va_arg(ap, char const*);
			be_emit_string(str);
			break;
		}

		case 'u': {
			unsigned const num = va_arg(ap, unsigned);
			be_emit_irprintf(plus ? "%+u" : "%u", num);
			break;
		}

		default:
unknown:
			panic("unknown format conversion in sparc_emitf()");
		}
	}
	be_emit_finish_line_gas(node);
	va_end(ap);
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

/**
 * Emits code for stack space management.
 */
static void emit_sparc_SubSP(const ir_node *irn)
{
	sparc_emitf(irn, "sub %S0, %SI1, %D0");
	sparc_emitf(irn, "add %S0, %u, %D1", SPARC_MIN_STACKSIZE);
}

static void fill_delay_slot(void)
{
	emitting_delay_slot = true;
	if (delay_slot_filler != NULL) {
		sparc_emit_node(delay_slot_filler);
		delay_slot_filler = NULL;
	} else {
		sparc_emitf(NULL, "nop");
	}
	emitting_delay_slot = false;
}

static void emit_sparc_Div(const ir_node *node, char const *const insn)
{
	/* can we get the delay count of the wr instruction somewhere? */
	unsigned wry_delay_count = 3;
	unsigned i;

	sparc_emitf(node, "wr %S0, 0, %%y");

	for (i = 0; i < wry_delay_count; ++i) {
		fill_delay_slot();
	}

	sparc_emitf(node, "%s %S1, %SI2, %D0", insn);
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

	fill_delay_slot();

	if (arch_get_irn_flags(node) & sparc_arch_irn_flag_aggregate_return) {
		sparc_emitf(NULL, "unimp 8");
	}
}

static void emit_be_Perm(const ir_node *irn)
{
	sparc_emitf(irn, "xor %S1, %S0, %S0");
	sparc_emitf(irn, "xor %S1, %S0, %S1");
	sparc_emitf(irn, "xor %S1, %S0, %S0");
}

/* The stack pointer must always be SPARC_STACK_ALIGNMENT bytes aligned, so get
 * the next bigger integer that's evenly divisible by it. */
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

		/* Keep stack pointer aligned. */
		unsigned sp_change = get_aligned_sp_change(2);
		sparc_emitf(node, "sub %%sp, %u, %%sp", sp_change);

		/* Spill register l0. */
		sparc_emitf(node, "st %%l0, [%%sp%+d]", SPARC_MIN_STACKSIZE);
	}

	if (n_to_spill == 2) {
		/* Spill register l1. */
		sparc_emitf(node, "st %%l1, [%%sp%+d]", SPARC_MIN_STACKSIZE + SPARC_REGISTER_SIZE);
	}
}

/* Restore register l0 or both l0 and l1, depending on n_spilled. */
static void memperm_emit_restore_registers(const ir_node *node, int n_spilled)
{
	unsigned sp_change;

	if (n_spilled == 2) {
		/* Restore register l1. */
		sparc_emitf(node, "ld [%%sp%+d], %%l1", SPARC_MIN_STACKSIZE + SPARC_REGISTER_SIZE);
	}

	/* Restore register l0. */
	sparc_emitf(node, "ld [%%sp%+d], %%l0", SPARC_MIN_STACKSIZE);

	/* Restore stack pointer. */
	sp_change = get_aligned_sp_change(2);
	sparc_emitf(node, "add %%sp, %u, %%sp", sp_change);
}

/* Emit code to copy in_ent to out_ent.  Only uses l0. */
static void memperm_emit_copy(const ir_node *node, ir_entity *in_ent,
                              ir_entity *out_ent)
{
	ir_graph          *irg     = get_irn_irg(node);
	be_stack_layout_t *layout  = be_get_irg_stack_layout(irg);
	int                off_in  = be_get_stack_entity_offset(layout, in_ent, 0);
	int                off_out = be_get_stack_entity_offset(layout, out_ent, 0);

	/* Load from input entity. */
	sparc_emitf(node, "ld [%%fp%+d], %%l0", off_in);
	/* Store to output entity. */
	sparc_emitf(node, "st %%l0, [%%fp%+d]", off_out);
}

/* Emit code to swap ent1 and ent2.  Uses l0 and l1. */
static void memperm_emit_swap(const ir_node *node, ir_entity *ent1,
                              ir_entity *ent2)
{
	ir_graph          *irg     = get_irn_irg(node);
	be_stack_layout_t *layout  = be_get_irg_stack_layout(irg);
	int                off1    = be_get_stack_entity_offset(layout, ent1, 0);
	int                off2    = be_get_stack_entity_offset(layout, ent2, 0);

	/* Load from first input entity. */
	sparc_emitf(node, "ld [%%fp%+d], %%l0", off1);
	/* Load from second input entity. */
	sparc_emitf(node, "ld [%%fp%+d], %%l1", off2);
	/* Store first value to second output entity. */
	sparc_emitf(node, "st %%l0, [%%fp%+d]", off2);
	/* Store second value to first output entity. */
	sparc_emitf(node, "st %%l1, [%%fp%+d]", off1);
}

/* Find the index of ent in ents or return -1 if not found. */
static int get_index(ir_entity **ents, int n, ir_entity *ent)
{
	int i;

	for (i = 0; i < n; ++i)
		if (ents[i] == ent)
			return i;

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
	int         i, n, oidx;

	/* This implementation currently only works with frame pointers. */
	ir_graph          *irg    = get_irn_irg(node);
	be_stack_layout_t *layout = be_get_irg_stack_layout(irg);
	assert(!layout->sp_relative && "MemPerms currently do not work without frame pointers");

	for (i = 0; i < max_size; ++i) {
		sourceof[i] = i;
	}

	for (i = n = 0; i < memperm_arity; ++i) {
		ir_entity *out  = be_get_MemPerm_out_entity(node, i);
		ir_entity *in   = be_get_MemPerm_in_entity(node, i);
		int              oidx; /* Out index */
		int              iidx; /* In index */

		/* Insert into entities to be able to operate on unique indices. */
		if (get_index(entities, n, out) == -1)
			entities[n++] = out;
		if (get_index(entities, n, in) == -1)
			entities[n++] = in;

		oidx = get_index(entities, n, out);
		iidx = get_index(entities, n, in);

		sourceof[oidx] = iidx; /* Remember the source. */
		++n_users[iidx]; /* Increment number of users of this entity. */
	}

	/* First do all the copies. */
	for (oidx = 0; oidx < n; /* empty */) {
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
		}
		memperm_emit_copy(node, entities[iidx], entities[oidx]);

		/* Mark as done. */
		sourceof[oidx] = oidx;

		assert(n_users[iidx] > 0);
		/* Decrementing the number of users might enable us to do another
		 * copy. */
		--n_users[iidx];

		if (iidx < oidx && n_users[iidx] == 0) {
			oidx = iidx;
		} else {
			++oidx;
		}
	}

	/* The rest are cycles. */
	for (oidx = 0; oidx < n; /* empty */) {
		int iidx = sourceof[oidx];
		int tidx;

		/* Nothing to do for fix points. */
		if (iidx == oidx) {
			++oidx;
			continue;
		}

		assert(n_users[iidx] == 1);

		/* Swap the two values to resolve the cycle. */
		if (n_spilled < 2) {
			memperm_emit_spill_registers(node, n_spilled, /*n_to_spill=*/2);
			n_spilled = 2;
		}
		memperm_emit_swap(node, entities[iidx], entities[oidx]);

		tidx = sourceof[iidx];
		/* Mark as done. */
		sourceof[iidx] = iidx;

		/* The source of oidx is now the old source of iidx, because we swapped
		 * the two entities. */
		sourceof[oidx] = tidx;
	}

#ifdef DEBUG_libfirm
	/* Only fix points should remain. */
	for (i = 0; i < max_size; ++i) {
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

	const char *destreg = "%o7";

	/* hack: we don't explicitely model register changes because of the
	 * restore node. So we have to do it manually here */
	if (delay_slot_filler != NULL &&
			(is_sparc_Restore(delay_slot_filler)
			 || is_sparc_RestoreZero(delay_slot_filler))) {
		destreg = "%i7";
	}
	char const *const offset = get_method_calling_convention(type) & cc_compound_ret ? "12" : "8";
	sparc_emitf(node, "jmp %s+%s", destreg, offset);
	fill_delay_slot();
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
	sparc_emitf(node, "%s %S0, %d, %D0", insn, offset);
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
	const ir_node *proj_true   = NULL;
	const ir_node *proj_false  = NULL;
	const ir_node *block;
	const ir_node *next_block;

	foreach_out_edge(node, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		long nr = get_Proj_proj(proj);
		if (nr == pn_Cond_true) {
			proj_true = proj;
		} else {
			proj_false = proj;
		}
	}

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	/* we have a block schedule */
	next_block = (ir_node*)get_irn_link(block);

	if (get_irn_link(proj_true) == next_block) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj_true;

		proj_true  = proj_false;
		proj_false = t;
		relation   = get_negated_relation(relation);
	}

	/* emit the true proj */
	sparc_emitf(proj_true, "%s %L", get_cc(relation));
	fill_delay_slot();

	if (get_irn_link(proj_false) == next_block) {
		if (be_options.verbose_asm) {
			sparc_emitf(proj_false, "/* fallthrough to %L */");
		}
	} else {
		sparc_emitf(proj_false, "ba %L");
		fill_delay_slot();
	}
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
		 * more complicated tests to see wether the flag producing node is
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
	if (ba_is_fallthrough(node)) {
		if (be_options.verbose_asm) {
			sparc_emitf(node, "/* fallthrough to %L */");
		}
	} else {
		sparc_emitf(node, "ba %L");
		fill_delay_slot();
	}
}

static void emit_sparc_SwitchJmp(const ir_node *node)
{
	const sparc_switch_jmp_attr_t *attr = get_sparc_switch_jmp_attr_const(node);

	sparc_emitf(node, "jmp %S0");
	fill_delay_slot();

	be_emit_jump_table(node, attr->table, attr->table_entity, get_jump_target);
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
	ir_mode               *mode    = get_irn_mode(node);
	const arch_register_t *src_reg = arch_get_irn_register_in(node, 0);
	const arch_register_t *dst_reg = arch_get_irn_register_out(node, 0);

	if (src_reg == dst_reg)
		return;

	if (mode_is_float(mode)) {
		unsigned bits = get_mode_size_bits(mode);
		int      n    = bits > 32 ? bits > 64 ? 3 : 1 : 0;
		int      i;
		emit_fmov(node, src_reg, dst_reg);
		for (i = 0; i < n; ++i) {
			src_reg = get_next_fp_reg(src_reg);
			dst_reg = get_next_fp_reg(dst_reg);
			emit_fmov(node, src_reg, dst_reg);
		}
	} else if (mode_is_data(mode)) {
		sparc_emitf(node, "mov %S0, %D0");
	} else {
		panic("invalid mode");
	}
}

static void emit_nothing(const ir_node *irn)
{
	(void) irn;
}

typedef void (*emit_func) (const ir_node *);

static inline void set_emitter(ir_op *op, emit_func sparc_emit_node)
{
	op->ops.generic = (op_func)sparc_emit_node;
}

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void sparc_register_emitters(void)
{
	/* first clear the generic function pointer for all ops */
	ir_clear_opcodes_generic_func();
	/* register all emitter functions defined in spec */
	sparc_register_spec_emitters();

	/* custom emitter */
	set_emitter(op_be_Copy,         emit_be_Copy);
	set_emitter(op_be_CopyKeep,     emit_be_Copy);
	set_emitter(op_be_IncSP,        emit_be_IncSP);
	set_emitter(op_be_MemPerm,      emit_be_MemPerm);
	set_emitter(op_be_Perm,         emit_be_Perm);
	set_emitter(op_sparc_Ba,        emit_sparc_Ba);
	set_emitter(op_sparc_Bicc,      emit_sparc_Bicc);
	set_emitter(op_sparc_Call,      emit_sparc_Call);
	set_emitter(op_sparc_fbfcc,     emit_sparc_fbfcc);
	set_emitter(op_sparc_FrameAddr, emit_sparc_FrameAddr);
	set_emitter(op_sparc_SubSP,     emit_sparc_SubSP);
	set_emitter(op_sparc_Restore,   emit_sparc_Restore);
	set_emitter(op_sparc_Return,    emit_sparc_Return);
	set_emitter(op_sparc_SDiv,      emit_sparc_SDiv);
	set_emitter(op_sparc_SwitchJmp, emit_sparc_SwitchJmp);
	set_emitter(op_sparc_UDiv,      emit_sparc_UDiv);

	/* no need to emit anything for the following nodes */
	set_emitter(op_be_Keep,     emit_nothing);
	set_emitter(op_sparc_Start, emit_nothing);
	set_emitter(op_Phi,         emit_nothing);
}

/**
 * Emits code for a node.
 */
static void sparc_emit_node(const ir_node *node)
{
	ir_op *op = get_irn_op(node);

	if (op->ops.generic) {
		emit_func func = (emit_func) op->ops.generic;
		be_dwarf_location(get_irn_dbg_info(node));
		(*func) (node);
	} else {
		panic("No emit handler for node %+F (graph %+F)\n", node,
		      get_irn_irg(node));
	}
}

static ir_node *find_next_delay_slot(ir_node *from)
{
	ir_node *schedpoint = from;
	while (!has_delay_slot(schedpoint)) {
		if (!sched_has_next(schedpoint))
			return NULL;
		schedpoint = sched_next(schedpoint);
	}
	return schedpoint;
}

static bool block_needs_label(const ir_node *block, const ir_node *sched_prev)
{
	int n_cfgpreds;

	if (get_Block_entity(block) != NULL)
		return true;

	n_cfgpreds = get_Block_n_cfgpreds(block);
	if (n_cfgpreds == 0) {
		return false;
	} else if (n_cfgpreds > 1) {
		return true;
	} else {
		ir_node *cfgpred       = get_Block_cfgpred(block, 0);
		ir_node *cfgpred_block = get_nodes_block(cfgpred);
		if (is_Proj(cfgpred) && is_sparc_SwitchJmp(get_Proj_pred(cfgpred)))
			return true;
		return sched_prev != cfgpred_block || get_irn_link(cfgpred) != block;
	}
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void sparc_emit_block(ir_node *block, ir_node *prev)
{
	ir_node *next_delay_slot;
	bool     needs_label = block_needs_label(block, prev);

	be_gas_begin_block(block, needs_label);

	next_delay_slot = find_next_delay_slot(sched_first(block));
	if (next_delay_slot != NULL)
		delay_slot_filler = pick_delay_slot_for(next_delay_slot);

	sched_foreach(block, node) {
		if (node == delay_slot_filler) {
			continue;
		}

		sparc_emit_node(node);

		if (node == next_delay_slot) {
			assert(delay_slot_filler == NULL);
			next_delay_slot = find_next_delay_slot(sched_next(node));
			if (next_delay_slot != NULL)
				delay_slot_filler = pick_delay_slot_for(next_delay_slot);
		}
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

static void sparc_gen_labels(ir_node *block, void *env)
{
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);
	(void) env;

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block); // link the pred of a block (which is a jmp)
	}
}

void sparc_emit_routine(ir_graph *irg)
{
	ir_node **block_schedule;
	size_t    i;
	size_t    n;

	heights = heights_new(irg);

	/* register all emitter functions */
	sparc_register_emitters();

	/* create the block schedule. For now, we don't need it earlier. */
	block_schedule = be_create_block_schedule(irg);

	sparc_emit_func_prolog(irg);
	irg_block_walk_graph(irg, sparc_gen_labels, NULL, NULL);

	/* inject block scheduling links & emit code of each block */
	n = ARR_LEN(block_schedule);
	for (i = 0; i < n; ++i) {
		ir_node *block      = block_schedule[i];
		ir_node *next_block = i+1 < n ? block_schedule[i+1] : NULL;
		set_irn_link(block, next_block);
	}

	for (i = 0; i < n; ++i) {
		ir_node *block = block_schedule[i];
		ir_node *prev  = i>=1 ? block_schedule[i-1] : NULL;
		if (block == get_irg_end_block(irg))
			continue;
		sparc_emit_block(block, prev);
	}

	/* emit function epilog */
	sparc_emit_func_epilog(irg);

	heights_free(heights);
}

void sparc_init_emitter(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.sparc.emit");
}
