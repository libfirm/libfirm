/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   emit assembler for a backend graph
 */
#include <limits.h>

#include "be_t.h"
#include "error.h"
#include "xmalloc.h"
#include "tv.h"
#include "iredges.h"
#include "debug.h"
#include "irgwalk.h"
#include "irop_t.h"
#include "irargs_t.h"
#include "irprog.h"

#include "besched.h"
#include "begnuas.h"
#include "beblocksched.h"

#include "amd64_emitter.h"
#include "gen_amd64_emitter.h"
#include "gen_amd64_regalloc_if.h"
#include "amd64_nodes_attr.h"
#include "amd64_new_nodes.h"

#include "benode.h"

/**
 * Returns the target block for a control flow node.
 */
static ir_node *get_cfop_target_block(const ir_node *irn)
{
	return (ir_node*)get_irn_link(irn);
}

static void amd64_emit_insn_mode_suffix(amd64_insn_mode_t mode)
{
	char c;
	switch (mode) {
	case INSN_MODE_8:  c = 'b'; break;
	case INSN_MODE_16: c = 'w'; break;
	case INSN_MODE_32: c = 'l'; break;
	case INSN_MODE_64: c = 'q'; break;
	default:
		panic("invalid insn mode");
	}
	be_emit_char(c);
}

static void amd64_emit_mode_suffix(const ir_mode *mode)
{
	assert(mode_is_int(mode) || mode_is_reference(mode));
	char c;
	switch (get_mode_size_bits(mode)) {
	case 8:  c = 'b'; break;
	case 16: c = 'w'; break;
	case 32: c = 'l'; break;
	case 64: c = 'q'; break;
	default:
		panic("Can't output mode_suffix for %+F", mode);
	}
	be_emit_char(c);
}

static const char *get_8bit_name(const arch_register_t *reg)
{
	switch (reg->index) {
	case REG_GP_RAX: return "al";
	case REG_GP_RBX: return "bl";
	case REG_GP_RCX: return "cl";
	case REG_GP_RDX: return "dl";
	case REG_GP_RSP: return "spl";
	case REG_GP_RBP: return "bpl";
	case REG_GP_RSI: return "sil";
	case REG_GP_RDI: return "dil";
	case REG_GP_R8:  return "r8b";
	case REG_GP_R9:  return "r9b";
	case REG_GP_R10: return "r10b";
	case REG_GP_R11: return "r11b";
	case REG_GP_R12: return "r12b";
	case REG_GP_R13: return "r13b";
	case REG_GP_R14: return "r14b";
	case REG_GP_R15: return "r15b";
	}
	panic("unexpected register number");
}

static const char *get_16bit_name(const arch_register_t *reg)
{
	switch (reg->index) {
	case REG_GP_RAX: return "ax";
	case REG_GP_RBX: return "bx";
	case REG_GP_RCX: return "cx";
	case REG_GP_RDX: return "dx";
	case REG_GP_RSP: return "sp";
	case REG_GP_RBP: return "bp";
	case REG_GP_RSI: return "si";
	case REG_GP_RDI: return "di";
	case REG_GP_R8:  return "r8w";
	case REG_GP_R9:  return "r9w";
	case REG_GP_R10: return "r10w";
	case REG_GP_R11: return "r11w";
	case REG_GP_R12: return "r12w";
	case REG_GP_R13: return "r13w";
	case REG_GP_R14: return "r14w";
	case REG_GP_R15: return "r15w";
	}
	panic("unexpected register number");
}

static const char *get_32bit_name(const arch_register_t *reg)
{
	switch (reg->index) {
	case REG_GP_RAX: return "eax";
	case REG_GP_RBX: return "ebx";
	case REG_GP_RCX: return "ecx";
	case REG_GP_RDX: return "edx";
	case REG_GP_RSP: return "esp";
	case REG_GP_RBP: return "ebp";
	case REG_GP_RSI: return "esi";
	case REG_GP_RDI: return "edi";
	case REG_GP_R8:  return "r8d";
	case REG_GP_R9:  return "r9d";
	case REG_GP_R10: return "r10d";
	case REG_GP_R11: return "r11d";
	case REG_GP_R12: return "r12d";
	case REG_GP_R13: return "r13d";
	case REG_GP_R14: return "r14d";
	case REG_GP_R15: return "r15d";
	}
	panic("unexpected register number");
}

static void emit_register(const arch_register_t *reg)
{
	be_emit_char('%');
	be_emit_string(reg->name);
}

static void emit_register_mode(const arch_register_t *reg, const ir_mode *mode)
{
	const char *name;
	switch (get_mode_size_bits(mode)) {
	case 8:  name = get_8bit_name(reg);  break;
	case 16: name = get_16bit_name(reg); break;
	case 32: name = get_32bit_name(reg); break;
	case 64: name = reg->name;           break;
	default:
		panic("invalid mode");
	}
	be_emit_char('%');
	be_emit_string(name);
}

static void emit_register_insn_mode(const arch_register_t *reg,
                                    amd64_insn_mode_t mode)
{
	const char *name;
	switch (mode) {
	case INSN_MODE_8:  name = get_8bit_name(reg);  break;
	case INSN_MODE_16: name = get_16bit_name(reg); break;
	case INSN_MODE_32: name = get_32bit_name(reg); break;
	case INSN_MODE_64: name = reg->name;           break;
	default:
		panic("invalid mode");
	}
	be_emit_char('%');
	be_emit_string(name);
}

typedef enum amd64_emit_mod_t {
	EMIT_NONE        = 0,
	EMIT_RESPECT_LS  = 1U << 0,
	EMIT_IGNORE_MODE = 1U << 1,
} amd64_emit_mod_t;
ENUM_BITSET(amd64_emit_mod_t)

static void amd64_emit_immediate(const amd64_movimm_attr_t *const imm)
{
	ir_entity *entity = imm->entity;
	if (entity != NULL) {
		be_gas_emit_entity(entity);
	}
	if (entity == NULL || imm->offset != 0) {
		if (entity != NULL) {
			be_emit_irprintf("%+ld", imm->offset);
		} else {
			be_emit_irprintf("0x%lX", imm->offset);
		}
	}
}

static void amd64_emit_am(const ir_node *const node,
                          const amd64_am_info_t *const am)
{
	ir_entity *entity = am->entity;
	if (entity != NULL) {
		ir_type *owner = get_entity_owner(entity);
		if (is_frame_type(owner)) {
			entity = NULL; /* only emit offset for frame entities */
		} else {
			be_gas_emit_entity(entity);
		}
	}

	int32_t offset      = am->offset;
	uint8_t base_input  = am->base_input;
	uint8_t index_input = am->index_input;
	if (offset != 0 || (entity == NULL && base_input == NO_INPUT
	                    && index_input == NO_INPUT)) {
		if (entity != NULL) {
			be_emit_irprintf("%+d", offset);
		} else {
			be_emit_irprintf("%d", offset);
		}
	}

	if (base_input != NO_INPUT || index_input != NO_INPUT) {
		be_emit_char('(');

		if (base_input == RIP_INPUT) {
			be_emit_cstring("%rip");
		} else if (base_input != NO_INPUT) {
			const arch_register_t *reg
				= arch_get_irn_register_in(node, base_input);
			emit_register(reg);
		}

		if (index_input != NO_INPUT) {
			be_emit_char(',');
			const arch_register_t *reg
				= arch_get_irn_register_in(node, index_input);
			emit_register(reg);

			unsigned scale = am->log_scale;
			if (scale > 0)
				be_emit_irprintf(",%u", 1 << scale);
		}
		be_emit_char(')');
	}
}

void amd64_emitf(ir_node const *const node, char const *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);

	be_emit_char('\t');
	for (;;) {
		char const *start = fmt;

		while (*fmt != '%' && *fmt != '\n' && *fmt != '\0')
			++fmt;
		if (fmt != start) {
			be_emit_string_len(start, fmt - start);
		}

		if (*fmt == '\n') {
			be_emit_char('\n');
			be_emit_write_line();
			be_emit_char('\t');
			++fmt;
			continue;
		}

		if (*fmt == '\0')
			break;

		++fmt;
		amd64_emit_mod_t mod = EMIT_NONE;
		for (;;) {
			switch (*fmt) {
			case '#': mod |= EMIT_RESPECT_LS;  break;
			case '^': mod |= EMIT_IGNORE_MODE; break;
			default:
				goto end_of_mods;
			}
			++fmt;
		}
end_of_mods:

		switch (*fmt++) {
			arch_register_t const *reg;

			case '%':
				be_emit_char('%');
				break;

			case 'A':
				switch (*fmt++) {
				case 'M': {
					amd64_attr_t const *const attr = get_amd64_attr_const(node);
					amd64_emit_am(node, &attr->am);
					break;
				}
				default:
					goto unknown;
				}
				break;

			case 'C': {
				amd64_movimm_attr_t const *const attr
					= get_amd64_movimm_attr_const(node);
				amd64_emit_immediate(attr);
				break;
			}

			case 'D':
				if (*fmt < '0' || '9' <= *fmt)
					goto unknown;
				reg = arch_get_irn_register_out(node, *fmt++ - '0');
				goto emit_R;

			case 'E': {
				ir_entity const *const ent = va_arg(ap, ir_entity const*);
				be_gas_emit_entity(ent);
				break;
			}

			case 'L': {
				ir_node *const block = get_cfop_target_block(node);
				be_gas_emit_block_name(block);
				break;
			}

			case 'P': {
				x86_condition_code_t cc;
				if (*fmt == 'X') {
					++fmt;
					cc = (x86_condition_code_t)va_arg(ap, int);
				} else {
					panic("unknown modifier");
				}
				x86_emit_condition_code(cc);
				break;
			}

			case 'R':
				reg = va_arg(ap, arch_register_t const*);
emit_R:
				if (mod & EMIT_IGNORE_MODE) {
					emit_register(reg);
				} else {
					amd64_attr_t const *const attr = get_amd64_attr_const(node);
					if (mod & EMIT_RESPECT_LS) {
						emit_register_mode(reg, attr->ls_mode);
					} else {
						emit_register_insn_mode(reg, attr->data.insn_mode);
					}
				}
				break;

			case 'S': {
				int pos;
				if ('0' <= *fmt && *fmt <= '9') {
					pos = *fmt++ - '0';
				} else {
					goto unknown;
				}
				reg = arch_get_irn_register_in(node, pos);
				goto emit_R;
			}

			case 'M': {
				amd64_attr_t const *const attr = get_amd64_attr_const(node);
				if (mod & EMIT_RESPECT_LS) {
					amd64_emit_mode_suffix(attr->ls_mode);
				} else {
					amd64_emit_insn_mode_suffix(attr->data.insn_mode);
				}
				break;
			}

			case 'd': {
				int const num = va_arg(ap, int);
				be_emit_irprintf("%d", num);
				break;
			}

			case 's': {
				char const *const str = va_arg(ap, char const*);
				be_emit_string(str);
				break;
			}

			case 'u': {
				unsigned const num = va_arg(ap, unsigned);
				be_emit_irprintf("%u", num);
				break;
			}

			case 'c': {
				amd64_attr_t const *const attr = get_amd64_attr_const(node);
				ir_mode                  *mode = attr->ls_mode;
				if (get_mode_size_bits(mode) == 64)
					break;
				if (get_mode_size_bits(mode) == 32 && !mode_is_signed(mode)
				 && attr->data.insn_mode == INSN_MODE_32)
					break;
				be_emit_char(mode_is_signed(mode) ? 's' : 'z');
				amd64_emit_mode_suffix(mode);
				break;
			}

			default:
unknown:
				panic("unknown format conversion");
		}
	}

	be_emit_finish_line_gas(node);
	va_end(ap);
}

/**
 * Returns the next block in a block schedule.
 */
static ir_node *sched_next_block(const ir_node *block)
{
    return (ir_node*)get_irn_link(block);
}

/**
 * Emit a Jmp.
 */
static void emit_amd64_Jmp(const ir_node *node)
{
	ir_node *block, *next_block;

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(node);

	/* we have a block schedule */
	next_block = sched_next_block(block);
	if (get_cfop_target_block(node) != next_block) {
		amd64_emitf(node, "jmp %L");
	} else if (be_options.verbose_asm) {
		amd64_emitf(node, "/* fallthrough to %L */");
	}
}

static void emit_amd64_SwitchJmp(const ir_node *node)
{
	const amd64_switch_jmp_attr_t *attr = get_amd64_switch_jmp_attr_const(node);

	amd64_emitf(node, "jmp *%E(,%S0,8)", attr->table_entity);
	be_emit_jump_table(node, attr->table, attr->table_entity, get_cfop_target_block);
}

/**
 * Emit a Compare with conditional branch.
 */
static void emit_amd64_Jcc(const ir_node *irn)
{
	const ir_node         *proj_true  = NULL;
	const ir_node         *proj_false = NULL;
	const ir_node         *block;
	const ir_node         *next_block;
	const amd64_cc_attr_t *attr = get_amd64_cc_attr_const(irn);
	x86_condition_code_t   cc   = attr->cc;

	foreach_out_edge(irn, edge) {
		ir_node *proj = get_edge_src_irn(edge);
		long nr = get_Proj_proj(proj);
		if (nr == pn_Cond_true) {
			proj_true = proj;
		} else {
			proj_false = proj;
		}
	}

	/* for now, the code works for scheduled and non-schedules blocks */
	block = get_nodes_block(irn);

	/* we have a block schedule */
	next_block = sched_next_block(block);

	if (get_cfop_target_block(proj_true) == next_block) {
		/* exchange both proj's so the second one can be omitted */
		const ir_node *t = proj_true;

		proj_true  = proj_false;
		proj_false = t;
		cc         = x86_negate_condition_code(cc);
	}

	/* emit the true proj */
	amd64_emitf(proj_true, "j%PX %L", (int)cc);

	if (get_cfop_target_block(proj_false) == next_block) {
		if (be_options.verbose_asm)
			amd64_emitf(proj_false, "/* fallthrough to %L */");
	} else  {
		amd64_emitf(proj_false, "jmp %L");
	}
}

static void emit_amd64_LoadZ(const ir_node *node)
{
	const amd64_attr_t *attr = get_amd64_attr_const(node);
	switch (attr->data.insn_mode) {
	case INSN_MODE_8:  amd64_emitf(node, "movzbq %AM, %^D0"); break;
	case INSN_MODE_16: amd64_emitf(node, "movzwq %AM, %^D0"); break;
	case INSN_MODE_32:
	case INSN_MODE_64: amd64_emitf(node, "mov%M %AM, %D0");   break;
	default:
		panic("invalid insn mode");
	}
}

static void emit_amd64_Call(const ir_node *node)
{
	const amd64_attr_t *attr = get_amd64_attr_const(node);

	be_emit_cstring("\tcall *");
	int arity = get_irn_arity(node);
	const arch_register_t *reg = arch_get_irn_register_in(node, arity-1);
	emit_register_insn_mode(reg, attr->data.insn_mode);
	be_emit_finish_line_gas(node);
}

/**
 * emit copy node
 */
static void emit_be_Copy(const ir_node *irn)
{
	ir_mode *mode = get_irn_mode(irn);

	if (arch_get_irn_register_in(irn, 0) == arch_get_irn_register_out(irn, 0)) {
		/* omitted Copy */
		return;
	}

	if (mode_is_float(mode)) {
		panic("move not supported for FP");
	} else if (mode_is_data(mode)) {
		amd64_emitf(irn, "mov %^S0, %^D0");
	} else {
		panic("move not supported for this mode");
	}
}

static void emit_be_Perm(const ir_node *node)
{
	arch_register_t const *const reg0 = arch_get_irn_register_out(node, 0);
	arch_register_t const *const reg1 = arch_get_irn_register_out(node, 1);

	arch_register_class_t const* const cls0 = reg0->reg_class;
	assert(cls0 == reg1->reg_class && "Register class mismatch at Perm");

	amd64_emitf(node, "xchg %^R, %^R", reg0, reg1);

	if (cls0 != &amd64_reg_classes[CLASS_amd64_gp]) {
		panic("unexpected register class in be_Perm (%+F)", node);
	}
}

/**
 * Emits code to increase stack pointer.
 */
static void emit_be_IncSP(const ir_node *node)
{
	int offs = be_get_IncSP_offset(node);

	if (offs == 0)
		return;

	if (offs > 0) {
		amd64_emitf(node, "subq $%d, %^D0", offs);
	} else {
		amd64_emitf(node, "addq $%d, %^D0", -offs);
	}
}

static void emit_amd64_Start(const ir_node *node)
{
	ir_graph *irg        = get_irn_irg(node);
	ir_type  *frame_type = get_irg_frame_type(irg);
	unsigned  size       = get_type_size_bytes(frame_type);

	if (size > 0) {
		amd64_emitf(node, "subq $%u, %%rsp", size);
	}
}

static void emit_amd64_Return(const ir_node *node)
{
	ir_graph *irg        = get_irn_irg(node);
	ir_type  *frame_type = get_irg_frame_type(irg);
	unsigned  size       = get_type_size_bytes(frame_type);

	if (size > 0) {
		amd64_emitf(node, "addq $%u, %%rsp", size);
	}

	be_emit_cstring("\tret");
	be_emit_finish_line_gas(node);
}

/**
 * Enters the emitter functions for handled nodes into the generic
 * pointer of an opcode.
 */
static void amd64_register_emitters(void)
{
	/* first clear the generic function pointer for all ops */
	ir_clear_opcodes_generic_func();

	/* register all emitter functions defined in spec */
	amd64_register_spec_emitters();

	be_set_emitter(op_amd64_Call,      emit_amd64_Call);
	be_set_emitter(op_amd64_Jcc,       emit_amd64_Jcc);
	be_set_emitter(op_amd64_Jmp,       emit_amd64_Jmp);
	be_set_emitter(op_amd64_LoadZ,     emit_amd64_LoadZ);
	be_set_emitter(op_amd64_Return,    emit_amd64_Return);
	be_set_emitter(op_amd64_Start,     emit_amd64_Start);
	be_set_emitter(op_amd64_SwitchJmp, emit_amd64_SwitchJmp);
	be_set_emitter(op_be_Copy,         emit_be_Copy);
	be_set_emitter(op_be_CopyKeep,     emit_be_Copy);
	be_set_emitter(op_be_IncSP,        emit_be_IncSP);
	be_set_emitter(op_be_Keep,         be_emit_nothing);
	be_set_emitter(op_be_Perm,         emit_be_Perm);
	be_set_emitter(op_Phi,             be_emit_nothing);
}

/**
 * Walks over the nodes in a block connected by scheduling edges
 * and emits code for each node.
 */
static void amd64_gen_block(ir_node *block, void *data)
{
	(void) data;

	if (! is_Block(block))
		return;

	be_gas_begin_block(block, true);

	sched_foreach(block, node) {
		be_emit_node(node);
	}
}


/**
 * Sets labels for control flow nodes (jump target)
 * TODO: Jump optimization
 */
static void amd64_gen_labels(ir_node *block, void *env)
{
	ir_node *pred;
	int n = get_Block_n_cfgpreds(block);
	(void) env;

	for (n--; n >= 0; n--) {
		pred = get_Block_cfgpred(block, n);
		set_irn_link(pred, block);
	}
}

void amd64_emit_function(ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);
	ir_node  **blk_sched;
	size_t i, n;

	/* register all emitter functions */
	amd64_register_emitters();

	blk_sched = be_create_block_schedule(irg);

	be_gas_emit_function_prolog(entity, 4, NULL);

	irg_block_walk_graph(irg, amd64_gen_labels, NULL, NULL);

	n = ARR_LEN(blk_sched);
	for (i = 0; i < n; i++) {
		ir_node *block = blk_sched[i];
		ir_node *next  = (i + 1) < n ? blk_sched[i+1] : NULL;

		set_irn_link(block, next);
	}

	for (i = 0; i < n; ++i) {
		ir_node *block = blk_sched[i];

		amd64_gen_block(block, 0);
	}

	be_gas_emit_function_epilog(entity);
}
