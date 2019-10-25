/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   code selection (transform FIRM into SPARC FIRM)
 * @author  Hannes Rapp, Matthias Braun
 */
#include "sparc_transform.h"

#include "beasm.h"
#include "beirg.h"
#include "benode.h"
#include "betranshlp.h"
#include "beutil.h"
#include "dbginfo.h"
#include "debug.h"
#include "gen_sparc_new_nodes.h"
#include "gen_sparc_regalloc_if.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "iroptimize.h"
#include "panic.h"
#include "sparc_bearch_t.h"
#include "sparc_cconv.h"
#include "sparc_new_nodes.h"
#include "sparc_nodes_attr.h"
#include "util.h"
#include <stdbool.h>
#include <stdint.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static calling_convention_t *current_cconv = NULL;
static be_stack_env_t        stack_env;
static ir_mode              *mode_gp;
static ir_mode              *mode_fp;
static ir_mode              *mode_fp2;
//static ir_mode              *mode_fp4;
static ir_node              *frame_base;
static ir_node              *initial_va_list;

static const arch_register_t *const omit_fp_callee_saves[] = {
	&sparc_registers[REG_L0],
	&sparc_registers[REG_L1],
	&sparc_registers[REG_L2],
	&sparc_registers[REG_L3],
	&sparc_registers[REG_L4],
	&sparc_registers[REG_L5],
	&sparc_registers[REG_L6],
	&sparc_registers[REG_L7],
	&sparc_registers[REG_I0],
	&sparc_registers[REG_I1],
	&sparc_registers[REG_I2],
	&sparc_registers[REG_I3],
	&sparc_registers[REG_I4],
	&sparc_registers[REG_I5],
};

/**
 * Create an And that will zero out upper bits.
 *
 * @param dbgi      debug info
 * @param block     the basic block
 * @param op        the original node
 * @param src_bits  number of lower bits that will remain
 */
static ir_node *gen_zero_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
                                   int src_bits)
{
	if (src_bits == 8) {
		return new_bd_sparc_And_imm(dbgi, block, op, NULL, 0xFF);
	} else if (src_bits == 16) {
		ir_node *lshift = new_bd_sparc_Sll_imm(dbgi, block, op, NULL, 16);
		ir_node *rshift = new_bd_sparc_Srl_imm(dbgi, block, lshift, NULL, 16);
		return rshift;
	} else {
		panic("zero extension only supported for 8 and 16 bits");
	}
}

/**
 * Generate code for a sign extension.
 *
 * @param dbgi      debug info
 * @param block     the basic block
 * @param op        the original node
 * @param src_bits  number of lower bits that will remain
 */
static ir_node *gen_sign_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
                                   int src_bits)
{
	int shift_width = 32 - src_bits;
	ir_node *lshift_node = new_bd_sparc_Sll_imm(dbgi, block, op, NULL, shift_width);
	ir_node *rshift_node = new_bd_sparc_Sra_imm(dbgi, block, lshift_node, NULL, shift_width);
	return rshift_node;
}

/**
 * Extend a value to 32 bit signed/unsigned depending on its mode.
 *
 * @param dbgi      debug info
 * @param block     the basic block
 * @param op        the original node
 * @param orig_mode the original mode of op
 */
static ir_node *gen_extension(dbg_info *dbgi, ir_node *block, ir_node *op,
                              ir_mode *orig_mode)
{
	int bits = get_mode_size_bits(orig_mode);
	assert(bits < 32);

	if (mode_is_signed(orig_mode)) {
		return gen_sign_extension(dbgi, block, op, bits);
	} else {
		return gen_zero_extension(dbgi, block, op, bits);
	}
}

typedef enum {
	MATCH_NONE          = 0,
	MATCH_COMMUTATIVE   = 1U << 0, /**< commutative operation. */
	MATCH_MODE_NEUTRAL  = 1U << 1, /**< the higher bits of the inputs don't
	                                    influence the significant lower bit at
	                                    all (for cases where mode < 32bit) */
	MATCH_SIGN_EXT_LEFT = 1U << 2, /**< we need to sign-extend the left operand
	                                    (for cases when mode < 32bit) */
	MATCH_ZERO_EXT_LEFT = 1U << 3, /**< we need to zero-extend the left operand
                                            (for cases when mode < 32bit) */
} match_flags_t;
ENUM_BITSET(match_flags_t)

typedef ir_node* (*new_binop_reg_func) (dbg_info *dbgi, ir_node *block, ir_node *op1, ir_node *op2);
typedef ir_node* (*new_binop_fp_func) (dbg_info *dbgi, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode);
typedef ir_node* (*new_binop_imm_func) (dbg_info *dbgi, ir_node *block, ir_node *op1, ir_entity *entity, int32_t immediate);
typedef ir_node* (*new_unop_fp_func) (dbg_info *dbgi, ir_node *block, ir_node *op1, ir_mode *mode);

/**
 * checks if a node's value can be encoded as a immediate
 */
static bool is_imm_encodeable(const ir_node *node)
{
	if (!is_Const(node))
		return false;

	long const value = get_Const_long(node);
	return sparc_is_value_imm_encodeable(value);
}

static bool needs_extension(ir_node *op)
{
	ir_mode *mode = get_irn_mode(op);
	unsigned gp_bits = get_mode_size_bits(mode_gp);
	if (get_mode_size_bits(mode) >= gp_bits)
		return false;
	return !be_upper_bits_clean(op, mode);
}

static void sparc_parse_constraint_letter(void const *const env, be_asm_constraint_t* const c, char const l)
{
	(void)env;

	switch (l) {
	case 'r':
		c->cls                   = &sparc_reg_classes[CLASS_sparc_gp];
		c->all_registers_allowed = true;
		break;

	case 'e':
	case 'f':
		c->cls                   = &sparc_reg_classes[CLASS_sparc_fp];
		c->all_registers_allowed = true;
		break;

	case 'g':
		c->all_registers_allowed = true;
		c->memory_possible       = true;
		/* FALLTHROUGH */
	case 'A':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'O':
	case 'P':
	case 'i':
	case 'n':
		c->cls            = &sparc_reg_classes[CLASS_sparc_gp];
		c->immediate_type = l;
		break;

	case 'm':
	case 'w':
		c->memory_possible = true;
		break;

	default:
		panic("unknown asm constraint '%c'", l);
	}
}

void sparc_init_asm_constraints(void)
{
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER,  "efr");
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE, "AIJKLMOPin");
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_ANY,       "g");
	be_set_constraint_support(ASM_CONSTRAINT_FLAG_SUPPORTS_MEMOP,     "mw");
	/* Note there are many more flags in gcc which we can't properly support
	 * at the moment. see gcc/config/sparc/constraints.md */
}

static bool sparc_check_immediate_constraint(long const val, char const imm_type)
{
	switch (imm_type) {
	case 'A': return   -16 <= val && val <   16;
	case 'I': return sparc_is_value_imm_encodeable(val);
	case 'J': return val ==    0;
	case 'K': return (val & 0x03FF) == 0;
	case 'L': return -1024 <= val && val < 1024;
	case 'M': return  -512 <= val && val <  512;
	case 'O': return val == 4096;
	case 'P': return val ==   -1;

	case 'g':
	case 'i':
	case 'n': return true;
	}
	panic("invalid immediate constraint found");
}

static bool sparc_match_immediate(sparc_asm_operand_t *const operand, ir_node *const node, char const imm_type)
{
	ir_tarval *offset;
	ir_entity *entity;
	unsigned   reloc_kind;
	if (!be_match_immediate(node, &offset, &entity, &reloc_kind))
		return false;

	if (entity && imm_type != 'g' && imm_type != 'i')
		return false;

	long value;
	if (offset) {
		value = get_tarval_long(offset);
		if (!sparc_check_immediate_constraint(value, imm_type))
			return false;
	} else {
		value = 0;
	}

	operand->immediate_value        = value;
	operand->immediate_value_entity = entity;
	return true;
}

static ir_node *gen_ASM(ir_node *node)
{
	be_asm_info_t info = be_asm_prepare_info(node);

	ir_asm_constraint const *const constraints   = get_ASM_constraints(node);
	size_t                   const n_constraints = get_ASM_n_constraints(node);
	ir_graph                *const irg           = get_irn_irg(node);
	struct obstack          *const obst          = get_irg_obstack(irg);
	sparc_asm_operand_t     *const operands      = NEW_ARR_DZ(sparc_asm_operand_t, obst, n_constraints);
	for (size_t i = 0; i != n_constraints; ++i) {
		ir_asm_constraint const *const c = &constraints[i];

		be_asm_constraint_t be_constraint;
		be_parse_asm_constraints_internal(&be_constraint, c->constraint, &sparc_parse_constraint_letter, NULL);

		sparc_asm_operand_t *const op = &operands[i];

		int const in_pos = c->in_pos;
		if (in_pos >= 0) {
			ir_node *const in  = get_ASM_input(node, in_pos);
			char     const imm = be_constraint.immediate_type;
			if (imm != '\0' && sparc_match_immediate(op, in, imm)) {
				be_asm_add_immediate(&op->op);
			} else if (be_constraint.same_as >= 0) {
				int                        const out_pos = operands[be_constraint.same_as].op.pos;
				arch_register_req_t const *const ireq    = info.out_reqs[out_pos];
				be_asm_add_inout(&info, &op->op, obst, in, ireq, out_pos);
			} else if (be_constraint.cls) {
				arch_register_req_t const *const ireq = be_make_register_req(obst, &be_constraint);
				be_asm_add_inout(&info, &op->op, obst, in, ireq, c->out_pos);
			} else {
				ir_node                   *const new_in = be_transform_node(in);
				arch_register_req_t const *const ireq   = arch_get_irn_register_req(new_in)->cls->class_req;
				be_asm_add_in(&info, &op->op, BE_ASM_OPERAND_MEMORY, new_in, ireq);
			}
		} else {
			be_asm_add_out(&info, &op->op, obst, &be_constraint, c->out_pos);
		}
	}

	/* Handle clobber "cc". */
	ident **const clobbers = get_ASM_clobbers(node);
	for (size_t c = 0; c < get_ASM_n_clobbers(node); ++c) {
		const char *const clobber = get_id_str(clobbers[c]);
		if (streq(clobber, "cc")) {
			ARR_APP1(arch_register_req_t const*, info.out_reqs, &sparc_single_reg_req_flags_psr);
			ARR_APP1(arch_register_req_t const*, info.out_reqs, &sparc_single_reg_req_fpflags_fsr);
		}
	}

	return be_make_asm(node, &info, operands);
}

/* Transforms the left operand of a binary operation.
 *
 * Performs sign/zero extension if necessary.
 */
static ir_node *gen_helper_binop_left(ir_node *left, dbg_info *dbgi,
                                      ir_node *block, match_flags_t flags)
{
	ir_mode *mode     = get_irn_mode(left);
	ir_node *new_left = be_transform_node(left);

	if (flags & MATCH_MODE_NEUTRAL)
		return new_left;

	int bits    = get_mode_size_bits(mode);
	int gp_bits = get_mode_size_bits(mode_gp);

	if (bits >= gp_bits)
		return new_left;

	if (flags & MATCH_SIGN_EXT_LEFT) {
		if (!mode_is_signed(mode) || needs_extension(left)) {
			return gen_sign_extension(dbgi, block, new_left, bits);
		}
	} else if (flags & MATCH_ZERO_EXT_LEFT) {
		if (mode_is_signed(mode) || needs_extension(left)) {
			return gen_zero_extension(dbgi, block, new_left, bits);
		}
	} else if (needs_extension(left)) {
		return gen_extension(dbgi, block, new_left, mode);
	}

	return new_left;
}

/**
 * helper function for binop operations
 *
 * @param new_reg  register generation function ptr
 * @param new_imm  immediate generation function ptr
 */
static ir_node *gen_helper_binop_args(ir_node *node,
                                      ir_node *op1, ir_node *op2,
                                      match_flags_t flags,
                                      new_binop_reg_func new_reg,
                                      new_binop_imm_func new_imm)
{
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_node  *block = be_transform_nodes_block(node);

	if (flags & MATCH_MODE_NEUTRAL) {
		op1 = be_skip_downconv(op1, false);
		op2 = be_skip_downconv(op2, false);
	}
	ir_mode *mode2 = get_irn_mode(op2);
	/* we should not see 64bit code */
	assert(get_mode_size_bits(get_irn_mode(op1)) <= 32);
	assert(get_mode_size_bits(mode2) <= 32);

	if (is_imm_encodeable(op2)) {
		int32_t  const immediate = get_Const_long(op2);
		ir_node *const new_op1   = gen_helper_binop_left(op1, dbgi, block, flags);
		return new_imm(dbgi, block, new_op1, NULL, immediate);
	}
	ir_node *new_op2 = be_transform_node(op2);
	if (! (flags & MATCH_MODE_NEUTRAL) && needs_extension(op2)) {
		new_op2 = gen_extension(dbgi, block, new_op2, mode2);
	}

	if ((flags & MATCH_COMMUTATIVE) && is_imm_encodeable(op1)) {
		int32_t const immediate = get_Const_long(op1);
		return new_imm(dbgi, block, new_op2, NULL, immediate);
	}

	ir_node *new_op1 = gen_helper_binop_left(op1, dbgi, block, flags);
	return new_reg(dbgi, block, new_op1, new_op2);
}

static ir_node *gen_helper_binop(ir_node *node, match_flags_t flags,
                                 new_binop_reg_func new_reg,
                                 new_binop_imm_func new_imm)
{
	ir_node *op1 = get_binop_left(node);
	ir_node *op2 = get_binop_right(node);
	return gen_helper_binop_args(node, op1, op2, flags, new_reg, new_imm);
}

/**
 * helper function for FP binop operations
 */
static ir_node *gen_helper_binfpop(ir_node *node, ir_mode *mode,
                                   new_binop_fp_func new_func_single,
                                   new_binop_fp_func new_func_double,
                                   new_binop_fp_func new_func_quad)
{
	ir_node  *block   = be_transform_nodes_block(node);
	ir_node  *op1     = get_binop_left(node);
	ir_node  *new_op1 = be_transform_node(op1);
	ir_node  *op2     = get_binop_right(node);
	ir_node  *new_op2 = be_transform_node(op2);
	dbg_info *dbgi    = get_irn_dbg_info(node);
	unsigned  bits    = get_mode_size_bits(mode);

	switch (bits) {
	case 32:
		return new_func_single(dbgi, block, new_op1, new_op2, mode);
	case 64:
		return new_func_double(dbgi, block, new_op1, new_op2, mode);
	case 128:
		return new_func_quad(dbgi, block, new_op1, new_op2, mode);
	default:
		break;
	}
	panic("unsupported mode %+F for float op", mode);
}

typedef ir_node* (*new_binopx_imm_func)(dbg_info *dbgi, ir_node *block,
                                        ir_node *op1, ir_node *flags,
                                        ir_entity *imm_entity, int32_t imm);

typedef ir_node* (*new_binopx_reg_func)(dbg_info *dbgi, ir_node *block,
                                        ir_node *op1, ir_node *op2,
                                        ir_node *flags);

static ir_node *gen_helper_binopx(ir_node *node, match_flags_t match_flags,
                                  new_binopx_reg_func new_binopx_reg,
                                  new_binopx_imm_func new_binopx_imm)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = be_transform_nodes_block(node);
	ir_node  *op1       = get_irn_n(node, 0);
	ir_node  *op2       = get_irn_n(node, 1);
	ir_node  *flags     = get_irn_n(node, 2);
	ir_node  *new_flags = be_transform_node(flags);

	/* only support for mode-neutral implemented so far */
	assert(match_flags & MATCH_MODE_NEUTRAL);

	if (is_imm_encodeable(op2)) {
		int32_t  const immediate = get_Const_long(op2);
		ir_node *const new_op1   = be_transform_node(op1);
		return new_binopx_imm(dbgi, block, new_op1, new_flags, NULL, immediate);
	}
	ir_node *new_op2 = be_transform_node(op2);
	if ((match_flags & MATCH_COMMUTATIVE) && is_imm_encodeable(op1)) {
		int32_t const immediate = get_Const_long(op1);
		return new_binopx_imm(dbgi, block, new_op2, new_flags, NULL, immediate);
	}
	ir_node *new_op1 = be_transform_node(op1);
	return new_binopx_reg(dbgi, block, new_op1, new_op2, new_flags);

}

static ir_node *get_g0(ir_graph *irg)
{
	return be_get_Start_proj(irg, &sparc_registers[REG_G0]);
}

static ir_node *get_g7(ir_graph *irg)
{
	return be_get_Start_proj(irg, &sparc_registers[REG_G7]);
}

static ir_node *make_tls_offset(dbg_info *dbgi, ir_node *block,
                                ir_entity *entity, int32_t offset)
{
	ir_node  *hi  = new_bd_sparc_SetHi(dbgi, block, entity, offset);
	ir_node  *low = new_bd_sparc_Xor_imm(dbgi, block, hi, entity, offset);
	return low;
}

static ir_node *make_address(dbg_info *dbgi, ir_node *block, ir_entity *entity,
                             int32_t offset)
{
	if (is_tls_entity(entity)) {
		ir_graph *irg     = get_irn_irg(block);
		ir_node  *g7      = get_g7(irg);
		ir_node  *offsetn = make_tls_offset(dbgi, block, entity, offset);
		ir_node  *add     = new_bd_sparc_Add_reg(dbgi, block, g7, offsetn);
		return add;
	} else {
		ir_node *hi  = new_bd_sparc_SetHi(dbgi, block, entity, offset);
		ir_node *low = new_bd_sparc_Or_imm(dbgi, block, hi, entity, offset);
		return low;
	}
}

typedef struct address_t {
	ir_node   *ptr;
	ir_node   *ptr2;
	ir_entity *entity;
	int32_t    offset;
} address_t;

/**
 * Match a load/store address
 */
static void match_address(ir_node *ptr, address_t *address, bool use_ptr2)
{
	ir_node   *base   = ptr;
	int32_t    offset = 0;

	if (is_Add(base)) {
		ir_node *add_right = get_Add_right(base);
		if (is_Const(add_right)) {
			base    = get_Add_left(base);
			offset += get_Const_long(add_right);
		}
	}
	/* Note that we don't match sub(x, Const) or chains of adds/subs
	 * because this should all be normalized by now */

	/* we only use the entity if we're the only user otherwise we probably
	 * won't save anything but produce multiple sethi+or combinations with
	 * just different offsets */
	ir_node   *ptr2   = NULL;
	ir_entity *entity = NULL;
	if (is_Address(base) && get_irn_n_edges(base) == 1) {
		ir_entity *sc_entity = get_Address_entity(base);
		dbg_info  *dbgi      = get_irn_dbg_info(ptr);
		ir_node   *new_block = be_transform_nodes_block(ptr);

		if (is_tls_entity(sc_entity)) {
			if (!use_ptr2) {
				goto only_offset;
			} else {
				ptr2   = make_tls_offset(dbgi, new_block, sc_entity, offset);
				offset = 0;
				base   = get_g7(get_irn_irg(base));
			}
		} else {
			entity = sc_entity;
			base   = new_bd_sparc_SetHi(dbgi, new_block, entity, offset);
		}
	} else if (use_ptr2 && is_Add(base) && offset == 0) {
		ptr2 = be_transform_node(get_Add_right(base));
		base = be_transform_node(get_Add_left(base));
	} else {
only_offset:
		if (sparc_is_value_imm_encodeable(offset)) {
			base = be_transform_node(base);
		} else {
			base   = be_transform_node(ptr);
			offset = 0;
		}
	}

	address->ptr    = base;
	address->ptr2   = ptr2;
	address->entity = entity;
	address->offset = offset;
}

/**
 * Creates an sparc Add.
 *
 * @param node   FIRM node
 * @return the created sparc Add node
 */
static ir_node *gen_Add(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (mode_is_float(mode)) {
		return gen_helper_binfpop(node, mode, new_bd_sparc_fadd_s,
		                          new_bd_sparc_fadd_d, new_bd_sparc_fadd_q);
	}

	/* special case: + 0x1000 can be represented as - 0x1000 */
	ir_node *right = get_Add_right(node);
	if (is_Const(right)) {
		ir_node   *left = get_Add_left(node);
		/* is this simple address arithmetic? then we can let the linker do
		 * the calculation. */
		if (is_Address(left) && get_irn_n_edges(left) == 1) {
			dbg_info *dbgi  = get_irn_dbg_info(node);
			ir_node  *block = be_transform_nodes_block(node);

			/* the value of use_ptr2 shouldn't matter here */
			address_t address;
			match_address(node, &address, false);
			assert(is_sparc_SetHi(address.ptr));
			return new_bd_sparc_Or_imm(dbgi, block, address.ptr,
			                           address.entity, address.offset);
		}

		uint32_t const val = get_Const_long(right);
		if (val == 0x1000) {
			dbg_info *dbgi   = get_irn_dbg_info(node);
			ir_node  *block  = be_transform_nodes_block(node);
			ir_node  *op     = get_Add_left(node);
			ir_node  *new_op = be_transform_node(op);
			return new_bd_sparc_Sub_imm(dbgi, block, new_op, NULL, -0x1000);
		}
	}

	return gen_helper_binop(node, MATCH_COMMUTATIVE | MATCH_MODE_NEUTRAL,
	                        new_bd_sparc_Add_reg, new_bd_sparc_Add_imm);
}

static ir_node *gen_AddCC_t(ir_node *node)
{
	ir_node *left     = get_irn_n(node, n_sparc_AddCC_t_left);
	ir_node *right    = get_irn_n(node, n_sparc_AddCC_t_right);
	ir_node *new_node = gen_helper_binop_args(node, left, right,
	                             MATCH_COMMUTATIVE | MATCH_MODE_NEUTRAL,
	                             new_bd_sparc_AddCC_reg, new_bd_sparc_AddCC_imm);
	arch_set_irn_register_out(new_node, pn_sparc_AddCC_flags, &sparc_registers[REG_PSR]);

	return new_node;
}

static ir_node *gen_Proj_AddCC_t(ir_node *node)
{
	unsigned pn       = get_Proj_num(node);
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);

	switch (pn) {
	case pn_sparc_AddCC_t_res:
		return be_new_Proj(new_pred, pn_sparc_AddCC_res);
	case pn_sparc_AddCC_t_flags:
		return be_new_Proj(new_pred, pn_sparc_AddCC_flags);
	default:
		panic("invalid proj found");
	}
}

static ir_node *gen_AddX_t(ir_node *node)
{
	return gen_helper_binopx(node, MATCH_COMMUTATIVE | MATCH_MODE_NEUTRAL,
	                         new_bd_sparc_AddX_reg, new_bd_sparc_AddX_imm);
}

/**
 * Creates an sparc Sub.
 *
 * @param node       FIRM node
 * @return the created sparc Sub node
 */
static ir_node *gen_Sub(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (mode_is_float(mode)) {
		return gen_helper_binfpop(node, mode, new_bd_sparc_fsub_s,
		                          new_bd_sparc_fsub_d, new_bd_sparc_fsub_q);
	}

	return gen_helper_binop(node, MATCH_MODE_NEUTRAL,
	                        new_bd_sparc_Sub_reg, new_bd_sparc_Sub_imm);
}

static ir_node *gen_SubCC_t(ir_node *node)
{
	ir_node *left     = get_irn_n(node, n_sparc_SubCC_t_left);
	ir_node *right    = get_irn_n(node, n_sparc_SubCC_t_right);
	ir_node *new_node = gen_helper_binop_args(node, left, right, MATCH_MODE_NEUTRAL,
	                             new_bd_sparc_SubCC_reg, new_bd_sparc_SubCC_imm);
	arch_set_irn_register_out(new_node, pn_sparc_SubCC_flags, &sparc_registers[REG_PSR]);

	return new_node;
}

static ir_node *gen_Proj_SubCC_t(ir_node *node)
{
	unsigned pn       = get_Proj_num(node);
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);

	switch (pn) {
	case pn_sparc_SubCC_t_res:
		return be_new_Proj(new_pred, pn_sparc_SubCC_res);
	case pn_sparc_SubCC_t_flags:
		return be_new_Proj(new_pred, pn_sparc_SubCC_flags);
	default:
		panic("invalid proj found");
	}
}

static ir_node *gen_SubX_t(ir_node *node)
{
	return gen_helper_binopx(node, MATCH_MODE_NEUTRAL,
	                         new_bd_sparc_SubX_reg, new_bd_sparc_SubX_imm);
}

ir_node *create_ldf(dbg_info *dbgi, ir_node *block, ir_node *ptr,
                    ir_node *mem, ir_mode *mode, ir_entity *entity,
                    long offset, bool is_frame_entity)
{
	unsigned bits = get_mode_size_bits(mode);
	assert(mode_is_float(mode));
	if (bits == 32) {
		return new_bd_sparc_Ldf_s(dbgi, block, ptr, mem, mode, entity,
		                          offset, is_frame_entity);
	} else if (bits == 64) {
		return new_bd_sparc_Ldf_d(dbgi, block, ptr, mem, mode, entity,
		                          offset, is_frame_entity);
	} else {
		assert(bits == 128);
		return new_bd_sparc_Ldf_q(dbgi, block, ptr, mem, mode, entity,
		                          offset, is_frame_entity);
	}
}

ir_node *create_stf(dbg_info *dbgi, ir_node *block, ir_node *value,
                    ir_node *ptr, ir_node *mem, ir_mode *mode,
                    ir_entity *entity, long offset,
                    bool is_frame_entity)
{
	unsigned bits = get_mode_size_bits(mode);
	assert(mode_is_float(mode));
	if (bits == 32) {
		return new_bd_sparc_Stf_s(dbgi, block, value, ptr, mem, mode, entity,
		                          offset, is_frame_entity);
	} else if (bits == 64) {
		return new_bd_sparc_Stf_d(dbgi, block, value, ptr, mem, mode, entity,
		                          offset, is_frame_entity);
	} else {
		assert(bits == 128);
		return new_bd_sparc_Stf_q(dbgi, block, value, ptr, mem, mode, entity,
		                          offset, is_frame_entity);
	}
}

/**
 * Transforms a Load.
 *
 * @param node    the ir Load node
 * @return the created sparc Load node
 */
static ir_node *gen_Load(ir_node *node)
{
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_mode  *mode     = get_Load_mode(node);
	ir_node  *block    = be_transform_nodes_block(node);
	ir_node  *ptr      = get_Load_ptr(node);
	ir_node  *mem      = get_Load_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);

	if (get_Load_unaligned(node) == align_non_aligned) {
		panic("transformation of unaligned Loads not implemented yet");
	}

	ir_node  *new_load;
	if (mode_is_float(mode)) {
		address_t address;
		match_address(ptr, &address, false);
		new_load = create_ldf(dbgi, block, address.ptr, new_mem, mode,
		                      address.entity, address.offset, false);
	} else {
		address_t address;
		match_address(ptr, &address, true);
		if (address.ptr2 != NULL) {
			assert(address.entity == NULL && address.offset == 0);
			new_load = new_bd_sparc_Ld_reg(dbgi, block, new_mem, address.ptr, address.ptr2, mode);
		} else {
			new_load = new_bd_sparc_Ld_imm(dbgi, block, new_mem, address.ptr, mode, address.entity, address.offset, false);
		}
	}
	set_irn_pinned(new_load, get_irn_pinned(node));

	return new_load;
}

/**
 * Transforms a Store.
 *
 * @param node    the ir Store node
 * @return the created sparc Store node
 */
static ir_node *gen_Store(ir_node *node)
{
	ir_node  *block    = be_transform_nodes_block(node);
	ir_node  *ptr      = get_Store_ptr(node);
	ir_node  *mem      = get_Store_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	ir_node  *val      = get_Store_value(node);
	ir_mode  *mode     = get_irn_mode(val);
	dbg_info *dbgi     = get_irn_dbg_info(node);

	if (get_Store_unaligned(node) == align_non_aligned) {
		panic("transformation of unaligned Stores not implemented yet");
	}

	ir_node  *new_store;
	if (mode_is_float(mode)) {
		ir_node *new_val = be_transform_node(val);
		/* TODO: variants with reg+reg address mode */
		address_t address;
		match_address(ptr, &address, false);
		new_store = create_stf(dbgi, block, new_val, address.ptr, new_mem,
		                       mode, address.entity, address.offset, false);
	} else {
		val = be_skip_downconv(val, false);
		ir_node *new_val = be_transform_node(val);

		assert(get_mode_size_bits(mode) <= 32);
		address_t address;
		match_address(ptr, &address, true);
		if (address.ptr2 != NULL) {
			assert(address.entity == NULL && address.offset == 0);
			new_store = new_bd_sparc_St_reg(dbgi, block, new_mem, new_val, address.ptr, address.ptr2, mode);
		} else {
			new_store = new_bd_sparc_St_imm(dbgi, block, new_mem, new_val, address.ptr, mode, address.entity, address.offset, false);
		}
	}
	set_irn_pinned(new_store, get_irn_pinned(node));

	return new_store;
}

/**
 * Creates an sparc Mul.
 * returns the lower 32bits of the 64bit multiply result
 *
 * @return the created sparc Mul node
 */
static ir_node *gen_Mul(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (mode_is_float(mode)) {
		return gen_helper_binfpop(node, mode, new_bd_sparc_fmul_s,
		                          new_bd_sparc_fmul_d, new_bd_sparc_fmul_q);
	}

	return gen_helper_binop(node, MATCH_COMMUTATIVE | MATCH_MODE_NEUTRAL,
	                        new_bd_sparc_SMul_reg, new_bd_sparc_SMul_imm);
}

/**
 * Creates an sparc Mulh.
 * Mulh returns the upper 32bits of a mul instruction
 *
 * @return the created sparc Mulh node
 */
static ir_node *gen_Mulh(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (mode_is_float(mode))
		panic("FP not supported yet");

	if (mode_is_signed(mode)) {
		return gen_helper_binop(node, MATCH_COMMUTATIVE, new_bd_sparc_SMulh_reg, new_bd_sparc_SMulh_imm);
	} else {
		return gen_helper_binop(node, MATCH_COMMUTATIVE, new_bd_sparc_UMulh_reg, new_bd_sparc_UMulh_imm);
	}
}

static ir_node *gen_sign_extension_value(ir_node *node)
{
	ir_node *new_block = be_transform_nodes_block(node);
	ir_node *new_node  = be_transform_node(node);
	/* TODO: we could do some shortcuts for some value types probably.
	 * (For constants or other cases where we know the sign bit in
	 *  advance) */
	return new_bd_sparc_Sra_imm(NULL, new_block, new_node, NULL, 31);
}

/**
 * Creates an sparc Div.
 *
 * @return the created sparc Div node
 */
static ir_node *gen_Div(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_mode  *mode      = get_Div_resmode(node);
	ir_node  *left      = get_Div_left(node);
	ir_node  *left_low  = be_transform_node(left);
	ir_node  *right     = get_Div_right(node);

	if (mode_is_float(mode)) {
		return gen_helper_binfpop(node, mode, new_bd_sparc_fdiv_s,
		                          new_bd_sparc_fdiv_d, new_bd_sparc_fdiv_q);
	}

	ir_node *mem     = get_Div_mem(node);
	ir_node *new_mem = be_transform_node(mem);
	ir_node *res;
	if (mode_is_signed(mode)) {
		ir_node *left_high = gen_sign_extension_value(left);

		if (is_imm_encodeable(right)) {
			int32_t const immediate = get_Const_long(right);
			res = new_bd_sparc_SDiv_imm(dbgi, new_block, new_mem, left_high, left_low,
			                            NULL, immediate);
		} else {
			ir_node *new_right = be_transform_node(right);
			res = new_bd_sparc_SDiv_reg(dbgi, new_block, new_mem, left_high, left_low,
			                            new_right);
		}
	} else {
		ir_graph *irg       = get_irn_irg(node);
		ir_node  *left_high = get_g0(irg);
		if (is_imm_encodeable(right)) {
			int32_t const immediate = get_Const_long(right);
			res = new_bd_sparc_UDiv_imm(dbgi, new_block, new_mem, left_high, left_low,
			                            NULL, immediate);
		} else {
			ir_node *new_right = be_transform_node(right);
			res = new_bd_sparc_UDiv_reg(dbgi, new_block, new_mem, left_high, left_low,
			                            new_right);
		}
	}

	return res;
}

/**
 * Transforms a Not node.
 *
 * @return the created sparc Not node
 */
static ir_node *gen_Not(ir_node *node)
{
	ir_node  *op     = get_Not_op(node);
	ir_graph *irg    = get_irn_irg(node);
	ir_node  *zero   = get_g0(irg);
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_node  *block  = be_transform_nodes_block(node);
	ir_node  *new_op = be_transform_node(op);

	/* Note: Not(Eor()) is normalize in firm localopts already so
	 * we don't match it for xnor here */

	/* Not can be represented with xnor 0, n */
	return new_bd_sparc_XNor_reg(dbgi, block, zero, new_op);
}

static ir_node *gen_helper_bitop(ir_node *node,
                                 new_binop_reg_func new_reg,
                                 new_binop_imm_func new_imm,
                                 new_binop_reg_func new_not_reg,
                                 new_binop_imm_func new_not_imm,
                                 match_flags_t flags)
{
	ir_node *op1 = get_binop_left(node);
	ir_node *op2 = get_binop_right(node);
	if (is_Not(op1)) {
		return gen_helper_binop_args(node, op2, get_Not_op(op1),
		                             flags,
		                             new_not_reg, new_not_imm);
	}
	if (is_Not(op2)) {
		return gen_helper_binop_args(node, op1, get_Not_op(op2),
		                             flags,
		                             new_not_reg, new_not_imm);
	}
	if (is_Const(op2) && get_irn_n_edges(op2) == 1) {
		long const value = get_Const_long(op2);
		if (!sparc_is_value_imm_encodeable(value)) {
			long notvalue = ~value;
			if ((notvalue & 0x3ff) == 0) {
				ir_node  *new_block = be_transform_nodes_block(node);
				dbg_info *dbgi      = get_irn_dbg_info(node);
				ir_node  *new_op2
					= new_bd_sparc_SetHi(NULL, new_block, NULL, notvalue);
				ir_node  *new_op1   = be_transform_node(op1);
				ir_node  *result
					= new_not_reg(dbgi, new_block, new_op1, new_op2);
				return result;
			}
		}
	}
	return gen_helper_binop_args(node, op1, op2,
	                             flags | MATCH_COMMUTATIVE,
	                             new_reg, new_imm);
}

static ir_node *gen_And(ir_node *node)
{
	return gen_helper_bitop(node,
	                        new_bd_sparc_And_reg,
	                        new_bd_sparc_And_imm,
	                        new_bd_sparc_AndN_reg,
	                        new_bd_sparc_AndN_imm,
	                        MATCH_MODE_NEUTRAL);
}

static ir_node *gen_Or(ir_node *node)
{
	return gen_helper_bitop(node,
	                        new_bd_sparc_Or_reg,
	                        new_bd_sparc_Or_imm,
	                        new_bd_sparc_OrN_reg,
	                        new_bd_sparc_OrN_imm,
	                        MATCH_MODE_NEUTRAL);
}

static ir_node *gen_Eor(ir_node *node)
{
	return gen_helper_bitop(node,
	                        new_bd_sparc_Xor_reg,
	                        new_bd_sparc_Xor_imm,
	                        new_bd_sparc_XNor_reg,
	                        new_bd_sparc_XNor_imm,
	                        MATCH_MODE_NEUTRAL);
}

static ir_node *gen_Shl(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (get_mode_modulo_shift(mode) != 32)
		panic("modulo_shift!=32 not supported");
	return gen_helper_binop(node, MATCH_NONE, new_bd_sparc_Sll_reg, new_bd_sparc_Sll_imm);
}

static ir_node *gen_Shr(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (get_mode_modulo_shift(mode) != 32)
		panic("modulo_shift!=32 not supported");
	return gen_helper_binop(node, MATCH_ZERO_EXT_LEFT, new_bd_sparc_Srl_reg, new_bd_sparc_Srl_imm);
}

static ir_node *gen_Shrs(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (get_mode_modulo_shift(mode) != 32)
		panic("modulo_shift!=32 not supported");
	return gen_helper_binop(node, MATCH_SIGN_EXT_LEFT, new_bd_sparc_Sra_reg, new_bd_sparc_Sra_imm);
}

static ir_node *gen_fneg(ir_node *node, ir_mode *mode)
{
	ir_node  *block  = be_transform_nodes_block(node);
	ir_node  *op     = get_Minus_op(node);
	ir_node  *new_op = be_transform_node(op);
	dbg_info *dbgi   = get_irn_dbg_info(node);
	unsigned  bits   = get_mode_size_bits(mode);

	switch (bits) {
	case 32:
		return new_bd_sparc_fneg_s(dbgi, block, new_op, mode);
	case 64:
		return new_bd_sparc_fneg_d(dbgi, block, new_op, mode);
	case 128:
		return new_bd_sparc_fneg_q(dbgi, block, new_op, mode);
	default:
		break;
	}
	panic("unsupported mode %+F for float op", mode);
}

/**
 * Transforms a Minus node.
 */
static ir_node *gen_Minus(ir_node *node)
{
	ir_mode  *mode = get_irn_mode(node);
	if (mode_is_float(mode)) {
		return gen_fneg(node, mode);
	}

	ir_node  *block  = be_transform_nodes_block(node);
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_node  *op     = get_Minus_op(node);
	ir_node  *new_op = be_transform_node(op);
	ir_node  *zero   = get_g0(get_irn_irg(node));
	return new_bd_sparc_Sub_reg(dbgi, block, zero, new_op);
}

/**
 * Create an entity for a given (floating point) tarval
 */
static ir_entity *create_float_const_entity(ir_tarval *const tv)
{
	ir_entity *entity = pmap_get(ir_entity, sparc_constants, tv);
	if (entity != NULL)
		return entity;

	ir_mode *mode   = get_tarval_mode(tv);
	ir_type *type   = get_type_for_mode(mode);
	ir_type *glob   = get_glob_type();
	entity = new_global_entity(glob, id_unique("C"), type,
	                           ir_visibility_private,
	                           IR_LINKAGE_CONSTANT | IR_LINKAGE_NO_IDENTITY);

	ir_initializer_t *initializer = create_initializer_tarval(tv);
	set_entity_initializer(entity, initializer);

	pmap_insert(sparc_constants, tv, entity);
	return entity;
}

static ir_node *gen_float_const(dbg_info *dbgi, ir_node *block, ir_tarval *tv)
{
	ir_entity *entity = create_float_const_entity(tv);
	ir_node   *hi     = new_bd_sparc_SetHi(dbgi, block, entity, 0);
	ir_graph  *irg    = get_irn_irg(block);
	ir_node   *mem    = get_irg_no_mem(irg);
	ir_mode   *mode   = get_tarval_mode(tv);
	ir_node   *new_op
		= create_ldf(dbgi, block, hi, mem, mode, entity, 0, false);
	ir_node   *proj   = be_new_Proj(new_op, pn_sparc_Ldf_res);

	set_irn_pinned(new_op, false);
	return proj;
}

static ir_node *create_int_const(ir_node *block, int32_t value)
{
	if (value == 0) {
		ir_graph *irg = get_irn_irg(block);
		return get_g0(irg);
	} else if (sparc_is_value_imm_encodeable(value)) {
		ir_graph *irg = get_irn_irg(block);
		return new_bd_sparc_Or_imm(NULL, block, get_g0(irg), NULL, value);
	} else {
		ir_node *hi = new_bd_sparc_SetHi(NULL, block, NULL, value);
		if ((value & 0x3ff) != 0) {
			return new_bd_sparc_Or_imm(NULL, block, hi, NULL, value & 0x3ff);
		} else {
			return hi;
		}
	}
}

static ir_node *gen_Const(ir_node *node)
{
	ir_node   *block = be_transform_nodes_block(node);
	ir_mode   *mode  = get_irn_mode(node);
	dbg_info  *dbgi  = get_irn_dbg_info(node);
	ir_tarval *tv    = get_Const_tarval(node);

	if (mode_is_float(mode)) {
		return gen_float_const(dbgi, block, tv);
	}

	assert(get_mode_size_bits(get_tarval_mode(tv)) <= 32);
	int32_t val = (int32_t)get_tarval_long(tv);
	return create_int_const(block, val);
}

static ir_node *gen_Switch(ir_node *node)
{
	dbg_info              *dbgi         = get_irn_dbg_info(node);
	ir_node               *new_block    = be_transform_nodes_block(node);
	ir_graph              *irg          = get_irn_irg(node);
	ir_node               *selector     = get_Switch_selector(node);
	ir_node               *new_selector = be_transform_node(selector);
	const ir_switch_table *table        = get_Switch_table(node);

	table = ir_switch_table_duplicate(irg, table);

	/* switch with smaller mode not implemented yet */
	assert(get_mode_size_bits(get_irn_mode(selector)) == 32);

	ir_type   *const utype = get_unknown_type();
	ir_entity *const entity
		= new_global_entity(irp->dummy_owner, id_unique("TBL"), utype,
		                    ir_visibility_private,
		                    IR_LINKAGE_CONSTANT | IR_LINKAGE_NO_IDENTITY);

	/* construct base address */
	ir_node *table_address = make_address(dbgi, new_block, entity, 0);
	/* scale index */
	ir_node *idx = new_bd_sparc_Sll_imm(dbgi, new_block, new_selector, NULL, 2);
	/* load from jumptable */
	ir_node *load = new_bd_sparc_Ld_reg(dbgi, new_block, get_irg_no_mem(irg), table_address, idx, mode_gp);
	ir_node *address = be_new_Proj(load, pn_sparc_Ld_res);

	unsigned n_outs = get_Switch_n_outs(node);
	return new_bd_sparc_SwitchJmp(dbgi, new_block, address, n_outs, table, entity);
}

static ir_node *gen_Cond(ir_node *node)
{
	/* note: after lower_mode_b we are guaranteed to have a Cmp input */
	ir_node    *selector  = get_Cond_selector(node);
	ir_node    *block     = be_transform_nodes_block(node);
	dbg_info   *dbgi      = get_irn_dbg_info(node);
	ir_node    *cmp_left  = get_Cmp_left(selector);
	ir_mode    *cmp_mode  = get_irn_mode(cmp_left);
	ir_node    *flag_node = be_transform_node(selector);
	ir_relation relation  = get_Cmp_relation(selector);
	if (mode_is_float(cmp_mode)) {
		return new_bd_sparc_fbfcc(dbgi, block, flag_node, relation);
	} else {
		bool is_unsigned = !mode_is_signed(cmp_mode);
		return new_bd_sparc_Bicc(dbgi, block, flag_node, relation, is_unsigned);
	}
}

/**
 * transform Cmp
 */
static ir_node *gen_Cmp(ir_node *node)
{
	ir_node  *op1      = get_Cmp_left(node);
	ir_node  *op2      = get_Cmp_right(node);
	ir_mode  *cmp_mode = get_irn_mode(op1);
	unsigned  bits     = get_mode_size_bits(cmp_mode);
	assert(get_irn_mode(op2) == cmp_mode);

	if (mode_is_float(cmp_mode)) {
		ir_node  *block   = be_transform_nodes_block(node);
		dbg_info *dbgi    = get_irn_dbg_info(node);
		ir_node  *new_op1 = be_transform_node(op1);
		ir_node  *new_op2 = be_transform_node(op2);
		if (bits == 32) {
			return new_bd_sparc_fcmp_s(dbgi, block, new_op1, new_op2, cmp_mode);
		} else if (bits == 64) {
			return new_bd_sparc_fcmp_d(dbgi, block, new_op1, new_op2, cmp_mode);
		} else {
			assert(bits == 128);
			return new_bd_sparc_fcmp_q(dbgi, block, new_op1, new_op2, cmp_mode);
		}
	}

	/* when we compare a bitop like and,or,... with 0 then we can directly use
	 * the bitopcc variant.
	 * Currently we only do this when we're the only user of the node...
	 */
	if (bits == 32 && is_irn_null(op2) && get_irn_n_edges(op1) == 1) {
		if (is_And(op1)) {
			ir_node *new_node = gen_helper_bitop(op1,
			                        new_bd_sparc_AndCCZero_reg,
			                        new_bd_sparc_AndCCZero_imm,
			                        new_bd_sparc_AndNCCZero_reg,
			                        new_bd_sparc_AndNCCZero_imm,
			                        MATCH_NONE);
			arch_set_irn_register(new_node, &sparc_registers[REG_PSR]);
			return new_node;
		} else if (is_Or(op1)) {
			ir_node *new_node = gen_helper_bitop(op1,
			                        new_bd_sparc_OrCCZero_reg,
			                        new_bd_sparc_OrCCZero_imm,
			                        new_bd_sparc_OrNCCZero_reg,
			                        new_bd_sparc_OrNCCZero_imm,
			                        MATCH_NONE);
			arch_set_irn_register(new_node, &sparc_registers[REG_PSR]);
			return new_node;
		} else if (is_Eor(op1)) {
			ir_node *new_node = gen_helper_bitop(op1,
			                        new_bd_sparc_XorCCZero_reg,
			                        new_bd_sparc_XorCCZero_imm,
			                        new_bd_sparc_XNorCCZero_reg,
			                        new_bd_sparc_XNorCCZero_imm,
			                        MATCH_NONE);
			arch_set_irn_register(new_node, &sparc_registers[REG_PSR]);
			return new_node;
		} else if (is_Mul(op1)) {
			ir_node *new_node = gen_helper_binop(op1, MATCH_COMMUTATIVE,
			                        new_bd_sparc_SMulCCZero_reg,
			                        new_bd_sparc_SMulCCZero_imm);
			arch_set_irn_register(new_node, &sparc_registers[REG_PSR]);
			return new_node;
		}
	}

	/* integer compare */
	return gen_helper_binop_args(node, op1, op2, MATCH_NONE,
	                             new_bd_sparc_Cmp_reg, new_bd_sparc_Cmp_imm);
}

/**
 * Transforms an Address node.
 */
static ir_node *gen_Address(ir_node *node)
{
	ir_entity *entity    = get_Address_entity(node);
	dbg_info  *dbgi      = get_irn_dbg_info(node);
	ir_node   *new_block = be_transform_nodes_block(node);
	return make_address(dbgi, new_block, entity, 0);
}

static ir_node *create_fftof(dbg_info *dbgi, ir_node *block, ir_node *op,
                             ir_mode *src_mode, ir_mode *dst_mode)
{
	unsigned src_bits = get_mode_size_bits(src_mode);
	unsigned dst_bits = get_mode_size_bits(dst_mode);
	if (src_bits == 32) {
		if (dst_bits == 64) {
			return new_bd_sparc_fftof_s_d(dbgi, block, op, src_mode, dst_mode);
		} else {
			assert(dst_bits == 128);
			return new_bd_sparc_fftof_s_q(dbgi, block, op, src_mode, dst_mode);
		}
	} else if (src_bits == 64) {
		if (dst_bits == 32) {
			return new_bd_sparc_fftof_d_s(dbgi, block, op, src_mode, dst_mode);
		} else {
			assert(dst_bits == 128);
			return new_bd_sparc_fftof_d_q(dbgi, block, op, src_mode, dst_mode);
		}
	} else {
		assert(src_bits == 128);
		if (dst_bits == 32) {
			return new_bd_sparc_fftof_q_s(dbgi, block, op, src_mode, dst_mode);
		} else {
			assert(dst_bits == 64);
			return new_bd_sparc_fftof_q_d(dbgi, block, op, src_mode, dst_mode);
		}
	}
}

static ir_node *create_ftoi(dbg_info *dbgi, ir_node *block, ir_node *op,
                            ir_mode *src_mode)
{
	unsigned  bits = get_mode_size_bits(src_mode);
	ir_node  *ftoi;
	if (bits == 32) {
		ftoi = new_bd_sparc_fftoi_s(dbgi, block, op, src_mode);
	} else if (bits == 64) {
		ftoi = new_bd_sparc_fftoi_d(dbgi, block, op, src_mode);
	} else {
		assert(bits == 128);
		ftoi = new_bd_sparc_fftoi_q(dbgi, block, op, src_mode);
	}

	ir_graph *irg   = get_irn_irg(block);
	ir_node  *sp    = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *stf   = new_bd_sparc_Stf_s(dbgi, block, ftoi, sp, nomem, mode_fp, NULL, 0, true);
	arch_add_irn_flags(stf, arch_irn_flag_spill);
	ir_node  *ld    = new_bd_sparc_Ld_imm(dbgi, block, stf, sp, mode_gp, NULL, 0, true);
	ir_node  *res   = be_new_Proj(ld, pn_sparc_Ld_res);
	set_irn_pinned(stf, false);
	set_irn_pinned(ld, false);
	return res;
}

static ir_node *create_itof(dbg_info *dbgi, ir_node *block, ir_node *op,
                            ir_mode *dst_mode)
{
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *sp    = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *st    = new_bd_sparc_St_imm(dbgi, block, nomem, op, sp, mode_gp, NULL, 0, true);
	arch_add_irn_flags(st, arch_irn_flag_spill);
	ir_node  *ldf   = new_bd_sparc_Ldf_s(dbgi, block, sp, st, mode_fp,
	                                     NULL, 0, true);
	ir_node  *res   = be_new_Proj(ldf, pn_sparc_Ldf_res);
	unsigned  bits  = get_mode_size_bits(dst_mode);
	set_irn_pinned(st, false);
	set_irn_pinned(ldf, false);

	if (bits == 32) {
		return new_bd_sparc_fitof_s(dbgi, block, res, dst_mode);
	} else if (bits == 64) {
		return new_bd_sparc_fitof_d(dbgi, block, res, dst_mode);
	} else {
		assert(bits == 128);
		return new_bd_sparc_fitof_q(dbgi, block, res, dst_mode);
	}
}

static ir_node *gen_Conv(ir_node *node)
{
	ir_node  *op       = get_Conv_op(node);
	ir_mode  *src_mode = get_irn_mode(op);
	ir_mode  *dst_mode = get_irn_mode(node);

	if (src_mode == dst_mode)
		return be_transform_node(op);

	ir_node  *block    = be_transform_nodes_block(node);
	int       src_bits = get_mode_size_bits(src_mode);
	int       dst_bits = get_mode_size_bits(dst_mode);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	if (mode_is_float(src_mode) || mode_is_float(dst_mode)) {
		assert((src_bits <= 64 && dst_bits <= 64) && "quad FP not implemented");

		ir_node *new_op = be_transform_node(op);
		if (mode_is_float(src_mode)) {
			if (mode_is_float(dst_mode)) {
				/* float -> float conv */
				return create_fftof(dbgi, block, new_op, src_mode, dst_mode);
			} else {
				/* float -> int conv */
				if (!mode_is_signed(dst_mode))
					panic("float to unsigned not lowered");
				return create_ftoi(dbgi, block, new_op, src_mode);
			}
		} else {
			/* int -> float conv */
			if (src_bits < 32) {
				new_op = gen_extension(dbgi, block, new_op, src_mode);
			} else if (src_bits == 32 && !mode_is_signed(src_mode)) {
				panic("unsigned to float not lowered");
			}
			return create_itof(dbgi, block, new_op, dst_mode);
		}
	} else { /* complete in gp registers */
		if (src_bits >= dst_bits) {
			/* kill unnecessary conv */
			return be_transform_node(op);
		}

		if (be_upper_bits_clean(op, src_mode)) {
			return be_transform_node(op);
		}
		ir_node *new_op = be_transform_node(op);

		if (mode_is_signed(src_mode)) {
			return gen_sign_extension(dbgi, block, new_op, src_bits);
		} else {
			return gen_zero_extension(dbgi, block, new_op, src_bits);
		}
	}
}

static ir_node *gen_Unknown(ir_node *node)
{
	ir_node *const block = be_transform_nodes_block(node);
	ir_mode *const mode  = get_irn_mode(node);
	if (mode_is_float(mode)) {
		return be_new_Unknown(block, &sparc_class_reg_req_fp);
	} else if (be_mode_needs_gp_reg(mode)) {
		return be_new_Unknown(block, &sparc_class_reg_req_gp);
	} else {
		panic("unexpected Unknown mode");
	}
}

/**
 * transform the start node to the prolog code
 */
static ir_node *gen_Start(ir_node *node)
{
	calling_convention_t const *const cconv = current_cconv;

	be_start_out outs[N_SPARC_REGISTERS] = {
		[REG_G0] = BE_START_IGNORE, /* the zero register */
		[REG_G7] = BE_START_IGNORE, /* g7 is used for TLS data */
		[REG_SP] = BE_START_IGNORE, /* we need an output for the stack pointer */
	};

	if (cconv->omit_fp) {
		/* We need the values of the callee saves. */
		for (size_t c = 0; c < ARRAY_SIZE(omit_fp_callee_saves); ++c) {
			outs[omit_fp_callee_saves[c]->global_index] = BE_START_REG;
		}
	} else {
		/* Non-omit-fp mode has no callee saves. */
		outs[REG_FP] = BE_START_IGNORE;
	}

	/* function parameters in registers */
	for (size_t i = 0, n = cconv->n_parameters; i != n; ++i) {
		reg_or_stackslot_t const *const param = &cconv->parameters[i];
		arch_register_t    const *const reg0  = param->reg0;
		if (reg0)
			outs[reg0->global_index] = BE_START_REG;
		arch_register_t const *const reg1 = param->reg1;
		if (reg1)
			outs[reg1->global_index] = BE_START_REG;
	}

	ir_graph *const irg = get_irn_irg(node);
	return be_new_Start(irg, outs);
}

static ir_node *get_initial_sp(ir_graph *irg)
{
	return be_get_Start_proj(irg, &sparc_registers[REG_SP]);
}

static ir_node *get_initial_fp(ir_graph *irg)
{
	return be_get_Start_proj(irg, &sparc_registers[REG_FP]);
}

/**
 * transform a Return node into epilogue code + return statement
 */
static ir_node *gen_Return(ir_node *node)
{
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *mem       = get_Return_mem(node);
	ir_node  *new_mem   = be_transform_node(mem);
	size_t    n_res     = get_Return_n_ress(node);

	/* estimate number of return values */
	unsigned p     = n_sparc_Return_first_result;
	unsigned n_ins = p + n_res;
	if (current_cconv->omit_fp)
		n_ins += ARRAY_SIZE(omit_fp_callee_saves);

	arch_register_req_t const **const reqs = be_allocate_in_reqs(irg, n_ins);
	ir_node **in = ALLOCAN(ir_node*, n_ins);

	in[n_sparc_Return_mem]   = new_mem;
	reqs[n_sparc_Return_mem] = arch_memory_req;

	in[n_sparc_Return_sp]   = get_initial_sp(irg);
	reqs[n_sparc_Return_sp] = &sparc_single_reg_req_gp_sp;

	/* result values */
	for (size_t i = 0; i < n_res; ++i) {
		ir_node                  *res_value     = get_Return_res(node, i);
		ir_node                  *new_res_value = be_transform_node(res_value);
		const reg_or_stackslot_t *slot          = &current_cconv->results[i];
		assert(slot->req1 == NULL);
		in[p]   = new_res_value;
		reqs[p] = slot->req0;
		++p;
	}
	/* callee saves */
	if (current_cconv->omit_fp) {
		for (size_t i = 0; i < ARRAY_SIZE(omit_fp_callee_saves); ++i) {
			arch_register_t const *const reg = omit_fp_callee_saves[i];
			in[p]   = be_get_Start_proj(irg, reg);
			reqs[p] = reg->single_req;
			++p;
		}
	}
	assert(p == n_ins);

	ir_node *const ret = new_bd_sparc_Return_reg(dbgi, new_block, n_ins, in, reqs);
	be_stack_record_chain(&stack_env, ret, n_sparc_Return_sp, NULL);
	return ret;
}

static ir_node *bitcast_int_to_float(dbg_info *dbgi, ir_node *block,
                                     ir_node *value0, ir_node *value1)
{
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *sp    = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *st    = new_bd_sparc_St_imm(dbgi, block, nomem, value0, sp, mode_gp, NULL, 0, true);
	arch_add_irn_flags(st, arch_irn_flag_spill);
	set_irn_pinned(st, false);

	ir_mode *mode;
	ir_node *mem;
	if (value1 != NULL) {
		ir_node *st1 = new_bd_sparc_St_imm(dbgi, block, nomem, value1, sp, mode_gp, NULL, 4, true);
		arch_add_irn_flags(st1, arch_irn_flag_spill);
		ir_node *in[2] = { st, st1 };
		ir_node *sync  = new_r_Sync(block, 2, in);
		set_irn_pinned(st1, false);
		mem  = sync;
		mode = mode_fp2;
	} else {
		mem  = st;
		mode = mode_fp;
	}

	ir_node *ldf = create_ldf(dbgi, block, sp, mem, mode, NULL, 0, true);
	set_irn_pinned(ldf, false);

	return be_new_Proj(ldf, pn_sparc_Ldf_res);
}

static void bitcast_float_to_int(dbg_info *dbgi, ir_node *block,
                                 ir_node *value, ir_mode *float_mode,
                                 ir_node **result)
{
	int bits = get_mode_size_bits(float_mode);
	if (is_Const(value)) {
		ir_tarval *tv = get_Const_tarval(value);
		int32_t const val = be_get_tv_bits32(tv, 0);
		ir_node *valc = create_int_const(block, val);
		if (bits == 64) {
			int32_t const val2 = be_get_tv_bits32(tv, 4);
			ir_node *valc2 = create_int_const(block, val2);
			result[0] = valc2;
			result[1] = valc;
		} else {
			assert(bits == 32);
			result[0] = valc;
			result[1] = NULL;
		}
	} else {
		ir_graph *irg   = get_irn_irg(block);
		ir_node  *stack = get_irg_frame(irg);
		ir_node  *nomem = get_irg_no_mem(irg);
		ir_node  *new_value = be_transform_node(value);
		ir_node  *stf   = create_stf(dbgi, block, new_value, stack, nomem,
		                             float_mode, NULL, 0, true);
		arch_add_irn_flags(stf, arch_irn_flag_spill);
		set_irn_pinned(stf, false);

		ir_node *ld = new_bd_sparc_Ld_imm(dbgi, block, stf, stack, mode_gp, NULL, 0, true);
		set_irn_pinned(ld, false);
		result[0] = be_new_Proj(ld, pn_sparc_Ld_res);

		if (bits == 64) {
			ir_node *ld2 = new_bd_sparc_Ld_imm(dbgi, block, stf, stack, mode_gp, NULL, 4, true);
			set_irn_pinned(ld, false);
			result[1] = be_new_Proj(ld2, pn_sparc_Ld_res);

			arch_add_irn_flags(ld, (arch_irn_flags_t)sparc_arch_irn_flag_needs_64bit_spillslot);
			arch_add_irn_flags(ld2, (arch_irn_flags_t)sparc_arch_irn_flag_needs_64bit_spillslot);
		} else {
			assert(bits == 32);
			result[1] = NULL;
		}
	}
}

static ir_node *gen_Call(ir_node *node)
{
	ir_graph        *irg          = get_irn_irg(node);
	ir_node         *callee       = get_Call_ptr(node);
	ir_node         *new_block    = be_transform_nodes_block(node);
	ir_node         *mem          = get_Call_mem(node);
	ir_node         *new_mem      = be_transform_node(mem);
	dbg_info        *dbgi         = get_irn_dbg_info(node);
	ir_type         *type         = get_Call_type(node);
	size_t           n_params     = get_Call_n_params(node);
	size_t           n_ress       = get_method_n_ress(type);
	/* max inputs: memory, callee, register arguments */
	ir_node        **sync_ins     = ALLOCAN(ir_node*, n_params);
	calling_convention_t *cconv
		= sparc_decide_calling_convention(type, NULL);
	size_t           n_param_regs = cconv->n_param_regs;
	/* param-regs + mem + stackpointer + callee */
	unsigned         max_inputs   = 3 + n_param_regs;
	ir_node        **in           = ALLOCAN(ir_node*, max_inputs);
	arch_register_req_t const **const in_req = be_allocate_in_reqs(irg, max_inputs);
	int              in_arity     = 0;
	int              sync_arity   = 0;
	int              n_caller_saves
		= rbitset_popcount(cconv->caller_saves, N_SPARC_REGISTERS);
	ir_entity       *entity       = NULL;
	bool             aggregate_return
		= get_method_calling_convention(type) & cc_compound_ret;

	assert(n_params == cconv->n_parameters);

	record_returns_twice(irg, type);

	/* construct arguments */

	/* memory input */
	in_req[in_arity] = arch_memory_req;
	int mem_pos      = in_arity;
	++in_arity;

	/* stack pointer input */
	/* construct an IncSP -> we have to always be sure that the stack is
	 * aligned even if we don't push arguments on it */
	ir_node *const new_frame = get_initial_sp(irg);
	ir_node *const callframe = be_new_IncSP(new_block, new_frame, cconv->param_stack_size, false);
	in_req[in_arity] = &sparc_single_reg_req_gp_sp;
	in[in_arity]     = callframe;
	++in_arity;

	/* parameters */
	for (size_t p = 0; p < n_params; ++p) {
		ir_node                  *value      = get_Call_param(node, p);
		const reg_or_stackslot_t *param      = &cconv->parameters[p];
		ir_type                  *param_type = get_method_param_type(type, p);
		ir_mode                  *mode       = get_type_mode(param_type);
		ir_node                  *partial_value;
		ir_node                  *new_values[2];
		int                       offset;

		if (mode_is_float(mode) && param->reg0 != NULL) {
			assert(get_mode_size_bits(mode) <= 64);
			bitcast_float_to_int(dbgi, new_block, value, mode, new_values);
		} else {
			ir_node *new_value = be_transform_node(value);
			new_values[0] = new_value;
			new_values[1] = NULL;
		}

		/* put value into registers */
		if (param->reg0 != NULL) {
			in[in_arity]     = new_values[0];
			in_req[in_arity] = param->reg0->single_req;
			++in_arity;
			if (new_values[1] == NULL)
				continue;
		}
		if (param->reg1 != NULL) {
			assert(new_values[1] != NULL);
			in[in_arity]     = new_values[1];
			in_req[in_arity] = param->reg1->single_req;
			++in_arity;
			continue;
		}

		/* we need a store if we're here */
		if (new_values[1] != NULL) {
			partial_value = new_values[1];
			mode          = mode_gp;
		} else {
			partial_value = new_values[0];
		}

		/* we need to skip over our save area when constructing the call
		 * arguments on stack */
		offset = param->offset;

		ir_node *const str = mode_is_float(mode) ?
			create_stf(         dbgi, new_block, partial_value, callframe, new_mem, mode, NULL, offset, true) :
			new_bd_sparc_St_imm(dbgi, new_block, new_mem, partial_value, callframe, mode, NULL, offset, true);
		set_irn_pinned(str, false);
		sync_ins[sync_arity++] = str;
	}

	/* construct memory input */
	if (sync_arity == 0) {
		in[mem_pos] = new_mem;
	} else {
		in[mem_pos] = be_make_Sync(new_block, sync_arity, sync_ins);
	}

	if (is_Address(callee)) {
		entity = get_Address_entity(callee);
	} else {
		in[in_arity]     = be_transform_node(callee);
		in_req[in_arity] = &sparc_class_reg_req_gp;
		++in_arity;
	}
	assert(in_arity <= (int)max_inputs);

	/* Count outputs. */
	unsigned       o         = pn_sparc_Call_first_result;
	unsigned const out_arity = o + cconv->n_reg_results + n_caller_saves;

	/* create call node */
	ir_node *const res = entity ?
		new_bd_sparc_Call_imm(dbgi, new_block, in_arity, in, in_req, out_arity, entity, 0, aggregate_return) :
		new_bd_sparc_Call_reg(dbgi, new_block, in_arity, in, in_req, out_arity,            aggregate_return);

	/* create output register reqs */
	arch_set_irn_register_req_out(res, pn_sparc_Call_M, arch_memory_req);
	arch_copy_irn_out_info(res, pn_sparc_Call_stack, callframe);

	/* add register requirements for the result regs */
	for (size_t r = 0; r < n_ress; ++r) {
		const reg_or_stackslot_t  *result_info = &cconv->results[r];
		const arch_register_req_t *req         = result_info->req0;
		if (req != NULL) {
			arch_set_irn_register_req_out(res, o++, req);
		}
		assert(result_info->req1 == NULL);
	}
	const unsigned *allocatable_regs = be_birg_from_irg(irg)->allocatable_regs;
	for (size_t i = 0; i < N_SPARC_REGISTERS; ++i) {
		const arch_register_t *reg;
		if (!rbitset_is_set(cconv->caller_saves, i))
			continue;
		reg = &sparc_registers[i];
		arch_set_irn_register_req_out(res, o, reg->single_req);
		if (!rbitset_is_set(allocatable_regs, reg->global_index))
			arch_set_irn_register_out(res, o, reg);
		++o;
	}
	assert(o == out_arity);

	/* copy pinned attribute */
	set_irn_pinned(res, get_irn_pinned(node));

	/* IncSP to destroy the call stackframe */
	ir_node *const call_stack = be_new_Proj(res, pn_sparc_Call_stack);
	ir_node *const incsp      = be_new_IncSP(new_block, call_stack, -cconv->param_stack_size, false);
	be_stack_record_chain(&stack_env, callframe, n_be_IncSP_pred, incsp);

	sparc_free_calling_convention(cconv);
	return res;
}

static ir_node *gen_Member(ir_node *node)
{
	dbg_info  *dbgi      = get_irn_dbg_info(node);
	ir_node   *new_block = be_transform_nodes_block(node);
	ir_node   *ptr       = get_Member_ptr(node);
	ir_node   *new_ptr   = be_transform_node(ptr);
	ir_entity *entity    = get_Member_entity(node);

	/* must be the frame pointer all other sels must have been lowered
	 * already */
	assert(is_Proj(ptr) && is_Start(get_Proj_pred(ptr)));

	return new_bd_sparc_FrameAddr(dbgi, new_block, new_ptr, entity, 0);
}

static ir_node *gen_Alloc(ir_node *node)
{
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node  *new_block  = be_transform_nodes_block(node);
	ir_node  *size       = get_Alloc_size(node);
	ir_graph *irg        = get_irn_irg(node);
	ir_node  *stack_pred = get_initial_sp(irg);
	ir_node  *mem        = get_Alloc_mem(node);
	ir_node  *new_mem    = be_transform_node(mem);

	ir_node *subsp;
	if (is_Const(size)) {
		long const sizel = get_Const_long(size);
		assert((sizel & (SPARC_STACK_ALIGNMENT - 1)) == 0 && "Found Alloc with misaligned constant");
		subsp = new_bd_sparc_SubSP_imm(dbgi, new_block, new_mem, stack_pred, NULL, sizel);
	} else {
		ir_node *new_size = be_transform_node(size);
		subsp = new_bd_sparc_SubSP_reg(dbgi, new_block, new_mem, stack_pred, new_size);
	}

	ir_node *const stack_proj = be_new_Proj_reg(subsp, pn_sparc_SubSP_stack, &sparc_registers[REG_SP]);
	be_stack_record_chain(&stack_env, subsp, n_sparc_SubSP_stack, stack_proj);

	return subsp;
}

static ir_node *gen_Proj_Alloc(ir_node *node)
{
	ir_node *alloc     = get_Proj_pred(node);
	ir_node *new_alloc = be_transform_node(alloc);
	unsigned pn        = get_Proj_num(node);

	switch ((pn_Alloc)pn) {
	case pn_Alloc_M:   return be_new_Proj(new_alloc, pn_sparc_SubSP_M);
	case pn_Alloc_res: return be_new_Proj(new_alloc, pn_sparc_SubSP_addr);
	}
	panic("invalid Proj->Alloc");
}

static ir_node *gen_Free(ir_node *node)
{
	(void)node;
	panic("Free not supported yet");
}

static const arch_register_req_t float1_req = {
	.cls   = &sparc_reg_classes[CLASS_sparc_fp],
	.width = 1,
};
static const arch_register_req_t float2_req = {
	.cls   = &sparc_reg_classes[CLASS_sparc_fp],
	.width = 2,
};
static const arch_register_req_t float4_req = {
	.cls   = &sparc_reg_classes[CLASS_sparc_fp],
	.width = 4,
};

static const arch_register_req_t *get_float_req(ir_mode *mode)
{
	assert(mode_is_float(mode));
	switch (get_mode_size_bits(mode)) {
		case  32: return &float1_req;
		case  64: return &float2_req;
		case 128: return &float4_req;
		default:  panic("invalid float mode");
	}
}

static ir_node *gen_Phi(ir_node *node)
{
	ir_mode                   *mode = get_irn_mode(node);
	const arch_register_req_t *req;
	if (be_mode_needs_gp_reg(mode)) {
		/* we shouldn't have any 64bit stuff around anymore */
		assert(get_mode_size_bits(mode) <= 32);
		/* all integer operations are on 32bit registers now */
		req  = &sparc_class_reg_req_gp;
	} else if (mode_is_float(mode)) {
		req  = get_float_req(mode);
	} else {
		req = arch_memory_req;
	}

	return be_transform_phi(node, req);
}

/*
 * Transform saturating increment.
 */
static ir_node *gen_saturating_increment(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = be_transform_nodes_block(node);
	ir_node  *operand   = be_transform_node(get_Builtin_param(node, 0));
	ir_node  *increment = new_bd_sparc_AddCC_imm(dbgi, block, operand, NULL, 1);
	ir_node  *value     = be_new_Proj(increment, pn_sparc_AddCC_res);
	ir_node  *eflags    = be_new_Proj(increment, pn_sparc_AddCC_flags);
	ir_graph *irg       = get_irn_irg(block);
	ir_node  *zero      = get_g0(irg);
	ir_node  *sbb       = new_bd_sparc_SubX_reg(dbgi, block, value, zero, eflags);

	return sbb;
}

static ir_node *gen_compare_swap(ir_node *node)
{
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *block   = be_transform_nodes_block(node);
	ir_node  *ptr     = get_Builtin_param(node, 0);
	ir_node  *new_ptr = be_transform_node(ptr);
	ir_node  *old     = get_Builtin_param(node, 1);
	ir_node  *new_old = be_transform_node(old);
	ir_node  *new     = get_Builtin_param(node, 2);
	ir_node  *new_new = be_transform_node(new);
	ir_node  *mem     = get_Builtin_mem(node);
	ir_node  *new_mem = be_transform_node(mem);
	ir_node  *stbar   = new_bd_sparc_Stbar(dbgi, block, new_mem);
	ir_node  *cas     = new_bd_sparc_Cas(dbgi, block, new_ptr, new_old,
	                                     new_new, stbar);
	op_pin_state pinned = get_irn_pinned(node);
	set_irn_pinned(stbar, pinned);
	set_irn_pinned(cas, pinned);

	ir_mode *mode = get_irn_mode(old);
	assert(get_irn_mode(new) == mode);
	if (!be_mode_needs_gp_reg(mode) || get_mode_size_bits(mode) != 32)
		panic("compare and swap only allowed for 32bit values");

	return cas;
}

static ir_node *get_frame_base(ir_graph *irg)
{
	if (frame_base == NULL) {
		if (current_cconv->omit_fp) {
			frame_base = get_initial_sp(irg);
		} else {
			frame_base = get_initial_fp(irg);
		}
	}
	return frame_base;
}

static ir_node *gen_va_start(ir_node *node)
{
	if (initial_va_list == NULL) {
		dbg_info  *dbgi   = get_irn_dbg_info(node);
		ir_graph  *irg    = get_irn_irg(node);
		ir_node   *block  = get_irg_start_block(irg);
		ir_entity *entity = current_cconv->va_start_addr;
		ir_node   *frame  = get_frame_base(irg);
		ir_node   *ap     = new_bd_sparc_FrameAddr(dbgi, block, frame, entity, 0);

		initial_va_list = ap;
	}

	return initial_va_list;
}

/**
 * Transform Builtin node.
 */
static ir_node *gen_Builtin(ir_node *node)
{
	ir_builtin_kind kind = get_Builtin_kind(node);

	switch (kind) {
	case ir_bk_bswap:
	case ir_bk_clz:
	case ir_bk_ctz:
	case ir_bk_ffs:
	case ir_bk_parity:
	case ir_bk_popcount:
	case ir_bk_prefetch:
		panic("builtin not lowered(%+F)", node);

	case ir_bk_trap:
	case ir_bk_debugbreak:
	case ir_bk_return_address:
	case ir_bk_frame_address:
	case ir_bk_outport:
	case ir_bk_inport:
		/* not supported */
		break;
	case ir_bk_compare_swap:
		return gen_compare_swap(node);
	case ir_bk_saturating_increment:
		return gen_saturating_increment(node);
	case ir_bk_va_start:
		return gen_va_start(node);
	case ir_bk_may_alias:
	case ir_bk_va_arg:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

/**
 * Transform Proj(Builtin) node.
 */
static ir_node *gen_Proj_Builtin(ir_node *proj)
{
	ir_node         *pred     = get_Proj_pred(proj);
	ir_node         *new_pred = be_transform_node(pred);
	ir_builtin_kind  kind     = get_Builtin_kind(pred);
	unsigned         pn       = get_Proj_num(proj);

	switch (kind) {
	case ir_bk_return_address:
	case ir_bk_frame_address:
	case ir_bk_ffs:
	case ir_bk_clz:
	case ir_bk_ctz:
	case ir_bk_parity:
	case ir_bk_popcount:
	case ir_bk_bswap:
	case ir_bk_trap:
	case ir_bk_debugbreak:
	case ir_bk_prefetch:
	case ir_bk_outport:
	case ir_bk_inport:
		/* not supported / should be lowered */
		break;
	case ir_bk_saturating_increment:
		assert(pn == pn_Builtin_max+1);
		return new_pred;
	case ir_bk_compare_swap:
		if (pn == pn_Builtin_M) {
			return be_new_Proj(new_pred, pn_sparc_Cas_M);
		} else {
			assert(pn == pn_Builtin_max+1);
			return be_new_Proj(new_pred, pn_sparc_Cas_res);
		}
	case ir_bk_va_start:
		if (pn == pn_Builtin_M) {
			return be_transform_node(get_Builtin_mem(pred));
		} else {
			assert(pn == pn_Builtin_max+1);
			return new_pred;
		}
	case ir_bk_may_alias:
	case ir_bk_va_arg:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

/**
 * Transform Bitcast node.
 */
static ir_node *gen_Bitcast(ir_node *node)
{
	ir_mode  *dst_mode  = get_irn_mode(node);
	ir_node  *op        = get_Bitcast_op(node);
	ir_mode  *src_mode  = get_irn_mode(op);
	ir_node  *new_op    = be_transform_node(op);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_graph *irg       = get_irn_irg(new_block);
	ir_node  *sp        = get_irg_frame(irg);
	ir_node  *nomem     = get_irg_no_mem(irg);

	switch (get_mode_arithmetic(src_mode)) {
	case irma_twos_complement: {
		assert(get_mode_arithmetic(dst_mode) == irma_ieee754);

		ir_node *st  = new_bd_sparc_St_imm(dbgi, new_block, nomem, new_op, sp, src_mode, NULL, 0, true);
		arch_add_irn_flags(st, arch_irn_flag_spill);
		ir_node *const ldf = create_ldf(dbgi, new_block, sp, st, dst_mode, NULL, 0, true);
		ir_node *const res = be_new_Proj(ldf, pn_sparc_Ldf_res);
		set_irn_pinned(st, false);
		set_irn_pinned(ldf, false);
		return res;
	}
	case irma_ieee754: {
		assert(get_mode_arithmetic(dst_mode) == irma_twos_complement);

		ir_node *stf = create_stf(dbgi, new_block, new_op, sp, nomem, src_mode,
		                          NULL, 0, true);
		arch_add_irn_flags(stf, arch_irn_flag_spill);
		ir_node *const ld  = new_bd_sparc_Ld_imm(dbgi, new_block, stf, sp, dst_mode, NULL, 0, true);
		ir_node *const res = be_new_Proj(ld, pn_sparc_Ld_res);
		set_irn_pinned(stf, false);
		set_irn_pinned(ld, false);
		return res;
	}
	default:
		panic("unexpected src mode in Bitcast");
	}
}

/**
 * Transform a Proj from a Load.
 */
static ir_node *gen_Proj_Load(ir_node *node)
{
	ir_node  *load     = get_Proj_pred(node);
	ir_node  *new_load = be_transform_node(load);
	unsigned  pn       = get_Proj_num(node);

	/* renumber the proj */
	switch (get_sparc_irn_opcode(new_load)) {
	case iro_sparc_Ld:
		/* handle all gp loads equal: they have the same proj numbers. */
		if (pn == pn_Load_res) {
			return be_new_Proj(new_load, pn_sparc_Ld_res);
		} else if (pn == pn_Load_M) {
			return be_new_Proj(new_load, pn_sparc_Ld_M);
		}
		break;
	case iro_sparc_Ldf:
		if (pn == pn_Load_res) {
			return be_new_Proj(new_load, pn_sparc_Ldf_res);
		} else if (pn == pn_Load_M) {
			return be_new_Proj(new_load, pn_sparc_Ldf_M);
		}
		break;
	default:
		break;
	}
	panic("unsupported Proj from Load");
}

static ir_node *gen_Proj_Store(ir_node *node)
{
	ir_node  *store     = get_Proj_pred(node);
	ir_node  *new_store = be_transform_node(store);
	unsigned  pn        = get_Proj_num(node);

	/* renumber the proj */
	switch (get_sparc_irn_opcode(new_store)) {
	case iro_sparc_St:
	case iro_sparc_Stf:
		if (pn == pn_Store_M)
			return new_store;
		break;
	default:
		break;
	}
	panic("unsupported Proj from Store");
}

/**
 * transform Projs from a Div
 */
static ir_node *gen_Proj_Div(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);

	assert((unsigned)pn_sparc_SDiv_res == (unsigned)pn_sparc_UDiv_res);
	assert((unsigned)pn_sparc_SDiv_M   == (unsigned)pn_sparc_UDiv_M);
	assert((unsigned)pn_sparc_SDiv_res == (unsigned)pn_sparc_fdiv_res);
	assert((unsigned)pn_sparc_SDiv_M   == (unsigned)pn_sparc_fdiv_M);
	unsigned pn = get_Proj_num(node);
	switch (pn) {
	case pn_Div_res:
		return be_new_Proj(new_pred, pn_sparc_SDiv_res);
	case pn_Div_M:
		return be_new_Proj(new_pred, pn_sparc_SDiv_M);
	default:
		break;
	}
	panic("unsupported Proj from Div");
}

static ir_node *gen_Proj_Start(ir_node *node)
{
	unsigned pn = get_Proj_num(node);
	/* make sure prolog is constructed */
	be_transform_node(get_Proj_pred(node));

	switch ((pn_Start) pn) {
	case pn_Start_M: {
		ir_graph *irg = get_irn_irg(node);
		ir_node  *mem = be_get_Start_mem(irg);
		keep_alive(mem);
		return mem;
	}
	case pn_Start_T_args:
		return new_r_Bad(get_irn_irg(node), mode_T);
	case pn_Start_P_frame_base:
		return get_frame_base(get_irn_irg(node));
	}
	panic("unexpected start proj: %u", pn);
}

static ir_node *gen_Proj_Proj_Start(ir_node *node)
{
	/* Proj->Proj->Start must be a method argument */
	assert(get_Proj_num(get_Proj_pred(node)) == pn_Start_T_args);

	ir_graph                 *const irg       = get_irn_irg(node);
	ir_node                  *const new_block = be_transform_nodes_block(node);
	unsigned                  const pn        = get_Proj_num(node);
	reg_or_stackslot_t const *const param     = &current_cconv->parameters[pn];
	arch_register_t    const *const reg0      = param->reg0;
	if (reg0) {
		/* argument transmitted in register */
		ir_node *value    = be_get_Start_proj(irg, reg0);
		bool     is_float = false;

		ir_entity *entity      = get_irg_entity(irg);
		ir_type   *method_type = get_entity_type(entity);
		if (pn < get_method_n_params(method_type)) {
			ir_type *param_type = get_method_param_type(method_type, pn);
			ir_mode *mode       = get_type_mode(param_type);
			is_float = mode_is_float(mode);
		}

		if (is_float) {
			const arch_register_t *reg1 = param->reg1;
			ir_node *value1 = NULL;

			if (reg1 != NULL) {
				value1 = be_get_Start_proj(irg, reg1);
			} else if (param->entity != NULL) {
				ir_node *frame = get_irg_frame(irg);
				ir_node *mem   = be_get_Start_mem(irg);
				ir_node *ld    = new_bd_sparc_Ld_imm(NULL, new_block, mem, frame, mode_gp, param->entity, 0, true);
				value1 = be_new_Proj(ld, pn_sparc_Ld_res);
			}

			/* convert integer value to float */
			value = bitcast_int_to_float(NULL, new_block, value, value1);
		}
		return value;
	} else {
		/* argument transmitted on stack */
		ir_node *mem  = be_get_Start_mem(irg);
		ir_mode *mode = get_type_mode(param->type);
		ir_node *base = get_frame_base(irg);

		ir_node *load;
		ir_node *value;
		if (mode_is_float(mode)) {
			load  = create_ldf(NULL, new_block, base, mem, mode,
			                   param->entity, 0, true);
			value = be_new_Proj(load, pn_sparc_Ldf_res);
		} else {
			load  = new_bd_sparc_Ld_imm(NULL, new_block, mem, base, mode, param->entity, 0, true);
			value = be_new_Proj(load, pn_sparc_Ld_res);
		}
		set_irn_pinned(load, false);

		return value;
	}
}

static ir_node *gen_Proj_Call(ir_node *node)
{
	unsigned pn        = get_Proj_num(node);
	ir_node *call      = get_Proj_pred(node);
	ir_node *new_call  = be_transform_node(call);

	switch ((pn_Call) pn) {
	case pn_Call_M:
		return be_new_Proj(new_call, pn_sparc_Call_M);
	case pn_Call_X_regular:
	case pn_Call_X_except:
	case pn_Call_T_result:
		break;
	}
	panic("unexpected Call proj %u", pn);
}

static ir_node *gen_Proj_Proj_Call(ir_node *node)
{
	ir_node *const call     = get_Proj_pred(get_Proj_pred(node));
	ir_node *const new_call = be_transform_node(call);
	unsigned const pn       = get_Proj_num(node);
	unsigned const new_pn   = pn_sparc_Call_first_result + pn;
	return be_new_Proj(new_call, new_pn);
}

static ir_node *gen_Proj_Proj(ir_node *node)
{
	ir_node *pred      = get_Proj_pred(node);
	ir_node *pred_pred = get_Proj_pred(pred);
	if (is_Call(pred_pred)) {
		return gen_Proj_Proj_Call(node);
	} else if (is_Start(pred_pred)) {
		return gen_Proj_Proj_Start(node);
	}
	panic("code selection didn't expect Proj(Proj) after %+F", pred_pred);
}

static ir_node *gen_IJmp(ir_node *node)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const op    = get_IJmp_target(node);
	address_t       address;
	match_address(op, &address, true);
	if (address.ptr2) {
		assert(!address.entity && address.offset == 0);
		return new_bd_sparc_IJmp_reg(dbgi, block, address.ptr, address.ptr2);
	} else {
		return new_bd_sparc_IJmp_imm(dbgi, block, address.ptr, address.entity, address.offset, false);
	}
}

/**
 * transform a Jmp
 */
static ir_node *gen_Jmp(ir_node *node)
{
	ir_node  *new_block = be_transform_nodes_block(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);

	return new_bd_sparc_Ba(dbgi, new_block);
}

/**
 * configure transformation callbacks
 */
static void sparc_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_ASM,          gen_ASM);
	be_set_transform_function(op_Add,          gen_Add);
	be_set_transform_function(op_Address,      gen_Address);
	be_set_transform_function(op_Alloc,        gen_Alloc);
	be_set_transform_function(op_And,          gen_And);
	be_set_transform_function(op_Builtin,      gen_Builtin);
	be_set_transform_function(op_Bitcast,      gen_Bitcast);
	be_set_transform_function(op_Call,         gen_Call);
	be_set_transform_function(op_Cmp,          gen_Cmp);
	be_set_transform_function(op_Cond,         gen_Cond);
	be_set_transform_function(op_Const,        gen_Const);
	be_set_transform_function(op_Conv,         gen_Conv);
	be_set_transform_function(op_Div,          gen_Div);
	be_set_transform_function(op_Eor,          gen_Eor);
	be_set_transform_function(op_Free,         gen_Free);
	be_set_transform_function(op_IJmp,         gen_IJmp);
	be_set_transform_function(op_Jmp,          gen_Jmp);
	be_set_transform_function(op_Load,         gen_Load);
	be_set_transform_function(op_Member,       gen_Member);
	be_set_transform_function(op_Minus,        gen_Minus);
	be_set_transform_function(op_Mul,          gen_Mul);
	be_set_transform_function(op_Mulh,         gen_Mulh);
	be_set_transform_function(op_Not,          gen_Not);
	be_set_transform_function(op_Or,           gen_Or);
	be_set_transform_function(op_Phi,          gen_Phi);
	be_set_transform_function(op_Return,       gen_Return);
	be_set_transform_function(op_Shl,          gen_Shl);
	be_set_transform_function(op_Shr,          gen_Shr);
	be_set_transform_function(op_Shrs,         gen_Shrs);
	be_set_transform_function(op_Start,        gen_Start);
	be_set_transform_function(op_Store,        gen_Store);
	be_set_transform_function(op_Sub,          gen_Sub);
	be_set_transform_function(op_Switch,       gen_Switch);
	be_set_transform_function(op_Unknown,      gen_Unknown);

	be_set_transform_function(op_sparc_AddX_t, gen_AddX_t);
	be_set_transform_function(op_sparc_AddCC_t,gen_AddCC_t);
	be_set_transform_function(op_sparc_SubX_t, gen_SubX_t);
	be_set_transform_function(op_sparc_SubCC_t,gen_SubCC_t);

	be_set_transform_proj_function(op_Alloc,         gen_Proj_Alloc);
	be_set_transform_proj_function(op_Builtin,       gen_Proj_Builtin);
	be_set_transform_proj_function(op_Call,          gen_Proj_Call);
	be_set_transform_proj_function(op_Div,           gen_Proj_Div);
	be_set_transform_proj_function(op_Load,          gen_Proj_Load);
	be_set_transform_proj_function(op_Proj,          gen_Proj_Proj);
	be_set_transform_proj_function(op_sparc_AddCC_t, gen_Proj_AddCC_t);
	be_set_transform_proj_function(op_sparc_SubCC_t, gen_Proj_SubCC_t);
	be_set_transform_proj_function(op_Start,         gen_Proj_Start);
	be_set_transform_proj_function(op_Store,         gen_Proj_Store);
}

/**
 * Transform a Firm graph into a SPARC graph.
 */
void sparc_transform_graph(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_TUPLES
	                         | IR_GRAPH_PROPERTY_NO_BADS);

	sparc_register_transformers();

	mode_gp    = sparc_reg_classes[CLASS_sparc_gp].mode;
	mode_fp    = sparc_reg_classes[CLASS_sparc_fp].mode;
	mode_fp2   = mode_D;
	//mode_fp4 = ?

	frame_base = NULL;

	be_stack_init(&stack_env);
	current_cconv = sparc_prepare_calling_convention(irg);

	ir_entity *need_stores[current_cconv->n_param_regs];
	unsigned   n_stores = 0;
	for (size_t i = 0, n = current_cconv->n_parameters; i < n; ++i) {
		reg_or_stackslot_t const *const param  = &current_cconv->parameters[i];
		if (param->already_stored)
			continue;
		ir_entity *const entity = param->entity;
		if (entity == NULL)
			continue;
		assert(n_stores < current_cconv->n_param_regs);
		need_stores[n_stores++] = entity;
	}
	be_add_parameter_entity_stores_list(irg, n_stores, need_stores);

	be_transform_graph(irg, NULL);

	be_stack_finish(&stack_env);
	sparc_free_calling_convention(current_cconv);

	initial_va_list = NULL;

	/* do code placement, to optimize the position of constants */
	place_code(irg);
	/* backend expects outedges to be always on */
	assure_edges(irg);
}

void sparc_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.sparc.transform");
}
