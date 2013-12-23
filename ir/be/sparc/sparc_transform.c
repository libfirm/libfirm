/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   code selection (transform FIRM into SPARC FIRM)
 * @author  Hannes Rapp, Matthias Braun
 */
#include <stdint.h>
#include <stdbool.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "iredges.h"
#include "ircons.h"
#include "iroptimize.h"
#include "dbginfo.h"
#include "iropt_t.h"
#include "debug.h"
#include "error.h"
#include "util.h"

#include "benode.h"
#include "beirg.h"
#include "beutil.h"
#include "betranshlp.h"
#include "beabihelper.h"
#include "bearch_sparc_t.h"

#include "sparc_nodes_attr.h"
#include "sparc_transform.h"
#include "sparc_new_nodes.h"
#include "gen_sparc_new_nodes.h"

#include "gen_sparc_regalloc_if.h"
#include "sparc_cconv.h"

#include <limits.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct reg_info_t {
	size_t   offset;
	ir_node *irn;
} reg_info_t;

static const arch_register_t *sp_reg = &sparc_registers[REG_SP];
static const arch_register_t *fp_reg = &sparc_registers[REG_FRAME_POINTER];
static calling_convention_t  *current_cconv = NULL;
static be_stackorder_t       *stackorder;
static ir_mode               *mode_gp;
static ir_mode               *mode_flags;
static ir_mode               *mode_fp;
static ir_mode               *mode_fp2;
//static ir_mode               *mode_fp4;
static pmap                  *node_to_stack;
static reg_info_t             start_mem;
static reg_info_t             start_g0;
static reg_info_t             start_g7;
static reg_info_t             start_sp;
static reg_info_t             start_fp;
static ir_node               *frame_base;
static size_t                 start_params_offset;
static size_t                 start_callee_saves_offset;

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

static inline bool mode_needs_gp_reg(ir_mode *mode)
{
	if (mode_is_int(mode) || mode_is_reference(mode)) {
		/* we should only see 32bit code */
		assert(get_mode_size_bits(mode) <= 32);
		return true;
	}
	return false;
}

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
	MATCH_SIGN_EXT_LEFT = 1U << 7, /**< we need to sign_extend the left operand
	                                    (for cases wheew mode < 32bit) */
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

	long value = get_tarval_long(get_Const_tarval(node));
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

/**
 * Check, if a given node is a Down-Conv, i.e. a integer Conv
 * from a mode with a mode with more bits to a mode with lesser bits.
 * Moreover, we return only true if the node has not more than 1 user.
 *
 * @param node   the node
 * @return non-zero if node is a Down-Conv
 */
static bool is_downconv(const ir_node *node)
{
	if (!is_Conv(node))
		return false;

	ir_mode *src_mode  = get_irn_mode(get_Conv_op(node));
	ir_mode *dest_mode = get_irn_mode(node);
	return
		mode_needs_gp_reg(src_mode)  &&
		mode_needs_gp_reg(dest_mode) &&
		get_mode_size_bits(dest_mode) <= get_mode_size_bits(src_mode);
}

static ir_node *skip_downconv(ir_node *node)
{
	while (is_downconv(node)) {
		node = get_Conv_op(node);
	}
	return node;
}

/**
 * An assembler constraint.
 */
typedef struct constraint_t {
	const arch_register_class_t *cls;
	char                         all_registers_allowed;
	char                         immediate_type;
	int                          same_as;
} constraint_t;

static void parse_asm_constraints(constraint_t *const constraint,
                                  ident *const constraint_text,
                                  bool const is_output)
{
	memset(constraint, 0, sizeof(constraint[0]));
	constraint->same_as = -1;

	char const *c = get_id_str(constraint_text);
	if (*c == '\0') {
		/* a memory constraint: no need to do anything in backend about it
		 * (the dependencies are already respected by the memory edge of
		 * the node) */
		return;
	}

	char                         immediate_type        = '\0';
	arch_register_class_t const *cls                   = NULL;
	bool                         all_registers_allowed = false;
	int                          same_as               = -1;
	while (*c != 0) {
		arch_register_class_t const *new_cls = NULL;
		char                         new_imm = '\0';
		switch (*c) {
		/* Skip spaces, out/in-out marker */
		case ' ':
		case '\t':
		case '\n':
		case '=':
		case '+':
		case '&': break;
		case '*':
			++c;
			break;
		case '#':
			while (*c != 0 && *c != ',')
				++c;
			break;
		case 'r':
			new_cls               = &sparc_reg_classes[CLASS_sparc_gp];
			all_registers_allowed = true;
			break;
		case 'e':
		case 'f':
			new_cls               = &sparc_reg_classes[CLASS_sparc_fp];
			all_registers_allowed = true;
			break;
		case 'A':
		case 'I':
			new_cls = &sparc_reg_classes[CLASS_sparc_gp];
			new_imm = *c;
			break;

		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9': {
			if (is_output)
				panic("can only specify same constraint on input");

			int p;
			sscanf(c, "%d%n", &same_as, &p);
			if (same_as >= 0) {
				c += p;
				continue;
			}
			break;
		}

		default:
			panic("unknown asm constraint '%c' found in (%+F)", *c, current_ir_graph);
		}

		if (new_cls) {
			if (!cls) {
				cls = new_cls;
			} else if (cls != new_cls) {
				panic("multiple register classes not supported");
			}
		}

		if (new_imm != '\0') {
			if (immediate_type == '\0') {
				immediate_type = new_imm;
			} else if (immediate_type != new_imm) {
				panic("multiple immediate types not supported");
			}
		}

		++c;
	}

	if (same_as >= 0) {
		if (cls != NULL)
			panic("same as and register constraint not supported");
		if (immediate_type != '\0')
			panic("same as and immediate constraint not supported");
	}

	if (!cls && same_as < 0)
		panic("no constraint specified for assembler input");

	constraint->same_as               = same_as;
	constraint->cls                   = cls;
	constraint->all_registers_allowed = all_registers_allowed;
	constraint->immediate_type        = immediate_type;
}

static const arch_register_t *find_register(const char *name)
{
	for (size_t i = 0; i < N_SPARC_REGISTERS; ++i) {
		const arch_register_t *const reg = &sparc_registers[i];
		if (strcmp(reg->name, name) == 0)
			return reg;
	}
	return NULL;
}

static arch_register_req_t const *make_register_req(ir_graph *const irg,
	constraint_t const *const c, int const n_outs,
	arch_register_req_t const **const out_reqs, int const pos)
{
	int const same_as = c->same_as;
	if (same_as >= 0) {
		if (same_as >= n_outs)
			panic("invalid output number in same_as constraint");

		struct obstack            *const obst  = get_irg_obstack(irg);
		arch_register_req_t       *const req   = OALLOC(obst, arch_register_req_t);
		arch_register_req_t const *const other = out_reqs[same_as];
		*req            = *other;
		req->type      |= arch_register_req_type_should_be_same;
		req->other_same = 1U << pos;

		/* Switch constraints. This is because in firm we have same_as
		 * constraints on the output constraints while in the gcc asm syntax
		 * they are specified on the input constraints. */
		out_reqs[same_as] = req;
		return other;
	}

	return c->cls->class_req;
}

void sparc_init_asm_constraints(void)
{
	static unsigned char const register_flags[] = {
		'r', 'e', 'f', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
	};
	for (size_t i = 0; i < ARRAY_SIZE(register_flags); ++i) {
		unsigned char const c = register_flags[i];
		asm_constraint_flags[c] = ASM_CONSTRAINT_FLAG_SUPPORTS_REGISTER;
	}

	asm_constraint_flags['A'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;
	asm_constraint_flags['I'] = ASM_CONSTRAINT_FLAG_SUPPORTS_IMMEDIATE;

	/* Note there are many more flags in gcc which we can't properly support
	 * at the moment. see gcc/config/sparc/constraints.md */
}

int sparc_is_valid_clobber(const char *clobber)
{
	return strcmp(clobber, "memory") == 0 || strcmp(clobber, "cc") == 0;
}

static ir_node *gen_ASM(ir_node *node)
{
	int       n_inputs     = get_ASM_n_inputs(node);
	size_t    n_clobbers   = 0;
	ident   **clobbers     = get_ASM_clobbers(node);
	ir_graph *irg          = get_irn_irg(node);
	//unsigned  clobber_bits[BITSET_SIZE_ELEMS(N_SPARC_REGISTERS)];

	for (size_t c = 0; c < get_ASM_n_clobbers(node); ++c) {
		const char *const clobber = get_id_str(clobbers[c]);
		if (strcmp(clobber, "memory") == 0)
			continue;
		if (strcmp(clobber, "cc") == 0) {
			n_clobbers += 2;
			continue;
		}

		const arch_register_t *reg = find_register(clobber);
		if (reg == NULL)
			panic("invalid clobber in sparc asm");

#if 0
		rbitset_set(clobber_bits, reg->global_index);
		++n_clobbers;
#else
		panic("clobbers not correctly supported yet");
#endif
	}
	size_t n_out_constraints = get_ASM_n_output_constraints(node);
	size_t n_outs            = n_out_constraints + n_clobbers;

	const ir_asm_constraint *in_constraints  = get_ASM_input_constraints(node);
	const ir_asm_constraint *out_constraints = get_ASM_output_constraints(node);

	/* determine number of operands */
	unsigned n_operands = 0;
	for (size_t out_idx = 0; out_idx < n_out_constraints; ++out_idx) {
		const ir_asm_constraint *constraint = &out_constraints[out_idx];
		if (constraint->pos+1 > n_operands)
			n_operands = constraint->pos+1;
	}
	for (int i = 0; i < n_inputs; ++i) {
		const ir_asm_constraint *constraint = &in_constraints[i];
		if (constraint->pos+1 > n_operands)
			n_operands = constraint->pos+1;
	}

	struct obstack      *const obst = get_irg_obstack(irg);
	sparc_asm_operand_t *const operands
		= NEW_ARR_DZ(sparc_asm_operand_t, obst, n_operands);

	/* construct output constraints */
	size_t                      out_size = n_outs + 1;
	const arch_register_req_t **out_reg_reqs
		= OALLOCN(obst, const arch_register_req_t*, out_size);

	size_t out_idx;
	for (out_idx = 0; out_idx < n_out_constraints; ++out_idx) {
		const ir_asm_constraint *constraint = &out_constraints[out_idx];
		unsigned                 pos        = constraint->pos;
		constraint_t             parsed_constraint;
		parse_asm_constraints(&parsed_constraint, constraint->constraint, true);

		assert(parsed_constraint.immediate_type == 0);
		arch_register_req_t const *const req
			= make_register_req(irg, &parsed_constraint, n_out_constraints,
			                    out_reg_reqs, out_idx);
		out_reg_reqs[out_idx] = req;

		/* TODO: adjust register_req for clobbers */

		sparc_asm_operand_t *const operand = &operands[pos];
		operand->kind = ASM_OPERAND_OUTPUT_VALUE;
		operand->pos  = out_idx;
	}

	/* inputs + input constraints */
	int       max_ins      = n_inputs+1;
	ir_node **in = ALLOCANZ(ir_node*, max_ins);
	const arch_register_req_t **in_reg_reqs
		= OALLOCN(obst, const arch_register_req_t*, max_ins);
	int n_ins = 0;
	for (int i = 0; i < n_inputs; ++i) {
		ir_node                 *pred         = get_ASM_input(node, i);
		const ir_asm_constraint *constraint   = &in_constraints[i];
		unsigned                 pos          = constraint->pos;

		constraint_t parsed_constraint;
		parse_asm_constraints(&parsed_constraint, constraint->constraint, false);

		sparc_asm_operand_t *const operand = &operands[pos];

		/* try to use an immediate value */
		char imm_type = parsed_constraint.immediate_type;
		if (imm_type == 'I') {
			if (is_imm_encodeable(pred)) {
				operand->kind = ASM_OPERAND_IMMEDIATE;
				operand->immediate_value = get_tarval_long(get_Const_tarval(pred));
				continue;
			}
		} else if (imm_type == 'A') {
			/* TODO: match Add(SymConst,Const), ... */
			if (is_SymConst(pred)) {
				operand->kind = ASM_OPERAND_IMMEDIATE;
				operand->immediate_value_entity = get_SymConst_entity(pred);
				continue;
			} else if (is_Const(pred)) {
				operand->kind = ASM_OPERAND_IMMEDIATE;
				operand->immediate_value = get_tarval_long(get_Const_tarval(pred));
				continue;
			}
		}

		arch_register_req_t const *const req = make_register_req(irg, &parsed_constraint, n_out_constraints, out_reg_reqs, i);
		in_reg_reqs[i] = req;

		int      op_pos      = n_ins++;
		ir_node *new_pred = be_transform_node(pred);
		in[op_pos]    = new_pred;
		operand->kind = ASM_OPERAND_INPUT_VALUE;
		operand->pos  = op_pos;
	}

	int      mem_pos     = n_ins++;
	ir_node *mem         = get_ASM_mem(node);
	in[mem_pos]          = be_transform_node(mem);
	in_reg_reqs[mem_pos] = arch_no_register_req;

	/* parse clobbers */
	for (size_t c = 0; c < get_ASM_n_clobbers(node); ++c) {
		const char *const clobber = get_id_str(clobbers[c]);
		if (strcmp(clobber, "memory") == 0)
			continue;
		if (strcmp(clobber, "cc") == 0) {
			const arch_register_t *flags_reg
				= &sparc_registers[REG_FLAGS];
			out_reg_reqs[out_idx++] = flags_reg->single_req;
			const arch_register_t *fpflags_reg
				= &sparc_registers[REG_FPFLAGS];
			out_reg_reqs[out_idx++] = fpflags_reg->single_req;
			continue;
		}

		const arch_register_t *reg = find_register(clobber);
		assert(reg != NULL); /* otherwise we had a panic earlier */
		out_reg_reqs[out_idx++] = reg->single_req;
	}

	/* append none register requirement for the memory output */
	if (n_outs+1 >= out_size) {
		out_size = n_outs + 1;
		const arch_register_req_t **new_out_reg_reqs
			= OALLOCN(obst, const arch_register_req_t*, out_size);
		memcpy(new_out_reg_reqs, out_reg_reqs,
			   n_outs * sizeof(new_out_reg_reqs[0]));
		out_reg_reqs = new_out_reg_reqs;
	}

	/* add a new (dummy) output which occupies the register */
	out_reg_reqs[n_outs] = arch_no_register_req;
	++n_outs;

	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const block     = get_nodes_block(node);
	ir_node  *const new_block = be_transform_node(block);
	ident    *const text      = get_ASM_text(node);
	ir_node  *const new_node
		= new_bd_sparc_ASM(dbgi, new_block, n_ins, in, n_outs, text, operands);

	backend_info_t *const info = be_get_info(new_node);
	for (size_t o = 0; o < n_outs; ++o) {
		info->out_infos[o].req = out_reg_reqs[o];
	}
	arch_set_irn_register_reqs_in(new_node, in_reg_reqs);

	return new_node;
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
	ir_node  *block = be_transform_node(get_nodes_block(node));

	if (flags & MATCH_MODE_NEUTRAL) {
		op1 = skip_downconv(op1);
		op2 = skip_downconv(op2);
	}
	ir_mode *mode1 = get_irn_mode(op1);
	ir_mode *mode2 = get_irn_mode(op2);
	/* we shouldn't see 64bit code */
	assert(get_mode_size_bits(mode1) <= 32);
	assert(get_mode_size_bits(mode2) <= 32);

	if (is_imm_encodeable(op2)) {
		int32_t  immediate = get_tarval_long(get_Const_tarval(op2));
		ir_node *new_op1 = be_transform_node(op1);
		if (! (flags & MATCH_MODE_NEUTRAL) && needs_extension(op1)) {
			if (flags & MATCH_SIGN_EXT_LEFT) {
				int bits = get_mode_size_bits(mode1);
				new_op1 = gen_sign_extension(dbgi, block, new_op1, bits);
			} else {
				new_op1 = gen_extension(dbgi, block, new_op1, mode1);
			}
		}
		return new_imm(dbgi, block, new_op1, NULL, immediate);
	}
	ir_node *new_op2 = be_transform_node(op2);
	if (! (flags & MATCH_MODE_NEUTRAL) && needs_extension(op2)) {
		new_op2 = gen_extension(dbgi, block, new_op2, mode2);
	}

	if ((flags & MATCH_COMMUTATIVE) && is_imm_encodeable(op1)) {
		int32_t immediate = get_tarval_long(get_Const_tarval(op1));
		return new_imm(dbgi, block, new_op2, NULL, immediate);
	}

	ir_node *new_op1 = be_transform_node(op1);
	if (! (flags & MATCH_MODE_NEUTRAL) && needs_extension(op1)) {
		if (flags & MATCH_SIGN_EXT_LEFT) {
			int bits = get_mode_size_bits(mode1);
			new_op1 = gen_sign_extension(dbgi, block, new_op1, bits);
		} else {
			new_op1 = gen_extension(dbgi, block, new_op1, mode1);
		}
	}
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
	ir_node  *block   = be_transform_node(get_nodes_block(node));
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
	ir_node  *block     = be_transform_node(get_nodes_block(node));
	ir_node  *op1       = get_irn_n(node, 0);
	ir_node  *op2       = get_irn_n(node, 1);
	ir_node  *flags     = get_irn_n(node, 2);
	ir_node  *new_flags = be_transform_node(flags);

	/* only support for mode-neutral implemented so far */
	assert(match_flags & MATCH_MODE_NEUTRAL);

	if (is_imm_encodeable(op2)) {
		int32_t  immediate = get_tarval_long(get_Const_tarval(op2));
		ir_node *new_op1   = be_transform_node(op1);
		return new_binopx_imm(dbgi, block, new_op1, new_flags, NULL, immediate);
	}
	ir_node *new_op2 = be_transform_node(op2);
	if ((match_flags & MATCH_COMMUTATIVE) && is_imm_encodeable(op1)) {
		int32_t immediate = get_tarval_long(get_Const_tarval(op1));
		return new_binopx_imm(dbgi, block, new_op2, new_flags, NULL, immediate);
	}
	ir_node *new_op1 = be_transform_node(op1);
	return new_binopx_reg(dbgi, block, new_op1, new_op2, new_flags);

}

static ir_node *get_reg(ir_graph *const irg, reg_info_t *const reg)
{
	if (!reg->irn) {
		/* this is already the transformed start node */
		ir_node *const start = get_irg_start(irg);
		assert(is_sparc_Start(start));
		arch_register_class_t const *const cls = arch_get_irn_register_req_out(start, reg->offset)->cls;
		reg->irn = new_r_Proj(start, cls ? cls->mode : mode_M, reg->offset);
	}
	return reg->irn;
}

static ir_node *get_g0(ir_graph *irg)
{
	return get_reg(irg, &start_g0);
}

static ir_node *get_g7(ir_graph *irg)
{
	return get_reg(irg, &start_g7);
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
	if (get_entity_owner(entity) == get_tls_type()) {
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
			offset += get_tarval_long(get_Const_tarval(add_right));
		}
	}
	/* Note that we don't match sub(x, Const) or chains of adds/subs
	 * because this should all be normalized by now */

	/* we only use the symconst if we're the only user otherwise we probably
	 * won't save anything but produce multiple sethi+or combinations with
	 * just different offsets */
	ir_node   *ptr2   = NULL;
	ir_entity *entity = NULL;
	if (is_SymConst(base) && get_irn_n_edges(base) == 1) {
		ir_entity *sc_entity = get_SymConst_entity(base);
		dbg_info  *dbgi      = get_irn_dbg_info(ptr);
		ir_node   *block     = get_nodes_block(ptr);
		ir_node   *new_block = be_transform_node(block);

		if (get_entity_owner(sc_entity) == get_tls_type()) {
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

static ir_node *gen_Proj_ASM(ir_node *node)
{
	ir_mode *mode     = get_irn_mode(node);
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);
	long     pos      = get_Proj_proj(node);

	if (mode == mode_M) {
		pos = arch_get_irn_n_outs(new_pred)-1;
	} else if (mode_needs_gp_reg(mode)) {
		mode = mode_gp;
	} else {
		panic("unexpected proj mode at ASM");
	}

	return new_r_Proj(new_pred, mode, pos);
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
		if (is_SymConst(left) && get_irn_n_edges(left) == 1) {
			dbg_info *dbgi  = get_irn_dbg_info(node);
			ir_node  *block = be_transform_node(get_nodes_block(node));

			/* the value of use_ptr2 shouldn't matter here */
			address_t address;
			match_address(node, &address, false);
			assert(is_sparc_SetHi(address.ptr));
			return new_bd_sparc_Or_imm(dbgi, block, address.ptr,
			                           address.entity, address.offset);
		}

		ir_tarval *tv  = get_Const_tarval(right);
		uint32_t   val = get_tarval_long(tv);
		if (val == 0x1000) {
			dbg_info *dbgi   = get_irn_dbg_info(node);
			ir_node  *block  = be_transform_node(get_nodes_block(node));
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
	arch_set_irn_register_out(new_node, pn_sparc_AddCC_flags, &sparc_registers[REG_FLAGS]);

	return new_node;
}

static ir_node *gen_Proj_AddCC_t(ir_node *node)
{
	long     pn       = get_Proj_proj(node);
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);

	switch (pn) {
	case pn_sparc_AddCC_t_res:
		return new_r_Proj(new_pred, mode_gp, pn_sparc_AddCC_res);
	case pn_sparc_AddCC_t_flags:
		return new_r_Proj(new_pred, mode_flags, pn_sparc_AddCC_flags);
	default:
		panic("Invalid proj found");
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
	arch_set_irn_register_out(new_node, pn_sparc_SubCC_flags, &sparc_registers[REG_FLAGS]);

	return new_node;
}

static ir_node *gen_Proj_SubCC_t(ir_node *node)
{
	long     pn       = get_Proj_proj(node);
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);

	switch (pn) {
	case pn_sparc_SubCC_t_res:
		return new_r_Proj(new_pred, mode_gp, pn_sparc_SubCC_res);
	case pn_sparc_SubCC_t_flags:
		return new_r_Proj(new_pred, mode_flags, pn_sparc_SubCC_flags);
	default:
		panic("Invalid proj found");
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
	ir_node  *block    = be_transform_node(get_nodes_block(node));
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
			new_load = new_bd_sparc_Ld_reg(dbgi, block, address.ptr,
			                               address.ptr2, new_mem, mode);
		} else {
			new_load = new_bd_sparc_Ld_imm(dbgi, block, address.ptr, new_mem,
			                               mode, address.entity, address.offset,
			                               false);
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
	ir_node  *block    = be_transform_node(get_nodes_block(node));
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
		unsigned dest_bits = get_mode_size_bits(mode);
		while (is_downconv(node)
		       && get_mode_size_bits(get_irn_mode(node)) >= dest_bits) {
		    val = get_Conv_op(val);
		}
		ir_node *new_val = be_transform_node(val);

		assert(dest_bits <= 32);
		address_t address;
		match_address(ptr, &address, true);
		if (address.ptr2 != NULL) {
			assert(address.entity == NULL && address.offset == 0);
			new_store = new_bd_sparc_St_reg(dbgi, block, new_val, address.ptr,
			                                address.ptr2, new_mem, mode);
		} else {
			new_store = new_bd_sparc_St_imm(dbgi, block, new_val, address.ptr,
			                                new_mem, mode, address.entity,
			                                address.offset, false);
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
	                        new_bd_sparc_Mul_reg, new_bd_sparc_Mul_imm);
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
		ir_node *mul = gen_helper_binop(node, MATCH_COMMUTATIVE, new_bd_sparc_SMulh_reg, new_bd_sparc_SMulh_imm);
		return new_r_Proj(mul, mode_gp, pn_sparc_SMulh_low);
	} else {
		ir_node *mul = gen_helper_binop(node, MATCH_COMMUTATIVE, new_bd_sparc_UMulh_reg, new_bd_sparc_UMulh_imm);
		return new_r_Proj(mul, mode_gp, pn_sparc_UMulh_low);
	}
}

static ir_node *gen_sign_extension_value(ir_node *node)
{
	ir_node *block     = get_nodes_block(node);
	ir_node *new_block = be_transform_node(block);
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
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_mode  *mode      = get_Div_resmode(node);
	ir_node  *left      = get_Div_left(node);
	ir_node  *left_low  = be_transform_node(left);
	ir_node  *right     = get_Div_right(node);

	if (mode_is_float(mode)) {
		return gen_helper_binfpop(node, mode, new_bd_sparc_fdiv_s,
								  new_bd_sparc_fdiv_d, new_bd_sparc_fdiv_q);
	}

	ir_node *res;
	if (mode_is_signed(mode)) {
		ir_node *left_high = gen_sign_extension_value(left);

		if (is_imm_encodeable(right)) {
			int32_t immediate = get_tarval_long(get_Const_tarval(right));
			res = new_bd_sparc_SDiv_imm(dbgi, new_block, left_high, left_low,
			                            NULL, immediate);
		} else {
			ir_node *new_right = be_transform_node(right);
			res = new_bd_sparc_SDiv_reg(dbgi, new_block, left_high, left_low,
			                            new_right);
		}
	} else {
		ir_graph *irg       = get_irn_irg(node);
		ir_node  *left_high = get_g0(irg);
		if (is_imm_encodeable(right)) {
			int32_t immediate = get_tarval_long(get_Const_tarval(right));
			res = new_bd_sparc_UDiv_imm(dbgi, new_block, left_high, left_low,
			                            NULL, immediate);
		} else {
			ir_node *new_right = be_transform_node(right);
			res = new_bd_sparc_UDiv_reg(dbgi, new_block, left_high, left_low,
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
	ir_node  *block  = be_transform_node(get_nodes_block(node));
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
		ir_tarval *tv    = get_Const_tarval(op2);
		long       value = get_tarval_long(tv);
		if (!sparc_is_value_imm_encodeable(value)) {
			long notvalue = ~value;
			if ((notvalue & 0x3ff) == 0) {
				ir_node  *block     = get_nodes_block(node);
				ir_node  *new_block = be_transform_node(block);
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
	return gen_helper_binop(node, MATCH_NONE, new_bd_sparc_Srl_reg, new_bd_sparc_Srl_imm);
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
	ir_node  *block  = be_transform_node(get_nodes_block(node));
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

	ir_node  *block  = be_transform_node(get_nodes_block(node));
	dbg_info *dbgi   = get_irn_dbg_info(node);
	ir_node  *op     = get_Minus_op(node);
	ir_node  *new_op = be_transform_node(op);
	ir_node  *zero   = get_g0(get_irn_irg(node));
	return new_bd_sparc_Sub_reg(dbgi, block, zero, new_op);
}

/**
 * Create an entity for a given (floating point) tarval
 */
static ir_entity *create_float_const_entity(ir_graph *const irg, ir_tarval *const tv)
{
	const arch_env_t *arch_env = be_get_irg_arch_env(irg);
	sparc_isa_t      *isa      = (sparc_isa_t*) arch_env;
	ir_entity        *entity   = pmap_get(ir_entity, isa->constants, tv);
	if (entity != NULL)
		return entity;

	ir_mode *mode   = get_tarval_mode(tv);
	ir_type *type   = get_type_for_mode(mode);
	ir_type *glob   = get_glob_type();
	entity = new_entity(glob, id_unique("C%u"), type);
	set_entity_visibility(entity, ir_visibility_private);
	add_entity_linkage(entity, IR_LINKAGE_CONSTANT);

	ir_initializer_t *initializer = create_initializer_tarval(tv);
	set_entity_initializer(entity, initializer);

	pmap_insert(isa->constants, tv, entity);
	return entity;
}

static ir_node *gen_float_const(dbg_info *dbgi, ir_node *block, ir_tarval *tv)
{
	ir_graph  *irg    = get_Block_irg(block);
	ir_entity *entity = create_float_const_entity(irg, tv);
	ir_node   *hi     = new_bd_sparc_SetHi(dbgi, block, entity, 0);
	ir_node   *mem    = get_irg_no_mem(irg);
	ir_mode   *mode   = get_tarval_mode(tv);
	ir_node   *new_op
		= create_ldf(dbgi, block, hi, mem, mode, entity, 0, false);
	ir_node   *proj   = new_r_Proj(new_op, mode, pn_sparc_Ldf_res);

	set_irn_pinned(new_op, op_pin_state_floats);
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
	ir_node   *block = be_transform_node(get_nodes_block(node));
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
	ir_node               *block        = get_nodes_block(node);
	ir_node               *new_block    = be_transform_node(block);
	ir_graph              *irg          = get_irn_irg(block);
	ir_node               *selector     = get_Switch_selector(node);
	ir_node               *new_selector = be_transform_node(selector);
	const ir_switch_table *table        = get_Switch_table(node);

	table = ir_switch_table_duplicate(irg, table);

	/* switch with smaller mode not implemented yet */
	assert(get_mode_size_bits(get_irn_mode(selector)) == 32);

	ir_type   *const utype  = get_unknown_type();
	ir_entity *const entity = new_entity(utype, id_unique("TBL%u"), utype);
	set_entity_visibility(entity, ir_visibility_private);
	add_entity_linkage(entity, IR_LINKAGE_CONSTANT);

	/* construct base address */
	ir_node *table_address = make_address(dbgi, new_block, entity, 0);
	/* scale index */
	ir_node *idx = new_bd_sparc_Sll_imm(dbgi, new_block, new_selector, NULL, 2);
	/* load from jumptable */
	ir_node *load = new_bd_sparc_Ld_reg(dbgi, new_block, table_address, idx,
	                                    get_irg_no_mem(irg), mode_gp);
	ir_node *address = new_r_Proj(load, mode_gp, pn_sparc_Ld_res);

	unsigned n_outs = get_Switch_n_outs(node);
	return new_bd_sparc_SwitchJmp(dbgi, new_block, address, n_outs, table, entity);
}

static ir_node *gen_Cond(ir_node *node)
{
	/* note: after lower_mode_b we are guaranteed to have a Cmp input */
	ir_node    *selector  = get_Cond_selector(node);
	ir_node    *block     = be_transform_node(get_nodes_block(node));
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
	ir_node *op1      = get_Cmp_left(node);
	ir_node *op2      = get_Cmp_right(node);
	ir_mode *cmp_mode = get_irn_mode(op1);
	assert(get_irn_mode(op2) == cmp_mode);

	if (mode_is_float(cmp_mode)) {
		ir_node  *block   = be_transform_node(get_nodes_block(node));
		dbg_info *dbgi    = get_irn_dbg_info(node);
		ir_node  *new_op1 = be_transform_node(op1);
		ir_node  *new_op2 = be_transform_node(op2);
		unsigned  bits    = get_mode_size_bits(cmp_mode);
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
	if (is_Const(op2) && is_Const_null(op2) && get_irn_n_edges(op1) == 1) {
		if (is_And(op1)) {
			ir_node *new_node = gen_helper_bitop(op1,
			                        new_bd_sparc_AndCCZero_reg,
			                        new_bd_sparc_AndCCZero_imm,
			                        new_bd_sparc_AndNCCZero_reg,
			                        new_bd_sparc_AndNCCZero_imm,
			                        MATCH_NONE);
			arch_set_irn_register(new_node, &sparc_registers[REG_FLAGS]);
			return new_node;
		} else if (is_Or(op1)) {
			ir_node *new_node = gen_helper_bitop(op1,
			                        new_bd_sparc_OrCCZero_reg,
			                        new_bd_sparc_OrCCZero_imm,
			                        new_bd_sparc_OrNCCZero_reg,
			                        new_bd_sparc_OrNCCZero_imm,
			                        MATCH_NONE);
			arch_set_irn_register(new_node, &sparc_registers[REG_FLAGS]);
			return new_node;
		} else if (is_Eor(op1)) {
			ir_node *new_node = gen_helper_bitop(op1,
			                        new_bd_sparc_XorCCZero_reg,
			                        new_bd_sparc_XorCCZero_imm,
			                        new_bd_sparc_XNorCCZero_reg,
			                        new_bd_sparc_XNorCCZero_imm,
			                        MATCH_NONE);
			arch_set_irn_register(new_node, &sparc_registers[REG_FLAGS]);
			return new_node;
		} else if (is_Add(op1)) {
			ir_node *new_node = gen_helper_binop(op1, MATCH_COMMUTATIVE,
			                        new_bd_sparc_AddCCZero_reg,
			                        new_bd_sparc_AddCCZero_imm);
			arch_set_irn_register(new_node, &sparc_registers[REG_FLAGS]);
			return new_node;
		} else if (is_Sub(op1)) {
			ir_node *new_node = gen_helper_binop(op1, MATCH_NONE,
			                        new_bd_sparc_SubCCZero_reg,
			                        new_bd_sparc_SubCCZero_imm);
			arch_set_irn_register(new_node, &sparc_registers[REG_FLAGS]);
			return new_node;
		} else if (is_Mul(op1)) {
			ir_node *new_node = gen_helper_binop(op1, MATCH_COMMUTATIVE,
			                        new_bd_sparc_MulCCZero_reg,
			                        new_bd_sparc_MulCCZero_imm);
			arch_set_irn_register(new_node, &sparc_registers[REG_FLAGS]);
			return new_node;
		}
	}

	/* integer compare */
	return gen_helper_binop_args(node, op1, op2, MATCH_NONE,
	                             new_bd_sparc_Cmp_reg, new_bd_sparc_Cmp_imm);
}

/**
 * Transforms a SymConst node.
 */
static ir_node *gen_SymConst(ir_node *node)
{
	ir_entity *entity    = get_SymConst_entity(node);
	dbg_info  *dbgi      = get_irn_dbg_info(node);
	ir_node   *block     = get_nodes_block(node);
	ir_node   *new_block = be_transform_node(block);
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
	ir_node  *stf   = create_stf(dbgi, block, ftoi, sp, nomem, mode_fp,
	                             NULL, 0, true);
	arch_add_irn_flags(stf, arch_irn_flag_spill);
	ir_node  *ld    = new_bd_sparc_Ld_imm(dbgi, block, sp, stf, mode_gp,
	                                      NULL, 0, true);
	ir_node  *res   = new_r_Proj(ld, mode_gp, pn_sparc_Ld_res);
	set_irn_pinned(stf, op_pin_state_floats);
	set_irn_pinned(ld, op_pin_state_floats);
	return res;
}

static ir_node *create_itof(dbg_info *dbgi, ir_node *block, ir_node *op,
                            ir_mode *dst_mode)
{
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *sp    = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *st    = new_bd_sparc_St_imm(dbgi, block, op, sp, nomem,
	                                      mode_gp, NULL, 0, true);
	arch_add_irn_flags(st, arch_irn_flag_spill);
	ir_node  *ldf   = new_bd_sparc_Ldf_s(dbgi, block, sp, st, mode_fp,
	                                     NULL, 0, true);
	ir_node  *res   = new_r_Proj(ldf, mode_fp, pn_sparc_Ldf_res);
	unsigned  bits  = get_mode_size_bits(dst_mode);
	set_irn_pinned(st, op_pin_state_floats);
	set_irn_pinned(ldf, op_pin_state_floats);

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

	if (src_mode == mode_b)
		panic("ConvB not lowered %+F", node);

	if (src_mode == dst_mode)
		return be_transform_node(op);

	ir_node  *block    = be_transform_node(get_nodes_block(node));
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
				panic("unsigned to float not lowered!");
			}
			return create_itof(dbgi, block, new_op, dst_mode);
		}
	} else { /* complete in gp registers */
		if (src_bits >= dst_bits || dst_mode == mode_b) {
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
	/* just produce a 0 */
	ir_mode *mode = get_irn_mode(node);
	if (mode_is_float(mode)) {
		ir_node *block = be_transform_node(get_nodes_block(node));
		return gen_float_const(NULL, block, get_mode_null(mode));
	} else if (mode_needs_gp_reg(mode)) {
		ir_graph *irg = get_irn_irg(node);
		return get_g0(irg);
	}

	panic("Unexpected Unknown mode");
}

static void make_start_out(reg_info_t *const info, struct obstack *const obst, ir_node *const start, size_t const offset, arch_register_t const *const reg, arch_register_req_type_t const flags)
{
	info->offset = offset;
	info->irn    = NULL;
	arch_register_req_t const *const req = be_create_reg_req(obst, reg, arch_register_req_type_ignore | flags);
	arch_set_irn_register_req_out(start, offset, req);
	arch_set_irn_register_out(start, offset, reg);
}

/**
 * transform the start node to the prolog code
 */
static ir_node *gen_Start(ir_node *node)
{
	ir_graph  *irg           = get_irn_irg(node);
	ir_entity *entity        = get_irg_entity(irg);
	ir_type   *function_type = get_entity_type(entity);
	ir_node   *block         = get_nodes_block(node);
	ir_node   *new_block     = be_transform_node(block);
	dbg_info  *dbgi          = get_irn_dbg_info(node);
	struct obstack *obst     = be_get_be_obst(irg);

	/* start building list of start constraints */

	/* calculate number of outputs */
	size_t n_outs = 4; /* memory, g0, g7, sp */
	if (!current_cconv->omit_fp)
		++n_outs; /* frame pointer */
	/* function parameters */
	n_outs += current_cconv->n_param_regs;
	/* callee saves */
	if (current_cconv->omit_fp) {
		n_outs += ARRAY_SIZE(omit_fp_callee_saves);
	}

	ir_node *start = new_bd_sparc_Start(dbgi, new_block, n_outs);

	size_t o = 0;

	/* first output is memory */
	start_mem.offset = o;
	start_mem.irn    = NULL;
	arch_set_irn_register_req_out(start, o, arch_no_register_req);
	++o;

	/* the zero register */
	make_start_out(&start_g0, obst, start, o++, &sparc_registers[REG_G0], arch_register_req_type_none);

	/* g7 is used for TLS data */
	make_start_out(&start_g7, obst, start, o++, &sparc_registers[REG_G7], arch_register_req_type_none);

	/* we need an output for the stack pointer */
	make_start_out(&start_sp, obst, start, o++, sp_reg, arch_register_req_type_produces_sp);

	if (!current_cconv->omit_fp) {
		make_start_out(&start_fp, obst, start, o++, fp_reg, arch_register_req_type_none);
	}

	/* function parameters in registers */
	start_params_offset = o;
	for (size_t i = 0; i < get_method_n_params(function_type); ++i) {
		const reg_or_stackslot_t *param = &current_cconv->parameters[i];
		const arch_register_t    *reg0  = param->reg0;
		const arch_register_t    *reg1  = param->reg1;
		if (reg0 != NULL) {
			arch_set_irn_register_req_out(start, o, reg0->single_req);
			arch_set_irn_register_out(start, o, reg0);
			++o;
		}
		if (reg1 != NULL) {
			arch_set_irn_register_req_out(start, o, reg1->single_req);
			arch_set_irn_register_out(start, o, reg1);
			++o;
		}
	}
	/* we need the values of the callee saves (Note: non omit-fp mode has no
	 * callee saves) */
	start_callee_saves_offset = o;
	if (current_cconv->omit_fp) {
		size_t n_callee_saves = ARRAY_SIZE(omit_fp_callee_saves);
		for (size_t c = 0; c < n_callee_saves; ++c) {
			const arch_register_t *reg = omit_fp_callee_saves[c];
			arch_set_irn_register_req_out(start, o, reg->single_req);
			arch_set_irn_register_out(start, o, reg);
			++o;
		}
	}
	assert(n_outs == o);

	return start;
}

static ir_node *get_initial_sp(ir_graph *irg)
{
	return get_reg(irg, &start_sp);
}

static ir_node *get_initial_fp(ir_graph *irg)
{
	return get_reg(irg, &start_fp);
}

static ir_node *get_initial_mem(ir_graph *irg)
{
	return get_reg(irg, &start_mem);
}

static ir_node *get_stack_pointer_for(ir_node *node)
{
	/* get predecessor in stack_order list */
	ir_node *stack_pred = be_get_stack_pred(stackorder, node);
	if (stack_pred == NULL) {
		/* first stack user in the current block. We can simply use the
		 * initial sp_proj for it */
		ir_graph *irg = get_irn_irg(node);
		return get_initial_sp(irg);
	}

	be_transform_node(stack_pred);
	ir_node *stack = pmap_get(ir_node, node_to_stack, stack_pred);
	if (stack == NULL) {
		return get_stack_pointer_for(stack_pred);
	}

	return stack;
}

/**
 * transform a Return node into epilogue code + return statement
 */
static ir_node *gen_Return(ir_node *node)
{
	ir_node  *block     = get_nodes_block(node);
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *new_block = be_transform_node(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *mem       = get_Return_mem(node);
	ir_node  *new_mem   = be_transform_node(mem);
	ir_node  *sp        = get_stack_pointer_for(node);
	size_t    n_res     = get_Return_n_ress(node);
	struct obstack *be_obst = be_get_be_obst(irg);

	/* estimate number of return values */
	size_t n_ins = 2 + n_res; /* memory + stackpointer, return values */
	if (current_cconv->omit_fp)
		n_ins += ARRAY_SIZE(omit_fp_callee_saves);

	const arch_register_req_t **reqs
		= OALLOCN(be_obst, const arch_register_req_t*, n_ins);
	ir_node **in = ALLOCAN(ir_node*, n_ins);
	size_t    p  = 0;

	in[p]   = new_mem;
	reqs[p] = arch_no_register_req;
	++p;

	in[p]   = sp;
	reqs[p] = sp_reg->single_req;
	++p;

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
		ir_node  *start          = get_irg_start(irg);
		size_t    n_callee_saves = ARRAY_SIZE(omit_fp_callee_saves);
		for (size_t i = 0; i < n_callee_saves; ++i) {
			const arch_register_t *reg   = omit_fp_callee_saves[i];
			ir_mode               *mode  = reg->reg_class->mode;
			ir_node               *value
					= new_r_Proj(start, mode, i + start_callee_saves_offset);
			in[p]   = value;
			reqs[p] = reg->single_req;
			++p;
		}
	}
	assert(p == n_ins);

	ir_node *bereturn = new_bd_sparc_Return_reg(dbgi, new_block, n_ins, in);
	arch_set_irn_register_reqs_in(bereturn, reqs);

	return bereturn;
}

static ir_node *bitcast_int_to_float(dbg_info *dbgi, ir_node *block,
                                     ir_node *value0, ir_node *value1)
{
	ir_graph *irg   = get_Block_irg(block);
	ir_node  *sp    = get_irg_frame(irg);
	ir_node  *nomem = get_irg_no_mem(irg);
	ir_node  *st    = new_bd_sparc_St_imm(dbgi, block, value0, sp, nomem,
	                                      mode_gp, NULL, 0, true);
	arch_add_irn_flags(st, arch_irn_flag_spill);
	set_irn_pinned(st, op_pin_state_floats);

	ir_mode *mode;
	ir_node *mem;
	if (value1 != NULL) {
		ir_node *st1 = new_bd_sparc_St_imm(dbgi, block, value1, sp, nomem,
		                                   mode_gp, NULL, 4, true);
		arch_add_irn_flags(st1, arch_irn_flag_spill);
		ir_node *in[2] = { st, st1 };
		ir_node *sync  = new_r_Sync(block, 2, in);
		set_irn_pinned(st1, op_pin_state_floats);
		mem  = sync;
		mode = mode_fp2;
	} else {
		mem  = st;
		mode = mode_fp;
	}

	ir_node *ldf = create_ldf(dbgi, block, sp, mem, mode, NULL, 0, true);
	set_irn_pinned(ldf, op_pin_state_floats);

	return new_r_Proj(ldf, mode, pn_sparc_Ldf_res);
}

static void bitcast_float_to_int(dbg_info *dbgi, ir_node *block,
                                 ir_node *value, ir_mode *float_mode,
                                 ir_node **result)
{
	int bits = get_mode_size_bits(float_mode);
	if (is_Const(value)) {
		ir_tarval *tv = get_Const_tarval(value);
		int32_t val = get_tarval_sub_bits(tv, 0)         |
		              (get_tarval_sub_bits(tv, 1) << 8)  |
		              (get_tarval_sub_bits(tv, 2) << 16) |
		              (get_tarval_sub_bits(tv, 3) << 24);
		ir_node *valc = create_int_const(block, val);
		if (bits == 64) {
			int32_t val2 = get_tarval_sub_bits(tv, 4)         |
						  (get_tarval_sub_bits(tv, 5) << 8)  |
						  (get_tarval_sub_bits(tv, 6) << 16) |
						  (get_tarval_sub_bits(tv, 7) << 24);
			ir_node *valc2 = create_int_const(block, val2);
			result[0] = valc2;
			result[1] = valc;
		} else {
			assert(bits == 32);
			result[0] = valc;
			result[1] = NULL;
		}
	} else {
		ir_graph *irg   = get_Block_irg(block);
		ir_node  *stack = get_irg_frame(irg);
		ir_node  *nomem = get_irg_no_mem(irg);
		ir_node  *new_value = be_transform_node(value);
		ir_node  *stf   = create_stf(dbgi, block, new_value, stack, nomem,
		                             float_mode, NULL, 0, true);
		arch_add_irn_flags(stf, arch_irn_flag_spill);
		set_irn_pinned(stf, op_pin_state_floats);

		ir_node *ld = new_bd_sparc_Ld_imm(dbgi, block, stack, stf, mode_gp, NULL, 0, true);
		set_irn_pinned(ld, op_pin_state_floats);
		result[0] = new_r_Proj(ld, mode_gp, pn_sparc_Ld_res);

		if (bits == 64) {
			ir_node *ld2 = new_bd_sparc_Ld_imm(dbgi, block, stack, stf, mode_gp,
											   NULL, 4, true);
			set_irn_pinned(ld, op_pin_state_floats);
			result[1] = new_r_Proj(ld2, mode_gp, pn_sparc_Ld_res);

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
	ir_node         *block        = get_nodes_block(node);
	ir_node         *new_block    = be_transform_node(block);
	ir_node         *mem          = get_Call_mem(node);
	ir_node         *new_mem      = be_transform_node(mem);
	dbg_info        *dbgi         = get_irn_dbg_info(node);
	ir_type         *type         = get_Call_type(node);
	size_t           n_params     = get_Call_n_params(node);
	size_t           n_ress       = get_method_n_ress(type);
	/* max inputs: memory, callee, register arguments */
	ir_node        **sync_ins     = ALLOCAN(ir_node*, n_params);
	struct obstack  *obst         = be_get_be_obst(irg);
	calling_convention_t *cconv
		= sparc_decide_calling_convention(type, NULL);
	size_t           n_param_regs = cconv->n_param_regs;
	/* param-regs + mem + stackpointer + callee */
	unsigned         max_inputs   = 3 + n_param_regs;
	ir_node        **in           = ALLOCAN(ir_node*, max_inputs);
	const arch_register_req_t **in_req
		= OALLOCNZ(obst, const arch_register_req_t*, max_inputs);
	int              in_arity     = 0;
	int              sync_arity   = 0;
	int              n_caller_saves
		= rbitset_popcount(cconv->caller_saves, N_SPARC_REGISTERS);
	ir_entity       *entity       = NULL;
	ir_node         *new_frame    = get_stack_pointer_for(node);
	bool             aggregate_return
		= get_method_calling_convention(type) & cc_compound_ret;

	assert(n_params == get_method_n_params(type));

	/* construct arguments */

	/* memory input */
	in_req[in_arity] = arch_no_register_req;
	int mem_pos      = in_arity;
	++in_arity;

	/* stack pointer input */
	/* construct an IncSP -> we have to always be sure that the stack is
	 * aligned even if we don't push arguments on it */
	ir_node *incsp = be_new_IncSP(sp_reg, new_block, new_frame,
	                              cconv->param_stack_size, 1);
	in_req[in_arity] = sp_reg->single_req;
	in[in_arity]     = incsp;
	++in_arity;

	/* parameters */
	for (size_t p = 0; p < n_params; ++p) {
		ir_node                  *value      = get_Call_param(node, p);
		const reg_or_stackslot_t *param      = &cconv->parameters[p];
		ir_type                  *param_type = get_method_param_type(type, p);
		ir_mode                  *mode       = get_type_mode(param_type);
		ir_node                  *partial_value;
		ir_node                  *new_values[2];
		ir_node                  *str;
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
		offset = param->offset + SPARC_MIN_STACKSIZE;

		if (mode_is_float(mode)) {
			str = create_stf(dbgi, new_block, partial_value, incsp, new_mem,
			                 mode, NULL, offset, true);
		} else {
			str = new_bd_sparc_St_imm(dbgi, new_block, partial_value, incsp,
			                          new_mem, mode, NULL, offset, true);
		}
		set_irn_pinned(str, op_pin_state_floats);
		sync_ins[sync_arity++] = str;
	}

	/* construct memory input */
	if (sync_arity == 0) {
		in[mem_pos] = new_mem;
	} else if (sync_arity == 1) {
		in[mem_pos] = sync_ins[0];
	} else {
		in[mem_pos] = new_rd_Sync(NULL, new_block, sync_arity, sync_ins);
	}

	if (is_SymConst(callee)) {
		entity = get_SymConst_entity(callee);
	} else {
		in[in_arity]     = be_transform_node(callee);
		in_req[in_arity] = sparc_reg_classes[CLASS_sparc_gp].class_req;
		++in_arity;
	}
	assert(in_arity <= (int)max_inputs);

	/* outputs:
	 *  - memory
	 *  - results
	 *  - caller saves
	 */
	int out_arity = 1 + cconv->n_reg_results + n_caller_saves;

	/* create call node */
	ir_node *res;
	if (entity != NULL) {
		res = new_bd_sparc_Call_imm(dbgi, new_block, in_arity, in, out_arity,
		                            type, entity, 0, aggregate_return);
	} else {
		res = new_bd_sparc_Call_reg(dbgi, new_block, in_arity, in, out_arity,
		                            type, aggregate_return);
	}
	arch_set_irn_register_reqs_in(res, in_req);

	/* create output register reqs */
	int o = 0;
	arch_set_irn_register_req_out(res, o++, arch_no_register_req);
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
	incsp = be_new_IncSP(sp_reg, new_block, incsp, -cconv->param_stack_size, 0);
	/* if we are the last IncSP producer in a block then we have to keep
	 * the stack value.
	 * Note: This here keeps all producers which is more than necessary */
	add_irn_dep(incsp, res);
	keep_alive(incsp);

	pmap_insert(node_to_stack, node, incsp);

	sparc_free_calling_convention(cconv);
	return res;
}

static ir_node *gen_Sel(ir_node *node)
{
	dbg_info  *dbgi      = get_irn_dbg_info(node);
	ir_node   *block     = get_nodes_block(node);
	ir_node   *new_block = be_transform_node(block);
	ir_node   *ptr       = get_Sel_ptr(node);
	ir_node   *new_ptr   = be_transform_node(ptr);
	ir_entity *entity    = get_Sel_entity(node);

	/* must be the frame pointer all other sels must have been lowered
	 * already */
	assert(is_Proj(ptr) && is_Start(get_Proj_pred(ptr)));

	return new_bd_sparc_FrameAddr(dbgi, new_block, new_ptr, entity, 0);
}

static ir_node *gen_Alloc(ir_node *node)
{
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node  *block      = get_nodes_block(node);
	ir_node  *new_block  = be_transform_node(block);
	ir_node  *size       = get_Alloc_size(node);
	ir_node  *stack_pred = get_stack_pointer_for(node);
	ir_node  *mem        = get_Alloc_mem(node);
	ir_node  *new_mem    = be_transform_node(mem);

	ir_node *subsp;
	if (is_Const(size)) {
		ir_tarval *tv    = get_Const_tarval(size);
		long       sizel = get_tarval_long(tv);

		assert((sizel & (SPARC_STACK_ALIGNMENT - 1)) == 0 && "Found Alloc with misaligned constant");
		subsp = new_bd_sparc_SubSP_imm(dbgi, new_block, stack_pred, new_mem, NULL, sizel);
	} else {
		ir_node *new_size = be_transform_node(size);
		subsp = new_bd_sparc_SubSP_reg(dbgi, new_block, stack_pred, new_size, new_mem);
	}

	ir_node *stack_proj = new_r_Proj(subsp, mode_gp, pn_sparc_SubSP_stack);
	arch_set_irn_register(stack_proj, sp_reg);
	/* If we are the last stack producer in a block, we have to keep the
	 * stack value.  This keeps all producers, which is more than necessary. */
	keep_alive(stack_proj);

	pmap_insert(node_to_stack, node, stack_proj);

	return subsp;
}

static ir_node *gen_Proj_Alloc(ir_node *node)
{
	ir_node *alloc     = get_Proj_pred(node);
	ir_node *new_alloc = be_transform_node(alloc);
	long     pn        = get_Proj_proj(node);

	switch ((pn_Alloc)pn) {
	case pn_Alloc_M:   return new_r_Proj(new_alloc, mode_M,  pn_sparc_SubSP_M);
	case pn_Alloc_res: return new_r_Proj(new_alloc, mode_gp, pn_sparc_SubSP_addr);
	}
	panic("invalid Proj->Alloc");
}

static ir_node *gen_Free(ir_node *node)
{
	(void)node;
	panic("Free not supported yet");
}

static const arch_register_req_t float1_req = {
	arch_register_req_type_normal,
	&sparc_reg_classes[CLASS_sparc_fp],
	NULL,
	0,
	0,
	1
};
static const arch_register_req_t float2_req = {
	arch_register_req_type_normal | arch_register_req_type_aligned,
	&sparc_reg_classes[CLASS_sparc_fp],
	NULL,
	0,
	0,
	2
};
static const arch_register_req_t float4_req = {
	arch_register_req_type_normal | arch_register_req_type_aligned,
	&sparc_reg_classes[CLASS_sparc_fp],
	NULL,
	0,
	0,
	4
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
	if (mode_needs_gp_reg(mode)) {
		/* we shouldn't have any 64bit stuff around anymore */
		assert(get_mode_size_bits(mode) <= 32);
		/* all integer operations are on 32bit registers now */
		req  = sparc_reg_classes[CLASS_sparc_gp].class_req;
	} else if (mode_is_float(mode)) {
		req  = get_float_req(mode);
	} else {
		req = arch_no_register_req;
	}

	return be_transform_phi(node, req);
}

/*
 * Transform saturating increment.
 */
static ir_node *gen_saturating_increment(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = be_transform_node(get_nodes_block(node));
	ir_node  *operand   = be_transform_node(get_Builtin_param(node, 0));
	ir_node  *increment = new_bd_sparc_AddCC_imm(dbgi, block, operand, NULL, 1);
	ir_node  *value     = new_rd_Proj(dbgi, increment, mode_Iu, pn_sparc_AddCC_res);
	ir_node  *eflags    = new_rd_Proj(dbgi, increment, mode_Iu, pn_sparc_AddCC_flags);
	ir_graph *irg       = get_Block_irg(block);
	ir_node  *zero      = get_g0(irg);
	ir_node  *sbb       = new_bd_sparc_SubX_reg(dbgi, block, value, zero, eflags);

	return sbb;
}

static ir_node *gen_compare_swap(ir_node *node)
{
	dbg_info *dbgi    = get_irn_dbg_info(node);
	ir_node  *block   = be_transform_node(get_nodes_block(node));
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
	if ((!mode_is_int(mode) && !mode_is_reference(mode))
	    || get_mode_size_bits(mode) != 32) {
	    panic("sparc: compare and swap only allowed for 32bit values");
	}

	return cas;
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
	case ir_bk_inner_trampoline:
		/* not supported */
		break;
	case ir_bk_compare_swap:
		return gen_compare_swap(node);
	case ir_bk_saturating_increment:
		return gen_saturating_increment(node);
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
	long             pn       = get_Proj_proj(proj);

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
	case ir_bk_inner_trampoline:
		/* not supported / should be lowered */
		break;
	case ir_bk_saturating_increment:
		assert(pn == pn_Builtin_max+1);
		return new_pred;
	case ir_bk_compare_swap:
		if (pn == pn_Builtin_M) {
			return new_r_Proj(new_pred, mode_M, pn_sparc_Cas_M);
		} else {
			assert(pn == pn_Builtin_max+1);
			return new_r_Proj(new_pred, mode_gp, pn_sparc_Cas_res);
		}
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

/**
 * Transform a Proj from a Load.
 */
static ir_node *gen_Proj_Load(ir_node *node)
{
	ir_node  *load     = get_Proj_pred(node);
	ir_node  *new_load = be_transform_node(load);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long      pn       = get_Proj_proj(node);

	/* renumber the proj */
	switch (get_sparc_irn_opcode(new_load)) {
	case iro_sparc_Ld:
		/* handle all gp loads equal: they have the same proj numbers. */
		if (pn == pn_Load_res) {
			return new_rd_Proj(dbgi, new_load, mode_gp, pn_sparc_Ld_res);
		} else if (pn == pn_Load_M) {
			return new_rd_Proj(dbgi, new_load, mode_M, pn_sparc_Ld_M);
		}
		break;
	case iro_sparc_Ldf:
		if (pn == pn_Load_res) {
			const sparc_load_store_attr_t *attr
				= get_sparc_load_store_attr_const(new_load);
			ir_mode *mode = attr->load_store_mode;
			return new_rd_Proj(dbgi, new_load, mode, pn_sparc_Ldf_res);
		} else if (pn == pn_Load_M) {
			return new_rd_Proj(dbgi, new_load, mode_M, pn_sparc_Ld_M);
		}
		break;
	default:
		break;
	}
	panic("Unsupported Proj from Load");
}

static ir_node *gen_Proj_Store(ir_node *node)
{
	ir_node  *store     = get_Proj_pred(node);
	ir_node  *new_store = be_transform_node(store);
	long      pn        = get_Proj_proj(node);

	/* renumber the proj */
	switch (get_sparc_irn_opcode(new_store)) {
	case iro_sparc_St:
		if (pn == pn_Store_M) {
			return new_store;
		}
		break;
	case iro_sparc_Stf:
		if (pn == pn_Store_M) {
			return new_store;
		}
		break;
	default:
		break;
	}
	panic("Unsupported Proj from Store");
}

/**
 * transform Projs from a Div
 */
static ir_node *gen_Proj_Div(ir_node *node)
{
	ir_node  *pred     = get_Proj_pred(node);
	ir_node  *new_pred = be_transform_node(pred);

	ir_mode *res_mode;
	if (is_sparc_SDiv(new_pred) || is_sparc_UDiv(new_pred)) {
		res_mode = mode_gp;
	} else if (is_sparc_fdiv(new_pred)) {
		res_mode = get_Div_resmode(pred);
	} else {
		panic("Div transformed to something unexpected: %+F",
		      new_pred);
	}
	assert((int)pn_sparc_SDiv_res == (int)pn_sparc_UDiv_res);
	assert((int)pn_sparc_SDiv_M   == (int)pn_sparc_UDiv_M);
	assert((int)pn_sparc_SDiv_res == (int)pn_sparc_fdiv_res);
	assert((int)pn_sparc_SDiv_M   == (int)pn_sparc_fdiv_M);
	long pn = get_Proj_proj(node);
	switch (pn) {
	case pn_Div_res:
		return new_r_Proj(new_pred, res_mode, pn_sparc_SDiv_res);
	case pn_Div_M:
		return new_r_Proj(new_pred, mode_M, pn_sparc_SDiv_M);
	default:
		break;
	}
	panic("Unsupported Proj from Div");
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

static ir_node *gen_Proj_Start(ir_node *node)
{
	ir_node *block     = get_nodes_block(node);
	ir_node *new_block = be_transform_node(block);
	long     pn        = get_Proj_proj(node);
	/* make sure prolog is constructed */
	be_transform_node(get_Proj_pred(node));

	switch ((pn_Start) pn) {
	case pn_Start_X_initial_exec:
		/* exchange ProjX with a jump */
		return new_bd_sparc_Ba(NULL, new_block);
	case pn_Start_M: {
		ir_graph *irg = get_irn_irg(node);
		ir_node  *mem = get_initial_mem(irg);
		keep_alive(mem);
		return mem;
	}
	case pn_Start_T_args:
		return new_r_Bad(get_irn_irg(block), mode_T);
	case pn_Start_P_frame_base:
		return get_frame_base(get_irn_irg(block));
	}
	panic("Unexpected start proj: %ld\n", pn);
}

static ir_node *gen_Proj_Proj_Start(ir_node *node)
{
	long      pn        = get_Proj_proj(node);
	ir_node  *block     = get_nodes_block(node);
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *args      = get_Proj_pred(node);
	ir_node  *start     = get_Proj_pred(args);
	ir_node  *new_start = be_transform_node(start);

	/* Proj->Proj->Start must be a method argument */
	assert(get_Proj_proj(get_Proj_pred(node)) == pn_Start_T_args);

	const reg_or_stackslot_t *param = &current_cconv->parameters[pn];

	if (param->reg0 != NULL) {
		/* argument transmitted in register */
		const arch_register_t *reg      = param->reg0;
		ir_mode               *reg_mode = reg->reg_class->mode;
		long                   new_pn   = param->reg_offset + start_params_offset;
		ir_node               *value    = new_r_Proj(new_start, reg_mode, new_pn);
		bool                   is_float = false;

		ir_entity *entity      = get_irg_entity(irg);
		ir_type   *method_type = get_entity_type(entity);
		if (pn < (long)get_method_n_params(method_type)) {
			ir_type *param_type = get_method_param_type(method_type, pn);
			ir_mode *mode       = get_type_mode(param_type);
			is_float = mode_is_float(mode);
		}

		if (is_float) {
			const arch_register_t *reg1 = param->reg1;
			ir_node *value1 = NULL;

			if (reg1 != NULL) {
				ir_mode *reg1_mode = reg1->reg_class->mode;
				value1 = new_r_Proj(new_start, reg1_mode, new_pn+1);
			} else if (param->entity != NULL) {
				ir_node *fp  = get_initial_fp(irg);
				ir_node *mem = get_initial_mem(irg);
				ir_node *ld  = new_bd_sparc_Ld_imm(NULL, new_block, fp, mem,
				                                   mode_gp, param->entity,
				                                   0, true);
				value1 = new_r_Proj(ld, mode_gp, pn_sparc_Ld_res);
			}

			/* convert integer value to float */
			value = bitcast_int_to_float(NULL, new_block, value, value1);
		}
		return value;
	} else {
		/* argument transmitted on stack */
		ir_node *mem  = get_initial_mem(irg);
		ir_mode *mode = get_type_mode(param->type);
		ir_node *base = get_frame_base(irg);

		ir_node *load;
		ir_node *value;
		if (mode_is_float(mode)) {
			load  = create_ldf(NULL, new_block, base, mem, mode,
			                   param->entity, 0, true);
			value = new_r_Proj(load, mode_fp, pn_sparc_Ldf_res);
		} else {
			load  = new_bd_sparc_Ld_imm(NULL, new_block, base, mem, mode,
			                            param->entity, 0, true);
			value = new_r_Proj(load, mode_gp, pn_sparc_Ld_res);
		}
		set_irn_pinned(load, op_pin_state_floats);

		return value;
	}
}

static ir_node *gen_Proj_Call(ir_node *node)
{
	long     pn        = get_Proj_proj(node);
	ir_node *call      = get_Proj_pred(node);
	ir_node *new_call  = be_transform_node(call);

	switch ((pn_Call) pn) {
	case pn_Call_M:
		return new_r_Proj(new_call, mode_M, 0);
	case pn_Call_X_regular:
	case pn_Call_X_except:
	case pn_Call_T_result:
		break;
	}
	panic("Unexpected Call proj %ld\n", pn);
}

static ir_node *gen_Proj_Proj_Call(ir_node *node)
{
	long                  pn            = get_Proj_proj(node);
	ir_node              *call          = get_Proj_pred(get_Proj_pred(node));
	ir_node              *new_call      = be_transform_node(call);
	ir_type              *function_type = get_Call_type(call);
	calling_convention_t *cconv
		= sparc_decide_calling_convention(function_type, NULL);
	const reg_or_stackslot_t  *res  = &cconv->results[pn];
	ir_mode                   *mode = get_irn_mode(node);
	long                       new_pn = 1 + res->reg_offset;

	assert(res->req0 != NULL && res->req1 == NULL);
	if (mode_needs_gp_reg(mode)) {
		mode = mode_gp;
	}
	sparc_free_calling_convention(cconv);

	return new_r_Proj(new_call, mode, new_pn);
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
	panic("code selection didn't expect Proj(Proj) after %+F\n", pred_pred);
}

/**
 * transform a Jmp
 */
static ir_node *gen_Jmp(ir_node *node)
{
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
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
	be_set_transform_function(op_Alloc,        gen_Alloc);
	be_set_transform_function(op_And,          gen_And);
	be_set_transform_function(op_Builtin,      gen_Builtin);
	be_set_transform_function(op_Call,         gen_Call);
	be_set_transform_function(op_Cmp,          gen_Cmp);
	be_set_transform_function(op_Cond,         gen_Cond);
	be_set_transform_function(op_Const,        gen_Const);
	be_set_transform_function(op_Conv,         gen_Conv);
	be_set_transform_function(op_Div,          gen_Div);
	be_set_transform_function(op_Eor,          gen_Eor);
	be_set_transform_function(op_Free,         gen_Free);
	be_set_transform_function(op_Jmp,          gen_Jmp);
	be_set_transform_function(op_Load,         gen_Load);
	be_set_transform_function(op_Minus,        gen_Minus);
	be_set_transform_function(op_Mul,          gen_Mul);
	be_set_transform_function(op_Mulh,         gen_Mulh);
	be_set_transform_function(op_Not,          gen_Not);
	be_set_transform_function(op_Or,           gen_Or);
	be_set_transform_function(op_Phi,          gen_Phi);
	be_set_transform_function(op_Return,       gen_Return);
	be_set_transform_function(op_Sel,          gen_Sel);
	be_set_transform_function(op_Shl,          gen_Shl);
	be_set_transform_function(op_Shr,          gen_Shr);
	be_set_transform_function(op_Shrs,         gen_Shrs);
	be_set_transform_function(op_Start,        gen_Start);
	be_set_transform_function(op_Store,        gen_Store);
	be_set_transform_function(op_Sub,          gen_Sub);
	be_set_transform_function(op_Switch,       gen_Switch);
	be_set_transform_function(op_SymConst,     gen_SymConst);
	be_set_transform_function(op_Unknown,      gen_Unknown);

	be_set_transform_function(op_sparc_AddX_t, gen_AddX_t);
	be_set_transform_function(op_sparc_AddCC_t,gen_AddCC_t);
	be_set_transform_function(op_sparc_Save,   be_duplicate_node);
	be_set_transform_function(op_sparc_SubX_t, gen_SubX_t);
	be_set_transform_function(op_sparc_SubCC_t,gen_SubCC_t);

	be_set_transform_proj_function(op_Alloc,         gen_Proj_Alloc);
	be_set_transform_proj_function(op_ASM,           gen_Proj_ASM);
	be_set_transform_proj_function(op_Builtin,       gen_Proj_Builtin);
	be_set_transform_proj_function(op_Call,          gen_Proj_Call);
	be_set_transform_proj_function(op_Cond,          be_duplicate_node);
	be_set_transform_proj_function(op_Div,           gen_Proj_Div);
	be_set_transform_proj_function(op_Load,          gen_Proj_Load);
	be_set_transform_proj_function(op_Proj,          gen_Proj_Proj);
	be_set_transform_proj_function(op_sparc_AddCC_t, gen_Proj_AddCC_t);
	be_set_transform_proj_function(op_sparc_SubCC_t, gen_Proj_SubCC_t);
	be_set_transform_proj_function(op_Start,         gen_Proj_Start);
	be_set_transform_proj_function(op_Store,         gen_Proj_Store);
	be_set_transform_proj_function(op_Switch,        be_duplicate_node);
}

/**
 * Transform a Firm graph into a SPARC graph.
 */
void sparc_transform_graph(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_TUPLES
	                         | IR_GRAPH_PROPERTY_NO_BADS);

	ir_entity *entity = get_irg_entity(irg);

	sparc_register_transformers();

	node_to_stack = pmap_create();

	mode_gp    = sparc_reg_classes[CLASS_sparc_gp].mode;
	mode_fp    = sparc_reg_classes[CLASS_sparc_fp].mode;
	mode_fp2   = mode_D;
	//mode_fp4 = ?
	mode_flags = sparc_reg_classes[CLASS_sparc_flags_class].mode;
	assert(sparc_reg_classes[CLASS_sparc_fpflags_class].mode == mode_flags);

	frame_base = NULL;

	stackorder = be_collect_stacknodes(irg);
	current_cconv
		= sparc_decide_calling_convention(get_entity_type(entity), irg);
	if (sparc_variadic_fixups(irg, current_cconv)) {
		sparc_free_calling_convention(current_cconv);
		current_cconv
			= sparc_decide_calling_convention(get_entity_type(entity), irg);
	}
	sparc_create_stacklayout(irg, current_cconv);
	be_add_parameter_entity_stores(irg);

	be_transform_graph(irg, NULL);

	be_free_stackorder(stackorder);
	sparc_free_calling_convention(current_cconv);

	ir_type *frame_type = get_irg_frame_type(irg);
	if (get_type_state(frame_type) == layout_undefined)
		default_layout_compound_type(frame_type);

	pmap_destroy(node_to_stack);
	node_to_stack = NULL;

	/* do code placement, to optimize the position of constants */
	place_code(irg);
	/* backend expects outedges to be always on */
	assure_edges(irg);
}

void sparc_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.sparc.transform");
}
