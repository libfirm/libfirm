/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   code selection (transform FIRM into amd64 FIRM)
 */
#include "debug.h"
#include "panic.h"
#include "heights.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "tv_t.h"
#include "util.h"

#include "benode.h"
#include "betranshlp.h"
#include "bearch_amd64_t.h"
#include "beirg.h"
#include "besched.h"

#include "amd64_new_nodes.h"
#include "amd64_nodes_attr.h"
#include "amd64_transform.h"
#include "../ia32/x86_address_mode.h"
#include "../ia32/x86_cconv.h"

#include "gen_amd64_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static ir_mode         *mode_gp;
static ir_mode         *mode_flags;
static x86_cconv_t     *current_cconv = NULL;
static be_start_info_t  start_mem;
static be_start_info_t  start_val[N_AMD64_REGISTERS];
static pmap            *node_to_stack;
static be_stackorder_t *stackorder;

/** we don't have a concept of aliasing registers, so enumerate them
 * manually for the asm nodes. */
const x86_clobber_name_t amd64_additional_clobber_names[] = {
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

#define GP &amd64_reg_classes[CLASS_amd64_gp]
const x86_asm_constraint_list_t amd64_asm_constraints = {
	['A'] = { MATCH_REG, GP, 1 << REG_GP_RAX | 1 << REG_GP_RDX },
	['D'] = { MATCH_REG, GP, 1 << REG_GP_RDI },
	['I'] = { MATCH_IMM, GP, 0 },
	['J'] = { MATCH_IMM, GP, 0 },
	['K'] = { MATCH_IMM, GP, 0 },
	['L'] = { MATCH_IMM, GP, 0 },
	['M'] = { MATCH_IMM, GP, 0 },
	['N'] = { MATCH_IMM, GP, 0 },
	['O'] = { MATCH_IMM, GP, 0 },
	['R'] = { MATCH_REG, GP, 1 << REG_GP_RAX | 1 << REG_GP_RBX
		| 1 << REG_GP_RCX | 1 << REG_GP_RDX | 1 << REG_GP_RSI
		| 1 << REG_GP_RDI | 1 << REG_GP_RBP | 1 << REG_GP_RSP },
	['S'] = { MATCH_REG, GP, 1 << REG_GP_RSI },
	['Q'] = { MATCH_REG, GP, 1 << REG_GP_RAX | 1 << REG_GP_RBX
		| 1 << REG_GP_RCX | 1 << REG_GP_RDX },
	['V'] = { MATCH_MEM, GP, 0 },
	['X'] = { MATCH_ANY, GP, 0 },
	['a'] = { MATCH_REG, GP, 1 << REG_GP_RAX },
	['b'] = { MATCH_REG, GP, 1 << REG_GP_RBX },
	['c'] = { MATCH_REG, GP, 1 << REG_GP_RCX },
	['d'] = { MATCH_REG, GP, 1 << REG_GP_RDX },
	['g'] = { MATCH_ANY, GP, 0 },
	['i'] = { MATCH_IMM, GP, 0 },
	['l'] = { MATCH_REG, GP, 1 << REG_GP_RAX | 1 << REG_GP_RBX
		| 1 << REG_GP_RCX | 1 << REG_GP_RDX | 1 << REG_GP_RSI
		| 1 << REG_GP_RDI | 1 << REG_GP_RBP },
	['m'] = { MATCH_MEM, GP, 0 },
	['n'] = { MATCH_IMM, GP, 0 },
	['o'] = { MATCH_MEM, GP, 0 },
	['p'] = { MATCH_REG, GP, 0 },
	['q'] = { MATCH_REG, GP, 0 },
	['r'] = { MATCH_REG, GP, 0 },
	['x'] = { MATCH_REG, &amd64_reg_classes[CLASS_amd64_xmm], 0 },

	// see comments in ia32_transform.c about unimplemented stuff.
};
#undef GP

#define BIT(x)    (1u << x)

static const arch_register_req_t amd64_requirement_gp_same_0 = {
	.cls               = &amd64_reg_classes[CLASS_amd64_gp],
	.should_be_same    = BIT(0),
	.width             = 1,
};

static const arch_register_req_t amd64_requirement_xmm_same_0 = {
	.cls               = &amd64_reg_classes[CLASS_amd64_xmm],
	.should_be_same    = BIT(0),
	.width             = 1,
};

static const arch_register_req_t amd64_requirement_gp_same_0_not_1 = {
	.cls               = &amd64_reg_classes[CLASS_amd64_gp],
	.should_be_same    = BIT(0),
	.must_be_different = BIT(1),
	.width             = 1,
};

static const arch_register_req_t amd64_requirement_xmm_same_0_not_1 = {
	.cls               = &amd64_reg_classes[CLASS_amd64_xmm],
	.should_be_same    = BIT(0),
	.must_be_different = BIT(1),
	.width             = 1,
};

static const arch_register_req_t *mem_reqs[] = {
	&arch_no_requirement,
};

static const arch_register_req_t *reg_mem_reqs[] = {
	&amd64_class_reg_req_gp,
	&arch_no_requirement,
};

static const arch_register_req_t *rsp_mem_reqs[] = {
	&amd64_single_reg_req_gp_rsp,
	&arch_no_requirement,
};

static const arch_register_req_t *rsp_reg_mem_reqs[] = {
	&amd64_single_reg_req_gp_rsp,
	&amd64_class_reg_req_gp,
	&arch_no_requirement,
};

static const arch_register_req_t *xmm_mem_reqs[] = {
	&amd64_class_reg_req_xmm,
	&arch_no_requirement,
};

static const arch_register_req_t *reg_reg_mem_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
	&arch_no_requirement,
};

static const arch_register_req_t *xmm_reg_mem_reqs[] = {
	&amd64_class_reg_req_xmm,
	&amd64_class_reg_req_gp,
	&arch_no_requirement,
};

static const arch_register_req_t *reg_reg_reg_mem_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
	&arch_no_requirement,
};

static const arch_register_req_t *xmm_reg_reg_mem_reqs[] = {
	&amd64_class_reg_req_xmm,
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
	&arch_no_requirement,
};

static const arch_register_req_t *reg_flags_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_flags,
};

static const arch_register_req_t *reg_reg_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
};

static const arch_register_req_t *rax_reg_reqs[] = {
	&amd64_single_reg_req_gp_rax,
	&amd64_class_reg_req_gp,
};

static const arch_register_req_t *rax_reg_rdx_mem_reqs[] = {
	&amd64_single_reg_req_gp_rax,
	&amd64_class_reg_req_gp,
	&amd64_single_reg_req_gp_rdx,
	&arch_no_requirement,
};

static const arch_register_req_t *reg_reqs[] = {
	&amd64_class_reg_req_gp,
};

arch_register_req_t const *amd64_xmm_reqs[] = {
	&amd64_class_reg_req_xmm,
};

static const arch_register_req_t *reg_rcx_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_single_reg_req_gp_rcx,
};

static const arch_register_req_t *no_reqs[] = {
};

static const arch_register_req_t *xmm_xmm_reqs[] = {
	&amd64_class_reg_req_xmm,
	&amd64_class_reg_req_xmm,
};

arch_register_req_t const **const gp_am_reqs[] = {
	mem_reqs,
	reg_mem_reqs,
	reg_reg_mem_reqs,
	reg_reg_reg_mem_reqs,
};

static arch_register_req_t const **const xmm_am_reqs[] = {
	mem_reqs,
	xmm_mem_reqs,
	xmm_reg_mem_reqs,
	xmm_reg_reg_mem_reqs,
};

static inline bool mode_needs_gp_reg(ir_mode *mode)
{
	return get_mode_arithmetic(mode) == irma_twos_complement
	    && mode != amd64_mode_xmm; /* mode_xmm is 128bit int at the moment */
}

static ir_node *get_initial_sp(ir_graph *irg)
{
	return be_get_start_proj(irg, &start_val[REG_RSP]);
}

static ir_node *get_initial_fp(ir_graph *irg)
{
	return be_get_start_proj(irg, &start_val[REG_RBP]);
}

static ir_node *get_initial_mem(ir_graph *irg)
{
	return be_get_start_proj(irg, &start_mem);
}

static ir_node *get_frame_base(ir_graph *irg)
{
	if (current_cconv->omit_fp) {
		return get_initial_sp(irg);
	} else {
		return get_initial_fp(irg);
	}
}

static amd64_insn_mode_t get_insn_mode_from_mode(const ir_mode *mode)
{
	switch (get_mode_size_bits(mode)) {
	case   8: return INSN_MODE_8;
	case  16: return INSN_MODE_16;
	case  32: return INSN_MODE_32;
	case  64: return INSN_MODE_64;
	case 128: return INSN_MODE_128;
	}
	panic("unexpected mode");
}

ir_entity *create_float_const_entity(ir_tarval *const tv)
{
	ir_entity *entity = pmap_get(ir_entity, amd64_constants, tv);
	if (entity != NULL)
		return entity;

	ir_mode *mode = get_tarval_mode(tv);
	ir_type *type = get_type_for_mode(mode);
	ir_type *glob = get_glob_type();

	entity = new_entity(glob, id_unique("C%u"), type);
	set_entity_visibility(entity, ir_visibility_private);
	add_entity_linkage(entity, IR_LINKAGE_CONSTANT);

	ir_initializer_t *initializer = create_initializer_tarval(tv);
	set_entity_initializer(entity, initializer);

	pmap_insert(amd64_constants, tv, entity);
	return entity;
}

typedef enum reference_mode_t {
	REFERENCE_DIRECT,
	REFERENCE_IP_RELATIVE,
	REFERENCE_GOT,
} reference_mode_t;

static reference_mode_t need_relative_addressing(const ir_entity *entity)
{
	if (!be_options.pic)
		return REFERENCE_DIRECT;

	/* simply everything is instruction pointer relative, external functions
	 * use a global offset table */
	return entity_has_definition(entity)
	   && (get_entity_linkage(entity) & IR_LINKAGE_MERGE) == 0
	    ? REFERENCE_IP_RELATIVE : REFERENCE_GOT;
}

static ir_node *create_float_const(dbg_info *dbgi, ir_node *block,
                                   ir_tarval *tv)
{
	ir_graph  *irg     = get_irn_irg(block);
	ir_mode   *tv_mode = get_tarval_mode(tv);
	ir_entity *entity  = create_float_const_entity(tv);
	ir_node   *nomem   = get_irg_no_mem(irg);

	ir_node *in[] = { nomem };
	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));

	addr.immediate.entity       = entity;
	amd64_insn_mode_t insn_mode = get_insn_mode_from_mode(tv_mode);

	addr.index_input = NO_INPUT;
	if (need_relative_addressing(entity) == REFERENCE_DIRECT) {
		addr.base_input = NO_INPUT;
	} else {
		assert(need_relative_addressing(entity) == REFERENCE_IP_RELATIVE);
		addr.base_input = RIP_INPUT;
	}

	ir_node *load;
	unsigned pn_res;
	if (insn_mode == INSN_MODE_128) {
		load = new_bd_amd64_movdqa(dbgi, block, ARRAY_SIZE(in), in,
		                           AMD64_OP_ADDR, addr);
		pn_res = pn_amd64_movdqa_res;
	} else {
		load = new_bd_amd64_movs_xmm(dbgi, block, ARRAY_SIZE(in), in,
		                             insn_mode, AMD64_OP_ADDR, addr);
		pn_res = pn_amd64_movs_xmm_res;
	}
	arch_set_irn_register_reqs_in(load, mem_reqs);
	set_irn_pinned(load, op_pin_state_floats);

	return new_r_Proj(load, amd64_mode_xmm, pn_res);
}

ir_tarval *create_sign_tv(ir_mode *mode)
{
	unsigned size = get_mode_size_bits(mode);
	assert(size == 32 || size == 64 || size == 128);
	ir_mode *intmode = size == 128 ? amd64_mode_xmm
	                 : size == 64  ? mode_Lu
	                               : mode_Iu;
	ir_tarval *one  = get_mode_one(intmode);
	ir_tarval *sign = tarval_shl_unsigned(one, size-1);
	return tarval_bitcast(sign, mode);
}

static ir_node *gen_Const(ir_node *node)
{
	ir_node  *block = be_transform_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);
	ir_tarval *tv = get_Const_tarval(node);

	if (!mode_needs_gp_reg(mode)) {
		if (tarval_is_null(tv)) {
			return new_bd_amd64_xorpd_0(dbgi, block);
		}

		return create_float_const(dbgi, block, tv);
	}

	uint64_t val = get_tarval_uint64(tv);
	amd64_insn_mode_t imode = val > UINT32_MAX ? INSN_MODE_64 : INSN_MODE_32;
	return new_bd_amd64_mov_imm(dbgi, block, imode, val, NULL);
}

static ir_node *gen_Address(ir_node *node)
{
	ir_node   *block  = be_transform_nodes_block(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_entity *entity = get_Address_entity(node);

	/* do we need RIP-relative addressing because of PIC? */
	reference_mode_t mode = need_relative_addressing(entity);
	if (mode == REFERENCE_DIRECT)
		return new_bd_amd64_mov_imm(dbgi, block, INSN_MODE_64, 0, entity);

	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.base_input  = RIP_INPUT;
	addr.index_input = NO_INPUT;
	addr.mem_input   = NO_INPUT;

	if (mode == REFERENCE_IP_RELATIVE) {
		addr.immediate.entity = entity;
		return new_bd_amd64_lea(dbgi, block, 0, NULL, INSN_MODE_64, addr);
	} else {
		assert(mode == REFERENCE_GOT);
		addr.immediate.entity = new_got_entry_entity(entity);
		ir_node *load = new_bd_amd64_mov_gp(dbgi, block, 0, NULL, INSN_MODE_64,
		                                    AMD64_OP_ADDR, addr);
		return new_r_Proj(load, mode_gp, pn_amd64_mov_gp_res);
	}
}

ir_node *amd64_new_IncSP(ir_node *block, ir_node *old_sp, int offset,
                         unsigned align)
{
	ir_node *incsp = be_new_IncSP(&amd64_registers[REG_RSP], block, old_sp,
	                              offset, align);
	arch_add_irn_flags(incsp, arch_irn_flag_modify_flags);
	return incsp;
}

typedef ir_node *(*construct_binop_func)(dbg_info *dbgi, ir_node *block,
	int arity, ir_node *in[], const amd64_binop_addr_attr_t *attr_init);

typedef ir_node *(*construct_rax_binop_func)(dbg_info *dbgi, ir_node *block,
	int arity, ir_node *in[], amd64_insn_mode_t insn_mode,
	amd64_op_mode_t op_mode, amd64_addr_t addr);

typedef enum match_flags_t {
	match_am           = 1 << 0,
	match_mode_neutral = 1 << 1,
	match_immediate    = 1 << 2,
	match_commutative  = 1 << 3,
} match_flags_t;

typedef struct amd64_args_t {
	amd64_binop_addr_attr_t     attr;
	ir_node                    *mem_proj;
	ir_node                    *in[4];
	int                         arity;
	const arch_register_req_t **reqs;
} amd64_args_t;

static bool match_immediate_32(x86_imm32_t *imm, const ir_node *op,
                               bool can_match_ip_relative,
                               bool upper32_dont_care)
{
	assert(mode_needs_gp_reg(get_irn_mode(op)));
	assert(imm->offset == 0 && imm->entity == NULL);

	ir_tarval *tv;
	ir_entity *entity;
	if (!be_match_immediate(op, &tv, &entity))
		return false;

	int32_t val;
	if (tv) {
		if (!tarval_is_long(tv))
			return false;
		long lval = get_tarval_long(tv);
		val = (int32_t)lval;
		if ((long)val != lval)
			return false;
		/** the immediate value is signed extended to 64bit, sometimes
		 * this is not what we want. */
		if (!upper32_dont_care && val < 0
		    && !mode_is_signed(get_tarval_mode(tv)))
		    return false;
	} else {
		val = 0;
	}

	if (entity && !can_match_ip_relative) {
		/* TODO: check if entity is in lower 4GB address space/relative */
		return false;
	}

	imm->offset = val;
	imm->entity = entity;
	return true;
}

static ir_heights_t *heights;

static bool input_depends_on_load(ir_node *load, ir_node *input)
{
	ir_node *block = get_nodes_block(load);
	/* if the dependency is in another block, then we ignore it for now
	   as we only match address mode loads in the same block. */
	return get_nodes_block(input) == block
	    && heights_reachable_in_block(heights, input, load);
}

static void fix_node_mem_proj(ir_node *node, ir_node *mem_proj)
{
	if (mem_proj == NULL)
		return;

	ir_node *load = get_Proj_pred(mem_proj);
	be_set_transformed_node(load, node);
}

static ir_node *source_am_possible(ir_node *block, ir_node *node)
{
	if (!is_Proj(node))
		return NULL;
	ir_node *load = get_Proj_pred(node);
	if (!is_Load(load))
		return NULL;
	assert(get_Proj_num(node) == pn_Load_res);
	if (get_nodes_block(load) != block)
		return NULL;
	/* make sure we are the only user */
	if (get_irn_n_edges(node) != 1)
		return NULL;
	/* From ia32_transform.c:751:
	 * in some edge cases with address mode we might reach the load normally
	 * and through some AM sequence, if it is already materialized then we
	 * can't create an AM node from it */
	if (be_is_transformed(node))
		return NULL;
	return load;
}

static bool needs_extension(ir_node *op)
{
	ir_mode *mode = get_irn_mode(op);
	if (get_mode_size_bits(mode) >= 32)
		return false;
	return !be_upper_bits_clean(op, mode);
}

static ir_node *create_sext(ir_node *new_block, ir_node *const node, ir_mode *mode)
{
	amd64_insn_mode_t insn_mode = get_insn_mode_from_mode(mode);
	dbg_info *const   dbgi      = get_irn_dbg_info(node);
	ir_node  *const   new_node  = be_transform_node(node);

	amd64_shift_attr_t attr;
	memset(&attr, 0, sizeof(attr));
	attr.base.op_mode = AMD64_OP_SHIFT_IMM;
	attr.insn_mode    = insn_mode;
	attr.immediate    = get_mode_size_bits(mode) - 1;
	ir_node *in[1]    = { new_node };
	ir_node *sar      = new_bd_amd64_sar(dbgi, new_block, ARRAY_SIZE(in),
	                                     in, &attr);

	arch_set_irn_register_reqs_in(sar, reg_reqs);
	arch_set_irn_register_req_out(sar, 0, &amd64_requirement_gp_same_0);
	return new_r_Proj(sar, mode_gp, pn_amd64_sar_res);
}

static ir_node *create_zext(ir_node *new_block, ir_node *const node)
{
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const xor0      = new_bd_amd64_xor_0(dbgi, new_block);
	arch_set_irn_register_reqs_in(xor0, reg_reqs);
	return new_r_Proj(xor0, mode_gp, pn_amd64_xor_0_res);
}

static bool use_address_matching(ir_mode *mode, match_flags_t flags,
                                 ir_node *block,
                                 ir_node *op1, ir_node *op2,
                                 ir_node **out_load, ir_node **out_op)
{
	if (! (flags & match_am))
		return false;

	unsigned mode_bits = get_mode_size_bits(mode);
	if (mode_bits == 8 || mode_bits == 16)
		return false;

	ir_node *load2 = source_am_possible(block, op2);
	if (load2 != NULL && !input_depends_on_load(load2, op1)) {
		(*out_load) = load2;
		(*out_op)   = op1;
		return true;
	}

	if (flags & match_commutative) {
		ir_node *load1 = source_am_possible(block, op1);
		if (load1 != NULL && !input_depends_on_load(load1, op2)) {
			(*out_load) = load1;
			(*out_op)   = op2;
			return true;
		}
	}
	return false;
}

static void perform_address_matching(ir_node *ptr, int *arity,
                                     ir_node **in, amd64_addr_t *addr)
{
	x86_address_t maddr;
	memset(&maddr, 0, sizeof(maddr));
	x86_create_address_mode(&maddr, ptr, x86_create_am_normal);

	if (maddr.base != NULL) {
		int base_input   = (*arity)++;
		addr->base_input = base_input;
		in[base_input]   = be_transform_node(maddr.base);
	} else {
		ir_entity *entity = maddr.imm.entity;
		if (entity != NULL
		    && need_relative_addressing(entity) != REFERENCE_DIRECT) {
		    addr->base_input = RIP_INPUT;
		} else {
			addr->base_input = NO_INPUT;
		}
	}
	if (maddr.index != NULL) {
		int index_input = (*arity)++;
		addr->index_input = index_input;
		in[index_input]  = be_transform_node(maddr.index);
	} else {
		addr->index_input = NO_INPUT;
	}
	ir_entity *entity = maddr.imm.entity;
	if (entity != NULL && is_parameter_entity(entity) &&
		get_entity_parameter_number(entity) == IR_VA_START_PARAMETER_NUMBER)
		panic("VA_START not supported yet");

	addr->immediate = maddr.imm;
	addr->log_scale = maddr.scale;
}

static void match_binop(amd64_args_t *args, ir_node *block,
                        ir_mode *mode, ir_node *op1, ir_node *op2,
                        match_flags_t flags)
{
	memset(args, 0, sizeof(*args));

	bool use_am;
	bool use_xmm       = mode_is_float(mode);
	bool use_immediate = flags & match_immediate;
	bool mode_neutral  = flags & match_mode_neutral;

	args->attr.base.insn_mode = get_insn_mode_from_mode(mode);

	/* TODO: legalize phase */
	if (mode_neutral) {
		op1 = be_skip_downconv(op1, true);
		op2 = be_skip_downconv(op2, true);
	} else {
		/* TODO: extend inputs? */
		(void)needs_extension;
	}

	ir_node *load;
	ir_node *op;

	use_am = use_address_matching(mode, flags, block, op1, op2, &load, &op);

	if (use_immediate
	    && match_immediate_32(&args->attr.u.immediate, op2, false, mode_neutral)) {
		assert(!use_xmm && "Can't (yet) match binop with xmm immediate");
		/* fine, we found an immediate */
		args->attr.base.base.op_mode = AMD64_OP_REG_IMM;
		args->in[args->arity++]      = be_transform_node(op1);
		args->reqs                   = reg_reqs;
	} else if (use_am) {
		ir_node *new_op        = be_transform_node(op);
		int      reg_input     = args->arity++;
		args->attr.u.reg_input = reg_input;
		args->in[reg_input]    = new_op;
		amd64_addr_t *addr     = &args->attr.base.addr;

		ir_node *ptr = get_Load_ptr(load);
		perform_address_matching(ptr, &(args->arity), args->in, addr);

		args->reqs = (use_xmm ? xmm_am_reqs : gp_am_reqs)[args->arity];

		ir_node *new_mem    = be_transform_node(get_Load_mem(load));
		int mem_input       = args->arity++;
		args->in[mem_input] = new_mem;
		addr->mem_input     = mem_input;

		args->mem_proj      = get_Proj_for_pn(load, pn_Load_M);
		args->attr.base.base.op_mode = AMD64_OP_ADDR_REG;
	} else {
		/* simply transform the arguments */
		args->in[args->arity++] = be_transform_node(op1);
		args->in[args->arity++] = be_transform_node(op2);
		args->attr.base.base.op_mode = AMD64_OP_REG_REG;

		args->reqs = use_xmm ? xmm_xmm_reqs : reg_reg_reqs;
	}
}

static ir_node *gen_binop_am(ir_node *node, ir_node *op1, ir_node *op2,
                             construct_binop_func func, unsigned pn_res,
                             match_flags_t flags)
{
	ir_node *block = get_nodes_block(node);
	ir_mode *mode  = get_irn_mode(node);
	amd64_args_t args;
	match_binop(&args, block, mode, op1, op2, flags);

	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_node(block);

	ir_node *new_node = func(dbgi, new_block, args.arity, args.in, &args.attr);
	arch_set_irn_register_reqs_in(new_node, args.reqs);

	fix_node_mem_proj(new_node, args.mem_proj);

	if (mode_is_float(mode)) {
		arch_set_irn_register_req_out(new_node, 0,
		                              &amd64_requirement_xmm_same_0);
		return new_r_Proj(new_node, amd64_mode_xmm, pn_res);
	} else {
		arch_set_irn_register_req_out(new_node, 0,
		                              &amd64_requirement_gp_same_0);
		return new_r_Proj(new_node, mode_gp, pn_res);
	}
}

static ir_node *gen_binop_rax(ir_node *node, ir_node *op1, ir_node *op2,
                              construct_rax_binop_func make_node,
                              match_flags_t flags)
{
	bool use_am;
	bool mode_neutral  = flags & match_mode_neutral;
	assert(! (flags & match_immediate));

	ir_mode *mode = get_irn_mode(op1);
	amd64_insn_mode_t insn_mode = get_insn_mode_from_mode(mode);

	/* TODO: legalize phase */
	if (mode_neutral) {
		op1 = be_skip_downconv(op1, true);
		op2 = be_skip_downconv(op2, true);
	} else {
		/* TODO: extend inputs? */
		(void)needs_extension;
	}

	ir_node *load;
	ir_node *op;
	ir_node *block = get_nodes_block(node);
	ir_node *in[4];
	int      arity = 0;
	const arch_register_req_t **reqs;
	amd64_op_mode_t op_mode;
	amd64_addr_t    addr;
	memset(&addr, 0, sizeof(addr));

	use_am = use_address_matching(mode, flags, block, op1, op2, &load, &op);

	ir_node *mem_proj = NULL;
	if (use_am) {
		ir_node *new_op    = be_transform_node(op);
		int      reg_input = arity++;
		in[reg_input]      = new_op;

		ir_node *ptr = get_Load_ptr(load);
		perform_address_matching(ptr, &arity, in, &addr);

		reqs = gp_am_reqs[arity];

		ir_node *new_mem = be_transform_node(get_Load_mem(load));
		int mem_input    = arity++;
		in[mem_input]    = new_mem;
		addr.mem_input   = mem_input;

		mem_proj                = get_Proj_for_pn(load, pn_Load_M);
		op_mode                 = AMD64_OP_RAX_ADDR;
	} else {
		/* simply transform the arguments */
		in[arity++] = be_transform_node(op1);
		in[arity++] = be_transform_node(op2);
		reqs        = rax_reg_reqs;
		op_mode     = AMD64_OP_RAX_REG;
	}

	assert((size_t)arity <= ARRAY_SIZE(in));
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *new_node  = make_node(dbgi, new_block, arity, in, insn_mode,
	                                op_mode, addr);
	arch_set_irn_register_reqs_in(new_node, reqs);
	if (mem_proj != NULL) {
		be_set_transformed_node(load, new_node);
	}
	return new_node;
}

static ir_node *gen_binop_xmm(ir_node *node, ir_node *op0, ir_node *op1,
                              construct_binop_func make_node,
                              match_flags_t flags)
{
	ir_node *block = get_nodes_block(node);
	ir_mode *mode  = get_irn_mode(op0);
	amd64_args_t args;
	memset(&args, 0, sizeof(args));

	ir_node *load;
	ir_node *op;
	bool use_am = use_address_matching(mode, flags, block, op0, op1, &load,
	                                   &op);

	if (use_am) {
		int reg_input = args.arity++;
		args.attr.u.reg_input = reg_input;
		args.in[reg_input]    = be_transform_node(op);

		amd64_addr_t *addr = &args.attr.base.addr;
		ir_node      *ptr  = get_Load_ptr(load);
		perform_address_matching(ptr, &args.arity, args.in, addr);

		args.reqs = xmm_am_reqs[args.arity];

		ir_node *new_mem   = be_transform_node(get_Load_mem(load));
		int mem_input      = args.arity++;
		args.in[mem_input] = new_mem;
		addr->mem_input    = mem_input;

		args.mem_proj      = get_Proj_for_pn(load, pn_Load_M);
		args.attr.base.base.op_mode = AMD64_OP_ADDR_REG;
	} else {
		args.in[args.arity++] = be_transform_node(op0);
		args.in[args.arity++] = be_transform_node(op1);
		args.attr.base.base.op_mode = AMD64_OP_REG_REG;
		args.reqs = xmm_xmm_reqs;
	}

	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_node(block);
	ir_node *new_node = make_node(dbgi, new_block, args.arity, args.in,
	                              &args.attr);
	arch_set_irn_register_reqs_in(new_node, args.reqs);

	fix_node_mem_proj(new_node, args.mem_proj);

	arch_set_irn_register_req_out(new_node, 0,
								  &amd64_requirement_xmm_same_0);
	return new_r_Proj(new_node, amd64_mode_xmm, pn_amd64_subs_res);
}

typedef ir_node *(*construct_shift_func)(dbg_info *dbgi, ir_node *block,
	int arity, ir_node *in[], const amd64_shift_attr_t *attr_init);

static ir_node *gen_shift_binop(ir_node *node, ir_node *op1, ir_node *op2,
                                construct_shift_func func, unsigned pn_res,
                                match_flags_t flags)
{
	ir_mode *mode = get_irn_mode(node);
	assert(!mode_is_float(mode));

	if (get_mode_modulo_shift(mode) != 32 && get_mode_size_bits(mode) != 64)
		panic("insupported modulo shift used");

	ir_node *in[3];
	int      arity = 0;
	if (flags & match_mode_neutral) {
		op1 = be_skip_downconv(op1, true);
		in[arity++] = be_transform_node(op1);
		mode = get_mode_size_bits(mode) > 32 ? mode_gp : mode_Iu;
	} else {
		op1 = be_skip_sameconv(op1);

		/* Use 8/16bit operations instead of doing zext/upconv */
		in[arity++] = be_transform_node(op1);
	}

	/* we can skip any convs for the shift count, as it only uses the lowest
	 * 5/6 bits anyway */
	while (is_Conv(op2) && get_irn_n_edges(op2) == 1) {
		ir_node *const op = get_Conv_op(op2);
		if (get_mode_arithmetic(get_irn_mode(op)) != irma_twos_complement)
			break;
		op2 = op;
	}

	amd64_shift_attr_t attr;
	memset(&attr, 0, sizeof(attr));
	const arch_register_req_t **reqs;
	const arch_register_req_t  *out_req0;
	if (is_Const(op2)) {
		attr.base.op_mode = AMD64_OP_SHIFT_IMM;
		reqs              = reg_reqs;
		out_req0          = &amd64_requirement_gp_same_0;
		attr.immediate    = get_Const_long(op2);
	} else {
		attr.base.op_mode = AMD64_OP_SHIFT_REG;
		in[arity++]       = be_transform_node(op2);
		reqs              = reg_rcx_reqs;
		out_req0          = &amd64_requirement_gp_same_0_not_1;
	}
	attr.insn_mode = get_insn_mode_from_mode(mode);

	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_nodes_block(node);
	ir_node  *const new_node  = func(dbgi, new_block, arity, in, &attr);
	arch_set_irn_register_reqs_in(new_node, reqs);
	arch_set_irn_register_req_out(new_node, 0, out_req0);
	return new_r_Proj(new_node, mode_gp, pn_res);
}

static ir_node *create_lea_as_add(ir_node *node, ir_node *op1, ir_node *op2)
{
	dbg_info *const dbgi = get_irn_dbg_info(node);
	ir_node  *new_block  = be_transform_nodes_block(node);
	ir_mode *mode        = get_irn_mode(node);

	amd64_insn_mode_t insn_mode;
	if (get_mode_size_bits(mode) <= 32)
		insn_mode = INSN_MODE_32;
	else
		insn_mode = INSN_MODE_64;

	const arch_register_req_t **reqs;
	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));

	ir_node *in[2];
	int arity = 0;

	if (match_immediate_32(&addr.immediate, op2, false, true)) {
		in[arity++]      = be_transform_node(op1);
		reqs             = reg_reqs;
		addr.index_input = NO_INPUT;
	} else {
		in[arity++]      = be_transform_node(op1);
		in[arity++]      = be_transform_node(op2);
		addr.base_input  = 0;
		addr.index_input = 1;
		reqs             = reg_reg_reqs;
	}

	ir_node *res = new_bd_amd64_lea(dbgi, new_block, arity, in, insn_mode, addr);
	arch_set_irn_register_reqs_in(res, reqs);
	return res;
}

static ir_node *gen_Add(ir_node *const node)
{
	match_flags_t flags = match_immediate | match_am | match_mode_neutral
	                      | match_commutative;

	ir_node *op1 = get_Add_left(node);
	ir_node *op2 = get_Add_right(node);

	ir_mode *mode  = get_irn_mode(node);
	ir_node *block = get_nodes_block(node);
	ir_node *load, *op;

	if (mode_is_float(mode)) {
		return gen_binop_am(node, op1, op2, new_bd_amd64_adds,
							pn_amd64_adds_res, match_commutative | match_am);
	}

	bool use_am = use_address_matching(mode, flags, block, op1, op2, &load, &op);

	ir_node *res;
	if (use_am)
		res = gen_binop_am(node, op1, op2, new_bd_amd64_add, pn_amd64_add_res,
		                   flags);
	else
		res = create_lea_as_add(node, op1, op2);

	x86_mark_non_am(node);
	return res;
}

static ir_node *gen_Sub(ir_node *const node)
{
	ir_node  *const op1     = get_Sub_left(node);
	ir_node  *const op2     = get_Sub_right(node);
	ir_mode  *const mode    = get_irn_mode(node);

	if (mode_is_float(mode)) {
		return gen_binop_am(node, op1, op2, new_bd_amd64_subs,
		                    pn_amd64_subs_res, match_am);
	} else {
		/* TODO: do not match AM yet until we have a sub->neg+add rule
		 * in amd64_finish */
		return gen_binop_am(node, op1, op2, new_bd_amd64_sub, pn_amd64_sub_res,
		                    match_immediate);
	}
}

static ir_node *gen_And(ir_node *const node)
{
	ir_node *op1 = get_And_left(node);
	ir_node *op2 = get_And_right(node);
	return gen_binop_am(node, op1, op2, new_bd_amd64_and, pn_amd64_and_res,
	                    match_immediate | match_am | match_mode_neutral
	                    | match_commutative);
}

static ir_node *gen_Eor(ir_node *const node)
{
	ir_node *op1 = get_Eor_left(node);
	ir_node *op2 = get_Eor_right(node);
	return gen_binop_am(node, op1, op2, new_bd_amd64_xor, pn_amd64_xor_res,
	                    match_immediate | match_am | match_mode_neutral
	                    | match_commutative);
}

static ir_node *gen_Or(ir_node *const node)
{
	ir_node *op1 = get_Or_left(node);
	ir_node *op2 = get_Or_right(node);
	return gen_binop_am(node, op1, op2, new_bd_amd64_or, pn_amd64_or_res,
	                    match_immediate | match_am | match_mode_neutral
	                    | match_commutative);
}

static ir_node *gen_Mul(ir_node *const node)
{
	ir_node *op1  = get_Mul_left(node);
	ir_node *op2  = get_Mul_right(node);
	ir_mode *mode = get_irn_mode(node);

	if (get_mode_size_bits(mode) < 16) {
		/* imulb only supports rax - reg form */
		ir_node *new_node =
		            gen_binop_rax(node, op1, op2, new_bd_amd64_imul_1op,
		                          match_mode_neutral
		                          | match_commutative);
		return new_r_Proj(new_node, mode_gp, pn_amd64_imul_1op_res_low);
	} else if (mode_is_float(mode)) {
		return gen_binop_am(node, op1, op2, new_bd_amd64_muls,
		                    pn_amd64_muls_res, match_commutative | match_am);
	} else {
		return gen_binop_am(node, op1, op2, new_bd_amd64_imul,
		                    pn_amd64_imul_res, match_immediate | match_am
		                    | match_mode_neutral | match_commutative);
	}
}

static ir_node *gen_Mulh(ir_node *const node)
{
	ir_node *op1  = get_Mulh_left(node);
	ir_node *op2  = get_Mulh_right(node);
	ir_mode *mode = get_irn_mode(op1);

	unsigned pn_res;
	ir_node *new_node;
	if (mode_is_signed(mode)) {
		new_node = gen_binop_rax(node, op1, op2, new_bd_amd64_imul_1op,
                        /* match_am TODO */
                        match_mode_neutral | match_commutative);
        pn_res = pn_amd64_imul_1op_res_high;
	} else {
		new_node = gen_binop_rax(node, op1, op2, new_bd_amd64_mul,
                         /* match_am TODO */
                         match_mode_neutral | match_commutative);
        pn_res = pn_amd64_mul_res_high;
	}
	return new_r_Proj(new_node, mode_gp, pn_res);
}

static ir_node *gen_Shl(ir_node *const node)
{
	ir_node *op1 = get_Shl_left(node);
	ir_node *op2 = get_Shl_right(node);
	return gen_shift_binop(node, op1, op2, new_bd_amd64_shl, pn_amd64_shl_res,
	                       match_immediate | match_mode_neutral);
}

static ir_node *gen_Shr(ir_node *const node)
{
	ir_node *op1 = get_Shr_left(node);
	ir_node *op2 = get_Shr_right(node);
	return gen_shift_binop(node, op1, op2, new_bd_amd64_shr, pn_amd64_shr_res,
	                       match_immediate);
}

static ir_node *gen_Shrs(ir_node *const node)
{
	ir_node *op1 = get_Shrs_left(node);
	ir_node *op2 = get_Shrs_right(node);
	return gen_shift_binop(node, op1, op2, new_bd_amd64_sar, pn_amd64_sar_res,
	                       match_immediate);
}

static ir_node *create_div(ir_node *const node, ir_mode *const mode,
                           ir_node *const op1, ir_node *const op2,
                           ir_node *const mem)
{
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_nodes_block(node);
	amd64_insn_mode_t insn_mode
		= get_mode_size_bits(mode) > 32 ? INSN_MODE_64 : INSN_MODE_32;

	ir_node *new_op1 = be_transform_node(op1);
	ir_node *new_op2 = be_transform_node(op2);
	ir_node *new_mem = be_transform_node(mem);
	ir_node *upper_value;
	ir_node *(*constructor)(dbg_info*,ir_node*,int,ir_node**,amd64_insn_mode_t);
	if (mode_is_signed(mode)) {
		upper_value = create_sext(new_block, op1, mode);
		constructor = new_bd_amd64_idiv;
	} else {
		upper_value = create_zext(new_block, node);
		constructor = new_bd_amd64_div;
	}

	ir_node *in[] = { new_op1, new_op2, upper_value, new_mem };
	ir_node *res = constructor(dbgi, new_block, ARRAY_SIZE(in), in, insn_mode);
	arch_set_irn_register_reqs_in(res, rax_reg_rdx_mem_reqs);
	return res;
}

static ir_node *create_sse_div(ir_node *const node, ir_mode *const mode,
                               ir_node *const op1, ir_node *const op2)
{
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const block     = get_nodes_block(node);
	ir_node  *const new_block = be_transform_node(block);

	amd64_args_t args;
	match_binop(&args, block, mode, op1, op2, match_am);

	ir_node *const new_node = new_bd_amd64_divs(dbgi, new_block, args.arity,
	                                            args.in, &args.attr);
	arch_set_irn_register_reqs_in(new_node, args.reqs);

	fix_node_mem_proj(new_node, args.mem_proj);

	if (args.reqs == xmm_xmm_reqs) {
		/* If this Div uses 2 xmm registers, we require
		 * "output 0 must be different from input 1" in order
		 * to give the finishing phase enough room for a copy,
		 * in case the requirement "output 0 should be the
		 * same as input 0" cannot be met.
		 */
		arch_set_irn_register_req_out(new_node, 0,
		                              &amd64_requirement_xmm_same_0_not_1);
	} else {
		arch_set_irn_register_req_out(new_node, 0,
		                              &amd64_requirement_xmm_same_0);
	}
	return new_node;
}

static ir_node *gen_Div(ir_node *const node)
{
	ir_mode *const mode = get_Div_resmode(node);
	ir_node *const op1  = get_Div_left(node);
	ir_node *const op2  = get_Div_right(node);
	ir_node *const mem  = get_Div_mem(node);

	if (mode_is_float(mode))
		return create_sse_div(node, mode, op1, op2);
	else
		return create_div(node, mode, op1, op2, mem);
}

static ir_node *gen_Proj_Div(ir_node *const node)
{
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);
	unsigned pn       = get_Proj_num(node);

	assert((unsigned)pn_amd64_div_M == (unsigned)pn_amd64_idiv_M);
	assert((unsigned)pn_amd64_div_res_div == (unsigned)pn_amd64_idiv_res_div);
	assert((unsigned)pn_amd64_divs_M == (unsigned)pn_amd64_idiv_M);
	assert((unsigned)pn_amd64_divs_res == (unsigned)pn_amd64_idiv_res_div);

	ir_mode *mode;
	if (mode_is_float(get_Div_resmode(pred)))
		mode = amd64_mode_xmm;
	else
		mode = mode_gp;

	switch ((pn_Div)pn) {
	case pn_Div_M:
		return new_r_Proj(new_pred, mode_M, pn_amd64_div_M);
	case pn_Div_res:
		return new_r_Proj(new_pred, mode, pn_amd64_div_res_div);
	case pn_Div_X_except:
	case pn_Div_X_regular:
		panic("amd64 exception NIY");
	}
	panic("invalid Div Proj");
}

static ir_node *gen_Mod(ir_node *const node)
{
	ir_mode *const mode = get_Mod_resmode(node);
	ir_node *const op1  = get_Mod_left(node);
	ir_node *const op2  = get_Mod_right(node);
	ir_node *const mem  = get_Mod_mem(node);
	assert(mode_needs_gp_reg(mode));
	return create_div(node, mode, op1, op2, mem);
}

static ir_node *gen_Proj_Mod(ir_node *const node)
{
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);
	unsigned pn       = get_Proj_num(node);

	assert((unsigned)pn_amd64_div_M == (unsigned)pn_amd64_idiv_M);
	assert((unsigned)pn_amd64_div_res_mod == (unsigned)pn_amd64_idiv_res_mod);
	switch ((pn_Mod)pn) {
	case pn_Mod_M:
		return new_r_Proj(new_pred, mode_M, pn_amd64_div_M);
	case pn_Mod_res:
		return new_r_Proj(new_pred, mode_gp, pn_amd64_div_res_mod);
	case pn_Mod_X_except:
	case pn_Mod_X_regular:
		panic("amd64 exception NIY");
	}
	panic("invalid Mod Proj");
}

typedef ir_node* (*unop_constructor)(dbg_info*,ir_node*block,ir_node*op,amd64_insn_mode_t insn_mode);

static ir_node *gen_unop(ir_node *const node, int op_pos, unop_constructor gen,
                         unsigned pn_res)
{
	dbg_info *const dbgi   = get_irn_dbg_info(node);
	ir_node  *const block  = be_transform_nodes_block(node);
	ir_node  *const op     = get_irn_n(node, op_pos);
	ir_node  *const new_op = be_transform_node(op);
	ir_mode  *const mode   = get_irn_mode(node);

	amd64_insn_mode_t insn_mode
		= get_mode_size_bits(mode) > 32 ? INSN_MODE_64 : INSN_MODE_32;
	ir_node *new_node = gen(dbgi, block, new_op, insn_mode);
	return new_r_Proj(new_node, mode_gp, pn_res);
}

/** Create a floating point negation by switching the sign bit using a xor.
  */
static ir_node *gen_float_neg(ir_node *const node)
{
	dbg_info  *const dbgi = get_irn_dbg_info(node);
	ir_node   *new_block  = be_transform_nodes_block(node);
	ir_node   *op         = get_irn_n(node, n_Minus_op);
	ir_node   *new_op     = be_transform_node(op);
	ir_mode   *mode       = get_irn_mode(node);
	ir_tarval *tv         = create_sign_tv(mode);
	ir_node   *load       = create_float_const(dbgi, new_block, tv);
	ir_node   *in[]       = { new_op, load };

	amd64_binop_addr_attr_t attr;
	memset(&attr, 0, sizeof(attr));
	attr.base.base.op_mode = AMD64_OP_REG_REG;
	attr.base.insn_mode    = get_insn_mode_from_mode(mode);

	ir_node *xor = new_bd_amd64_xorp(dbgi, new_block, ARRAY_SIZE(in),
	                                  in, &attr);
	arch_set_irn_register_reqs_in(xor, xmm_xmm_reqs);
	arch_set_irn_register_req_out(xor, 0, &amd64_requirement_xmm_same_0);

	return new_r_Proj(xor, amd64_mode_xmm, pn_amd64_xorp_res);
}

static ir_node *gen_Minus(ir_node *const node)
{
	ir_mode *mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		return gen_float_neg(node);
	} else {
		return gen_unop(node, n_Minus_op, &new_bd_amd64_neg, pn_amd64_neg_res);
	}
}

static ir_node *gen_Not(ir_node *const node)
{
	return gen_unop(node, n_Not_op, &new_bd_amd64_not, pn_amd64_not_res);
}

static ir_node *gen_Member(ir_node *const node)
{
	ir_node   *new_block = be_transform_nodes_block(node);
	dbg_info  *dbgi      = get_irn_dbg_info(node);
	ir_node   *ptr       = get_Member_ptr(node);
	ir_graph  *irg       = get_irn_irg(node);
	ir_node   *base      = get_frame_base(irg);
	ir_entity *entity    = get_Member_entity(node);
	if (!is_Proj(ptr) || !is_Start(get_Proj_pred(ptr)))
		panic("Sel not lowered");
	if (is_parameter_entity(entity) &&
	    get_entity_parameter_number(entity) == IR_VA_START_PARAMETER_NUMBER)
	    panic("va_start NIY");
	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.base_input  = 0;
	addr.index_input = NO_INPUT;
	addr.immediate.entity = entity;
	ir_node *in[] = { base };
	ir_node *res = new_bd_amd64_lea(dbgi, new_block, ARRAY_SIZE(in), in,
	                                INSN_MODE_64, addr);
	arch_set_irn_register_reqs_in(res, reg_reqs);
	return res;
}

static ir_node *gen_IJmp(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	ir_node  *op        = get_IJmp_target(node);
	assert(get_irn_mode(op) == mode_P);

	int arity = 0;
	ir_node *in[3];
	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));

	const arch_register_req_t **reqs;
	amd64_op_mode_t             op_mode;
	ir_node                    *mem_proj = NULL;
	if (match_immediate_32(&addr.immediate, op, true, false)) {
		// TODO: do we need a must_match_ip_relative in match_immediate_32
		op_mode = AMD64_OP_UNOP_IMM32;
		arity   = 0;
		reqs    = no_reqs;
	} else {
		ir_node *load = source_am_possible(block, op);
		if (load != NULL) {
			ir_node *load_ptr = get_Load_ptr(load);
			mem_proj          = get_Proj_for_pn(load, pn_Load_M);

			perform_address_matching(load_ptr, &arity, in, &addr);
			assert((size_t)arity < ARRAY_SIZE(in));

			reqs = gp_am_reqs[arity];
			ir_node *load_mem = get_Load_mem(load);
			ir_node *new_mem  = be_transform_node(load_mem);
			int mem_input     = arity++;
			in[mem_input]     = new_mem;
			addr.mem_input    = mem_input;

			op_mode = AMD64_OP_UNOP_ADDR;
		} else {
			op_mode          = AMD64_OP_UNOP_REG;
			assert(arity == 0); // UNOP_REG always outputs the first input
			in[arity++]      = be_transform_node(op);
			addr.base_input  = NO_INPUT;
			addr.index_input = NO_INPUT;
			reqs             = reg_reqs;
		}
	}

	ir_node *jmp = new_bd_amd64_ijmp(dbgi, new_block, arity, in,
	                                 INSN_MODE_64, op_mode, addr);

	arch_set_irn_register_reqs_in(jmp, reqs);
	fix_node_mem_proj(jmp, mem_proj);

	ir_node *proj_X = new_r_Proj(jmp, mode_X, pn_amd64_ijmp_X);
	return proj_X;
}

static ir_node *gen_Jmp(ir_node *node)
{
	ir_node  *new_block = be_transform_nodes_block(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);

	return new_bd_amd64_jmp(dbgi, new_block);
}

static ir_node *gen_Switch(ir_node *node)
{
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_node  *sel       = get_Switch_selector(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_sel   = be_transform_node(sel);
	const ir_switch_table *table  = get_Switch_table(node);
	unsigned               n_outs = get_Switch_n_outs(node);

	ir_type   *const utype = get_unknown_type();
	ir_entity *const entity
		= new_entity(irp->dummy_owner, id_unique("TBL%u"), utype);
	set_entity_visibility(entity, ir_visibility_private);
	add_entity_linkage(entity, IR_LINKAGE_CONSTANT);

	table = ir_switch_table_duplicate(irg, table);

	ir_node *out = new_bd_amd64_jmp_switch(dbgi, new_block, new_sel, n_outs,
	                                       table, entity);
	return out;
}

static ir_node *gen_Start(ir_node *node)
{
	ir_graph  *irg           = get_irn_irg(node);
	ir_entity *entity        = get_irg_entity(irg);
	ir_type   *function_type = get_entity_type(entity);
	ir_node   *new_block     = be_transform_nodes_block(node);
	dbg_info  *dbgi          = get_irn_dbg_info(node);

	x86_cconv_t const *const cconv = current_cconv;

	/* start building list of start constraints */

	/* calculate number of outputs */
	size_t n_outs = 2; /* memory, rsp */
	/* function parameters */
	n_outs += cconv->n_param_regs;
	size_t n_callee_saves
		= rbitset_popcount(cconv->callee_saves, N_AMD64_REGISTERS);
	n_outs += n_callee_saves;

	ir_node *start = new_bd_amd64_start(dbgi, new_block, n_outs);

	size_t o = 0;

	/* first output is memory */
	be_make_start_mem(&start_mem, start, o++);

	/* the stack pointer */
	be_make_start_out(&start_val[REG_RSP], start, o++, &amd64_registers[REG_RSP], true);

	/* function parameters in registers */
	for (size_t i = 0; i < get_method_n_params(function_type); ++i) {
		const reg_or_stackslot_t *param = &current_cconv->parameters[i];
		const arch_register_t    *reg   = param->reg;
		if (reg)
			be_make_start_out(&start_val[reg->global_index], start, o++, reg, false);
	}

	/* callee saves */
	for (size_t i = 0; i < N_AMD64_REGISTERS; ++i) {
		if (!rbitset_is_set(cconv->callee_saves, i))
			continue;
		bool ignore = i == REG_RBP && !cconv->omit_fp;
		be_make_start_out(&start_val[i], start, o++, &amd64_registers[i], ignore);
	}
	assert(n_outs == o);

	return start;
}

static ir_node *gen_Proj_Start(ir_node *node)
{
	ir_graph *irg = get_irn_irg(node);
	unsigned  pn  = get_Proj_num(node);
	be_transform_node(get_Proj_pred(node));

	switch ((pn_Start)pn) {
	case pn_Start_M:
		return get_initial_mem(irg);
	case pn_Start_T_args:
		return new_r_Bad(irg, mode_T);
	case pn_Start_P_frame_base:
		return get_frame_base(irg);
	}
	panic("unexpected Start Proj: %u", pn);
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

static ir_node *gen_Return(ir_node *node)
{
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *mem       = get_Return_mem(node);
	ir_node  *new_mem   = be_transform_node(mem);
	ir_node  *sp        = get_stack_pointer_for(node);
	size_t    n_res     = get_Return_n_ress(node);
	x86_cconv_t    *cconv   = current_cconv;

	/* estimate number of return values */
	size_t       p              = n_amd64_ret_first_result;
	size_t const n_callee_saves = rbitset_popcount(cconv->callee_saves, N_AMD64_REGISTERS);
	size_t const n_ins          = p + n_res + n_callee_saves;

	arch_register_req_t const **const reqs = be_allocate_in_reqs(irg, n_ins);
	ir_node **in = ALLOCAN(ir_node*, n_ins);

	in[n_amd64_ret_mem]   = new_mem;
	reqs[n_amd64_ret_mem] = arch_no_register_req;

	in[n_amd64_ret_stack]   = sp;
	reqs[n_amd64_ret_stack] = amd64_registers[REG_RSP].single_req;

	/* result values */
	for (size_t i = 0; i < n_res; ++i) {
		ir_node                  *res_value     = get_Return_res(node, i);
		ir_node                  *new_res_value = be_transform_node(res_value);
		const reg_or_stackslot_t *slot          = &current_cconv->results[i];
		in[p]   = new_res_value;
		reqs[p] = slot->reg->single_req;
		++p;
	}
	/* callee saves */
	for (size_t i = 0; i < N_AMD64_REGISTERS; ++i) {
		if (!rbitset_is_set(cconv->callee_saves, i))
			continue;
		in[p]   = be_get_start_proj(irg, &start_val[i]);
		reqs[p] = amd64_registers[i].single_req;
		++p;
	}
	assert(p == n_ins);

	ir_node *returnn = new_bd_amd64_ret(dbgi, new_block, n_ins, in);
	arch_set_irn_register_reqs_in(returnn, reqs);

	return returnn;
}

static ir_node *gen_Call(ir_node *node)
{
	ir_node         *callee       = get_Call_ptr(node);
	ir_node         *block        = get_nodes_block(node);
	ir_node         *new_block    = be_transform_node(block);
	dbg_info        *dbgi         = get_irn_dbg_info(node);
	ir_node         *mem          = get_Call_mem(node);
	ir_node         *new_mem      = be_transform_node(mem);
	ir_type         *type         = get_Call_type(node);
	size_t           n_params     = get_Call_n_params(node);
	size_t           n_ress       = get_method_n_ress(type);
	/* max inputs: memory, callee, register arguments */
	ir_node        **sync_ins     = ALLOCAN(ir_node*, n_params+1);
	ir_graph        *irg          = get_irn_irg(node);
	x86_cconv_t   *cconv
		= amd64_decide_calling_convention(type, NULL);
	size_t           n_param_regs = cconv->n_param_regs;
	/* param-regs + mem + stackpointer + callee(2) + n_sse_regs */
	unsigned         max_inputs   = 5 + n_param_regs;
	ir_node        **in           = ALLOCAN(ir_node*, max_inputs);
	int              in_arity     = 0;
	int              sync_arity   = 0;
	ir_node         *new_frame    = get_stack_pointer_for(node);

	assert(n_params == get_method_n_params(type));

	/* construct arguments */

	/* stack pointer input */
	/* construct an IncSP -> we have to always be sure that the stack is
	 * aligned even if we don't push arguments on it */
	const arch_register_t *sp_reg = &amd64_registers[REG_RSP];
	ir_node *incsp = amd64_new_IncSP(new_block, new_frame,
	                                 cconv->callframe_size,
	                                 AMD64_PO2_STACK_ALIGNMENT);

	/* match callee */
	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.mem_input = NO_INPUT;
	amd64_op_mode_t op_mode;

	ir_node *mem_proj = NULL;

	arch_register_req_t const **const in_req = be_allocate_in_reqs(irg, max_inputs);

	if (match_immediate_32(&addr.immediate, callee, true, true)) {
		op_mode = AMD64_OP_UNOP_IMM32;
	} else {
		ir_node *load    = source_am_possible(block, callee);
		bool     am_okay = false;
		if (load != NULL) {
			am_okay = true;
			/* none of the other Call inputs must depend on the load */
			foreach_irn_in(node, i, in) {
				if (i == n_Call_ptr)
					continue;
				/* if the memory input is the load memory output, then we are
				 * fine.
				 * TODO: handle more complicated Sync cases
				 */
				if (i == n_Call_mem && is_Proj(in) && get_Proj_pred(in) == load)
					continue;
				if (input_depends_on_load(load, in)) {
					am_okay = false;
					break;
				}
			}
		}
		if (am_okay) {
			ir_node *load_ptr = get_Load_ptr(load);
			mem_proj = get_Proj_for_pn(load, pn_Load_M);

			perform_address_matching(load_ptr, &in_arity, in, &addr);

			if (addr.base_input != NO_INPUT) {
				in_req[addr.base_input] = &amd64_class_reg_req_gp;
			}

			if (addr.index_input != NO_INPUT) {
				in_req[addr.index_input] = &amd64_class_reg_req_gp;
			}

			ir_node *load_mem     = get_Load_mem(load);
			ir_node *new_load_mem = be_transform_node(load_mem);
			sync_ins[sync_arity++] = new_load_mem;

			op_mode = AMD64_OP_UNOP_ADDR;
		} else {
			int input = in_arity++;
			assert(input == 0); /* UNOP_REG is currently hardcoded to always
			                     * output the register of the first input. */
			in[input]          = be_transform_node(callee);
			in_req[input]      = &amd64_class_reg_req_gp;
			addr.base_input    = NO_INPUT;
			addr.index_input   = NO_INPUT;
			op_mode            = AMD64_OP_UNOP_REG;
		}
	}

	in_req[in_arity] = sp_reg->single_req;
	in[in_arity]     = incsp;
	++in_arity;

	/* vararg calls need the number of SSE registers used */
	if (get_method_variadicity(type) == variadicity_variadic) {
		unsigned xmm_regs = cconv->n_xmm_regs;
		ir_node *xmm_imm  = new_bd_amd64_mov_imm(dbgi, block, INSN_MODE_32,
		                                         xmm_regs, NULL);
		in_req[in_arity] = amd64_registers[REG_RAX].single_req;
		in[in_arity] = xmm_imm;
		++in_arity;
	}

	/* parameters */
	for (size_t p = 0; p < n_params; ++p) {
		ir_node                  *value = get_Call_param(node, p);
		const reg_or_stackslot_t *param = &cconv->parameters[p];

		ir_node *new_value = be_transform_node(value);

		/* put value into registers */
		if (param->reg != NULL) {
			in[in_arity]     = new_value;
			in_req[in_arity] = param->reg->single_req;
			++in_arity;
			continue;
		}

		ir_mode *mode  = get_type_mode(get_method_param_type(type, p));

		/* we need a store if we're here */
		amd64_binop_addr_attr_t attr;
		memset(&attr, 0, sizeof(attr));
		attr.base.base.op_mode          = AMD64_OP_ADDR_REG;
		attr.base.addr.immediate.offset = param->offset;
		attr.base.addr.base_input       = 1;
		attr.base.addr.index_input      = NO_INPUT;
		attr.base.insn_mode             = INSN_MODE_64;
		ir_node *in[] = { new_value, incsp, new_mem };

		const arch_register_req_t **reqs;
		ir_node *store;

		if (mode_is_float(mode)) {
			store = new_bd_amd64_movs_store_xmm(dbgi, new_block,
			                                    ARRAY_SIZE(in), in, &attr);

			reqs  = xmm_reg_mem_reqs;
		} else {
			store = new_bd_amd64_mov_store(dbgi, new_block,
			                               ARRAY_SIZE(in), in, &attr);
			reqs  = reg_reg_mem_reqs;
		}

		arch_set_irn_register_reqs_in(store, reqs);

		set_irn_pinned(store, op_pin_state_floats);
		sync_ins[sync_arity++] = store;
	}

	/* memory input */
	in_req[in_arity] = arch_no_register_req;
	int mem_pos      = in_arity;
	addr.mem_input   = mem_pos;
	++in_arity;

	/* construct memory input */
	if (sync_arity == 0) {
		in[mem_pos] = new_mem;
	} else if (sync_arity == 1) {
		in[mem_pos] = sync_ins[0];
	} else {
		in[mem_pos] = new_r_Sync(new_block, sync_arity, sync_ins);
	}

	assert(in_arity <= (int)max_inputs);

	/* count outputs */
	unsigned       o              = pn_amd64_call_first_result;
	unsigned const n_caller_saves = rbitset_popcount(cconv->caller_saves, N_AMD64_REGISTERS);
	unsigned const out_arity      = o + cconv->n_reg_results + n_caller_saves;

	/* create call node */
	ir_node *call = new_bd_amd64_call(dbgi, new_block, in_arity, in, out_arity,
	                                  op_mode, addr);
	arch_set_irn_register_reqs_in(call, in_req);

	fix_node_mem_proj(call, mem_proj);

	/* create output register reqs */
	arch_set_irn_register_req_out(call, pn_amd64_call_M, arch_no_register_req);
	arch_copy_irn_out_info(call, pn_amd64_call_stack, incsp);

	arch_register_class_t const *const flags = &amd64_reg_classes[CLASS_amd64_flags];
	arch_set_irn_register_req_out(call, pn_amd64_call_flags, flags->class_req);

	/* add register requirements for the result regs */
	for (size_t r = 0; r < n_ress; ++r) {
		const reg_or_stackslot_t  *result_info = &cconv->results[r];
		const arch_register_t     *reg         = result_info->reg;
		if (reg != NULL)
			arch_set_irn_register_req_out(call, o++, reg->single_req);
	}
	const unsigned *allocatable_regs = be_birg_from_irg(irg)->allocatable_regs;
	for (size_t i = 0; i < N_AMD64_REGISTERS; ++i) {
		if (!rbitset_is_set(cconv->caller_saves, i))
			continue;
		const arch_register_t *reg = &amd64_registers[i];
		arch_set_irn_register_req_out(call, o, reg->single_req);
		if (!rbitset_is_set(allocatable_regs, reg->global_index))
			arch_set_irn_register_out(call, o, reg);
		++o;
	}
	assert(o == out_arity);

	/* copy pinned attribute */
	set_irn_pinned(call, get_irn_pinned(node));

	/* IncSP to destroy the call stackframe */
	ir_node *const call_stack = new_r_Proj(call, mode_gp, pn_amd64_call_stack);
	incsp = amd64_new_IncSP(new_block, call_stack, -cconv->callframe_size, 0);
	/* if we are the last IncSP producer in a block then we have to keep
	 * the stack value.
	 * Note: This here keeps all producers which is more than necessary */
	keep_alive(incsp);

	pmap_insert(node_to_stack, node, incsp);

	x86_free_calling_convention(cconv);
	return call;
}

static ir_node *gen_Proj_Call(ir_node *node)
{
	unsigned pn       = get_Proj_num(node);
	ir_node *call     = get_Proj_pred(node);
	ir_node *new_call = be_transform_node(call);
	switch ((pn_Call)pn) {
	case pn_Call_M:
		return new_r_Proj(new_call, mode_M, pn_amd64_call_M);
	case pn_Call_X_regular:
	case pn_Call_X_except:
	case pn_Call_T_result:
		break;
	}
	panic("unexpected Call proj %+F", node);
}

static ir_node *gen_Proj_Proj_Call(ir_node *node)
{
	ir_node *const call     = get_Proj_pred(get_Proj_pred(node));
	ir_node *const new_call = be_transform_node(call);

	ir_mode *mode = get_irn_mode(node);
	if (mode_needs_gp_reg(mode))
		mode = mode_gp;
	else if (mode_is_float(mode))
		mode = amd64_mode_xmm;

	unsigned const pn     = get_Proj_num(node);
	unsigned const new_pn = pn_amd64_call_first_result + pn;

	return new_r_Proj(new_call, mode, new_pn);
}

static ir_node *gen_Proj_Proj_Start(ir_node *node)
{
	assert(get_Proj_num(get_Proj_pred(node)) == pn_Start_T_args);

	ir_graph                 *const irg   = get_irn_irg(node);
	unsigned                  const pn    = get_Proj_num(node);
	reg_or_stackslot_t const *const param = &current_cconv->parameters[pn];
	arch_register_t    const *const reg   = param->reg;
	if (reg) {
		/* argument transmitted in register */
		return be_get_start_proj(irg, &start_val[reg->global_index]);
	} else {
		/* argument transmitted on stack */
		ir_mode  *mode = get_type_mode(param->type);
		ir_node  *base = get_frame_base(irg);

		amd64_insn_mode_t insn_mode = get_insn_mode_from_mode(mode);
		amd64_addr_t      addr;
		memset(&addr, 0, sizeof(addr));
		addr.base_input       = 0;
		addr.index_input      = NO_INPUT;
		addr.mem_input        = NO_INPUT;
		addr.immediate.entity = param->entity;
		ir_node *in[]  = { base };
		ir_node *load;
		ir_node *value;

		ir_node *const new_block = be_transform_nodes_block(node);
		if (mode_is_float(mode)) {
			load  = new_bd_amd64_movs_xmm(NULL, new_block, ARRAY_SIZE(in),
			                              in, insn_mode, AMD64_OP_ADDR, addr);
			value = new_r_Proj(load, amd64_mode_xmm, pn_amd64_movs_xmm_res);
		} else if (get_mode_size_bits(mode) < 64 && mode_is_signed(mode)) {
			load  = new_bd_amd64_movs(NULL, new_block, ARRAY_SIZE(in),
			                          in, insn_mode, AMD64_OP_ADDR, addr);
			value = new_r_Proj(load, mode_gp, pn_amd64_movs_res);
		} else {
			load  = new_bd_amd64_mov_gp(NULL, new_block, ARRAY_SIZE(in),
			                            in, insn_mode, AMD64_OP_ADDR, addr);
			value = new_r_Proj(load, mode_gp, pn_amd64_mov_gp_res);
		}
		arch_set_irn_register_reqs_in(load, reg_mem_reqs);
		set_irn_pinned(load, op_pin_state_floats);
		return value;
	}
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
	panic("amd64: unexpected Proj(Proj) after %+F", pred_pred);
}

static ir_node *gen_Cmp(ir_node *node)
{
	ir_node *op1      = get_Cmp_left(node);
	ir_node *op2      = get_Cmp_right(node);
	ir_mode *cmp_mode = get_irn_mode(op1);

	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *block           = get_nodes_block(node);
	ir_node  *const new_block = be_transform_node(block);

	ir_node     *new_node;
	amd64_args_t args;
	if (mode_is_float(cmp_mode)) {
		match_binop(&args, block, cmp_mode, op1, op2, match_am);

		new_node = new_bd_amd64_ucomis(dbgi, new_block, args.arity,
		                               args.in, &args.attr);
	} else {
		match_binop(&args, block, cmp_mode, op1, op2, match_immediate
		            | match_am);

		new_node = new_bd_amd64_cmp(dbgi, new_block, args.arity,
		                            args.in, &args.attr);
	}

	arch_set_irn_register_reqs_in(new_node, args.reqs);
	fix_node_mem_proj(new_node, args.mem_proj);
	return new_r_Proj(new_node, mode_flags, pn_amd64_cmp_flags);
}

static ir_node *get_flags_node(ir_node *cmp, x86_condition_code_t *cc_out)
{
	/* must have a Cmp as input */
	ir_relation relation = get_Cmp_relation(cmp);
	ir_node    *l        = get_Cmp_left(cmp);
	ir_node    *r        = get_Cmp_right(cmp);
	ir_mode    *mode     = get_irn_mode(l);

	/* the middle-end tries to eliminate impossible relations, so a ptr <> 0
	 * test becomes ptr > 0. But for x86 an equal comparison is preferable to
	 * a >0 (we can sometimes eliminate the cmp in favor of flags produced by
	 * a predecessor node). So add the < bit.
	 * (Note that we do not want to produce <=> (which can happen for
	 * unoptimized code), because no x86 flag can represent that */
	if (!(relation & ir_relation_equal) && relation & ir_relation_less_greater)
		relation |= get_negated_relation(ir_get_possible_cmp_relations(l, r)) & ir_relation_less_greater;

	bool const overflow_possible = !is_Const(r) || !is_Const_null(r);

	/* just do a normal transformation of the Cmp */
	*cc_out = ir_relation_to_x86_condition_code(relation, mode,
	                                            overflow_possible);
	ir_node *flags = be_transform_node(cmp);
	return flags;
}

static ir_node *gen_Cond(ir_node *node)
{
	ir_node             *sel = get_Cond_selector(node);
	x86_condition_code_t cc;
	ir_node             *flags = get_flags_node(sel, &cc);

	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	return new_bd_amd64_jcc(dbgi, block, flags, cc);
}

static ir_node *gen_ASM(ir_node *node)
{
	return x86_match_ASM(node, amd64_additional_clobber_names, &amd64_asm_constraints);
}

static ir_node *gen_Proj_ASM(ir_node *node)
{
	ir_mode *mode     = get_irn_mode(node);
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);
	unsigned pn       = get_Proj_num(node);

	if (mode == mode_M) {
		pn = arch_get_irn_n_outs(new_pred)-1;
	} else if (mode_is_int(mode) || mode_is_reference(mode)) {
		mode = mode_gp;
	} else if (mode_is_float(mode)) {
		mode = amd64_mode_xmm;
	} else {
		panic("unexpected proj mode at ASM");
	}

	return new_r_Proj(new_pred, mode, pn);
}

static ir_node *gen_Phi(ir_node *node)
{
	ir_mode                   *mode = get_irn_mode(node);
	const arch_register_req_t *req;
	if (mode_needs_gp_reg(mode)) {
		/* all integer operations are on 64bit registers now */
		req = amd64_reg_classes[CLASS_amd64_gp].class_req;
	} else if (mode_is_float(mode)) {
		req = amd64_reg_classes[CLASS_amd64_xmm].class_req;
	} else {
		req = arch_no_register_req;
	}

	return be_transform_phi(node, req);
}

typedef ir_node* (*create_mov_func)(dbg_info *dbgi, ir_node *block, int arity,
                                    ir_node **in, amd64_op_mode_t op_mode,
                                    amd64_addr_t addr);

static ir_node *match_mov(dbg_info *dbgi, ir_node *block, ir_node *value,
                          create_mov_func create_mov, unsigned pn_res)
{
	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	ir_node *load;
	ir_node *op;
	int      arity = 0;
	ir_node *in[4];
	ir_mode *mode = get_irn_mode(value);
	bool use_am = use_address_matching(mode, match_am, block, NULL,
									   value, &load, &op);

	amd64_op_mode_t op_mode;
	const arch_register_req_t **reqs;
	ir_node *mem_proj = NULL;
	if (use_am) {
		ir_node *ptr = get_Load_ptr(load);
		perform_address_matching(ptr, &arity, in, &addr);

		reqs = gp_am_reqs[arity];

		ir_node *new_mem = be_transform_node(get_Load_mem(load));
		int mem_input  = arity++;
		in[mem_input]  = new_mem;
		addr.mem_input = mem_input;

		mem_proj = get_Proj_for_pn(load, pn_Load_M);
		op_mode  = AMD64_OP_ADDR;
	} else {
		ir_node *new_value = be_transform_node(value);
		in[arity++] = new_value;
		reqs        = get_irn_mode(new_value) == amd64_mode_xmm ? amd64_xmm_reqs
		                                                        : reg_reqs;
		op_mode     = AMD64_OP_REG;
	}

	assert((size_t)arity <= ARRAY_SIZE(in));
	ir_node *new_node = create_mov(dbgi, block, arity, in, op_mode,
	                               addr);
	arch_set_irn_register_reqs_in(new_node, reqs);

	if (mem_proj != NULL)
		be_set_transformed_node(load, new_node);

	ir_node *res = new_r_Proj(new_node, amd64_mode_xmm, pn_res);
	return res;
}

static ir_node *create_movq(dbg_info *dbgi, ir_node *block, ir_node *value)
{
	return match_mov(dbgi, block, value, new_bd_amd64_movq, pn_amd64_movq_res);
}

static ir_node *create_cvtsd2ss(dbg_info *dbgi, ir_node *block, ir_node *value)
{
	return match_mov(dbgi, block, value, new_bd_amd64_cvtsd2ss,
	                 pn_amd64_cvtsd2ss_res);
}

static ir_node *gen_Conv(ir_node *node)
{
	ir_node  *block    = be_transform_nodes_block(node);
	ir_node  *op       = get_Conv_op(node);
	ir_mode  *src_mode = get_irn_mode(op);
	ir_mode  *dst_mode = get_irn_mode(node);
	dbg_info *dbgi     = get_irn_dbg_info(node);

	int src_bits = get_mode_size_bits(src_mode);
	int dst_bits = get_mode_size_bits(dst_mode);
	/* ad-hoc assumption until libfirm has vector modes:
	 * we assume 128bit modes are two packed doubles. */
	if (dst_bits == 128) {
		/* int -> 2xdouble */
		assert(get_mode_arithmetic(src_mode) == irma_twos_complement);
		assert(src_bits == 64);
		assert(dst_mode == amd64_mode_xmm);
		return create_movq(dbgi, block, op);
	} else if (src_bits == 128) {
		/* 2xdouble -> float */
		assert(src_mode == amd64_mode_xmm);
		assert(mode_is_float(dst_mode));
		if (dst_bits == 32) {
			return create_cvtsd2ss(dbgi, block, op);
		} else if (dst_bits == 64) {
			/* this is a NOP */
			return be_transform_node(op);
		} else {
			panic("amd64: can't transform %+F", node);
		}
	}

	bool src_float = mode_is_float(src_mode);
	bool dst_float = mode_is_float(dst_mode);
	bool is_gp     = !src_float && !dst_float;

	ir_mode *min_mode;
	if (src_bits < dst_bits) {
		min_mode = src_mode;
	} else if (src_bits > dst_bits) {
		min_mode = dst_mode;
	} else if ((src_float && dst_float) || is_gp) {
		/* skip unnecessary conv */
		return be_transform_node(op);
	} else {
		/* src_bits == dst_bits, but one is float the other integer*/
		min_mode = src_mode;
	}

	if (is_gp && be_upper_bits_clean(op, min_mode))
		return be_transform_node(op);

	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));

	amd64_insn_mode_t insn_mode;
	if (!is_gp && get_mode_size_bits(min_mode) < 32) {
		/* Only 32-bit and 64-bit register size allowed for
		 * floating point conversion */
		insn_mode = INSN_MODE_32;
	} else if (!src_float && dst_float) {
		 /* In this case the size of the the floating point mode is irrelevant
		  * and could result in invalid register sizes. */
		insn_mode = get_insn_mode_from_mode(src_mode);
	} else {
		insn_mode = get_insn_mode_from_mode(min_mode);
	}

	ir_node *new_op = be_transform_node(op);
	ir_node *in[1]  = { new_op };
	ir_node *conv;
	ir_node *res;
	const arch_register_req_t **reqs;

	if (src_float && dst_float) {
		/* float to float */
		unsigned pn_res;
		if (src_bits < dst_bits) {
			conv = new_bd_amd64_cvtss2sd(dbgi, block, ARRAY_SIZE(in),
			                             in, insn_mode, AMD64_OP_REG,
			                             addr);
			pn_res = pn_amd64_cvtss2sd_res;
		} else {
			conv = new_bd_amd64_cvtsd2ss(dbgi, block, ARRAY_SIZE(in),
			                             in, AMD64_OP_REG, addr);
			pn_res = pn_amd64_cvtsd2ss_res;
		}
		res  = new_r_Proj(conv, amd64_mode_xmm, pn_res);
		reqs = amd64_xmm_reqs;

	} else if (src_float && !dst_float) {
		/* float to int */

		if (!mode_is_signed(dst_mode) && dst_bits <= 32) {
			/* The conversion is signed only; simply use 64-bit register*/
			insn_mode = INSN_MODE_64;
		} else if (!mode_is_signed(dst_mode) && dst_bits == 64) {
			panic("cannot convert floating point to 64-bit unsigned");
		}

		unsigned pn_res;
		if (src_bits < 64) {
			conv = new_bd_amd64_cvttss2si(dbgi, block, ARRAY_SIZE(in),
			                             in, insn_mode, AMD64_OP_REG,
			                             addr);
			pn_res = pn_amd64_cvttss2si_res;
		} else {
			conv = new_bd_amd64_cvttsd2si(dbgi, block, ARRAY_SIZE(in),
			                             in, insn_mode, AMD64_OP_REG,
			                             addr);
			pn_res = pn_amd64_cvttsd2si_res;
		}
		res  = new_r_Proj(conv, mode_gp, pn_res);
		reqs = amd64_xmm_reqs;

	} else if (!src_float && dst_float) {
		/* int to float */

		if (!mode_is_signed(src_mode) && src_bits <= 32) {
			/* Conversion is signed only, therefore use up to 64-bit register
			 * size and require that the upper bits are zero. This is done with
			 * an explicit move instruction */

			insn_mode = INSN_MODE_64;
			amd64_insn_mode_t move_mode = get_insn_mode_from_mode(src_mode);

			ir_node *ext = new_bd_amd64_mov_gp(dbgi, block, ARRAY_SIZE(in),
			                                   in, move_mode,
			                                   AMD64_OP_REG, addr);
			arch_set_irn_register_reqs_in(ext, reg_reqs);
			in[0] = new_r_Proj(ext, mode_gp, pn_amd64_mov_gp_res);

		} else if (!mode_is_signed(src_mode) && src_bits == 64) {
			panic("cannot convert 64-bit unsigned to floating point");
		}

		unsigned pn_res;
		if (dst_bits < 64) {
			conv = new_bd_amd64_cvtsi2ss(dbgi, block, ARRAY_SIZE(in),
			                             in, insn_mode, AMD64_OP_REG,
			                             addr);
			pn_res = pn_amd64_cvtsi2ss_res;
		} else {
			conv = new_bd_amd64_cvtsi2sd(dbgi, block, ARRAY_SIZE(in),
			                             in, insn_mode, AMD64_OP_REG,
			                             addr);
			pn_res = pn_amd64_cvtsi2sd_res;
		}
		res = new_r_Proj(conv, amd64_mode_xmm, pn_res);
		reqs = reg_reqs;

	} else {
		/* int to int */
		unsigned pn_res;
		if (!mode_is_signed(min_mode) || get_mode_size_bits(min_mode) == 64) {
			conv = new_bd_amd64_mov_gp(dbgi, block, ARRAY_SIZE(in),
			                           in, insn_mode, AMD64_OP_REG, addr);
			pn_res = pn_amd64_mov_gp_res;
		} else {
			conv = new_bd_amd64_movs(dbgi, block, ARRAY_SIZE(in),
			                         in, insn_mode, AMD64_OP_REG, addr);
			pn_res = pn_amd64_movs_res;
		}
		res = new_r_Proj(conv, mode_gp, pn_res);
		reqs = reg_reqs;
	}

	arch_set_irn_register_reqs_in(conv, reqs);
	return res;
}

static ir_node *gen_Store(ir_node *node)
{
	dbg_info *dbgi = get_irn_dbg_info(node);
	ir_node *block = be_transform_nodes_block(node);
	ir_node *val   = get_Store_value(node);
	ir_mode *mode  = get_irn_mode(val);

	amd64_binop_addr_attr_t attr;
	memset(&attr, 0, sizeof(attr));
	amd64_addr_t *addr = &attr.base.addr;

	attr.base.base.op_mode = AMD64_OP_ADDR_REG;

	ir_node *in[4];
	int      arity = 0;

	int reg_input    = arity++;
	in[reg_input]    = be_transform_node(val);
	attr.u.reg_input = reg_input;

	ir_node *ptr     = get_Store_ptr(node);
	perform_address_matching(ptr, &arity, in, addr);

	bool const need_xmm = mode_is_float(mode);

	arch_register_req_t const **const reqs = (need_xmm ? xmm_am_reqs : gp_am_reqs)[arity];

	ir_node *mem     = get_Store_mem(node);
	ir_node *new_mem = be_transform_node(mem);
	in[arity++]      = new_mem;
	assert((size_t)arity <= ARRAY_SIZE(in));
	attr.base.insn_mode = get_insn_mode_from_mode(mode);

	ir_node *new_store;
	if (need_xmm) {
		new_store = new_bd_amd64_movs_store_xmm(dbgi, block, arity, in, &attr);
	} else {
		new_store = new_bd_amd64_mov_store(dbgi, block, arity, in, &attr);
	}

	arch_set_irn_register_reqs_in(new_store, reqs);
	set_irn_pinned(new_store, get_irn_pinned(node));
	return new_store;
}

ir_node *amd64_new_spill(ir_node *value, ir_node *after)
{
	ir_node  *block = get_block(after);
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *frame = get_irg_frame(irg);
	ir_node  *mem   = get_irg_no_mem(irg);
	ir_mode  *mode  = get_irn_mode(value);

	amd64_binop_addr_attr_t attr;
	memset(&attr, 0, sizeof(attr));
	attr.base.base.op_mode    = AMD64_OP_ADDR_REG;
	attr.base.insn_mode       = INSN_MODE_64;
	attr.base.needs_frame_ent = true;

	amd64_addr_t *addr = &attr.base.addr;
	addr->base_input   = 1;
	addr->index_input  = NO_INPUT;
	ir_node *in[]      = { value, frame, mem };

	const arch_register_req_t **reqs;
	ir_node *store;

	if (mode_is_float(mode) || mode == amd64_mode_xmm) {
		/* TODO: currently our stack alignment is messed up so we can't use
		 * movdqa for spilling... */
		attr.base.insn_mode = INSN_MODE_128;
		store = new_bd_amd64_movdqu_store(NULL, block, ARRAY_SIZE(in), in, &attr);
		reqs  = xmm_reg_mem_reqs;
	} else {
		store = new_bd_amd64_mov_store(NULL, block, ARRAY_SIZE(in), in, &attr);
		reqs  = reg_reg_mem_reqs;
	}

	arch_set_irn_register_reqs_in(store, reqs);
	arch_add_irn_flags(store, arch_irn_flag_spill);
	sched_add_after(after, store);
	return store;
}

ir_node *amd64_new_reload(ir_node *value, ir_node *spill, ir_node *before)
{
	ir_node  *block = get_block(before);
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *frame = get_irg_frame(irg);
	ir_mode  *mode  = get_irn_mode(value);

	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.base_input  = 0;
	addr.index_input = NO_INPUT;

	ir_node *in[] = { frame, spill };

	unsigned pn_res;
	ir_node *load;
	if (mode_is_float(mode) || mode == amd64_mode_xmm) {
		load = new_bd_amd64_movdqu(NULL, block, ARRAY_SIZE(in), in,
		                           AMD64_OP_ADDR, addr);
		pn_res = pn_amd64_movdqu_res;
	} else {
		load = new_bd_amd64_mov_gp(NULL, block, ARRAY_SIZE(in), in,
	                            INSN_MODE_64, AMD64_OP_ADDR, addr);
		pn_res = pn_amd64_mov_gp_res;
	}
	arch_set_irn_register_reqs_in(load, reg_mem_reqs);
	arch_add_irn_flags(load, arch_irn_flag_reload);
	sched_add_before(before, load);
	amd64_addr_attr_t *attr = get_amd64_addr_attr(load);
	attr->needs_frame_ent = true;
	ir_node *res = new_r_Proj(load, mode, pn_res);
	return res;
}

static ir_node *gen_Load(ir_node *node)
{

	dbg_info *dbgi = get_irn_dbg_info(node);
	ir_node *block = be_transform_nodes_block(node);
	ir_mode *mode  = get_Load_mode(node);

	ir_node *ptr = get_Load_ptr(node);

	int arity = 0;
	ir_node *in[3];
	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));

	perform_address_matching(ptr, &arity, in, &addr);

	arch_register_req_t const **const reqs = gp_am_reqs[arity];

	ir_node *mem     = get_Load_mem(node);
	ir_node *new_mem = be_transform_node(mem);
	in[arity++]      = new_mem;
	assert((size_t)arity <= ARRAY_SIZE(in));

	amd64_insn_mode_t insn_mode = get_insn_mode_from_mode(mode);
	ir_node  *new_load;

	if (mode_is_float(mode)) {
		new_load = new_bd_amd64_movs_xmm(dbgi, block, arity, in,
		                                 insn_mode, AMD64_OP_ADDR, addr);
	} else if (get_mode_size_bits(mode) < 64 && mode_is_signed(mode)) {
		new_load = new_bd_amd64_movs(dbgi, block, arity, in,
		                             insn_mode, AMD64_OP_ADDR, addr);
	} else {
		new_load = new_bd_amd64_mov_gp(dbgi, block, arity, in,
		                               insn_mode, AMD64_OP_ADDR, addr);
	}

	arch_set_irn_register_reqs_in(new_load, reqs);
	set_irn_pinned(new_load, get_irn_pinned(node));

	return new_load;
}

static ir_node *gen_Unknown(ir_node *node)
{
	/* for now, there should be more efficient ways to do this */
	ir_node *block = be_transform_nodes_block(node);

	if (mode_is_float(get_irn_mode(node))) {
		return new_bd_amd64_xorpd_0(NULL, block);
	} else {
		ir_node *res = new_bd_amd64_xor_0(NULL, block);
		return new_r_Proj(res, mode_gp, pn_amd64_xor_0_res);
	}
}

static const unsigned pn_amd64_mem = 2;

static ir_node *gen_Proj_Load(ir_node *node)
{
	ir_node  *load     = get_Proj_pred(node);
	ir_node  *new_load = be_transform_node(load);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	unsigned  pn       = get_Proj_num(node);

	/* loads might be part of source address mode matches, so we don't
	   transform the ProjMs yet (with the exception of loads whose result is
	   not used) */
	if (is_Load(load) && pn == pn_Load_M && get_irn_n_edges(load) > 1) {
		/* this is needed, because sometimes we have loops that are only
		   reachable through the ProjM */
		be_enqueue_preds(node);
		/* do it in 2 steps, to silence firm verifier */
		ir_node *res = new_rd_Proj(dbgi, load, mode_M, pn_Load_M);
		set_Proj_num(res, pn_amd64_mem);
		return res;
	}

	/* renumber the proj */
	switch (get_amd64_irn_opcode(new_load)) {
	case iro_amd64_movs_xmm:
		if (pn == pn_Load_res) {
			return new_rd_Proj(dbgi, new_load, amd64_mode_xmm,
			                   pn_amd64_movs_xmm_res);
		} else if (pn == pn_Load_M) {
			return new_rd_Proj(dbgi, new_load, mode_M, pn_amd64_movs_xmm_M);
		}
		break;
	case iro_amd64_movs:
	case iro_amd64_mov_gp:
		assert((unsigned)pn_amd64_movs_res == (unsigned)pn_amd64_mov_gp_res);
		assert((unsigned)pn_amd64_movs_M   == (unsigned)pn_amd64_mov_gp_M);
		/* handle all gp loads equal: they have the same proj numbers. */
		if (pn == pn_Load_res) {
			return new_rd_Proj(dbgi, new_load, mode_Lu, pn_amd64_movs_res);
		} else if (pn == pn_Load_M) {
			return new_rd_Proj(dbgi, new_load, mode_M, pn_amd64_movs_M);
		}
		break;
	case iro_amd64_add:
	case iro_amd64_and:
	case iro_amd64_cmp:
		assert(pn == pn_Load_M);
		return new_r_Proj(new_load, mode_M, pn_amd64_mem);
	default:
		panic("unsupported Proj from Load");
	}

    return be_duplicate_node(node);
}

static ir_node *gen_Proj_Store(ir_node *node)
{
	ir_node *pred = get_Proj_pred(node);
	unsigned pn   = get_Proj_num(node);
	if (pn == pn_Store_M) {
		return be_transform_node(pred);
	} else {
		panic("unsupported Proj from Store");
	}
}

static ir_node *gen_Alloc(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_node  *size      = get_Alloc_size(node);
	ir_node  *mem       = get_Alloc_mem(node);
	ir_node  *new_mem   = be_transform_node(mem);
	ir_node  *sp        = get_stack_pointer_for(node);

	const arch_register_req_t **reqs;
	amd64_binop_addr_attr_t attr;
	memset(&attr, 0, sizeof(attr));
	attr.base.insn_mode = INSN_MODE_64;

	ir_node *subsp;
	ir_node *in[3];
	unsigned arity = 0;
	in[arity++]    = sp;

	if (is_Const(size)) {
		ir_tarval *tv           = get_Const_tarval(size);
		long       sizel        = get_tarval_long(tv);

		attr.base.base.op_mode  = AMD64_OP_REG_IMM;
		attr.u.immediate.offset = sizel;
		reqs                    = rsp_mem_reqs;
	} else {
		attr.base.base.op_mode  = AMD64_OP_REG_REG;
		in[arity++]             = be_transform_node(size);
		reqs                    = rsp_reg_mem_reqs;
	}
	in[arity++] = new_mem;
	subsp = new_bd_amd64_sub_sp(dbgi, new_block, arity, in, &attr);

	arch_set_irn_register_reqs_in(subsp, reqs);
	arch_set_irn_register_out(subsp, pn_amd64_sub_sp_stack,
	                          &amd64_registers[REG_RSP]);

	ir_node *stack_proj = new_r_Proj(subsp, mode_gp, pn_amd64_sub_sp_stack);

	keep_alive(stack_proj);
	pmap_insert(node_to_stack, node, stack_proj);

	return subsp;
}

static ir_node *gen_Proj_Alloc(ir_node *node)
{
	ir_node *alloc     = get_Proj_pred(node);
	ir_node *new_alloc = be_transform_node(alloc);
	unsigned pn        = get_Proj_num(node);

	switch ((pn_Alloc)pn) {
	case pn_Alloc_M:   return new_r_Proj(new_alloc, mode_M, pn_amd64_sub_sp_M);
	case pn_Alloc_res: return new_r_Proj(new_alloc, mode_gp,
	                                     pn_amd64_sub_sp_addr);
	}
	panic("invalid Proj->Alloc");
}

static ir_node *gen_saturating_increment(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_nodes_block(node);
	ir_node  *param0    = get_Builtin_param(node, 0);
	ir_node  *operand   = be_transform_node(param0);
	ir_mode  *mode      = get_irn_mode(param0);

	amd64_binop_addr_attr_t inc_attr;
	memset(&inc_attr, 0, sizeof(inc_attr));
	inc_attr.base.base.op_mode  = AMD64_OP_REG_IMM;
	inc_attr.u.immediate.offset = 1;
	inc_attr.base.insn_mode     = get_insn_mode_from_mode(mode);
	ir_node  *inc_in[]          = { operand };

	ir_node  *inc = new_bd_amd64_add(dbgi, new_block, ARRAY_SIZE(inc_in),
	                                 inc_in, &inc_attr);

	arch_set_irn_register_reqs_in(inc, reg_reqs);
	arch_set_irn_register_req_out(inc, 0, &amd64_requirement_gp_same_0);

	ir_node *value  = new_rd_Proj(dbgi, inc, mode_gp, pn_amd64_add_res);
	ir_node *eflags = new_rd_Proj(dbgi, inc, mode_flags, pn_amd64_add_flags);

	amd64_binop_addr_attr_t sbb_attr;
	memset(&sbb_attr, 0, sizeof(sbb_attr));
	sbb_attr.base.base.op_mode  = AMD64_OP_REG_IMM;
	sbb_attr.u.immediate.offset = 0;
	sbb_attr.base.insn_mode     = get_insn_mode_from_mode(mode);
	ir_node *in[2]              = { value, eflags };

	ir_node *sbb = new_bd_amd64_sbb(dbgi, new_block, ARRAY_SIZE(in), in,
	                                &sbb_attr);

	arch_set_irn_register_reqs_in(sbb, reg_flags_reqs);
	arch_set_irn_register_req_out(sbb, 0, &amd64_requirement_gp_same_0);

	return sbb;
}

static ir_node *gen_Builtin(ir_node *node)
{
	ir_builtin_kind kind = get_Builtin_kind(node);

	switch (kind) {
	case ir_bk_saturating_increment:
		return gen_saturating_increment(node);
	default:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

static ir_node *gen_Proj_Builtin(ir_node *proj)
{
	ir_node   *node      = get_Proj_pred(proj);
	ir_node   *new_node  = be_transform_node(node);
	ir_builtin_kind kind = get_Builtin_kind(node);

	switch (kind) {
	case ir_bk_saturating_increment:
		return new_r_Proj(new_node, mode_gp, pn_amd64_sbb_res);
	default:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

static ir_node *gen_Bitcast(ir_node *node)
{
	ir_node  *op   = get_Bitcast_op(node);
	dbg_info *dbgi = get_irn_dbg_info(node);

	ir_mode *dst_mode = get_irn_mode(node);
	ir_mode *src_mode = get_irn_mode(op);

	bool dst_float = mode_is_float(dst_mode);
	bool src_float = mode_is_float(src_mode);

	ir_node *be_op    = be_transform_node(op);
	ir_node *be_block = get_nodes_block(be_op);

	amd64_addr_t no_addr = {
		.immediate = {
			.entity = NULL,
			.offset = 0,
		},
		.base_input  = NO_INPUT,
		.index_input = NO_INPUT,
		.mem_input   = NO_INPUT,
		.log_scale   = AMD64_SEGMENT_DEFAULT
	};

	if (src_float && !dst_float) {
		ir_node * new_node = new_bd_amd64_movd_xmm_gp(
			dbgi, be_block, be_op, INSN_MODE_64, AMD64_OP_REG, no_addr);
		return new_r_Proj(new_node, mode_gp, pn_amd64_movd_gp_xmm_res);
	} else if (!src_float && dst_float) {
		ir_node * new_node = new_bd_amd64_movd_gp_xmm(
			dbgi, be_block, be_op, INSN_MODE_64, AMD64_OP_REG, no_addr);
		return new_r_Proj(new_node, amd64_mode_xmm, pn_amd64_movd_xmm_gp_res);
	} else {
		panic("Unhandled bitcast modes: %+F to %+F\n", src_mode, dst_mode);
	}
}

static ir_node *gen_amd64_l_punpckldq(ir_node *node)
{
	ir_node *op0 = get_irn_n(node, n_amd64_l_punpckldq_arg0);
	ir_node *op1 = get_irn_n(node, n_amd64_l_punpckldq_arg1);
	return gen_binop_xmm(node, op0, op1, new_bd_amd64_punpckldq, match_am);
}

static ir_node *gen_amd64_l_subpd(ir_node *node)
{
	ir_node *op0 = get_irn_n(node, n_amd64_l_subpd_arg0);
	ir_node *op1 = get_irn_n(node, n_amd64_l_subpd_arg1);
	return gen_binop_xmm(node, op0, op1, new_bd_amd64_subpd, match_am);
}

static ir_node *gen_amd64_l_haddpd(ir_node *node)
{
	ir_node *op0 = get_irn_n(node, n_amd64_l_haddpd_arg0);
	ir_node *op1 = get_irn_n(node, n_amd64_l_haddpd_arg1);
	return gen_binop_xmm(node, op0, op1, new_bd_amd64_haddpd, match_am);
}

/* Boilerplate code for transformation: */

static void amd64_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_Add,               gen_Add);
	be_set_transform_function(op_Address,           gen_Address);
	be_set_transform_function(op_Alloc,             gen_Alloc);
	be_set_transform_function(op_And,               gen_And);
	be_set_transform_function(op_ASM,               gen_ASM);
	be_set_transform_function(op_Bitcast,           gen_Bitcast);
	be_set_transform_function(op_Builtin,           gen_Builtin);
	be_set_transform_function(op_Call,              gen_Call);
	be_set_transform_function(op_Cmp,               gen_Cmp);
	be_set_transform_function(op_Cond,              gen_Cond);
	be_set_transform_function(op_Const,             gen_Const);
	be_set_transform_function(op_Conv,              gen_Conv);
	be_set_transform_function(op_Div,               gen_Div);
	be_set_transform_function(op_Eor,               gen_Eor);
	be_set_transform_function(op_IJmp,              gen_IJmp);
	be_set_transform_function(op_Jmp,               gen_Jmp);
	be_set_transform_function(op_Load,              gen_Load);
	be_set_transform_function(op_Member,            gen_Member);
	be_set_transform_function(op_Minus,             gen_Minus);
	be_set_transform_function(op_Mod,               gen_Mod);
	be_set_transform_function(op_Mul,               gen_Mul);
	be_set_transform_function(op_Mulh,              gen_Mulh);
	be_set_transform_function(op_Not,               gen_Not);
	be_set_transform_function(op_Or,                gen_Or);
	be_set_transform_function(op_Phi,               gen_Phi);
	be_set_transform_function(op_Return,            gen_Return);
	be_set_transform_function(op_Shl,               gen_Shl);
	be_set_transform_function(op_Shr,               gen_Shr);
	be_set_transform_function(op_Shrs,              gen_Shrs);
	be_set_transform_function(op_Start,             gen_Start);
	be_set_transform_function(op_Store,             gen_Store);
	be_set_transform_function(op_Sub,               gen_Sub);
	be_set_transform_function(op_Switch,            gen_Switch);
	be_set_transform_function(op_Unknown,           gen_Unknown);
	be_set_transform_function(op_amd64_l_punpckldq, gen_amd64_l_punpckldq);
	be_set_transform_function(op_amd64_l_subpd,     gen_amd64_l_subpd);
	be_set_transform_function(op_amd64_l_haddpd,    gen_amd64_l_haddpd);

	be_set_transform_proj_function(op_Alloc,   gen_Proj_Alloc);
	be_set_transform_proj_function(op_ASM,     gen_Proj_ASM);
	be_set_transform_proj_function(op_Builtin, gen_Proj_Builtin);
	be_set_transform_proj_function(op_Call,    gen_Proj_Call);
	be_set_transform_proj_function(op_Cond,    be_duplicate_node);
	be_set_transform_proj_function(op_Div,     gen_Proj_Div);
	be_set_transform_proj_function(op_Load,    gen_Proj_Load);
	be_set_transform_proj_function(op_Mod,     gen_Proj_Mod);
	be_set_transform_proj_function(op_Proj,    gen_Proj_Proj);
	be_set_transform_proj_function(op_Start,   gen_Proj_Start);
	be_set_transform_proj_function(op_Store,   gen_Proj_Store);
	be_set_transform_proj_function(op_Switch,  be_duplicate_node);

	/* upper_bits_clean can't handle different register sizes, so
	 * arithmetic operations are problematic. Disable them. */
	be_set_upper_bits_clean_function(op_And, NULL);
	be_set_upper_bits_clean_function(op_Eor, NULL);
	be_set_upper_bits_clean_function(op_Mux, NULL);
	be_set_upper_bits_clean_function(op_Or, NULL);
	be_set_upper_bits_clean_function(op_Shr, NULL);
	be_set_upper_bits_clean_function(op_Shrs, NULL);
}

static ir_type *amd64_get_between_type(bool omit_fp)
{
	static ir_type *between_type         = NULL;
	static ir_type *omit_fp_between_type = NULL;

	if (between_type == NULL) {
		between_type = new_type_class(new_id_from_str("amd64_between_type"));
		/* between type contains return address + saved base pointer */
		set_type_size_bytes(between_type, 2*get_mode_size_bytes(mode_gp));

		omit_fp_between_type
			= new_type_class(new_id_from_str("amd64_between_type"));
		/* between type contains return address */
		set_type_size_bytes(omit_fp_between_type, get_mode_size_bytes(mode_gp));
	}

	return omit_fp ? omit_fp_between_type : between_type;
}

static void amd64_create_stacklayout(ir_graph *irg, const x86_cconv_t *cconv)
{
	ir_entity         *entity        = get_irg_entity(irg);
	ir_type           *function_type = get_entity_type(entity);
	be_stack_layout_t *layout        = be_get_irg_stack_layout(irg);

	/* construct argument type */
	ident   *const arg_id   = new_id_fmt("%s_arg_type", get_entity_ident(entity));
	ir_type *const arg_type = new_type_struct(arg_id);
	size_t   const n_params = get_method_n_params(function_type);
	for (size_t p = 0; p < n_params; ++p) {
		reg_or_stackslot_t *param = &cconv->parameters[p];
		if (param->type == NULL)
			continue;

		ident *const id = new_id_fmt("param_%u", (unsigned)p);
		param->entity = new_entity(arg_type, id, param->type);
		set_entity_offset(param->entity, param->offset);
	}

	memset(layout, 0, sizeof(*layout));
	layout->frame_type     = get_irg_frame_type(irg);
	layout->between_type   = amd64_get_between_type(cconv->omit_fp);
	layout->arg_type       = arg_type;
	layout->initial_offset = 0;
	layout->initial_bias   = 0;
	layout->sp_relative    = cconv->omit_fp;

	assert(N_FRAME_TYPES == 3);
	layout->order[0] = layout->frame_type;
	layout->order[1] = layout->between_type;
	layout->order[2] = layout->arg_type;
}

void amd64_transform_graph(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_TUPLES
	                         | IR_GRAPH_PROPERTY_NO_BADS
	                         | IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	start_mem.irn = NULL;
	memset(&start_val, 0, sizeof(start_val));

	amd64_register_transformers();
	mode_gp    = mode_Lu;
	mode_flags = mode_Iu;
	node_to_stack = pmap_create();

	stackorder = be_collect_stacknodes(irg);
	ir_entity *entity = get_irg_entity(irg);
	ir_type   *mtp    = get_entity_type(entity);
	current_cconv = amd64_decide_calling_convention(mtp, irg);
	amd64_create_stacklayout(irg, current_cconv);
	be_add_parameter_entity_stores(irg);

	heights = heights_new(irg);
	x86_calculate_non_address_mode_nodes(irg);
	be_transform_graph(irg, NULL);
	x86_free_non_address_mode_nodes();
	heights_free(heights);
	heights = NULL;

	be_free_stackorder(stackorder);
	x86_free_calling_convention(current_cconv);
	pmap_destroy(node_to_stack);
	node_to_stack = NULL;

	ir_type *frame_type = get_irg_frame_type(irg);
	if (get_type_state(frame_type) == layout_undefined)
		default_layout_compound_type(frame_type);

	place_code(irg);

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);
}

void amd64_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.transform");
}
