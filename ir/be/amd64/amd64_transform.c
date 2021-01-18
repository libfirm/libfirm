/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   code selection (transform FIRM into amd64 FIRM)
 */
#include "amd64_transform.h"

#include "../ia32/x86_address_mode.h"
#include "../ia32/x86_cconv.h"
#include "amd64_architecture.h"
#include "amd64_bearch_t.h"
#include "amd64_new_nodes.h"
#include "amd64_nodes_attr.h"
#include "amd64_varargs.h"
#include "beirg.h"
#include "benode.h"
#include "besched.h"
#include "betranshlp.h"
#include "debug.h"
#include "gen_amd64_regalloc_if.h"
#include "heights.h"
#include "ircons.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "irprog_t.h"
#include "panic.h"
#include "platform_t.h"
#include "tv_t.h"
#include "util.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static x86_cconv_t    *current_cconv = NULL;
static be_stack_env_t  stack_env;

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
	['e'] = { MATCH_IMM, GP, 0 },
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

arch_register_req_t const amd64_requirement_gp_same_0 = {
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

static const arch_register_req_t amd64_requirement_x87killed = {
	.cls         = &amd64_reg_classes[CLASS_amd64_x87],
	.width       = 1,
	.kills_value = true,
};

static const arch_register_req_t amd64_requirement_xmm_killed = {
	.cls         = &amd64_reg_classes[CLASS_amd64_xmm],
	.width       = 1,
	.kills_value = true,
};

static const arch_register_req_t *mem_reqs[] = {
	&arch_memory_requirement,
};

static const arch_register_req_t *reg_mem_reqs[] = {
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

static const arch_register_req_t *rsp_mem_reqs[] = {
	&amd64_single_reg_req_gp_rsp,
	&arch_memory_requirement,
};

arch_register_req_t const *rsp_reg_mem_reqs[] = {
	&amd64_single_reg_req_gp_rsp,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

static const arch_register_req_t *xmm_mem_reqs[] = {
	&amd64_class_reg_req_xmm,
	&arch_memory_requirement,
};

static const arch_register_req_t *x87K_mem_reqs[] = {
	&amd64_requirement_x87killed,
	&arch_memory_requirement,
};

static const arch_register_req_t *reg_reg_mem_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

arch_register_req_t const *xmm_reg_mem_reqs[] = {
	&amd64_class_reg_req_xmm,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

static const arch_register_req_t *x87_reg_mem_reqs[] = {
	&amd64_class_reg_req_x87,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

static const arch_register_req_t *x87K_reg_mem_reqs[] = {
	&amd64_requirement_x87killed,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

static const arch_register_req_t *reg_reg_reg_mem_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

static const arch_register_req_t *xmm_reg_reg_mem_reqs[] = {
	&amd64_class_reg_req_xmm,
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

static const arch_register_req_t *xmm_xmm_mem_reqs[] = {
	&amd64_requirement_xmm_killed,
	&amd64_class_reg_req_xmm,
	&arch_memory_requirement,
};

static const arch_register_req_t *xmm_xmm_reg_mem_reqs[] = {
	&amd64_requirement_xmm_killed,
	&amd64_class_reg_req_xmm,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

static const arch_register_req_t *xmm_xmm_reg_reg_mem_reqs[] = {
	&amd64_requirement_xmm_killed,
	&amd64_class_reg_req_xmm,
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

static const arch_register_req_t *x87K_reg_reg_mem_reqs[] = {
	&amd64_requirement_x87killed,
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

static const arch_register_req_t *reg_flags_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_flags,
};

arch_register_req_t const *amd64_reg_reg_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
};

static const arch_register_req_t *reg_rax_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_single_reg_req_gp_rax,
};

static const arch_register_req_t *reg_rax_rdx_mem_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_single_reg_req_gp_rax,
	&amd64_single_reg_req_gp_rdx,
	&arch_memory_requirement,
};

static const arch_register_req_t *rax_reg_mem_reqs[] = {
	&amd64_single_reg_req_gp_rax,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

static const arch_register_req_t *reg_rax_reg_mem_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_single_reg_req_gp_rax,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

static const arch_register_req_t *reg_reg_rax_reg_mem_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_class_reg_req_gp,
	&amd64_single_reg_req_gp_rax,
	&amd64_class_reg_req_gp,
	&arch_memory_requirement,
};

arch_register_req_t const *reg_reqs[] = {
	&amd64_class_reg_req_gp,
};

arch_register_req_t const *amd64_xmm_reqs[] = {
	&amd64_class_reg_req_xmm,
};

static const arch_register_req_t *reg_rcx_reqs[] = {
	&amd64_class_reg_req_gp,
	&amd64_single_reg_req_gp_rcx,
};

arch_register_req_t const *amd64_xmm_xmm_reqs[] = {
	&amd64_class_reg_req_xmm,
	&amd64_class_reg_req_xmm,
};

arch_register_req_t const *amd64_xmm_xmm_xmm_reqs[] = {
	&amd64_requirement_xmm_killed,
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

static arch_register_req_t const **const xmm_fma_am_reqs[] = {
		mem_reqs,
		xmm_mem_reqs,
		xmm_xmm_mem_reqs,
		xmm_xmm_reg_mem_reqs,
		xmm_xmm_reg_reg_mem_reqs,
};

static arch_register_req_t const **const x87K_am_reqs[] = {
	mem_reqs,
	x87K_mem_reqs,
	x87K_reg_mem_reqs,
	x87K_reg_reg_mem_reqs,
};

static inline bool mode_needs_gp_reg(ir_mode *mode)
{
	return get_mode_arithmetic(mode) == irma_twos_complement
	    && mode != amd64_mode_xmm; /* mode_xmm is 128bit int at the moment */
}

static ir_node *get_initial_sp(ir_graph *irg)
{
	return be_get_Start_proj(irg, &amd64_registers[REG_RSP]);
}

static ir_node *get_initial_fp(ir_graph *irg)
{
	return be_get_Start_proj(irg, &amd64_registers[REG_RBP]);
}

static ir_node *get_frame_base(ir_graph *irg)
{
	if (current_cconv->omit_fp) {
		return get_initial_sp(irg);
	} else {
		return get_initial_fp(irg);
	}
}

ir_entity *create_float_const_entity(ir_tarval *const tv)
{
	/* TODO: share code with ia32 backend */
	ir_entity *entity = pmap_get(ir_entity, amd64_constants, tv);
	if (entity != NULL)
		return entity;

	ir_mode *mode = get_tarval_mode(tv);
	ir_type *type = get_type_for_mode(mode);
	ir_type *glob = get_glob_type();

	entity = new_global_entity(glob, id_unique("C"), type,
	                           ir_visibility_private,
	                           IR_LINKAGE_CONSTANT | IR_LINKAGE_NO_IDENTITY);

	ir_initializer_t *initializer = create_initializer_tarval(tv);
	set_entity_initializer(entity, initializer);

	pmap_insert(amd64_constants, tv, entity);
	return entity;
}

void init_lconst_addr(x86_addr_t *addr, ir_entity *entity)
{
	assert(entity_has_definition(entity));
	assert(get_entity_linkage(entity) & IR_LINKAGE_CONSTANT);
	assert(get_entity_visibility(entity) == ir_visibility_private);
	x86_immediate_kind_t kind = ir_platform.pic_style != BE_PIC_NONE
	                          ? X86_IMM_PCREL : X86_IMM_ADDR;
	*addr = (x86_addr_t) {
		.immediate = {
			.entity = entity,
			.kind   = kind,
		},
		.variant = kind == X86_IMM_PCREL ? X86_ADDR_RIP : X86_ADDR_JUST_IMM,
	};
}

static ir_node *create_float_const(dbg_info *dbgi, ir_node *block,
                                   ir_tarval *tv)
{
	ir_graph  *irg     = get_irn_irg(block);
	ir_mode   *tv_mode = get_tarval_mode(tv);
	ir_entity *entity  = create_float_const_entity(tv);
	ir_node   *nomem   = get_irg_no_mem(irg);

	ir_node *in[] = { nomem };
	x86_addr_t addr;
	init_lconst_addr(&addr, entity);

	ir_node *load;
	unsigned pn_res;
	x86_insn_size_t size = x86_size_from_mode(tv_mode);
	if (size == X86_SIZE_128) {
		load = new_bd_amd64_movdqa(dbgi, block, ARRAY_SIZE(in), in, mem_reqs, AMD64_OP_ADDR, addr);
		pn_res = pn_amd64_movdqa_res;
	} else {
		load = new_bd_amd64_movs_xmm(dbgi, block, ARRAY_SIZE(in), in, mem_reqs, size, AMD64_OP_ADDR, addr);
		pn_res = pn_amd64_movs_xmm_res;
	}
	set_irn_pinned(load, false);
	arch_add_irn_flags(load, arch_irn_flag_rematerializable);

	return be_new_Proj(load, pn_res);
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

static ir_node *gen_x87_Const(dbg_info *const dbgi, ir_node *const block, ir_tarval *tv)
{
	/* TODO: avoid code duplication with ia32 backend */
	if (tarval_is_null(tv)) {
		return new_bd_amd64_fldz(dbgi, block);
	} else if (tarval_is_one(tv)) {
		return new_bd_amd64_fld1(dbgi, block);
	} else {
		ir_mode *mode = get_tarval_mode(tv);
		/* try to reduce the mode to produce smaller sized entities */
		ir_mode *const modes[] = { mode_F, mode_D, NULL };
		for (ir_mode *const *i = modes; *i != NULL; ++i) {
			ir_mode *const to = *i;
			if (tarval_ieee754_can_conv_lossless(tv, to)) {
				tv   = tarval_convert_to(tv, to);
				mode = to;
				break;
			}
		}
		ir_entity *entity = create_float_const_entity(tv);
		ir_graph  *irg    = get_irn_irg(block);
		ir_node   *nomem  = get_irg_no_mem(irg);
		ir_node   *in[1]  = { nomem };
		x86_addr_t addr;
		init_lconst_addr(&addr, entity);
		x86_insn_size_t size = x86_size_from_mode(mode);
		ir_node *load = new_bd_amd64_fld(dbgi, block, ARRAY_SIZE(in), in,
		                                 mem_reqs, size, AMD64_OP_ADDR, addr);
		set_irn_pinned(load, false);
		return be_new_Proj(load, pn_amd64_fld_res);
	}
}

static ir_node *make_const(dbg_info *const dbgi, ir_node *const block, uint64_t const val)
{
	x86_insn_size_t const imode = val > UINT32_MAX ? X86_SIZE_64 : X86_SIZE_32;
	amd64_imm64_t const imm = {
		.kind   = X86_IMM_VALUE,
		.offset = val,
	};
	return new_bd_amd64_mov_imm(dbgi, block, imode, &imm);
}

static ir_node *gen_Const(ir_node *const node)
{
	ir_node  *block = be_transform_nodes_block(node);
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);
	ir_tarval *tv = get_Const_tarval(node);

	if (!mode_needs_gp_reg(mode)) {
		if (mode == x86_mode_E) {
			return gen_x87_Const(dbgi, block, tv);
		} else if (tarval_is_null(tv)) {
			return new_bd_amd64_xorp_0(dbgi, block, X86_SIZE_64);
		}
		return create_float_const(dbgi, block, tv);
	}

	uint64_t const val = get_tarval_uint64(tv);
	return make_const(dbgi, block, val);
}

static ir_node *gen_Address(ir_node *const node)
{
	ir_node   *block  = be_transform_nodes_block(node);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_entity *entity = get_Address_entity(node);

	amd64_imm64_t const imm = {
		.kind   = X86_IMM_ADDR,
		.entity = entity,
	};
	return new_bd_amd64_mov_imm(dbgi, block, X86_SIZE_64, &imm);
}

static ir_node *create_picaddr_lea(dbg_info *const dbgi, ir_node *const block,
                                   x86_immediate_kind_t const kind,
                                   ir_entity *const entity)
{
	x86_addr_t addr = {
		.immediate = (x86_imm32_t) {
			.kind   = kind,
			.entity = entity,
		},
		.variant = X86_ADDR_RIP,
	};
	return new_bd_amd64_lea(dbgi, block, 0, NULL, NULL, X86_SIZE_64, addr);
}

static ir_node *gen_be_Relocation(ir_node *const node)
{
	dbg_info            *const dbgi   = get_irn_dbg_info(node);
	ir_node             *const block  = be_transform_nodes_block(node);
	ir_entity           *const entity = be_get_Relocation_entity(node);
	x86_immediate_kind_t const kind
		= (x86_immediate_kind_t)be_get_Relocation_kind(node);

	switch (kind) {
	case X86_IMM_ADDR: {
		amd64_imm64_t const imm = {
			.kind   = X86_IMM_ADDR,
			.entity = entity,
		};
		return new_bd_amd64_mov_imm(dbgi, block, X86_SIZE_64, &imm);
	}
	case X86_IMM_PCREL:
	case X86_IMM_GOTPCREL: /* can GOTPCREL happen here? */
		return create_picaddr_lea(dbgi, block, kind, entity);
	default:
		break;
	}
	panic("Unexpected relocation kind");
}

ir_node *amd64_new_IncSP(ir_node *block, ir_node *old_sp, int offset,
                         bool no_align)
{
	ir_node *const incsp = be_new_IncSP(block, old_sp, offset, no_align);
	arch_add_irn_flags(incsp, arch_irn_flag_modify_flags);
	return incsp;
}

typedef ir_node *(*construct_binop_func)(dbg_info *dbgi, ir_node *block, int arity, ir_node *const *in, arch_register_req_t const **in_reqs, amd64_binop_addr_attr_t const *attr_init);

typedef ir_node *(*construct_rax_binop_func)(dbg_info *dbgi, ir_node *block, int arity, ir_node *const *in, arch_register_req_t const **in_reqs, x86_insn_size_t size, amd64_op_mode_t op_mode, x86_addr_t addr);

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

static bool match_immediate_32(x86_imm32_t *const imm, ir_node const *const op, bool const must_match_ip_relative)
{
	assert(mode_needs_gp_reg(get_irn_mode(op)));
	assert(imm->offset == 0 && imm->entity == NULL
	       && imm->kind == X86_IMM_VALUE);

	ir_tarval *tv;
	ir_entity *entity;
	unsigned   reloc_kind;
	if (!be_match_immediate(op, &tv, &entity, &reloc_kind))
		return false;

	int32_t val;
	if (tv) {
		if (get_tarval_magnitude(tv) > 32)
			return false;
		val = get_tarval_uint64(tv);
	} else {
		val = 0;
	}

	if ((entity != NULL) != must_match_ip_relative)
		return false;

	x86_immediate_kind_t kind = (x86_immediate_kind_t)reloc_kind;
	if (entity != NULL) {
		if (kind == X86_IMM_VALUE || kind == X86_IMM_ADDR) {
			kind = X86_IMM_PCREL;
		} else if (kind != X86_IMM_PCREL && kind != X86_IMM_PLT)
			return false;
	}

	imm->entity = entity;
	imm->offset = val;
	imm->kind   = kind;
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
	if (load2 != NULL && (op1 == NULL || !input_depends_on_load(load2, op1))) {
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

static void perform_address_matching_flags(ir_node *const ptr, int *const arity, ir_node **const in, x86_addr_t *const addr, x86_create_am_flags_t const flags)
{
	x86_address_t maddr;
	x86_create_address_mode(&maddr, ptr, flags);

	x86_addr_variant_t variant = maddr.variant;
	assert(variant != X86_ADDR_INVALID);
	if (x86_addr_variant_has_base(variant)) {
		int base_input   = (*arity)++;
		addr->base_input = base_input;
		in[base_input]   = be_transform_node(maddr.base);
	} else {
		assert(maddr.base == NULL);
	}
	if (x86_addr_variant_has_index(variant)) {
		int index_input   = (*arity)++;
		addr->index_input = index_input;
		in[index_input]   = be_transform_node(maddr.index);
	} else {
		assert(maddr.index == NULL);
	}
	ir_entity *entity = maddr.imm.entity;
	if (entity != NULL && is_parameter_entity(entity) &&
		get_entity_parameter_number(entity) == IR_VA_START_PARAMETER_NUMBER)
		panic("request for invalid parameter (va_start parameter)");

	addr->segment   = X86_SEGMENT_DEFAULT;
	addr->immediate = maddr.imm;
	addr->log_scale = maddr.scale;
	addr->variant   = variant;
}

static void perform_address_matching(ir_node *const ptr, int *const arity, ir_node **const in, x86_addr_t *const addr)
{
	perform_address_matching_flags(ptr, arity, in, addr, x86_create_am_normal);
}

static void match_binop(amd64_args_t *args, ir_node *block,
                        ir_mode *mode, ir_node *op1, ir_node *op2,
                        match_flags_t flags)
{
	memset(args, 0, sizeof(*args));

	bool use_xmm       = mode_is_float(mode);
	bool use_immediate = flags & match_immediate;
	bool mode_neutral  = flags & match_mode_neutral;

	amd64_binop_addr_attr_t *const attr = &args->attr;
	attr->base.base.size = x86_size_from_mode(mode);

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
	bool     use_am
		= use_address_matching(mode, flags, block, op1, op2, &load, &op);

	x86_addr_t *addr = &attr->base.addr;
	if (use_immediate && match_immediate_32(&attr->u.immediate, op2, false)) {
		assert(!use_xmm && "Can't (yet) match binop with xmm immediate");
		/* fine, we found an immediate */
		int const reg_input = args->arity++;
		args->in[reg_input]     = be_transform_node(op1);
		addr->variant           = X86_ADDR_REG;
		addr->base_input        = reg_input;
		attr->base.base.op_mode = AMD64_OP_REG_IMM;
		args->reqs              = reg_reqs;
	} else if (use_am) {
		int const reg_input = args->arity++;
		attr->u.reg_input   = reg_input;
		args->in[reg_input] = be_transform_node(op);

		ir_node *ptr = get_Load_ptr(load);
		perform_address_matching(ptr, &(args->arity), args->in, addr);

		args->reqs = (use_xmm ? xmm_am_reqs : gp_am_reqs)[args->arity];

		ir_node *new_mem    = be_transform_node(get_Load_mem(load));
		int mem_input       = args->arity++;
		args->in[mem_input] = new_mem;
		addr->mem_input     = mem_input;

		args->mem_proj      = get_Proj_for_pn(load, pn_Load_M);
		attr->base.base.op_mode = AMD64_OP_REG_ADDR;
	} else {
		/* simply transform the arguments */
		int const reg_input0 = args->arity++;
		int const reg_input1 = args->arity++;
		args->in[reg_input0]    = be_transform_node(op1);
		args->in[reg_input1]    = be_transform_node(op2);
		addr->variant           = X86_ADDR_REG;
		addr->base_input        = reg_input0;
		attr->u.reg_input       = reg_input1;
		attr->base.base.op_mode = AMD64_OP_REG_REG;

		args->reqs = use_xmm ? amd64_xmm_xmm_reqs : amd64_reg_reg_reqs;
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
	ir_node  *const new_node  = func(dbgi, new_block, args.arity, args.in, args.reqs, &args.attr);

	fix_node_mem_proj(new_node, args.mem_proj);

	if (mode_is_float(mode)) {
		arch_set_irn_register_req_out(new_node, 0,
		                              &amd64_requirement_xmm_same_0);
	} else {
		arch_set_irn_register_req_out(new_node, 0,
		                              &amd64_requirement_gp_same_0);
	}
	return be_new_Proj(new_node, pn_res);
}

static ir_node *gen_binop_rax(ir_node *node, ir_node *op0, ir_node *op1,
                              construct_rax_binop_func make_node,
                              match_flags_t flags)
{
	bool mode_neutral = flags & match_mode_neutral;
	assert(! (flags & match_immediate));

	ir_mode        *mode = get_irn_mode(op0);
	x86_insn_size_t size = x86_size_from_mode(mode);

	/* TODO: legalize phase */
	if (mode_neutral) {
		op0 = be_skip_downconv(op0, true);
		op1 = be_skip_downconv(op1, true);
	} else {
		/* TODO: extend inputs? */
		(void)needs_extension;
	}

	ir_node *block = get_nodes_block(node);
	ir_node *load;
	ir_node *op;
	bool     use_am
		= use_address_matching(mode, flags, block, op0, op1, &load, &op);

	ir_node *in[4];
	int      arity = 0;
	amd64_op_mode_t op_mode;
	x86_addr_t addr;
	memset(&addr, 0, sizeof(addr));

	const arch_register_req_t **reqs;
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
		op_mode                 = AMD64_OP_ADDR;
	} else {
		/* simply transform the arguments */
		int const input0 = arity++;
		int const input1 = arity++;
		in[input0]      = be_transform_node(op0);
		in[input1]      = be_transform_node(op1);
		reqs            = reg_rax_reqs;
		op_mode         = AMD64_OP_REG;
		addr.variant    = X86_ADDR_REG;
		addr.base_input = input0;
	}

	assert((size_t)arity <= ARRAY_SIZE(in));
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_node(block);
	ir_node  *const new_node  = make_node(dbgi, new_block, arity, in, reqs, size, op_mode, addr);
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
	amd64_binop_addr_attr_t *const attr = &args.attr;
	attr->base.base.size = X86_SIZE_64;

	ir_node *load;
	ir_node *op;
	bool use_am = use_address_matching(mode, flags, block, op0, op1, &load,
	                                   &op);

	x86_addr_t *addr = &attr->base.addr;
	if (use_am) {
		int reg_input = args.arity++;
		attr->u.reg_input  = reg_input;
		args.in[reg_input] = be_transform_node(op);

		ir_node      *ptr  = get_Load_ptr(load);
		perform_address_matching(ptr, &args.arity, args.in, addr);

		args.reqs = xmm_am_reqs[args.arity];

		ir_node *new_mem   = be_transform_node(get_Load_mem(load));
		int mem_input      = args.arity++;
		args.in[mem_input] = new_mem;
		addr->mem_input    = mem_input;

		args.mem_proj      = get_Proj_for_pn(load, pn_Load_M);
		attr->base.base.op_mode = AMD64_OP_REG_ADDR;
	} else {
		int const input0 = args.arity++;
		int const input1 = args.arity++;
		args.in[input0]         = be_transform_node(op0);
		args.in[input1]         = be_transform_node(op1);
		addr->base_input        = input0;
		addr->variant           = X86_ADDR_REG;
		attr->u.reg_input       = input1;
		attr->base.base.op_mode = AMD64_OP_REG_REG;
		args.reqs               = amd64_xmm_xmm_reqs;
	}

	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_node(block);
	ir_node  *const new_node  = make_node(dbgi, new_block, args.arity, args.in, args.reqs, &args.attr);

	fix_node_mem_proj(new_node, args.mem_proj);

	arch_set_irn_register_req_out(new_node, 0, &amd64_requirement_xmm_same_0);
	return be_new_Proj(new_node, pn_amd64_subs_res);
}

typedef ir_node *(*construct_x87_binop_func)(
		dbg_info *dbgi, ir_node *block, ir_node *op0, ir_node *op1);

static ir_node *gen_binop_x87(ir_node *const node, ir_node *const op0,
                              ir_node *const op1, construct_x87_binop_func cons)
{
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_node(get_nodes_block(node));
	ir_node  *const new_op0   = be_transform_node(op0);
	ir_node  *const new_op1   = be_transform_node(op1);
	ir_node  *const res       = cons(dbgi, new_block, new_op0, new_op1);
	/* TODO: address modes */
	return res;
}

typedef ir_node *(*construct_shift_func)(dbg_info *dbgi, ir_node *block, int arity, ir_node *const *in, arch_register_req_t const **in_reqs, amd64_shift_attr_t const *attr_init);

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
		mode = get_mode_size_bits(mode) > 32 ? mode_Lu : mode_Iu;
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
	attr.base.size = x86_size_from_mode(mode);

	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_nodes_block(node);
	ir_node  *const new_node  = func(dbgi, new_block, arity, in, reqs, &attr);
	arch_set_irn_register_req_out(new_node, 0, out_req0);
	return be_new_Proj(new_node, pn_res);
}

static ir_node *create_add_lea(dbg_info *dbgi, ir_node *new_block,
                               x86_insn_size_t size, ir_node *op1, ir_node *op2)
{
	ir_node *in[] = { op1, op2 };
	x86_addr_t addr = {
		.variant     = X86_ADDR_BASE_INDEX,
		.base_input  = 0,
		.index_input = 1,
	};
	return new_bd_amd64_lea(dbgi, new_block, ARRAY_SIZE(in), in,
	                        amd64_reg_reg_reqs, size, addr);
}

static x86_insn_size_t get_size_32_64_from_mode(ir_mode *const mode)
{
	return get_mode_size_bits(mode) <= 32 ? X86_SIZE_32 : X86_SIZE_64;
}

static ir_node *gen_fma(ir_node *const add, ir_node *const op1, ir_node *const op2)
{
	if (!amd64_cg_config.use_scalar_fma3)
		return NULL;
	ir_mode *const add_mode = get_irn_mode(add);
	if (get_mode_size_bits(add_mode) != 64 && get_mode_size_bits(add_mode) != 32)
		return NULL;
	ir_node *mul, *add_op;
	if (is_Mul(op1)) {
		mul = op1;
		add_op = op2;
	} else if (is_Mul(op2)) {
		mul = op2;
		add_op = op1;
	} else {
		return NULL;
	}
	if (get_irn_mode(mul) != add_mode)
		return NULL;
	if (get_irn_n_edges(mul) != 1)
		return NULL;

	ir_node *const block     = get_nodes_block(add);
	ir_node *const mul_op1   = get_Mul_left(mul);
	ir_node *const mul_op2   = get_Mul_right(mul);
	ir_node *load, *reg_op, *source1, *source2;
	bool use_am;
	ir_node *(*fma_variant)(dbg_info *, ir_node *, const int, ir_node *const *, const arch_register_req_t **,
	                        x86_insn_size_t, amd64_op_mode_t, x86_addr_t);
	//try if Add operand, left Mul operand or right Mul operand can be used as AM input
	if ((use_am = use_address_matching(add_mode, match_am, block, mul_op1, add_op, &load, &reg_op)
	              && (!input_depends_on_load(load, mul_op2)))) {
		source1 = mul_op2;
		source2 = reg_op;
		fma_variant = &new_bd_amd64_vfmadd213s;
	} else if ((use_am = use_address_matching(add_mode, match_am, block, add_op, mul_op1, &load, &reg_op)
	                     && (!input_depends_on_load(load, mul_op2)))) {
		source1 = mul_op2;
		source2 = reg_op;
		fma_variant = &new_bd_amd64_vfmadd132s;
	} else if ((use_am = use_address_matching(add_mode, match_am, block, add_op, mul_op2, &load, &reg_op)
	                     && (!input_depends_on_load(load, mul_op1)))) {
		source1 = reg_op;
		source2 = mul_op1;
		fma_variant = &new_bd_amd64_vfmadd231s;
	}
	int arity = 0;
	amd64_op_mode_t op_mode;
	ir_node  *mem_proj = NULL;
	ir_node  *in[5];
	const arch_register_req_t **reqs;
	x86_addr_t addr = {
			.base_input = 0,
			.variant    = X86_ADDR_REG,
	};

	if (use_am) {
		int reg_input = arity++;
		in[reg_input] = be_transform_node(source1);
		reg_input = arity++;
		in[reg_input] = be_transform_node(source2);

		ir_node      *ptr  = get_Load_ptr(load);
		perform_address_matching(ptr, &arity, in, &addr);

		reqs = xmm_fma_am_reqs[arity];

		ir_node *new_mem = be_transform_node(get_Load_mem(load));
		int mem_input    = arity++;
		in[mem_input]    = new_mem;
		addr.mem_input   = mem_input;
		mem_proj         = get_Proj_for_pn(load, pn_Load_M);
		op_mode          = AMD64_OP_REG_REG_ADDR;
	} else {
		int const input0 = arity++;
		int const input1 = arity++;
		int const input2 = arity++;
		in[input0]  = be_transform_node(add_op);
		in[input1]  = be_transform_node(mul_op1);
		in[input2]  = be_transform_node(mul_op2);
		op_mode     = AMD64_OP_REG_REG_REG;
		reqs        = amd64_xmm_xmm_xmm_reqs;
		fma_variant = &new_bd_amd64_vfmadd231s;
	}
	x86_insn_size_t size = x86_size_from_mode(add_mode);
	dbg_info *const dbgi      = get_irn_dbg_info(add);
	ir_node  *const new_block = be_transform_node(block);
	ir_node  *const new_node  = fma_variant(dbgi, new_block, arity, in, reqs, size, op_mode, addr);

	fix_node_mem_proj(new_node, mem_proj);
	arch_set_irn_register_req_out(new_node, 0, &amd64_requirement_xmm_same_0);
	return be_new_Proj(new_node, pn_amd64_vfmadd132s_res);
}

static ir_node *gen_Add(ir_node *const node)
{
	ir_node *const op1   = get_Add_left(node);
	ir_node *const op2   = get_Add_right(node);
	ir_mode *const mode  = get_irn_mode(node);
	ir_node *const block = get_nodes_block(node);

	if (mode_is_float(mode)) {
		if (mode == x86_mode_E)
			return gen_binop_x87(node, op1, op2, new_bd_amd64_fadd);
		ir_node *const fma = gen_fma(node, op1, op2);
		if (fma)
			return fma;
		return gen_binop_am(node, op1, op2, new_bd_amd64_adds,
		                    pn_amd64_adds_res, match_commutative | match_am);
	}

	match_flags_t flags = match_immediate | match_am | match_mode_neutral
	                    | match_commutative;
	ir_node *load;
	ir_node *op;
	bool     use_am
		= use_address_matching(mode, flags, block, op1, op2, &load, &op);

	ir_node *res;
	if (use_am)
		res = gen_binop_am(node, op1, op2, new_bd_amd64_add, pn_amd64_add_res,
		                   flags);
	else {
		int        arity = 0;
		ir_node   *in[2];
		x86_addr_t addr;
		perform_address_matching_flags(node, &arity, in, &addr, x86_create_am_force);

		dbg_info       *const dbgi      = get_irn_dbg_info(node);
		ir_node        *const new_block = be_transform_node(block);
		x86_insn_size_t const size      = get_size_32_64_from_mode(mode);
		res = new_bd_amd64_lea(dbgi, new_block, arity, in, amd64_reg_reg_reqs, size, addr);
	}

	x86_mark_non_am(node);
	return res;
}

static ir_node *gen_Sub(ir_node *const node)
{
	ir_node *const op1  = get_Sub_left(node);
	ir_node *const op2  = get_Sub_right(node);
	ir_mode *const mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		if (mode == x86_mode_E)
			return gen_binop_x87(node, op1, op2, new_bd_amd64_fsub);
		return gen_binop_am(node, op1, op2, new_bd_amd64_subs,
		                    pn_amd64_subs_res, match_am);
	} else {
		/* TODO: do not match AM yet until we have a sub->neg+add rule
		 * in amd64_finish */
		return gen_binop_am(node, op1, op2, new_bd_amd64_sub, pn_amd64_sub_res,
		                    match_immediate);
	}
}

typedef ir_node* (*create_mov_func)(dbg_info *dbgi, ir_node *block, int arity,
	ir_node *const *in, arch_register_req_t const **in_reqs,
	x86_insn_size_t size, amd64_op_mode_t op_mode, x86_addr_t addr);

static ir_node *match_mov(dbg_info *dbgi, ir_node *block, ir_node *value, x86_insn_size_t size, create_mov_func create_mov, unsigned pn_res);

static ir_node *gen_And(ir_node *const node)
{
	ir_node *const op1 = get_And_left(node);
	ir_node *const op2 = get_And_right(node);

	/* Is it a zero extension? */
	if (is_Const(op2)) {
		x86_insn_size_t size;
		ir_tarval *const tv = get_Const_tarval(op2);
		uint64_t   const v  = get_tarval_uint64(tv);
		if (v == 0xFF) {
			size = X86_SIZE_8;
			goto movzx;
		} else if (v == 0xFFFF) {
			size = X86_SIZE_16;
			goto movzx;
		} else if (v == 0xFFFFFFFF) {
			size = X86_SIZE_32;
movzx:;
			dbg_info *const dbgi  = get_irn_dbg_info(node);
			ir_node  *const block = get_nodes_block(node);
			return match_mov(dbgi, block, op1, size, &new_bd_amd64_mov_gp, pn_amd64_mov_gp_res);
		}
	}

	return gen_binop_am(node, op1, op2, new_bd_amd64_and, pn_amd64_and_res,
	                    match_immediate | match_am | match_mode_neutral
	                    | match_commutative);
}

static ir_node *gen_Eor(ir_node *const node)
{
	ir_node *const op1 = get_Eor_left(node);
	ir_node *const op2 = get_Eor_right(node);
	return gen_binop_am(node, op1, op2, new_bd_amd64_xor, pn_amd64_xor_res,
	                    match_immediate | match_am | match_mode_neutral
	                    | match_commutative);
}

static ir_node *gen_Or(ir_node *const node)
{
	ir_node *const op1 = get_Or_left(node);
	ir_node *const op2 = get_Or_right(node);
	return gen_binop_am(node, op1, op2, new_bd_amd64_or, pn_amd64_or_res,
	                    match_immediate | match_am | match_mode_neutral
	                    | match_commutative);
}

static ir_node *gen_Mul(ir_node *const node)
{
	ir_node *const op1  = get_Mul_left(node);
	ir_node *const op2  = get_Mul_right(node);
	ir_mode *const mode = get_irn_mode(node);

	if (get_mode_size_bits(mode) < 16) {
		/* imulb only supports rax - reg form */
		ir_node *new_node
			= gen_binop_rax(node, op1, op2, new_bd_amd64_imul_1op,
			                match_mode_neutral | match_commutative);
		return be_new_Proj(new_node, pn_amd64_imul_1op_res_low);
	} else if (mode_is_float(mode)) {
		if (mode == x86_mode_E)
			return gen_binop_x87(node, op1, op2, new_bd_amd64_fmul);
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
	ir_node *const op1  = get_Mulh_left(node);
	ir_node *const op2  = get_Mulh_right(node);
	ir_mode *const mode = get_irn_mode(op1);

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
	return be_new_Proj(new_node, pn_res);
}

static ir_node *gen_Shl(ir_node *const node)
{
	ir_node *const op1 = get_Shl_left(node);
	ir_node *const op2 = get_Shl_right(node);

	/* shl $1, x -> lea (x,x)
	 * lea provides a copy for free. */
	if (is_irn_one(op2)) {
		dbg_info       *const dbgi    = get_irn_dbg_info(node);
		ir_node        *const block   = be_transform_nodes_block(node);
		ir_mode        *const mode    = get_irn_mode(node);
		x86_insn_size_t const size    = get_size_32_64_from_mode(mode);
		ir_node        *const new_op1 = be_transform_node(op1);
		return create_add_lea(dbgi, block, size, new_op1, new_op1);
	}

	return gen_shift_binop(node, op1, op2, new_bd_amd64_shl, pn_amd64_shl_res,
	                       match_immediate | match_mode_neutral);
}

static ir_node *gen_Shr(ir_node *const node)
{
	ir_node *const op1 = get_Shr_left(node);
	ir_node *const op2 = get_Shr_right(node);
	return gen_shift_binop(node, op1, op2, new_bd_amd64_shr, pn_amd64_shr_res,
	                       match_immediate);
}

static ir_node *gen_Shrs(ir_node *const node)
{
	ir_node *const op1 = get_Shrs_left(node);
	ir_node *const op2 = get_Shrs_right(node);
	return gen_shift_binop(node, op1, op2, new_bd_amd64_sar, pn_amd64_sar_res,
	                       match_immediate);
}

static ir_node *create_div(ir_node *const node, ir_mode *const mode,
                           ir_node *const op1, ir_node *const op2,
                           ir_node *const mem)
{
	dbg_info       *const dbgi      = get_irn_dbg_info(node);
	ir_node        *const new_block = be_transform_nodes_block(node);
	x86_insn_size_t const size      = get_size_32_64_from_mode(mode);

	ir_node *const new_op1 = be_transform_node(op1);
	ir_node *const new_op2 = be_transform_node(op2);
	ir_node *const new_mem = be_transform_node(mem);
	ir_node *upper_value;
	ir_node *(*constructor)(dbg_info*, ir_node*, int, ir_node *const*,
	                        arch_register_req_t const**, x86_insn_size_t);
	/* We have to extend the value to a 2nd register */
	if (mode_is_signed(mode)) {
		if (size == X86_SIZE_64) {
			upper_value = new_bd_amd64_cqto(dbgi, new_block, new_op1);
		} else {
			upper_value = new_bd_amd64_cltd(dbgi, new_block, new_op1);
		}
		constructor = new_bd_amd64_idiv;
	} else {
		upper_value = make_const(dbgi, new_block, 0);
		constructor = new_bd_amd64_div;
	}

	ir_node *const in[] = { new_op2, new_op1, upper_value, new_mem };
	return constructor(dbgi, new_block, ARRAY_SIZE(in), in,
	                   reg_rax_rdx_mem_reqs, size);
}

static ir_node *create_sse_div(ir_node *const node, ir_mode *const mode,
                               ir_node *const op1, ir_node *const op2)
{
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const block     = get_nodes_block(node);
	ir_node  *const new_block = be_transform_node(block);

	amd64_args_t args;
	match_binop(&args, block, mode, op1, op2, match_am);

	ir_node *const new_node = new_bd_amd64_divs(dbgi, new_block, args.arity, args.in, args.reqs, &args.attr);

	fix_node_mem_proj(new_node, args.mem_proj);

	if (args.reqs == amd64_xmm_xmm_reqs) {
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

	if (mode_is_float(mode)) {
		if (mode == x86_mode_E)
			return gen_binop_x87(node, op1, op2, new_bd_amd64_fdiv);
		return create_sse_div(node, mode, op1, op2);
	} else {
		return create_div(node, mode, op1, op2, mem);
	}
}

static ir_node *gen_Proj_Div(ir_node *const node)
{
	ir_node *const pred     = get_Proj_pred(node);
	ir_node *const new_pred = be_transform_node(pred);
	pn_Div   const pn       = get_Proj_num(node);

	if (is_amd64_fdiv(new_pred)) {
		switch (pn) {
		case pn_Div_M:
			/* float divs don't trap, skip memory */
			return be_transform_node(get_Div_mem(pred));
		case pn_Div_res:
			return new_pred;
		case pn_Div_X_except:
		case pn_Div_X_regular:
			panic("amd64 exception NIY");
		}
		panic("invalid Div Proj");
	} else if (is_amd64_divs(new_pred)) {
		switch (pn) {
		case pn_Div_M:
			/* float divs don't trap, skip memory */
			return be_transform_node(get_Div_mem(pred));
		case pn_Div_res:
			return be_new_Proj(new_pred, pn_amd64_divs_res);
		case pn_Div_X_except:
		case pn_Div_X_regular:
			panic("amd64 exception NIY");
		}
		panic("invalid Div Proj");
	} else {
		assert(is_amd64_div(new_pred) || is_amd64_idiv(new_pred));
		assert((unsigned)pn_amd64_div_M == (unsigned)pn_amd64_idiv_M);
		assert((unsigned)pn_amd64_div_res_mod
		    == (unsigned)pn_amd64_idiv_res_mod);
		switch (pn) {
		case pn_Div_M:
			return be_new_Proj(new_pred, pn_amd64_div_M);
		case pn_Div_res:
			return be_new_Proj(new_pred, pn_amd64_div_res_div);
		case pn_Div_X_except:
		case pn_Div_X_regular:
			panic("amd64 exception NIY");
		}
		panic("invalid Div Proj");
	}
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
	ir_node *const pred     = get_Proj_pred(node);
	ir_node *const new_pred = be_transform_node(pred);
	unsigned const pn       = get_Proj_num(node);

	assert((unsigned)pn_amd64_div_M == (unsigned)pn_amd64_idiv_M);
	assert((unsigned)pn_amd64_div_res_mod == (unsigned)pn_amd64_idiv_res_mod);
	switch ((pn_Mod)pn) {
	case pn_Mod_M:
		return be_new_Proj(new_pred, pn_amd64_div_M);
	case pn_Mod_res:
		return be_new_Proj(new_pred, pn_amd64_div_res_mod);
	case pn_Mod_X_except:
	case pn_Mod_X_regular:
		panic("amd64 exception NIY");
	}
	panic("invalid Mod Proj");
}

typedef ir_node* (*unop_constructor)(dbg_info*,ir_node*block,ir_node*op,x86_insn_size_t size);

static ir_node *gen_unop(ir_node *const node, int op_pos, unop_constructor gen,
                         unsigned pn_res)
{
	dbg_info *const dbgi   = get_irn_dbg_info(node);
	ir_node  *const block  = be_transform_nodes_block(node);
	ir_node  *const op     = get_irn_n(node, op_pos);
	ir_node  *const new_op = be_transform_node(op);
	ir_mode  *const mode   = get_irn_mode(node);

	x86_insn_size_t const size     = get_size_32_64_from_mode(mode);
	ir_node        *const new_node = gen(dbgi, block, new_op, size);
	return be_new_Proj(new_node, pn_res);
}

typedef ir_node* (*unop_out_constructor)(dbg_info*, ir_node *block, const int arity, ir_node *const *const in,
                                         arch_register_req_t const ** const reqs,
                                         x86_insn_size_t size, amd64_op_mode_t opmode,
                                         x86_addr_t addr);

/**
 * Generates a back-end node with dedicated output register for a unary
 * builtin node in the middle-end. Takes care of address modes and memory edges.
 *
 * @param node    The unlowered firm IR middle-end node.
 * @param op_pos  The index of the input operand in the middle-end node.
 * @param gen     The constructor function that actually creates the node.
 * @param pn_res  The index where the node for the builtin stores its result.
 */
static ir_node *gen_unop_out(ir_node *const node, int op_pos,
                             unop_out_constructor gen, unsigned pn_res)
{
	dbg_info        *const dbgi      = get_irn_dbg_info(node);
	ir_node         *const block     = get_nodes_block(node);
	ir_node         *const op        = get_irn_n(node, op_pos);
	ir_mode         *const op_mode   = get_irn_mode(op);
	ir_node         *const new_block = be_transform_nodes_block(node);
	x86_insn_size_t  const size      = x86_size_from_mode(op_mode);
	ir_node         *const load      = source_am_possible(block, op);
	ir_node         *      new_node;

	if (load != NULL) {
		x86_addr_t addr;
		ir_node   *in[5];
		int        arity = 0;

		perform_address_matching(get_Load_ptr(load), &arity, in, &addr);
		int npred         = arity;
		ir_node *load_mem = get_Load_mem(load);
		ir_node *new_mem  = be_transform_node(load_mem);
		int mem_input     = npred++;
		in[mem_input]     = new_mem;
		addr.mem_input    = mem_input;
		new_node = gen(dbgi, new_block, npred, in, gp_am_reqs[arity], size, AMD64_OP_ADDR, addr);
		ir_node *mem_proj = get_Proj_for_pn(load, pn_Load_M);
		// Do we really need to call fix_node_mem_proj here?
		// Why not just be_set_transformed_node(load, new_node);?
		fix_node_mem_proj(new_node, mem_proj);
	} else {
		x86_addr_t addr = {
			.base_input = 0,
			.variant    = X86_ADDR_REG,
		};
		ir_node *in[] = { be_transform_node(op) };
		new_node = gen(dbgi, new_block, ARRAY_SIZE(in), in, reg_reqs, size, AMD64_OP_REG, addr);
	}

	return be_new_Proj(new_node, pn_res);
}

/** Create a floating point negation by switching the sign bit using a xor. */
static ir_node *gen_float_neg(ir_node *const node)
{
	dbg_info  *const dbgi      = get_irn_dbg_info(node);
	ir_node   *const new_block = be_transform_nodes_block(node);
	ir_node   *const op        = get_irn_n(node, n_Minus_op);
	ir_node   *const new_op    = be_transform_node(op);
	ir_mode   *const mode      = get_irn_mode(node);
	ir_tarval *const tv        = create_sign_tv(mode);
	ir_node   *const load      = create_float_const(dbgi, new_block, tv);
	ir_node   *const in[]      = { new_op, load };

	amd64_binop_addr_attr_t attr = {
		.base = {
			.base = {
				.op_mode = AMD64_OP_REG_REG,
				.size    = x86_size_from_mode(mode),
			},
			.addr = {
				.base_input = 0,
				.variant    = X86_ADDR_REG,
			},
		},
	};
	ir_node *const xor
		= new_bd_amd64_xorp(dbgi, new_block, ARRAY_SIZE(in), in,
		                    amd64_xmm_xmm_reqs, &attr);
	arch_set_irn_register_req_out(xor, 0, &amd64_requirement_xmm_same_0);

	return be_new_Proj(xor, pn_amd64_xorp_res);
}

static ir_node *gen_Minus(ir_node *const node)
{
	ir_mode *mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		if (mode == x86_mode_E) {
			dbg_info *dbgi   = get_irn_dbg_info(node);
			ir_node  *block  = be_transform_node(get_nodes_block(node));
			ir_node  *op     = get_Minus_op(node);
			ir_node  *new_op = be_transform_node(op);
			return new_bd_amd64_fchs(dbgi, block, new_op);
		}
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
	ir_node   *const new_block = be_transform_nodes_block(node);
	dbg_info  *const dbgi      = get_irn_dbg_info(node);
	ir_node   *const ptr       = get_Member_ptr(node);
	ir_graph  *const irg       = get_irn_irg(node);
	ir_node   *const base      = get_frame_base(irg);
	ir_entity *const entity    = get_Member_entity(node);
	if (!is_Proj(ptr) || !is_Start(get_Proj_pred(ptr)))
		panic("Sel not lowered");
	if (is_parameter_entity(entity) &&
	    get_entity_parameter_number(entity) == IR_VA_START_PARAMETER_NUMBER)
		panic("request for invalid parameter (va_start parameter)");

	x86_addr_t addr = {
		.immediate = {
			.entity = entity,
			.kind   = X86_IMM_FRAMEENT,
		},
		.variant    = X86_ADDR_BASE,
		.base_input = 0,
	};
	ir_node *in[] = { base };
	return new_bd_amd64_lea(dbgi, new_block, ARRAY_SIZE(in), in, reg_reqs, X86_SIZE_64, addr);
}

static ir_node *gen_IJmp(ir_node *const node)
{
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const block     = get_nodes_block(node);
	ir_node  *const new_block = be_transform_node(block);
	ir_node  *const op        = get_IJmp_target(node);

	int arity = 0;
	ir_node *in[3];
	x86_addr_t addr;
	memset(&addr, 0, sizeof(addr));

	const arch_register_req_t **reqs;
	amd64_op_mode_t             op_mode;
	ir_node                    *mem_proj = NULL;
	if (match_immediate_32(&addr.immediate, op, true)) {
		op_mode = AMD64_OP_IMM32;
		reqs    = NULL;
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

			op_mode = AMD64_OP_ADDR;
		} else {
			op_mode         = AMD64_OP_REG;
			int const input = arity++;
			in[input]       = be_transform_node(op);
			reqs            = reg_reqs;
			addr.variant    = X86_ADDR_REG;
			addr.base_input = input;
		}
	}

	ir_node *const jmp = new_bd_amd64_ijmp(dbgi, new_block, arity, in, reqs,
	                                       X86_SIZE_64, op_mode, addr);
	fix_node_mem_proj(jmp, mem_proj);

	return be_new_Proj(jmp, pn_amd64_ijmp_X);
}

static ir_node *gen_Jmp(ir_node *const node)
{
	ir_node  *const new_block = be_transform_nodes_block(node);
	dbg_info *const dbgi      = get_irn_dbg_info(node);

	return new_bd_amd64_jmp(dbgi, new_block);
}

static ir_node *gen_Switch(ir_node *const node)
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
		= new_global_entity(irp->dummy_owner, id_unique("TBL"), utype,
		                    ir_visibility_private,
		                    IR_LINKAGE_CONSTANT | IR_LINKAGE_NO_IDENTITY);

	arch_register_req_t const **in_reqs;
	amd64_op_mode_t op_mode;
	int arity = 0;
	ir_node *in[1];
	x86_addr_t addr;
	if (ir_platform.pic_style != BE_PIC_NONE) {
		ir_node *const base
			= create_picaddr_lea(dbgi, new_block, X86_IMM_PCREL, entity);
		ir_node *load_in[3];
		int load_arity = 0;
		int load_base = load_arity++;
		int load_index = load_arity++;
		load_in[load_base]  = base;
		load_in[load_index] = new_sel;
		addr = (x86_addr_t) {
			.variant     = X86_ADDR_BASE_INDEX,
			.base_input  = load_base,
			.index_input = load_index,
			.log_scale   = 2,
		};
		ir_node *const load
			= new_bd_amd64_movs(dbgi, new_block, load_arity, load_in,
			                    amd64_reg_reg_reqs, X86_SIZE_32,
			                    AMD64_OP_ADDR, addr);
		ir_node *const load_res = be_new_Proj(load, pn_amd64_movs_res);

		ir_node *const add = create_add_lea(dbgi, new_block, X86_SIZE_64,
		                                    base, load_res);

		int const input = arity++;
		addr = (x86_addr_t) {
			.base_input = input,
			.variant    = X86_ADDR_REG,
		};
		op_mode   = AMD64_OP_REG;
		in[input] = add;
		in_reqs   = reg_reqs;
	} else {
		int index_in = arity++;
		in[index_in] = new_sel;
		in_reqs = reg_reqs;
		addr = (x86_addr_t) {
			.immediate = {
				.kind   = X86_IMM_ADDR,
				.entity = entity,
			},
			.variant     = X86_ADDR_INDEX,
			.index_input = index_in,
			.log_scale   = 3,
		};
		op_mode = AMD64_OP_ADDR;
	}

	table = ir_switch_table_duplicate(irg, table);

	ir_node *const out = new_bd_amd64_jmp_switch(dbgi, new_block, arity, in,
	                                             in_reqs, n_outs, op_mode,
	                                             X86_SIZE_64, &addr, table,
	                                             entity);
	return out;
}

static ir_node *gen_Start(ir_node *const node)
{
	x86_cconv_t const *const cconv = current_cconv;

	be_start_out outs[N_AMD64_REGISTERS] = { [REG_RSP] = BE_START_IGNORE };

	/* function parameters in registers */
	for (size_t i = 0, n = cconv->n_parameters; i != n; ++i) {
		reg_or_stackslot_t const *const param = &cconv->parameters[i];
		arch_register_t    const *const reg   = param->reg;
		if (reg)
			outs[reg->global_index] = BE_START_REG;
	}

	/* variadic parameters in registers */
	ir_graph  *const irg           = get_irn_irg(node);
	ir_entity *const entity        = get_irg_entity(irg);
	ir_type   *const function_type = get_entity_type(entity);
	if (is_method_variadic(function_type))
		amd64_collect_variadic_params(outs, current_cconv);

	/* callee saves */
	for (size_t i = 0; i < N_AMD64_REGISTERS; ++i) {
		if (rbitset_is_set(cconv->callee_saves, i))
			outs[i] = BE_START_REG;
	}
	if (!cconv->omit_fp)
		outs[REG_RBP] = BE_START_IGNORE;

	return be_new_Start(irg, outs);
}

static ir_node *gen_Proj_Start(ir_node *const node)
{
	ir_graph *const irg = get_irn_irg(node);
	unsigned  const pn  = get_Proj_num(node);
	be_transform_node(get_Proj_pred(node));

	switch ((pn_Start)pn) {
	case pn_Start_M:
		return be_get_Start_mem(irg);
	case pn_Start_T_args:
		return new_r_Bad(irg, mode_T);
	case pn_Start_P_frame_base:
		return get_frame_base(irg);
	}
	panic("unexpected Start Proj: %u", pn);
}

static ir_node *gen_Return(ir_node *const node)
{
	ir_graph          *const irg       = get_irn_irg(node);
	ir_node           *const new_block = be_transform_nodes_block(node);
	dbg_info          *const dbgi      = get_irn_dbg_info(node);
	ir_node           *const mem       = get_Return_mem(node);
	ir_node           *const new_mem   = be_transform_node(mem);
	size_t             const n_res     = get_Return_n_ress(node);
	x86_cconv_t const *const cconv     = current_cconv;

	/* estimate number of return values */
	size_t       p              = n_amd64_ret_first_result;
	size_t const n_callee_saves = rbitset_popcount(cconv->callee_saves, N_AMD64_REGISTERS);
	size_t const n_ins          = p + n_res + n_callee_saves;

	arch_register_req_t const **const reqs = be_allocate_in_reqs(irg, n_ins);
	ir_node **in = ALLOCAN(ir_node*, n_ins);

	in[n_amd64_ret_mem]   = new_mem;
	reqs[n_amd64_ret_mem] = arch_memory_req;

	in[n_amd64_ret_stack]   = get_initial_sp(irg);
	reqs[n_amd64_ret_stack] = &amd64_single_reg_req_gp_rsp;

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
		arch_register_t const *const reg = &amd64_registers[i];
		in[p]   = be_get_Start_proj(irg, reg);
		reqs[p] = reg->single_req;
		++p;
	}
	assert(p == n_ins);

	ir_node *const ret = new_bd_amd64_ret(dbgi, new_block, n_ins, in, reqs);
	be_stack_record_chain(&stack_env, ret, n_amd64_ret_stack, NULL);
	return ret;
}

static int make_store_value(amd64_binop_addr_attr_t *const attr, ir_mode *const mode, ir_node *val, ir_node **const in)
{
	int arity = 0;
	if (!mode_needs_gp_reg(mode)) {
		goto store_reg;
	} else if (match_immediate_32(&attr->u.immediate, val, false)) {
		attr->base.base.op_mode = AMD64_OP_ADDR_IMM;
	} else {
		val = be_skip_downconv(val, false);
store_reg:
		attr->base.base.op_mode = AMD64_OP_ADDR_REG;
		int const reg_input = arity++;
		in[reg_input]     = be_transform_node(val);
		attr->u.reg_input = reg_input;
	}
	return arity;
}

static ir_node *make_store_for_mode(ir_mode *const mode, dbg_info *const dbgi, ir_node *const block, int const arity, ir_node *const *const in, amd64_binop_addr_attr_t const *const attr, bool const pinned)
{
	construct_binop_func               cons;
	arch_register_req_t const **const *reqs;
	if (!mode_is_float(mode)) {
		cons = &new_bd_amd64_mov_store;
		reqs = gp_am_reqs;
	} else if (mode == x86_mode_E) {
		cons = &new_bd_amd64_fstp;
		reqs = x87K_am_reqs;
	} else {
		cons = &new_bd_amd64_movs_store_xmm;
		reqs = xmm_am_reqs;
	}
	ir_node *const new_store = cons(dbgi, block, arity, in, reqs[arity - 1], attr);
	set_irn_pinned(new_store, pinned);
	return new_store;
}

static ir_node *make_lea_base_frameoffset(dbg_info *dbgi, ir_node *block, ir_node *base, int32_t offset)
{
	ir_node *lea_in[] = { base };
	x86_addr_t lea_addr = {
		.immediate = {
			.offset = offset,
			.kind   = X86_IMM_VALUE,
		},
		.variant    = X86_ADDR_BASE,
		.base_input = 0,
	};
	return new_bd_amd64_lea(dbgi, block, ARRAY_SIZE(lea_in), lea_in, reg_reqs, X86_SIZE_64, lea_addr);
}

static ir_node *gen_Call(ir_node *const node)
{
	ir_node           *const callee       = get_Call_ptr(node);
	ir_node           *const block        = get_nodes_block(node);
	ir_node           *const new_block    = be_transform_node(block);
	dbg_info          *const dbgi         = get_irn_dbg_info(node);
	ir_node           *const mem          = get_Call_mem(node);
	ir_type           *const type         = get_Call_type(node);
	size_t             const n_params     = get_Call_n_params(node);
	size_t             const n_ress       = get_method_n_ress(type);
	/* max inputs: call, callee, arguments */
	ir_graph          *const irg          = get_irn_irg(node);
	x86_cconv_t       *const cconv
		= amd64_decide_calling_convention(type, NULL);
	size_t             const n_param_regs = cconv->n_param_regs;
	/* param-regs + mem + stackpointer + callee(2) + n_sse_regs */
	unsigned           const max_inputs   = 5 + n_param_regs;

	assert(n_params == cconv->n_parameters);

	record_returns_twice(irg, type);

	/* construct arguments */

	/* stack pointer input */
	ir_node *const stack           = get_initial_sp(irg);
	unsigned       param_stacksize = cconv->param_stacksize;
	/* Always construct an IncSP, so calls do not accidentally CSE. */
	ir_node *const callframe       = amd64_new_IncSP(new_block, stack, param_stacksize, false);

	/* match callee */
	x86_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	amd64_op_mode_t op_mode;

	ir_node *mem_proj = NULL;

	arch_register_req_t const **const in_req = be_allocate_in_reqs(irg, max_inputs);

	ir_node **const in         = ALLOCAN(ir_node*, max_inputs);
	int             in_arity   = 0;
	ir_node **const sync_ins   = ALLOCAN(ir_node*, 1 + 1 + n_params);
	int             sync_arity = 0;

	if (match_immediate_32(&addr.immediate, callee, true)) {
		op_mode = AMD64_OP_IMM32;
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

			x86_addr_variant_t variant = addr.variant;
			if (x86_addr_variant_has_base(variant))
				in_req[addr.base_input] = &amd64_class_reg_req_gp;
			if (x86_addr_variant_has_index(variant))
				in_req[addr.index_input] = &amd64_class_reg_req_gp;

			ir_node *load_mem     = get_Load_mem(load);
			ir_node *new_load_mem = be_transform_node(load_mem);
			sync_ins[sync_arity++] = new_load_mem;

			op_mode = AMD64_OP_ADDR;
			if (mem == load_mem || (is_Proj(mem) && get_Proj_pred(mem) == load))
				goto no_call_mem;
		} else {
			int const input = in_arity++;
			addr = (x86_addr_t) {
				.base_input = input,
				.variant    = X86_ADDR_REG,
			};
			in[input]     = be_transform_node(callee);
			in_req[input] = &amd64_class_reg_req_gp;
			op_mode       = AMD64_OP_REG;
		}
	}
	sync_ins[sync_arity++] = be_transform_node(mem);
no_call_mem:;

	ir_node *sync1 = be_make_Sync(new_block, sync_arity, sync_ins);
	// Reset for next sync
	sync_arity = 0;

	int const call_sp_pos = in_arity++;
	in_req[call_sp_pos]   = &amd64_single_reg_req_gp_rsp;
	in[call_sp_pos]       = callframe;

	/* vararg calls need the number of SSE registers used */
	if (is_method_variadic(type)) {
		ir_node *const nxmm = make_const(dbgi, new_block, cconv->n_xmm_regs);
		in_req[in_arity] = &amd64_single_reg_req_gp_rax;
		in[in_arity]     = nxmm;
		++in_arity;
	}

	/* parameters */
	for (size_t p = 0; p < n_params; ++p) {
		ir_node                  *const value = get_Call_param(node, p);
		reg_or_stackslot_t const *const param = &cconv->parameters[p];
		ir_type                  *const param_type = get_method_param_type(type, p);
		if (is_aggregate_type(param_type)) {
			/* Copy aggregate onto stack */
			ir_node *const lea       = make_lea_base_frameoffset(dbgi, block, callframe, param->offset);
			ir_node *const new_value = be_transform_node(value);
			unsigned const size      = get_type_size(param_type);
			ir_node *const copyb     = new_bd_amd64_copyB_i(dbgi, block, lea, new_value, sync1, size);
			sync_ins[sync_arity++] = be_new_Proj(copyb, pn_amd64_copyB_i_M);
		} else if (param->reg != NULL) {
			/* put value into registers */
			in[in_arity]     = be_transform_node(value);
			in_req[in_arity] = param->reg->single_req;
			++in_arity;
		} else {
			/* put value onto stack */
			ir_mode        *const mode = get_type_mode(get_method_param_type(type, p));
			x86_insn_size_t       size = x86_size_from_mode(mode);
			if (size < X86_SIZE_32)
				size = X86_SIZE_32;

			amd64_binop_addr_attr_t attr = {
				.base = {
					.base = {
						.size = size,
					},
					.addr = {
						.immediate = {
							.kind   = X86_IMM_VALUE,
							.offset = param->offset,
						},
						.variant = X86_ADDR_BASE,
					},
				},
			};

			ir_node *in[3];
			int      arity = make_store_value(&attr, mode, value, in);

			attr.base.addr.base_input = arity;
			in[arity++]               = callframe;
			in[arity++]               = sync1;
			sync_ins[sync_arity++]    = make_store_for_mode(mode, dbgi, new_block, arity, in, &attr, false);
		}
	}

	if (sync_arity == 0) {
		sync_ins[sync_arity++] = sync1;
	}

	/* memory input */
	in_req[in_arity] = arch_memory_req;
	int mem_pos      = in_arity;
	addr.mem_input   = mem_pos;
	++in_arity;

	/* construct memory input */
	in[mem_pos] = be_make_Sync(new_block, sync_arity, sync_ins);

	assert(in_arity <= (int)max_inputs);

	/* count outputs */
	unsigned       o              = pn_amd64_call_first_result;
	unsigned const n_caller_saves = rbitset_popcount(cconv->caller_saves, N_AMD64_REGISTERS);
	unsigned const out_arity      = o + cconv->n_reg_results + n_caller_saves;

	/* create call attributes */
	amd64_call_addr_attr_t call_attr = {
		.base = {
			.base = {
				.op_mode = op_mode,
				.size    = X86_SIZE_64,
			},
			.addr = addr,
		},
		.n_reg_results = cconv->n_reg_results,
	};

	/* create call node */
	ir_node *const call = new_bd_amd64_call(dbgi, new_block, in_arity, in, in_req, out_arity, &call_attr);
	fix_node_mem_proj(call, mem_proj);

	/* create output register reqs */
	arch_set_irn_register_req_out(call, pn_amd64_call_M, arch_memory_req);
	arch_copy_irn_out_info(call, pn_amd64_call_stack, callframe);

	arch_register_class_t const *const flags = &amd64_reg_classes[CLASS_amd64_flags];
	arch_set_irn_register_req_out(call, pn_amd64_call_flags, flags->class_req);

	/* add register requirements for the result regs */
	for (size_t r = 0; r < n_ress; ++r) {
		const reg_or_stackslot_t *result_info = &cconv->results[r];
		const arch_register_t    *reg         = result_info->reg;
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
	ir_node *sp = be_new_Proj(call, pn_amd64_call_stack);
	if (param_stacksize != 0)
		sp = amd64_new_IncSP(new_block, sp, -param_stacksize, false);
	be_stack_record_chain(&stack_env, callframe, n_be_IncSP_pred, sp);

	x86_free_calling_convention(cconv);
	return call;
}

static ir_node *gen_Proj_Call(ir_node *const node)
{
	unsigned const pn       = get_Proj_num(node);
	ir_node *const call     = get_Proj_pred(node);
	ir_node *const new_call = be_transform_node(call);
	switch ((pn_Call)pn) {
	case pn_Call_M:
		return be_new_Proj(new_call, pn_amd64_call_M);
	case pn_Call_X_regular:
	case pn_Call_X_except:
	case pn_Call_T_result:
		break;
	}
	panic("unexpected Call proj %+F", node);
}

static ir_node *gen_Proj_Proj_Call(ir_node *const node)
{
	ir_node *const call     = get_Proj_pred(get_Proj_pred(node));
	ir_node *const new_call = be_transform_node(call);
	unsigned const pn       = get_Proj_num(node);
	unsigned const new_pn   = pn_amd64_call_first_result + pn;
	return be_new_Proj(new_call, new_pn);
}

static ir_node *gen_Proj_Proj_Start(ir_node *const node)
{
	assert(get_Proj_num(get_Proj_pred(node)) == pn_Start_T_args);

	ir_graph                 *const irg   = get_irn_irg(node);
	unsigned                  const pn    = get_Proj_num(node);
	reg_or_stackslot_t const *const param = &current_cconv->parameters[pn];
	return be_get_Start_proj(irg, param->reg);
}

static ir_node *gen_Proj_Proj(ir_node *const node)
{
	ir_node *const pred      = get_Proj_pred(node);
	ir_node *const pred_pred = get_Proj_pred(pred);
	if (is_Call(pred_pred)) {
		return gen_Proj_Proj_Call(node);
	} else if (is_Start(pred_pred)) {
		return gen_Proj_Proj_Start(node);
	}
	panic("unexpected Proj(Proj) after %+F", pred_pred);
}

static ir_node *match_cmp_x87(ir_node *const node, ir_node *const op0,
                              ir_node *op1)
{
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_node(get_nodes_block(node));
	ir_node  *const new_op0   = be_transform_node(op0);
	ir_node  *const new_op1   = be_transform_node(op1);
	return new_bd_amd64_fucomi(dbgi, new_block, new_op0, new_op1);
}

static ir_node *gen_Cmp(ir_node *const node)
{
	ir_node  *const op1       = get_Cmp_left(node);
	ir_node  *const op2       = get_Cmp_right(node);
	ir_mode  *const cmp_mode  = get_irn_mode(op1);
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const block     = get_nodes_block(node);
	ir_node  *const new_block = be_transform_node(block);

	ir_node     *new_node;
	amd64_args_t args;
	if (mode_is_float(cmp_mode)) {
		if (cmp_mode == x86_mode_E)
			return match_cmp_x87(node, op1, op2);
		match_binop(&args, block, cmp_mode, op1, op2, match_am);
		new_node = new_bd_amd64_ucomis(dbgi, new_block, args.arity, args.in, args.reqs, &args.attr);
	} else {
		match_binop(&args, block, cmp_mode, op1, op2, match_immediate | match_am);
		new_node = new_bd_amd64_cmp(dbgi, new_block, args.arity, args.in, args.reqs, &args.attr);
	}

	fix_node_mem_proj(new_node, args.mem_proj);
	return be_new_Proj(new_node, pn_amd64_cmp_flags);
}

static ir_node *get_flags_node(ir_node *cmp, x86_condition_code_t *cc_out)
{
	/* must have a Cmp as input */
	ir_relation       relation = get_Cmp_relation(cmp);
	ir_node    *const l        = get_Cmp_left(cmp);
	ir_node    *const r        = get_Cmp_right(cmp);
	ir_mode    *const mode     = get_irn_mode(l);

	/* the middle-end tries to eliminate impossible relations, so a ptr <> 0
	 * test becomes ptr > 0. But for x86 an equal comparison is preferable to
	 * a >0 (we can sometimes eliminate the cmp in favor of flags produced by
	 * a predecessor node). So add the < bit.
	 * (Note that we do not want to produce <=> (which can happen for
	 * unoptimized code), because no x86 flag can represent that */
	if (!(relation & ir_relation_equal) && relation & ir_relation_less_greater)
		relation |= get_negated_relation(ir_get_possible_cmp_relations(l, r)) & ir_relation_less_greater;

	bool const overflow_possible = !is_irn_null(r);

	/* just do a normal transformation of the Cmp */
	*cc_out = ir_relation_to_x86_condition_code(relation, mode,
	                                            overflow_possible);
	ir_node *flags = be_transform_node(cmp);
	return flags;
}

static ir_node *gen_Cond(ir_node *const node)
{
	ir_node             *const sel = get_Cond_selector(node);
	x86_condition_code_t       cc;
	ir_node             *const flags = get_flags_node(sel, &cc);
	dbg_info            *const dbgi  = get_irn_dbg_info(node);
	ir_node             *const block = be_transform_nodes_block(node);
	return new_bd_amd64_jcc(dbgi, block, flags, cc);
}

static ir_node *gen_ASM(ir_node *const node)
{
	return x86_match_ASM(node, &amd64_asm_constraints);
}

static ir_node *gen_Phi(ir_node *const node)
{
	ir_mode                   *mode = get_irn_mode(node);
	const arch_register_req_t *req;
	if (mode_needs_gp_reg(mode)) {
		/* all integer operations are on 64bit registers now */
		req = &amd64_class_reg_req_gp;
	} else if (mode_is_float(mode)) {
		req = mode == x86_mode_E
		    ? &amd64_class_reg_req_x87
		    : &amd64_class_reg_req_xmm;
	} else {
		req = arch_memory_req;
	}

	return be_transform_phi(node, req);
}

static ir_node *match_mov(dbg_info *dbgi, ir_node *block, ir_node *value,
                          x86_insn_size_t size, create_mov_func create_mov,
                          unsigned pn_res)
{
	int      arity = 0;
	ir_node *in[4];
	ir_mode *mode = get_irn_mode(value);
	ir_node *load;
	ir_node *op;
	bool use_am = use_address_matching(mode, match_am, block, NULL,
	                                   value, &load, &op);

	amd64_op_mode_t op_mode;
	x86_addr_t      addr;
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
		int const input = arity++;
		addr = (x86_addr_t) {
			.base_input = input,
			.variant    = X86_ADDR_REG,
		};
		in[input] = new_value;
		reqs      = get_irn_mode(new_value) == amd64_mode_xmm ? amd64_xmm_reqs
		                                                      : reg_reqs;
		op_mode   = AMD64_OP_REG;
	}

	assert((size_t)arity <= ARRAY_SIZE(in));
	ir_node *const new_node = create_mov(dbgi, block, arity, in, reqs, size,
	                                     op_mode, addr);

	if (mem_proj != NULL)
		be_set_transformed_node(load, new_node);

	return be_new_Proj(new_node, pn_res);
}

static ir_node *gen_extend(dbg_info *const dbgi, ir_node *const block,
                           ir_node *const value, ir_mode *const from)
{
	assert(get_mode_size_bits(from) != 64);
	x86_insn_size_t const size        = x86_size_from_mode(from);
	bool            const is_signed   = mode_is_signed(from);
	create_mov_func const constructor = is_signed ? new_bd_amd64_movs
	                                              : new_bd_amd64_mov_gp;
	unsigned        const pn          = is_signed ? pn_amd64_movs_res
	                                              : pn_amd64_mov_gp_res;
	return match_mov(dbgi, block, value, size, constructor, pn);
}

static ir_node *extend_if_necessary(dbg_info *const dbgi,
                                    ir_node *const block, ir_node *const value)
{
	if (!needs_extension(value))
		return be_transform_node(value);
	return gen_extend(dbgi, block, value, get_irn_mode(value));
}

static ir_node *new_movd_wrapper(dbg_info *dbgi, ir_node *block, int arity,
                                 ir_node *const *in,
                                 arch_register_req_t const **in_reqs,
                                 x86_insn_size_t size, amd64_op_mode_t op_mode,
                                 x86_addr_t addr)
{
	(void)size;
	assert(size == X86_SIZE_64);
	return new_bd_amd64_movd(dbgi, block, arity, in, in_reqs, op_mode, addr);
}

static ir_node *create_movd(dbg_info *dbgi, ir_node *block, ir_node *value)
{
	return match_mov(dbgi, block, value, X86_SIZE_64, new_movd_wrapper,
	                 pn_amd64_movd_res);
}

static ir_node *new_cvtsd2ss_wrapper(dbg_info *dbgi, ir_node *block, int arity,
                                     ir_node *const *in,
                                     arch_register_req_t const **in_reqs,
                                     x86_insn_size_t size,
                                     amd64_op_mode_t op_mode, x86_addr_t addr)
{
	(void)size;
	assert(size == X86_SIZE_64);
	return new_bd_amd64_cvtsd2ss(dbgi, block, arity, in, in_reqs, op_mode, addr);
}

static ir_node *create_cvtsd2ss(dbg_info *dbgi, ir_node *block, ir_node *value)
{
	return match_mov(dbgi, block, value, X86_SIZE_64, new_cvtsd2ss_wrapper,
	                 pn_amd64_cvtsd2ss_res);
}

static void store_to_temp(construct_binop_func const new_store,
                          arch_register_req_t const **const in_reqs, x86_addr_t *addr,
                          dbg_info *dbgi, ir_node *block, ir_node **in, int *n_in,
                          ir_node *new_op, x86_insn_size_t size)
{
	ir_graph *const irg    = get_irn_irg(block);
	ir_node  *const frame  = get_irg_frame(irg);
	ir_node  *const nomem  = get_irg_no_mem(irg);
	ir_node  *const sin[]  = { new_op, frame, nomem };

	amd64_binop_addr_attr_t const attr = {
		.base = {
			.base = {
				.op_mode = AMD64_OP_ADDR_REG,
				.size    = size,
			},
			.addr = {
				.immediate = {
					.kind = X86_IMM_FRAMEENT,
				},
				.variant     = X86_ADDR_BASE,
				.base_input  = 1,
				.mem_input   = 2,
			},
		},
		.u.reg_input = 0,
	};

	ir_node *const store
		= new_store(dbgi, block, ARRAY_SIZE(sin), sin, in_reqs, &attr);
	set_irn_pinned(store, false);

	int base_input = (*n_in)++;
	in[base_input] = frame;
	int mem_input  = (*n_in)++;
	in[mem_input]  = store;
	*addr = attr.base.addr;
	addr->base_input = base_input;
	addr->mem_input  = mem_input;
}

static ir_node *conv_sse_to_x87(dbg_info *dbgi, ir_node *block, ir_node *op)
{
	ir_mode        *const mode   = get_irn_mode(op);
	x86_insn_size_t const size   = x86_size_from_mode(mode);
	ir_node        *const new_op = be_transform_node(op);
	ir_node *in[5];
	int      n_in = 0;
	x86_addr_t addr;
	store_to_temp(new_bd_amd64_movs_store_xmm, xmm_reg_mem_reqs, &addr, dbgi,
	              block, in, &n_in, new_op, size);
	assert(n_in < (int)ARRAY_SIZE(in));

	ir_node *load = new_bd_amd64_fld(dbgi, block, n_in, in, reg_mem_reqs, size,
	                                 AMD64_OP_ADDR, addr);
	set_irn_pinned(load, false);
	return be_new_Proj(load, pn_amd64_fld_res);
}

static ir_node *conv_x87_to_sse(dbg_info *dbgi, ir_node *block, ir_node *op,
                                ir_mode *dst_mode)
{
	x86_insn_size_t const size   = x86_size_from_mode(dst_mode);
	ir_node        *const new_op = be_transform_node(op);
	ir_node *in[5];
	int      n_in = 0;
	x86_addr_t addr;
	assert(get_mode_size_bits(dst_mode) <= 64);
	store_to_temp(new_bd_amd64_fst, x87_reg_mem_reqs, &addr, dbgi, block, in,
	              &n_in, new_op, size);
	assert(n_in < (int)ARRAY_SIZE(in));

	ir_node *load = new_bd_amd64_movs_xmm(dbgi, block, n_in, in, reg_mem_reqs,
	                                      size, AMD64_OP_ADDR, addr);
	set_irn_pinned(load, false);
	return be_new_Proj(load, pn_amd64_fld_res);
}

static ir_node *conv_int_to_x87(dbg_info *dbgi, ir_node *block, ir_node *val)
{
	ir_mode        *const mode    = get_irn_mode(val);
	ir_node        *const new_val = extend_if_necessary(dbgi, block, val);
	x86_insn_size_t const size    = get_size_32_64_from_mode(mode);
	if (!mode_is_signed(mode))
		panic("unsigned int -> x87 NIY");

	ir_node *in[5];
	int      n_in = 0;
	x86_addr_t addr;
	store_to_temp(new_bd_amd64_mov_store, reg_reg_mem_reqs, &addr, dbgi, block,
	              in, &n_in, new_val, size);
	assert(n_in < (int)ARRAY_SIZE(in));

	ir_node *load = new_bd_amd64_fild(dbgi, block, n_in, in, reg_mem_reqs, size,
	                                  AMD64_OP_ADDR, addr);
	set_irn_pinned(load, false);
	return be_new_Proj(load, pn_amd64_fild_res);
}

static ir_node *conv_x87_to_int(dbg_info *const dbgi, ir_node *const block,
                                ir_node *const val, ir_mode *const dest_mode)
{
	ir_node *const new_val = be_transform_node(val);

	x86_insn_size_t const insn_size_dest = x86_size_from_mode(dest_mode);
	x86_insn_size_t const insn_size_src  = insn_size_dest > X86_SIZE_32
	                                       ? X86_SIZE_64 : X86_SIZE_32;

	ir_node *in[5];
	int      n_in = 0;
	x86_addr_t addr;
	store_to_temp(new_bd_amd64_fisttp, x87K_reg_mem_reqs, &addr, dbgi, block,
	              in, &n_in, new_val, insn_size_src);
	assert(n_in < (int)ARRAY_SIZE(in));

	create_mov_func new_mov = insn_size_dest < X86_SIZE_64
		? new_bd_amd64_movs : new_bd_amd64_mov_gp;

	ir_node *load = new_mov(dbgi, block, n_in, in, reg_mem_reqs, insn_size_dest,
	                        AMD64_OP_ADDR, addr);
	set_irn_pinned(load, false);
	return be_new_Proj(load, pn_amd64_movs_res);
}

static ir_node *gen_Conv(ir_node *const node)
{
	ir_node  *const block    = be_transform_nodes_block(node);
	ir_node  *const op       = get_Conv_op(node);
	ir_mode  *const src_mode = get_irn_mode(op);
	ir_mode  *const dst_mode = get_irn_mode(node);
	dbg_info *const dbgi     = get_irn_dbg_info(node);
	unsigned  const src_bits = get_mode_size_bits(src_mode);
	unsigned  const dst_bits = get_mode_size_bits(dst_mode);

	/* ad-hoc assumption until libfirm has vector modes:
	 * we assume 128bit modes are two packed doubles. */
	if (dst_bits == 128) {
		/* int -> 2xdouble */
		assert(get_mode_arithmetic(src_mode) == irma_twos_complement);
		assert(dst_mode == amd64_mode_xmm);

		if (src_bits < 64) {
			ir_node *const op_ext = gen_extend(dbgi, block, op, src_mode);
			// No point in address matching here, the sign-/zero-extending mov
			// has done that already.
			ir_node   *const in[] = { op_ext };
			unsigned   const n_in = ARRAY_SIZE(in);
			x86_addr_t const addr = {
				.base_input = 0,
				.variant    = X86_ADDR_REG,
			};
			ir_node *const movd = new_bd_amd64_movd(dbgi, block, n_in, in,
			                                        reg_reqs, AMD64_OP_REG,
			                                        addr);
			return be_new_Proj(movd, pn_amd64_movd_res);
		} else if (src_bits == 64) {
			return create_movd(dbgi, block, op);
		} else {
			panic("cannot transform %+F", node);
		}
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
			panic("cannot transform %+F", node);
		}
	}

	bool const src_float = mode_is_float(src_mode);
	bool const dst_float = mode_is_float(dst_mode);

	if (!src_float && !dst_float) {
		/* int -> int */
		if (src_bits >= dst_bits || be_upper_bits_clean(op, src_mode)) {
			/* Omit unnecessary conversion. */
			return be_transform_node(op);
		} else {
			return gen_extend(dbgi, block, op, src_mode);
		}
	}

	ir_mode *min_mode;
	if (src_bits < dst_bits) {
		min_mode = src_mode;
	} else if (src_bits > dst_bits) {
		min_mode = dst_mode;
	} else {
		assert(src_bits == dst_bits);
		/* skip unnecessary conv */
		if (src_float && dst_float)
			return be_transform_node(op);
		min_mode = src_mode;
	}

	x86_insn_size_t size;
	if (get_mode_size_bits(min_mode) < 32) {
		/* Only 32-bit and 64-bit register size allowed for
		 * floating point conversion */
		size = X86_SIZE_32;
	} else if (!src_float && dst_float) {
		 /* In this case the size of the the floating point mode is irrelevant
		  * and could result in invalid register sizes. */
		size = x86_size_from_mode(src_mode);
	} else {
		size = x86_size_from_mode(min_mode);
	}

	if (dst_mode == x86_mode_E) {
		if (!src_float)
			return conv_int_to_x87(dbgi, block, op);
		/* SSE to x87 */
		return conv_sse_to_x87(dbgi, block, op);
	} else if (src_mode == x86_mode_E) {
		if (!dst_float)
			return conv_x87_to_int(dbgi, block, op, dst_mode);
		return conv_x87_to_sse(dbgi, block, op, dst_mode);
	}

	ir_node *const new_op = be_transform_node(op);
	ir_node *      in[]   = { new_op };
	unsigned const n_in   = ARRAY_SIZE(in);
	x86_addr_t     addr = {
		.base_input = 0,
		.variant    = X86_ADDR_REG,
	};

	ir_node *conv;
	unsigned pn_res;
	if (src_float && dst_float) {
		/* float to float */
		if (src_bits == 32 && dst_bits == 64) {
			conv   = new_bd_amd64_cvtss2sd(dbgi, block, n_in, in,
			                               amd64_xmm_reqs, size, AMD64_OP_REG,
			                               addr);
			pn_res = pn_amd64_cvtss2sd_res;
		} else if (src_bits == 64 && dst_bits == 32) {
			conv   = new_bd_amd64_cvtsd2ss(dbgi, block, n_in, in,
			                               amd64_xmm_reqs, AMD64_OP_REG, addr);
			pn_res = pn_amd64_cvtsd2ss_res;
		} else {
			panic("cannot transform %+F", node);
		}
	} else if (src_float && !dst_float) {
		/* float to int */

		if (!mode_is_signed(dst_mode) && dst_bits <= 32) {
			/* The conversion is signed only; simply use 64-bit register*/
			size = X86_SIZE_64;
		} else if (!mode_is_signed(dst_mode) && dst_bits == 64) {
			panic("cannot convert floating point to 64-bit unsigned");
		}

		if (src_bits == 32) {
			conv   = new_bd_amd64_cvttss2si(dbgi, block, n_in, in,
			                                amd64_xmm_reqs, size, AMD64_OP_REG,
			                                addr);
			pn_res = pn_amd64_cvttss2si_res;
		} else if (src_bits == 64) {
			conv   = new_bd_amd64_cvttsd2si(dbgi, block, n_in, in,
			                                amd64_xmm_reqs, size, AMD64_OP_REG,
			                                addr);
			pn_res = pn_amd64_cvttsd2si_res;
		} else {
			panic("cannot transform %+F", node);
		}
	} else {
		/* int to float */
		assert(!src_float && dst_float);

		// Conversion is from signed Dword/Qword only.
		// uint32_t have to be zero-extended into a int64_t,
		// while signed ints with less than 32 bit have to be
		// sign-extended.
		// This is done with an explicit mov/movs instruction.
		x86_insn_size_t move_mode = x86_size_from_mode(src_mode);
		if (mode_is_signed(src_mode)) {
			if (src_bits < 32) {
				// Let's just assume there is no integer type of width
				// between 32 and 64 bits. So we only extend to 32 bits.
				size = X86_SIZE_32;
				ir_node *const ext = new_bd_amd64_movs(dbgi, block, n_in, in,
														 reg_reqs, move_mode,
														 AMD64_OP_REG, addr);
				in[0] = be_new_Proj(ext, pn_amd64_movs_res);
			} else if (src_bits != 32 && src_bits != 64) {
				panic("conversion of signed %d bit integer to "
						"floating point not implemented", src_bits);
			}
		} else {
			assert(!mode_is_signed(src_mode));

			if (src_bits >= 64) {
				panic("cannot convert unsigned %d-bit to floating point",
						src_bits);
			}

			size = src_bits >= 32 ? X86_SIZE_64 : X86_SIZE_32;
			ir_node *const ext = new_bd_amd64_mov_gp(dbgi, block, n_in, in,
														 reg_reqs, move_mode,
														 AMD64_OP_REG, addr);
			in[0] = be_new_Proj(ext, pn_amd64_mov_gp_res);
		}

		if (dst_bits == 32) {
			// http://www.felixcloutier.com/x86/CVTSI2SS.html
			conv   = new_bd_amd64_cvtsi2ss(dbgi, block, n_in, in, reg_reqs,
			                               size, AMD64_OP_REG, addr);
			pn_res = pn_amd64_cvtsi2ss_res;
		} else if (dst_bits == 64) {
			// http://www.felixcloutier.com/x86/CVTSI2SD.html
			conv   = new_bd_amd64_cvtsi2sd(dbgi, block, n_in, in, reg_reqs,
			                               size, AMD64_OP_REG, addr);
			pn_res = pn_amd64_cvtsi2sd_res;
		} else {
			panic("cannot transform %+F", node);
		}
	}
	return be_new_Proj(conv, pn_res);
}

static ir_node *gen_Store(ir_node *const node)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const val   = get_Store_value(node);
	ir_mode  *const mode  = get_irn_mode(val);

	amd64_binop_addr_attr_t attr;
	memset(&attr, 0, sizeof(attr));

	ir_node *in[4];
	int      arity = make_store_value(&attr, mode, val, in);

	ir_node *ptr     = get_Store_ptr(node);
	perform_address_matching(ptr, &arity, in, &attr.base.addr);

	ir_node *mem     = get_Store_mem(node);
	ir_node *new_mem = be_transform_node(mem);
	in[arity++]      = new_mem;
	assert((size_t)arity <= ARRAY_SIZE(in));
	attr.base.base.size = x86_size_from_mode(mode);
	bool const pinned = get_irn_pinned(node);
	return make_store_for_mode(mode, dbgi, block, arity, in, &attr, pinned);
}

static bool amd64_mode_needs_gp_reg(ir_mode *const mode)
{
	return be_mode_needs_gp_reg(mode) && mode != amd64_mode_xmm;
}

ir_node *amd64_new_spill(ir_node *const value, ir_node *const after)
{
	x86_insn_size_t             size;
	construct_binop_func        cons;
	arch_register_req_t const **reqs;
	ir_mode            *const   mode = get_irn_mode(value);
	if (amd64_mode_needs_gp_reg(mode)) {
		size = X86_SIZE_64;
		cons = &new_bd_amd64_mov_store;
		reqs = reg_reg_mem_reqs;
	} else if (mode == x86_mode_E) {
		size = X86_SIZE_80;
		cons = &new_bd_amd64_fst;
		reqs = x87_reg_mem_reqs;
	} else {
		size = X86_SIZE_128;
		/* TODO: currently our stack alignment is messed up so we can't use
		 * movdqa for spilling... */
		cons = new_bd_amd64_movdqu_store;
		reqs = xmm_reg_mem_reqs;
	}

	amd64_binop_addr_attr_t const attr = {
		.base = {
			.base = {
				.op_mode = AMD64_OP_ADDR_REG,
				.size    = size,
			},
			.addr = {
				.immediate.kind = X86_IMM_FRAMEENT,
				.variant        = X86_ADDR_BASE,
				.base_input     = 1,
			},
		},
		.u.reg_input = 0,
	};

	ir_node  *const block = get_block(after);
	ir_graph *const irg   = get_irn_irg(block);
	ir_node  *const frame = get_irg_frame(irg);
	ir_node  *const mem   = get_irg_no_mem(irg);
	ir_node  *const in[]  = { value, frame, mem };
	ir_node  *const store = cons(NULL, block, ARRAY_SIZE(in), in, reqs, &attr);
	arch_add_irn_flags(store, arch_irn_flag_spill);
	sched_add_after(after, store);
	return store;
}

static ir_node *create_sse_spill(dbg_info *const dbgi, ir_node *const block,
                                 int const arity, ir_node *const *const in,
                                 arch_register_req_t const **const in_reqs,
                                 x86_insn_size_t const size, amd64_op_mode_t const op_mode,
                                 x86_addr_t const addr)
{
	(void)size; /* TODO */
	return new_bd_amd64_movdqu(dbgi, block, arity, in, in_reqs, op_mode, addr);
}

ir_node *amd64_new_reload(ir_node *value, ir_node *spill, ir_node *before)
{
	ir_node  *const block = get_block(before);
	ir_graph *const irg   = get_irn_irg(block);
	ir_node  *const frame = get_irg_frame(irg);
	ir_mode  *const mode  = get_irn_mode(value);

	x86_addr_t addr = {
		.immediate.kind = X86_IMM_FRAMEENT,
		.variant    = X86_ADDR_BASE,
		.base_input = 0,
		.mem_input  = 1,
	};
	ir_node *in[] = { frame, spill };

	unsigned        pn_res;
	create_mov_func cons;
	x86_insn_size_t size;
	if (amd64_mode_needs_gp_reg(mode)) {
		size   = X86_SIZE_64;
		cons   = &new_bd_amd64_mov_gp;
		pn_res = pn_amd64_mov_gp_res;
	} else if (mode == x86_mode_E) {
		size   = X86_SIZE_80;
		cons   = &new_bd_amd64_fld;
		pn_res = pn_amd64_fld_res;
	} else {
		size   = X86_SIZE_128;
		cons   = &create_sse_spill;
		pn_res = pn_amd64_movdqu_res;
	}
	ir_node *const load = cons(NULL, block, ARRAY_SIZE(in), in, reg_mem_reqs,
	                           size, AMD64_OP_ADDR, addr);
	arch_add_irn_flags(load, arch_irn_flag_reload);
	sched_add_before(before, load);
	amd64_addr_attr_t *attr = get_amd64_addr_attr(load);
	attr->addr.immediate.kind = X86_IMM_FRAMEENT;
	return be_new_Proj(load, pn_res);
}

static ir_node *gen_Load(ir_node *const node)
{

	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_mode  *const mode  = get_Load_mode(node);
	ir_node  *const ptr   = get_Load_ptr(node);

	int arity = 0;
	ir_node *in[3];
	x86_addr_t addr;
	memset(&addr, 0, sizeof(addr));

	perform_address_matching(ptr, &arity, in, &addr);

	arch_register_req_t const **const reqs = gp_am_reqs[arity];

	ir_node *const mem     = get_Load_mem(node);
	ir_node *const new_mem = be_transform_node(mem);
	in[arity++]      = new_mem;
	assert((size_t)arity <= ARRAY_SIZE(in));

	create_mov_func   const cons      =
		mode_is_float(mode)                                   ?
			(mode == x86_mode_E ? new_bd_amd64_fld : &new_bd_amd64_movs_xmm) :
		get_mode_size_bits(mode) < 64 && mode_is_signed(mode) ? &new_bd_amd64_movs     :
		/**/                                                    &new_bd_amd64_mov_gp;
	x86_insn_size_t const size     = x86_size_from_mode(mode);
	ir_node        *const new_load = cons(dbgi, block, arity, in, reqs, size, AMD64_OP_ADDR, addr);
	set_irn_pinned(new_load, get_irn_pinned(node));

	return new_load;
}

static ir_node *gen_Unknown(ir_node *const node)
{
	ir_node *const block = be_transform_nodes_block(node);
	ir_mode *const mode  = get_irn_mode(node);
	if (mode_is_float(mode)) {
		return be_new_Unknown(block, &amd64_class_reg_req_xmm);
	} else if (be_mode_needs_gp_reg(mode)) {
		return be_new_Unknown(block, &amd64_class_reg_req_gp);
	} else {
		panic("unexpected Unknown mode");
	}
}

static const unsigned pn_amd64_mem = 2;

static ir_node *gen_Proj_Load(ir_node *const node)
{
	ir_node  *const load     = get_Proj_pred(node);
	ir_node  *const new_load = be_transform_node(load);
	unsigned  const pn       = get_Proj_num(node);

	/* loads might be part of source address mode matches, so we don't
	   transform the ProjMs yet (with the exception of loads whose result is
	   not used) */
	if (is_Load(load) && pn == pn_Load_M && get_irn_n_edges(load) > 1) {
		/* this is needed, because sometimes we have loops that are only
		   reachable through the ProjM */
		be_enqueue_operands(node);
		/* do it in 2 steps, to silence firm verifier */
		ir_node *const res = new_r_Proj(load, mode_M, pn_Load_M);
		set_Proj_num(res, pn_amd64_mem);
		return res;
	}

	/* renumber the proj */
	switch (get_amd64_irn_opcode(new_load)) {
	case iro_amd64_movs_xmm:
		if (pn == pn_Load_res) {
			return be_new_Proj(new_load, pn_amd64_movs_xmm_res);
		} else if (pn == pn_Load_M) {
			return be_new_Proj(new_load, pn_amd64_movs_xmm_M);
		}
		break;
	case iro_amd64_movs:
	case iro_amd64_mov_gp:
		assert((unsigned)pn_amd64_movs_res == (unsigned)pn_amd64_mov_gp_res);
		assert((unsigned)pn_amd64_movs_M   == (unsigned)pn_amd64_mov_gp_M);
		/* handle all gp loads equal: they have the same proj numbers. */
		if (pn == pn_Load_res) {
			return be_new_Proj(new_load, pn_amd64_movs_res);
		} else if (pn == pn_Load_M) {
			return be_new_Proj(new_load, pn_amd64_movs_M);
		}
		break;
	case iro_amd64_fld:
		if (pn == pn_Load_res) {
			return be_new_Proj(new_load, pn_amd64_fld_res);
		} else if (pn == pn_Load_M) {
			return be_new_Proj(new_load, pn_amd64_fld_M);
		}
		break;
	case iro_amd64_add:
	case iro_amd64_and:
	case iro_amd64_cmp:
		assert(pn == pn_Load_M);
		return be_new_Proj(new_load, pn_amd64_mem);
	default:
		panic("unsupported Proj from Load");
	}

    return be_duplicate_node(node);
}

static ir_node *gen_Proj_Store(ir_node *const node)
{
	ir_node *const pred = get_Proj_pred(node);
	unsigned const pn   = get_Proj_num(node);
	if (pn == pn_Store_M) {
		return be_transform_node(pred);
	} else {
		panic("unsupported Proj from Store");
	}
}

static ir_node *gen_Alloc(ir_node *const node)
{
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_nodes_block(node);
	ir_node  *const size      = get_Alloc_size(node);
	ir_node  *const mem       = get_Alloc_mem(node);
	ir_node  *const new_mem   = be_transform_node(mem);

	const arch_register_req_t **reqs;

	ir_node *in[3];
	unsigned arity = 0;

	ir_graph *const irg = get_irn_irg(node);
	int const input0 = arity++;
	in[input0] = get_initial_sp(irg);

	amd64_binop_addr_attr_t attr = {
		.base = {
			.base.size = X86_SIZE_64,
			.addr = {
				.base_input = input0,
				.variant    = X86_ADDR_REG,
			},
		},
	};
	if (is_Const(size)) {
		ir_tarval *tv           = get_Const_tarval(size);
		long       sizel        = get_tarval_long(tv);

		attr.base.base.op_mode  = AMD64_OP_REG_IMM;
		attr.u.immediate.offset = sizel;
		reqs                    = rsp_mem_reqs;
	} else {
		int const input1 = arity++;
		in[input1]       = be_transform_node(size);
		attr.base.base.op_mode  = AMD64_OP_REG_REG;
		attr.u.reg_input        = input1;
		reqs                    = rsp_reg_mem_reqs;
	}
	in[arity++] = new_mem;
	ir_node *subsp = new_bd_amd64_sub_sp(dbgi, new_block, arity, in, reqs,
	                                     &attr);

	ir_node *const stack_proj = be_new_Proj_reg(subsp, pn_amd64_sub_sp_stack,
	                                            &amd64_registers[REG_RSP]);
	be_stack_record_chain(&stack_env, subsp, n_amd64_sub_sp_stack, stack_proj);

	return subsp;
}

static ir_node *gen_Proj_Alloc(ir_node *const node)
{
	ir_node *const alloc     = get_Proj_pred(node);
	ir_node *const new_alloc = be_transform_node(alloc);
	unsigned const pn        = get_Proj_num(node);

	switch ((pn_Alloc)pn) {
	case pn_Alloc_M:   return be_new_Proj(new_alloc, pn_amd64_sub_sp_M);
	case pn_Alloc_res: return be_new_Proj(new_alloc, pn_amd64_sub_sp_addr);
	}
	panic("invalid Proj->Alloc");
}

static ir_node *gen_clz(ir_node *const node)
{
	// https://fgiesen.wordpress.com/2013/10/18/bit-scanning-equivalencies/
	ir_node         *const bsr   = gen_unop_out(node, n_Builtin_max + 1,
	                                            new_bd_amd64_bsr, pn_amd64_bsr_res);
	ir_node         *const real  = skip_Proj(bsr);
	dbg_info        *const dbgi  = get_irn_dbg_info(real);
	ir_node         *const block = get_nodes_block(real);
	x86_insn_size_t  const size  = get_amd64_attr_const(real)->size;
	size_t           const mask  = (x86_bytes_from_size(size) * 8) - 1;
	ir_node         *const in[]  = { bsr };
	amd64_binop_addr_attr_t attr = {
		.base = {
			.base = {
				.op_mode = AMD64_OP_REG_IMM,
				.size    = size,
			},
			.addr = {
				.base_input = 0,
				.variant    = X86_ADDR_REG,
			},
		},
		.u = {
			.immediate = {
				.entity = NULL,
				.offset = mask,
			},
		},
	};
	ir_node *xor = new_bd_amd64_xor(dbgi, block, ARRAY_SIZE(in), in, reg_reqs,
	                                &attr);

	arch_set_irn_register_req_out(xor, 0, &amd64_requirement_gp_same_0);

	return be_new_Proj(xor, pn_amd64_xor_res);
}

static ir_node *gen_ctz(ir_node *const node)
{
	return gen_unop_out(node, n_Builtin_max + 1, new_bd_amd64_bsf,
	                    pn_amd64_bsf_res);
}

static ir_node *gen_ffs(ir_node *const node)
{
	/* bsf input, result */
	ir_node  *const bsf_res = gen_unop_out(node, n_Builtin_max + 1,
	                                       new_bd_amd64_bsf, pn_amd64_bsf_res);
	ir_node  *const bsf     = skip_Proj(bsf_res);

	/* seteq temp */
	dbg_info *const dbgi    = get_irn_dbg_info(bsf);
	ir_node  *const block   = get_nodes_block(bsf);
	ir_node  *const flags   = be_new_Proj(bsf, pn_amd64_bsf_flags);
	ir_node  *const setcc   = new_bd_amd64_setcc(dbgi, block, flags, x86_cc_equal);

	/* movzbl temp, temp */
	ir_node  *const movzbl_in[] = { setcc };
	x86_addr_t movzbl_addr = {
		.base_input = 0,
		.variant    = X86_ADDR_REG,
	};
	ir_node *const movzbl
		= new_bd_amd64_mov_gp(dbgi, block, ARRAY_SIZE(movzbl_in), movzbl_in,
		                      reg_reqs, X86_SIZE_8, AMD64_OP_REG, movzbl_addr);
	ir_node *const movzbl_res = be_new_Proj(movzbl, pn_amd64_mov_gp_res);

	/* neg temp */
	x86_insn_size_t size    = get_amd64_attr_const(bsf)->size;
	ir_node  *const neg     = new_bd_amd64_neg(dbgi, block, movzbl_res, size);
	ir_node  *const neg_res = be_new_Proj(neg, pn_amd64_neg_res);

	/* or temp, result */
	ir_node *const or_in[] = { neg_res, bsf_res };
	amd64_binop_addr_attr_t const or_attr = {
		.base = {
			.base = {
				.op_mode = AMD64_OP_REG_REG,
				.size    = size,
			},
			.addr = {
				.base_input = 0,
				.variant    = X86_ADDR_REG,
			},
		},
		.u = {
			.reg_input = 1,
		},
	};
	ir_node *or     = new_bd_amd64_or(dbgi, block, ARRAY_SIZE(or_in), or_in, amd64_reg_reg_reqs, &or_attr);
	arch_set_irn_register_req_out(or, 0, &amd64_requirement_gp_same_0);
	ir_node *or_res = be_new_Proj(or, pn_amd64_or_res);

	/* add $1, result */
	ir_node *inc_in[] = { or_res };
	amd64_binop_addr_attr_t inc_attr = {
		.base = {
			.base = {
				.op_mode = AMD64_OP_REG_IMM,
				.size    = size,
			},
			.addr = {
				.base_input = 0,
				.variant    = X86_ADDR_REG,
			},
		},
		.u = {
			.immediate = {
				.entity = NULL,
				.offset = 1,
			},
		},
	};
	ir_node *inc = new_bd_amd64_add(dbgi, block, ARRAY_SIZE(inc_in), inc_in, reg_reqs, &inc_attr);
	arch_set_irn_register_req_out(inc, 0, &amd64_requirement_gp_same_0);
	return be_new_Proj(inc, pn_amd64_add_res);
}

static ir_node *gen_compare_swap(ir_node *const node)
{
	dbg_info *const dbgi    = get_irn_dbg_info(node);
	ir_node  *const block   = be_transform_nodes_block(node);
	ir_node  *const ptr     = get_Builtin_param(node, 0);
	ir_node  *const old     = get_Builtin_param(node, 1);
	ir_node  *const new     = get_Builtin_param(node, 2);
	ir_node  *const mem     = get_Builtin_mem(node);
	ir_node  *const new_old = be_transform_node(old);
	ir_node  *const new_new = be_transform_node(new);
	ir_node  *const new_mem = be_transform_node(mem);
	ir_mode  *const mode    = get_irn_mode(new);
	assert(get_irn_mode(old) == mode);

	ir_node *in[5];
	int arity = 0;
	x86_addr_t addr;
	perform_address_matching(ptr, &arity, in, &addr);

	static arch_register_req_t const **const am_rax_reg_mem_reqs[] = {
		rax_reg_mem_reqs,
		reg_rax_reg_mem_reqs,
		reg_reg_rax_reg_mem_reqs,
	};
	assert((size_t)arity < ARRAY_SIZE(am_rax_reg_mem_reqs));
	arch_register_req_t const **const reqs = am_rax_reg_mem_reqs[arity];

	in[arity++] = new_old;
	int new_input = arity;
	in[arity++] = new_new;
	in[arity++] = new_mem;

	amd64_binop_addr_attr_t const attr = {
		.base = {
			.base = {
				.op_mode = AMD64_OP_ADDR_REG,
				.size    = x86_size_from_mode(mode),
			},
			.addr = addr,
		},
		.u = {
			.reg_input = new_input,
		},
	};
	return new_bd_amd64_cmpxchg(dbgi, block, arity, in, reqs, &attr);
}

static ir_node *gen_saturating_increment(ir_node *const node)
{
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_nodes_block(node);
	ir_node  *const param0    = get_Builtin_param(node, 0);
	ir_node  *const operand   = be_transform_node(param0);
	ir_mode  *const mode      = get_irn_mode(param0);

	amd64_binop_addr_attr_t inc_attr = {
		.base = {
			.base = {
				.op_mode = AMD64_OP_REG_IMM,
				.size    = x86_size_from_mode(mode),
			},
			.addr = {
				.base_input = 0,
				.variant    = X86_ADDR_REG,
			},
		},
		.u.immediate = {
			.kind   = X86_IMM_VALUE,
			.offset = 1,
		},
	};
	ir_node  *inc_in[]          = { operand };

	ir_node  *const inc = new_bd_amd64_add(dbgi, new_block, ARRAY_SIZE(inc_in), inc_in, reg_reqs, &inc_attr);
	arch_set_irn_register_req_out(inc, 0, &amd64_requirement_gp_same_0);

	ir_node *const value  = be_new_Proj(inc, pn_amd64_add_res);
	ir_node *const eflags = be_new_Proj(inc, pn_amd64_add_flags);

	amd64_binop_addr_attr_t sbb_attr = {
		.base = {
			.base = {
				.op_mode = AMD64_OP_REG_IMM,
				.size    = x86_size_from_mode(mode),
			},
			.addr = {
				.base_input = 0,
				.variant    = X86_ADDR_REG,
			},
		},
		.u.immediate = {
			.kind   = X86_IMM_VALUE,
			.offset = 0,
		},
	};
	ir_node *in[2]     = { value, eflags };

	ir_node *const sbb = new_bd_amd64_sbb(dbgi, new_block, ARRAY_SIZE(in), in, reg_flags_reqs, &sbb_attr);
	arch_set_irn_register_req_out(sbb, 0, &amd64_requirement_gp_same_0);

	return sbb;
}

static ir_node *gen_va_start(ir_node *const node)
{
	ir_graph *const irg   = get_irn_irg(node);
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const mem   = be_transform_node(get_Builtin_mem(node));
	ir_node  *const ap    = be_transform_node(get_irn_n(node, pn_Builtin_max + 1));
	ir_node  *const fp    = get_frame_base(irg);

	return amd64_initialize_va_list(dbgi, block, current_cconv, mem, ap, fp);
}

static ir_node *gen_Builtin(ir_node *const node)
{
	ir_builtin_kind const kind = get_Builtin_kind(node);
	switch (kind) {
	case ir_bk_clz:
		return gen_clz(node);
	case ir_bk_ctz:
		return gen_ctz(node);
	case ir_bk_ffs:
		return gen_ffs(node);
	case ir_bk_compare_swap:
		return gen_compare_swap(node);
	case ir_bk_saturating_increment:
		return gen_saturating_increment(node);
	case ir_bk_va_start:
		return gen_va_start(node);
	default:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

static ir_node *gen_Proj_Builtin(ir_node *const proj)
{
	ir_node        *const node      = get_Proj_pred(proj);
	ir_node        *const new_node  = be_transform_node(node);
	ir_builtin_kind const kind      = get_Builtin_kind(node);

	switch (kind) {
	case ir_bk_clz:
	case ir_bk_ctz:
	case ir_bk_ffs:
	case ir_bk_parity:
		return new_node;
	case ir_bk_compare_swap:
		assert(is_amd64_cmpxchg(new_node));
		if (get_Proj_num(proj) == pn_Builtin_M) {
			return be_new_Proj(new_node, pn_amd64_cmpxchg_M);
		} else {
			assert(get_Proj_num(proj) == pn_Builtin_max+1);
			return be_new_Proj(new_node, pn_amd64_cmpxchg_res);
		}
	case ir_bk_saturating_increment:
		return be_new_Proj(new_node, pn_amd64_sbb_res);
	case ir_bk_va_start:
		assert(get_Proj_num(proj) == pn_Builtin_M);
		return new_node;
	default:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

static ir_node *gen_Bitcast(ir_node *const node)
{
	ir_node  *const op        = get_Bitcast_op(node);
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_mode  *const dst_mode  = get_irn_mode(node);
	ir_mode  *const src_mode  = get_irn_mode(op);
	bool      const dst_float = mode_is_float(dst_mode);
	bool      const src_float = mode_is_float(src_mode);
	ir_node  *const be_op     = be_transform_node(op);
	ir_node  *const be_block  = get_nodes_block(be_op);

	x86_addr_t const addr = {
		.base_input = 0,
		.variant    = X86_ADDR_REG,
	};

	if (src_float && !dst_float) {
		return new_bd_amd64_movd_xmm_gp(dbgi, be_block, be_op, X86_SIZE_64, AMD64_OP_REG, addr);
	} else if (!src_float && dst_float) {
		return new_bd_amd64_movd_gp_xmm(dbgi, be_block, be_op, X86_SIZE_64, AMD64_OP_REG, addr);
	} else {
		panic("unhandled bitcast modes: %+F to %+F\n", src_mode, dst_mode);
	}
}

static ir_node *gen_amd64_l_punpckldq(ir_node *const node)
{
	ir_node *const op0 = get_irn_n(node, n_amd64_l_punpckldq_arg0);
	ir_node *const op1 = get_irn_n(node, n_amd64_l_punpckldq_arg1);
	return gen_binop_xmm(node, op0, op1, new_bd_amd64_punpckldq, match_am);
}

static ir_node *gen_amd64_l_subpd(ir_node *const node)
{
	ir_node *const op0 = get_irn_n(node, n_amd64_l_subpd_arg0);
	ir_node *const op1 = get_irn_n(node, n_amd64_l_subpd_arg1);
	return gen_binop_xmm(node, op0, op1, new_bd_amd64_subpd, match_am);
}

static ir_node *gen_amd64_l_haddpd(ir_node *const node)
{
	ir_node *const op0 = get_irn_n(node, n_amd64_l_haddpd_arg0);
	ir_node *const op1 = get_irn_n(node, n_amd64_l_haddpd_arg1);
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
	be_set_transform_function(op_amd64_l_haddpd,    gen_amd64_l_haddpd);
	be_set_transform_function(op_amd64_l_subpd,     gen_amd64_l_subpd);
	be_set_transform_function(op_be_Relocation,     gen_be_Relocation);

	be_set_transform_proj_function(op_Alloc,   gen_Proj_Alloc);
	be_set_transform_proj_function(op_Builtin, gen_Proj_Builtin);
	be_set_transform_proj_function(op_Call,    gen_Proj_Call);
	be_set_transform_proj_function(op_Div,     gen_Proj_Div);
	be_set_transform_proj_function(op_Load,    gen_Proj_Load);
	be_set_transform_proj_function(op_Mod,     gen_Proj_Mod);
	be_set_transform_proj_function(op_Proj,    gen_Proj_Proj);
	be_set_transform_proj_function(op_Start,   gen_Proj_Start);
	be_set_transform_proj_function(op_Store,   gen_Proj_Store);

	/* upper_bits_clean can't handle different register sizes, so
	 * arithmetic operations are problematic. Disable them. */
	be_set_upper_bits_clean_function(op_And,  NULL);
	be_set_upper_bits_clean_function(op_Eor,  NULL);
	be_set_upper_bits_clean_function(op_Mux,  NULL);
	be_set_upper_bits_clean_function(op_Or,   NULL);
	be_set_upper_bits_clean_function(op_Shr,  NULL);
	be_set_upper_bits_clean_function(op_Shrs, NULL);
}

void amd64_transform_graph(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_TUPLES
	                         | IR_GRAPH_PROPERTY_NO_BADS
	                         | IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	amd64_register_transformers();

	be_stack_init(&stack_env);
	ir_entity *entity = get_irg_entity(irg);
	ir_type   *mtp    = get_entity_type(entity);
	current_cconv = amd64_decide_calling_convention(mtp, irg);
	bool const is_variadic = is_method_variadic(mtp);
	if (is_variadic)
		amd64_insert_reg_save_area(irg, current_cconv);
	x86_layout_param_entities(irg, current_cconv, AMD64_REGISTER_SIZE);
	amd64_set_va_stack_args_param(current_cconv->va_start_addr);
	be_add_parameter_entity_stores(irg);
	x86_create_parameter_loads(irg, current_cconv);

	heights = heights_new(irg);
	x86_calculate_non_address_mode_nodes(irg);
	be_transform_graph(irg, NULL);
	x86_free_non_address_mode_nodes();
	heights_free(heights);
	heights = NULL;

	be_stack_finish(&stack_env);

	if (is_variadic) {
		ir_node *const fp = get_frame_base(irg);
		amd64_save_vararg_registers(irg, current_cconv, fp);
	}

	x86_free_calling_convention(current_cconv);
	place_code(irg);

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);
}

void amd64_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.transform");
}
