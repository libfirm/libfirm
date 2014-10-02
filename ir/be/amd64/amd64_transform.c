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
#include "beutil.h"
#include "bearch_amd64_t.h"
#include "beirg.h"
#include "beabihelper.h"
#include "besched.h"

#include "amd64_cconv.h"
#include "amd64_common_transform.h"
#include "amd64_new_nodes.h"
#include "amd64_nodes_attr.h"
#include "amd64_transform.h"
#include "../ia32/x86_address_mode.h"

#include "gen_amd64_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct reg_info_t {
	size_t   offset;
	ir_node *irn;
} reg_info_t;

static ir_mode         *mode_gp;
static ir_mode         *mode_flags;
static amd64_cconv_t   *current_cconv = NULL;
static reg_info_t       start_mem;
static reg_info_t       start_sp;
static reg_info_t       start_fp;
static size_t           start_callee_saves_offset;
static size_t           start_params_offset;
static pmap            *node_to_stack;
static be_stackorder_t *stackorder;

static const arch_register_req_t amd64_requirement_gp = {
	.cls             = &amd64_reg_classes[CLASS_amd64_gp],
	.limited         = NULL,
	.type            = arch_register_req_type_normal,
	.other_same      = 0,
	.other_different = 0,
	.width           = 1,
};

static const arch_register_req_t amd64_requirement_flags = {
	.cls             = &amd64_reg_classes[CLASS_amd64_flags],
	.limited         = NULL,
	.type            = arch_register_req_type_normal,
	.other_same      = 0,
	.other_different = 0,
	.width           = 1,
};

static const arch_register_req_t amd64_requirement_xmm = {
	.cls             = &amd64_reg_classes[CLASS_amd64_xmm],
	.limited         = NULL,
	.type            = arch_register_req_type_normal,
	.other_same      = 0,
	.other_different = 0,
	.width           = 1,
};

#define BIT(x)    (1u << x)

static const arch_register_req_t amd64_requirement_gp_same_0 = {
	.cls             = &amd64_reg_classes[CLASS_amd64_gp],
	.limited         = NULL,
	.type            = arch_register_req_type_normal
	                   | arch_register_req_type_should_be_same,
	.other_same      = BIT(0),
	.other_different = 0,
	.width           = 1,
};

static const arch_register_req_t amd64_requirement_xmm_same_0 = {
	.cls             = &amd64_reg_classes[CLASS_amd64_xmm],
	.limited         = NULL,
	.type            = arch_register_req_type_normal
	                   | arch_register_req_type_should_be_same,
	.other_same      = BIT(0),
	.other_different = 0,
	.width           = 1,
};

static const arch_register_req_t amd64_requirement_gp_same_0_not_1 = {
	.cls             = &amd64_reg_classes[CLASS_amd64_gp],
	.limited         = NULL,
	.type            = arch_register_req_type_normal
	                   | arch_register_req_type_should_be_same
	                   | arch_register_req_type_must_be_different,
	.other_same      = BIT(0),
	.other_different = BIT(1),
	.width           = 1,
};

static const unsigned amd64_limited_gp_rcx [] = { BIT(REG_GP_RCX) };
static const arch_register_req_t amd64_requirement_rcx = {
	.cls             = &amd64_reg_classes[CLASS_amd64_gp],
	.limited         = amd64_limited_gp_rcx,
	.type            = arch_register_req_type_limited,
	.other_same      = 0,
	.other_different = 0,
	.width           = 1,
};

static const unsigned amd64_limited_gp_rax [] = { BIT(REG_GP_RAX) };
static const arch_register_req_t amd64_requirement_rax = {
	.cls             = &amd64_reg_classes[CLASS_amd64_gp],
	.limited         = amd64_limited_gp_rax,
	.type            = arch_register_req_type_limited,
	.other_same      = 0,
	.other_different = 0,
	.width           = 1,
};

static const unsigned amd64_limited_gp_rdx [] = { BIT(REG_GP_RDX) };
static const arch_register_req_t amd64_requirement_rdx = {
	.cls             = &amd64_reg_classes[CLASS_amd64_gp],
	.limited         = amd64_limited_gp_rdx,
	.type            = arch_register_req_type_limited,
	.other_same      = 0,
	.other_different = 0,
	.width           = 1,
};

static const arch_register_req_t *mem_reqs[] = {
	&arch_no_requirement,
};

static const arch_register_req_t *reg_mem_reqs[] = {
	&amd64_requirement_gp,
	&arch_no_requirement,
};

static const arch_register_req_t *xmm_mem_reqs[] = {
	&amd64_requirement_xmm,
	&arch_no_requirement,
};

static const arch_register_req_t *reg_reg_mem_reqs[] = {
	&amd64_requirement_gp,
	&amd64_requirement_gp,
	&arch_no_requirement,
};

static const arch_register_req_t *xmm_reg_mem_reqs[] = {
	&amd64_requirement_xmm,
	&amd64_requirement_gp,
	&arch_no_requirement,
};

static const arch_register_req_t *reg_reg_reg_mem_reqs[] = {
	&amd64_requirement_gp,
	&amd64_requirement_gp,
	&amd64_requirement_gp,
	&arch_no_requirement,
};

static const arch_register_req_t *xmm_reg_reg_mem_reqs[] = {
	&amd64_requirement_xmm,
	&amd64_requirement_gp,
	&amd64_requirement_gp,
	&arch_no_requirement,
};

static const arch_register_req_t *reg_flags_reqs[] = {
	&amd64_requirement_gp,
	&amd64_requirement_flags,
};

static const arch_register_req_t *reg_reg_reqs[] = {
	&amd64_requirement_gp,
	&amd64_requirement_gp,
};

static const arch_register_req_t *rax_reg_reqs[] = {
	&amd64_requirement_rax,
	&amd64_requirement_gp,
};

static const arch_register_req_t *rax_reg_rdx_reqs[] = {
	&amd64_requirement_rax,
	&amd64_requirement_gp,
	&amd64_requirement_rdx,
};

static const arch_register_req_t *reg_reqs[] = {
	&amd64_requirement_gp,
};

static const arch_register_req_t *xmm_reqs[] = {
	&amd64_requirement_xmm,
};

static const arch_register_req_t *reg_rcx_reqs[] = {
	&amd64_requirement_gp,
	&amd64_requirement_rcx,
};

static const arch_register_req_t *no_reqs[] = {
};

static const arch_register_req_t *xmm_xmm_reqs[] = {
	&amd64_requirement_xmm,
	&amd64_requirement_xmm,
};

static inline bool mode_needs_gp_reg(ir_mode *mode)
{
	return get_mode_arithmetic(mode) == irma_twos_complement;
}

static bool is_downconv(const ir_node *node)
{
	if (!is_Conv(node))
		return false;

	ir_mode *dest_mode = get_irn_mode(node);
	if (!mode_needs_gp_reg(dest_mode))
		return false;
	ir_mode *src_mode = get_irn_mode(get_Conv_op(node));
	if (!mode_needs_gp_reg(src_mode))
		return false;
	return get_mode_size_bits(dest_mode) <= get_mode_size_bits(src_mode);
}

static ir_node *skip_downconv(ir_node *node)
{
	while (is_downconv(node)) {
		if (get_irn_n_edges(node) > 1)
			break;
		node = get_Conv_op(node);
	}
	return node;
}

static bool is_sameconv(const ir_node *node)
{
	if (!is_Conv(node))
		return false;
	ir_mode *dest_mode = get_irn_mode(node);
	if (!mode_needs_gp_reg(dest_mode))
		return false;
	ir_mode *src_mode = get_irn_mode(get_Conv_op(node));
	if (!mode_needs_gp_reg(src_mode))
		return false;
	return get_mode_size_bits(dest_mode) == get_mode_size_bits(src_mode);
}

static ir_node *skip_sameconv(ir_node *node)
{
	while (is_sameconv(node)) {
		if (get_irn_n_edges(node) > 1)
			break;
		node = get_Conv_op(node);
	}
	return node;
}

static ir_node *get_reg(ir_graph *const irg, reg_info_t *const reg)
{
	if (!reg->irn) {
		/* this is already the transformed start node */
		ir_node *const start = get_irg_start(irg);
		assert(is_amd64_Start(start));
		arch_register_class_t const *const cls
			= arch_get_irn_register_req_out(start, reg->offset)->cls;
		reg->irn = new_r_Proj(start, cls ? cls->mode : mode_M, reg->offset);
	}
	return reg->irn;
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

static ir_node *get_frame_base(ir_graph *irg)
{
	if (current_cconv->omit_fp) {
		return get_initial_sp(irg);
	} else {
		return get_initial_fp(irg);
	}
}

static ir_node *gen_Const(ir_node *node)
{
	ir_node  *block = be_transform_node(get_nodes_block(node));
	dbg_info *dbgi  = get_irn_dbg_info(node);
	ir_mode  *mode  = get_irn_mode(node);
	ir_tarval *tv = get_Const_tarval(node);

	if (!mode_needs_gp_reg(mode)) {
		if (tarval_is_null(tv)) {
			return new_bd_amd64_xXorp0(dbgi, block);
		}

		return create_float_const(dbgi, block, tv);
	}

	uint64_t val = get_tarval_uint64(tv);
	amd64_insn_mode_t imode = val > UINT32_MAX ? INSN_MODE_64 : INSN_MODE_32;
	return new_bd_amd64_MovImm(dbgi, block, imode, val, NULL);
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

static ir_node *gen_Address(ir_node *node)
{
	ir_node   *block  = be_transform_node(get_nodes_block(node));
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_entity *entity = get_Address_entity(node);

	/* do we need RIP-relative addressing because of PIC? */
	reference_mode_t mode = need_relative_addressing(entity);
	if (mode == REFERENCE_DIRECT)
		return new_bd_amd64_MovImm(dbgi, block, INSN_MODE_64, 0, entity);

	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.base_input  = NO_INPUT;
	addr.index_input = NO_INPUT;
	addr.mem_input   = NO_INPUT;

	if (mode == REFERENCE_IP_RELATIVE) {
		addr.immediate.entity = entity;
		ir_node *lea = new_bd_amd64_Lea(dbgi, block, 0, NULL, INSN_MODE_64, addr);
		return new_r_Proj(lea, mode_gp, pn_amd64_Lea_res);
	} else {
		assert(mode == REFERENCE_GOT);
		addr.immediate.entity = new_got_entry_entity(entity);
		ir_node *load = new_bd_amd64_Mov(dbgi, block, 0, NULL, INSN_MODE_64,
										 AMD64_OP_ADDR, addr);
		return new_r_Proj(load, mode_gp, pn_amd64_Mov_res);
	}
}

static ir_node *create_IncSP(const arch_register_t *sp_reg, ir_node *block,
                             ir_node *old_sp, int offset, int align)
{
	ir_node *res = be_new_IncSP(sp_reg, block, old_sp, offset, align);
	arch_add_irn_flags(res, arch_irn_flag_modify_flags);
	return res;
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

static bool match_immediate_32(amd64_imm32_t *imm, const ir_node *op,
                               bool can_match_ip_relative,
                               bool upper32_dont_care)
{
	assert(mode_needs_gp_reg(get_irn_mode(op)));
	assert(imm->offset == 0 && imm->entity == NULL);
	if (is_Const(op)) {
		ir_tarval *tv = get_Const_tarval(op);
		if (!tarval_is_long(tv))
			return false;
		long    lval = get_tarval_long(tv);
		int32_t val  = (int32_t)lval;
		if ((long)val != lval)
			return false;
		/** the immediate value is signed extended to 64bit, sometimes
		 * this is not what we want. */
		if (!upper32_dont_care && val < 0
		    && !mode_is_signed(get_tarval_mode(tv)))
		    return false;
		imm->offset = val;
		return true;
	} else if (can_match_ip_relative && is_Address(op)) {
		/* TODO: check if entity is in lower 4GB address space/relative */
		ir_entity *entity = get_Address_entity(op);
		imm->entity = entity;
		return true;
	}
	/* TODO: SymConst, Add(SymConst, Const) ... */
	return false;
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
	/* ia32 backend claims this can happen, use an assert for now and see
	 * if we hit it :) */
	assert(!be_is_transformed(node));
	return load;
}

static bool needs_extension(ir_node *op)
{
	ir_mode *mode = get_irn_mode(op);
	if (get_mode_size_bits(mode) >= 32)
		return false;
	return !be_upper_bits_clean(op, mode);
}

static ir_node *create_sext(ir_node *block, ir_node *const node, ir_mode *mode)
{
	amd64_insn_mode_t insn_mode = get_insn_mode_from_mode(mode);
	dbg_info *const   dbgi      = get_irn_dbg_info(node);
	ir_node  *const   new_node  = be_transform_node(node);
	ir_node  *const   new_block = be_transform_node(block);

	amd64_shift_attr_t attr;
	memset(&attr, 0, sizeof(attr));
	attr.base.op_mode = AMD64_OP_SHIFT_IMM;
	attr.insn_mode    = insn_mode;
	attr.immediate    = get_mode_size_bits(mode) - 1;
	ir_node *in[1]    = { new_node };
	ir_node *sext     = new_bd_amd64_Sar(dbgi, new_block, ARRAY_SIZE(in),
	                                     in, &attr);

	arch_set_irn_register_reqs_in(sext, reg_reqs);
	arch_set_irn_register_req_out(sext, 0, &amd64_requirement_gp_same_0);

	return sext;
}

static ir_node *create_zext(ir_node *block, ir_node *const node)
{
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_node(block);
	ir_node  *const zero      = new_bd_amd64_Xor0(dbgi, new_block);

	arch_set_irn_register_reqs_in(zero, reg_reqs);
	return zero;
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
		addr->base_input = NO_INPUT;
	}
	if (maddr.index != NULL) {
		int index_input = (*arity)++;
		addr->index_input = index_input;
		in[index_input]  = be_transform_node(maddr.index);
	} else {
		addr->index_input = NO_INPUT;
	}
	if (maddr.frame_entity != NULL) {
		assert(maddr.entity == NULL);
		addr->immediate.entity = maddr.frame_entity;
		/* not supported yet */
		assert(!is_parameter_entity(maddr.frame_entity)
		       || get_entity_parameter_number(maddr.frame_entity)
		          != IR_VA_START_PARAMETER_NUMBER);
	} else {
		addr->immediate.entity = maddr.entity;
	}
	addr->immediate.offset = maddr.offset;
	addr->log_scale        = maddr.scale;
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
		op1 = skip_downconv(op1);
		op2 = skip_downconv(op2);
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

		args->reqs = use_xmm ? xmm_mem_reqs : reg_mem_reqs;
		if (addr->base_input != NO_INPUT && addr->index_input != NO_INPUT) {
			args->reqs = use_xmm ? xmm_reg_reg_mem_reqs
			             : reg_reg_reg_mem_reqs;
		} else if (addr->base_input != NO_INPUT || addr->index_input != NO_INPUT) {
			args->reqs = use_xmm ? xmm_reg_mem_reqs
			             : reg_reg_mem_reqs;
		}
		ir_node *new_mem    = be_transform_node(get_Load_mem(load));
		int mem_input       = args->arity++;
		args->in[mem_input] = new_mem;
		addr->mem_input     = mem_input;

		args->mem_proj      = be_get_Proj_for_pn(load, pn_Load_M);
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
                             construct_binop_func func, match_flags_t flags)
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
		return new_r_Proj(new_node, mode_D, pn_amd64_xSubs_res);
	} else {
		arch_set_irn_register_req_out(new_node, 0,
		                              &amd64_requirement_gp_same_0);
		return new_r_Proj(new_node, mode_gp, pn_amd64_Sub_res);
	}
}

/* Transform arguments for a binop using RAX and register as input */
static void gen_binop_rax_reg(ir_node *op1, ir_node *op2, ir_node **in,
                              int *arity, amd64_op_mode_t *op_mode,
                              const arch_register_req_t ***reqs)
{
	in[(*arity)++] = be_transform_node(op1);
	in[(*arity)++] = be_transform_node(op2);
	*reqs          = rax_reg_reqs;
	*op_mode       = AMD64_OP_RAX_REG;
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
		op1 = skip_downconv(op1);
		op2 = skip_downconv(op2);
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

		reqs = reg_mem_reqs;
		if (addr.base_input != NO_INPUT && addr.index_input != NO_INPUT) {
			reqs = reg_reg_reg_mem_reqs;
		} else if (addr.base_input != NO_INPUT || addr.index_input != NO_INPUT) {
			reqs = reg_reg_mem_reqs;
		}

		ir_node *new_mem = be_transform_node(get_Load_mem(load));
		int mem_input    = arity++;
		in[mem_input]    = new_mem;
		addr.mem_input   = mem_input;

		mem_proj                = be_get_Proj_for_pn(load, pn_Load_M);
		op_mode                 = AMD64_OP_RAX_ADDR;
	} else {
		/* simply transform the arguments */
		gen_binop_rax_reg(op1, op2, in, &arity, &op_mode, &reqs);
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

typedef ir_node *(*construct_shift_func)(dbg_info *dbgi, ir_node *block,
	int arity, ir_node *in[], const amd64_shift_attr_t *attr_init);

static ir_node *gen_shift_binop(ir_node *node, ir_node *op1, ir_node *op2,
                                construct_shift_func func, match_flags_t flags)
{
	ir_mode *mode = get_irn_mode(node);
	assert(!mode_is_float(mode));

	if (get_mode_modulo_shift(mode) != 32 && get_mode_size_bits(mode) != 64)
		panic("insupported modulo shift used");

	ir_node *in[3];
	int      arity = 0;
	if (flags & match_mode_neutral) {
		op1 = skip_downconv(op1);
		in[arity++] = be_transform_node(op1);
		mode = get_mode_size_bits(mode) > 32 ? mode_gp : mode_Iu;
	} else {
		op1 = skip_sameconv(op1);

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
		attr.immediate    = get_tarval_long(get_Const_tarval(op2));
	} else {
		attr.base.op_mode = AMD64_OP_SHIFT_REG;
		in[arity++]       = be_transform_node(op2);
		reqs              = reg_rcx_reqs;
		out_req0          = &amd64_requirement_gp_same_0_not_1;
	}
	attr.insn_mode = get_insn_mode_from_mode(mode);

	ir_node  *const block     = get_nodes_block(node);
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const new_block = be_transform_node(block);
	ir_node  *const new_node  = func(dbgi, new_block, arity, in, &attr);
	arch_set_irn_register_reqs_in(new_node, reqs);
	arch_set_irn_register_req_out(new_node, 0, out_req0);
	return new_node;
}

static ir_node *create_Lea_as_Add(ir_node *node, ir_node *op1, ir_node *op2)
{
	dbg_info *const dbgi = get_irn_dbg_info(node);
	ir_node  *block      = get_nodes_block(node);
	ir_node  *new_block  = be_transform_node(block);
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
		addr.index_input = 0;
		addr.base_input  = 1;
		reqs             = reg_reg_reqs;
	}

	ir_node *res = new_bd_amd64_Lea(dbgi, new_block, arity, in, insn_mode, addr);
	arch_set_irn_register_reqs_in(res, reqs);
	return new_r_Proj(res, mode_gp, pn_amd64_Lea_res);
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
		return gen_binop_am(node, op1, op2, new_bd_amd64_xAdds,
		                    match_commutative | match_am);
	}

	bool use_am = use_address_matching(mode, flags, block, op1, op2, &load, &op);

	ir_node *res;
	if (use_am)
		res = gen_binop_am(node, op1, op2, new_bd_amd64_Add, flags);
	else
		res = create_Lea_as_Add(node, op1, op2);

	x86_mark_non_am(node);
	return res;
}

static ir_node *gen_Sub(ir_node *const node)
{
	ir_node  *const op1     = get_Sub_left(node);
	ir_node  *const op2     = get_Sub_right(node);
	ir_mode  *const mode    = get_irn_mode(node);

	if (mode_is_float(mode)) {
		return gen_binop_am(node, op1, op2, new_bd_amd64_xSubs, match_am);
	} else {
		/* do not match AM yet until we have a sub->neg+add rule
		 * in amd64_finish */
		return gen_binop_am(node, op1, op2, new_bd_amd64_Sub,
		                    /* match_am | */ match_immediate);
	}
}

static ir_node *gen_And(ir_node *const node)
{
	ir_node *op1 = get_And_left(node);
	ir_node *op2 = get_And_right(node);
	return gen_binop_am(node, op1, op2, new_bd_amd64_And,
	                    match_immediate | match_am | match_mode_neutral
	                    | match_commutative);
}

static ir_node *gen_Eor(ir_node *const node)
{
	ir_node *op1 = get_Eor_left(node);
	ir_node *op2 = get_Eor_right(node);
	return gen_binop_am(node, op1, op2, new_bd_amd64_Xor,
	                    match_immediate | match_am | match_mode_neutral
	                    | match_commutative);
}

static ir_node *gen_Or(ir_node *const node)
{
	ir_node *op1 = get_Or_left(node);
	ir_node *op2 = get_Or_right(node);
	return gen_binop_am(node, op1, op2, new_bd_amd64_Or,
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
		            gen_binop_rax(node, op1, op2, new_bd_amd64_IMul1Op,
		                          match_mode_neutral
		                          | match_commutative);
		return new_r_Proj(new_node, mode_gp, pn_amd64_IMul1Op_res_low);
	} else if (mode_is_float(mode)) {
		return gen_binop_am(node, op1, op2, new_bd_amd64_xMuls,
		                    match_commutative | match_am);
	} else {
		return gen_binop_am(node, op1, op2, new_bd_amd64_IMul,
		                    match_immediate | match_am
		                    | match_mode_neutral | match_commutative);
	}
}

static ir_node *gen_Mulh(ir_node *const node)
{
	ir_node *op1  = get_Mulh_left(node);
	ir_node *op2  = get_Mulh_right(node);
	ir_mode *mode = get_irn_mode(op1);
	ir_node *new_node;

	if (mode_is_signed(mode)) {
		new_node = gen_binop_rax(node, op1, op2, new_bd_amd64_IMul1Op,
                        /* match_am TODO */
                        match_mode_neutral | match_commutative);
		return new_r_Proj(new_node, mode_gp, pn_amd64_IMul1Op_res_high);
	} else {
		new_node = gen_binop_rax(node, op1, op2, new_bd_amd64_Mul,
                         /* match_am TODO */
                         match_mode_neutral | match_commutative);
		return new_r_Proj(new_node, mode_gp, pn_amd64_Mul_res_high);
	}
}

static ir_node *gen_Shl(ir_node *const node)
{
	ir_node *op1 = get_Shl_left(node);
	ir_node *op2 = get_Shl_right(node);
	return gen_shift_binop(node, op1, op2, new_bd_amd64_Shl,
	                       match_immediate | match_mode_neutral);
}

static ir_node *gen_Shr(ir_node *const node)
{
	ir_node *op1 = get_Shr_left(node);
	ir_node *op2 = get_Shr_right(node);
	return gen_shift_binop(node, op1, op2, new_bd_amd64_Shr,
	                       match_immediate);
}

static ir_node *gen_Shrs(ir_node *const node)
{
	ir_node *op1 = get_Shrs_left(node);
	ir_node *op2 = get_Shrs_right(node);
	return gen_shift_binop(node, op1, op2, new_bd_amd64_Sar,
	                       match_immediate);
}

static ir_node *create_div(ir_node *const node, ir_mode *const mode,
                           ir_node *const op1, ir_node *const op2)
{
	dbg_info *const dbgi      = get_irn_dbg_info(node);
	ir_node  *const block     = get_nodes_block(node);
	ir_node  *const new_block = be_transform_node(block);
	amd64_insn_mode_t insn_mode
		= get_mode_size_bits(mode) > 32 ? INSN_MODE_64 : INSN_MODE_32;

	ir_node *res;
	ir_node *in[3];
	int      arity = 0;
	const arch_register_req_t **reqs;

	amd64_op_mode_t op_mode;
	amd64_addr_t    addr;
	memset(&addr, 0, sizeof(addr));

	gen_binop_rax_reg(op1, op2, in, &arity, &op_mode, &reqs);
	reqs = rax_reg_rdx_reqs;

	if (mode_is_signed(mode)) {
		/* Sign extend RAX to RDX */
		in[arity++] = create_sext(block, op1, mode);

		res = new_bd_amd64_IDiv(dbgi, new_block, arity, in, insn_mode,
                                op_mode, addr);
	} else {
		/* Zero extend to register RDX */
		in[arity++] = create_zext(block, node);

		res = new_bd_amd64_Div(dbgi, new_block, arity, in, insn_mode,
                               op_mode, addr);
	}

	arch_set_irn_register_reqs_in(res, reqs);
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

	ir_node *const new_node = new_bd_amd64_xDivs(dbgi, new_block, args.arity,
	                                             args.in, &args.attr);
	arch_set_irn_register_reqs_in(new_node, args.reqs);

	fix_node_mem_proj(new_node, args.mem_proj);

	arch_set_irn_register_req_out(new_node, 0,
	                              &amd64_requirement_xmm_same_0);
	return new_node;
}

static ir_node *gen_Div(ir_node *const node)
{
	ir_mode *const mode = get_Div_resmode(node);
	ir_node *const op1 = get_Div_left(node);
	ir_node *const op2 = get_Div_right(node);

	if (mode_is_float(mode))
		return create_sse_div(node, mode, op1, op2);
	else
		return create_div(node, mode, op1, op2);
}

static ir_node *gen_Proj_Div(ir_node *const node)
{
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);
	unsigned pn       = get_Proj_num(node);

	assert((unsigned)pn_amd64_Div_M == (unsigned)pn_amd64_IDiv_M);
	assert((unsigned)pn_amd64_Div_res_div == (unsigned)pn_amd64_IDiv_res_div);
	assert((unsigned)pn_amd64_xDivs_M == (unsigned)pn_amd64_IDiv_M);
	assert((unsigned)pn_amd64_xDivs_res_div == (unsigned)pn_amd64_IDiv_res_div);

	ir_mode *mode;
	if (mode_is_float(get_Div_resmode(pred)))
		mode = mode_D;
	else
		mode = mode_gp;

	switch ((pn_Div)pn) {
	case pn_Div_M:
		return new_r_Proj(new_pred, mode_M, pn_amd64_Div_M);
	case pn_Div_res:
		return new_r_Proj(new_pred, mode, pn_amd64_Div_res_div);
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
	assert(mode_needs_gp_reg(mode));
	return create_div(node, mode, op1, op2);
}

static ir_node *gen_Proj_Mod(ir_node *const node)
{
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);
	unsigned pn       = get_Proj_num(node);

	assert((unsigned)pn_amd64_Div_M == (unsigned)pn_amd64_IDiv_M);
	assert((unsigned)pn_amd64_Div_res_mod == (unsigned)pn_amd64_IDiv_res_mod);
	switch ((pn_Mod)pn) {
	case pn_Mod_M:
		return new_r_Proj(new_pred, mode_M, pn_amd64_Div_M);
	case pn_Mod_res:
		return new_r_Proj(new_pred, mode_gp, pn_amd64_Div_res_mod);
	case pn_Mod_X_except:
	case pn_Mod_X_regular:
		panic("amd64 exception NIY");
	}
	panic("invalid Mod Proj");
}

typedef ir_node* (*unop_constructor)(dbg_info*,ir_node*block,ir_node*op,amd64_insn_mode_t insn_mode);

static ir_node *gen_unop(ir_node *const node, int op_pos, unop_constructor gen)
{
	dbg_info *const dbgi   = get_irn_dbg_info(node);
	ir_node  *const block  = be_transform_node(get_nodes_block(node));
	ir_node  *const op     = get_irn_n(node, op_pos);
	ir_node  *const new_op = be_transform_node(op);
	ir_mode  *const mode   = get_irn_mode(node);

	amd64_insn_mode_t insn_mode
		= get_mode_size_bits(mode) > 32 ? INSN_MODE_64 : INSN_MODE_32;
	return gen(dbgi, block, new_op, insn_mode);
}

/** Create a floating point negation by switching the sign bit using a Xor.
  */
static ir_node *gen_float_neg(ir_node *const node)
{
	dbg_info  *const dbgi = get_irn_dbg_info(node);
	ir_node   *block      = get_nodes_block(node);
	ir_node   *new_block  = be_transform_node(block);
	ir_node   *op         = get_irn_n(node, n_Minus_op);
	ir_node   *new_op     = be_transform_node(op);
	ir_mode   *mode       = get_irn_mode(node);
	ir_tarval *tv         = create_sign_tv(mode);
	ir_node   *load       = create_float_const(dbgi, new_block, tv);
	ir_node   *in[]       = { new_op, load };

	amd64_binop_addr_attr_t attr;
	memset(&attr, 0, sizeof(attr));
	attr.base.base.op_mode = AMD64_OP_REG_REG;

	ir_node *xor = new_bd_amd64_xXorp(dbgi, new_block, ARRAY_SIZE(in),
	                                  in, &attr);
	arch_set_irn_register_reqs_in(xor, xmm_xmm_reqs);
	arch_set_irn_register_req_out(xor, 0, &amd64_requirement_xmm_same_0);

	return new_r_Proj(xor, mode, pn_amd64_xXorp_res);
}

static ir_node *gen_Minus(ir_node *const node)
{
	ir_mode *mode = get_irn_mode(node);

	if (mode_is_float(mode)) {
		return gen_float_neg(node);
	} else {
		return gen_unop(node, n_Minus_op, &new_bd_amd64_Neg);
	}
}
static ir_node *gen_Not(ir_node *const node)
{
	return gen_unop(node, n_Not_op, &new_bd_amd64_Not);
}

static ir_node *gen_Member(ir_node *const node)
{
	ir_node   *block     = get_nodes_block(node);
	ir_node   *new_block = be_transform_node(block);
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
	ir_node *res = new_bd_amd64_Lea(dbgi, new_block, ARRAY_SIZE(in), in,
	                                INSN_MODE_64, addr);
	arch_set_irn_register_reqs_in(res, reg_reqs);
	return new_r_Proj(res, mode_gp, pn_amd64_Lea_res);
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
			mem_proj          = be_get_Proj_for_pn(load, pn_Load_M);

			perform_address_matching(load_ptr, &arity, in, &addr);
			assert((size_t)arity < ARRAY_SIZE(in));

			reqs = mem_reqs;
			if (addr.base_input != NO_INPUT && addr.index_input != NO_INPUT) {
				reqs = reg_reg_mem_reqs;
			} else if (addr.base_input != NO_INPUT || addr.index_input != NO_INPUT) {
				reqs = reg_mem_reqs;
			}
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

	ir_node *jmp = new_bd_amd64_IJmp(dbgi, new_block, arity, in,
	                                 INSN_MODE_64, op_mode, addr);

	arch_set_irn_register_reqs_in(jmp, reqs);
	fix_node_mem_proj(jmp, mem_proj);

	ir_node *proj_X = new_r_Proj(jmp, mode_X, pn_amd64_IJmp_X);
	return proj_X;
}

static ir_node *gen_Jmp(ir_node *node)
{
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);

	return new_bd_amd64_Jmp(dbgi, new_block);
}

static ir_node *gen_Switch(ir_node *node)
{
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *new_block = be_transform_node(get_nodes_block(node));
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

	ir_node *out = new_bd_amd64_SwitchJmp(dbgi, new_block, new_sel, n_outs, table, entity);
	return out;
}

static void make_start_out(reg_info_t *const info, struct obstack *const obst,
                           ir_node *const start, size_t const offset,
                           arch_register_t const *const reg,
                           arch_register_req_type_t const flags)
{
	info->offset = offset;
	info->irn    = NULL;
	arch_register_req_t const *const req
		= be_create_reg_req(obst, reg, arch_register_req_type_ignore | flags);
	arch_set_irn_register_req_out(start, offset, req);
	arch_set_irn_register_out(start, offset, reg);
}

static ir_node *gen_Start(ir_node *node)
{
	ir_graph  *irg           = get_irn_irg(node);
	ir_entity *entity        = get_irg_entity(irg);
	ir_type   *function_type = get_entity_type(entity);
	ir_node   *block         = get_nodes_block(node);
	ir_node   *new_block     = be_transform_node(block);
	dbg_info  *dbgi          = get_irn_dbg_info(node);
	struct obstack *obst     = be_get_be_obst(irg);

	amd64_cconv_t const *const cconv = current_cconv;

	/* start building list of start constraints */

	/* calculate number of outputs */
	size_t n_outs = 2; /* memory, rsp */
	/* function parameters */
	n_outs += cconv->n_param_regs;
	size_t n_callee_saves
		= rbitset_popcount(cconv->callee_saves, N_AMD64_REGISTERS);
	n_outs += n_callee_saves;

	ir_node *start = new_bd_amd64_Start(dbgi, new_block, n_outs);

	size_t o = 0;

	/* first output is memory */
	start_mem.offset = o;
	start_mem.irn    = NULL;
	arch_set_irn_register_req_out(start, o, arch_no_register_req);
	++o;

	/* the stack pointer */
	make_start_out(&start_sp, obst, start, o++, &amd64_registers[REG_RSP],
	               arch_register_req_type_produces_sp);

	/* function parameters in registers */
	start_params_offset = o;
	for (size_t i = 0; i < get_method_n_params(function_type); ++i) {
		const reg_or_stackslot_t *param = &current_cconv->parameters[i];
		const arch_register_t    *reg   = param->reg;
		if (reg != NULL) {
			arch_set_irn_register_req_out(start, o, reg->single_req);
			arch_set_irn_register_out(start, o, reg);
			++o;
		}
	}

	/* callee saves */
	start_callee_saves_offset = o;
	for (size_t i = 0; i < N_AMD64_REGISTERS; ++i) {
		if (!rbitset_is_set(cconv->callee_saves, i))
			continue;
		if (!cconv->omit_fp && i == REG_RBP) {
			make_start_out(&start_fp, obst, start, o++, &amd64_registers[REG_RBP],
						   arch_register_req_type_none);
		} else {
			const arch_register_t *reg = &amd64_registers[i];
			arch_set_irn_register_req_out(start, o, reg->single_req);
			arch_set_irn_register_out(start, o, reg);
			++o;
		}
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
	panic("Unexpected Start Proj: %u\n", pn);
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
	ir_node  *block     = get_nodes_block(node);
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *new_block = be_transform_node(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *mem       = get_Return_mem(node);
	ir_node  *new_mem   = be_transform_node(mem);
	ir_node  *sp        = get_stack_pointer_for(node);
	size_t    n_res     = get_Return_n_ress(node);
	struct obstack *be_obst = be_get_be_obst(irg);
	amd64_cconv_t  *cconv   = current_cconv;

	/* estimate number of return values */
	size_t n_ins = 2 + n_res; /* memory + stackpointer, return values */
	size_t n_callee_saves
		= rbitset_popcount(cconv->callee_saves, N_AMD64_REGISTERS);
	n_ins += n_callee_saves;

	const arch_register_req_t **reqs
		= OALLOCN(be_obst, const arch_register_req_t*, n_ins);
	ir_node **in = ALLOCAN(ir_node*, n_ins);
	size_t    p  = 0;

	in[p]   = new_mem;
	reqs[p] = arch_no_register_req;
	++p;

	unsigned spp = p++;
	assert(spp == n_amd64_Return_stack);
	in[spp]   = sp;
	reqs[spp] = amd64_registers[REG_RSP].single_req;

	/* result values */
	for (size_t i = 0; i < n_res; ++i) {
		ir_node                  *res_value     = get_Return_res(node, i);
		ir_node                  *new_res_value = be_transform_node(res_value);
		const reg_or_stackslot_t *slot          = &current_cconv->results[i];
		in[p]   = new_res_value;
		reqs[p] = slot->req;
		++p;
	}
	/* callee saves */
	ir_node *start = get_irg_start(irg);
	unsigned start_pn = start_callee_saves_offset;
	for (size_t i = 0; i < N_AMD64_REGISTERS; ++i) {
		if (!rbitset_is_set(cconv->callee_saves, i))
			continue;
		const arch_register_t *reg   = &amd64_registers[i];
		ir_mode               *mode  = reg->reg_class->mode;
		ir_node               *value = new_r_Proj(start, mode, start_pn++);
		in[p]   = value;
		reqs[p] = reg->single_req;
		++p;
	}
	assert(p == n_ins);

	ir_node *returnn = new_bd_amd64_Return(dbgi, new_block, n_ins, in);
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
	struct obstack  *obst         = be_get_be_obst(irg);
	amd64_cconv_t   *cconv
		= amd64_decide_calling_convention(type, NULL);
	size_t           n_param_regs = cconv->n_param_regs;
	/* param-regs + mem + stackpointer + callee(2) + n_sse_regs */
	unsigned         max_inputs   = 5 + n_param_regs;
	ir_node        **in           = ALLOCAN(ir_node*, max_inputs);
	const arch_register_req_t **in_req
		= OALLOCNZ(obst, const arch_register_req_t*, max_inputs);
	int              in_arity     = 0;
	int              sync_arity   = 0;
	ir_node         *new_frame    = get_stack_pointer_for(node);

	assert(n_params == get_method_n_params(type));

	/* construct arguments */

	/* stack pointer input */
	/* construct an IncSP -> we have to always be sure that the stack is
	 * aligned even if we don't push arguments on it */
	const arch_register_t *sp_reg = &amd64_registers[REG_RSP];
	ir_node *incsp = create_IncSP(sp_reg, new_block, new_frame,
	                              cconv->param_stack_size, 1);

	/* match callee */
	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));
	addr.mem_input = NO_INPUT;
	amd64_op_mode_t op_mode;

	ir_node *mem_proj = NULL;

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
			mem_proj = be_get_Proj_for_pn(load, pn_Load_M);

			perform_address_matching(load_ptr, &in_arity, in, &addr);

			if (addr.base_input != NO_INPUT) {
				in_req[addr.base_input] = &amd64_requirement_gp;
			}

			if (addr.index_input != NO_INPUT) {
				in_req[addr.index_input] = &amd64_requirement_gp;
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
			in_req[input]      = &amd64_requirement_gp;
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
		ir_node *xmm_imm  = new_bd_amd64_MovImm(dbgi, block, INSN_MODE_32,
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
			store = new_bd_amd64_xStores(dbgi, new_block,
			                             ARRAY_SIZE(in), in, &attr);

			reqs  = xmm_reg_mem_reqs;
		} else {
			store = new_bd_amd64_Store(dbgi, new_block,
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

	/* outputs:
	 *  - memory
	 *  - results
	 *  - caller saves
	 */
	int n_caller_saves
		= rbitset_popcount(cconv->caller_saves, N_AMD64_REGISTERS);
	int out_arity = 1 + cconv->n_reg_results + n_caller_saves;

	/* create call node */
	ir_node *call = new_bd_amd64_Call(dbgi, new_block, in_arity, in, out_arity,
	                                  op_mode, addr);
	arch_set_irn_register_reqs_in(call, in_req);

	fix_node_mem_proj(call, mem_proj);

	/* create output register reqs */
	int o = 0;

	int memo = o++;
	arch_set_irn_register_req_out(call, memo, arch_no_register_req);
	assert(memo == pn_amd64_Call_mem);

	/* add register requirements for the result regs */
	assert(o == pn_amd64_Call_first_res);
	for (size_t r = 0; r < n_ress; ++r) {
		const reg_or_stackslot_t  *result_info = &cconv->results[r];
		const arch_register_req_t *req         = result_info->req;
		if (req != NULL) {
			arch_set_irn_register_req_out(call, o++, req);
		}
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
	incsp = create_IncSP(sp_reg, new_block, incsp, -cconv->param_stack_size, 0);
	/* if we are the last IncSP producer in a block then we have to keep
	 * the stack value.
	 * Note: This here keeps all producers which is more than necessary */
	add_irn_dep(incsp, call);
	keep_alive(incsp);

	pmap_insert(node_to_stack, node, incsp);

	amd64_free_calling_convention(cconv);
	return call;
}

static ir_node *gen_Proj_Call(ir_node *node)
{
	unsigned pn       = get_Proj_num(node);
	ir_node *call     = get_Proj_pred(node);
	ir_node *new_call = be_transform_node(call);
	switch ((pn_Call)pn) {
	case pn_Call_M:
		return new_r_Proj(new_call, mode_M, pn_amd64_Call_mem);
	case pn_Call_X_regular:
	case pn_Call_X_except:
	case pn_Call_T_result:
		break;
	}
	panic("Unexpected Call proj %+F\n", node);
}

static ir_node *gen_Proj_Proj_Call(ir_node *node)
{
	unsigned       pn       = get_Proj_num(node);
	ir_node       *call     = get_Proj_pred(get_Proj_pred(node));
	ir_node       *new_call = be_transform_node(call);
	ir_type       *tp       = get_Call_type(call);
	amd64_cconv_t *cconv    = amd64_decide_calling_convention(tp, NULL);
	const reg_or_stackslot_t *res    = &cconv->results[pn];
	ir_mode                  *mode   = get_irn_mode(node);
	unsigned                  new_pn = pn_amd64_Call_first_res+res->reg_offset;

	assert(res->req != NULL);
	if (mode_needs_gp_reg(mode))
		mode = mode_gp;
	else if (mode_is_float(mode))
		mode = mode_D;
	amd64_free_calling_convention(cconv);
	return new_r_Proj(new_call, mode, new_pn);
}

static ir_node *gen_Proj_Proj_Start(ir_node *node)
{
	ir_node *block     = get_nodes_block(node);
	ir_node *new_block = be_transform_node(block);
	unsigned pn        = get_Proj_num(node);
	ir_node *args      = get_Proj_pred(node);
	ir_node *start     = get_Proj_pred(args);
	ir_node *new_start = be_transform_node(start);

	assert(get_Proj_num(args) == pn_Start_T_args);

	const reg_or_stackslot_t *param = &current_cconv->parameters[pn];
	if (param->reg != NULL) {
		/* argument transmitted in register */
		const arch_register_t *reg    = param->reg;
		ir_mode               *mode   = reg->reg_class->mode;
		unsigned               new_pn = param->reg_offset + start_params_offset;
		ir_node               *value  = new_r_Proj(new_start, mode, new_pn);
		return value;
	} else {
		/* argument transmitted on stack */
		ir_graph *irg  = get_irn_irg(node);
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

		if (mode_is_float(mode)) {
			load  = new_bd_amd64_xMovs(NULL, new_block, ARRAY_SIZE(in),
			                           in, insn_mode, AMD64_OP_ADDR, addr);
			value = new_r_Proj(load, mode_D, pn_amd64_xMovs_res);
		} else if (get_mode_size_bits(mode) < 64 && mode_is_signed(mode)) {
			load  = new_bd_amd64_Movs(NULL, new_block, ARRAY_SIZE(in),
			                          in, insn_mode, AMD64_OP_ADDR, addr);
			value = new_r_Proj(load, mode_gp, pn_amd64_Movs_res);
		} else {
			load  = new_bd_amd64_Mov(NULL, new_block, ARRAY_SIZE(in),
			                         in, insn_mode, AMD64_OP_ADDR, addr);
			value = new_r_Proj(load, mode_gp, pn_amd64_Mov_res);
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


	amd64_args_t args;

	ir_node *new_node;
	if (mode_is_float(cmp_mode)) {
		match_binop(&args, block, cmp_mode, op1, op2, match_am);

		new_node = new_bd_amd64_xUcomis(dbgi, new_block, args.arity,
		                                args.in, &args.attr);
	} else {
		match_binop(&args, block, cmp_mode, op1, op2, match_immediate
		            | match_am);

		new_node = new_bd_amd64_Cmp(dbgi, new_block, args.arity,
		                            args.in, &args.attr);
	}

	arch_set_irn_register_reqs_in(new_node, args.reqs);

	fix_node_mem_proj(new_node, args.mem_proj);

	return new_r_Proj(new_node, mode_flags, pn_amd64_Cmp_flags);
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

	bool overflow_possible = true;
	if (is_Const(r) && is_Const_null(r))
		overflow_possible = false;

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
	ir_node  *const block = be_transform_node(get_nodes_block(node));
	return new_bd_amd64_Jcc(dbgi, block, flags, cc);
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

static ir_node *gen_Conv(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *op       = get_Conv_op(node);
	ir_mode  *src_mode = get_irn_mode(op);
	ir_mode  *dst_mode = get_irn_mode(node);
	dbg_info *dbgi     = get_irn_dbg_info(node);

	if (src_mode == dst_mode)
		return be_transform_node(op);

	bool src_float = mode_is_float(src_mode);
	bool dst_float = mode_is_float(dst_mode);
	bool is_gp     = !src_float && !dst_float;

	int src_bits = get_mode_size_bits(src_mode);
	int dst_bits = get_mode_size_bits(dst_mode);
	ir_mode *min_mode;

	if (src_bits < dst_bits) {
		min_mode = src_mode;
	} else if (src_bits > dst_bits) {
		min_mode = dst_mode;
	} else if ((src_float && dst_float) || is_gp){
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
		/* fp conv */
		if (src_bits < dst_bits) {
			conv = new_bd_amd64_CvtSS2SD(dbgi, block, ARRAY_SIZE(in),
			                             in, insn_mode, AMD64_OP_REG,
			                             addr);
			res  = new_r_Proj(conv, mode_D, pn_amd64_CvtSS2SD_res);
		} else {
			conv = new_bd_amd64_CvtSD2SS(dbgi, block, ARRAY_SIZE(in),
			                             in, insn_mode, AMD64_OP_REG,
			                             addr);
			res  = new_r_Proj(conv, mode_F, pn_amd64_CvtSD2SS_res);
		}
		reqs = xmm_reqs;

	} else if (src_float && !dst_float) {
		/* fp to integer */

		if (!mode_is_signed(dst_mode) && dst_bits <= 32) {
			/* The conversion is signed only; simply use 64-bit register*/
			insn_mode = INSN_MODE_64;
		} else if (!mode_is_signed(dst_mode) && dst_bits == 64) {
			panic("Cannot convert floating point to 64-bit unsigned\n");
		}

		if (src_bits < 64) {
			conv = new_bd_amd64_CvtSS2SI(dbgi, block, ARRAY_SIZE(in),
			                             in, insn_mode, AMD64_OP_REG,
			                             addr);
		} else {
			conv = new_bd_amd64_CvtSD2SI(dbgi, block, ARRAY_SIZE(in),
			                             in, insn_mode, AMD64_OP_REG,
			                             addr);
		}
		assert((unsigned)pn_amd64_CvtSS2SI_res == (unsigned)pn_amd64_CvtSD2SI_res);
		res  = new_r_Proj(conv, mode_gp, pn_amd64_CvtSS2SI_res);
		reqs = xmm_reqs;

	} else if (!src_float && dst_float) {
		/* integer to fp */

		if (!mode_is_signed(src_mode) && src_bits <= 32) {
			/* Conversion is signed only, therefore use up to 64-bit register
			 * size and require that the upper bits are zero. This is done with
			 * an explicit move instruction */

			insn_mode = INSN_MODE_64;
			amd64_insn_mode_t move_mode = get_insn_mode_from_mode(src_mode);

			ir_node *ext = new_bd_amd64_Mov(dbgi, block, ARRAY_SIZE(in),
			                                in, move_mode,
			                                AMD64_OP_REG, addr);
			arch_set_irn_register_reqs_in(ext, reg_reqs);
			in[0] = new_r_Proj(ext, mode_gp, pn_amd64_Mov_res);

		} else if (!mode_is_signed(src_mode) && src_bits == 64) {
			panic("Cannot convert 64-bit unsigned to floating point\n");
		}

		if (dst_bits < 64) {
			conv = new_bd_amd64_CvtSI2SS(dbgi, block, ARRAY_SIZE(in),
			                             in, insn_mode, AMD64_OP_REG,
			                             addr);

			res = new_r_Proj(conv, mode_F, pn_amd64_CvtSI2SS_res);
		} else {
			conv = new_bd_amd64_CvtSI2SD(dbgi, block, ARRAY_SIZE(in),
			                             in, insn_mode, AMD64_OP_REG,
			                             addr);

			res = new_r_Proj(conv, mode_D, pn_amd64_CvtSI2SD_res);
		}
		reqs = reg_reqs;

	} else {
		/* integer conv */
		if (!mode_is_signed(min_mode)
		    || get_mode_size_bits(min_mode) == 64) {
			conv = new_bd_amd64_Mov(dbgi, block, ARRAY_SIZE(in),
			                        in, insn_mode,
			                        AMD64_OP_REG, addr);
			res = new_r_Proj(conv, mode_gp, pn_amd64_Mov_res);
		} else {
			conv = new_bd_amd64_Movs(dbgi, block, ARRAY_SIZE(in),
			                         in, insn_mode,
			                         AMD64_OP_REG, addr);
			res = new_r_Proj(conv, mode_gp, pn_amd64_Movs_res);
		}
		reqs = reg_reqs;
	}

	arch_set_irn_register_reqs_in(conv, reqs);
	return res;
}

static ir_node *gen_Store(ir_node *node)
{
	dbg_info *dbgi = get_irn_dbg_info(node);
	ir_node *block = be_transform_node(get_nodes_block(node));
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

	ir_node *mem     = get_Store_mem(node);
	ir_node *new_mem = be_transform_node(mem);
	in[arity++]      = new_mem;
	assert((size_t)arity <= ARRAY_SIZE(in));
	attr.base.insn_mode = get_insn_mode_from_mode(mode);

	const arch_register_req_t **reqs;

	bool need_xmm = mode_is_float(mode);

	switch (arity) {
	case 1: reqs = mem_reqs; break;
	case 2: reqs = need_xmm ? xmm_mem_reqs : reg_mem_reqs; break;
	case 3: reqs = need_xmm ? xmm_reg_mem_reqs : reg_reg_mem_reqs; break;
	case 4: reqs = need_xmm ? xmm_reg_reg_mem_reqs : reg_reg_reg_mem_reqs;
		break;
	default:
		panic("Unexpected arity %d while transforming node %+F", arity, node);
	}


	ir_node *new_store;
	if (mode_is_float(mode)) {
		new_store = new_bd_amd64_xStores(dbgi, block, arity, in, &attr);
	} else {
		new_store = new_bd_amd64_Store(dbgi, block, arity, in, &attr);
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

	if (mode_is_float(mode)) {
		store = new_bd_amd64_xStores(NULL, block, ARRAY_SIZE(in), in, &attr);
		reqs  = xmm_reg_mem_reqs;
	} else {
		store = new_bd_amd64_Store(NULL, block, ARRAY_SIZE(in), in, &attr);
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

	ir_node *load;
	if (mode_is_float(mode)) {
		load = new_bd_amd64_xMovs(NULL, block, ARRAY_SIZE(in), in,
		                          INSN_MODE_64, AMD64_OP_ADDR, addr);
	} else {
		load = new_bd_amd64_Mov(NULL, block, ARRAY_SIZE(in), in,
	                            INSN_MODE_64, AMD64_OP_ADDR, addr);
	}
	arch_set_irn_register_reqs_in(load, reg_mem_reqs);
	arch_add_irn_flags(load, arch_irn_flag_reload);
	sched_add_before(before, load);
	amd64_addr_attr_t *attr = get_amd64_addr_attr(load);
	attr->needs_frame_ent = true;
	ir_node *res = new_r_Proj(load, mode, pn_amd64_Mov_res);
	return res;
}

static ir_node *gen_Load(ir_node *node)
{

	dbg_info *dbgi = get_irn_dbg_info(node);
	ir_node *block = be_transform_node(get_nodes_block(node));
	ir_mode *mode  = get_Load_mode(node);

	ir_node *ptr = get_Load_ptr(node);

	int arity = 0;
	ir_node *in[3];
	amd64_addr_t addr;
	memset(&addr, 0, sizeof(addr));

	perform_address_matching(ptr, &arity, in, &addr);

	const arch_register_req_t **reqs = mem_reqs;
	if (addr.base_input != NO_INPUT && addr.index_input != NO_INPUT) {
		reqs = reg_reg_mem_reqs;
	} else if (addr.base_input != NO_INPUT || addr.index_input != NO_INPUT) {
		reqs = reg_mem_reqs;
	}

	ir_node *mem     = get_Load_mem(node);
	ir_node *new_mem = be_transform_node(mem);
	in[arity++]      = new_mem;
	assert((size_t)arity <= ARRAY_SIZE(in));

	amd64_insn_mode_t insn_mode = get_insn_mode_from_mode(mode);
	ir_node  *new_load;

	if (mode_is_float(mode)) {
		new_load = new_bd_amd64_xMovs(dbgi, block, arity, in,
		                             insn_mode, AMD64_OP_ADDR, addr);
	} else if (get_mode_size_bits(mode) < 64 && mode_is_signed(mode)) {
		new_load = new_bd_amd64_Movs(dbgi, block, arity, in,
		                             insn_mode, AMD64_OP_ADDR, addr);
	} else {
		new_load = new_bd_amd64_Mov(dbgi, block, arity, in,
		                            insn_mode, AMD64_OP_ADDR, addr);
	}

	arch_set_irn_register_reqs_in(new_load, reqs);
	set_irn_pinned(new_load, get_irn_pinned(node));

	return new_load;
}

static ir_node *gen_Unknown(ir_node *node)
{
	/* for now, there should be more efficient ways to do this */
	ir_node *block = be_transform_node(get_nodes_block(node));

	if (mode_is_float(get_irn_mode(node))) {
		return new_bd_amd64_xXorp0(NULL, block);
	} else {
		return new_bd_amd64_Xor0(NULL, block);
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
	case iro_amd64_xMovs:
		if (pn == pn_Load_res) {
			return new_rd_Proj(dbgi, new_load, mode_D, pn_amd64_xMovs_res);
		} else if (pn == pn_Load_M) {
			return new_rd_Proj(dbgi, new_load, mode_M, pn_amd64_xMovs_M);
		}
		break;
	case iro_amd64_Movs:
	case iro_amd64_Mov:
		assert((int)pn_amd64_Movs_res == (int)pn_amd64_Mov_res);
		assert((int)pn_amd64_Movs_M   == (int)pn_amd64_Mov_M);
		/* handle all gp loads equal: they have the same proj numbers. */
		if (pn == pn_Load_res) {
			return new_rd_Proj(dbgi, new_load, mode_Lu, pn_amd64_Movs_res);
		} else if (pn == pn_Load_M) {
			return new_rd_Proj(dbgi, new_load, mode_M, pn_amd64_Movs_M);
		}
		break;
	case iro_amd64_Add:
	case iro_amd64_And:
	case iro_amd64_Cmp:
		assert(pn == pn_Load_M);
		return new_r_Proj(new_load, mode_M, pn_amd64_mem);
	default:
		panic("Unsupported Proj from Load");
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
		panic("Unsupported Proj from Store");
	}
}

static ir_node *gen_saturating_increment(ir_node *node)
{
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *new_block = be_transform_node(get_nodes_block(node));
	ir_node  *param0    = get_Builtin_param(node, 0);
	ir_node  *operand   = be_transform_node(param0);
	ir_mode  *mode      = get_irn_mode(param0);

	amd64_binop_addr_attr_t inc_attr;
	memset(&inc_attr, 0, sizeof(inc_attr));
	inc_attr.base.base.op_mode  = AMD64_OP_REG_IMM;
	inc_attr.u.immediate.offset = 1;
	inc_attr.base.insn_mode     = get_insn_mode_from_mode(mode);
	ir_node  *inc_in[]          = { operand };

	ir_node  *inc = new_bd_amd64_Add(dbgi, new_block, ARRAY_SIZE(inc_in),
	                                 inc_in, &inc_attr);

	arch_set_irn_register_reqs_in(inc, reg_reqs);
	arch_set_irn_register_req_out(inc, 0, &amd64_requirement_gp_same_0);
	set_irn_mode(inc, mode_T);

	ir_node *value  = new_rd_Proj(dbgi, inc, mode_gp, pn_amd64_Add_res);
	ir_node *eflags = new_rd_Proj(dbgi, inc, mode_flags, pn_amd64_Add_flags);

	amd64_binop_addr_attr_t sbb_attr;
	memset(&sbb_attr, 0, sizeof(sbb_attr));
	sbb_attr.base.base.op_mode  = AMD64_OP_REG_IMM;
	sbb_attr.u.immediate.offset = 0;
	sbb_attr.base.insn_mode     = get_insn_mode_from_mode(mode);
	ir_node *in[2]              = { value, eflags };

	ir_node *sbb = new_bd_amd64_Sbb(dbgi, new_block, ARRAY_SIZE(in), in,
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
		return new_r_Proj(new_node, mode_gp, pn_amd64_Sbb_res);
	default:
		break;
	}
	panic("Builtin %s not implemented", get_builtin_kind_name(kind));
}

static ir_node *gen_Bitcast(ir_node *node)
{
	(void)node;
	panic("Bitcast NIY\n");
}

/* Boilerplate code for transformation: */

static void amd64_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_Add,      gen_Add);
	be_set_transform_function(op_Address,  gen_Address);
	be_set_transform_function(op_And,      gen_And);
	be_set_transform_function(op_Bitcast,  gen_Bitcast);
	be_set_transform_function(op_Builtin,  gen_Builtin);
	be_set_transform_function(op_Call,     gen_Call);
	be_set_transform_function(op_Cmp,      gen_Cmp);
	be_set_transform_function(op_Cond,     gen_Cond);
	be_set_transform_function(op_Const,    gen_Const);
	be_set_transform_function(op_Conv,     gen_Conv);
	be_set_transform_function(op_Div,      gen_Div);
	be_set_transform_function(op_Eor,      gen_Eor);
	be_set_transform_function(op_IJmp,     gen_IJmp);
	be_set_transform_function(op_Jmp,      gen_Jmp);
	be_set_transform_function(op_Load,     gen_Load);
	be_set_transform_function(op_Member,   gen_Member);
	be_set_transform_function(op_Minus,    gen_Minus);
	be_set_transform_function(op_Mod,      gen_Mod);
	be_set_transform_function(op_Mul,      gen_Mul);
	be_set_transform_function(op_Mulh,     gen_Mulh);
	be_set_transform_function(op_Not,      gen_Not);
	be_set_transform_function(op_Or,       gen_Or);
	be_set_transform_function(op_Phi,      gen_Phi);
	be_set_transform_function(op_Return,   gen_Return);
	be_set_transform_function(op_Shl,      gen_Shl);
	be_set_transform_function(op_Shr,      gen_Shr);
	be_set_transform_function(op_Shrs,     gen_Shrs);
	be_set_transform_function(op_Start,    gen_Start);
	be_set_transform_function(op_Store,    gen_Store);
	be_set_transform_function(op_Sub,      gen_Sub);
	be_set_transform_function(op_Switch,   gen_Switch);
	be_set_transform_function(op_Unknown,  gen_Unknown);

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

static void amd64_create_stacklayout(ir_graph *irg, amd64_cconv_t *cconv)
{
	ir_entity         *entity        = get_irg_entity(irg);
	ir_type           *function_type = get_entity_type(entity);
	be_stack_layout_t *layout        = be_get_irg_stack_layout(irg);

	/* construct argument type */
	assert(cconv != NULL);
	ident   *arg_type_id = new_id_from_str("arg_type");
	ident   *arg_id      = id_mangle_u(get_entity_ident(entity), arg_type_id);
	ir_type *arg_type    = new_type_struct(arg_id);
	size_t n_params = get_method_n_params(function_type);
	for (size_t p = 0; p < n_params; ++p) {
		reg_or_stackslot_t *param = &cconv->parameters[p];
		if (param->type == NULL)
			continue;

		char buf[128];
		snprintf(buf, sizeof(buf), "param_%u", (unsigned)p);
		ident *id     = new_id_from_str(buf);
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
	start_sp.irn  = NULL;
	start_fp.irn  = NULL;

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
	amd64_free_calling_convention(current_cconv);
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
