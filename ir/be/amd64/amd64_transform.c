/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   code selection (transform FIRM into amd64 FIRM)
 */
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "ircons.h"
#include "iropt_t.h"
#include "error.h"
#include "debug.h"
#include "tv_t.h"
#include "util.h"

#include "benode.h"
#include "betranshlp.h"
#include "beutil.h"
#include "bearch_amd64_t.h"
#include "beirg.h"
#include "beabihelper.h"
#include "besched.h"

#include "amd64_nodes_attr.h"
#include "amd64_transform.h"
#include "amd64_new_nodes.h"
#include "amd64_cconv.h"

#include "gen_amd64_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

typedef struct reg_info_t {
	size_t   offset;
	ir_node *irn;
} reg_info_t;

static ir_mode         *mode_gp;
static amd64_cconv_t   *current_cconv = NULL;
static reg_info_t       start_mem;
static reg_info_t       start_sp;
static reg_info_t       start_fp;
static size_t           start_callee_saves_offset;
static size_t           start_params_offset;
static pmap            *node_to_stack;
static be_stackorder_t *stackorder;

static const arch_register_req_t amd64_requirement_gp = {
	arch_register_req_type_normal,
	&amd64_reg_classes[CLASS_amd64_gp],
	NULL,
	0,
	0,
	1
};

static const arch_register_req_t *am_load_reqs[] = {
	&arch_no_requirement,
};

static const arch_register_req_t *am_load_base_reqs[] = {
	&amd64_requirement_gp,
	&arch_no_requirement,
};

static const arch_register_req_t *am_store_base_reqs[] = {
	&amd64_requirement_gp,
	&amd64_requirement_gp,
	&arch_no_requirement,
};

static inline int mode_needs_gp_reg(ir_mode *mode)
{
	return mode_is_int(mode) || mode_is_reference(mode);
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
	if (!mode_needs_gp_reg(mode))
		panic("amd64: float constant not supported yet");
	ir_tarval *tv = get_Const_tarval(node);
	uint64_t val = get_tarval_uint64(tv);
	amd64_insn_mode_t imode = val > UINT32_MAX ? INSN_MODE_64 : INSN_MODE_32;
	return new_bd_amd64_Const(dbgi, block, imode, val, NULL);
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

static ir_node *gen_SymConst(ir_node *node)
{
	ir_node   *block  = be_transform_node(get_nodes_block(node));
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_entity *entity = get_SymConst_entity(node);

	/* do we need RIP-relative addressing because of PIC? */
	reference_mode_t mode = need_relative_addressing(entity);
	if (mode == REFERENCE_DIRECT) {
		return new_bd_amd64_Const(dbgi, block, INSN_MODE_64, 0, entity);
	}

	amd64_am_info_t am;
	memset(&am, 0, sizeof(am));
	am.base_input = RIP_INPUT;
	am.index_input = NO_INPUT;
	if (mode == REFERENCE_IP_RELATIVE) {
		am.symconst = entity;
		ir_node *lea
			= new_bd_amd64_Lea(dbgi, block, 0, NULL, INSN_MODE_64, am);
		return lea;
	} else {
		assert(mode == REFERENCE_GOT);
		am.symconst = new_got_entry_entity(entity);
		ir_graph *irg  = get_irn_irg(node);
		ir_node  *in[] = { get_irg_no_mem(irg) };
		ir_node *load
			= new_bd_amd64_LoadZ(dbgi, block, ARRAY_SIZE(in), in,
								 INSN_MODE_64, am);
		arch_set_irn_register_reqs_in(load, am_load_reqs);
		return new_r_Proj(load, mode_gp, pn_amd64_LoadZ_res);
	}
}

typedef ir_node* (*binop_constructor)(dbg_info *dbgi, ir_node *block,
		ir_node *left, ir_node *right, amd64_insn_mode_t insn_mode);

static ir_node *gen_binop(ir_node *const node, binop_constructor const new_node)
{
	dbg_info *const dbgi    = get_irn_dbg_info(node);
	ir_node  *const block   = be_transform_node(get_nodes_block(node));
	ir_node  *const op1     = get_binop_left(node);
	ir_node  *const new_op1 = be_transform_node(op1);
	ir_node  *const op2     = get_binop_right(node);
	ir_node  *const new_op2 = be_transform_node(op2);
	ir_mode  *const mode    = get_irn_mode(node);
	amd64_insn_mode_t imode
		= get_mode_size_bits(mode) > 32 ? INSN_MODE_64 : INSN_MODE_32;
	return new_node(dbgi, block, new_op1, new_op2, imode);
}

static ir_node *gen_Add (ir_node *const node) { return gen_binop(node, &new_bd_amd64_Add);  }
static ir_node *gen_And (ir_node *const node) { return gen_binop(node, &new_bd_amd64_And);  }
static ir_node *gen_Eor (ir_node *const node) { return gen_binop(node, &new_bd_amd64_Xor);  }
static ir_node *gen_Or  (ir_node *const node) { return gen_binop(node, &new_bd_amd64_Or);   }
static ir_node *gen_Mul (ir_node *const node) { return gen_binop(node, &new_bd_amd64_IMul); }
static ir_node *gen_Shl (ir_node *const node) { return gen_binop(node, &new_bd_amd64_Shl);  }
static ir_node *gen_Shr (ir_node *const node) { return gen_binop(node, &new_bd_amd64_Shr);  }
static ir_node *gen_Shrs(ir_node *const node) { return gen_binop(node, &new_bd_amd64_Sar);  }
static ir_node *gen_Sub (ir_node *const node) { return gen_binop(node, &new_bd_amd64_Sub);  }

static ir_node *create_div(ir_node *const node, ir_mode *const mode,
                           ir_node *const op1, ir_node *const op2,
                           ir_node *const mem)
{
	dbg_info *const dbgi    = get_irn_dbg_info(node);
	ir_node  *const block   = be_transform_node(get_nodes_block(node));
	ir_node  *const new_op1 = be_transform_node(op1);
	ir_node  *const new_op2 = be_transform_node(op2);
	ir_node  *const new_mem = be_transform_node(mem);
	amd64_insn_mode_t insn_mode
		= get_mode_size_bits(mode) > 32 ? INSN_MODE_64 : INSN_MODE_32;
	ir_node *res;
	if (mode_is_signed(mode)) {
		/* TODO: use immediates... */
		int      const sval  = get_mode_size_bits(mode)-1;
		ir_node *const cnst  = new_bd_amd64_Const(dbgi, block, INSN_MODE_32,
		                                          sval, NULL);
		ir_node *const upper = new_bd_amd64_Sar(dbgi, block, new_op1, cnst,
		                                        insn_mode);
		res = new_bd_amd64_IDiv(dbgi, block, upper, new_op1, new_op2, new_mem,
		                        insn_mode);
	} else {
		ir_node *const zero = new_bd_amd64_Xor0(dbgi, block);
		res = new_bd_amd64_Div(dbgi, block, zero, new_op1, new_op2, new_mem,
		                       insn_mode);
	}
	return res;
}

static ir_node *gen_Div(ir_node *const node)
{
	ir_mode *const mode = get_Div_resmode(node);
	if (!mode_needs_gp_reg(mode))
		panic("amd64: float div NIY");
	ir_node *const op1 = get_Div_left(node);
	ir_node *const op2 = get_Div_right(node);
	ir_node *const mem = get_Div_mem(node);
	return create_div(node, mode, op1, op2, mem);
}

static ir_node *gen_Proj_Div(ir_node *const node)
{
	ir_node *pred     = get_Proj_pred(node);
	ir_node *new_pred = be_transform_node(pred);
	long     pn       = get_Proj_proj(node);

	assert((long)pn_amd64_Div_M == (long)pn_amd64_IDiv_M);
	assert((long)pn_amd64_Div_res_div == (long)pn_amd64_IDiv_res_div);
	switch((pn_Div)pn) {
	case pn_Div_M:
		return new_r_Proj(new_pred, mode_M, pn_amd64_Div_M);
	case pn_Div_res:
		return new_r_Proj(new_pred, mode_gp, pn_amd64_Div_res_div);
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
	long     pn       = get_Proj_proj(node);

	assert((long)pn_amd64_Div_M == (long)pn_amd64_IDiv_M);
	assert((long)pn_amd64_Div_res_mod == (long)pn_amd64_IDiv_res_mod);
	switch((pn_Mod)pn) {
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

static ir_node *gen_Minus(ir_node *const node)
{
	return gen_unop(node, n_Minus_op, &new_bd_amd64_Neg);
}
static ir_node *gen_Not(ir_node *const node)
{
	return gen_unop(node, n_Not_op, &new_bd_amd64_Not);
}

static ir_node *gen_Sel(ir_node *const node)
{
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	dbg_info *dbgi      = get_irn_dbg_info(node);
	ir_node  *ptr       = get_Sel_ptr(node);
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *base      = get_frame_base(irg);
	if (!is_Proj(ptr) || !is_Start(get_Proj_pred(ptr)))
		panic("Sel not lowered");
	if (get_Sel_n_indexs(node) > 0)
		panic("array Sel not lowered %+F", node);
	ir_entity *entity = get_Sel_entity(node);
	return new_bd_amd64_FrameAddr(dbgi, new_block, base, entity);
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

	ir_type   *const utype  = get_unknown_type();
	ir_entity *const entity = new_entity(utype, id_unique("TBL%u"), utype);
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
	if (!cconv->omit_fp)
		++n_outs; /* rbp */
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

	if (!cconv->omit_fp) {
		make_start_out(&start_fp, obst, start, o++, &amd64_registers[REG_RBP],
		               arch_register_req_type_none);
	}

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
		const arch_register_t *reg = &amd64_registers[i];
		arch_set_irn_register_req_out(start, o, reg->single_req);
		arch_set_irn_register_out(start, o, reg);
		++o;
	}
	assert(n_outs == o);

	return start;
}

static ir_node *gen_Proj_Start(ir_node *node)
{
	ir_graph *irg       = get_irn_irg(node);
	ir_node  *block     = get_nodes_block(node);
	ir_node  *new_block = be_transform_node(block);
	long      pn        = get_Proj_proj(node);
	be_transform_node(get_Proj_pred(node));

	switch ((pn_Start)pn) {
	case pn_Start_X_initial_exec:
		return new_bd_amd64_Jmp(NULL, new_block);
	case pn_Start_M:
		return get_initial_mem(irg);
	case pn_Start_T_args:
		return new_r_Bad(irg, mode_T);
	case pn_Start_P_frame_base:
		return get_frame_base(irg);
	}
	panic("Unexpected Start Proj: %ld\n", pn);
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

	in[p]   = sp;
	reqs[p] = amd64_registers[REG_RSP].single_req;
	++p;

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
	long start_pn = start_callee_saves_offset;
	for (size_t i = 0; i < N_AMD64_REGISTERS; ++i) {
		if (!rbitset_is_set(cconv->callee_saves, i))
			continue;
		const arch_register_t *reg    = &amd64_registers[i];
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

static amd64_insn_mode_t get_insn_mode_from_mode(const ir_mode *mode)
{
	switch (get_mode_size_bits(mode)) {
	case  8: return INSN_MODE_8;
	case 16: return INSN_MODE_16;
	case 32: return INSN_MODE_32;
	case 64: return INSN_MODE_64;
	}
	panic("unexpected mode");
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
	ir_node        **sync_ins     = ALLOCAN(ir_node*, n_params);
	ir_graph        *irg          = get_irn_irg(node);
	struct obstack  *obst         = be_get_be_obst(irg);
	amd64_cconv_t   *cconv
		= amd64_decide_calling_convention(type, NULL);
	size_t           n_param_regs = cconv->n_param_regs;
	/* param-regs + mem + stackpointer + callee + n_sse_regs */
	unsigned         max_inputs   = 4 + n_param_regs;
	ir_node        **in           = ALLOCAN(ir_node*, max_inputs);
	const arch_register_req_t **in_req
		= OALLOCNZ(obst, const arch_register_req_t*, max_inputs);
	int              in_arity     = 0;
	int              sync_arity   = 0;
	ir_node         *new_frame    = get_stack_pointer_for(node);

	assert(n_params == get_method_n_params(type));

	/* construct arguments */

	/* memory input */
	in_req[in_arity] = arch_no_register_req;
	int mem_pos      = in_arity;
	++in_arity;

	/* stack pointer input */
	/* construct an IncSP -> we have to always be sure that the stack is
	 * aligned even if we don't push arguments on it */
	const arch_register_t *sp_reg = &amd64_registers[REG_RSP];
	ir_node *incsp = be_new_IncSP(sp_reg, new_block, new_frame,
	                              cconv->param_stack_size, 1);
	in_req[in_arity] = sp_reg->single_req;
	in[in_arity]     = incsp;
	++in_arity;

	/* vararg calls need the number of SSE registers used */
	if (get_method_variadicity(type) == variadicity_variadic) {
		ir_node *zero = new_bd_amd64_Xor0(NULL, block);
		in_req[in_arity] = amd64_registers[REG_RAX].single_req;
		in[in_arity] = zero;
		++in_arity;
	}

	/* parameters */
	for (size_t p = 0; p < n_params; ++p) {
		ir_node                  *value      = get_Call_param(node, p);
		const reg_or_stackslot_t *param      = &cconv->parameters[p];
		ir_type                  *param_type = get_method_param_type(type, p);
		ir_mode                  *mode       = get_type_mode(param_type);
		assert(mode_needs_gp_reg(mode));

		ir_node *new_value = be_transform_node(value);

		/* put value into registers */
		if (param->reg != NULL) {
			in[in_arity]     = new_value;
			in_req[in_arity] = param->reg->single_req;
			++in_arity;
			continue;
		}
		/* we need a store if we're here */
		mode = mode_gp;
		amd64_insn_mode_t imode = get_insn_mode_from_mode(mode);
		amd64_am_info_t am;
		memset(&am, 0, sizeof(am));
		am.base_input  = 1;
		am.index_input = NO_INPUT;
		ir_node *in[] = { new_value, incsp, new_mem };
		ir_node *store = new_bd_amd64_Store(dbgi, new_block, ARRAY_SIZE(in), in,
		                                    imode, am);
		set_irn_pinned(store, op_pin_state_floats);
		sync_ins[sync_arity++] = store;
	}

	/* construct memory input */
	if (sync_arity == 0) {
		in[mem_pos] = new_mem;
	} else if (sync_arity == 1) {
		in[mem_pos] = sync_ins[0];
	} else {
		in[mem_pos] = new_rd_Sync(NULL, new_block, sync_arity, sync_ins);
	}

	in[in_arity]     = be_transform_node(callee);
	in_req[in_arity] = amd64_reg_classes[CLASS_amd64_gp].class_req;
	++in_arity;
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
	ir_node *call = new_bd_amd64_Call(dbgi, new_block, in_arity, in, out_arity);
	arch_set_irn_register_reqs_in(call, in_req);

	/* create output register reqs */
	int o = 0;
	arch_set_irn_register_req_out(call, o++, arch_no_register_req);
	/* add register requirements for the result regs */
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
	incsp = be_new_IncSP(sp_reg, new_block, incsp, -cconv->param_stack_size, 0);
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
	long     pn       = get_Proj_proj(node);
	ir_node *call     = get_Proj_pred(node);
	ir_node *new_call = be_transform_node(call);
	switch ((pn_Call)pn) {
	case pn_Call_M:
		return new_r_Proj(new_call, mode_M, 0);
	case pn_Call_X_regular:
	case pn_Call_X_except:
	case pn_Call_T_result:
		break;
	}
	panic("Unexpected Call proj %+F\n", node);
}

static ir_node *gen_Proj_Proj_Call(ir_node *node)
{
	long           pn       = get_Proj_proj(node);
	ir_node       *call     = get_Proj_pred(get_Proj_pred(node));
	ir_node       *new_call = be_transform_node(call);
	ir_type       *tp       = get_Call_type(call);
	amd64_cconv_t *cconv    = amd64_decide_calling_convention(tp, NULL);
	const reg_or_stackslot_t *res  = &cconv->results[pn];
	ir_mode                  *mode = get_irn_mode(node);
	long                      new_pn = 1 + res->reg_offset;

	assert(res->req != NULL);
	if (mode_needs_gp_reg(mode))
		mode = mode_gp;
	amd64_free_calling_convention(cconv);
	return new_r_Proj(new_call, mode, new_pn);
}

static ir_node *gen_Proj_Proj_Start(ir_node *node)
{
	ir_node *block     = get_nodes_block(node);
	ir_node *new_block = be_transform_node(block);
	long     pn        = get_Proj_proj(node);
	ir_node *args      = get_Proj_pred(node);
	ir_node *start     = get_Proj_pred(args);
	ir_node *new_start = be_transform_node(start);

	assert(get_Proj_proj(args) == pn_Start_T_args);

	const reg_or_stackslot_t *param = &current_cconv->parameters[pn];
	if (param->reg != NULL) {
		/* argument transmitted in register */
		const arch_register_t *reg    = param->reg;
		ir_mode               *mode   = reg->reg_class->mode;
		long                   new_pn = param->reg_offset + start_params_offset;
		ir_node               *value  = new_r_Proj(new_start, mode, new_pn);
		return value;
	} else {
		/* argument transmitted on stack */
		ir_graph *irg  = get_irn_irg(node);
		ir_node  *mem  = get_initial_mem(irg);
		ir_mode  *mode = get_type_mode(param->type);
		ir_node  *base = get_frame_base(irg);

		amd64_insn_mode_t imode = get_insn_mode_from_mode(mode);
		/* TODO: use the AM form for the address calculation */
		amd64_am_info_t am;
		memset(&am, 0, sizeof(am));
		am.base_input = 0;
		am.mem_input  = 1;
		am.symconst   = param->entity;
		ir_node *in[] = { base, mem };
		ir_node *load;
		ir_node *value;
		if (get_mode_size_bits(mode) < 64 && mode_is_signed(mode)) {
			load  = new_bd_amd64_LoadS(NULL, new_block, ARRAY_SIZE(in),
			                           in, imode, am);
			value = new_r_Proj(load, mode_gp, pn_amd64_LoadS_res);
		} else {
			load  = new_bd_amd64_LoadZ(NULL, new_block, ARRAY_SIZE(in),
			                           in, imode, am);
			value = new_r_Proj(load, mode_gp, pn_amd64_LoadZ_res);
		}
		arch_set_irn_register_reqs_in(load, am_load_base_reqs);
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

static bool needs_extension(ir_node *op)
{
	ir_mode *mode = get_irn_mode(op);
	if (get_mode_size_bits(mode) >= 32)
		return false;
	return !be_upper_bits_clean(op, mode);
}

static ir_node *gen_Cmp(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *op1      = get_Cmp_left(node);
	ir_node  *op2      = get_Cmp_right(node);
	ir_mode  *cmp_mode = get_irn_mode(op1);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node  *new_op1;
	ir_node  *new_op2;
	bool      is_unsigned;

	if (mode_is_float(cmp_mode)) {
		panic("Floating point not implemented yet!");
	}

	amd64_insn_mode_t insn_mode
		= get_mode_size_bits(cmp_mode) > 32 ? INSN_MODE_64 : INSN_MODE_32;

	assert(get_irn_mode(op2) == cmp_mode);
	is_unsigned = !mode_is_signed(cmp_mode);

	new_op1 = be_transform_node(op1);
	if (needs_extension(op1))
		new_op1 = new_bd_amd64_Conv(dbgi, block, new_op1, cmp_mode);
	new_op2 = be_transform_node(op2);
	if (needs_extension(op2))
		new_op2 = new_bd_amd64_Conv(dbgi, block, new_op2, cmp_mode);
	return new_bd_amd64_Cmp(dbgi, block, new_op1, new_op2, insn_mode, false,
	                        is_unsigned);
}

static ir_node *gen_Cond(ir_node *node)
{
	ir_node    *const block     = be_transform_node(get_nodes_block(node));
	dbg_info   *const dbgi      = get_irn_dbg_info(node);
	ir_node    *const selector  = get_Cond_selector(node);
	ir_node    *const flag_node = be_transform_node(selector);
	ir_relation const relation  = get_Cmp_relation(selector);
	return new_bd_amd64_Jcc(dbgi, block, flag_node, relation);
}

static ir_node *gen_Phi(ir_node *node)
{
	ir_mode                   *mode = get_irn_mode(node);
	const arch_register_req_t *req;
	if (mode_needs_gp_reg(mode)) {
		/* all integer operations are on 64bit registers now */
		req  = amd64_reg_classes[CLASS_amd64_gp].class_req;
	} else {
		req = arch_no_register_req;
	}

	return be_transform_phi(node, req);
}

static ir_node *gen_Conv(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *op       = get_Conv_op(node);
	ir_node  *new_op   = be_transform_node(op);
	ir_mode  *src_mode = get_irn_mode(op);
	ir_mode  *dst_mode = get_irn_mode(node);
	dbg_info *dbgi     = get_irn_dbg_info(node);

	if (src_mode == dst_mode)
		return new_op;

	if (mode_is_float(src_mode) || mode_is_float(dst_mode)) {
		panic("float not supported yet");
	} else { /* complete in gp registers */
		int src_bits = get_mode_size_bits(src_mode);
		int dst_bits = get_mode_size_bits(dst_mode);
		ir_mode *min_mode;

		if (src_bits == dst_bits) {
			/* kill unnecessary conv */
			return new_op;
		}

		if (src_bits < dst_bits) {
			min_mode = src_mode;
		} else {
			min_mode = dst_mode;
		}

		ir_node *res = new_bd_amd64_Conv(dbgi, block, new_op, min_mode);
		if (!mode_is_signed(min_mode) && get_mode_size_bits(min_mode) == 32) {
			amd64_attr_t *const attr = get_amd64_attr(res);
			attr->data.insn_mode = INSN_MODE_32;
		}

		return res;
	}
}

static ir_node *gen_Store(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *ptr      = get_Store_ptr(node);
	ir_node  *new_ptr  = be_transform_node(ptr);
	ir_node  *mem      = get_Store_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	ir_node  *val      = get_Store_value(node);
	ir_node  *new_val  = be_transform_node(val);
	ir_mode  *mode     = get_irn_mode(val);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node *new_store;

	if (mode_is_float(mode)) {
		panic("Float not supported yet");
	} else {
		assert(mode_needs_gp_reg(mode) && "unsupported mode for Store");
		amd64_insn_mode_t insn_mode = get_insn_mode_from_mode(mode);
		amd64_am_info_t am;
		memset(&am, 0, sizeof(am));
		am.base_input  = 1;
		am.index_input = NO_INPUT;
		ir_node *in[] = { new_val, new_ptr, new_mem };
		new_store = new_bd_amd64_Store(dbgi, block, ARRAY_SIZE(in), in,
		                               insn_mode, am);
		arch_set_irn_register_reqs_in(new_store, am_store_base_reqs);
	}
	set_irn_pinned(new_store, get_irn_pinned(node));
	return new_store;
}

ir_node *amd64_new_spill(ir_node *value, ir_node *after)
{
	ir_node  *block = get_block(after);
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *frame = get_irg_frame(irg);
	ir_node  *mem   = get_irg_no_mem(irg);

	amd64_am_info_t am;
	memset(&am, 0, sizeof(am));
	am.base_input  = 1;
	am.index_input = NO_INPUT;

	ir_node *in[] = { value, frame, mem };
	ir_node *store = new_bd_amd64_Store(NULL, block, ARRAY_SIZE(in), in,
	                                    INSN_MODE_64, am);
	arch_set_irn_register_reqs_in(store, am_store_base_reqs);
	sched_add_after(after, store);
	return store;
}

ir_node *amd64_new_reload(ir_node *value, ir_node *spill, ir_node *before)
{
	ir_node  *block = get_block(before);
	ir_graph *irg   = get_irn_irg(block);
	ir_node  *frame = get_irg_frame(irg);
	ir_mode  *mode  = get_irn_mode(value);

	amd64_am_info_t am;
	memset(&am, 0, sizeof(am));
	am.base_input  = 0;
	am.index_input = NO_INPUT;

	ir_node *in[] = { frame, spill };
	ir_node *load = new_bd_amd64_LoadZ(NULL, block, ARRAY_SIZE(in), in,
	                                   INSN_MODE_64, am);
	arch_set_irn_register_reqs_in(load, am_load_base_reqs);
	sched_add_before(before, load);
	ir_node *res = new_r_Proj(load, mode, pn_amd64_LoadZ_res);
	return res;
}

static ir_node *gen_Load(ir_node *node)
{
	ir_node  *block    = be_transform_node(get_nodes_block(node));
	ir_node  *ptr      = get_Load_ptr(node);
	ir_node  *new_ptr  = be_transform_node(ptr);
	ir_node  *mem      = get_Load_mem(node);
	ir_node  *new_mem  = be_transform_node(mem);
	ir_mode  *mode     = get_Load_mode(node);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	ir_node  *new_load;

	if (mode_is_float(mode)) {
		panic("Float not supported yet");
	} else {
		assert(mode_needs_gp_reg(mode) && "unsupported mode for Load");
		amd64_insn_mode_t insn_mode = get_insn_mode_from_mode(mode);
		amd64_am_info_t am;
		memset(&am, 0, sizeof(am));
		am.base_input = 0;
		am.mem_input  = 1;
		ir_node *in[] = { new_ptr, new_mem };
		if (get_mode_size_bits(mode) < 64 && mode_is_signed(mode)) {
			new_load = new_bd_amd64_LoadS(dbgi, block, ARRAY_SIZE(in), in,
			                              insn_mode, am);
		} else {
			new_load = new_bd_amd64_LoadZ(dbgi, block, ARRAY_SIZE(in), in,
			                              insn_mode, am);
		}
		arch_set_irn_register_reqs_in(new_load, am_load_base_reqs);
	}
	set_irn_pinned(new_load, get_irn_pinned(node));

	return new_load;
}

static ir_node *gen_Proj_Load(ir_node *node)
{
	ir_node  *load     = get_Proj_pred(node);
	ir_node  *new_load = be_transform_node(load);
	dbg_info *dbgi     = get_irn_dbg_info(node);
	long     proj      = get_Proj_proj(node);

	/* renumber the proj */
	switch (get_amd64_irn_opcode(new_load)) {
		case iro_amd64_LoadS:
			/* handle all gp loads equal: they have the same proj numbers. */
			if (proj == pn_Load_res) {
				return new_rd_Proj(dbgi, new_load, mode_Lu, pn_amd64_LoadS_res);
			} else if (proj == pn_Load_M) {
				return new_rd_Proj(dbgi, new_load, mode_M, pn_amd64_LoadS_M);
			}
		break;
		case iro_amd64_LoadZ:
			/* handle all gp loads equal: they have the same proj numbers. */
			if (proj == pn_Load_res) {
				return new_rd_Proj(dbgi, new_load, mode_Lu, pn_amd64_LoadZ_res);
			} else if (proj == pn_Load_M) {
				return new_rd_Proj(dbgi, new_load, mode_M, pn_amd64_LoadZ_M);
			}
		break;
		default:
			panic("Unsupported Proj from Load");
	}

    return be_duplicate_node(node);
}

static ir_node *gen_Proj_Store(ir_node *node)
{
	ir_node *pred = get_Proj_pred(node);
	long     pn   = get_Proj_proj(node);
	if (pn == pn_Store_M) {
		return be_transform_node(pred);
	} else {
		panic("Unsupported Proj from Store");
	}
}

/* Boilerplate code for transformation: */

static void amd64_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_Add,      gen_Add);
	be_set_transform_function(op_And,      gen_And);
	be_set_transform_function(op_Cmp,      gen_Cmp);
	be_set_transform_function(op_Call,     gen_Call);
	be_set_transform_function(op_Cond,     gen_Cond);
	be_set_transform_function(op_Const,    gen_Const);
	be_set_transform_function(op_Conv,     gen_Conv);
	be_set_transform_function(op_Div,      gen_Div);
	be_set_transform_function(op_Eor,      gen_Eor);
	be_set_transform_function(op_Jmp,      gen_Jmp);
	be_set_transform_function(op_Load,     gen_Load);
	be_set_transform_function(op_Minus,    gen_Minus);
	be_set_transform_function(op_Mod,      gen_Mod);
	be_set_transform_function(op_Mul,      gen_Mul);
	be_set_transform_function(op_Not,      gen_Not);
	be_set_transform_function(op_Or,       gen_Or);
	be_set_transform_function(op_Phi,      gen_Phi);
	be_set_transform_function(op_Return,   gen_Return);
	be_set_transform_function(op_Sel,      gen_Sel);
	be_set_transform_function(op_Shl,      gen_Shl);
	be_set_transform_function(op_Shr,      gen_Shr);
	be_set_transform_function(op_Shrs,     gen_Shrs);
	be_set_transform_function(op_Start,    gen_Start);
	be_set_transform_function(op_Store,    gen_Store);
	be_set_transform_function(op_Sub,      gen_Sub);
	be_set_transform_function(op_Switch,   gen_Switch);
	be_set_transform_function(op_SymConst, gen_SymConst);

	be_set_transform_proj_function(op_Call,   gen_Proj_Call);
	be_set_transform_proj_function(op_Cond,   be_duplicate_node);
	be_set_transform_proj_function(op_Div,    gen_Proj_Div);
	be_set_transform_proj_function(op_Load,   gen_Proj_Load);
	be_set_transform_proj_function(op_Mod,    gen_Proj_Mod);
	be_set_transform_proj_function(op_Proj,   gen_Proj_Proj);
	be_set_transform_proj_function(op_Start,  gen_Proj_Start);
	be_set_transform_proj_function(op_Store,  gen_Proj_Store);
	be_set_transform_proj_function(op_Switch, be_duplicate_node);
}

static ir_type *amd64_get_between_type(void)
{
	static ir_type *between_type = NULL;

	assert(current_cconv->omit_fp);
	if (between_type == NULL) {
		between_type = new_type_class(new_id_from_str("amd64_between_type"));
		set_type_size_bytes(between_type, get_mode_size_bytes(mode_gp));
	}

	return between_type;
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
	layout->between_type   = amd64_get_between_type();
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
	                         | IR_GRAPH_PROPERTY_NO_BADS);

	start_mem.irn = NULL;
	start_sp.irn  = NULL;
	start_fp.irn  = NULL;

	amd64_register_transformers();
	mode_gp = mode_Lu;
	node_to_stack = pmap_create();

	stackorder = be_collect_stacknodes(irg);
	ir_entity *entity = get_irg_entity(irg);
	ir_type   *mtp    = get_entity_type(entity);
	current_cconv = amd64_decide_calling_convention(mtp, irg);
	amd64_create_stacklayout(irg, current_cconv);

	be_transform_graph(irg, NULL);

	be_free_stackorder(stackorder);
	amd64_free_calling_convention(current_cconv);
	pmap_destroy(node_to_stack);
	node_to_stack = NULL;

	ir_type *frame_type = get_irg_frame_type(irg);
	if (get_type_state(frame_type) == layout_undefined)
		default_layout_compound_type(frame_type);

	place_code(irg);

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	be_add_missing_keeps(irg);
}

void amd64_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.transform");
}
