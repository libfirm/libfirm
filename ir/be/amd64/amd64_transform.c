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

#include "benode.h"
#include "betranshlp.h"
#include "beutil.h"
#include "bearch_amd64_t.h"

#include "amd64_nodes_attr.h"
#include "amd64_transform.h"
#include "amd64_new_nodes.h"

#include "gen_amd64_regalloc_if.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static ir_mode *mode_gp;

/* Some support functions: */

static inline int mode_needs_gp_reg(ir_mode *mode)
{
	return mode_is_int(mode) || mode_is_reference(mode);
}

/* Op transformers: */

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
	return new_bd_amd64_Const(dbgi, block, imode, val, false, NULL);
}

static ir_node *gen_SymConst(ir_node *node)
{
	ir_node   *block  = be_transform_node(get_nodes_block(node));
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_entity *entity = get_SymConst_entity(node);

	return new_bd_amd64_Const(dbgi, block, INSN_MODE_32, 0, false, entity);
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
		                                          sval, false, NULL);
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

static ir_node *gen_be_Call(ir_node *node)
{
	ir_node *res = be_duplicate_node(node);
	arch_add_irn_flags(res, arch_irn_flag_modify_flags);

	return res;
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
		new_store = new_bd_amd64_Store(dbgi, block, new_ptr, new_val, new_mem, insn_mode, NULL);
	}
	set_irn_pinned(new_store, get_irn_pinned(node));
	return new_store;
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
		if (get_mode_size_bits(mode) < 64 && mode_is_signed(mode)) {
			new_load = new_bd_amd64_LoadS(dbgi, block, new_ptr, new_mem, insn_mode, NULL);
		} else {
			new_load = new_bd_amd64_LoadZ(dbgi, block, new_ptr, new_mem, insn_mode, NULL);
		}
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

static ir_node *gen_Proj_be_Call(ir_node *node)
{
	ir_mode *mode = get_irn_mode(node);
	if (mode_needs_gp_reg(mode)) {
		ir_node *pred     = get_Proj_pred(node);
		ir_node *new_pred = be_transform_node(pred);
		long     pn       = get_Proj_proj(node);
		ir_node *new_proj = new_r_Proj(new_pred, mode_Lu, pn);
		return new_proj;
	} else {
		return be_duplicate_node(node);
	}
}

static ir_node *gen_be_FrameAddr(ir_node *node)
{
	ir_node   *block  = be_transform_node(get_nodes_block(node));
	ir_entity *ent    = be_get_frame_entity(node);
	ir_node   *fp     = be_get_FrameAddr_frame(node);
	ir_node   *new_fp = be_transform_node(fp);
	dbg_info  *dbgi   = get_irn_dbg_info(node);
	ir_node   *new_node;

	new_node = new_bd_amd64_FrameAddr(dbgi, block, new_fp, ent);
	return new_node;
}

static ir_node *gen_be_Start(ir_node *node)
{
	ir_node *new_node = be_duplicate_node(node);
	be_start_set_setup_stackframe(new_node, true);
	return new_node;
}

static ir_node *gen_be_Return(ir_node *node)
{
	ir_node *new_node = be_duplicate_node(node);
	be_return_set_destroy_stackframe(new_node, true);
	return new_node;
}

/* Boilerplate code for transformation: */

static void amd64_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_Add,          gen_Add);
	be_set_transform_function(op_And,          gen_And);
	be_set_transform_function(op_be_Call,      gen_be_Call);
	be_set_transform_function(op_be_FrameAddr, gen_be_FrameAddr);
	be_set_transform_function(op_be_Return,    gen_be_Return);
	be_set_transform_function(op_be_Start,     gen_be_Start);
	be_set_transform_function(op_Cmp,          gen_Cmp);
	be_set_transform_function(op_Cond,         gen_Cond);
	be_set_transform_function(op_Const,        gen_Const);
	be_set_transform_function(op_Conv,         gen_Conv);
	be_set_transform_function(op_Div,          gen_Div);
	be_set_transform_function(op_Eor,          gen_Eor);
	be_set_transform_function(op_Jmp,          gen_Jmp);
	be_set_transform_function(op_Load,         gen_Load);
	be_set_transform_function(op_Minus,        gen_Minus);
	be_set_transform_function(op_Mod,          gen_Mod);
	be_set_transform_function(op_Mul,          gen_Mul);
	be_set_transform_function(op_Not,          gen_Not);
	be_set_transform_function(op_Or,           gen_Or);
	be_set_transform_function(op_Phi,          gen_Phi);
	be_set_transform_function(op_Shl,          gen_Shl);
	be_set_transform_function(op_Shr,          gen_Shr);
	be_set_transform_function(op_Shrs,         gen_Shrs);
	be_set_transform_function(op_Store,        gen_Store);
	be_set_transform_function(op_Sub,          gen_Sub);
	be_set_transform_function(op_Switch,       gen_Switch);
	be_set_transform_function(op_SymConst,     gen_SymConst);

	be_set_transform_proj_function(op_be_Call,  gen_Proj_be_Call);
	be_set_transform_proj_function(op_be_Start, be_duplicate_node);
	be_set_transform_proj_function(op_Cond,     be_duplicate_node);
	be_set_transform_proj_function(op_Div,      gen_Proj_Div);
	be_set_transform_proj_function(op_Load,     gen_Proj_Load);
	be_set_transform_proj_function(op_Mod,      gen_Proj_Mod);
	be_set_transform_proj_function(op_Store,    gen_Proj_Store);
	be_set_transform_proj_function(op_Switch,   be_duplicate_node);
}

void amd64_transform_graph(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_TUPLES
	                         | IR_GRAPH_PROPERTY_NO_BADS);

	amd64_register_transformers();
	mode_gp = mode_Lu;
	be_transform_graph(irg, NULL);
}

void amd64_init_transform(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.be.amd64.transform");
}
