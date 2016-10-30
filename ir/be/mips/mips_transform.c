/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#include "mips_transform.h"

#include "beirg.h"
#include "benode.h"
#include "betranshlp.h"
#include "gen_mips_new_nodes.h"
#include "gen_mips_regalloc_if.h"
#include "mips_bearch_t.h"
#include "mips_cconv.h"
#include "nodes.h"
#include "panic.h"
#include "util.h"

static mips_calling_convention_t cur_cconv;

static be_stack_env_t stack_env;

static unsigned const callee_saves[] = {
	REG_S0,
	REG_S1,
	REG_S2,
	REG_S3,
	REG_S4,
	REG_S5,
	REG_S6,
	REG_S7,
	REG_S8,
};

static unsigned const caller_saves[] = {
	REG_V0,
	REG_V1,
	REG_A0,
	REG_A1,
	REG_A2,
	REG_A3,
	REG_T0,
	REG_T1,
	REG_T2,
	REG_T3,
	REG_T4,
	REG_T5,
	REG_T6,
	REG_T7,
	REG_T8,
	REG_T9,
	REG_RA,
};

static ir_node *get_Start_sp(ir_graph *const irg)
{
	return be_get_Start_proj(irg, &mips_registers[REG_SP]);
}

static ir_node *get_Start_zero(ir_graph *const irg)
{
	return be_get_Start_proj(irg, &mips_registers[REG_ZERO]);
}

static inline bool is_uimm5(long const val)
{
	return 0 <= val && val < 32;
}

static inline bool is_simm16(long const val)
{
	return -32768 <= val && val < 32768;
}

static inline bool is_uimm16(long const val)
{
	return 0 <= val && val < 65536;
}

static ir_node *gen_Add(ir_node *const node)
{
	ir_node *const l    = get_Add_left(node);
	ir_node *const r    = get_Add_right(node);
	ir_mode *const mode = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = be_transform_nodes_block(node);
		ir_node  *const new_l = be_transform_node(l);
		if (is_Const(r)) {
			long const val = get_Const_long(r);
			if (is_simm16(val))
				return new_bd_mips_addiu(dbgi, block, new_l, val);
		}
		ir_node *const new_r = be_transform_node(r);
		return new_bd_mips_addu(dbgi, block, new_l, new_r);
	}
	panic("TODO");
}

typedef ir_node *cons_binop(dbg_info*, ir_node*, ir_node*, ir_node*);
typedef ir_node *cons_binop_imm(dbg_info*, ir_node*, ir_node*, int32_t);

static ir_node *gen_logic_op(ir_node *const node, cons_binop *const cons, cons_binop_imm *const cons_imm)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const l     = get_binop_left(node);
	ir_node  *const new_l = be_transform_node(l);
	ir_node  *const r     = get_binop_right(node);
	if (is_Const(r)) {
		long const val = get_Const_long(r);
		if (is_uimm16(val))
			return cons_imm(dbgi, block, new_l, val);
	}
	ir_node *const new_r = be_transform_node(r);
	return cons(dbgi, block, new_l, new_r);
}

static ir_node *gen_And(ir_node *const node)
{
	return gen_logic_op(node, &new_bd_mips_and, &new_bd_mips_andi);
}

static ir_node *gen_Cmp(ir_node *const node)
{
	ir_node       *l    = get_Cmp_left(node);
	ir_node       *r    = get_Cmp_right(node);
	ir_mode *const mode = get_irn_mode(l);
	if (be_mode_needs_gp_reg(mode) && get_mode_size_bits(mode) == MIPS_MACHINE_SIZE) {
		ir_relation const rel = get_Cmp_relation(node);
		switch (rel) {
		case ir_relation_greater:
		case ir_relation_less_equal: {
			ir_node *const t = l;
			l = r;
			r = t;
		} /* FALLTHROUGH */
		case ir_relation_greater_equal:
		case ir_relation_less: {
			dbg_info *const dbgi  = get_irn_dbg_info(node);
			ir_node  *const block = be_transform_nodes_block(node);
			ir_node  *const new_l = be_transform_node(l);
			ir_node  *const new_r = be_transform_node(r);
			if (mode_is_signed(mode)) {
				return new_bd_mips_slt(dbgi, block, new_l, new_r);
			} else {
				return new_bd_mips_sltu(dbgi, block, new_l, new_r);
			}
		}

		case ir_relation_equal:
		case ir_relation_false:
		case ir_relation_less_equal_greater:
		case ir_relation_less_greater:
		case ir_relation_true:
		case ir_relation_unordered:
		case ir_relation_unordered_equal:
		case ir_relation_unordered_greater:
		case ir_relation_unordered_greater_equal:
		case ir_relation_unordered_less:
		case ir_relation_unordered_less_equal:
		case ir_relation_unordered_less_greater:
			panic("unexpected relation");
		}
	}
	panic("TODO");
}

static ir_node *gen_Cond(ir_node *const node)
{
	ir_node *const sel = get_Cond_selector(node);
	if (is_Cmp(sel)) {
		ir_node *const l    = get_Cmp_left(sel);
		ir_mode *const mode = get_irn_mode(l);
		if (be_mode_needs_gp_reg(mode) && get_mode_size_bits(mode) == MIPS_MACHINE_SIZE) {
			ir_relation    rel = get_Cmp_relation(sel);
			ir_node *const r   = get_Cmp_right(sel);
			if (is_irn_null(r)) {
				if (mode_is_signed(mode)) {
					switch (rel) {
						mips_cond_t cc;
					case ir_relation_greater:       cc = mips_cc_gtz; goto bccrelz;
					case ir_relation_greater_equal: cc = mips_cc_gez; goto bccrelz;
					case ir_relation_less:          cc = mips_cc_ltz; goto bccrelz;
					case ir_relation_less_equal:    cc = mips_cc_lez; goto bccrelz;
bccrelz:;
						dbg_info *const dbgi  = get_irn_dbg_info(node);
						ir_node  *const block = be_transform_nodes_block(node);
						ir_node  *const new_l = be_transform_node(l);
						return new_bd_mips_bcc_z(dbgi, block, new_l, cc);

					default: goto normal;
					}
				} else {
					/* Handle 'x >u 0' as cheaper 'x != 0'. */
					if (rel == ir_relation_greater)
						rel = ir_relation_less_greater;
					goto normal;
				}
			} else {
normal:
				switch (rel) {
				{
					mips_cond_t cc;
				case ir_relation_equal:        cc = mips_cc_eq; goto bcc;
				case ir_relation_less_greater: cc = mips_cc_ne; goto bcc;
bcc:;
					dbg_info *const dbgi  = get_irn_dbg_info(node);
					ir_node  *const block = be_transform_nodes_block(node);
					ir_node  *const new_l = be_transform_node(l);
					ir_node  *const new_r = be_transform_node(r);
					return new_bd_mips_bcc(dbgi, block, new_l, new_r, cc);
				}

				{
					mips_cond_t cc;
				case ir_relation_greater:       cc = mips_cc_ne; goto bcceqz;
				case ir_relation_greater_equal: cc = mips_cc_eq; goto bcceqz;
				case ir_relation_less:          cc = mips_cc_ne; goto bcceqz;
				case ir_relation_less_equal:    cc = mips_cc_eq; goto bcceqz;
bcceqz:;
					dbg_info *const dbgi    = get_irn_dbg_info(node);
					ir_node  *const block   = be_transform_nodes_block(node);
					ir_node  *const new_sel = be_transform_node(sel);
					ir_graph *const irg     = get_irn_irg(node);
					ir_node  *const zero    = get_Start_zero(irg);
					return new_bd_mips_bcc(dbgi, block, new_sel, zero, cc);
				}

				case ir_relation_false:
				case ir_relation_less_equal_greater:
				case ir_relation_true:
				case ir_relation_unordered:
				case ir_relation_unordered_equal:
				case ir_relation_unordered_greater:
				case ir_relation_unordered_greater_equal:
				case ir_relation_unordered_less:
				case ir_relation_unordered_less_equal:
				case ir_relation_unordered_less_greater:
					panic("unexpected relation");
				}
			}
		}
	}
	panic("TODO");
}

static ir_node *gen_Const(ir_node *const node)
{
	ir_mode *const mode = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		long const val = get_Const_long(node);
		if (val == 0) {
			ir_graph *const irg = get_irn_irg(node);
			return get_Start_zero(irg);
		} else if (is_simm16(val)) {
			dbg_info *const dbgi  = get_irn_dbg_info(node);
			ir_node  *const block = be_transform_nodes_block(node);
			ir_graph *const irg   = get_irn_irg(node);
			ir_node  *const zero  = get_Start_zero(irg);
			return new_bd_mips_addiu(dbgi, block, zero, val);
		} else {
			ir_node        *res;
			dbg_info *const dbgi  = get_irn_dbg_info(node);
			ir_node  *const block = be_transform_nodes_block(node);
			int32_t   const hi    = (uint32_t)val >> 16;
			if (hi != 0) {
				res = new_bd_mips_lui(dbgi, block, hi);
			} else {
				ir_graph *const irg = get_irn_irg(node);
				res = get_Start_zero(irg);
			}
			int32_t const lo = val & 0xFFFF;
			if (lo != 0)
				res = new_bd_mips_ori(dbgi, block, res, lo);
			return res;
		}
	}
	panic("TODO");
}

static ir_node *gen_Eor(ir_node *const node)
{
	return gen_logic_op(node, &new_bd_mips_xor, &new_bd_mips_xori);
}

static ir_node *gen_Jmp(ir_node *const node)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	return new_bd_mips_b(dbgi, block);
}

static ir_node *gen_Minus(ir_node *const node)
{
	ir_node *const val  = get_Minus_op(node);
	ir_mode *const mode = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = be_transform_nodes_block(node);
		ir_graph *const irg   = get_irn_irg(node);
		ir_node  *const new_l = get_Start_zero(irg);
		ir_node  *const new_r = be_transform_node(val);
		return new_bd_mips_subu(dbgi, block, new_l, new_r);
	}
	panic("TODO");
}

static ir_node *gen_Not(ir_node *const node)
{
	dbg_info *const dbgi    = get_irn_dbg_info(node);
	ir_node  *const block   = be_transform_nodes_block(node);
	ir_node  *const old_val = get_Not_op(node);
	if (is_Or(old_val)) {
		/* ~(l | r) -> nor(l, r) */
		ir_node *const old_l = get_Or_left(old_val);
		ir_node *const l     = be_transform_node(old_l);
		ir_node *const old_r = get_Or_right(old_val);
		ir_node *const r     = be_transform_node(old_r);
		return new_bd_mips_nor(dbgi, block, l, r);
	}
	/* ~v -> nor(v, v) */
	ir_node *const val = be_transform_node(old_val);
	return new_bd_mips_nor(dbgi, block, val, val);
}

static ir_node *gen_Or(ir_node *const node)
{
	return gen_logic_op(node, &new_bd_mips_or, &new_bd_mips_ori);
}

static ir_node *gen_Phi(ir_node *const node)
{
	arch_register_req_t const *req;
	ir_mode            *const  mode = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		req = &mips_class_reg_req_gp;
	} else if (mode == mode_M) {
		req = arch_memory_req;
	} else {
		panic("unhandled mode");
	}
	return be_transform_phi(node, req);
}

static ir_node *gen_Proj_Proj_Start(ir_node *const node)
{
	assert(get_Proj_num(get_Proj_pred(node)) == pn_Start_T_args);

	ir_graph           *const irg   = get_irn_irg(node);
	unsigned            const num   = get_Proj_num(node);
	mips_reg_or_slot_t *const param = &cur_cconv.parameters[num];
	if (param->reg) {
		return be_get_Start_proj(irg, param->reg);
	} else {
		panic("TODO");
	}
}

static ir_node *gen_Proj_Proj(ir_node *const node)
{
	ir_node *const pred      = get_Proj_pred(node);
	ir_node *const pred_pred = get_Proj_pred(pred);
	switch (get_irn_opcode(pred_pred)) {
	case iro_Start: return gen_Proj_Proj_Start(node);
	default:        panic("unexpected Proj-Proj");
	}
}

static ir_node *gen_Proj_Start(ir_node *const node)
{
	ir_graph *const irg = get_irn_irg(node);
	switch ((pn_Start)get_Proj_num(node)) {
	case pn_Start_M:            return be_get_Start_mem(irg);
	case pn_Start_P_frame_base: return get_Start_sp(irg);
	case pn_Start_T_args:       return new_r_Bad(irg, mode_T);
	}
	panic("unexpected Proj");
}

static ir_node *gen_Return(ir_node *const node)
{
	unsigned       p     = n_mips_ret_first_result;
	unsigned const n_res = get_Return_n_ress(node);
	unsigned const n_ins = p + n_res + ARRAY_SIZE(callee_saves);

	ir_graph                   *const irg  = get_irn_irg(node);
	arch_register_req_t const **const reqs = be_allocate_in_reqs(irg, n_ins);
	ir_node                   **const in   = ALLOCAN(ir_node*, n_ins);

	ir_node *const mem = get_Return_mem(node);
	in[n_mips_ret_mem]   = be_transform_node(mem);
	reqs[n_mips_ret_mem] = arch_memory_req;

	in[n_mips_ret_stack]   = get_Start_sp(irg);
	reqs[n_mips_ret_stack] = &mips_single_reg_req_gp_sp;

	in[n_mips_ret_addr]    = be_get_Start_proj(irg, &mips_registers[REG_RA]);
	reqs[n_mips_ret_addr]  = &mips_class_reg_req_gp;

	mips_reg_or_slot_t *const results = cur_cconv.results;
	for (size_t i = 0; i != n_res; ++i) {
		ir_node *const res = get_Return_res(node, i);
		in[p]   = be_transform_node(res);
		reqs[p] = results[i].reg->single_req;
		++p;
	}

	for (size_t i = 0; i != ARRAY_SIZE(callee_saves); ++i) {
		arch_register_t const *const reg = &mips_registers[callee_saves[i]];
		in[p]   = be_get_Start_proj(irg, reg);
		reqs[p] = reg->single_req;
		++p;
	}

	assert(p == n_ins);
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const ret   = new_bd_mips_ret(dbgi, block, n_ins, in, reqs);
	be_stack_record_chain(&stack_env, ret, n_mips_ret_stack, NULL);
	return ret;
}

static ir_node *gen_shift_op(ir_node *const node, cons_binop *const cons, cons_binop_imm *const cons_imm)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const l     = get_binop_left(node);
	ir_node  *const new_l = be_transform_node(l);
	ir_node  *const r     = get_binop_right(node);
	if (is_Const(r)) {
		long const val = get_Const_long(r);
		if (is_uimm5(val))
			return cons_imm(dbgi, block, new_l, val);
	}
	ir_node *const new_r = be_transform_node(r);
	return cons(dbgi, block, new_l, new_r);
}

static ir_node *gen_Shl(ir_node *const node)
{
	return gen_shift_op(node, &new_bd_mips_sllv, &new_bd_mips_sll);
}

static ir_node *gen_Shr(ir_node *const node)
{
	ir_mode *const mode = get_irn_mode(node);
	unsigned const size = get_mode_size_bits(mode);
	if (size == MIPS_MACHINE_SIZE)
		return gen_shift_op(node, &new_bd_mips_srlv, &new_bd_mips_srl);
	panic("TODO");
}

static ir_node *gen_Shrs(ir_node *const node)
{
	ir_mode *const mode = get_irn_mode(node);
	unsigned const size = get_mode_size_bits(mode);
	if (size == MIPS_MACHINE_SIZE)
		return gen_shift_op(node, &new_bd_mips_srav, &new_bd_mips_sra);
	panic("TODO");
}

static ir_node *gen_Start(ir_node *const node)
{
	be_start_out outs[N_MIPS_REGISTERS] = {
		[REG_ZERO] = BE_START_IGNORE,
		[REG_SP]   = BE_START_IGNORE,
		[REG_RA]   = BE_START_REG,
	};
	for (size_t i = 0; i != ARRAY_SIZE(callee_saves); ++i) {
		outs[callee_saves[i]] = BE_START_REG;
	}

	ir_graph  *const irg  = get_irn_irg(node);
	ir_entity *const ent  = get_irg_entity(irg);
	ir_type   *const type = get_entity_type(ent);
	for (size_t i = 0, n = get_method_n_params(type); i != n; ++i) {
		arch_register_t const *const reg = cur_cconv.parameters[i].reg;
		if (reg)
			outs[reg->global_index] = BE_START_REG;
	}

	return be_new_Start(irg, outs);
}

static ir_node *gen_Sub(ir_node *const node)
{
	ir_node *const l    = get_Sub_left(node);
	ir_node *const r    = get_Sub_right(node);
	ir_mode *const mode = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = be_transform_nodes_block(node);
		ir_node  *const new_l = be_transform_node(l);
		ir_node  *const new_r = be_transform_node(r);
		return new_bd_mips_subu(dbgi, block, new_l, new_r);
	}
	panic("TODO");
}

static void mips_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_Add,    gen_Add);
	be_set_transform_function(op_And,    gen_And);
	be_set_transform_function(op_Cmp,    gen_Cmp);
	be_set_transform_function(op_Cond,   gen_Cond);
	be_set_transform_function(op_Const,  gen_Const);
	be_set_transform_function(op_Eor,    gen_Eor);
	be_set_transform_function(op_Jmp,    gen_Jmp);
	be_set_transform_function(op_Minus,  gen_Minus);
	be_set_transform_function(op_Not,    gen_Not);
	be_set_transform_function(op_Or,     gen_Or);
	be_set_transform_function(op_Phi,    gen_Phi);
	be_set_transform_function(op_Return, gen_Return);
	be_set_transform_function(op_Shl,    gen_Shl);
	be_set_transform_function(op_Shr,    gen_Shr);
	be_set_transform_function(op_Shrs,   gen_Shrs);
	be_set_transform_function(op_Start,  gen_Start);
	be_set_transform_function(op_Sub,    gen_Sub);

	be_set_transform_proj_function(op_Proj,  gen_Proj_Proj);
	be_set_transform_proj_function(op_Start, gen_Proj_Start);
}

static void mips_set_allocatable_regs(ir_graph *const irg)
{
	be_irg_t       *const birg = be_birg_from_irg(irg);
	struct obstack *const obst = &birg->obst;

	unsigned *const a = rbitset_obstack_alloc(obst, N_MIPS_REGISTERS);
	for (size_t i = 0; i != ARRAY_SIZE(callee_saves); ++i) {
		rbitset_set(a, callee_saves[i]);
	}
	for (size_t i = 0; i != ARRAY_SIZE(caller_saves); ++i) {
		rbitset_set(a, caller_saves[i]);
	}
	birg->allocatable_regs = a;
}

void mips_transform_graph(ir_graph *const irg)
{
	mips_register_transformers();
	mips_set_allocatable_regs(irg);
	be_stack_init(&stack_env);

	ir_entity *const fun_ent  = get_irg_entity(irg);
	ir_type   *const fun_type = get_entity_type(fun_ent);
	mips_determine_calling_convention(&cur_cconv, fun_type);
	be_transform_graph(irg, NULL);
	mips_free_calling_convention(&cur_cconv);

	be_stack_finish(&stack_env);
}
