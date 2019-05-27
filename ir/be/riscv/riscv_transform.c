/*
 * This file is part of libFirm.
 * Copyright (C) 2018 Christoph Mallon.
 */

#include "riscv_transform.h"

#include "becconv.h"
#include "beirg.h"
#include "benode.h"
#include "betranshlp.h"
#include "gen_riscv_new_nodes.h"
#include "gen_riscv_regalloc_if.h"
#include "irprog_t.h"
#include "nodes.h"
#include "riscv_bearch_t.h"
#include "riscv_cconv.h"
#include "util.h"

static riscv_calling_convention_t cur_cconv;

static be_stack_env_t stack_env;

static const unsigned ignore_regs[] = {
	REG_T0,
};

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
	REG_S9,
	REG_S10,
	REG_S11,
};

static unsigned const caller_saves[] = {
	REG_RA,
	REG_T1,
	REG_T2,
	REG_A0,
	REG_A1,
	REG_A2,
	REG_A3,
	REG_A4,
	REG_A5,
	REG_A6,
	REG_A7,
	REG_T3,
	REG_T4,
	REG_T5,
	REG_T6,
};

static ir_node *get_Start_sp(ir_graph *const irg)
{
	return be_get_Start_proj(irg, &riscv_registers[REG_SP]);
}

static ir_node *get_Start_fp(ir_graph *const irg)
{
	return be_get_Start_proj(irg, &riscv_registers[REG_S0]);
}

ir_node *get_Start_zero(ir_graph *const irg)
{
	return be_get_Start_proj(irg, &riscv_registers[REG_ZERO]);
}

typedef struct riscv_addr {
	ir_node   *base;
	ir_entity *ent;
	int32_t    val;
} riscv_addr;

static riscv_addr make_addr(ir_node *addr)
{
	ir_entity *ent = 0;
	int32_t    val = 0;

	if (is_Add(addr)) {
		ir_node *const r = get_Add_right(addr);
		if (is_Const(r)) {
			long const v = get_Const_long(r);
			if (is_simm12(v)) {
				val  = v;
				addr = get_Add_left(addr);
			}
		}
	}

	if (is_Member(addr)) {
		ent  = get_Member_entity(addr);
		addr = get_Member_ptr(addr);
		assert(is_Proj(addr) && get_Proj_num(addr) == pn_Start_P_frame_base && is_Start(get_Proj_pred(addr)));
	}

	ir_node *const base = be_transform_node(addr);
	return (riscv_addr){ base, ent, val };
}

static ir_node *make_address(ir_node const *const node, ir_entity *const ent, int32_t const val)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const lui   = new_bd_riscv_lui(dbgi, block, ent, val);
	return new_bd_riscv_addi(dbgi, block, lui, ent, val);
}

static ir_node *make_extension(dbg_info *const dbgi, ir_node *const op, unsigned const to_size)
{
	ir_node *const new_op  = be_transform_node(op);
	ir_mode *const op_mode = get_irn_mode(op);
	unsigned const op_size = get_mode_size_bits(op_mode);
	if (op_size >= to_size)
		return new_op;

	/* Check whether the value is correctly extended already. */
	if (is_Proj(op)) {
		ir_node *const pred = get_Proj_pred(op);
		if (is_Load(pred))
			return new_op;
	}

	assert(op_size <= 16);
	ir_node *const block = get_nodes_block(new_op);
	if (mode_is_signed(op_mode)) {
		int32_t  const val = RISCV_MACHINE_SIZE - op_size;
		ir_node *const sll = new_bd_riscv_slli(dbgi, block, new_op, NULL, val);
		ir_node *const sra = new_bd_riscv_srai(dbgi, block, sll,    NULL, val);
		return sra;
	} else if (op_size == 8) {
		return new_bd_riscv_andi(dbgi, block, new_op, NULL, (1U << op_size) - 1);
	} else {
		int32_t  const val = RISCV_MACHINE_SIZE - op_size;
		ir_node *const sll = new_bd_riscv_slli(dbgi, block, new_op, NULL, val);
		ir_node *const srl = new_bd_riscv_srli(dbgi, block, sll,    NULL, val);
		return srl;
	}
}

static ir_node *extend_value(ir_node *const val)
{
	return make_extension(NULL, val, RISCV_MACHINE_SIZE);
}

static void riscv_parse_constraint_letter(void const *const env, be_asm_constraint_t* const c, char const l)
{
	(void)env;

	switch (l) {
	case 'g':
		c->all_registers_allowed = true;
		c->memory_possible       = true;
		/* FALLTHROUGH */
	case 'I':
	case 'J':
	case 'K':
	case 'i':
	case 'n':
		c->cls            = &riscv_reg_classes[CLASS_riscv_gp];
		c->immediate_type = l;
		break;

	case 'm':
		c->memory_possible = true;
		break;

	case 'r':
		c->cls                   = &riscv_reg_classes[CLASS_riscv_gp];
		c->all_registers_allowed = true;
		break;

	default:
		panic("unknown asm constraint '%c'", l);
	}
}

static bool riscv_check_immediate_constraint(long const val, char const imm_type)
{
	switch (imm_type) {
	case 'I': return is_simm12(val);
	case 'J': return val == 0;
	case 'K': return is_uimm5(val);

	case 'g':
	case 'i':
	case 'n': return true;
	default:
		panic("invalid immediate constraint found");
	}
}

static bool riscv_match_immediate(riscv_asm_operand_t *const operand, ir_node *const node, char const imm_type)
{
	ir_tarval *offset;
	ir_entity *entity;
	unsigned   reloc_kind;
	if (!be_match_immediate(node, &offset, &entity, &reloc_kind))
		return false;
	assert(reloc_kind == 0);

	if (entity && imm_type != 'g' && imm_type != 'i')
		return false;

	long value;
	if (offset) {
		value = get_tarval_long(offset);
		if (!riscv_check_immediate_constraint(value, imm_type))
			return false;
	} else {
		value = 0;
	}

	operand->val = value;
	operand->ent = entity;
	return true;
}

static ir_node *gen_ASM(ir_node *const node)
{
	be_asm_info_t info = be_asm_prepare_info(node);

	ir_asm_constraint const *const constraints   = get_ASM_constraints(node);
	size_t                   const n_constraints = get_ASM_n_constraints(node);
	ir_graph                *const irg           = get_irn_irg(node);
	struct obstack          *const obst          = get_irg_obstack(irg);
	riscv_asm_operand_t     *const operands      = NEW_ARR_DZ(riscv_asm_operand_t, obst, n_constraints);
	for (size_t i = 0; i != n_constraints; ++i) {
		ir_asm_constraint const *const c = &constraints[i];

		be_asm_constraint_t be_constraint;
		be_parse_asm_constraints_internal(&be_constraint, c->constraint, &riscv_parse_constraint_letter, NULL);

		riscv_asm_operand_t *const op = &operands[i];

		int const in_pos = c->in_pos;
		if (in_pos >= 0) {
			ir_node *const in  = get_ASM_input(node, in_pos);
			char     const imm = be_constraint.immediate_type;
			if (imm != '\0' && riscv_match_immediate(op, in, imm)) {
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

	return be_make_asm(node, &info, operands);
}

static ir_node *gen_Add(ir_node *const node)
{
	ir_tarval *tv;
	ir_entity *ent;
	unsigned   reloc_kind;
	if (be_match_immediate(node, &tv, &ent, &reloc_kind)) {
		assert(reloc_kind == 0);
		int32_t const val = get_tarval_long(tv);
		return make_address(node, ent, val);
	}

	ir_node *const l    = get_Add_left(node);
	ir_node *const r    = get_Add_right(node);
	ir_mode *const mode = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = be_transform_nodes_block(node);
		ir_node  *const new_l = be_transform_node(l);
		if (is_Const(r)) {
			long const val = get_Const_long(r);
			if (is_simm12(val))
				return new_bd_riscv_addi(dbgi, block, new_l, NULL, val);
		}
		ir_node *const new_r = be_transform_node(r);
		return new_bd_riscv_add(dbgi, block, new_l, new_r);
	}
	TODO(node);
}

static ir_node *gen_Address(ir_node *const node)
{
	ir_entity *const ent = get_Address_entity(node);
	return make_address(node, ent, 0);
}

static ir_node *gen_Alloc(ir_node *node)
{
	dbg_info *dbgi       = get_irn_dbg_info(node);
	ir_node  *new_block  = be_transform_nodes_block(node);
	ir_node  *size       = get_Alloc_size(node);
	ir_graph *irg        = get_irn_irg(node);
	ir_node  *stack_pred = be_get_Start_proj(irg, &riscv_registers[REG_SP]);
	ir_node  *mem        = get_Alloc_mem(node);
	ir_node  *new_mem    = be_transform_node(mem);

	ir_node *subsp;
	if (is_Const(size)) {
		long const sizel = get_Const_long(size);
		assert((sizel & ((1<<RISCV_PO2_STACK_ALIGNMENT) - 1)) == 0 && "Found Alloc with misaligned constant");
		assert(is_simm12(sizel));
		subsp = new_bd_riscv_SubSPimm(dbgi, new_block, new_mem, stack_pred, NULL, -sizel);
	} else {
		ir_node *new_size = be_transform_node(size);
		subsp = new_bd_riscv_SubSP(dbgi, new_block, new_mem, stack_pred, new_size);
	}

	ir_node *const stack_proj = be_new_Proj_reg(subsp, pn_riscv_SubSP_stack, &riscv_registers[REG_SP]);
	be_stack_record_chain(&stack_env, subsp, n_riscv_SubSP_stack, stack_proj);

	return subsp;
}

static ir_node *gen_Proj_Alloc(ir_node *node)
{
	ir_node *alloc     = get_Proj_pred(node);
	ir_node *new_alloc = be_transform_node(alloc);
	unsigned pn        = get_Proj_num(node);

	switch ((pn_Alloc)pn) {
		case pn_Alloc_M:   return be_new_Proj(new_alloc, pn_riscv_SubSP_M);
		case pn_Alloc_res: return be_new_Proj(new_alloc, pn_riscv_SubSP_addr);
	}
	panic("invalid Proj->Alloc");
}

typedef ir_node *cons_binop(dbg_info*, ir_node*, ir_node*, ir_node*);
typedef ir_node *cons_binop_imm(dbg_info*, ir_node*, ir_node*, ir_entity*, int32_t);

static ir_node *gen_logic_op(ir_node *const node, cons_binop *const cons, cons_binop_imm *const cons_imm)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const l     = get_binop_left(node);
	ir_node  *const new_l = be_transform_node(l);
	ir_node  *const r     = get_binop_right(node);
	if (is_Const(r)) {
		long const val = get_Const_long(r);
		if (is_simm12(val))
			return cons_imm(dbgi, block, new_l, NULL, val);
	}
	ir_node *const new_r = be_transform_node(r);
	return cons(dbgi, block, new_l, new_r);
}

static ir_node *gen_And(ir_node *const node)
{
	return gen_logic_op(node, &new_bd_riscv_and, &new_bd_riscv_andi);
}

static ir_node *gen_saturating_increment(ir_node *const node)
{
  dbg_info *const dbgi    = get_irn_dbg_info(node);
  ir_node  *const block   = be_transform_nodes_block(node);
  ir_node  *const operand = be_transform_node(get_Builtin_param(node, 0));
  assert(get_mode_size_bits(get_irn_mode(operand)) == 32);

  ir_node *const sltiu = new_bd_riscv_sltiu(dbgi, block, operand, NULL, -1);
  ir_node *const addu  = new_bd_riscv_add(  dbgi, block, operand, sltiu);
  return addu;
}

static ir_node *gen_Builtin(ir_node *const node)
{
	ir_builtin_kind const kind = get_Builtin_kind(node);
	switch (kind) {
	case ir_bk_saturating_increment: return gen_saturating_increment(node);

	case ir_bk_bswap:
	case ir_bk_clz:
	case ir_bk_compare_swap:
	case ir_bk_ctz:
	case ir_bk_debugbreak:
	case ir_bk_ffs:
	case ir_bk_frame_address:
	case ir_bk_inport:
	case ir_bk_may_alias:
	case ir_bk_outport:
	case ir_bk_parity:
	case ir_bk_popcount:
	case ir_bk_prefetch:
	case ir_bk_return_address:
	case ir_bk_trap:
	case ir_bk_va_arg:
	case ir_bk_va_start:
		TODO(node);
	}
	panic("unexpected Builtin");
}

static ir_node *gen_Call(ir_node *const node)
{
	ir_graph *const irg = get_irn_irg(node);

	unsigned                          p        = n_riscv_jal_first_argument;
	unsigned                    const n_params = get_Call_n_params(node);
	unsigned                    const n_ins    = p + 1 + n_params;
	arch_register_req_t const **const reqs     = be_allocate_in_reqs(irg, n_ins);
	ir_node                          *ins[n_ins];

	ir_entity     *callee;
	ir_node *const ptr = get_Call_ptr(node);
	if (is_Address(ptr)) {
		callee = get_Address_entity(ptr);
	} else {
		callee  = NULL;
		ins[p]  = be_transform_node(ptr);
		reqs[p] = &riscv_class_reg_req_gp;
		++p;
	}

	ir_type *const fun_type = get_Call_type(node);
	record_returns_twice(irg, fun_type);

	riscv_calling_convention_t cconv;
	riscv_determine_calling_convention(&cconv, fun_type, NULL);

	ir_node *mems[1 + cconv.n_mem_param];
	unsigned m = 0;

	ir_node *const mem = get_Call_mem(node);
	mems[m++] = be_transform_node(mem);

	int      const frame_size = cconv.param_stack_size;
	ir_node *const block      = be_transform_nodes_block(node);
	ir_node *const sp         = get_Start_sp(irg);
	ir_node *const call_frame = be_new_IncSP(block, sp, frame_size, 0);

	ins[n_riscv_jal_stack]  = call_frame;
	reqs[n_riscv_jal_stack] = &riscv_single_reg_req_gp_sp;

	dbg_info *const dbgi = get_irn_dbg_info(node);
	for (size_t i = 0; i != n_params; ++i) {
		ir_node *const arg = get_Call_param(node, i);
		ir_node *const val = extend_value(arg);

		riscv_reg_or_slot_t const *const param = &cconv.parameters[i];
		if (param->reg) {
			ins[p]  = val;
			reqs[p] = param->reg->single_req;
			++p;
		} else {
			ir_node *const nomem = get_irg_no_mem(irg);
			mems[m++] = new_bd_riscv_sw(dbgi, block, nomem, call_frame, val, NULL, param->offset);
		}
	}

	riscv_free_calling_convention(&cconv);

	ins[n_riscv_jal_mem]  = be_make_Sync(block, m, mems);
	reqs[n_riscv_jal_mem] = arch_memory_req;

	unsigned const n_res = pn_riscv_jal_first_result + ARRAY_SIZE(caller_saves);

	ir_node *const jal = callee ?
		new_bd_riscv_jal( dbgi, block, p, ins, reqs, n_res, callee, 0) :
		new_bd_riscv_jalr(dbgi, block, p, ins, reqs, n_res);

	arch_set_irn_register_req_out(jal, pn_riscv_jal_M, arch_memory_req);
	arch_copy_irn_out_info(jal, pn_riscv_jal_stack, sp);
	for (size_t i = 0; i != ARRAY_SIZE(caller_saves); ++i) {
		arch_set_irn_register_req_out(jal, pn_riscv_jal_first_result + i, riscv_registers[caller_saves[i]].single_req);
	}

	ir_node *const jal_stack = be_new_Proj(jal, pn_riscv_jal_stack);
	ir_node *const new_stack = be_new_IncSP(block, jal_stack, -frame_size, 0);
	be_stack_record_chain(&stack_env, call_frame, n_be_IncSP_pred, new_stack);

	return jal;
}

static ir_node *gen_Cmp(ir_node *const node)
{
	ir_node       *l    = get_Cmp_left(node);
	ir_node       *r    = get_Cmp_right(node);
	ir_mode *const mode = get_irn_mode(l);
	if (be_mode_needs_gp_reg(mode)) {
		ir_relation const rel = get_Cmp_relation(node) & ir_relation_less_equal_greater;
		switch (rel) {
		case ir_relation_greater: {
			ir_node *const t = l;
			l = r;
			r = t;
		} /* FALLTHROUGH */
		case ir_relation_less: {
			dbg_info *const dbgi  = get_irn_dbg_info(node);
			ir_node  *const block = be_transform_nodes_block(node);
			ir_node  *const new_l = extend_value(l);
			ir_node  *const new_r = extend_value(r);
			if (mode_is_signed(mode)) {
				return new_bd_riscv_slt(dbgi, block, new_l, new_r);
			} else {
				return new_bd_riscv_sltu(dbgi, block, new_l, new_r);
			}
		}

		case ir_relation_equal:
		case ir_relation_false:
		case ir_relation_greater_equal:
		case ir_relation_less_equal:
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
	TODO(node);
}

static ir_node *gen_Cond(ir_node *const node)
{
	ir_node *const sel = get_Cond_selector(node);
	if (is_Cmp(sel)) {
		ir_node       *l    = get_Cmp_left(sel);
		ir_mode *const mode = get_irn_mode(l);
		if (be_mode_needs_gp_reg(mode)) {
			ir_relation const rel = get_Cmp_relation(sel) & ir_relation_less_equal_greater;
			ir_node          *r   = get_Cmp_right(sel);
			switch (rel) {
			{
				riscv_cond_t cc;
			case ir_relation_equal:         cc = riscv_cc_eq; goto bcc;
			case ir_relation_less_greater:  cc = riscv_cc_ne; goto bcc;

			case ir_relation_greater: {
				ir_node *const t = l;
				l = r;
				r = t;
			} /* FALLTHROUGH */
			case ir_relation_less:
				cc = mode_is_signed(mode) ? riscv_cc_lt : riscv_cc_ltu;
				goto bcc;

			case ir_relation_less_equal: {
				ir_node *const t = l;
				l = r;
				r = t;
			} /* FALLTHROUGH */
			case ir_relation_greater_equal:
				cc = mode_is_signed(mode) ? riscv_cc_ge : riscv_cc_geu;
				goto bcc;

bcc:;
				dbg_info *const dbgi  = get_irn_dbg_info(node);
				ir_node  *const block = be_transform_nodes_block(node);
				ir_node  *const new_l = extend_value(l);
				ir_node  *const new_r = extend_value(r);
				return new_bd_riscv_bcc(dbgi, block, new_l, new_r, cc);
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
	TODO(node);
}

static ir_node *gen_Const(ir_node *const node)
{
	ir_mode *const mode = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		long const val = get_Const_long(node);
		if (val == 0) {
			ir_graph *const irg = get_irn_irg(node);
			return get_Start_zero(irg);
		} else {
			dbg_info *const dbgi  = get_irn_dbg_info(node);
			ir_node  *const block = be_transform_nodes_block(node);

			riscv_hi_lo_imm imm = calc_hi_lo((int32_t)val);
			ir_node      *res;
			if (imm.hi != 0) {
				res = new_bd_riscv_lui(dbgi, block, NULL, imm.hi);
			} else {
				ir_graph *const irg = get_irn_irg(node);
				res = get_Start_zero(irg);
			}
			if (imm.lo != 0)
				res = new_bd_riscv_addi(dbgi, block, res, NULL, imm.lo);
			return res;
		}
	}
	TODO(node);
}

static ir_node *gen_Conv(ir_node *const node)
{
	ir_node *const op      = get_Conv_op(node);
	ir_mode *const op_mode = get_irn_mode(op);
	ir_mode *const mode    = get_irn_mode(node);
	if (be_mode_needs_gp_reg(op_mode) && be_mode_needs_gp_reg(mode)) {
		dbg_info *const dbgi = get_irn_dbg_info(node);
		return make_extension(dbgi, op, get_mode_size_bits(mode));
	}
	TODO(node);
}

static ir_node *gen_Div(ir_node *const node)
{
	ir_mode *const mode = get_Div_resmode(node);
	if (be_mode_needs_gp_reg(mode) && get_mode_size_bits(mode) == RISCV_MACHINE_SIZE) {
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = be_transform_nodes_block(node);
		ir_node  *const l     = be_transform_node(get_Div_left(node));
		ir_node  *const r     = be_transform_node(get_Div_right(node));
		if (mode_is_signed(mode)) {
			return new_bd_riscv_div(dbgi, block, l, r);
		} else {
			return new_bd_riscv_divu(dbgi, block, l, r);
		}
	}
	TODO(node);
}

static ir_node *gen_Eor(ir_node *const node)
{
	return gen_logic_op(node, &new_bd_riscv_xor, &new_bd_riscv_xori);
}

static ir_node *gen_IJmp(ir_node *const node)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const tgt   = be_transform_node(get_IJmp_target(node));
	return new_bd_riscv_ijmp(dbgi, block, tgt);
}

static ir_node *gen_Jmp(ir_node *const node)
{
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	return new_bd_riscv_j(dbgi, block);
}

typedef ir_node *cons_loadop(dbg_info*, ir_node*, ir_node*, ir_node*, ir_entity*, int32_t);

static ir_node *gen_Load(ir_node *const node)
{
	ir_mode *const mode = get_Load_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		cons_loadop   *cons;
		unsigned const size = get_mode_size_bits(mode);
		if (size == 8) {
			cons = mode_is_signed(mode) ? &new_bd_riscv_lb : &new_bd_riscv_lbu;
		} else if (size == 16) {
			cons = mode_is_signed(mode) ? &new_bd_riscv_lh : &new_bd_riscv_lhu;
		} else if (size == 32) {
			cons = new_bd_riscv_lw;
		} else {
			panic("invalid load");
		}
		dbg_info  *const dbgi  = get_irn_dbg_info(node);
		ir_node   *const block = be_transform_nodes_block(node);
		ir_node   *const mem   = be_transform_node(get_Load_mem(node));
		riscv_addr const addr  = make_addr(get_Load_ptr(node));
		return cons(dbgi, block, mem, addr.base, addr.ent, addr.val);
	}
	TODO(node);
}

static ir_node *gen_Member(ir_node *const node)
{
	ir_node *const ptr = get_Member_ptr(node);
	assert(is_Proj(ptr) && get_Proj_num(ptr) == pn_Start_P_frame_base && is_Start(get_Proj_pred(ptr)));
	dbg_info  *const dbgi  = get_irn_dbg_info(node);
	ir_node   *const block = be_transform_nodes_block(node);
	ir_node   *const frame = be_transform_node(ptr);
	ir_entity *const ent   = get_Member_entity(node);
	return new_bd_riscv_FrameAddr(dbgi, block, frame, ent, 0);
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
		return new_bd_riscv_sub(dbgi, block, new_l, new_r);
	}
	TODO(node);
}

static ir_node *gen_Mod(ir_node *const node)
{
	ir_mode *const mode = get_Mod_resmode(node);
	if (be_mode_needs_gp_reg(mode) && get_mode_size_bits(mode) == RISCV_MACHINE_SIZE) {
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = be_transform_nodes_block(node);
		ir_node  *const l     = be_transform_node(get_Mod_left(node));
		ir_node  *const r     = be_transform_node(get_Mod_right(node));
		if (mode_is_signed(mode)) {
			return new_bd_riscv_rem(dbgi, block, l, r);
		} else {
			return new_bd_riscv_remu(dbgi, block, l, r);
		}
	}
	TODO(node);
}

static ir_node *gen_Mul(ir_node *const node)
{
	ir_mode *const mode = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = be_transform_nodes_block(node);
		ir_node  *const l     = be_transform_node(get_Mul_left(node));
		ir_node  *const r     = be_transform_node(get_Mul_right(node));
		return new_bd_riscv_mul(dbgi, block, l, r);
	}
	TODO(node);
}

static ir_node *gen_Mulh(ir_node *const node)
{
	ir_mode *const mode = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode) && get_mode_size_bits(mode) == RISCV_MACHINE_SIZE) {
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = be_transform_nodes_block(node);
		ir_node  *const l     = be_transform_node(get_Mulh_left(node));
		ir_node  *const r     = be_transform_node(get_Mulh_right(node));
		if (mode_is_signed(mode)) {
			return new_bd_riscv_mulh(dbgi, block, l, r);
		} else {
			return new_bd_riscv_mulhu(dbgi, block, l, r);
		}
	}
	TODO(node);
}

static ir_node *gen_Mux(ir_node *const node)
{
	ir_mode *const mode = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		if (is_irn_null(get_Mux_false(node)) && is_irn_one(get_Mux_true(node))) {
			ir_node *const sel = get_Mux_sel(node);
			if (is_Cmp(sel)) {
				ir_relation const rel = get_Cmp_relation(sel) & ir_relation_less_equal_greater;
				if (rel == ir_relation_less || rel == ir_relation_greater)
					return be_transform_node(sel);
			}
		}
	}
	TODO(node);
}

static ir_node *gen_Not(ir_node *const node)
{
	/* ~v -> xor(v, -1) */
	dbg_info *const dbgi    = get_irn_dbg_info(node);
	ir_node  *const block   = be_transform_nodes_block(node);
	ir_node  *const old_val = get_Not_op(node);
	ir_node *const val      = be_transform_node(old_val);
	return new_bd_riscv_xori(dbgi, block, val, NULL, -1);
}

static ir_node *gen_Or(ir_node *const node)
{
	return gen_logic_op(node, &new_bd_riscv_or, &new_bd_riscv_ori);
}

static ir_node *gen_Phi(ir_node *const node)
{
	arch_register_req_t const *req;
	ir_mode            *const  mode = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		req = &riscv_class_reg_req_gp;
	} else if (mode == mode_M) {
		req = arch_memory_req;
	} else {
		panic("unhandled mode");
	}
	return be_transform_phi(node, req);
}

static ir_node *gen_Proj_Builtin(ir_node *const node)
{
	ir_node         *const pred     = get_Proj_pred(node);
	ir_node         *const new_pred = be_transform_node(pred);
	ir_builtin_kind  const kind     = get_Builtin_kind(pred);
	switch (kind) {
	case ir_bk_saturating_increment:
		assert(get_Proj_num(node) == pn_Builtin_max + 1);
		return new_pred;

	case ir_bk_bswap:
	case ir_bk_clz:
	case ir_bk_compare_swap:
	case ir_bk_ctz:
	case ir_bk_debugbreak:
	case ir_bk_ffs:
	case ir_bk_frame_address:
	case ir_bk_inport:
	case ir_bk_may_alias:
	case ir_bk_outport:
	case ir_bk_parity:
	case ir_bk_popcount:
	case ir_bk_prefetch:
	case ir_bk_return_address:
	case ir_bk_trap:
	case ir_bk_va_arg:
	case ir_bk_va_start:
		TODO(node);
	}
	panic("unexpected Builtin");
}

static ir_node *gen_Proj_Call(ir_node *const node)
{
	ir_node *const pred = get_Proj_pred(node);
	ir_node *const call = be_transform_node(pred);
	unsigned const pn   = get_Proj_num(node);
	switch ((pn_Call)pn) {
	case pn_Call_M:
		return be_new_Proj(call, pn_riscv_jal_M);
	case pn_Call_T_result:
	case pn_Call_X_regular:
	case pn_Call_X_except:
		break;
	}
	panic("unexpected Proj-Call");
}

static ir_node *gen_Proj_Div(ir_node *const node)
{
	ir_node *const pred = get_Proj_pred(node);
	unsigned const pn   = get_Proj_num(node);
	switch ((pn_Div)pn) {
	case pn_Div_M:   return get_Div_mem(pred);
	case pn_Div_res: return be_transform_node(pred);
	case pn_Div_X_regular:
	case pn_Div_X_except:
		break;
	}
	TODO(node);
}

static ir_node *gen_Proj_Load(ir_node *const node)
{
	ir_node *const pred = get_Proj_pred(node);
	ir_node *const load = be_transform_node(pred);
	unsigned const pn   = get_Proj_num(node);
	switch ((pn_Load)pn) {
	case pn_Load_M:   return be_new_Proj(load, pn_riscv_lw_M);
	case pn_Load_res: return be_new_Proj(load, pn_riscv_lw_res);
	case pn_Load_X_regular:
	case pn_Load_X_except:
		break;
	}
	TODO(node);
}

static ir_node *gen_Proj_Mod(ir_node *const node)
{
	ir_node *const pred = get_Proj_pred(node);
	unsigned const pn   = get_Proj_num(node);
	switch ((pn_Mod)pn) {
	case pn_Mod_M:   return get_Mod_mem(pred);
	case pn_Mod_res: return be_transform_node(pred);
	case pn_Mod_X_regular:
	case pn_Mod_X_except:
		break;
	}
	TODO(node);
}

static ir_node *gen_Proj_Proj_Call(ir_node *const node)
{
	ir_node *const pred = get_Proj_pred(node);
	assert(get_Proj_num(pred) == pn_Call_T_result);

	ir_node *const ocall    = get_Proj_pred(pred);
	ir_type *const fun_type = get_Call_type(ocall);

	riscv_calling_convention_t cconv;
	riscv_determine_calling_convention(&cconv, fun_type, NULL);

	ir_node               *const call = be_transform_node(ocall);
	unsigned               const num  = get_Proj_num(node);
	arch_register_t const *const reg  = cconv.results[num].reg;
	unsigned               const pos  = be_get_out_for_reg(call, reg);

	riscv_free_calling_convention(&cconv);

	return be_new_Proj(call, pos);
}

static ir_node *gen_Proj_Proj_Start(ir_node *const node)
{
	assert(get_Proj_num(get_Proj_pred(node)) == pn_Start_T_args);

	ir_graph            *const irg   = get_irn_irg(node);
	unsigned             const num   = get_Proj_num(node);
	riscv_reg_or_slot_t *const param = &cur_cconv.parameters[num];
	if (param->reg) {
		return be_get_Start_proj(irg, param->reg);
	} else {
		dbg_info *const dbgi  = get_irn_dbg_info(node);
		ir_node  *const block = be_transform_nodes_block(node);
		ir_node  *const mem   = be_get_Start_mem(irg);
		ir_node  *const base  = cur_cconv.omit_fp ? get_Start_sp(irg) : get_Start_fp(irg);
		ir_node  *const load  = new_bd_riscv_lw(dbgi, block, mem, base, param->entity, 0);
		arch_add_irn_flags(load, (arch_irn_flags_t)riscv_arch_irn_flag_ignore_fp_offset_fix);
		return be_new_Proj(load, pn_riscv_lw_res);
	}
}

static ir_node *gen_Proj_Proj(ir_node *const node)
{
	ir_node *const pred      = get_Proj_pred(node);
	ir_node *const pred_pred = get_Proj_pred(pred);
	switch (get_irn_opcode(pred_pred)) {
	case iro_Call:  return gen_Proj_Proj_Call(node);
	case iro_Start: return gen_Proj_Proj_Start(node);
	default:        panic("unexpected Proj-Proj");
	}
}

static ir_node *gen_Proj_Start(ir_node *const node)
{
	ir_graph *const irg = get_irn_irg(node);
	switch ((pn_Start)get_Proj_num(node)) {
	case pn_Start_M:            return be_get_Start_mem(irg);
	case pn_Start_P_frame_base: return cur_cconv.omit_fp ? get_Start_sp(irg): get_Start_fp(irg);
	case pn_Start_T_args:       return new_r_Bad(irg, mode_T);
	}
	panic("unexpected Proj");
}

static ir_node *gen_Proj_Store(ir_node *const node)
{
	ir_node *const pred  = get_Proj_pred(node);
	ir_node *const store = be_transform_node(pred);
	unsigned const pn    = get_Proj_num(node);
	switch ((pn_Store)pn) {
	case pn_Store_M: return store;
	case pn_Store_X_regular:
	case pn_Store_X_except:
		break;
	}
	TODO(node);
}

static ir_node *gen_Return(ir_node *const node)
{
	unsigned       p     = n_riscv_ret_first_result;
	unsigned const n_res = get_Return_n_ress(node);
	unsigned const n_ins = p + n_res + ARRAY_SIZE(callee_saves);

	ir_graph                   *const irg  = get_irn_irg(node);
	arch_register_req_t const **const reqs = be_allocate_in_reqs(irg, n_ins);
	ir_node                   **const in   = ALLOCAN(ir_node*, n_ins);

	ir_node *const mem = get_Return_mem(node);
	in[n_riscv_ret_mem]   = be_transform_node(mem);
	reqs[n_riscv_ret_mem] = arch_memory_req;

	in[n_riscv_ret_stack]   = get_Start_sp(irg);
	reqs[n_riscv_ret_stack] = &riscv_single_reg_req_gp_sp;

	in[n_riscv_ret_addr]    = be_get_Start_proj(irg, &riscv_registers[REG_RA]);
	reqs[n_riscv_ret_addr]  = &riscv_single_reg_req_gp_ra;

	riscv_reg_or_slot_t *const results = cur_cconv.results;
	for (size_t i = 0; i != n_res; ++i) {
		ir_node *const res = get_Return_res(node, i);
		in[p]   = be_transform_node(res);
		reqs[p] = results[i].reg->single_req;
		++p;
	}

	for (size_t i = 0; i != ARRAY_SIZE(callee_saves); ++i) {
		arch_register_t const *const reg = &riscv_registers[callee_saves[i]];
		in[p]   = be_get_Start_proj(irg, reg);
		reqs[p] = reg->single_req;
		++p;
	}

	assert(p == n_ins);
	dbg_info *const dbgi  = get_irn_dbg_info(node);
	ir_node  *const block = be_transform_nodes_block(node);
	ir_node  *const ret   = new_bd_riscv_ret(dbgi, block, n_ins, in, reqs);
	be_stack_record_chain(&stack_env, ret, n_riscv_ret_stack, NULL);
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
			return cons_imm(dbgi, block, new_l, NULL, val);
	}
	ir_node *const new_r = be_transform_node(r);
	return cons(dbgi, block, new_l, new_r);
}

static ir_node *gen_Shl(ir_node *const node)
{
	return gen_shift_op(node, &new_bd_riscv_sll, &new_bd_riscv_slli);
}

static ir_node *gen_Shr(ir_node *const node)
{
	ir_mode *const mode = get_irn_mode(node);
	unsigned const size = get_mode_size_bits(mode);
	if (size == RISCV_MACHINE_SIZE)
		return gen_shift_op(node, &new_bd_riscv_srl, &new_bd_riscv_srli);
	TODO(node);
}

static ir_node *gen_Shrs(ir_node *const node)
{
	ir_mode *const mode = get_irn_mode(node);
	unsigned const size = get_mode_size_bits(mode);
	if (size == RISCV_MACHINE_SIZE)
		return gen_shift_op(node, &new_bd_riscv_sra, &new_bd_riscv_srai);
	TODO(node);
}

static ir_node *gen_Start(ir_node *const node)
{
	be_start_out outs[N_RISCV_REGISTERS] = {
		[REG_ZERO] = BE_START_IGNORE,
		[REG_RA]   = BE_START_REG,
		[REG_SP]   = BE_START_IGNORE,
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
	if (!cur_cconv.omit_fp)
		outs[REG_S0] = BE_START_IGNORE;

	return be_new_Start(irg, outs);
}

typedef ir_node *cons_storeop(dbg_info*, ir_node*, ir_node*, ir_node*, ir_node*, ir_entity*, int32_t);

static ir_node *gen_Store(ir_node *const node)
{
	ir_node       *old_val = get_Store_value(node);
	ir_mode *const mode    = get_irn_mode(old_val);
	if (be_mode_needs_gp_reg(mode)) {
		cons_storeop  *cons;
		unsigned const size = get_mode_size_bits(mode);
		if (size == 8) {
			cons = &new_bd_riscv_sb;
		} else if (size == 16) {
			cons = &new_bd_riscv_sh;
		} else if (size == 32) {
			cons = &new_bd_riscv_sw;
		} else {
			panic("invalid store");
		}
		old_val = be_skip_downconv(old_val, false);
		dbg_info  *const dbgi  = get_irn_dbg_info(node);
		ir_node   *const block = be_transform_nodes_block(node);
		ir_node   *const mem   = be_transform_node(get_Store_mem(node));
		ir_node   *const val   = be_transform_node(old_val);
		riscv_addr const addr  = make_addr(get_Store_ptr(node));
		return cons(dbgi, block, mem, addr.base, val, addr.ent, addr.val);
	}
	TODO(node);
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
		return new_bd_riscv_sub(dbgi, block, new_l, new_r);
	}
	TODO(node);
}

static ir_node *gen_Switch(ir_node *const node)
{
	ir_graph              *const irg   = get_irn_irg(node);
	ir_switch_table const *const table = ir_switch_table_duplicate(irg, get_Switch_table(node));

	ir_type   *const utype  = get_unknown_type();
	ident     *const id     = id_unique("TBL");
	ir_entity *const entity = new_global_entity(irp->dummy_owner, id, utype, ir_visibility_private, IR_LINKAGE_CONSTANT | IR_LINKAGE_NO_IDENTITY);

	dbg_info  *const dbgi   = get_irn_dbg_info(node);
	ir_node   *const block  = be_transform_nodes_block(node);
	ir_node   *const nomem  = get_irg_no_mem(irg);
	ir_node   *const sel    = be_transform_node(get_Switch_selector(node));
	ir_node   *const sll    = new_bd_riscv_slli(dbgi, block, sel, NULL, 2);
	ir_node   *const lui    = new_bd_riscv_lui(dbgi, block, entity, 0);
	ir_node   *const add    = new_bd_riscv_add(dbgi, block, sll, lui);
	ir_node   *const load   = new_bd_riscv_lw(dbgi, block, nomem, add, entity, 0);
	ir_node   *const res    = be_new_Proj(load, pn_riscv_lw_res);
	unsigned   const n_outs = get_Switch_n_outs(node);
	return new_bd_riscv_switch(dbgi, block, res, n_outs, table, entity);
}

static ir_node *gen_Unknown(ir_node *const node)
{
	ir_node *const block = be_transform_nodes_block(node);
	ir_mode *const mode  = get_irn_mode(node);
	if (be_mode_needs_gp_reg(mode)) {
		return be_new_Unknown(block, &riscv_class_reg_req_gp);
	} else {
		TODO(node);
	}
}

static void riscv_register_transformers(void)
{
	be_start_transform_setup();

	be_set_transform_function(op_ASM,     gen_ASM);
	be_set_transform_function(op_Add,     gen_Add);
	be_set_transform_function(op_Address, gen_Address);
	be_set_transform_function(op_Alloc,   gen_Alloc);
	be_set_transform_function(op_And,     gen_And);
	be_set_transform_function(op_Builtin, gen_Builtin);
	be_set_transform_function(op_Call,    gen_Call);
	be_set_transform_function(op_Cmp,     gen_Cmp);
	be_set_transform_function(op_Cond,    gen_Cond);
	be_set_transform_function(op_Const,   gen_Const);
	be_set_transform_function(op_Conv,    gen_Conv);
	be_set_transform_function(op_Div,     gen_Div);
	be_set_transform_function(op_Eor,     gen_Eor);
	be_set_transform_function(op_IJmp,    gen_IJmp);
	be_set_transform_function(op_Jmp,     gen_Jmp);
	be_set_transform_function(op_Load,    gen_Load);
	be_set_transform_function(op_Member,  gen_Member);
	be_set_transform_function(op_Minus,   gen_Minus);
	be_set_transform_function(op_Mod,     gen_Mod);
	be_set_transform_function(op_Mul,     gen_Mul);
	be_set_transform_function(op_Mulh,    gen_Mulh);
	be_set_transform_function(op_Mux,     gen_Mux);
	be_set_transform_function(op_Not,     gen_Not);
	be_set_transform_function(op_Or,      gen_Or);
	be_set_transform_function(op_Phi,     gen_Phi);
	be_set_transform_function(op_Return,  gen_Return);
	be_set_transform_function(op_Shl,     gen_Shl);
	be_set_transform_function(op_Shr,     gen_Shr);
	be_set_transform_function(op_Shrs,    gen_Shrs);
	be_set_transform_function(op_Start,   gen_Start);
	be_set_transform_function(op_Store,   gen_Store);
	be_set_transform_function(op_Sub,     gen_Sub);
	be_set_transform_function(op_Switch,  gen_Switch);
	be_set_transform_function(op_Unknown, gen_Unknown);

	be_set_transform_proj_function(op_Alloc,   gen_Proj_Alloc);
	be_set_transform_proj_function(op_Builtin, gen_Proj_Builtin);
	be_set_transform_proj_function(op_Call,    gen_Proj_Call);
	be_set_transform_proj_function(op_Div,     gen_Proj_Div);
	be_set_transform_proj_function(op_Load,    gen_Proj_Load);
	be_set_transform_proj_function(op_Mod,     gen_Proj_Mod);
	be_set_transform_proj_function(op_Proj,    gen_Proj_Proj);
	be_set_transform_proj_function(op_Start,   gen_Proj_Start);
	be_set_transform_proj_function(op_Store,   gen_Proj_Store);
}

static void riscv_set_allocatable_regs(ir_graph *const irg)
{
	be_irg_t       *const birg = be_birg_from_irg(irg);
	struct obstack *const obst = &birg->obst;

	unsigned *const a = rbitset_obstack_alloc(obst, N_RISCV_REGISTERS);
	for (size_t i = 0; i != ARRAY_SIZE(callee_saves); ++i) {
		rbitset_set(a, callee_saves[i]);
	}
	for (size_t i = 0; i != ARRAY_SIZE(caller_saves); ++i) {
		rbitset_set(a, caller_saves[i]);
	}
	birg->allocatable_regs = a;

	be_cconv_rem_regs(birg->allocatable_regs, ignore_regs, ARRAY_SIZE(ignore_regs));
}

void riscv_transform_graph(ir_graph *const irg)
{
	riscv_register_transformers();
	riscv_set_allocatable_regs(irg);
	be_stack_init(&stack_env);

	ir_entity *const fun_ent  = get_irg_entity(irg);
	ir_type   *const fun_type = get_entity_type(fun_ent);
	riscv_determine_calling_convention(&cur_cconv, fun_type, irg);
	riscv_layout_parameter_entities(&cur_cconv, irg);
	be_add_parameter_entity_stores(irg);
	be_transform_graph(irg, NULL);
	riscv_free_calling_convention(&cur_cconv);

	be_stack_finish(&stack_env);
}
