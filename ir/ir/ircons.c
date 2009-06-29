/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Various irnode constructors. Automatic construction of SSA
 *          representation.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Boris Boesler
            Michael Beck
 * @version $Id$
 */
#include "config.h"

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "irvrfy.h"
#include "irop_t.h"
#include "iropt_t.h"
#include "irgmod.h"
#include "irhooks.h"
#include "array_t.h"
#include "irbackedge_t.h"
#include "irflag_t.h"
#include "iredges_t.h"
#include "irflag_t.h"

/* when we need verifying */
#ifdef NDEBUG
# define IRN_VRFY_IRG(res, irg)
#else
# define IRN_VRFY_IRG(res, irg)  irn_vrfy_irg(res, irg)
#endif /* NDEBUG */

/**
 * Language dependent variable initialization callback.
 */
static uninitialized_local_variable_func_t *default_initialize_local_variable = NULL;

/* creates a bd constructor for a binop */
#define NEW_BD_BINOP(instr)                                     \
static ir_node *                                                \
new_bd_##instr(dbg_info *db, ir_node *block,                    \
       ir_node *op1, ir_node *op2, ir_mode *mode)               \
{                                                               \
  ir_node  *in[2];                                              \
  ir_node  *res;                                                \
  ir_graph *irg = current_ir_graph;                             \
  in[0] = op1;                                                  \
  in[1] = op2;                                                  \
  res = new_ir_node(db, irg, block, op_##instr, mode, 2, in);   \
  res = optimize_node(res);                                     \
  IRN_VRFY_IRG(res, irg);                                       \
  return res;                                                   \
}

/* creates a bd constructor for an unop */
#define NEW_BD_UNOP(instr)                                      \
static ir_node *                                                \
new_bd_##instr(dbg_info *db, ir_node *block,                    \
              ir_node *op, ir_mode *mode)                       \
{                                                               \
  ir_node  *res;                                                \
  ir_graph *irg = current_ir_graph;                             \
  res = new_ir_node(db, irg, block, op_##instr, mode, 1, &op);  \
  res = optimize_node(res);                                     \
  IRN_VRFY_IRG(res, irg);                                       \
  return res;                                                   \
}

/* creates a bd constructor for an divop */
#define NEW_BD_DIVOP(instr)                                     \
static ir_node *                                                \
new_bd_##instr(dbg_info *db, ir_node *block,                    \
            ir_node *memop, ir_node *op1, ir_node *op2, ir_mode *mode, op_pin_state state) \
{                                                               \
  ir_node  *in[3];                                              \
  ir_node  *res;                                                \
  ir_graph *irg = current_ir_graph;                             \
  in[0] = memop;                                                \
  in[1] = op1;                                                  \
  in[2] = op2;                                                  \
  res = new_ir_node(db, irg, block, op_##instr, mode_T, 3, in); \
  res->attr.divmod.exc.pin_state = state;                       \
  res->attr.divmod.resmode = mode;                              \
  res->attr.divmod.no_remainder = 0;                            \
  res = optimize_node(res);                                     \
  IRN_VRFY_IRG(res, irg);                                       \
  return res;                                                   \
}

/* creates a rd constructor for a binop */
#define NEW_RD_BINOP(instr)                                     \
ir_node *                                                       \
new_rd_##instr(dbg_info *db, ir_graph *irg, ir_node *block,     \
       ir_node *op1, ir_node *op2, ir_mode *mode)               \
{                                                               \
  ir_node  *res;                                                \
  ir_graph *rem = current_ir_graph;                             \
  current_ir_graph = irg;                                       \
  res = new_bd_##instr(db, block, op1, op2, mode);              \
  current_ir_graph = rem;                                       \
  return res;                                                   \
}

/* creates a rd constructor for an unop */
#define NEW_RD_UNOP(instr)                                      \
ir_node *                                                       \
new_rd_##instr(dbg_info *db, ir_graph *irg, ir_node *block,     \
              ir_node *op, ir_mode *mode)                       \
{                                                               \
  ir_node  *res;                                                \
  ir_graph *rem = current_ir_graph;                             \
  current_ir_graph = irg;                                       \
  res = new_bd_##instr(db, block, op, mode);                    \
  current_ir_graph = rem;                                       \
  return res;                                                   \
}

/* creates a rd constructor for an divop */
#define NEW_RD_DIVOP(instr)                                     \
ir_node *                                                       \
new_rd_##instr(dbg_info *db, ir_graph *irg, ir_node *block,     \
            ir_node *memop, ir_node *op1, ir_node *op2, ir_mode *mode, op_pin_state state) \
{                                                               \
  ir_node  *res;                                                \
  ir_graph *rem = current_ir_graph;                             \
  current_ir_graph = irg;                                       \
  res = new_bd_##instr(db, block, memop, op1, op2, mode, state);\
  current_ir_graph = rem;                                       \
  return res;                                                   \
}

/* creates a d constructor for an binop */
#define NEW_D_BINOP(instr)                                                    \
ir_node *                                                                     \
new_d_##instr(dbg_info *db, ir_node *op1, ir_node *op2, ir_mode *mode) {      \
  return new_bd_##instr(db, current_ir_graph->current_block, op1, op2, mode); \
}

/* creates a d constructor for an unop */
#define NEW_D_UNOP(instr)                                                     \
ir_node *                                                                     \
new_d_##instr(dbg_info *db, ir_node *op, ir_mode *mode) {                     \
  return new_bd_##instr(db, current_ir_graph->current_block, op, mode);       \
}

#include "gen_ir_cons.c.inl"

static ir_node *
new_bd_Start(dbg_info *db, ir_node *block) {
	ir_node  *res;
	ir_graph *irg = current_ir_graph;

	res = new_ir_node(db, irg, block, op_Start, mode_T, 0, NULL);

	IRN_VRFY_IRG(res, irg);
	return res;
}  /* new_bd_Start */

static ir_node *
new_bd_End(dbg_info *db, ir_node *block) {
	ir_node  *res;
	ir_graph *irg = current_ir_graph;

	res = new_ir_node(db, irg, block, op_End, mode_X, -1, NULL);

	IRN_VRFY_IRG(res, irg);
	return res;
}  /* new_bd_End */

/**
 * Creates a Phi node with all predecessors.  Calling this constructor
 * is only allowed if the corresponding block is mature.
 */
static ir_node *
new_bd_Phi(dbg_info *db, ir_node *block, int arity, ir_node **in, ir_mode *mode) {
	ir_node  *res;
	ir_graph *irg = current_ir_graph;
	int i;
	int has_unknown = 0;

	/* Don't assert that block matured: the use of this constructor is strongly
	   restricted ... */
	if (get_Block_matured(block))
		assert(get_irn_arity(block) == arity);

	res = new_ir_node(db, irg, block, op_Phi, mode, arity, in);

	res->attr.phi.u.backedge = new_backedge_arr(irg->obst, arity);

	for (i = arity - 1; i >= 0; --i)
		if (is_Unknown(in[i])) {
			has_unknown = 1;
			break;
		}

	if (!has_unknown) res = optimize_node(res);
	IRN_VRFY_IRG(res, irg);

	/* Memory Phis in endless loops must be kept alive.
	   As we can't distinguish these easily we keep all of them alive. */
	if (is_Phi(res) && mode == mode_M)
		add_End_keepalive(get_irg_end(irg), res);
	return res;
}  /* new_bd_Phi */

static ir_node *
new_bd_Const_type(dbg_info *db, tarval *con, ir_type *tp) {
	ir_node  *res;
	ir_graph *irg = current_ir_graph;

	res = new_ir_node(db, irg, get_irg_start_block(irg), op_Const, get_tarval_mode(con), 0, NULL);
	res->attr.con.tv = con;
	set_Const_type(res, tp);  /* Call method because of complex assertion. */
	res = optimize_node (res);
	assert(get_Const_type(res) == tp);
	IRN_VRFY_IRG(res, irg);

	return res;
}  /* new_bd_Const_type */

static ir_node *
new_bd_Const(dbg_info *db, tarval *con) {
	ir_graph *irg = current_ir_graph;

	return new_rd_Const_type (db, irg, con, firm_unknown_type);
}  /* new_bd_Const */

static ir_node *
new_bd_Const_long(dbg_info *db, ir_mode *mode, long value) {
	ir_graph *irg = current_ir_graph;

	return new_rd_Const(db, irg, new_tarval_from_long(value, mode));
}  /* new_bd_Const_long */

static ir_node *
new_bd_defaultProj(dbg_info *db, ir_node *block, ir_node *arg,
           long max_proj) {
	ir_node  *res;
	ir_graph *irg = current_ir_graph;

	assert(arg->op == op_Cond);
	arg->attr.cond.kind = fragmentary;
	arg->attr.cond.default_proj = max_proj;
	res = new_rd_Proj (db, irg, block, arg, mode_X, max_proj);
	return res;
}  /* new_bd_defaultProj */

static ir_node *
new_bd_Sel(dbg_info *db, ir_node *block, ir_node *store, ir_node *objptr,
           int arity, ir_node **in, ir_entity *ent) {
	ir_node  **r_in;
	ir_node  *res;
	int      r_arity;
	ir_graph *irg = current_ir_graph;
	ir_mode  *mode = is_Method_type(get_entity_type(ent)) ? mode_P_code : mode_P_data;

	assert(ent != NULL && is_entity(ent) && "entity expected in Sel construction");

	r_arity = arity + 2;
	NEW_ARR_A(ir_node *, r_in, r_arity);  /* uses alloca */
	r_in[0] = store;
	r_in[1] = objptr;
	memcpy(&r_in[2], in, sizeof(ir_node *) * arity);
	/*
	 * Sel's can select functions which should be of mode mode_P_code.
	 */
	res = new_ir_node(db, irg, block, op_Sel, mode, r_arity, r_in);
	res->attr.sel.entity = ent;
	res = optimize_node(res);
	IRN_VRFY_IRG(res, irg);
	return res;
}  /* new_bd_Sel */

static ir_node *
new_bd_SymConst_type(dbg_info *db, ir_node *block, ir_mode *mode,
                     symconst_symbol value,symconst_kind symkind, ir_type *tp) {
	ir_graph *irg = current_ir_graph;
	ir_node  *res = new_ir_node(db, irg, block, op_SymConst, mode, 0, NULL);

	res->attr.symc.kind = symkind;
	res->attr.symc.sym  = value;
	res->attr.symc.tp   = tp;

	res = optimize_node(res);
	IRN_VRFY_IRG(res, irg);
	return res;
}  /* new_bd_SymConst_type */

static ir_node *
new_bd_Sync(dbg_info *db, ir_node *block) {
	ir_node  *res;
	ir_graph *irg = current_ir_graph;

	res = new_ir_node(db, irg, block, op_Sync, mode_M, -1, NULL);
	/* no need to call optimize node here, Sync are always created with no predecessors */
	IRN_VRFY_IRG(res, irg);
	return res;
}  /* new_bd_Sync */


static ir_node *
new_bd_EndReg(dbg_info *db, ir_node *block) {
	ir_node  *res;
	ir_graph *irg = current_ir_graph;

	res = new_ir_node(db, irg, block, op_EndReg, mode_T, -1, NULL);
	set_irg_end_reg(irg, res);
	IRN_VRFY_IRG(res, irg);
	return res;
}  /* new_bd_EndReg */

static ir_node *
new_bd_EndExcept(dbg_info *db, ir_node *block) {
	ir_node  *res;
	ir_graph *irg = current_ir_graph;

	res = new_ir_node(db, irg, block, op_EndExcept, mode_T, -1, NULL);
	set_irg_end_except(irg, res);
	IRN_VRFY_IRG (res, irg);
	return res;
}  /* new_bd_EndExcept */

static ir_node *
new_bd_ASM(dbg_info *db, ir_node *block, int arity, ir_node *in[], ir_asm_constraint *inputs,
           int n_outs, ir_asm_constraint *outputs, int n_clobber, ident *clobber[], ident *asm_text) {
	ir_node  *res;
	ir_graph *irg = current_ir_graph;

	res = new_ir_node(db, irg, block, op_ASM, mode_T, arity, in);
	res->attr.assem.pin_state = op_pin_state_pinned;
	res->attr.assem.inputs    = NEW_ARR_D(ir_asm_constraint, irg->obst, arity);
	res->attr.assem.outputs   = NEW_ARR_D(ir_asm_constraint, irg->obst, n_outs);
	res->attr.assem.clobber   = NEW_ARR_D(ident *, irg->obst, n_clobber);
	res->attr.assem.asm_text  = asm_text;

	memcpy(res->attr.assem.inputs,  inputs,  sizeof(inputs[0]) * arity);
	memcpy(res->attr.assem.outputs, outputs, sizeof(outputs[0]) * n_outs);
	memcpy(res->attr.assem.clobber, clobber, sizeof(clobber[0]) * n_clobber);

	res = optimize_node(res);
	IRN_VRFY_IRG(res, irg);
	return res;
}  /* new_bd_ASM */

/* --------------------------------------------- */
/* private interfaces, for professional use only */
/* --------------------------------------------- */

ir_node *
new_rd_Start(dbg_info *db, ir_graph *irg, ir_node *block) {
	ir_graph *rem = current_ir_graph;
	ir_node  *res;

	current_ir_graph = irg;
	res = new_bd_Start(db, block);
	current_ir_graph = rem;

	return res;
}  /* new_rd_Start */

ir_node *
new_rd_End(dbg_info *db, ir_graph *irg, ir_node *block) {
	ir_node  *res;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;
	res = new_bd_End(db, block);
	current_ir_graph = rem;

	return res;
}  /* new_rd_End */

/* Creates a Phi node with all predecessors.  Calling this constructor
   is only allowed if the corresponding block is mature.  */
ir_node *
new_rd_Phi(dbg_info *db, ir_graph *irg, ir_node *block, int arity, ir_node **in, ir_mode *mode) {
	ir_node  *res;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;
	res = new_bd_Phi(db, block,arity, in, mode);
	current_ir_graph = rem;

	return res;
}  /* new_rd_Phi */

ir_node *
new_rd_Const_type(dbg_info *db, ir_graph *irg, tarval *con, ir_type *tp) {
	ir_node  *res;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;
	res = new_bd_Const_type(db, con, tp);
	current_ir_graph = rem;

	return res;
}  /* new_rd_Const_type */

ir_node *
new_rd_Const(dbg_info *db, ir_graph *irg, tarval *con) {
	ir_node  *res;
//#ifdef USE_ORIGINAL
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;
	res = new_bd_Const_type(db, con, firm_unknown_type);
	current_ir_graph = rem;
//#else
//	res = new_rd_Const_type(db, irg, con, firm_unknown_type);
//#endif

	return res;
}  /* new_rd_Const */

ir_node *
new_rd_Const_long(dbg_info *db, ir_graph *irg, ir_mode *mode, long value) {
	return new_rd_Const(db, irg, new_tarval_from_long(value, mode));
}  /* new_rd_Const_long */

ir_node *
new_rd_defaultProj(dbg_info *db, ir_graph *irg, ir_node *block, ir_node *arg,
                   long max_proj) {
	ir_node  *res;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;
	res = new_bd_defaultProj(db, block, arg, max_proj);
	current_ir_graph = rem;

	return res;
}  /* new_rd_defaultProj */

ir_node *
new_rd_simpleSel(dbg_info *db, ir_graph *irg, ir_node *block,
                 ir_node *store, ir_node *objptr, ir_entity *ent) {
	ir_node  *res;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;
	res = new_bd_Sel(db, block, store, objptr, 0, NULL, ent);
	current_ir_graph = rem;

	return res;
}  /* new_rd_simpleSel */

ir_node *
new_rd_SymConst_type(dbg_info *db, ir_graph *irg, ir_node *block, ir_mode *mode,
                     symconst_symbol value, symconst_kind symkind, ir_type *tp) {
	ir_node  *res;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;
	res = new_bd_SymConst_type(db, block, mode, value, symkind, tp);
	current_ir_graph = rem;

	return res;
}  /* new_rd_SymConst_type */

ir_node *
new_rd_SymConst(dbg_info *db, ir_graph *irg, ir_node *block, ir_mode *mode,
                symconst_symbol value, symconst_kind symkind) {
	return new_rd_SymConst_type(db, irg, block, mode, value, symkind, firm_unknown_type);
}  /* new_rd_SymConst */

 ir_node *new_rd_SymConst_addr_ent(dbg_info *db, ir_graph *irg, ir_mode *mode, ir_entity *symbol, ir_type *tp) {
	symconst_symbol sym;
	sym.entity_p = symbol;
	return new_rd_SymConst_type(db, irg, get_irg_start_block(irg), mode, sym, symconst_addr_ent, tp);
}  /* new_rd_SymConst_addr_ent */

ir_node *new_rd_SymConst_ofs_ent(dbg_info *db, ir_graph *irg, ir_mode *mode, ir_entity *symbol, ir_type *tp) {
	symconst_symbol sym;
	sym.entity_p = symbol;
	return new_rd_SymConst_type(db, irg, get_irg_start_block(irg), mode, sym, symconst_ofs_ent, tp);
}  /* new_rd_SymConst_ofs_ent */

ir_node *new_rd_SymConst_addr_name(dbg_info *db, ir_graph *irg, ir_mode *mode, ident *symbol, ir_type *tp) {
	symconst_symbol sym;
	sym.ident_p = symbol;
	return new_rd_SymConst_type(db, irg, get_irg_start_block(irg), mode, sym, symconst_addr_name, tp);
}  /* new_rd_SymConst_addr_name */

ir_node *new_rd_SymConst_type_tag(dbg_info *db, ir_graph *irg, ir_mode *mode, ir_type *symbol, ir_type *tp) {
	symconst_symbol sym;
	sym.type_p = symbol;
	return new_rd_SymConst_type(db, irg, get_irg_start_block(irg), mode, sym, symconst_type_tag, tp);
}  /* new_rd_SymConst_type_tag */

ir_node *new_rd_SymConst_size(dbg_info *db, ir_graph *irg, ir_mode *mode, ir_type *symbol, ir_type *tp) {
	symconst_symbol sym;
	sym.type_p = symbol;
	return new_rd_SymConst_type(db, irg, get_irg_start_block(irg), mode, sym, symconst_type_size, tp);
}  /* new_rd_SymConst_size */

ir_node *new_rd_SymConst_align(dbg_info *db, ir_graph *irg, ir_mode *mode, ir_type *symbol, ir_type *tp) {
	symconst_symbol sym;
	sym.type_p = symbol;
	return new_rd_SymConst_type(db, irg, get_irg_start_block(irg), mode, sym, symconst_type_align, tp);
}  /* new_rd_SymConst_align */

ir_node *
new_rd_Sync(dbg_info *db, ir_graph *irg, ir_node *block, int arity, ir_node *in[]) {
	ir_node  *res;
	ir_graph *rem = current_ir_graph;
	int      i;

	current_ir_graph = irg;
	res = new_bd_Sync(db, block);
	current_ir_graph = rem;

	for (i = 0; i < arity; ++i)
		add_Sync_pred(res, in[i]);

	return res;
}  /* new_rd_Sync */

ir_node *
new_rd_EndReg(dbg_info *db, ir_graph *irg, ir_node *block) {
	ir_node *res;

	res = new_ir_node(db, irg, block, op_EndReg, mode_T, -1, NULL);
	set_irg_end_reg(irg, res);
	IRN_VRFY_IRG(res, irg);
	return res;
}  /* new_rd_EndReg */

ir_node *
new_rd_EndExcept(dbg_info *db, ir_graph *irg, ir_node *block) {
	ir_node *res;

	res = new_ir_node(db, irg, block, op_EndExcept, mode_T, -1, NULL);
	set_irg_end_except(irg, res);
	IRN_VRFY_IRG (res, irg);
	return res;
}  /* new_rd_EndExcept */

ir_node *new_rd_ASM(dbg_info *db, ir_graph *irg, ir_node *block,
                    int arity, ir_node *in[], ir_asm_constraint *inputs,
                    int n_outs, ir_asm_constraint *outputs,
                    int n_clobber, ident *clobber[], ident *asm_text) {
	ir_node  *res;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;
	res = new_bd_ASM(db, block, arity, in, inputs, n_outs, outputs, n_clobber, clobber, asm_text);
	current_ir_graph = rem;

	return res;
}  /* new_rd_ASM */

ir_node *new_r_Start(ir_graph *irg, ir_node *block) {
	return new_rd_Start(NULL, irg, block);
}
ir_node *new_r_End(ir_graph *irg, ir_node *block) {
	return new_rd_End(NULL, irg, block);
}
ir_node *new_r_Const(ir_graph *irg, tarval *con) {
	return new_rd_Const(NULL, irg, con);
}
ir_node *new_r_Const_long(ir_graph *irg, ir_mode *mode, long value) {
	return new_rd_Const_long(NULL, irg, mode, value);
}
ir_node *new_r_Const_type(ir_graph *irg, tarval *con, ir_type *tp) {
	return new_rd_Const_type(NULL, irg, con, tp);
}
ir_node *new_r_SymConst(ir_graph *irg, ir_node *block, ir_mode *mode,
                        symconst_symbol value, symconst_kind symkind) {
	return new_rd_SymConst(NULL, irg, block, mode, value, symkind);
}
ir_node *new_r_simpleSel(ir_graph *irg, ir_node *block, ir_node *store,
                         ir_node *objptr, ir_entity *ent) {
	return new_rd_Sel(NULL, irg, block, store, objptr, 0, NULL, ent);
}
ir_node *new_r_Phi(ir_graph *irg, ir_node *block, int arity,
                   ir_node **in, ir_mode *mode) {
	return new_rd_Phi(NULL, irg, block, arity, in, mode);
}
ir_node *new_r_Sync(ir_graph *irg, ir_node *block, int arity, ir_node *in[]) {
	return new_rd_Sync(NULL, irg, block, arity, in);
}
ir_node *new_r_defaultProj(ir_graph *irg, ir_node *block, ir_node *arg,
                           long max_proj) {
	return new_rd_defaultProj(NULL, irg, block, arg, max_proj);
}
ir_node *new_r_Bad(ir_graph *irg) {
	return get_irg_bad(irg);
}
ir_node *new_r_EndReg(ir_graph *irg, ir_node *block) {
	return new_rd_EndReg(NULL, irg, block);
}
ir_node *new_r_EndExcept(ir_graph *irg, ir_node *block) {
	return new_rd_EndExcept(NULL, irg, block);
}
ir_node *new_r_NoMem(ir_graph *irg) {
	return get_irg_no_mem(irg);
}
ir_node *new_r_ASM(ir_graph *irg, ir_node *block,
                   int arity, ir_node *in[], ir_asm_constraint *inputs,
                   int n_outs, ir_asm_constraint *outputs,
                   int n_clobber, ident *clobber[], ident *asm_text) {
	return new_rd_ASM(NULL, irg, block, arity, in, inputs, n_outs, outputs, n_clobber, clobber, asm_text);
}

/** ********************/
/** public interfaces  */
/** construction tools */

ir_node *
new_d_Start(dbg_info *db) {
	ir_node *res;

	res = new_ir_node(db, current_ir_graph, current_ir_graph->current_block,
	                  op_Start, mode_T, 0, NULL);

	res = optimize_node(res);
	IRN_VRFY_IRG(res, current_ir_graph);
	return res;
}  /* new_d_Start */

ir_node *
new_d_End(dbg_info *db) {
	ir_node *res;
	res = new_ir_node(db, current_ir_graph,  current_ir_graph->current_block,
	                  op_End, mode_X, -1, NULL);
	res = optimize_node(res);
	IRN_VRFY_IRG(res, current_ir_graph);

	return res;
}  /* new_d_End */

/* ***********************************************************************/
/* Methods necessary for automatic Phi node creation                     */
/*
  ir_node *phi_merge            (ir_node *block, int pos, ir_mode *mode, ir_node **nin, int ins)
  ir_node *get_r_value_internal (ir_node *block, int pos, ir_mode *mode);
  ir_node *new_rd_Phi0          (ir_graph *irg, ir_node *block, ir_mode *mode)
  ir_node *new_rd_Phi_in        (ir_graph *irg, ir_node *block, ir_mode *mode, ir_node **in, int ins)

  Call Graph:   ( A ---> B == A "calls" B)

       get_value         mature_immBlock
          |                   |
          |                   |
          |                   |
          |          ---> phi_merge
          |         /       /   \
          |        /       /     \
         \|/      /      |/_      \
       get_r_value_internal        |
                |                  |
                |                  |
               \|/                \|/
           new_rd_Phi0          new_rd_Phi_in

* *************************************************************************** */

/** Creates a Phi node with 0 predecessors. */
static inline ir_node *
new_rd_Phi0(ir_graph *irg, ir_node *block, ir_mode *mode) {
	ir_node *res;

	res = new_ir_node(NULL, irg, block, op_Phi, mode, 0, NULL);
	IRN_VRFY_IRG(res, irg);
	return res;
}  /* new_rd_Phi0 */


/**
 * Internal constructor of a Phi node by a phi_merge operation.
 *
 * @param irg    the graph on which the Phi will be constructed
 * @param block  the block in which the Phi will be constructed
 * @param mode   the mod eof the Phi node
 * @param in     the input array of the phi node
 * @param ins    number of elements in the input array
 * @param phi0   in non-NULL: the Phi0 node in the same block that represents
 *               the value for which the new Phi is constructed
 */
static inline ir_node *
new_rd_Phi_in(ir_graph *irg, ir_node *block, ir_mode *mode,
              ir_node **in, int ins, ir_node *phi0) {
	int i;
	ir_node *res, *known;

	/* Allocate a new node on the obstack.  The allocation copies the in
	   array. */
	res = new_ir_node(NULL, irg, block, op_Phi, mode, ins, in);
	res->attr.phi.u.backedge = new_backedge_arr(irg->obst, ins);

	/* This loop checks whether the Phi has more than one predecessor.
	   If so, it is a real Phi node and we break the loop.  Else the
	   Phi node merges the same definition on several paths and therefore
	   is not needed.
	   Note: We MUST consider Bad nodes, else we might get data flow cycles in dead loops! */
	known = res;
	for (i = ins - 1; i >= 0; --i) 	{
		assert(in[i]);

		in[i] = skip_Id(in[i]);  /* increases the number of freed Phis. */

		/* Optimize self referencing Phis:  We can't detect them yet properly, as
		they still refer to the Phi0 they will replace.  So replace right now. */
		if (phi0 && in[i] == phi0)
			in[i] = res;

		if (in[i] == res || in[i] == known)
			continue;

		if (known == res)
			known = in[i];
		else
			break;
	}

	/* i < 0: there is at most one predecessor, we don't need a phi node. */
	if (i < 0) {
		if (res != known) {
			edges_node_deleted(res, current_ir_graph);
			obstack_free(current_ir_graph->obst, res);
			if (is_Phi(known)) {
				/* If pred is a phi node we want to optimize it: If loops are matured in a bad
				   order, an enclosing Phi know may get superfluous. */
				res = optimize_in_place_2(known);
				if (res != known)
					exchange(known, res);
			}
			else
				res = known;
		} else {
			/* A undefined value, e.g., in unreachable code. */
			res = new_Bad();
		}
	} else {
		res = optimize_node(res);  /* This is necessary to add the node to the hash table for cse. */
		IRN_VRFY_IRG(res, irg);
		/* Memory Phis in endless loops must be kept alive.
		   As we can't distinguish these easily we keep all of them alive. */
		if (is_Phi(res) && mode == mode_M)
			add_End_keepalive(get_irg_end(irg), res);
	}

	return res;
}  /* new_rd_Phi_in */

static ir_node *
get_r_value_internal(ir_node *block, int pos, ir_mode *mode);

static ir_node *
phi_merge(ir_node *block, int pos, ir_mode *mode, ir_node **nin, int ins);

/**
 * Construct a new frag_array for node n.
 * Copy the content from the current graph_arr of the corresponding block:
 * this is the current state.
 * Set ProjM(n) as current memory state.
 * Further the last entry in frag_arr of current block points to n.  This
 * constructs a chain block->last_frag_op-> ... first_frag_op of all frag ops in the block.
 */
static inline ir_node **new_frag_arr(ir_node *n) {
	ir_node **arr;
	int opt;

	arr = NEW_ARR_D (ir_node *, current_ir_graph->obst, current_ir_graph->n_loc);
	memcpy(arr, current_ir_graph->current_block->attr.block.graph_arr,
	       sizeof(ir_node *)*current_ir_graph->n_loc);

	/* turn off optimization before allocating Proj nodes, as res isn't
	   finished yet. */
	opt = get_opt_optimize(); set_optimize(0);
	/* Here we rely on the fact that all frag ops have Memory as first result! */
	if (is_Call(n)) {
		arr[0] = new_Proj(n, mode_M, pn_Call_M_except);
	} else if (is_CopyB(n)) {
		arr[0] = new_Proj(n, mode_M, pn_CopyB_M_except);
	} else {
		assert((pn_Quot_M == pn_DivMod_M) &&
		       (pn_Quot_M == pn_Div_M)    &&
		       (pn_Quot_M == pn_Mod_M)    &&
		       (pn_Quot_M == pn_Load_M)   &&
		       (pn_Quot_M == pn_Store_M)  &&
		       (pn_Quot_M == pn_Alloc_M)  &&
		       (pn_Quot_M == pn_Bound_M));
		arr[0] = new_Proj(n, mode_M, pn_Alloc_M);
	}
	set_optimize(opt);

	current_ir_graph->current_block->attr.block.graph_arr[current_ir_graph->n_loc-1] = n;
	return arr;
}  /* new_frag_arr */

/**
 * Returns the frag_arr from a node.
 */
static inline ir_node **get_frag_arr(ir_node *n) {
	switch (get_irn_opcode(n)) {
	case iro_Call:
		return n->attr.call.exc.frag_arr;
	case iro_Alloc:
		return n->attr.alloc.exc.frag_arr;
	case iro_Load:
		return n->attr.load.exc.frag_arr;
	case iro_Store:
		return n->attr.store.exc.frag_arr;
	default:
		return n->attr.except.frag_arr;
	}
}  /* get_frag_arr */

static void
set_frag_value(ir_node **frag_arr, int pos, ir_node *val) {
#ifdef DEBUG_libfirm
	int i;

	for (i = 1024; i >= 0; --i)
#else
	for (;;)
#endif
	{
		if (frag_arr[pos] == NULL)
			frag_arr[pos] = val;
		if (frag_arr[current_ir_graph->n_loc - 1] != NULL) {
			ir_node **arr = get_frag_arr(frag_arr[current_ir_graph->n_loc - 1]);
			assert(arr != frag_arr && "Endless recursion detected");
			frag_arr = arr;
		} else
			return;
	}
	assert(!"potential endless recursion in set_frag_value");
}  /* set_frag_value */

static ir_node *
get_r_frag_value_internal(ir_node *block, ir_node *cfOp, int pos, ir_mode *mode) {
	ir_node *res;
	ir_node **frag_arr;

	assert(is_fragile_op(cfOp) && !is_Bad(cfOp));

	frag_arr = get_frag_arr(cfOp);
	res = frag_arr[pos];
	if (res == NULL) {
		if (block->attr.block.graph_arr[pos] != NULL) {
			/* There was a set_value() after the cfOp and no get_value() before that
			   set_value().  We must build a Phi node now. */
			if (block->attr.block.is_matured) {
				int ins = get_irn_arity(block);
				ir_node **nin;
				NEW_ARR_A(ir_node *, nin, ins);
				res = phi_merge(block, pos, mode, nin, ins);
			} else {
				res = new_rd_Phi0(current_ir_graph, block, mode);
				res->attr.phi.u.pos    = pos;
				res->attr.phi.next     = block->attr.block.phis;
				block->attr.block.phis = res;
			}
			assert(res != NULL);
			/* It's a Phi, we can write this into all graph_arrs with NULL */
			set_frag_value(block->attr.block.graph_arr, pos, res);
		} else {
			res = get_r_value_internal(block, pos, mode);
			set_frag_value(block->attr.block.graph_arr, pos, res);
		}
	}
	return res;
}  /* get_r_frag_value_internal */

/**
 * Check whether a control flownode  cf_pred represents an exception flow.
 *
 * @param cf_pred     the control flow node
 * @param prev_cf_op  if cf_pred is a Proj, the predecessor node, else equal to cf_pred
 */
static int is_exception_flow(ir_node *cf_pred, ir_node *prev_cf_op) {
	/*
	 * Note: all projections from a raise are "exceptional control flow" we we handle it
	 * like a normal Jmp, because there is no "regular" one.
	 * That's why Raise is no "fragile_op"!
	 */
	if (is_fragile_op(prev_cf_op)) {
		if (is_Proj(cf_pred)) {
			if (get_Proj_proj(cf_pred) == pn_Generic_X_regular) {
				/* the regular control flow, NO exception */
				return 0;
			}
			assert(get_Proj_proj(cf_pred) == pn_Generic_X_except);
			return 1;
		}
		/* Hmm, exception but not a Proj? */
		assert(!"unexpected condition: fragile op without a proj");
		return 1;
	}
	return 0;
}  /* is_exception_flow */

/**
 * Computes the predecessors for the real phi node, and then
 * allocates and returns this node.  The routine called to allocate the
 * node might optimize it away and return a real value.
 * This function must be called with an in-array of proper size.
 */
static ir_node *
phi_merge(ir_node *block, int pos, ir_mode *mode, ir_node **nin, int ins) {
	ir_node *prevBlock, *res, *phi0, *phi0_all;
	int i;

	/* If this block has no value at pos create a Phi0 and remember it
	   in graph_arr to break recursions.
	   Else we may not set graph_arr as there a later value is remembered. */
	phi0 = NULL;
	if (block->attr.block.graph_arr[pos] == NULL) {
		ir_graph *irg = current_ir_graph;

		if (block == get_irg_start_block(irg)) {
 			/* Collapsing to Bad tarvals is no good idea.
 			   So we call a user-supplied routine here that deals with this case as
 			   appropriate for the given language. Sorrily the only help we can give
 			   here is the position.

 			   Even if all variables are defined before use, it can happen that
 			   we get to the start block, if a Cond has been replaced by a tuple
 			   (bad, jmp).  In this case we call the function needlessly, eventually
 			   generating an non existent error.
 			   However, this SHOULD NOT HAPPEN, as bad control flow nodes are intercepted
 			   before recurring.
			 */
			if (default_initialize_local_variable != NULL) {
				ir_node *rem = get_cur_block();

				set_cur_block(block);
				block->attr.block.graph_arr[pos] = default_initialize_local_variable(irg, mode, pos - 1);
				set_cur_block(rem);
			}
			else
				block->attr.block.graph_arr[pos] = new_Unknown(mode);
			/* We don't need to care about exception ops in the start block.
			   There are none by definition. */
			return block->attr.block.graph_arr[pos];
		} else {
			phi0 = new_rd_Phi0(irg, block, mode);
			block->attr.block.graph_arr[pos] = phi0;
			if (get_opt_precise_exc_context()) {
				/* Set graph_arr for fragile ops.  Also here we should break recursion.
				   We could choose a cyclic path through an cfop.  But the recursion would
				   break at some point. */
				set_frag_value(block->attr.block.graph_arr, pos, phi0);
			}
		}
	}

	/* This loop goes to all predecessor blocks of the block the Phi node
	   is in and there finds the operands of the Phi node by calling
	   get_r_value_internal.  */
	for (i = 1; i <= ins; ++i) {
		ir_node *cf_pred = block->in[i];
		ir_node *prevCfOp = skip_Proj(cf_pred);
		assert(prevCfOp);
		if (is_Bad(prevCfOp)) {
			/* In case a Cond has been optimized we would get right to the start block
			with an invalid definition. */
			nin[i-1] = new_Bad();
			continue;
		}
		prevBlock = prevCfOp->in[0]; /* go past control flow op to prev block */
		assert(prevBlock);
		if (!is_Bad(prevBlock)) {
			if (get_opt_precise_exc_context() && is_exception_flow(cf_pred, prevCfOp)) {
				assert(get_r_frag_value_internal(prevBlock, prevCfOp, pos, mode));
				nin[i-1] = get_r_frag_value_internal(prevBlock, prevCfOp, pos, mode);
			} else
				nin[i-1] = get_r_value_internal(prevBlock, pos, mode);
		} else {
			nin[i-1] = new_Bad();
		}
	}

	/* We want to pass the Phi0 node to the constructor: this finds additional
	   optimization possibilities.
	   The Phi0 node either is allocated in this function, or it comes from
	   a former call to get_r_value_internal(). In this case we may not yet
	   exchange phi0, as this is done in mature_immBlock(). */
	if (phi0 == NULL) {
		phi0_all = block->attr.block.graph_arr[pos];
		if (! is_Phi0(phi0_all)            ||
		    get_irn_arity(phi0_all) != 0   ||
		    get_nodes_block(phi0_all) != block)
			phi0_all = NULL;
	} else {
		phi0_all = phi0;
	}

	/* After collecting all predecessors into the array nin a new Phi node
	   with these predecessors is created.  This constructor contains an
	   optimization: If all predecessors of the Phi node are identical it
	   returns the only operand instead of a new Phi node.  */
	res = new_rd_Phi_in(current_ir_graph, block, mode, nin, ins, phi0_all);

	/* In case we allocated a Phi0 node at the beginning of this procedure,
	   we need to exchange this Phi0 with the real Phi. */
	if (phi0 != NULL) {
		exchange(phi0, res);
		block->attr.block.graph_arr[pos] = res;
		/* Don't set_frag_value as it does not overwrite.  Doesn't matter, is
		   only an optimization. */
	}

	return res;
}  /* phi_merge */

/**
 * This function returns the last definition of a value.  In case
 * this value was last defined in a previous block, Phi nodes are
 * inserted.  If the part of the firm graph containing the definition
 * is not yet constructed, a dummy Phi node is returned.
 *
 * @param block   the current block
 * @param pos     the value number of the value searched
 * @param mode    the mode of this value (needed for Phi construction)
 */
static ir_node *
get_r_value_internal(ir_node *block, int pos, ir_mode *mode) {
	ir_node *res;
	/* There are 4 cases to treat.

	   1. The block is not mature and we visit it the first time.  We can not
	      create a proper Phi node, therefore a Phi0, i.e., a Phi without
	      predecessors is returned.  This node is added to the linked list (block
	      attribute "phis") of the containing block to be completed when this block is
	      matured. (Completion will add a new Phi and turn the Phi0 into an Id
	      node.)

	   2. The value is already known in this block, graph_arr[pos] is set and we
	      visit the block the first time.  We can return the value without
	      creating any new nodes.

	   3. The block is mature and we visit it the first time.  A Phi node needs
	      to be created (phi_merge).  If the Phi is not needed, as all it's
	      operands are the same value reaching the block through different
	      paths, it's optimized away and the value itself is returned.

	   4. The block is mature, and we visit it the second time.  Now two
	      subcases are possible:
	      * The value was computed completely the last time we were here. This
	        is the case if there is no loop.  We can return the proper value.
	      * The recursion that visited this node and set the flag did not
	        return yet.  We are computing a value in a loop and need to
	        break the recursion.  This case only happens if we visited
	    the same block with phi_merge before, which inserted a Phi0.
	    So we return the Phi0.
	*/

	/* case 4 -- already visited. */
	if (get_irn_visited(block) == get_irg_visited(current_ir_graph)) {
		/* As phi_merge allocates a Phi0 this value is always defined. Here
		is the critical difference of the two algorithms. */
		assert(block->attr.block.graph_arr[pos]);
		return block->attr.block.graph_arr[pos];
	}

	/* visited the first time */
	set_irn_visited(block, get_irg_visited(current_ir_graph));

	/* Get the local valid value */
	res = block->attr.block.graph_arr[pos];

	/* case 2 -- If the value is actually computed, return it. */
	if (res != NULL)
		return res;

	if (block->attr.block.is_matured) { /* case 3 */

		/* The Phi has the same amount of ins as the corresponding block. */
		int ins = get_irn_arity(block);
		ir_node **nin;
		NEW_ARR_A(ir_node *, nin, ins);

		/* Phi merge collects the predecessors and then creates a node. */
		res = phi_merge(block, pos, mode, nin, ins);

	} else {  /* case 1 */
		/* The block is not mature, we don't know how many in's are needed.  A Phi
		   with zero predecessors is created.  Such a Phi node is called Phi0
		   node.  The Phi0 is then added to the list of Phi0 nodes in this block
		   to be matured by mature_immBlock later.
		   The Phi0 has to remember the pos of it's internal value.  If the real
		   Phi is computed, pos is used to update the array with the local
		   values. */
		res = new_rd_Phi0(current_ir_graph, block, mode);
		res->attr.phi.u.pos    = pos;
		res->attr.phi.next     = block->attr.block.phis;
		block->attr.block.phis = res;
	}

	assert(is_ir_node(res) && "phi_merge() failed to construct a definition");

	/* The local valid value is available now. */
	block->attr.block.graph_arr[pos] = res;

	return res;
}  /* get_r_value_internal */

/* ************************************************************************** */

/*
 * Finalize a Block node, when all control flows are known.
 * Acceptable parameters are only Block nodes.
 */
void
mature_immBlock(ir_node *block) {
	int ins;
	ir_node *n, **nin;
	ir_node *next;

	assert(is_Block(block));
	if (!get_Block_matured(block)) {
		ir_graph *irg = current_ir_graph;

		ins = ARR_LEN(block->in) - 1;
		/* Fix block parameters */
		block->attr.block.backedge = new_backedge_arr(irg->obst, ins);

		/* An array for building the Phi nodes. */
		NEW_ARR_A(ir_node *, nin, ins);

		/* Traverse a chain of Phi nodes attached to this block and mature
		these, too. **/
		for (n = block->attr.block.phis; n; n = next) {
			inc_irg_visited(irg);
			next = n->attr.phi.next;
			exchange(n, phi_merge(block, n->attr.phi.u.pos, n->mode, nin, ins));
		}

		block->attr.block.is_matured = 1;

		/* Now, as the block is a finished Firm node, we can optimize it.
		   Since other nodes have been allocated since the block was created
		   we can not free the node on the obstack.  Therefore we have to call
		   optimize_in_place().
		   Unfortunately the optimization does not change a lot, as all allocated
		   nodes refer to the unoptimized node.
		   We can call optimize_in_place_2(), as global cse has no effect on blocks. */
		block = optimize_in_place_2(block);
		IRN_VRFY_IRG(block, irg);
	}
}  /* mature_immBlock */

ir_node *
new_d_Phi(dbg_info *db, int arity, ir_node **in, ir_mode *mode) {
	return new_bd_Phi(db, current_ir_graph->current_block, arity, in, mode);
}  /* new_d_Phi */

ir_node *
new_d_Const(dbg_info *db, tarval *con) {
	return new_bd_Const(db, con);
}  /* new_d_Const */

ir_node *
new_d_Const_long(dbg_info *db, ir_mode *mode, long value) {
	return new_bd_Const_long(db, mode, value);
}  /* new_d_Const_long */

ir_node *
new_d_Const_type(dbg_info *db, tarval *con, ir_type *tp) {
	return new_bd_Const_type(db, con, tp);
}  /* new_d_Const_type */


ir_node *
new_d_defaultProj(dbg_info *db, ir_node *arg, long max_proj) {
	ir_node *res;
	assert(arg->op == op_Cond);
	arg->attr.cond.kind = fragmentary;
	arg->attr.cond.default_proj = max_proj;
	res = new_d_Proj(db, arg, mode_X, max_proj);
	return res;
}  /* new_d_defaultProj */

/**
 * Allocate a frag array for a node if the current graph state is phase_building.
 *
 * @param irn         the node for which the frag array should be allocated
 * @param op          the opcode of the (original) node, if does not match opcode of irn,
 *                    nothing is done
 * @param frag_store  the address of the frag store in irn attributes, if this
 *                    address contains a value != NULL, does nothing
 */
void firm_alloc_frag_arr(ir_node *irn, ir_op *op, ir_node ***frag_store) {
	if (get_opt_precise_exc_context()) {
		if ((current_ir_graph->phase_state == phase_building) &&
		    (get_irn_op(irn) == op) && /* Could be optimized away. */
		    !*frag_store)    /* Could be a cse where the arr is already set. */ {
			*frag_store = new_frag_arr(irn);
		}
	}
}  /* firm_alloc_frag_arr */

ir_node *
new_d_simpleSel(dbg_info *db, ir_node *store, ir_node *objptr, ir_entity *ent)
/* GL: objptr was called frame before.  Frame was a bad choice for the name
   as the operand could as well be a pointer to a dynamic object. */
{
	return new_bd_Sel(db, current_ir_graph->current_block,
	                  store, objptr, 0, NULL, ent);
}  /* new_d_simpleSel */

ir_node *
new_d_SymConst_type(dbg_info *db, ir_mode *mode, symconst_symbol value, symconst_kind kind, ir_type *tp) {
	return new_bd_SymConst_type(db, get_irg_start_block(current_ir_graph), mode,
	                            value, kind, tp);
}  /* new_d_SymConst_type */

ir_node *
new_d_SymConst(dbg_info *db, ir_mode *mode, symconst_symbol value, symconst_kind kind) {
	return new_bd_SymConst_type(db, get_irg_start_block(current_ir_graph), mode,
	                            value, kind, firm_unknown_type);
}  /* new_d_SymConst */

ir_node *
new_d_Sync(dbg_info *db, int arity, ir_node *in[]) {
	return new_rd_Sync(db, current_ir_graph, current_ir_graph->current_block, arity, in);
}  /* new_d_Sync */

ir_node *
new_d_EndReg(dbg_info *db) {
	return new_bd_EndReg(db, current_ir_graph->current_block);
}  /* new_d_EndReg */

ir_node *
new_d_EndExcept(dbg_info *db) {
	return new_bd_EndExcept(db, current_ir_graph->current_block);
}  /* new_d_EndExcept */


ir_node *
new_d_ASM(dbg_info *db, int arity, ir_node *in[], ir_asm_constraint *inputs,
          int n_outs, ir_asm_constraint *outputs,
          int n_clobber, ident *clobber[], ident *asm_text) {
	return new_bd_ASM(db, current_ir_graph->current_block, arity, in, inputs, n_outs, outputs, n_clobber, clobber, asm_text);
}  /* new_d_ASM */

/* ********************************************************************* */
/* Comfortable interface with automatic Phi node construction.           */
/* (Uses also constructors of ?? interface, except new_Block.            */
/* ********************************************************************* */

/*  Block construction */
/* immature Block without predecessors */
ir_node *
new_d_immBlock(dbg_info *db) {
	ir_node *res;

	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	/* creates a new dynamic in-array as length of in is -1 */
	res = new_ir_node(db, current_ir_graph, NULL, op_Block, mode_BB, -1, NULL);

	/* macroblock head */
	res->in[0] = res;

	res->attr.block.is_matured  = 0;
	res->attr.block.is_dead     = 0;
	res->attr.block.is_mb_head  = 1;
	res->attr.block.irg.irg     = current_ir_graph;
	res->attr.block.backedge    = NULL;
	res->attr.block.in_cg       = NULL;
	res->attr.block.cg_backedge = NULL;
	res->attr.block.extblk      = NULL;
	res->attr.block.region      = NULL;
	res->attr.block.mb_depth    = 0;
	res->attr.block.entity      = NULL;

	set_Block_block_visited(res, 0);

	/* Create and initialize array for Phi-node construction. */
	res->attr.block.graph_arr = NEW_ARR_D(ir_node *, current_ir_graph->obst,
	                                      current_ir_graph->n_loc);
	memset(res->attr.block.graph_arr, 0, sizeof(ir_node *)*current_ir_graph->n_loc);

	/* Immature block may not be optimized! */
	IRN_VRFY_IRG(res, current_ir_graph);

	return res;
}  /* new_d_immBlock */

ir_node *
new_immBlock(void) {
	return new_d_immBlock(NULL);
}  /* new_immBlock */

/* immature PartBlock with its predecessors */
ir_node *
new_d_immPartBlock(dbg_info *db, ir_node *pred_jmp) {
	ir_node *res = new_d_immBlock(db);
	ir_node *blk = get_nodes_block(pred_jmp);

	res->in[0] = blk->in[0];
	assert(res->in[0] != NULL);
	add_immBlock_pred(res, pred_jmp);

	res->attr.block.is_mb_head = 0;
	res->attr.block.mb_depth = blk->attr.block.mb_depth + 1;

	return res;
}  /* new_d_immPartBlock */

ir_node *
new_immPartBlock(ir_node *pred_jmp) {
	return new_d_immPartBlock(NULL, pred_jmp);
}  /* new_immPartBlock */

/* add an edge to a jmp/control flow node */
void
add_immBlock_pred(ir_node *block, ir_node *jmp) {
	int n = ARR_LEN(block->in) - 1;

	assert(!block->attr.block.is_matured && "Error: Block already matured!\n");
	assert(block->attr.block.is_mb_head && "Error: Cannot add a predecessor to a PartBlock");
	assert(is_ir_node(jmp));

	ARR_APP1(ir_node *, block->in, jmp);
	/* Call the hook */
	hook_set_irn_n(block, n, jmp, NULL);
}  /* add_immBlock_pred */

/* changing the current block */
void
set_cur_block(ir_node *target) {
	current_ir_graph->current_block = target;
}  /* set_cur_block */

/* ************************ */
/* parameter administration */

/* get a value from the parameter array from the current block by its index */
ir_node *
get_d_value(dbg_info *db, int pos, ir_mode *mode) {
	ir_graph *irg = current_ir_graph;
	assert(get_irg_phase_state(irg) == phase_building);
	inc_irg_visited(irg);
	(void) db;

	assert(pos >= 0);

	return get_r_value_internal(irg->current_block, pos + 1, mode);
}  /* get_d_value */

/* get a value from the parameter array from the current block by its index */
ir_node *
get_value(int pos, ir_mode *mode) {
	return get_d_value(NULL, pos, mode);
}  /* get_value */

/* set a value at position pos in the parameter array from the current block */
void
set_value(int pos, ir_node *value) {
	ir_graph *irg = current_ir_graph;
	assert(get_irg_phase_state(irg) == phase_building);
	assert(pos >= 0);
	assert(pos+1 < irg->n_loc);
	assert(is_ir_node(value));
	irg->current_block->attr.block.graph_arr[pos + 1] = value;
}  /* set_value */

/* Find the value number for a node in the current block.*/
int
find_value(ir_node *value) {
	int i;
	ir_node *bl = current_ir_graph->current_block;

	for (i = ARR_LEN(bl->attr.block.graph_arr) - 1; i >= 1; --i)
		if (bl->attr.block.graph_arr[i] == value)
			return i - 1;
	return -1;
}  /* find_value */

/* get the current store */
ir_node *
get_store(void) {
	ir_graph *irg = current_ir_graph;

	assert(get_irg_phase_state(irg) == phase_building);
	/* GL: one could call get_value instead */
	inc_irg_visited(irg);
	return get_r_value_internal(irg->current_block, 0, mode_M);
}  /* get_store */

/* set the current store: handles automatic Sync construction for Load nodes */
void
set_store(ir_node *store) {
	ir_node *load, *pload, *pred, *in[2];

	assert(get_irg_phase_state(current_ir_graph) == phase_building);
	/* Beware: due to dead code elimination, a store might become a Bad node even in
	   the construction phase. */
	assert((get_irn_mode(store) == mode_M || is_Bad(store)) && "storing non-memory node");

	if (get_opt_auto_create_sync()) {
		/* handle non-volatile Load nodes by automatically creating Sync's */
		load = skip_Proj(store);
		if (is_Load(load) && get_Load_volatility(load) == volatility_non_volatile) {
			pred = get_Load_mem(load);

			if (is_Sync(pred)) {
				/* a Load after a Sync: move it up */
				ir_node *mem = skip_Proj(get_Sync_pred(pred, 0));

				set_Load_mem(load, get_memop_mem(mem));
				add_Sync_pred(pred, store);
				store = pred;
			} else {
				pload = skip_Proj(pred);
				if (is_Load(pload) && get_Load_volatility(pload) == volatility_non_volatile) {
					/* a Load after a Load: create a new Sync */
					set_Load_mem(load, get_Load_mem(pload));

					in[0] = pred;
					in[1] = store;
					store = new_Sync(2, in);
				}
			}
		}
	}
	current_ir_graph->current_block->attr.block.graph_arr[0] = store;
}  /* set_store */

void
keep_alive(ir_node *ka) {
	add_End_keepalive(get_irg_end(current_ir_graph), ka);
}  /* keep_alive */

/* --- Useful access routines --- */
/* Returns the current block of the current graph.  To set the current
   block use set_cur_block. */
ir_node *get_cur_block(void) {
	return get_irg_current_block(current_ir_graph);
}  /* get_cur_block */

/* Returns the frame type of the current graph */
ir_type *get_cur_frame_type(void) {
	return get_irg_frame_type(current_ir_graph);
}  /* get_cur_frame_type */


/* ********************************************************************* */
/* initialize */

/* call once for each run of the library */
void
firm_init_cons(uninitialized_local_variable_func_t *func) {
	default_initialize_local_variable = func;
}  /* firm_init_cons */

void
irp_finalize_cons(void) {
	int i;
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		irg_finalize_cons(get_irp_irg(i));
	}
	irp->phase_state = phase_high;
}  /* irp_finalize_cons */

ir_node *new_Start(void) {
	return new_d_Start(NULL);
}
ir_node *new_End(void) {
	return new_d_End(NULL);
}
ir_node *new_Const(tarval *con) {
	return new_d_Const(NULL, con);
}

ir_node *new_Const_long(ir_mode *mode, long value) {
	return new_d_Const_long(NULL, mode, value);
}

ir_node *new_Const_type(tarval *con, ir_type *tp) {
	return new_d_Const_type(NULL, con, tp);
}

ir_node *new_SymConst_type(ir_mode *mode, symconst_symbol value, symconst_kind kind, ir_type *type) {
	return new_d_SymConst_type(NULL, mode, value, kind, type);
}
ir_node *new_SymConst(ir_mode *mode, symconst_symbol value, symconst_kind kind) {
	return new_d_SymConst(NULL, mode, value, kind);
}
ir_node *new_simpleSel(ir_node *store, ir_node *objptr, ir_entity *ent) {
	return new_d_simpleSel(NULL, store, objptr, ent);
}
ir_node *new_Phi(int arity, ir_node **in, ir_mode *mode) {
	return new_d_Phi(NULL, arity, in, mode);
}
ir_node *new_Sync(int arity, ir_node *in[]) {
	return new_d_Sync(NULL, arity, in);
}
ir_node *new_defaultProj(ir_node *arg, long max_proj) {
	return new_d_defaultProj(NULL, arg, max_proj);
}
ir_node *new_Bad(void) {
	return get_irg_bad(current_ir_graph);
}
ir_node *new_EndReg(void) {
	return new_d_EndReg(NULL);
}
ir_node *new_EndExcept(void) {
	return new_d_EndExcept(NULL);
}
ir_node *new_NoMem(void) {
	return get_irg_no_mem(current_ir_graph);
}
ir_node *new_ASM(int arity, ir_node *in[], ir_asm_constraint *inputs,
                 int n_outs, ir_asm_constraint *outputs,
                 int n_clobber, ident *clobber[], ident *asm_text) {
	return new_d_ASM(NULL, arity, in, inputs, n_outs, outputs, n_clobber, clobber, asm_text);
}

/* create a new anchor node */
ir_node *new_Anchor(ir_graph *irg) {
	ir_node *in[anchor_last];
	memset(in, 0, sizeof(in));
	return new_ir_node(NULL, irg, NULL, op_Anchor, mode_ANY, anchor_last, in);
}
