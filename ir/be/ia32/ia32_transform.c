#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "ircons.h"
#include "dbginfo.h"
#include "irop_t.h"
#include "debug.h"

#include "ia32_nodes_attr.h"
#include "../arch/archop.h"     /* we need this for Min and Max nodes */
#include "ia32_transform.h"
#include "ia32_new_nodes.h"

extern ir_op *get_op_Mulh(void);

/* determine if one operator is an Imm */
static ir_node *get_immediate_op(ir_node *op1, ir_node *op2) {
  if (op1)
    return is_ia32_Const(op1) ? op1 : (is_ia32_Const(op2) ? op2 : NULL);
  else return is_ia32_Const(op2) ? op2 : NULL;
}

/* determine if one operator is not an Imm */
static ir_node *get_expr_op(ir_node *op1, ir_node *op2) {
  return !is_ia32_Const(op1) ? op1 : (!is_ia32_Const(op2) ? op2 : NULL);
}



/**
 * Creates an ia32 Add with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Add_i node
 */
static ir_node *gen_imm_Add(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  ir_node *new_op;
  tarval  *tv = get_ia32_Immop_tarval(const_op);
  int     normal_add = 0;
  tarval_classification_t class_tv, class_negtv;

  /* const_op: tarval or SymConst? */
  if (tv) {
    /* optimize tarvals */
    class_tv    = classify_tarval(tv);
    class_negtv = classify_tarval(tarval_neg(tv));

    if (class_tv == TV_CLASSIFY_ONE) { /* + 1 == INC */
      DBG((mod, LEVEL_2, "optimizing Add(1) to Inc ... "));
      new_op = new_rd_ia32_Inc(dbg, current_ir_graph, block, expr_op, mode);
    }
    else if (class_tv == TV_CLASSIFY_ALL_ONE || class_negtv == TV_CLASSIFY_ONE) { /* + (-1) == DEC */
      DBG((mod, LEVEL_2, "optimizing Add(-1) to Dec ... "));
      new_op = new_rd_ia32_Dec(dbg, current_ir_graph, block, expr_op, mode);
    }
    else
      normal_add = 1;
  }
  else
    normal_add = 1;

  if (normal_add)
    new_op = new_rd_ia32_Lea_i(dbg, current_ir_graph, block, expr_op, mode);

  return new_op;
}

/**
 * Creates an ia32 Add.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Add node
 */
static ir_node *gen_Add(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  /* try to optimize with LEA */
  ir_node *shli_op = is_ia32_Shl_i(op1) ? op1 : (is_ia32_Shl_i(op2) ? op2 : NULL);
  ir_node *expr_op = shli_op == op1 ? op2 : (shli_op == op2 ? op1 : NULL);
  int normal_add   = 0;
  ir_node *new_op;

  if (shli_op) {
    tarval *tv   = get_ia32_Immop_tarval(shli_op);
    tarval *offs = NULL;
    if (tv) {
      switch (get_tarval_long(tv)) {
        case 1:
        case 2:
        case 3:
          // If the other operand of the LEA is an LEA_i (that means LEA ofs(%regop1)),
          // we can skip it and transform the whole sequence into LEA ofs(%regop1, %regop2, shl_val),
          if (is_ia32_Lea_i(expr_op)) {
            offs    = get_ia32_Immop_tarval(expr_op);
            expr_op = get_irn_n(expr_op, 0);
          }

          new_op = new_rd_ia32_Lea(dbg, current_ir_graph, block, expr_op, get_irn_n(shli_op, 0), mode);
          set_ia32_Immop_tarval(new_op, tv);
          set_ia32_offs(new_op, offs);

          break;
        default:
          normal_add = 1;
          break;
      }
    }
    else
      normal_add = 1;
  }
  else
    normal_add = 1;

  if (normal_add) {
    new_op = new_rd_ia32_Lea(dbg, current_ir_graph, block, op1, op2, mode);
    set_ia32_Immop_tarval(new_op, get_tarval_one(mode_Iu));
    set_ia32_offs(new_op, NULL);
  }

  return new_op;
}



/**
 * Creates an ia32 Mul with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Mul_i node
 */
static ir_node *gen_imm_Mul(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  return new_rd_ia32_Mul_i(dbg, current_ir_graph, block, expr_op, mode);
}

/**
 * Creates an ia32 Mul.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Mul node
 */
ir_node *gen_Mul(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Mul(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 Mulh with immediate.
 * Note: Mul produces a 64Bit result and Mulh returns the upper 32 bit of
 * this result while Mul returns the lower 32 bit.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Mulh_i node
 */
static ir_node *gen_imm_Mulh(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  return new_rd_ia32_Mulh_i(dbg, current_ir_graph, block, expr_op, mode);
}

/**
 * Creates an ia32 Mulh.
 * Note: Mul produces a 64Bit result and Mulh returns the upper 32 bit of
 * this result while Mul returns the lower 32 bit.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Mulh node
 */
static ir_node *gen_Mulh(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Mulh(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 And with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 And_i node
 */
static ir_node *gen_imm_And(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  return new_rd_ia32_And_i(dbg, current_ir_graph, block, expr_op, mode);
}

/**
 * Creates an ia32 And.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 And node
 */
static ir_node *gen_And(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_And(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 Or with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Or_i node
 */
static ir_node *gen_imm_Or(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  return new_rd_ia32_Or_i(dbg, current_ir_graph, block, expr_op, mode);
}

/**
 * Creates an ia32 Or.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Or node
 */
static ir_node *gen_Or(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Or(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 Eor with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Eor_i node
 */
static ir_node *gen_imm_Eor(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  return new_rd_ia32_Eor_i(dbg, current_ir_graph, block, expr_op, mode);
}

/**
 * Creates an ia32 Eor.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Eor node
 */
static ir_node *gen_Eor(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Eor(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 Max.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Max node
 */
static ir_node *gen_Max(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Max(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 Min.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Min node
 */
static ir_node *gen_Min(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Min(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 Cmp with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Cmp_i node
 */
static ir_node *gen_imm_Cmp(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  return new_rd_ia32_Cmp_i(dbg, current_ir_graph, block, expr_op, mode);
}

/**
 * Creates an ia32 Cmp.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Cmp node
 */
static ir_node *gen_Cmp(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Cmp(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 Sub with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Sub_i node
 */
static ir_node *gen_imm_Sub(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  ir_node *new_op;
  tarval  *tv = get_ia32_Immop_tarval(const_op);
  int     normal_sub = 0;
  tarval_classification_t class_tv, class_negtv;

  /* const_op: tarval or SymConst? */
  if (tv) {
    /* optimize tarvals */
    class_tv    = classify_tarval(tv);
    class_negtv = classify_tarval(tarval_neg(tv));

    if (class_tv == TV_CLASSIFY_ONE) { /* - 1 == DEC */
      DBG((mod, LEVEL_2, "optimizing Sub(1) to Dec ... "));
      new_op = new_rd_ia32_Dec(dbg, current_ir_graph, block, expr_op, mode);
    }
    else if (class_negtv == TV_CLASSIFY_ONE) { /* - (-1) == Sub */
      DBG((mod, LEVEL_2, "optimizing Sub(-1) to Inc ... "));
      new_op = new_rd_ia32_Inc(dbg, current_ir_graph, block, expr_op, mode);
    }
    else
      normal_sub = 1;
  }
  else
    normal_sub = 1;

  if (normal_sub)
    new_op = new_rd_ia32_Sub_i(dbg, current_ir_graph, block, expr_op, mode);

  return new_op;
}

/**
 * Creates an ia32 Sub.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Sub node
 */
static ir_node *gen_Sub(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Sub(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 Mod.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Mod node
 */
static ir_node *gen_Mod(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *mem, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_DivMod(dbg, current_ir_graph, block, mem, op1, op2, flavour_Mod, mode);
}



/**
 * Creates an ia32 Div.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Div node
 */
static ir_node *gen_Div(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *mem, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_DivMod(dbg, current_ir_graph, block, mem, op1, op2, flavour_Div, mode);
}



/**
 * Creates an ia32 DivMod.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 DivMod node
 */
static ir_node *gen_DivMod(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *mem, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_DivMod(dbg, current_ir_graph, block, mem, op1, op2, flavour_DivMod, mode);
}



/**
 * Creates an ia32 Shl with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Shl_i node
 */
static ir_node *gen_imm_Shl(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  return new_rd_ia32_Shl_i(dbg, current_ir_graph, block, expr_op, mode);
}

/**
 * Creates an ia32 Shl.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Shl node
 */
static ir_node *gen_Shl(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Shl(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 Shr with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Shr_i node
 */
static ir_node *gen_imm_Shr(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  return new_rd_ia32_Shr_i(dbg, current_ir_graph, block, expr_op, mode);
}

/**
 * Creates an ia32 Shr.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Shr node
 */
static ir_node *gen_Shr(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Shr(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 Shrs with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Shrs_i node
 */
static ir_node *gen_imm_Shrs(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  return new_rd_ia32_Shrs_i(dbg, current_ir_graph, block, expr_op, mode);
}

/**
 * Creates an ia32 Shrs.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Shrs node
 */
static ir_node *gen_Shrs(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Shrs(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 RotL.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 RotL node
 */
static ir_node *gen_RotL(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_RotL(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 RotR.
 * NOTE: There is no RotR with immediate because this would always be a RotL
 *       "imm-mode_size_bits" which can be pre-calculated.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 RotR node
 */
static ir_node *gen_RotR(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_RotR(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Transforms a Rot with immediate into an ia32 RotL with immediate
 * as the Firm Rot is a RotL (see NOTE on RotR with immediate above).
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 RotL node
 */
static ir_node *gen_imm_Rot(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  return new_rd_ia32_RotL_i(dbg, current_ir_graph, block, expr_op, mode);
}

/**
 * Creates an ia32 RotR or RotL (depending on the found pattern).
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 RotL or RotR node
 */
static ir_node *gen_Rot(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  ir_node *rotate = NULL;

  /* Firm has only Rot (which is a RotL), so we are looking for a right (op2)
     operand "-e+mode_size_bits" (it's an already modified "mode_size_bits-e",
     that means we can create a RotR instead of an Add and a RotL */

  if (is_ia32_Add_i(op2)) {
    ir_node *minus = get_irn_n(op2, 0); // is there an op_Minus?

    if (is_ia32_Minus(minus)) {
      tarval *tv = get_ia32_Immop_tarval(op2);
      long bits  = get_mode_size_bits(mode);

      if (tarval_is_long(tv) && get_tarval_long(tv) == bits) {
        DBG((mod, LEVEL_1, "optimizing RotL into RotR ... "));
        rotate = gen_RotR(mod, dbg, block, op1, get_irn_n(minus, 0), mode);
      }
    }
  }

  if (!rotate)
    rotate = gen_RotL(mod, dbg, block, op1, op2, mode);

  return rotate;
}



/**
 * Transforms commutative operations (op_Add, op_Mul, op_And, op_Or, op_Eor, op_Cmp)
 * and non-commutative operations with com == 0 (op_Sub, op_Shl, op_Shr, op_Shrs, op_Rot)
 *
 * @param mod       the debug module
 * @param block     the block node belongs to
 * @param node      the node to transform
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @param com       flag if op is commutative
 * @return the created assembler node
 */
static ir_node *gen_arith_Op(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_node *op1, ir_node *op2, ir_mode *mode, int com) {
  dbg_info *dbg      = get_irn_dbg_info(node);
  ir_node  *imm_op   = NULL;
  ir_node  *expr_op  = NULL;
  ir_node  *asm_node = NULL;
  opcode   opc       = get_irn_opcode(node);
  ir_op    *op       = get_irn_op(node);

#define GENOP(a)  case iro_##a: asm_node = gen_##a(mod, dbg, block, op1, op2, mode); break
#define GENOPI(a) case iro_##a: asm_node = gen_imm_##a(mod, dbg, block, expr_op, imm_op, mode); break

  if (com)
    imm_op  = get_immediate_op(op1, op2);
  else
    imm_op  = get_immediate_op(NULL, op2);

  expr_op = get_expr_op(op1, op2);

  /* TODO: Op(Const, Const) support */
  if (is_ia32_Const(op1) && is_ia32_Const(op2)) {
    DBG((mod, LEVEL_2, "found unexpected %s(Const, Const), creating binop ... ", get_irn_opname(node)));
    imm_op = NULL;
  }

  if (op == get_op_Min() || op == get_op_Max()) {
    DBG((mod, LEVEL_2, "MIN/MAX imm not available, creating binop ... "));
    imm_op = NULL;
  }

  DBG((mod, LEVEL_1, "(op1: %s -- op2: %s) ... ", get_irn_opname(op1), get_irn_opname(op2)));

  if (imm_op) {
    DBG((mod, LEVEL_1, "%s with imm ... ", get_irn_opname(node)));

    switch(opc) {
      GENOPI(Add);
      GENOPI(Mul);
      GENOPI(And);
      GENOPI(Or);
      GENOPI(Eor);
      GENOPI(Cmp);

      GENOPI(Sub);
      GENOPI(Shl);
      GENOPI(Shr);
      GENOPI(Shrs);
      GENOPI(Rot);
      default:
        if (op == get_op_Mulh()) {
          asm_node = gen_imm_Mulh(mod, dbg, block, expr_op, imm_op, mode);
        }
        else
          assert("binop_i: THIS SHOULD NOT HAPPEN");
    }

    set_ia32_Immop_attr(asm_node, imm_op);
  }
  else {
    DBG((mod, LEVEL_1, "%s as binop ... ", get_irn_opname(node)));

    switch(opc) {
      GENOP(Add);
      GENOP(Mul);
      GENOP(And);
      GENOP(Or);
      GENOP(Eor);
      GENOP(Cmp);

      GENOP(Sub);
      GENOP(Shl);
      GENOP(Shr);
      GENOP(Shrs);
      GENOP(Rot);
      default:
        if (op == get_op_Mulh()) {
          asm_node = gen_Mulh(mod, dbg, block, op1, op2, mode);
        }
        else if (op == get_op_Max()) {
          asm_node = gen_Max(mod, dbg, block, op1, op2, mode);
        }
        else if (op == get_op_Min()) {
          asm_node = gen_Min(mod, dbg, block, op1, op2, mode);
        }
        else
          assert("binop: THIS SHOULD NOT HAPPEN");
    }
  }

  return asm_node;
}



/**
 * Transforms a Minus node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Minus node
 * @param op      operator
 * @param mode    node mode
 * @return the created ia32 Minus node
 */
static ir_node *gen_Minus(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_node *op, ir_mode *mode) {
  if (is_ia32_Minus(op)) {
    DBG((mod, LEVEL_1, "optimizing --(e) to e ..."));
    return get_irn_n(op, 0);
  }
  else
    return new_rd_ia32_Minus(get_irn_dbg_info(node), current_ir_graph, block, op, mode);
}



/**
 * Transforms a Conv node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Conv node
 * @param op      operator
 * @param mode    node mode
 * @return the created ia32 Conv node
 */
static ir_node *gen_Conv(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_node *op, ir_mode *mode) {
  return new_rd_ia32_Conv(get_irn_dbg_info(node), current_ir_graph, block, op, mode);
}



/**
 * Transforms a Not node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Not node
 * @param op      operator
 * @param mode    node mode
 * @return the created ia32 Not node
 */
static ir_node *gen_Not(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_node *op, ir_mode *mode) {
  return new_rd_ia32_Not(get_irn_dbg_info(node), current_ir_graph, block, op, mode);
}



/**
 * Transforms an Abs node.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Abs node
 * @param op      operator
 * @param mode    node mode
 * @return the created ia32 Abs node
 */
static ir_node *gen_Abs(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_node *op, ir_mode *mode) {
  ir_node  *res, *p_eax, *p_edx;
  dbg_info *dbg = get_irn_dbg_info(node);

  res   = new_rd_ia32_Cltd(dbg, current_ir_graph, block, op, mode_T);
  p_eax = new_rd_Proj(dbg, current_ir_graph, block, res, mode, pn_EAX);
  p_edx = new_rd_Proj(dbg, current_ir_graph, block, res, mode, pn_EDX);
  res   = new_rd_ia32_Eor(dbg, current_ir_graph, block, p_eax, p_edx, mode);
  res   = new_rd_ia32_Sub(dbg, current_ir_graph, block, res, p_edx, mode);

  return res;
}



/**
 * Transforms a Load.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Load node
 * @param mode    node mode
 * @return the created ia32 Load node
 */
static ir_node *gen_Load(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_mode *mode) {
  return new_rd_ia32_Load(get_irn_dbg_info(node), current_ir_graph, block, get_Load_mem(node), get_Load_ptr(node), mode);
}



/**
 * Transforms a Store.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Store node
 * @param mode    node mode
 * @return the created ia32 Store node
 */
ir_node *gen_Store(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_mode *mode) {
  return new_rd_ia32_Store(get_irn_dbg_info(node), current_ir_graph, block, get_Store_mem(node), get_Store_ptr(node), get_Store_value(node), mode);
}



/**
 * Transforms a Call.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Call node
 * @param dummy   mode doesn't matter
 * @return the created ia32 Call node
 */
static ir_node *gen_Call(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_mode *dummy) {
  return new_rd_ia32_Call(get_irn_dbg_info(node), current_ir_graph, block, node);
}



/**
 * Transforms a Const.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir Const node
 * @param mode    mode of the Const
 * @return the created ia32 Const node
 */
static ir_node *gen_Const(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_mode *mode) {
  ir_node *cnst = new_rd_ia32_Const(get_irn_dbg_info(node), current_ir_graph, block, mode);
  set_ia32_Const_attr(cnst, node);
  return cnst;
}

/**
 * Transforms a SymConst.
 *
 * @param mod     the debug module
 * @param block   the block the new node should belong to
 * @param node    the ir SymConst node
 * @param mode    mode of the SymConst
 * @return the created ia32 Const node
 */
static ir_node *gen_SymConst(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_mode *mode) {
  ir_node *cnst = new_rd_ia32_Const(get_irn_dbg_info(node), current_ir_graph, block, mode);
  set_ia32_Const_attr(cnst, node);
  return cnst;
}



/**
 * Transforms the given firm node (and maybe some other related nodes)
 * into one or more assembler nodes.
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void ia32_transform_node(ir_node *node, void *env) {
  firm_dbg_module_t *mod = (firm_dbg_module_t *)env;
  opcode  code           = get_irn_opcode(node);
  ir_node *asm_node      = NULL;
  ir_node *block;
  ir_mode *mode;

  if (is_Block(node))
    return;

  block = get_nodes_block(node);
  mode  = get_irn_mode(node);

#define BINOP_COM(a)   case iro_##a: asm_node = gen_arith_Op(mod, block, node, get_irn_n(node, 0), get_irn_n(node, 1), mode, 1); break
#define BINOP_NCOM(a)  case iro_##a: asm_node = gen_arith_Op(mod, block, node, get_irn_n(node, 0), get_irn_n(node, 1), mode, 0); break
#define TRIOP(a)       case iro_##a: asm_node = gen_##a(mod, get_irn_dbg_info(node), block, get_irn_n(node, 0), get_irn_n(node, 1), get_irn_n(node, 2), mode); break
#define UNOP(a)        case iro_##a: asm_node = gen_##a(mod, block, node, get_irn_n(node, 0), mode); break
#define GEN(a)         case iro_##a: asm_node = gen_##a(mod, block, node, mode); break
#define IGN(a)         case iro_##a: break
#define BAD(a)         case iro_##a: goto bad

  DBG((mod, LEVEL_1, "transforming node %s (%ld) ... ", get_irn_opname(node), get_irn_node_nr(node)));

  switch (code) {
    BINOP_COM(Add);
    BINOP_COM(Mul);
    BINOP_COM(And);
    BINOP_COM(Or);
    BINOP_COM(Eor);
    BINOP_COM(Cmp);

    BINOP_NCOM(Sub);
    TRIOP(Mod);
    TRIOP(Div);
    TRIOP(DivMod);
    BINOP_NCOM(Shl);
    BINOP_NCOM(Shr);
    BINOP_NCOM(Shrs);

    UNOP(Minus);
    UNOP(Conv);
    UNOP(Abs);
    UNOP(Not);

    GEN(Load);
    GEN(Store);
    GEN(Call);
    GEN(Const);
    GEN(SymConst);

    IGN(Block);
    IGN(Start);
    IGN(End);
    IGN(NoMem);
    IGN(Phi);
    IGN(Cond);
    IGN(Jmp);
    IGN(IJmp);
    IGN(Proj);
    IGN(Break);

    BAD(Raise);
    BAD(Sel);
    BAD(InstOf);
    BAD(Quot);
    BAD(Cast);
    BAD(Alloc);
    BAD(Free);
    BAD(Sync);
    BAD(Tuple);
    BAD(Id);
    BAD(Bad);
    BAD(Confirm);
    BAD(Unknown);
    BAD(Filter);
    BAD(CallBegin);
    BAD(EndReg);
    BAD(EndExcept);
    BAD(Mux);
    BAD(CopyB);

    default:
      if (get_irn_op(node) == get_op_Mulh() ||
          get_irn_op(node) == get_op_Max()  ||
          get_irn_op(node) == get_op_Min())
      {
        asm_node = gen_arith_Op(mod, block, node, get_irn_n(node, 0), get_irn_n(node, 1), mode, 1);
      }
      break;
bad:
    fprintf(stderr, "Not implemented: %s\n", get_irn_opname(node));
    assert(0);
  }

  if (asm_node) {
    exchange(node, asm_node);
    DBG((mod, LEVEL_1, "created node %u\n", get_irn_node_nr(asm_node)));
  }
  else {
    DBG((mod, LEVEL_1, "ignored\n"));
  }
}
