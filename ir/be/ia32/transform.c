#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "irgmod.h"
#include "dbginfo.h"
#include "irop_t.h"
#include "debug.h"

#include "../firm2arch_nodes_attr.h"
#include "../bearch_firm.h"
#include "transform.h"
#include "new_nodes.h"



/* determine if one operator is an Imm */
ir_node *get_immediate_op(ir_node *op1, ir_node *op2) {
  if (op1)
    return is_Imm(op1) ? op1 : (is_Imm(op2) ? op2 : NULL);
  else return is_Imm(op2) ? op2 : NULL;
}

/* determine if one operator is not an Imm */
ir_node *get_expr_op(ir_node *op1, ir_node *op2) {
  return !is_Imm(op1) ? op1 : (!is_Imm(op2) ? op2 : NULL);
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
ir_node *gen_imm_Add(dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  ir_node *new_op;
  tarval  *tv = get_Immop_tarval(const_op);
  int     normal_add = 0;
  tarval_classification_t class_tv, class_negtv;

  /* const_op: tarval or SymConst? */
  if (tv) {
    /* optimize tarvals */
    class_tv    = classify_tarval(tv);
    class_negtv = classify_tarval(tarval_neg(tv));

    if (class_tv == TV_CLASSIFY_ONE) { /* + 1 == INC */
      new_op = new_rd_ia32_Inc(dbg, current_ir_graph, block, expr_op, mode);
    }
    else if (class_tv == TV_CLASSIFY_ALL_ONE || class_negtv == TV_CLASSIFY_ONE) { /* + (-1) == Sub */
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
ir_node *gen_Add(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  /* try to optimize with LEA */
  ir_node *shli_op = is_ia32_Shl_i(op1) ? op1 : (is_ia32_Shl_i(op2) ? op2 : NULL);
  ir_node *expr_op = shli_op == op1 ? op2 : (shli_op == op2 ? op1 : NULL);
  int normal_add   = 0;
  ir_node *new_op;

  if (shli_op) {
    tarval *tv   = get_Immop_tarval(shli_op);
    tarval *offs = NULL;
    if (tv) {
      switch (get_tarval_long(tv)) {
        case 1:
        case 2:
        case 3:
          // If the other operand of the LEA is an LEA_i (that means LEA ofs(%regop1)),
          // we can skip it and transform the whole sequence into LEA ofs(%regop1, %regop2, shl_val),
          if (is_ia32_Lea_i(expr_op)) {
            offs = get_Immop_tarval(expr_op);
            expr_op = get_irn_n(expr_op, 0);
          }

          new_op = new_rd_ia32_Lea(dbg, current_ir_graph, block, expr_op, get_irn_n(shli_op, 0), mode);
          set_Immop_attr_tv(new_op, tv);

          if (offs)
            set_ia32_Lea_offs(new_op, offs);

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
    set_Immop_attr_tv(new_op, get_tarval_one(mode));
    set_ia32_Lea_offs(new_op, NULL);
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
ir_node *gen_imm_Mul(dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
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
 * Creates an ia32 And with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 And_i node
 */
ir_node *gen_imm_And(dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
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
ir_node *gen_And(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
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
ir_node *gen_imm_Or(dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
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
ir_node *gen_Or(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
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
ir_node *gen_imm_Eor(dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
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
ir_node *gen_Eor(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Eor(dbg, current_ir_graph, block, op1, op2, mode);
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
ir_node *gen_imm_Cmp(dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
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
ir_node *gen_Cmp(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
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
ir_node *gen_imm_Sub(dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  ir_node *new_op;
  tarval  *tv = get_Immop_tarval(const_op);
  int     normal_sub = 0;
  tarval_classification_t class_tv, class_negtv;

  /* const_op: tarval or SymConst? */
  if (tv) {
    /* optimize tarvals */
    class_tv    = classify_tarval(tv);
    class_negtv = classify_tarval(tarval_neg(tv));

    if (class_tv == TV_CLASSIFY_ONE) { /* - 1 == DEC */
      new_op = new_rd_ia32_Dec(dbg, current_ir_graph, block, expr_op, mode);
    }
    else if (class_negtv == TV_CLASSIFY_ONE) { /* - (-1) == Sub */
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
ir_node *gen_Sub(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  ir_node *sub = NULL;

  /* transform "const - expr" into "-expr + const" */
  if (is_Imm(op1) && !is_Imm(op2)) {
    DBG((mod, LEVEL_1, "optimizing c-e into -e+c ... "));

    sub = new_rd_ia32_Minus(dbg, current_ir_graph, block, op2, mode);
    sub = gen_imm_Add(dbg, block, sub, op1, mode);
    set_Immop_attr(sub, op1);
  }
  else {
    sub = new_rd_ia32_Sub(dbg, current_ir_graph, block, op1, op2, mode);
  }

  return sub;
}



/**
 * Creates an ia32 Mod with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Mod_i node
 */
ir_node *gen_imm_Mod(dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  return new_rd_ia32_Mod_i(dbg, current_ir_graph, block, expr_op, mode);
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
ir_node *gen_Mod(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Mod(dbg, current_ir_graph, block, op1, op2, mode);
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
ir_node *gen_imm_Shl(dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
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
ir_node *gen_Shl(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
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
ir_node *gen_imm_Shr(dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
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
ir_node *gen_Shr(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
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
ir_node *gen_imm_Shrs(dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
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
ir_node *gen_Shrs(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Shrs(dbg, current_ir_graph, block, op1, op2, mode);
}



/**
 * Creates an ia32 Rot with immediate.
 *
 * @param dbg       firm dbg
 * @param block     the block the new node should belong to
 * @param expr_op   operator
 * @param mode      node mode
 * @return the created ia23 Rot_i node
 */
ir_node *gen_imm_Rot(dbg_info *dbg, ir_node *block, ir_node *expr_op, ir_node *const_op, ir_mode *mode) {
  return new_rd_ia32_Rot_i(dbg, current_ir_graph, block, expr_op, mode);
}

/**
 * Creates an ia32 Rot.
 *
 * @param dbg       firm node dbg
 * @param block     the block the new node should belong to
 * @param op1       first operator
 * @param op2       second operator
 * @param mode      node mode
 * @return the created ia32 Rot node
 */
ir_node *gen_Rot(firm_dbg_module_t *mod, dbg_info *dbg, ir_node *block, ir_node *op1, ir_node *op2, ir_mode *mode) {
  return new_rd_ia32_Rot(dbg, current_ir_graph, block, op1, op2, mode);
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
ir_node *gen_Minus(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_node *op, ir_mode *mode) {
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
ir_node *gen_Conv(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_node *op, ir_mode *mode) {
  return new_rd_ia32_Conv(get_irn_dbg_info(node), current_ir_graph, block, op, mode);
}



/**
 * Transforms commutative operations (op_Add, op_Mul, op_And, op_Or, op_Eor, op_Cmp)
 * and non-commutative operations with com == 0 (op_Sub, op_Mod, op_Shl, op_Shr, op_Shrs, op_Rot)
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
ir_node *gen_arith_Op(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_node *op1, ir_node *op2, ir_mode *mode, int com) {
  dbg_info *dbg     = get_irn_dbg_info(node);
  ir_node *imm_op   = NULL;
  ir_node *expr_op  = NULL;
  ir_node *asm_node = NULL;

#define GENOP(a)  case iro_##a: asm_node = gen_##a(mod, dbg, block, op1, op2, mode); break
#define GENOPI(a) case iro_##a: asm_node = gen_imm_##a(dbg, block, expr_op, imm_op, mode); break

  if (com)
    imm_op  = get_immediate_op(op1, op2);
  else
    imm_op  = get_immediate_op(NULL, op2);

  expr_op = get_expr_op(op1, op2);

  /* TODO: Op(Imm, Imm) support */
  if (is_Imm(op1) && is_Imm(op2)) {
    DBG((mod, LEVEL_1, "found unexpected %s(Imm, Imm), creating binop ... ", get_irn_opname(node)));
    imm_op = NULL;
  }

  DBG((mod, LEVEL_1, "(op1: %s -- op2: %s) ... ", get_irn_opname(op1), get_irn_opname(op2)));

  if (imm_op) {
    DBG((mod, LEVEL_1, "%s with imm ... ", get_irn_opname(node)));

    switch(get_irn_opcode(node)) {
      GENOPI(Add);
      GENOPI(Mul);
      GENOPI(And);
      GENOPI(Or);
      GENOPI(Eor);
      GENOPI(Cmp);

      GENOPI(Sub);
      GENOPI(Mod);
      GENOPI(Shl);
      GENOPI(Shr);
      GENOPI(Shrs);
      GENOPI(Rot);
      default:
        assert("binop_i: THIS SHOULD NOT HAPPEN");
    }

    set_Immop_attr(asm_node, imm_op);
  }
  else {
    DBG((mod, LEVEL_1, "%s as binop ... ", get_irn_opname(node)));

    switch(get_irn_opcode(node)) {
      GENOP(Add);
      GENOP(Mul);
      GENOP(And);
      GENOP(Or);
      GENOP(Eor);
      GENOP(Cmp);

      GENOP(Sub);
      GENOP(Mod);
      GENOP(Shl);
      GENOP(Shr);
      GENOP(Shrs);
      GENOP(Rot);
      default:
        assert("binop: THIS SHOULD NOT HAPPEN");
    }
  }

  return asm_node;
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
ir_node *gen_Load(firm_dbg_module_t *mod, ir_node *block, ir_node *node, ir_mode *mode) {
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
 * Transforms the given firm node (and maybe some other related nodes)
 * into one or more assembler nodes.
 *
 * @param node    the firm node
 * @param env     the debug module
 */
void transform_node(ir_node *node, void *env) {
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
#define UNOP(a)        case iro_##a: asm_node = gen_##a(mod, block, node, get_irn_n(node, 0), mode); break
#define GEN(a)         case iro_##a: asm_node = gen_##a(mod, block, node, mode); break
#define IGN(a)         case iro_##a: break

  DBG((mod, LEVEL_1, "transforming node %s (%ld) ... ", get_irn_opname(node), get_irn_node_nr(node)));

  switch (code) {
    BINOP_COM(Add);
    BINOP_COM(Mul);
    BINOP_COM(And);
    BINOP_COM(Or);
    BINOP_COM(Eor);
    BINOP_COM(Cmp);

    BINOP_NCOM(Sub);
    BINOP_NCOM(Mod);
    BINOP_NCOM(Shl);
    BINOP_NCOM(Shr);
    BINOP_NCOM(Shrs);
    BINOP_NCOM(Rot);
//    BINOP_ARITH_NCOM(DivMod);

    UNOP(Minus);
    UNOP(Conv);
    GEN(Load);
    GEN(Store);

    IGN(Block);
    IGN(Start);
    IGN(End);
  }

  if (asm_node) {
    exchange(node, asm_node);
    DBG((mod, LEVEL_1, "created node %u\n", get_irn_node_nr(asm_node)));
  }
  else {
    DBG((mod, LEVEL_1, "ignored\n"));
  }
}
