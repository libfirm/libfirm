/*
 * Project:     libFIRM
 * File name:   ir/opt/reassoc.c
 * Purpose:     Reassociation
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irnode_t.h"
# include "irgraph_t.h"
# include "irmode_t.h"
# include "iropt_t.h"
# include "ircons_t.h"
# include "irgmod.h"
# include "dbginfo.h"
# include "iropt_dbg.h"
# include "irflag_t.h"
# include "irgwalk.h"
# include "reassoc_t.h"
# include "firmstat.h"

typedef struct _walker_t {
  int changes;          /* set, if a reassociation take place */
} walker_t;

typedef enum {
  NO_CONSTANT   = 0,    /**< node is not constant */
  REAL_CONSTANT = 1,    /**< node is a constnt that is suitable for constant folding */
  CONST_EXPR    = 4     /**< node is not constnt expression in the current context,
                             use 4 here to simplify implementation of get_comm_Binop_ops() */
} const_class_t;

/**
 * returns wheater a node is constant, ie is a constant or
 * is loop invariant
 */
static const_class_t get_const_class(ir_node *n)
{
  ir_op *op = get_irn_op(n);

  if (op == op_Const)
    return REAL_CONSTANT;
  if (op == op_SymConst)
    return CONST_EXPR;

  return NO_CONSTANT;
}

/**
 * returns the operands of a commutative bin-op, if one operand is
 * a constant in the current context, it is returned as the second one.
 *
 * Beware: Real constrants must be returned with higher priority than
 * constnt expression, because they might be folded.
 */
static void get_comm_Binop_ops(ir_node *binop, ir_node **a, ir_node **c)
{
  ir_node *op_a = get_binop_left(binop);
  ir_node *op_b = get_binop_right(binop);
  int class_a = get_const_class(op_a);
  int class_b = get_const_class(op_b);

  assert(is_op_commutative(get_irn_op(binop)));

  switch (class_a + 2*class_b) {
    case REAL_CONSTANT + 2*NO_CONSTANT:
    case REAL_CONSTANT + 2*REAL_CONSTANT:
    case REAL_CONSTANT + 2*CONST_EXPR:
    case CONST_EXPR    + 2*NO_CONSTANT:
      *a = op_b;
      *c = op_a;
      break;
    default:
      *a = op_a;
      *c = op_b;
      break;
  }
}

/**
 * reassociate a Sub: x - c = (-c) + x
 */
static int reassoc_Sub(ir_node *n)
{
  ir_node *right = get_Sub_right(n);

  /* handles rule R6:
   * convert x - c => (-c) + x
   *
   * As there is NO real Minus in Firm it makes no sense to do this
   * for non-real constants yet.
   * */
  if (get_const_class(right) == REAL_CONSTANT) {
    ir_node *block = get_nodes_block(right);
    ir_mode *mode  = get_irn_mode(right);
    dbg_info *dbg  = get_irn_dbg_info(right);
    ir_node *irn, *c;

    c   = new_r_Const(current_ir_graph, block, mode, get_mode_null(mode));
    irn = new_rd_Sub(dbg, current_ir_graph, block, c, right, mode);

    irn = new_rd_Add(dbg, current_ir_graph, block, irn, get_Sub_left(n), get_irn_mode(n));

    printf("Applied: %s - %s => (-%s) + %s\n",
        get_irn_opname(get_Sub_left(n)), get_irn_opname(c),
        get_irn_opname(c), get_irn_opname(get_Sub_left(n)) );

    exchange(n, irn);

    return 1;
  }
  return 0;
}

/** Retrieve a mode form the operands. We need this, because
 * Add and Sub are allowed to operate on (P, Is)
 */
static ir_mode *get_mode_from_ops(ir_node *op1, ir_node *op2)
{
  ir_mode *m1, *m2;

  m1 = get_irn_mode(op1);
  if (mode_is_reference(m1))
    return m1;

  m2 = get_irn_mode(op2);
  if (mode_is_reference(m2))
    return m2;

  assert(m1 == m2);

  return m1;
}

/**
 * reassociate a commutative Binop
 *
 * BEWARE: this rule leads to a potential loop, if
 * all two operands are are constant expressions and the third is a
 * constant, so avoid this situation.
 */
static int reassoc_commutative(ir_node *n)
{
  ir_op *op      = get_irn_op(n);
  ir_node *block = get_nodes_block(n);
  ir_node *t1, *c1;

  get_comm_Binop_ops(n, &t1, &c1);

  if (get_irn_op(t1) == op) {
    ir_node *t2, *c2;
    const_class_t c_c1, c_c2, c_t2;

    get_comm_Binop_ops(t1, &t2, &c2);

    c_c1 = get_const_class(c1);
    c_c2 = get_const_class(c2);
    c_t2 = get_const_class(t2);

    if ( ((c_c1 > NO_CONSTANT) & (c_t2 > NO_CONSTANT)) &&
         ((((c_c1 ^ c_c2 ^ c_t2) & CONST_EXPR) == 0) || ((c_c1 & c_c2 & c_t2) == CONST_EXPR)) ) {
      /* all three are constant and either all are constant expressions or two of them are:
       * then, applying this rule would lead into a cycle
       *
       * Note that if t2 is a onstant so is c2, so we save one test.
       */
      return 0;
    }

    if ((c_c1 != NO_CONSTANT) & (c_c2 != NO_CONSTANT)) {
      /* handles rules R7, R8, R9, R10:
       * convert c1 .OP. (c2 .OP. x) => (c1 .OP. c2) .OP. x
       */
      ir_node *irn, *in[2];
      ir_mode *mode;

      in[0] = c1;
      in[1] = c2;

      mode = get_mode_from_ops(in[0], in[1]);
      in[0] = optimize_node(new_ir_node(NULL, current_ir_graph, block, op, mode, 2, in));
      in[1] = t2;

      mode = get_mode_from_ops(in[0], in[1]);
      irn   = optimize_node(new_ir_node(NULL, current_ir_graph, block, op, mode, 2, in));

      printf("Applied: %s .%s. (%s .%s. %s) => (%s .%s. %s) .%s. %s\n",
          get_irn_opname(c1), get_irn_opname(n), get_irn_opname(c2), get_irn_opname(n), get_irn_opname(t2),
          get_irn_opname(c1), get_irn_opname(n), get_irn_opname(c2), get_irn_opname(n), get_irn_opname(t2));

      exchange(n, irn);

      return 1;
    }
  }
  return 0;
}

#define reassoc_Add  reassoc_commutative

/**
 * reassociate using distibutive law for Mul and Add/Sub
 */
static int reassoc_Mul(ir_node *n)
{
  ir_node *add_sub, *c;
  ir_op *op;

  if (reassoc_commutative(n))
    return 1;

  get_comm_Binop_ops(n, &add_sub, &c);
  op = get_irn_op(add_sub);

  /* handles rules R11, R12, R13, R14, R15, R16, R17, R18, R19, R20 */
  if (op == op_Add || op == op_Sub) {
    ir_mode *mode = get_irn_mode(n);
    ir_node *irn, *block, *t1, *t2, *in[2];

    block = get_nodes_block(n);
    t1 = get_binop_left(add_sub);
    t2 = get_binop_right(add_sub);

    in[0] = new_rd_Mul(NULL, current_ir_graph, block, c, t1, mode);
    in[1] = new_rd_Mul(NULL, current_ir_graph, block, c, t2, mode);

    mode  = get_mode_from_ops(in[0], in[1]);
    irn   = optimize_node(new_ir_node(NULL, current_ir_graph, block, op, mode, 2, in));

    printf("Applied: (%s .%s. %s) %s %s => (%s %s %s) .%s. (%s %s %s)\n",
        get_irn_opname(t1), get_op_name(op), get_irn_opname(t2), get_irn_opname(n), get_irn_opname(c),
        get_irn_opname(t1), get_irn_opname(n), get_irn_opname(c),
        get_op_name(op),
        get_irn_opname(t2), get_irn_opname(n), get_irn_opname(c));

    exchange(n, irn);

    return 1;
  }
  return 0;
}

/**
 * The walker for the reassociation
 */
static void do_reassociation(ir_node *n, void *env)
{
  walker_t *wenv = env;
  int res;

  /* reassociation must run until fixpoint */
  do {
    ir_op   *op    = get_irn_op(n);
    ir_mode *mode  = get_irn_mode(n);

    res = 0;

    /* reassociation works only for integer or reference modes */
    if (op->reassociate && (mode_is_int(mode) || mode_is_reference(mode))) {
      res = op->reassociate(n);
      if (res) {
        wenv->changes = 1;

        /* we need a skip here, or we will see an Id in the next iteration */
        n = skip_Id(n);
      }
    }
  } while (res == 1);
}

/*
 * do the reassociation
 */
void optimize_reassociation(ir_graph *irg)
{
  walker_t env;

  assert(get_irg_phase_state(irg) != phase_building);

  /* reassociation needs constant folding */
  if (!get_opt_reassociation() || !get_opt_constant_folding())
    return;

  env.changes = 0;

  irg_walk_graph(irg, NULL, do_reassociation, &env);

  /* now we have collected enough information, optimize */
  irg_walk_graph(irg, NULL, do_reassociation, &env);

  /* Handle graph state */
  if (env.changes) {
    if (get_irg_outs_state(current_ir_graph) == outs_consistent)
      set_irg_outs_inconsistent(current_ir_graph);
  }
}

/* initialise the reassociation by adding operations to some opcodes */
void firm_init_reassociation(void)
{
#define INIT(a) op_##a->reassociate  = reassoc_##a;
  INIT(Mul);
  INIT(Add);
  INIT(Sub);
#undef CASE
}
