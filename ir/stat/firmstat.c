/*
 * Project:     libFIRM
 * File name:   ir/ir/firmstat.c
 * Purpose:     Statistics for Firm.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef FIRM_STATISTICS

#include <stdio.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif

#include "irouts.h"
#include "irdump.h"
#include "hashptr.h"
#include "firmstat_t.h"
#include "pattern.h"
#include "dags.h"
#include "stat_dmp.h"
#include "xmalloc.h"
#include "irhooks.h"

/*
 * need this to be static:
 * Special pseudo Opcodes that we need to count some interesting cases
 */

/**
 * The Phi0, a node that is created during SSA construction
 */
static ir_op _op_Phi0;

/** The PhiM, just to count memory Phi's. */
static ir_op _op_PhiM;

/** The Mul by Const node. */
static ir_op _op_MulC;

/** The Div by Const node. */
static ir_op _op_DivC;

/** The Div by Const node. */
static ir_op _op_ModC;

/** The Div by Const node. */
static ir_op _op_DivModC;

/** The memory Proj node. */
static ir_op _op_ProjM;

/* ---------------------------------------------------------------------------------- */

/** Marks the begin of a statistic (hook) function. */
#define STAT_ENTER		++status->recursive

/** Marks the end of a statistic (hook) functions. */
#define STAT_LEAVE		--status->recursive

/** Allows to enter a statistic function only when we are not already in a hook. */
#define STAT_ENTER_SINGLE	do { if (status->recursive > 0) return; ++status->recursive; } while (0)

/**
 * global status
 */
static stat_info_t _status, *status = &_status;

/**
 * compare two elements of the opcode hash
 */
static int opcode_cmp(const void *elt, const void *key)
{
  const node_entry_t *e1 = elt;
  const node_entry_t *e2 = key;

  return e1->op->code - e2->op->code;
}

/**
 * compare two elements of the graph hash
 */
static int graph_cmp(const void *elt, const void *key)
{
  const graph_entry_t *e1 = elt;
  const graph_entry_t *e2 = key;

  return e1->irg != e2->irg;
}

/**
 * compare two elements of the optimization hash
 */
static int opt_cmp(const void *elt, const void *key)
{
  const opt_entry_t *e1 = elt;
  const opt_entry_t *e2 = key;

  return e1->op->code != e2->op->code;
}

/**
 * compare two elements of the block hash
 */
static int block_cmp(const void *elt, const void *key)
{
  const block_entry_t *e1 = elt;
  const block_entry_t *e2 = key;

  return e1->block_nr != e2->block_nr;
}

/**
 * compare two elements of the ir_op hash
 */
static int opcode_cmp_2(const void *elt, const void *key)
{
  const ir_op *e1 = elt;
  const ir_op *e2 = key;

  return e1->code != e2->code;
}

/**
 * compare two elements of the address_mark set
 */
static int address_mark_cmp(const void *elt, const void *key, size_t size)
{
  const address_mark_entry_t *e1 = elt;
  const address_mark_entry_t *e2 = key;

  /* compare only the nodes, the rest is used as data container */
  return e1->node != e2->node;
}

/**
 * clears all counter in a node_entry_t
 */
static void opcode_clear_entry(node_entry_t *elem)
{
  cnt_clr(&elem->cnt_alive);
  cnt_clr(&elem->new_node);
  cnt_clr(&elem->into_Id);
}

/**
 * Returns the associates node_entry_t for an ir_op
 */
static node_entry_t *opcode_get_entry(const ir_op *op, pset *set)
{
  node_entry_t key;
  node_entry_t *elem;

  key.op = op;

  elem = pset_find(set, &key, op->code);
  if (elem)
    return elem;

  elem = obstack_alloc(&status->cnts, sizeof(*elem));

  /* clear counter */
  opcode_clear_entry(elem);

  elem->op = op;

  return pset_insert(set, elem, op->code);
}

/**
 * Returns the associates ir_op for an opcode
 */
static ir_op *opcode_find_entry(opcode code, pset *set)
{
  ir_op key;

  key.code = code;
  return pset_find(set, &key, code);
}

/**
 * clears all counter in a graph_entry_t
 */
static void graph_clear_entry(graph_entry_t *elem, int all)
{
  if (all) {
    cnt_clr(&elem->cnt_walked);
    cnt_clr(&elem->cnt_walked_blocks);
    cnt_clr(&elem->cnt_was_inlined);
    cnt_clr(&elem->cnt_got_inlined);
    cnt_clr(&elem->cnt_strength_red);
  }
  cnt_clr(&elem->cnt_edges);
  cnt_clr(&elem->cnt_all_calls);
  cnt_clr(&elem->cnt_indirect_calls);
}

/**
 * Returns the associated graph_entry_t for an irg
 */
static graph_entry_t *graph_get_entry(ir_graph *irg, pset *set)
{
  graph_entry_t key;
  graph_entry_t *elem;
  int i;

  key.irg = irg;

  elem = pset_find(set, &key, HASH_PTR(irg));
  if (elem)
    return elem;

  /* allocate a new one */
  elem = obstack_alloc(&status->cnts, sizeof(*elem));

  /* clear counter */
  graph_clear_entry(elem, 1);

  /* new hash table for opcodes here  */
  elem->opcode_hash  = new_pset(opcode_cmp, 5);
  elem->block_hash   = new_pset(block_cmp, 5);
  elem->address_mark = new_set(address_mark_cmp, 5);
  elem->irg          = irg;

  for (i = 0; i < sizeof(elem->opt_hash)/sizeof(elem->opt_hash[0]); ++i)
    elem->opt_hash[i] = new_pset(opt_cmp, 4);

  return pset_insert(set, elem, HASH_PTR(irg));
}

/**
 * clears all counter in an opt_entry_t
 */
static void opt_clear_entry(opt_entry_t *elem)
{
  cnt_clr(&elem->count);
}

/**
 * Returns the associated opt_entry_t for an ir_op
 */
static opt_entry_t *opt_get_entry(const ir_op *op, pset *set)
{
  opt_entry_t key;
  opt_entry_t *elem;

  key.op = op;

  elem = pset_find(set, &key, op->code);
  if (elem)
    return elem;

  elem = obstack_alloc(&status->cnts, sizeof(*elem));

  /* clear new counter */
  opt_clear_entry(elem);

  elem->op = op;

  return pset_insert(set, elem, op->code);
}

/**
 * clears all counter in a block_entry_t
 */
static void block_clear_entry(block_entry_t *elem)
{
  cnt_clr(&elem->cnt_nodes);
  cnt_clr(&elem->cnt_edges);
  cnt_clr(&elem->cnt_in_edges);
  cnt_clr(&elem->cnt_out_edges);
}

/**
 * Returns the associated block_entry_t for an block
 */
static block_entry_t *block_get_entry(long block_nr, pset *set)
{
  block_entry_t key;
  block_entry_t *elem;

  key.block_nr = block_nr;

  elem = pset_find(set, &key, block_nr);
  if (elem)
    return elem;

  elem = obstack_alloc(&status->cnts, sizeof(*elem));

  /* clear new counter */
  block_clear_entry(elem);

  elem->block_nr = block_nr;

  return pset_insert(set, elem, block_nr);
}


/**
 * Returns the ir_op for an IR-node,
 * handles special cases and return pseudo op codes
 */
static ir_op *stat_get_irn_op(ir_node *node)
{
  ir_op *op = get_irn_op(node);

  if (op == op_Phi && get_irn_arity(node) == 0) {
    /* special case, a Phi0 node, count on extra counter */
    op = status->op_Phi0 ? status->op_Phi0 : op;
  }
  else if (op == op_Phi && get_irn_mode(node) == mode_M) {
    /* special case, a Memory Phi node, count on extra counter */
    op = status->op_PhiM ? status->op_PhiM : op;
  }
  else if (op == op_Proj && get_irn_mode(node) == mode_M) {
    /* special case, a Memory Proj node, count on extra counter */
    op = status->op_ProjM ? status->op_ProjM : op;
  }
  else if (op == op_Mul &&
           (get_irn_op(get_Mul_left(node)) == op_Const || get_irn_op(get_Mul_right(node)) == op_Const)) {
    /* special case, a Multiply by a const, count on extra counter */
    op = status->op_MulC ? status->op_MulC : op;
  }
  else if (op == op_Div && get_irn_op(get_Div_right(node)) == op_Const) {
    /* special case, a division by a const, count on extra counter */
    op = status->op_DivC ? status->op_DivC : op;
  }
  else if (op == op_Mod && get_irn_op(get_Mod_right(node)) == op_Const) {
    /* special case, a module by a const, count on extra counter */
    op = status->op_ModC ? status->op_ModC : op;
  }
  else if (op == op_DivMod && get_irn_op(get_DivMod_right(node)) == op_Const) {
    /* special case, a division/modulo by a const, count on extra counter */
    op = status->op_DivModC ? status->op_DivModC : op;
  }

  return op;
}

/**
 * update the block counter
 */
static void count_block_info(ir_node *node, graph_entry_t *graph)
{
  ir_op *op = get_irn_op(node);
  ir_node *block;
  block_entry_t *b_entry;
  int i, arity;

  /* check for block */
  if (op == op_Block) {
    arity = get_irn_arity(node);
    b_entry = block_get_entry(get_irn_node_nr(node), graph->block_hash);

    /* count all incoming edges */
    for (i = 0; i < arity; ++i) {
      ir_node *pred = get_irn_n(node, i);
      ir_node *other_block = get_nodes_block(pred);
      block_entry_t *b_entry_other = block_get_entry(get_irn_node_nr(other_block), graph->block_hash);

      cnt_inc(&b_entry->cnt_in_edges);	/* an edge coming from another block */
      cnt_inc(&b_entry_other->cnt_out_edges);
    }
    return;
  }

  block   = get_nodes_block(node);
  b_entry = block_get_entry(get_irn_node_nr(block), graph->block_hash);

  /* we have a new nodes */
  cnt_inc(&b_entry->cnt_nodes);

  arity = get_irn_arity(node);

  for (i = 0; i < arity; ++i) {
    ir_node *pred = get_irn_n(node, i);
    ir_node *other_block;

    if (get_irn_op(pred) == op_Block)
      continue;

    other_block = get_nodes_block(pred);

    if (other_block == block)
      cnt_inc(&b_entry->cnt_edges);	/* a in block edge */
    else {
      block_entry_t *b_entry_other = block_get_entry(get_irn_node_nr(other_block), graph->block_hash);

      cnt_inc(&b_entry->cnt_in_edges);	/* an edge coming from another block */
      cnt_inc(&b_entry_other->cnt_out_edges);
    }
  }
}

/**
 * update info on calls
 */
static void update_call_stat(ir_node *call, graph_entry_t *graph)
{
  ir_node *block = get_nodes_block(call);
  ir_node *ptr = get_Call_ptr(call);
  entity *ent = NULL;
  ir_graph *callee = NULL;

  /*
   * If the block is bad, the whole subgraph will collapse later
   * so do not count this call.
   * This happens in dead code.
   */
  if (is_Bad(block))
    return;

  cnt_inc(&graph->cnt_all_calls);

  /* found a call, this function is not a leaf */
  graph->is_leaf = 0;

  if (get_irn_op(ptr) == op_SymConst) {
    if (get_SymConst_kind(ptr) == symconst_addr_ent) {
      /* ok, we seems to know the entity */
      ent = get_SymConst_entity(ptr);
      callee = get_entity_irg(ent);

      /* it is recursive, if it calls at least once */
      if (callee == graph->irg)
	graph->is_recursive = 1;
    }
  }
  else {
    /* indirect call, be could not predict */
    cnt_inc(&graph->cnt_indirect_calls);

    /* NOT a leaf call */
    graph->is_leaf_call = LCS_NON_LEAF_CALL;
  }

  /* check, if it's a chain-call: Then, the call-block
   * must dominate the end block. */
  {
    ir_node *curr = get_irg_end_block(graph->irg);
    int depth = get_Block_dom_depth(block);

    for (; curr != block && get_Block_dom_depth(curr) > depth;) {
      curr = get_Block_idom(curr);

      if (! curr || is_no_Block(curr))
	break;
    }

    if (curr != block)
      graph->is_chain_call = 0;
  }

  /* check, if the callee is a leaf */
  if (callee) {
    graph_entry_t *called = graph_get_entry(callee, status->irg_hash);

    if (called->is_analyzed) {
      if (! called->is_leaf)
        graph->is_leaf_call = LCS_NON_LEAF_CALL;
    }
  }
}

/**
 * update info on calls for graphs on the wait queue
 */
static void update_call_stat_2(ir_node *call, graph_entry_t *graph)
{
  ir_node *block = get_nodes_block(call);
  ir_node *ptr = get_Call_ptr(call);
  entity *ent = NULL;
  ir_graph *callee = NULL;

  /*
   * If the block is bad, the whole subgraph will collapse later
   * so do not count this call.
   * This happens in dead code.
   */
  if (is_Bad(block))
    return;

  if (get_irn_op(ptr) == op_SymConst) {
    if (get_SymConst_kind(ptr) == symconst_addr_ent) {
      /* ok, we seems to know the entity */
      ent = get_SymConst_entity(ptr);
      callee = get_entity_irg(ent);
    }
  }

  /* check, if the callee is a leaf */
  if (callee) {
    graph_entry_t *called = graph_get_entry(callee, status->irg_hash);

    assert(called->is_analyzed);

    if (! called->is_leaf)
      graph->is_leaf_call = LCS_NON_LEAF_CALL;
  }
  else
    graph->is_leaf_call = LCS_NON_LEAF_CALL;
}

/**
 * walker for reachable nodes count
 */
static void update_node_stat(ir_node *node, void *env)
{
  graph_entry_t *graph = env;
  node_entry_t *entry;

  ir_op *op = stat_get_irn_op(node);
  int arity = get_irn_arity(node);

  entry = opcode_get_entry(op, graph->opcode_hash);

  cnt_inc(&entry->cnt_alive);
  cnt_add_i(&graph->cnt_edges, arity);

  /* count block edges */
  count_block_info(node, graph);

  /* check for properties that depends on calls like recursion/leaf/indirect call */
  if (op == op_Call)
    update_call_stat(node, graph);
}

/**
 * walker for reachable nodes count for graphs on the wait_q
 */
static void update_node_stat_2(ir_node *node, void *env)
{
  graph_entry_t *graph = env;

  /* check for properties that depends on calls like recursion/leaf/indirect call */
  if (get_irn_op(node) == op_Call)
    update_call_stat_2(node, graph);
}

/**
 * get the current address mark
 */
static unsigned get_adr_mark(graph_entry_t *graph, ir_node *node)
{
  address_mark_entry_t *value = set_find(graph->address_mark, &node, sizeof(*value), HASH_PTR(node));

  return value ? value->mark : 0;
}

/**
 * set the current address mark
 */
static void set_adr_mark(graph_entry_t *graph, ir_node *node, unsigned val)
{
  address_mark_entry_t *value = set_insert(graph->address_mark, &node, sizeof(*value), HASH_PTR(node));

  value->mark = val;
}

/**
 * a vcg attribute hook: Color a node with a different color if
 * it's identified as a part of an address expression or at least referenced
 * by an address expression.
 */
static int stat_adr_mark_hook(FILE *F, ir_node *node, ir_node *local)
{
  ir_node *n           = local ? local : node;
  ir_graph *irg        = get_irn_irg(n);
  graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);
  unsigned mark        = get_adr_mark(graph, n);

  if (mark & MARK_ADDRESS_CALC)
    fprintf(F, "color: purple");
  else if ((mark & (MARK_REF_ADR | MARK_REF_NON_ADR)) == MARK_REF_ADR)
    fprintf(F, "color: pink");
  else if ((mark & (MARK_REF_ADR | MARK_REF_NON_ADR)) == (MARK_REF_ADR|MARK_REF_NON_ADR))
    fprintf(F, "color: lightblue");
  else
    return 0;

  /* I know the color! */
  return 1;
}

/**
 * walker that marks every node that is an address calculation
 *
 * predecessor nodes must be visited first. We ensure this by
 * calling in in the post of an outs walk. This should work even in cycles,
 * while the pre in a normal walk will not.
 */
static void mark_address_calc(ir_node *node, void *env)
{
  graph_entry_t *graph = env;
  ir_mode *mode = get_irn_mode(node);
  int i, n;
  unsigned mark_preds = MARK_REF_NON_ADR;

  if (! mode_is_numP(mode))
    return;

  if (mode_is_reference(mode)) {
    /* a reference is calculated here, we are sure */
    set_adr_mark(graph, node, MARK_ADDRESS_CALC);

    mark_preds = MARK_REF_ADR;
  }
  else {
    unsigned mark = get_adr_mark(graph, node);

    if ((mark & (MARK_REF_ADR | MARK_REF_NON_ADR)) == MARK_REF_ADR) {
      /*
       * this node has not an reference mode, but is only
       * referenced by address calculations
       */
      mark_preds = MARK_REF_ADR;
    }
  }

  /* mark all predecessors */
  for (i = 0, n = get_irn_arity(node); i < n; ++i) {
    ir_node *pred = get_irn_n(node, i);

    set_adr_mark(graph, pred, get_adr_mark(graph, pred) | mark_preds);
  }
}

/**
 * Called for every graph when the graph is either deleted or stat_finish()
 * is called, must recalculate all statistic info.
 *
 * @param global    The global entry
 * @param graph     The current entry
 */
static void update_graph_stat(graph_entry_t *global, graph_entry_t *graph)
{
  node_entry_t *entry;

  /* clear first the alive counter in the graph */
  for (entry = pset_first(graph->opcode_hash); entry; entry = pset_next(graph->opcode_hash)) {
    cnt_clr(&entry->cnt_alive);
  }

  /* set pessimistic values */
  graph->is_leaf       = 1;
  graph->is_leaf_call  = LCS_UNKNOWN;
  graph->is_recursive  = 0;
  graph->is_chain_call = 1;

  /* we need dominator info */
  if (graph->irg != get_const_code_irg())
    if (get_irg_dom_state(graph->irg) != dom_consistent)
      compute_doms(graph->irg);

  /* count the nodes in the graph */
  irg_walk_graph(graph->irg, update_node_stat, NULL, graph);

#if 0
  entry = opcode_get_entry(op_Call, graph->opcode_hash);

  /* check if we have more than 1 call */
  if (cnt_gt(entry->cnt_alive, 1))
    graph->is_chain_call = 0;
#endif

  /* recursive functions are never chain calls, leafs don't have calls */
  if (graph->is_recursive || graph->is_leaf)
    graph->is_chain_call = 0;

  /* assume we walk every graph only ONCE, we could sum here the global count */
  for (entry = pset_first(graph->opcode_hash); entry; entry = pset_next(graph->opcode_hash)) {
    node_entry_t *g_entry = opcode_get_entry(entry->op, global->opcode_hash);

    /* update the node counter */
    cnt_add(&g_entry->cnt_alive, &entry->cnt_alive);
  }

  /* update the edge counter */
  cnt_add(&global->cnt_edges, &graph->cnt_edges);

  /* count the number of address calculation */
  if (graph->irg != get_const_code_irg()) {
    ir_graph *rem = current_ir_graph;

    if (get_irg_outs_state(graph->irg) != outs_consistent)
      compute_outs(graph->irg);

    /* Must be done an the outs graph */
    current_ir_graph = graph->irg;
    irg_out_walk(get_irg_start(graph->irg), NULL, mark_address_calc, graph);
    current_ir_graph = rem;

#if 0
    set_dump_node_vcgattr_hook(stat_adr_mark_hook);
    dump_ir_block_graph(graph->irg, "-adr");
    set_dump_node_vcgattr_hook(NULL);
#endif
  }

  /* count the DAG's */
  if (status->stat_options & FIRMSTAT_COUNT_DAG)
    count_dags_in_graph(global, graph);

  /* calculate the patterns of this graph */
  stat_calc_pattern_history(graph->irg);

  /* leaf function did not call others */
  if (graph->is_leaf)
    graph->is_leaf_call = LCS_NON_LEAF_CALL;
  else if (graph->is_leaf_call == LCS_UNKNOWN) {
    /* we still don't know if this graph calls leaf-functions, so enqueue */
    pdeq_putl(status->wait_q, graph);
  }

  /* we have analyzed this graph */
  graph->is_analyzed = 1;
}

/**
 * Called for every graph that was on the wait_q in stat_finish()
 *  must finish all statistic info calculations.
 *
 * @param global    The global entry
 * @param graph     The current entry
 */
static void update_graph_stat_2(graph_entry_t *global, graph_entry_t *graph)
{
  if (graph->is_deleted) {
    /* deleted, ignore */
    return;
  }

  if (graph->irg) {
    /* count the nodes in the graph */
    irg_walk_graph(graph->irg, update_node_stat_2, NULL, graph);

    if (graph->is_leaf_call == LCS_UNKNOWN)
      graph->is_leaf_call = LCS_LEAF_CALL;
  }
}

/**
 * register a dumper
 */
static void stat_register_dumper(const dumper_t *dumper)
{
  dumper_t *p = xmalloc(sizeof(*p));

  if (p) {
    *p = *dumper;

    p->next        = status->dumper;
    p->status      = status;
    status->dumper = p;
  }

  /* FIXME: memory leak */
}

/**
 * dumps an irg
 */
static void stat_dump_graph(graph_entry_t *entry)
{
  dumper_t *dumper;

  for (dumper = status->dumper; dumper; dumper = dumper->next) {
    if (dumper->dump_graph)
      dumper->dump_graph(dumper, entry);
  }
}

/**
 * initialize the dumper
 */
static void stat_dump_init(const char *name)
{
  dumper_t *dumper;

  for (dumper = status->dumper; dumper; dumper = dumper->next) {
    if (dumper->init)
      dumper->init(dumper, name);
  }
}

/**
 * finish the dumper
 */
static void stat_dump_finish(void)
{
  dumper_t *dumper;

  for (dumper = status->dumper; dumper; dumper = dumper->next) {
    if (dumper->finish)
      dumper->finish(dumper);
  }
}

/* ---------------------------------------------------------------------- */

/*
 * helper: get an ir_op from an opcode
 */
ir_op *stat_get_op_from_opcode(opcode code)
{
  return opcode_find_entry(code, status->ir_op_hash);
}

/** A new IR op is registered. */
static void stat_new_ir_op(void *ctx, ir_op *op)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(NULL, status->irg_hash);

    /* execute for side effect :-) */
    opcode_get_entry(op, graph->opcode_hash);

    pset_insert(status->ir_op_hash, op, op->code);
  }
  STAT_LEAVE;
}

/** An IR op is freed. */
static void stat_free_ir_op(void *ctx, ir_op *op)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
  }
  STAT_LEAVE;
}

/** A new node is created. */
static void stat_new_node(void *ctx, ir_node *node)
{
  if (! status->stat_options)
    return;

  /* do NOT count during dead node elimination */
  if (status->in_dead_node_elim > 0)
    return;

  STAT_ENTER;
  {
    node_entry_t *entry;
    graph_entry_t *graph;
    ir_op *op = stat_get_irn_op(node);

    /* increase global value */
    graph = graph_get_entry(NULL, status->irg_hash);
    entry = opcode_get_entry(op, graph->opcode_hash);
    cnt_inc(&entry->new_node);

    /* increase local value */
    graph = graph_get_entry(current_ir_graph, status->irg_hash);
    entry = opcode_get_entry(op, graph->opcode_hash);
    cnt_inc(&entry->new_node);
  }
  STAT_LEAVE;
}

/** A node is changed into a Id node */
static void stat_turn_into_id(void *ctx, ir_node *node)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    node_entry_t *entry;
    graph_entry_t *graph;
    ir_op *op = stat_get_irn_op(node);

    /* increase global value */
    graph = graph_get_entry(NULL, status->irg_hash);
    entry = opcode_get_entry(op, graph->opcode_hash);
    cnt_inc(&entry->into_Id);

    /* increase local value */
    graph = graph_get_entry(current_ir_graph, status->irg_hash);
    entry = opcode_get_entry(op, graph->opcode_hash);
    cnt_inc(&entry->into_Id);
  }
  STAT_LEAVE;
}

/** A new graph was created */
static void stat_new_graph(void *ctx, ir_graph *irg, entity *ent)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    /* execute for side effect :-) */
    graph_entry_t * graph = graph_get_entry(irg, status->irg_hash);

    graph->ent           = ent;
    graph->is_deleted    = 0;
    graph->is_leaf       = 0;
    graph->is_leaf_call  = 0;
    graph->is_recursive  = 0;
    graph->is_chain_call = 0;
    graph->is_analyzed   = 0;
  }
  STAT_LEAVE;
}

/**
 * A graph will be deleted
 */
static void stat_free_graph(void *ctx, ir_graph *irg)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph  = graph_get_entry(irg, status->irg_hash);
    graph_entry_t *global = graph_get_entry(NULL, status->irg_hash);

    graph->is_deleted = 1;

    if (status->stat_options & FIRMSTAT_COUNT_DELETED) {
      /* count the nodes of the graph yet, it will be destroyed later */
      update_graph_stat(global, graph);
    }
  }
  STAT_LEAVE;
}

/**
 * A walk over a graph is initiated. Do not count walks from statistic code.
 */
static void stat_irg_walk(void *ctx, ir_graph *irg, void *pre, void *post)
{
  if (! status->stat_options)
    return;

  STAT_ENTER_SINGLE;
  {
    graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);

    cnt_inc(&graph->cnt_walked);
  }
  STAT_LEAVE;
}

/**
 * A walk over a graph in block-wise order is initiated. Do not count walks from statistic code.
 */
static void stat_irg_walk_blkwise(void *ctx, ir_graph *irg, void *pre, void *post)
{
  /* for now, do NOT differentiate between blockwise and normal */
  stat_irg_walk(ctx, irg, pre, post);
}

/**
 * A walk over the graph's blocks is initiated. Do not count walks from statistic code.
 */
static void stat_irg_block_walk(void *ctx, ir_graph *irg, ir_node *node, void *pre, void *post)
{
  if (! status->stat_options)
    return;

  STAT_ENTER_SINGLE;
  {
    graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);

    cnt_inc(&graph->cnt_walked_blocks);
  }
  STAT_LEAVE;
}

/**
 * called for every node that is removed due to an optimization
 */
static void removed_due_opt(ir_node *n, pset *set)
{
  ir_op *op          = stat_get_irn_op(n);
  opt_entry_t *entry = opt_get_entry(op, set);

  /* increase global value */
  cnt_inc(&entry->count);
}

/**
 * Some nodes were optimized into some others due to an optimization
 */
static void stat_merge_nodes(
    void *ctx,
    ir_node **new_node_array, int new_num_entries,
    ir_node **old_node_array, int old_num_entries,
    hook_opt_kind opt)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    int i, j;
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);

    if (status->reassoc_run)
      opt = HOOK_OPT_REASSOC;

    for (i = 0; i < old_num_entries; ++i) {
      for (j = 0; j < new_num_entries; ++j)
        if (old_node_array[i] == new_node_array[j])
          break;

      /* nodes might be in new and old, these are NOT removed */
      if (j >= new_num_entries) {
        removed_due_opt(old_node_array[i], graph->opt_hash[opt]);
      }
    }
  }
  STAT_LEAVE;
}

/**
 * reassociation started/stopped.
 */
static void stat_reassociate(void *ctx, int flag)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    status->reassoc_run = flag;
  }
  STAT_LEAVE;
}

/**
 * A node was lowered into other nodes
 */
static void stat_lower(void *ctx, ir_node *node)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);

    removed_due_opt(node, graph->opt_hash[HOOK_LOWERED]);
  }
  STAT_LEAVE;
}

/**
 * A graph was inlined
 */
static void stat_inline(void *ctx, ir_node *call, ir_graph *called_irg)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    ir_graph *irg = get_irn_irg(call);
    graph_entry_t *i_graph = graph_get_entry(called_irg, status->irg_hash);
    graph_entry_t *graph   = graph_get_entry(irg, status->irg_hash);

    cnt_inc(&graph->cnt_got_inlined);
    cnt_inc(&i_graph->cnt_was_inlined);
  }
  STAT_LEAVE;
}

/**
 * A graph with tail-recursions was optimized.
 */
static void stat_tail_rec(void *ctx, ir_graph *irg)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
  }
  STAT_LEAVE;
}

/**
 * Strength reduction was performed on an iteration variable.
 */
static void stat_strength_red(void *ctx, ir_graph *irg, ir_node *strong, ir_node *cmp)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);
    cnt_inc(&graph->cnt_strength_red);

    removed_due_opt(strong, graph->opt_hash[HOOK_OPT_STRENGTH_RED]);
  }
  STAT_LEAVE;
}

/**
 * Start the dead node elimination.
 */
static void stat_dead_node_elim_start(void *ctx, ir_graph *irg)
{
  if (! status->stat_options)
    return;

  ++status->in_dead_node_elim;
}

/**
 * Stops the dead node elimination.
 */
static void stat_dead_node_elim_stop(void *ctx, ir_graph *irg)
{
  if (! status->stat_options)
    return;

  --status->in_dead_node_elim;
}

/**
 * A multiply was replaced by a series of Shifts/Adds/Subs
 */
static void stat_arch_dep_replace_mul_with_shifts(void *ctx, ir_node *mul)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);
    removed_due_opt(mul, graph->opt_hash[HOOK_OPT_ARCH_DEP]);
  }
  STAT_LEAVE;
}

/**
 * A division was replaced by a series of Shifts/Muls
 */
static void stat_arch_dep_replace_div_by_const(void *ctx, ir_node *div)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);
    removed_due_opt(div, graph->opt_hash[HOOK_OPT_ARCH_DEP]);
  }
  STAT_LEAVE;
}

/**
 * A modulo was replaced by a series of Shifts/Muls
 */
static void stat_arch_dep_replace_mod_by_const(void *ctx, ir_node *mod)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);
    removed_due_opt(mod, graph->opt_hash[HOOK_OPT_ARCH_DEP]);
  }
  STAT_LEAVE;
}

/**
 * A DivMod was replaced by a series of Shifts/Muls
 */
static void stat_arch_dep_replace_DivMod_by_const(void *ctx, ir_node *divmod)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);
    removed_due_opt(divmod, graph->opt_hash[HOOK_OPT_ARCH_DEP]);
  }
  STAT_LEAVE;
}

/* Finish the statistics */
void stat_finish(const char *name)
{
  if (! status->stat_options)
    return;

  STAT_ENTER;
  {
    graph_entry_t *entry;
    graph_entry_t *global = graph_get_entry(NULL, status->irg_hash);

    stat_dump_init(name);

    /* calculate the graph statistics */
    for (entry = pset_first(status->irg_hash); entry; entry = pset_next(status->irg_hash)) {

      if (entry->irg == NULL) {
        /* special entry for the global count */
        continue;
      }

      if (! entry->is_deleted) {
        /* the graph is still alive, count the nodes on it */
        update_graph_stat(global, entry);
      }
    }

    /* some calculations are dependent, we pushed them on the wait_q */
    while (! pdeq_empty(status->wait_q)) {
      entry = pdeq_getr(status->wait_q);

      update_graph_stat_2(global, entry);
    }


    /* dump per graph */
    for (entry = pset_first(status->irg_hash); entry; entry = pset_next(status->irg_hash)) {

      if (entry->irg == NULL) {
        /* special entry for the global count */
        continue;
      }

      if (! entry->is_deleted || status->stat_options & FIRMSTAT_COUNT_DELETED)
        stat_dump_graph(entry);

      if (! entry->is_deleted) {
        /* clear the counter that are not accumulated */
        graph_clear_entry(entry, 0);
      }
    }

    /* dump global */
    stat_dump_graph(global);
    stat_dump_finish();

    stat_finish_pattern_history();

    /* clear the global counter here */
    {
      node_entry_t *entry;

      for (entry = pset_first(global->opcode_hash); entry; entry = pset_next(global->opcode_hash)) {
        opcode_clear_entry(entry);
      }
      /* clear all global counter */
      graph_clear_entry(global, 1);
    }

    /* finished */
//    status->stat_options = 0;
  }
  STAT_LEAVE;
}

static hook_entry_t stat_hooks[hook_last];

/* initialize the statistics module. */
void init_stat(unsigned enable_options)
{
#define X(a)  a, sizeof(a)-1
#define HOOK(h, fkt) \
  stat_hooks[h].hook._##h = fkt; register_hook(h, &stat_hooks[h])

  /* enable statistics */
  status->stat_options = enable_options & FIRMSTAT_ENABLED ? enable_options : 0;

  if (! status->stat_options)
    return;

  /* register all hooks */
  HOOK(hook_new_ir_op,                        stat_new_ir_op);
  HOOK(hook_free_ir_op,                       stat_free_ir_op);
  HOOK(hook_new_node,                         stat_new_node);
  HOOK(hook_turn_into_id,                     stat_turn_into_id);
  HOOK(hook_new_graph,                        stat_new_graph);
  HOOK(hook_free_graph,                       stat_free_graph);
  HOOK(hook_irg_walk,                         stat_irg_walk);
  HOOK(hook_irg_walk_blkwise,                 stat_irg_walk_blkwise);
  HOOK(hook_irg_block_walk,                   stat_irg_block_walk);
  HOOK(hook_merge_nodes,                      stat_merge_nodes);
  HOOK(hook_reassociate,                      stat_reassociate);
  HOOK(hook_lower,                            stat_lower);
  HOOK(hook_inline,                           stat_inline);
  HOOK(hook_tail_rec,                         stat_tail_rec);
  HOOK(hook_strength_red,                     stat_strength_red);
  HOOK(hook_dead_node_elim_start,             stat_dead_node_elim_start);
  HOOK(hook_dead_node_elim_stop,              stat_dead_node_elim_stop);
  HOOK(hook_arch_dep_replace_mul_with_shifts, stat_arch_dep_replace_mul_with_shifts);
  HOOK(hook_arch_dep_replace_div_by_const,    stat_arch_dep_replace_div_by_const);
  HOOK(hook_arch_dep_replace_mod_by_const,    stat_arch_dep_replace_mod_by_const);
  HOOK(hook_arch_dep_replace_DivMod_by_const, stat_arch_dep_replace_DivMod_by_const);

  obstack_init(&status->cnts);

  /* create the hash-tables */
  status->irg_hash   = new_pset(graph_cmp, 8);
  status->ir_op_hash = new_pset(opcode_cmp_2, 1);

  /* create the wait queue */
  status->wait_q     = new_pdeq();

  if (enable_options & FIRMSTAT_COUNT_STRONG_OP) {
    /* build the pseudo-ops */
    _op_Phi0.code    = get_next_ir_opcode();
    _op_Phi0.name    = new_id_from_chars(X("Phi0"));

    _op_PhiM.code    = get_next_ir_opcode();
    _op_PhiM.name    = new_id_from_chars(X("PhiM"));

    _op_ProjM.code   = get_next_ir_opcode();
    _op_ProjM.name   = new_id_from_chars(X("ProjM"));

    _op_MulC.code    = get_next_ir_opcode();
    _op_MulC.name    = new_id_from_chars(X("MulC"));

    _op_DivC.code    = get_next_ir_opcode();
    _op_DivC.name    = new_id_from_chars(X("DivC"));

    _op_ModC.code    = get_next_ir_opcode();
    _op_ModC.name    = new_id_from_chars(X("ModC"));

    _op_DivModC.code = get_next_ir_opcode();
    _op_DivModC.name = new_id_from_chars(X("DivModC"));

    status->op_Phi0    = &_op_Phi0;
    status->op_PhiM    = &_op_PhiM;
    status->op_ProjM   = &_op_ProjM;
    status->op_MulC    = &_op_MulC;
    status->op_DivC    = &_op_DivC;
    status->op_ModC    = &_op_ModC;
    status->op_DivModC = &_op_DivModC;
  }
  else {
    status->op_Phi0    = NULL;
    status->op_PhiM    = NULL;
    status->op_ProjM   = NULL;
    status->op_MulC    = NULL;
    status->op_DivC    = NULL;
    status->op_ModC    = NULL;
    status->op_DivModC = NULL;
  }

  /* register the dumper */
  stat_register_dumper(&simple_dumper);

  if (enable_options & FIRMSTAT_CSV_OUTPUT)
    stat_register_dumper(&csv_dumper);

  /* initialize the pattern hash */
  stat_init_pattern_history(enable_options & FIRMSTAT_PATTERN_ENABLED);
#undef HOOK
#undef X
}

#endif /* FIRM_STATISTICS */
