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
#include <stdlib.h>
#include <string.h>

#include "firmstat_t.h"
#include "pattern.h"
#include "dags.h"

/**
 * names of the optimizations
 */
static const char *opt_names[] = {
  "straightening optimization",
  "if simplification",
  "constant evaluation",
  "algebraic simplification",
  "Phi optmization",
  "Write-After-Write optimization",
  "Write-After-Read optimization",
  "Read-After-Write optimization",
  "Read-After-Read optimization",
  "Read-a-Const optimization",
  "Tuple optimization",
  "ID optimization",
  "Common subexpression elimination",
  "Strength reduction",
  "Architecture dependant optimization",
  "Reassociation optimization",
  "Polymorphic call optimization",
  "Lowered",
};

/*
 * need this to be static:
 * Special pseudo Opcodes that we need to count some interesting cases
 */

/**
 * The Phi0, a node that is created during SSA construction
 */
static ir_op _op_Phi0;

/** The PhiM, just to count memorty Phi's. */
static ir_op _op_PhiM;

/** The Mul by Const node. */
static ir_op _op_MulC;

/** The Div by Const node. */
static ir_op _op_DivC;

/** The Div by Const node. */
static ir_op _op_ModC;

/** The Div by Const node. */
static ir_op _op_DivModC;

/* ---------------------------------------------------------------------------------- */

#define STAT_ENTER		++status->recursive
#define STAT_LEAVE		--status->recursive
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
 * calculates a hash value for an irg
 * Addresses are typically aligned at 32bit, so we ignore the lowest bits
 */
static INLINE unsigned irg_hash(const ir_graph *irg)
{
  return (unsigned)irg >> 3;
}

/**
 * clears all counter in a graph_entry_t
 */
static void graph_clear_entry(graph_entry_t *elem)
{
  cnt_clr(&elem->cnt_walked);
  cnt_clr(&elem->cnt_walked_blocks);
  cnt_clr(&elem->cnt_was_inlined);
  cnt_clr(&elem->cnt_got_inlined);
  cnt_clr(&elem->cnt_strength_red);
  cnt_clr(&elem->cnt_edges);
  cnt_clr(&elem->cnt_all_calls);
  cnt_clr(&elem->cnt_indirect_calls);
}

/**
 * Returns the acssociates graph_entry_t for an irg
 */
static graph_entry_t *graph_get_entry(ir_graph *irg, pset *set)
{
  graph_entry_t key;
  graph_entry_t *elem;
  int i;

  key.irg = irg;

  elem = pset_find(set, &key, irg_hash(irg));
  if (elem)
    return elem;

  /* allocate a new one */
  elem = obstack_alloc(&status->cnts, sizeof(*elem));

  /* clear counter */
  graph_clear_entry(elem);

  /* new hash table for opcodes here  */
  elem->opcode_hash  = new_pset(opcode_cmp, 5);
  elem->block_hash   = new_pset(block_cmp, 5);
  elem->irg          = irg;

  for (i = 0; i < sizeof(elem->opt_hash)/sizeof(elem->opt_hash[0]); ++i)
    elem->opt_hash[i] = new_pset(opt_cmp, 4);

  return pset_insert(set, elem, irg_hash(irg));
}

/**
 * clears all counter in an opt_entry_t
 */
static void opt_clear_entry(opt_entry_t *elem)
{
  cnt_clr(&elem->count);
}

/**
 * Returns the associates opt_entry_t for an ir_op
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
 * Returns the associates block_entry_t for an block
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

  if (op->code == iro_Phi && get_irn_arity(node) == 0) {
    /* special case, a Phi0 node, count on extra counter */
    op = status->op_Phi0;
  }
  else if (op->code == iro_Phi && get_irn_mode(node) == mode_M) {
    /* special case, a Memory Phi node, count on extra counter */
    op = status->op_PhiM;
  }
  else if (op->code == iro_Mul &&
           (get_irn_op(get_Mul_left(node)) == op_Const || get_irn_op(get_Mul_right(node)) == op_Const)) {
    /* special case, a Multiply by a const, count on extra counter */
    op = status->op_MulC ? status->op_MulC : op_Mul;
  }
  else if (op->code == iro_Div && get_irn_op(get_Div_right(node)) == op_Const) {
    /* special case, a division by a const, count on extra counter */
    op = status->op_DivC ? status->op_DivC : op_Div;
  }
  else if (op->code == iro_Mod && get_irn_op(get_Mod_right(node)) == op_Const) {
    /* special case, a module by a const, count on extra counter */
    op = status->op_ModC ? status->op_ModC : op_Mod;
  }
  else if (op->code == iro_DivMod && get_irn_op(get_DivMod_right(node)) == op_Const) {
    /* special case, a division/modulo by a const, count on extra counter */
    op = status->op_DivModC ? status->op_DivModC : op_DivMod;
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
  else if (op == op_Call) {
    // return;
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
  ir_node *ptr = get_Call_ptr(call);
  entity *ent = NULL;

  cnt_inc(&graph->cnt_all_calls);

  /* found a call, is not a leaf function */
  graph->is_leaf = 0;

  if (get_irn_op(ptr) == op_SymConst) {
    if (get_SymConst_kind(ptr) == symconst_addr_ent) {
      /* ok, we seems to know the entity */
      ent = get_SymConst_entity(ptr);

      if (get_entity_irg(ent) == graph->irg)
	graph->is_recursive = 1;
    }
  }
  else {
    /* indirect call */
    cnt_inc(&graph->cnt_indirect_calls);
  }

  /* check, if it's a chain-call: Then, the call-block
   * must dominate the end block. */
  {
    ir_node *curr = get_irg_end_block(graph->irg);
    ir_node *block  = get_nodes_block(call);
    int depth = get_Block_dom_depth(block);

    for (; curr != block && get_Block_dom_depth(curr) > depth;) {
      curr = get_Block_idom(curr);

      if (! curr || is_no_Block(curr))
	break;
    }

    if (curr != block)
      graph->is_chain_call = 0;
  }
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
  if (get_irn_op(node) == op_Call)
    update_call_stat(node, graph);
}

/**
 * called for every graph when the graph is either deleted or stat_finish
 * is called, must recalculate all statistic info
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
}

/**
 * register a dumper
 */
static void stat_register_dumper(dumper_t *dumper)
{
  dumper->next   = status->dumper;
  status->dumper = dumper;
}

/**
 * dumps an irg
 */
static void dump_graph(graph_entry_t *entry)
{
  dumper_t *dumper;

  for (dumper = status->dumper; dumper; dumper = dumper->next) {
    if (dumper->dump_graph)
      dumper->dump_graph(dumper, entry);
  }
}

/**
 * initialise the dumper
 */
static void dump_init(const char *name)
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
static void dump_finish(void)
{
  dumper_t *dumper;

  for (dumper = status->dumper; dumper; dumper = dumper->next) {
    if (dumper->finish)
      dumper->finish(dumper);
  }
}

/* ---------------------------------------------------------------------- */

/**
 * dumps a opcode hash into human readable form
 */
static void simple_dump_opcode_hash(dumper_t *dmp, pset *set)
{
  node_entry_t *entry;
  counter_t f_alive;
  counter_t f_new_node;
  counter_t f_Id;

  cnt_clr(&f_alive);
  cnt_clr(&f_new_node);
  cnt_clr(&f_Id);

  fprintf(dmp->f, "%-16s %-8s %-8s %-8s\n", "Opcode", "alive", "created", "->Id");
  for (entry = pset_first(set); entry; entry = pset_next(set)) {
    fprintf(dmp->f, "%-16s %8d %8d %8d\n",
      get_id_str(entry->op->name), entry->cnt_alive.cnt[0], entry->new_node.cnt[0], entry->into_Id.cnt[0]);

    cnt_add(&f_alive,    &entry->cnt_alive);
    cnt_add(&f_new_node, &entry->new_node);
    cnt_add(&f_Id,       &entry->into_Id);
  }
  fprintf(dmp->f, "-------------------------------------------\n");
  fprintf(dmp->f, "%-16s %8d %8d %8d\n", "Sum",
     f_alive.cnt[0],
     f_new_node.cnt[0],
     f_Id.cnt[0]);
}

/**
 * dumps a optimization hash into human readable form
 */
static void simple_dump_opt_hash(dumper_t *dmp, pset *set, int index)
{
  opt_entry_t *entry = pset_first(set);

  if (entry) {
    fprintf(dmp->f, "\n%s:\n", opt_names[index]);
    fprintf(dmp->f, "%-16s %-8s\n", "Opcode", "deref");

    for (; entry; entry = pset_next(set)) {
      fprintf(dmp->f, "%-16s %8d\n",
        get_id_str(entry->op->name), entry->count.cnt[0]);
    }
  }
}

/**
 * dumps the endges count
 */
static void simple_dump_edges(dumper_t *dmp, counter_t *cnt)
{
  fprintf(dmp->f, "%-16s %8d\n", "Edges", cnt->cnt[0]);
}

/**
 * dumps the IRG
 */
static void simple_dump_graph(dumper_t *dmp, graph_entry_t *entry)
{
  int dump_opts = 1;
  block_entry_t *b_entry;

  if (entry->irg) {
    ir_graph *const_irg = get_const_code_irg();

    if (entry->irg == const_irg) {
      fprintf(dmp->f, "\nConst code Irg %p", (void *)entry->irg);
    }
    else {
      if (entry->ent)
        fprintf(dmp->f, "\nEntity %s, Irg %p", get_entity_name(entry->ent), (void *)entry->irg);
      else
        fprintf(dmp->f, "\nIrg %p", (void *)entry->irg);
    }

    fprintf(dmp->f, " %swalked %u over blocks %u:\n"
                    " was inlined   : %u\n"
		    " got inlined   : %u\n"
		    " strength red  : %u\n"
		    " leaf function : %s\n"
		    " recursive     : %s\n"
		    " chain call    : %s\n"
                    " calls         : %u\n"
                    " indirect calls: %u\n",
        entry->is_deleted ? "DELETED " : "",
        entry->cnt_walked.cnt[0], entry->cnt_walked_blocks.cnt[0],
        entry->cnt_was_inlined.cnt[0],
        entry->cnt_got_inlined.cnt[0],
	entry->cnt_strength_red.cnt[0],
	entry->is_leaf ? "YES" : "NO",
	entry->is_recursive ? "YES" : "NO",
	entry->is_chain_call ? "YES" : "NO",
        entry->cnt_all_calls.cnt[0],
        entry->cnt_indirect_calls.cnt[0]
    );
  }
  else {
    fprintf(dmp->f, "\nGlobals counts:\n");
    fprintf(dmp->f, "--------------\n");
    dump_opts = 0;
  }

  simple_dump_opcode_hash(dmp, entry->opcode_hash);
  simple_dump_edges(dmp, &entry->cnt_edges);

  /* effects of optimizations */
  if (dump_opts) {
    int i;

    for (i = 0; i < sizeof(entry->opt_hash)/sizeof(entry->opt_hash[0]); ++i) {
      simple_dump_opt_hash(dmp, entry->opt_hash[i], i);
    }

    /* dump block info */
    fprintf(dmp->f, "\n%12s %12s %12s %12s %12s %12s\n", "Block Nr", "Nodes", "intern E", "incoming E", "outgoing E", "quot");
    for (b_entry = pset_first(entry->block_hash);
	 b_entry;
	 b_entry = pset_next(entry->block_hash)) {
      fprintf(dmp->f, "BLK %12ld %12u %12u %12u %12u %4.8f\n",
	  b_entry->block_nr,
	  b_entry->cnt_nodes.cnt[0],
	  b_entry->cnt_edges.cnt[0],
	  b_entry->cnt_in_edges.cnt[0],
	  b_entry->cnt_out_edges.cnt[0],
	  (double)b_entry->cnt_edges.cnt[0] / (double)b_entry->cnt_nodes.cnt[0]
      );
    }
  }
}

/**
 * initialise the simple dumper
 */
static void simple_init(dumper_t *dmp, const char *name)
{
  char fname[2048];

  snprintf(fname, sizeof(fname), "%s.txt", name);
  dmp->f = fopen(fname, "w");
}

/**
 * finishes the simple dumper
 */
static void simple_finish(dumper_t *dmp)
{
  fclose(dmp->f);
  dmp->f = NULL;
}

/**
 * the simple human readable dumper
 */
static dumper_t simple_dumper = {
  simple_dump_graph,
  simple_init,
  simple_finish,
  NULL,
  NULL,
};

/* ---------------------------------------------------------------------- */

/**
 * count the nodes as needed:
 *
 * 1 normal (data) Phi's
 * 2 memory Phi's
 * 3 Proj
 * 0 all other nodes
 */
static void csv_count_nodes(graph_entry_t *graph, counter_t cnt[])
{
  node_entry_t *entry;
  int i;

  for (i = 0; i < 4; ++i)
    cnt_clr(&cnt[i]);

  for (entry = pset_first(graph->opcode_hash); entry; entry = pset_next(graph->opcode_hash)) {
    if (entry->op == op_Phi) {
      /* normal Phi */
      cnt_add(&cnt[1], &entry->cnt_alive);
    }
    else if (entry->op == status->op_PhiM) {
      /* memory Phi */
      cnt_add(&cnt[2], &entry->cnt_alive);
    }
    else if (entry->op == op_Proj) {
      /* Proj */
      cnt_add(&cnt[3], &entry->cnt_alive);
    }
    else {
      /* all other nodes */
      cnt_add(&cnt[0], &entry->cnt_alive);
    }
  }
}

/**
 * dumps the IRG
 */
static void csv_dump_graph(dumper_t *dmp, graph_entry_t *entry)
{
  const char *name;

  counter_t cnt[4];

  if (entry->irg && !entry->is_deleted) {
    ir_graph *const_irg = get_const_code_irg();

    if (entry->irg == const_irg) {
      name = "<Const code Irg>";
      return;
    }
    else {
      if (entry->ent)
        name = get_entity_name(entry->ent);
      else
        name = "<UNKNOWN IRG>";
    }

    csv_count_nodes(entry, cnt);

    fprintf(dmp->f, "%-40s, %p, %d, %d, %d, %d\n",
        name,
        (void *)entry->irg,
        cnt[0].cnt[0],
        cnt[1].cnt[0],
        cnt[2].cnt[0],
        cnt[3].cnt[0]
    );
  }
}

/**
 * initialise the simple dumper
 */
static void csv_init(dumper_t *dmp, const char *name)
{
  char fname[2048];

  snprintf(fname, sizeof(fname), "%s.csv", name);
  dmp->f = fopen(fname, "a");
}

/**
 * finishes the simple dumper
 */
static void csv_finish(dumper_t *dmp)
{
  fclose(dmp->f);
  dmp->f = NULL;
}

/**
 * the simple human readable dumper
 */
static dumper_t csv_dumper = {
  csv_dump_graph,
  csv_init,
  csv_finish,
  NULL,
  NULL,
};


/* ---------------------------------------------------------------------- */

/*
 * helper: get an ir_op from an opcode
 */
ir_op *stat_get_op_from_opcode(opcode code)
{
  return opcode_find_entry(code, status->ir_op_hash);
}

/* initialize the statistics module. */
void init_stat(unsigned enable_options)
{
#define X(a)  a, sizeof(a)-1

  int pseudo_id = 0;

  /* enable statistics */
  status->enable = enable_options & FIRMSTAT_ENABLED;

  if (! status->enable)
   return;

  obstack_init(&status->cnts);

  /* build the pseudo-ops */
  _op_Phi0.code = --pseudo_id;
  _op_Phi0.name = new_id_from_chars(X("Phi0"));

  _op_PhiM.code = --pseudo_id;
  _op_PhiM.name = new_id_from_chars(X("PhiM"));

  _op_MulC.code = --pseudo_id;
  _op_MulC.name = new_id_from_chars(X("MulC"));

  _op_DivC.code = --pseudo_id;
  _op_DivC.name = new_id_from_chars(X("DivC"));

  _op_ModC.code = --pseudo_id;
  _op_ModC.name = new_id_from_chars(X("ModC"));

  _op_DivModC.code = --pseudo_id;
  _op_DivModC.name = new_id_from_chars(X("DivModC"));

  /* create the hash-tables */
  status->irg_hash   = new_pset(graph_cmp, 8);
  status->ir_op_hash = new_pset(opcode_cmp_2, 1);

  status->op_Phi0    = &_op_Phi0;
  status->op_PhiM    = &_op_PhiM;

  if (enable_options & FIRMSTAT_COUNT_STRONG_OP) {
    status->op_MulC    = &_op_MulC;
    status->op_DivC    = &_op_DivC;
    status->op_ModC    = &_op_ModC;
    status->op_DivModC = &_op_DivModC;
  }
  else {
    status->op_MulC    = NULL;
    status->op_DivC    = NULL;
    status->op_ModC    = NULL;
    status->op_DivModC = NULL;
  }

  stat_register_dumper(&simple_dumper);

  if (enable_options & FIRMSTAT_CSV_OUTPUT)
    stat_register_dumper(&csv_dumper);

  /* initialize the pattern hash */
  stat_init_pattern_history(enable_options & FIRMSTAT_PATTERN_ENABLED);
#undef X
}

/* A new IR op is registered. */
void stat_new_ir_op(const ir_op *op)
{
  if (! status->enable)
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

/* An IR op is freed. */
void stat_free_ir_op(const ir_op *op)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
  }
  STAT_LEAVE;
}

/* A new node is created. */
void stat_new_node(ir_node *node)
{
  if (! status->enable)
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

/* A node is changed into a Id node */
void stat_turn_into_id(ir_node *node)
{
  if (! status->enable)
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

/* A new graph was created */
void stat_new_graph(ir_graph *irg, entity *ent)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
    /* execute for side effect :-) */
    graph_entry_t * graph = graph_get_entry(irg, status->irg_hash);

    graph->ent           = ent;
    graph->is_deleted    = 0;
    graph->is_leaf       = 0;
    graph->is_recursive  = 0;
    graph->is_chain_call = 0;
  }
  STAT_LEAVE;
}

/*
 * A graph will be deleted
 */
void stat_free_graph(ir_graph *irg)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph  = graph_get_entry(irg, status->irg_hash);
    graph_entry_t *global = graph_get_entry(NULL, status->irg_hash);

    graph->is_deleted = 1;

    /* count the nodes of the graph yet, it will be destroyed later */
    update_graph_stat(global, graph);

    /* count the DAG's */
//    count_dags_in_graph(global, graph);

    /* calculate the pattern */
    stat_calc_pattern_history(irg);
  }
  STAT_LEAVE;
}

/*
 * A walk over a graph is initiated. Do not count walks from statistic code.
 */
void stat_irg_walk(ir_graph *irg, void *pre, void *post)
{
  if (! status->enable)
    return;

  STAT_ENTER_SINGLE;
  {
    graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);

    cnt_inc(&graph->cnt_walked);
  }
  STAT_LEAVE;
}

/*
 * A walk over a graph in block-wise order is initiated. Do not count walks from statistic code.
 */
void stat_irg_walk_blkwise(ir_graph *irg, void *pre, void *post)
{
  /* for now, do NOT differentiate between blockwise and normal */
  stat_irg_walk(irg, pre, post);
}

/*
 * A walk over the graph's blocks is initiated. Do not count walks from statistic code.
 */
void stat_irg_block_walk(ir_graph *irg, const ir_node *node, void *pre, void *post)
{
  if (! status->enable)
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

/*
 * Some nodes were optimized into some others due to an optimization
 */
void stat_merge_nodes(
    ir_node **new_node_array, int new_num_entries,
    ir_node **old_node_array, int old_num_entries,
    stat_opt_kind opt)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
    int i, j;
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);

    if (status->reassoc_run)
      opt = STAT_OPT_REASSOC;

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

/*
 * reassociation started/stopped.
 */
void stat_reassociate(int flag)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
    status->reassoc_run = flag;
  }
  STAT_LEAVE;
}

/*
 * A node was lowered into other nodes
 */
void stat_lower(ir_node *node)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);

    removed_due_opt(node, graph->opt_hash[STAT_LOWERED]);
  }
  STAT_LEAVE;
}

/*
 * A graph was inlined
 */
void stat_inline(ir_node *call, ir_graph *called_irg)
{
  if (! status->enable)
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

/*
 * A graph with tail-recursions was optimized.
 */
void stat_tail_rec(ir_graph *irg)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
  }
  STAT_LEAVE;
}

/*
 * Strength reduction was performed on an iteration variable.
 */
void stat_strength_red(ir_graph *irg, ir_node *strong, ir_node *cmp)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);
    cnt_inc(&graph->cnt_strength_red);

    removed_due_opt(strong, graph->opt_hash[STAT_OPT_STRENGTH_RED]);
  }
  STAT_LEAVE;
}

/*
 * Start the dead node elimination.
 */
void stat_dead_node_elim_start(ir_graph *irg)
{
  if (! status->enable)
    return;

  ++status->in_dead_node_elim;
}

/*
 * Stops the dead node elimination.
 */
void stat_dead_node_elim_stop(ir_graph *irg)
{
  if (! status->enable)
    return;

  --status->in_dead_node_elim;
}

/*
 * A multiply was replaced by a series of Shifts/Adds/Subs
 */
void stat_arch_dep_replace_mul_with_shifts(ir_node *mul)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);
    removed_due_opt(mul, graph->opt_hash[STAT_OPT_ARCH_DEP]);
  }
  STAT_LEAVE;
}

/**
 * A division was replaced by a series of Shifts/Muls
 */
void stat_arch_dep_replace_div_by_const(ir_node *div)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);
    removed_due_opt(div, graph->opt_hash[STAT_OPT_ARCH_DEP]);
  }
  STAT_LEAVE;
}

/**
 * A modulo was replaced by a series of Shifts/Muls
 */
void stat_arch_dep_replace_mod_by_const(ir_node *mod)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);
    removed_due_opt(mod, graph->opt_hash[STAT_OPT_ARCH_DEP]);
  }
  STAT_LEAVE;
}

/**
 * A DivMod was replaced by a series of Shifts/Muls
 */
void stat_arch_dep_replace_DivMod_by_const(ir_node *divmod)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);
    removed_due_opt(divmod, graph->opt_hash[STAT_OPT_ARCH_DEP]);
  }
  STAT_LEAVE;
}

/* Finish the statistics */
void stat_finish(const char *name)
{
  if (! status->enable)
    return;

  STAT_ENTER;
  {
    graph_entry_t *entry;
    graph_entry_t *global = graph_get_entry(NULL, status->irg_hash);

    dump_init(name);

    /* dump per graph */
    for (entry = pset_first(status->irg_hash); entry; entry = pset_next(status->irg_hash)) {

      if (entry->irg == NULL) {
        /* special entry for the global count */
        continue;
      }

      if (! entry->is_deleted) {
        /* the graph is still alive, count the nodes on it */
        update_graph_stat(global, entry);

        /* count the DAG's */
//        count_dags_in_graph(global, entry);

        /* calculate the pattern */
        stat_calc_pattern_history(entry->irg);
      }

      dump_graph(entry);

      /* clear the counter */
      graph_clear_entry(entry);
    }

    /* dump global */
    dump_graph(global);
    dump_finish();

    stat_finish_pattern_history();

    /* clear the global counter here */
    {
      node_entry_t *entry;

      for (entry = pset_first(global->opcode_hash); entry; entry = pset_next(global->opcode_hash)) {
        opcode_clear_entry(entry);
      }
      graph_clear_entry(global);
    }

    /* finished */
//    status->enable = 0;
  }
  STAT_LEAVE;
}

#else

/* need this for prototypes */
#define FIRM_STATISTICS
#include "firmstat.h"

void init_stat(unsigned enable_options) {}

void stat_finish(const char *name) {}

void stat_new_ir_op(const ir_op *op) {}

void stat_free_ir_op(const ir_op *op) {}

void stat_new_node(ir_node *node) {}

void stat_turn_into_id(ir_node *node) {}

void stat_new_graph(ir_graph *irg, entity *ent) {}

void stat_free_graph(ir_graph *irg) {}

void stat_irg_walk(ir_graph *irg, void *pre, void *post) {}

void stat_irg_block_walk(ir_graph *irg, const ir_node *node, void *pre, void *post) {}

void stat_merge_nodes(
    ir_node **new_node_array, int new_num_entries,
    ir_node **old_node_array, int old_num_entries,
    stat_opt_kind opt) {}

void stat_reassociate(int start) {}

void stat_lower(ir_node *node) {}

void stat_inline(ir_node *call, ir_graph *irg) {}

void stat_tail_rec(ir_graph *irg) {}

void stat_strength_red(ir_graph *irg, ir_node *strong, ir_node *cmp) {}

void stat_dead_node_elim_start(ir_graph *irg) {}

void stat_dead_node_elim_stop(ir_graph *irg) {}

void stat_arch_dep_replace_mul_with_shifts(ir_node *mul) {}

void stat_arch_dep_replace_div_by_const(ir_node *div) {}

void stat_arch_dep_replace_mod_by_const(ir_node *mod) {}

void stat_arch_dep_replace_DivMod_by_const(ir_node *divmod) {}

#endif
