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

# include <string.h>

# include "irop_t.h"
# include "irnode_t.h"
# include "irgraph_t.h"
# include "pset.h"
# include "irprog.h"
# include "irgwalk.h"

#include <stdio.h>
#include <stdlib.h>

#undef obstack_chunk_alloc
#undef obstack_chunk_free
#define obstack_chunk_alloc	malloc
#define obstack_chunk_free	free
#include <obstack.h>

#ifdef FIRM_STATISTICS

#include "firmstat.h"

/*
 * just be make some things clear :-), the
 * poor man "generics"
 */
#define HASH_MAP(type)	pset_##type

typedef pset pset_node_entry_t;
typedef pset pset_graph_entry_t;
typedef pset pset_opt_entry_t;

/*
 * 32 bit should be enough for now
 */
#define STAT_CNT_NUM 1

typedef struct _counter_t {
  unsigned cnt[STAT_CNT_NUM];
} counter_t;

/*
 * An entry for ir_nodes
 */
typedef struct _node_entry_t {
  counter_t   cnt_alive;		/**< amount of nodes in this entry */
  counter_t   new_node;			/**< amount of new nodes for this entry */
  counter_t   into_Id;			/**< amount of nodes that turned into Id's for this entry */
  const ir_op *op;			/**< the op for this entry */
} node_entry_t;

/*
 * An entry for ir_graphs
 */
typedef struct _graph_entry_t {
  HASH_MAP(node_entry_t)  *opcode_hash;			/**< hash map containing the opcode counter */
  counter_t               cnt_walked;			/**< walker walked over the graph */
  counter_t               cnt_walked_blocks;		/**< walker walked over the graph blocks */
  counter_t               cnt_was_inlined;		/**< number of times other graph were inlined */
  counter_t               cnt_got_inlined;		/**< number of times this graph was inlined */
  counter_t               cnt_edges;			/**< number of DF edges in this graph */
  HASH_MAP(opt_entry_t)   *opt_hash[STAT_OPT_MAX];	/**< hash maps containing opcode counter for optimizations */
  ir_graph                *irg;				/**< the graph of this object */
  entity                  *ent;				/**< the entity of this graph if one exists */
  int                     deleted;			/**< set if this irg was deleted */
} graph_entry_t;

/**
 * An entry for optimized ir_nodes
 */
typedef struct _opt_entry_t {
  counter_t   count;			/**< optimization counter */
  const ir_op *op;			/**< the op for this entry */
} opt_entry_t;

/** forward */
typedef struct _dumper_t dumper_t;

/**
 * handler for dumping an IRG
 *
 * @param dmp   the dumper
 * @param entry the IR-graph hash map entry
 */
typedef void (*dump_graph_FUNC)(dumper_t *dmp, graph_entry_t *entry);

/**
 * handler for dumper init
 *
 * @param dmp   the dumper
 * @param name  name of the file to dump to
 */
typedef void (*dump_init_FUNC)(dumper_t *dmp, const char *name);

/**
 * handler for dumper finish
 *
 * @param dmp   the dumper
 */
typedef void (*dump_finish_FUNC)(dumper_t *dmp);


/**
 * a dumper description
 */
struct _dumper_t {
  dump_graph_FUNC         dump_graph;		/**< handler for dumping an irg */
  dump_init_FUNC          init;			/**< handler for init */
  dump_finish_FUNC        finish;		/**< handler for finish */
  FILE                    *f;			/**< the file to dump to */
  dumper_t                *next;		/**< link to the next dumper */
};

/**
 * statistics info
 */
typedef struct _statistic_info_t {
  struct obstack          cnts;			/**< obstack containing the counters */
  HASH_MAP(graph_entry_t) *irg_hash;		/**< hash map containing the counter for irgs */
  int                     recursive;		/**< flag for detecting recursive hook calls */
  int                     in_dead_node_elim;	/**< set, if dead node elimination runs */
  ir_op                   *op_Phi0;		/**< needed pseudo op */
  ir_op                   *op_PhiM;		/**< needed pseudo op */
  dumper_t                *dumper;		/**< list of dumper */
} stat_info_t;

/**
 * names of the optimizations
 */
static const char *opt_names[] = {
  "straightening optimization",
  "if simplification",
  "algebraic simplification",
  "Phi optmization",
  "Write-After-Write optimization",
  "Write-After-Read optimization",
  "Tuple optimization",
  "ID optimization",
  "Constant evaluation",
  "Lowered",
};

/**
 * need this to be static
 */
static ir_op _op_Phi0, _op_PhiM;

/* ---------------------------------------------------------------------------------- */

#define STAT_ENTER		++status->recursive
#define STAT_LEAVE		--status->recursive
#define STAT_ENTER_SINGLE	do { if (status->recursive > 0) return; ++status->recursive; } while (0)

/*
 * global status
 */
static stat_info_t _status, *status = &_status;

/**
 * increase a counter
 */
static INLINE void cnt_inc(counter_t *cnt)
{
  int i;

  for (i = 0; i < STAT_CNT_NUM; ++i) {
    if (++cnt->cnt[i])
      break;
  }
}

/**
 * decreace a counter
 */
static INLINE void cnt_dec(counter_t *cnt)
{
  int i;

  for (i = 0; i < STAT_CNT_NUM; ++i) {
    if (--cnt->cnt[i] != -1)
      break;
  }
}

/**
 * set a counter to zero
 */
static INLINE void cnt_clr(counter_t *cnt)
{
  memset(cnt->cnt, 0, sizeof(cnt->cnt));
}

/**
 * add a counter to another
 */
static inline void cnt_add(counter_t *dst, const counter_t *src)
{
  int i, carry = 0;

  for (i = 0; i < STAT_CNT_NUM; ++i) {
    unsigned a = dst->cnt[i] + src->cnt[i] + carry;

    if (carry)
      carry = a <= dst->cnt[i];
    else
      carry = a < dst->cnt[i];

    dst->cnt[i] = a;

    if (! carry)
      break;
  }
}

/**
 * add an integer to an counter
 */
static inline void cnt_add_i(counter_t *dst, int src)
{
  int i;
  unsigned a = dst->cnt[0] + src;
  unsigned carry = a < dst->cnt[i];

  dst->cnt[0] = a;
  if (! carry)
    return;

  for (i = 1; i < STAT_CNT_NUM; ++i) {
    unsigned a = dst->cnt[i] + carry;

    carry = a < dst->cnt[i];

    dst->cnt[i] = a;

    if (! carry)
      break;
  }
}

/*
 * compare two elements of the opcode hash
 */
static int opcode_cmp(const void *elt, const void *key)
{
  const node_entry_t *e1 = elt;
  const node_entry_t *e2 = key;

  return e1->op->code - e2->op->code;
}

/*
 * compare two elements of the graph hash
 */
static int graph_cmp(const void *elt, const void *key)
{
  const graph_entry_t *e1 = elt;
  const graph_entry_t *e2 = key;

  return e1->irg != e2->irg;
}

/*
 * compare two elements of the optimization hash
 */
static int opt_cmp(const void *elt, const void *key)
{
  const opt_entry_t *e1 = elt;
  const opt_entry_t *e2 = key;

  return e1->op->code != e2->op->code;
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
  cnt_clr(&elem->cnt_alive);
  cnt_clr(&elem->new_node);
  cnt_clr(&elem->into_Id);

  elem->op = op;

  return pset_insert(set, elem, op->code);
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

  elem = obstack_alloc(&status->cnts, sizeof(*elem));

  cnt_clr(&elem->cnt_walked);
  cnt_clr(&elem->cnt_walked_blocks);
  cnt_clr(&elem->cnt_got_inlined);
  cnt_clr(&elem->cnt_was_inlined);
  cnt_clr(&elem->cnt_edges);

  /* new hash table for opcodes here  */
  elem->opcode_hash  = new_pset(opcode_cmp, 5);
  elem->irg          = irg;

  for (i = 0; i < sizeof(elem->opt_hash)/sizeof(elem->opt_hash[0]); ++i)
    elem->opt_hash[i] = new_pset(opt_cmp, 4);

  return pset_insert(set, elem, irg_hash(irg));
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
  cnt_clr(&elem->count);

  elem->op = op;

  return pset_insert(set, elem, op->code);
}

/**
 * Returns the ir_op for an IR-node,
 * handles special cases and return pseudo op codes
 */
static ir_op *stat_get_irn_op(const ir_node *node)
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
  return op;
}


/**
 * environment for the count walker
 */
typedef struct _cnt_env_t {
  pset      *set;		/**< the hash map containing the ir_ops */
  counter_t *cnt_edges;		/**< the edges counter */
} cnt_env_t;

/**
 * walker for reachable nodes count
 */
static void count_nodes(ir_node *node, void *env)
{
  cnt_env_t *cenv = env;
  node_entry_t *entry;
  ir_op *op = stat_get_irn_op(node);
  int arity = get_irn_arity(node);

  entry = opcode_get_entry(op, cenv->set);

  cnt_inc(&entry->cnt_alive);
  cnt_add_i(cenv->cnt_edges, arity);
}

/**
 * count all alive nodes in a graph
 */
static void count_nodes_in_graph(graph_entry_t * global, graph_entry_t * graph)
{
  cnt_env_t env;
  node_entry_t *entry;

  env.set       = graph->opcode_hash;
  env.cnt_edges = &graph->cnt_edges;

  irg_walk_graph(graph->irg, count_nodes, NULL, &env);

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
static void stat_register_dumper(dumper_t *dumper, const char *name)
{
  dumper->next   = status->dumper;
  status->dumper = dumper;

  if (dumper->init)
    dumper->init(dumper, name);
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

    fprintf(dmp->f, " %swalked %d over blocks %d was inlined %d got inlined %d:\n",
	entry->deleted ? "DELETED " : "",
	entry->cnt_walked.cnt[0], entry->cnt_walked_blocks.cnt[0],
	entry->cnt_was_inlined.cnt[0],
	entry->cnt_got_inlined.cnt[0]
    );
  }
  else {
    fprintf(dmp->f, "\nGlobals counts:\n");
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
  }
}

/**
 * initialise the simple dumper
 */
static void simple_init(dumper_t *dmp, const char *name)
{
  dmp->f = fopen(name, "w");
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

  if (entry->irg) {
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
	entry->irg,
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
  dmp->f = fopen(name, "a");
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

/* initialize the statistics module. */
void stat_init(void)
{
#define X(a)	a, sizeof(a)-1

  int pseudo_id = 0;

  obstack_init(&status->cnts);

  /* build the pseudo-ops */
  _op_Phi0.code = --pseudo_id;
  _op_Phi0.name = id_from_str(X("Phi0"));

  _op_PhiM.code = --pseudo_id;
  _op_PhiM.name = id_from_str(X("PhiM"));

  /* create the hash-tables */
  status->irg_hash   = new_pset(graph_cmp, 8);

  status->op_Phi0    = &_op_Phi0;
  status->op_PhiM    = &_op_PhiM;

  stat_register_dumper(&simple_dumper, "firmstat.txt");
  stat_register_dumper(&csv_dumper, "firmstat.csv");

#undef X
}

/* A new IR op is registered. */
void stat_new_ir_op(const ir_op *op)
{
  STAT_ENTER;
  {
    graph_entry_t *graph = graph_get_entry(NULL, status->irg_hash);

    /* execute for side effect :-) */
    opcode_get_entry(op, graph->opcode_hash);
  }
  STAT_LEAVE;
}

/* An IR op is freed. */
void stat_free_ir_op(const ir_op *op)
{
  STAT_ENTER;
  {
  }
  STAT_LEAVE;
}

/* A new node is created. */
void stat_new_node(const ir_node *node)
{
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
void stat_turn_into_id(const ir_node *node)
{
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
  STAT_ENTER;
  {
    /* execute for side effect :-) */
    graph_entry_t * graph = graph_get_entry(irg, status->irg_hash);

    graph->ent     = ent;
    graph->deleted = 0;
  }
  STAT_LEAVE;
}

/*
 * A graph was deleted
 */
void stat_free_graph(ir_graph *irg)
{
  STAT_ENTER;
  {
    graph_entry_t *graph  = graph_get_entry(irg, status->irg_hash);
    graph_entry_t *global = graph_get_entry(NULL, status->irg_hash);

    graph->deleted = 1;

    /* count the nodes of the graph yet, it will be destroyed later */
    count_nodes_in_graph(global, graph);
  }
  STAT_LEAVE;
}

/*
 * A walk over a graph is initiated. Do not count walks from statistic code.
 */
void stat_irg_walk(ir_graph *irg, void *pre, void *post)
{
  STAT_ENTER_SINGLE;
  {
    graph_entry_t *graph = graph_get_entry(irg, status->irg_hash);

    cnt_inc(&graph->cnt_walked);
  }
  STAT_LEAVE;
}

/*
 * A walk over the graph's blocks is initiated. Do not count walks from statistic code.
 */
void stat_irg_block_walk(ir_graph *irg, const ir_node *node, void *pre, void *post)
{
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
  ir_op *op          = get_irn_op(n);
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
  STAT_ENTER;
  {
    int i, j;
    graph_entry_t *graph = graph_get_entry(current_ir_graph, status->irg_hash);

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
 * A node was lowered into other nodes
 */
void stat_lower(ir_node *node)
{
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
 * Start the dead node elimination.
 */
void stat_dead_node_elim_start(ir_graph *irg)
{
  ++status->in_dead_node_elim;
}

/*
 * Stops the dead node elimination.
 */
void stat_dead_node_elim_stop(ir_graph *irg)
{
  --status->in_dead_node_elim;
}

/* Finish the statistics */
void stat_finish(void)
{
  STAT_ENTER;
  {
    graph_entry_t *entry;
    graph_entry_t *global = graph_get_entry(NULL, status->irg_hash);

    /* dump per graph */
    for (entry = pset_first(status->irg_hash); entry; entry = pset_next(status->irg_hash)) {

      if (entry->irg == NULL) {
	/* special entry for the global count */
	continue;
      }

      if (! entry->deleted) {
	/* the graph is still alive, count the nodes on it */
	count_nodes_in_graph(global, entry);
      }

      dump_graph(entry);
    }

    /* dump global */
    dump_graph(global);
    dump_finish();

  }
  STAT_LEAVE;
}

#else

/* need this for prototypes */
#define FIRM_STATISTICS
#include "firmstat.h"

void stat_init(void) {}

void stat_finish(void) {}

void stat_new_ir_op(const ir_op *op) {}

void stat_free_ir_op(const ir_op *op) {}

void stat_new_node(const ir_node *node) {}

void stat_turn_into_id(const ir_node *node) {}

void stat_new_graph(ir_graph *irg, entity *ent) {}

void stat_free_graph(ir_graph *irg) {}

void stat_irg_walk(ir_graph *irg, void *pre, void *post) {}

void stat_irg_block_walk(ir_graph *irg, const ir_node *node, void *pre, void *post) {}

void stat_merge_nodes(
    ir_node **new_node_array, int new_num_entries,
    ir_node **old_node_array, int old_num_entries,
    stat_opt_kind opt) {}

void stat_lower(ir_node *node) {}

void stat_inline(ir_node *call, ir_graph *irg) {}

void stat_dead_node_elim_start(ir_graph *irg) {}

void stat_dead_node_elim_stop(ir_graph *irg) {}

#endif
