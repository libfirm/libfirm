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
  counter_t   count;			/**< amount of nodes in this entry */
  counter_t   new_node;			/**< amount of new nodes for this entry */
  counter_t   into_Id;			/**< amount of nodes that turned into Id's for this entry */
  const ir_op *op;			/**< the op for this entry */
} node_entry_t;

/*
 * An entry for ir_graphs
 */
typedef struct _graph_entry_t {
  pset           *opcode_hash;			/**< hash map containing the opcode counter */
  counter_t      walked;			/**< walker walked over the graph */
  counter_t      walked_blocks;			/**< walker walked over the graph blocks */
  counter_t      was_inlined;			/**< number of times other graph were inlined */
  counter_t      got_inlined;			/**< number of times this graph was inlined */
  pset           *opt_hash[STAT_OPT_MAX];	/**< hash maps containing opcode counter for optimizations */
  ir_graph       *irg;				/**< the graph of this object */
  entity         *ent;				/**< the entity of this graph if one exists */
  int            deleted;			/**< set if this irg was deleted */
} graph_entry_t;

/**
 * An entry for optimized ir_nodes
 */
typedef struct _opt_entry_t {
  counter_t   count;			/**< optimization counter */
  const ir_op *op;			/**< the op for this entry */
} opt_entry_t;

/**
 * statistics info
 */
typedef struct _statistic_info_t {
  struct obstack cnts;			/**< obstack containing the counters */
  pset           *opcode_hash;		/**< hash map containing the opcode counter */
  pset           *irg_hash;		/**< hash map containing the counter for irgs */
  FILE           *f;			/**< outputfile */
  ir_op          *op_Phi0;		/**< needed pseudo op */
  int            recursive;		/**< flag for detecting recursive hook calls */
  int            in_dead_node_elim;	/**< set, if dead node elimination runs */
} stat_info_t;

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
  int i;

  for (i = 0; i < STAT_CNT_NUM; ++i) {
    unsigned a = dst->cnt[i] + src->cnt[i];
    unsigned no_carry = a >= dst->cnt[i];

    dst->cnt[i] = a;

    if (no_carry) {
      /* no carry */
      break;
    }
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
  cnt_clr(&elem->count);
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

  cnt_clr(&elem->walked);
  cnt_clr(&elem->walked_blocks);
  cnt_clr(&elem->got_inlined);
  cnt_clr(&elem->was_inlined);

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
 * walker for reachable nodes count
 */
static void count_nodes(ir_node *node, void *env)
{
  pset *set = env;
  node_entry_t *entry;
  ir_op *op = get_irn_op(node);

  if (op->code == iro_Phi && get_irn_arity(node) == 0) {
    /* special case, a Phi0 node, count on extra counter */
    op = status->op_Phi0;
  }

  entry = opcode_get_entry(op, set);

  cnt_inc(&entry->count);
}

/**
 * count all reachable nodes in a graph
 */
static void count_nodes_in_graph(ir_graph *irg, pset *set)
{
  irg_walk_graph(irg, count_nodes, NULL, set);
}

/* ---------------------------------------------------------------------- */

/* initialize the statistics module. */
void stat_init(void)
{
  obstack_init(&status->cnts);

  /* create the hash-tables */
  status->opcode_hash = new_pset(opcode_cmp, 8);
  status->irg_hash    = new_pset(graph_cmp, 8);

  status->f           = fopen("firmstat.txt", "w");
  status->op_Phi0     = NULL;
}

/* A new IR op is registered. */
void stat_new_ir_op(const ir_op *op)
{
  STAT_ENTER;
  {
    /* execute for side effect :-) */
    opcode_get_entry(op, status->opcode_hash);
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
    ir_op *op = get_irn_op(node);

    if (op->code == iro_Phi && get_irn_arity(node) == 0) {
      /* special case, a Phi0 node, count on extra counter */
      if (! status->op_Phi0) {
	ir_op *op_Phi = get_op_Phi();

	status->op_Phi0 = new_ir_op(0xFFFF, "Phi0", op_Phi->pinned, op_Phi->flags, op_Phi->opar, op_Phi->op_index, op_Phi->attr_size);
      }
      op = status->op_Phi0;
    }

    entry = opcode_get_entry(op, status->opcode_hash);
    graph = graph_get_entry(current_ir_graph, status->irg_hash);

    /* increase global value */
    cnt_inc(&entry->new_node);

    /* increase local value */
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
    ir_op *op = get_irn_op(node);

    if (op->code == iro_Phi && get_irn_arity(node) == 0) {
      /* special case, a Phi0 node, count on extra counter */
      if (! status->op_Phi0) {
	ir_op *op_Phi = get_op_Phi();

	status->op_Phi0 = new_ir_op(0xFFFF, "Phi0", op_Phi->pinned, op_Phi->flags, op_Phi->opar, op_Phi->op_index, op_Phi->attr_size);
      }
      op = status->op_Phi0;
    }

    entry = opcode_get_entry(op, status->opcode_hash);
    graph = graph_get_entry(current_ir_graph, status->irg_hash);

    /* increase global value */
    cnt_inc(&entry->into_Id);

    /* increase local value */
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
    graph_entry_t * graph = graph_get_entry(irg, status->irg_hash);

    graph->deleted = 1;

    /* count the nodes of the graph yet, it will be destroyed later */
    count_nodes_in_graph(irg, graph->opcode_hash);
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

    cnt_inc(&graph->walked);
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

    cnt_inc(&graph->walked_blocks);
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

    cnt_inc(&graph->got_inlined);
    cnt_inc(&i_graph->was_inlined);
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

/**
 * dumps a opcode hash
 */
static void dump_opcode_hash(pset *set)
{
  node_entry_t *entry;
  counter_t f_count;
  counter_t f_new_node;
  counter_t f_Id;

  cnt_clr(&f_count);
  cnt_clr(&f_new_node);
  cnt_clr(&f_Id);

  fprintf(status->f, "%-16s %-8s %-8s %-8s\n", "Opcode", "alive", "created", "->Id");
  for (entry = pset_first(set); entry; entry = pset_next(set)) {
    fprintf(status->f, "%-16s %8d %8d %8d\n",
	get_id_str(entry->op->name), entry->count.cnt[0], entry->new_node.cnt[0], entry->into_Id.cnt[0]);

    cnt_add(&f_count,    &entry->count);
    cnt_add(&f_new_node, &entry->new_node);
    cnt_add(&f_Id,       &entry->into_Id);
  }
  fprintf(status->f, "-------------------------------------------\n");
  fprintf(status->f, "%-16s %8d %8d %8d\n", "Sum",
     f_count.cnt[0],
     f_new_node.cnt[0],
     f_Id.cnt[0]);
}

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
 * dumps a optimization hash
 */
static void dump_opt_hash(pset *set, int index)
{
  opt_entry_t *entry = pset_first(set);

  if (entry) {
    fprintf(status->f, "\n%s:\n", opt_names[index]);
    fprintf(status->f, "%-16s %-8s\n", "Opcode", "deref");

    for (; entry; entry = pset_next(set)) {
      fprintf(status->f, "%-16s %8d\n",
	  get_id_str(entry->op->name), entry->count.cnt[0]);
    }
  }
}

/* Finish the statistics */
void stat_finish(void)
{
  STAT_ENTER;
  {
    int i;
    graph_entry_t *entry;
    ir_graph *const_irg = get_const_code_irg();

    /* dump global */
    fprintf(status->f, "\nGlobal counts:\n");
    dump_opcode_hash(status->opcode_hash);

    for (entry = pset_first(status->irg_hash); entry; entry = pset_next(status->irg_hash)) {
      entity *ent = entry->ent;

      if (! entry->deleted) {
	/* the graph is still alive, count the nodes on it */
	count_nodes_in_graph(entry->irg, entry->opcode_hash);
      }

      if (entry->irg == const_irg) {
	fprintf(status->f, "\nConst code Irg %p", entry->irg);
      }
      else {
	if (ent)
	  fprintf(status->f, "\nEntity %s, Irg %p", get_entity_name(ent), entry->irg);
	else
	  fprintf(status->f, "\nIrg %p", entry->irg);
      }

      fprintf(status->f, " %swalked %d over blocks %d was inlined %d got inlined %d:\n",
	  entry->deleted ? "DELETED " : "",
	  entry->walked.cnt[0], entry->walked_blocks.cnt[0],
	  entry->was_inlined.cnt[0],
	  entry->got_inlined.cnt[0]
	  );

      dump_opcode_hash(entry->opcode_hash);

      for (i = 0; i < sizeof(entry->opt_hash)/sizeof(entry->opt_hash[0]); ++i) {
	dump_opt_hash(entry->opt_hash[i], i);
      }
    }

    fclose(status->f);
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

void stat_new_graph(const ir_graph *irg, entity *ent) {}

void stat_free_graph(const ir_graph *irg) {}

void stat_irg_walk(const ir_graph *irg, void *pre, void *post) {}

void stat_irg_block_walk(const ir_graph *irg, const ir_node *node, void *pre, void *post) {}

void stat_merge_nodes(
    ir_node **new_node_array, int new_num_entries,
    ir_node **old_node_array, int old_num_entries,
    stat_opt_kind opt) {}

void stat_lower(ir_node *node) {}

void stat_inline(const ir_node *call, const ir_graph *irg) {}

void stat_dead_node_elim_start(ir_graph *irg) {}

void stat_dead_node_elim_stop(ir_graph *irg) {}

#endif
