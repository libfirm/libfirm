#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "interval_analysis.h"

#include "firm_common_t.h"
#include "set.h"
#include "array.h"

#include "irloop.h"
#include "irnode.h"
#include "irdump_t.h"
#include "irdom.h"
#include "irflag.h"

/*------------------------------------------------------------------*/
/* A new in array via a hashmap. */
/* The in array refers to the loop the block is contained in if the */
/* block is not in blocks loop. */
/*------------------------------------------------------------------*/

typedef struct {
  void *reg;
  void **in_array;
  void **op_array;
  int n_outs;
  int n_exc_outs;
} region_attr;

static set *region_attr_set = NULL;

int region_attr_cmp(const void *e1, const void *e2, size_t size) {
  region_attr *ra1 = (region_attr *)e1;
  region_attr *ra2 = (region_attr *)e2;
  return (ra1->reg != ra2->reg);
}

static INLINE int attr_set_hash (region_attr *a) {
  unsigned int v = (unsigned int) a->reg;
  return v ^ (v>>8);
}

static INLINE region_attr *get_region_attr(void *region) {
  region_attr r_attr, *res;
  r_attr.reg = region;

  res = set_find(region_attr_set, &r_attr, sizeof(r_attr), attr_set_hash(&r_attr));

  if (!res) {
    r_attr.in_array = NEW_ARR_F(void *, 0);
    if (is_ir_loop(region))
      r_attr.op_array = NEW_ARR_F(void *, 0);
    else
      r_attr.op_array = NULL;
    r_attr.n_outs = 0;
    r_attr.n_exc_outs = 0;
    res = set_insert(region_attr_set, &r_attr, sizeof(r_attr), attr_set_hash(&r_attr));
  }

  return res;
}

int get_region_n_ins(void *region) {
  return ARR_LEN(get_region_attr(region)->in_array);
}

void *get_region_in(void *region, int pos) {
  assert(0 <= pos && pos < get_region_n_ins(region));
  return (get_region_attr(region)->in_array)[pos];
}

void add_region_in (void *region, void *in) {
  ARR_APP1(void *, get_region_attr(region)->in_array, in);
  get_region_attr(in)->n_outs++;
}

int get_region_n_outs(void *region) {
  return get_region_attr(region)->n_outs;
}

int get_region_n_exc_outs(void *region) {
  return get_region_attr(region)->n_exc_outs;
}

void inc_region_n_exc_outs(void *region) {
  (get_region_attr(region)->n_exc_outs)++;
}

void *get_loop_cfop(void *region, int pos) {
  assert(0 <= pos && pos < get_region_n_ins(region));
  return (get_region_attr(region)->op_array)[pos];
}

void add_loop_cfop (void *region, void *cfop) {
  assert(cfop);
  ARR_APP1(void *, get_region_attr(region)->op_array, cfop);
}

static INLINE void exc_outs(void *reg, ir_node *cfop) {
  if (is_fragile_op(cfop)) inc_region_n_exc_outs(reg);
}

/*------------------------------------------------------------------*/
/* Algorithm to construct the interval edges based on a loop tree. */
/* Walk a loop and add all edges.  Walk inner loops by recursion. */
/*------------------------------------------------------------------*/

/* return true if outer can be reached from inner via the outer loop relation */
static int find_outer_loop(ir_loop *inner, ir_loop *outer, ir_node *b, ir_node *cfop) {
  if (get_loop_outer_loop(inner) == outer) {
    add_region_in(inner, b);
    add_loop_cfop(inner, cfop);
    exc_outs(b, cfop);
    return true;
  }
  return false;
}

static int test_loop_nest(ir_node *pred_b, ir_loop *nest) {
  int i, n_elems = get_loop_n_elements(nest);

  for (i = 0; (i < n_elems); ++i) {
    loop_element e = get_loop_element(nest, i);
    switch (*e.kind) {
    case k_ir_node: {
      if (e.node == pred_b) return true;
    } break;
    case k_ir_loop: {
      if (test_loop_nest(pred_b, e.son)) return true;
    } break;
    default: break;
    }
  }
  return false;
}

static int find_inner_loop(ir_node *b, ir_loop *l, ir_node *pred, ir_node *cfop) {
  int i, n_elems = get_loop_n_elements(l);
  int found = false;

  for (i = 0; (i < n_elems) && !found; ++i) {
    loop_element e = get_loop_element(l, i);
    switch (*e.kind) {
    case k_ir_node: {
      if (e.node == b) return false;
    } break;
    case k_ir_loop: {
      found = test_loop_nest(pred, e.son);
      if (found) {
	add_region_in(b, e.son);
	exc_outs(e.son, cfop);
	//if (is_fragile_op(cfop)) inc_region_n_exc_outs(b);
	return found;
      }
    } break;
    default: break;
    }
  }
  return found;
}


static int find_previous_loop(ir_loop *l, ir_loop *pred_l, ir_node *b, ir_node *pred_b, ir_node *cfop) {
  ir_loop *outer = get_loop_outer_loop(l);
  int found, i;
  int l_pos = get_loop_element_pos(outer, l);
  assert(l_pos > -1);
  assert(l_pos > 0 && "Is this a necessary condition?  There could be a perfect nest ...");

  for (i = l_pos -1, found = false; i > -1 && !found; --i) {
    ir_loop *k = get_loop_element(outer, i).son;
    if (is_ir_loop(k)) {
      found = test_loop_nest(pred_b, k);
      if (found) {
	add_region_in(l, k);
	//if (is_fragile_op(cfop)) inc_region_n_exc_outs(k);
	exc_outs(k, cfop);
	add_loop_cfop(l, cfop);
	add_region_in(b, NULL);
      }
    }
  }

  if (!found) {
    DDMG(current_ir_graph);
    DDML(l);
    DDML(pred_l);
    DDMN(b);
    DDMN(pred_b);
  }

  return found;
}


/* Compute the edges for the interval graph.
 *
 * @param b The block for which to constuct the edges.
 * @param l The loop of b.
 *
 * There are four cases:
 * - The pred block is in the same loop.  Add a normal block-block edge.
 * - The pred block is in a loop contained in this loop, somewhere down in
 *   the nesting. The predecessor of this block is the outermost loop of the nest
 *   directly contained in l.
 * - The pred block is in the outer loop of l.  l gets an edge to the pred block.
 * - The outer loop of l contains another loop k just before l.  The control flow
 *   branches directly from loop k to loop l.  Add an edge l->k.  Watch it: k must
 *   not be a direct predecessor of l in the loop tree!
 */
static void construct_interval_block(ir_node *b, ir_loop *l) {
  int i, n_cfgpreds = get_Block_n_cfgpreds(b);

  if (b == get_irg_start_block(current_ir_graph)) return;
  /* We want nice blocks. */
  assert(n_cfgpreds > 0);

  for (i = 0; i < n_cfgpreds; ++i) {
    ir_node *cfop, *pred;
    ir_loop *pred_l;

    if (is_backedge(b, i)) {
      if (b != get_loop_element(l, 0).node) {
	    if (get_firm_verbosity()) {
	      printf("Loophead not at loop position 0. "); DDMN(b);
	    }
      }
      /* There are no backedges in the interval decomposition. */
      add_region_in(b, NULL);
      continue;
    }

    cfop = skip_Proj(get_Block_cfgpred(b, i));
    pred = get_nodes_block(cfop);
    /* We want nice blocks. */
    assert(   get_irn_op(pred) != op_Bad
           && get_irn_op(skip_Proj(get_Block_cfgpred(b, i))) != op_Bad);
    pred_l = get_irn_loop(pred);
    if (pred_l == l) {
      add_region_in(b, pred);
      //if (is_fragile_op(cfop)) inc_region_n_exc_outs(b);
      exc_outs(pred, cfop);
    } else {
      int found = find_inner_loop(b, l, pred, cfop);
      if (!found) {
	    if (b != get_loop_element(l, 0).node) {
	      if (get_firm_verbosity()) {
	        printf("Loop entry not at loop position 0. "); DDMN(b);
	      }
	    }
	    found = find_outer_loop(l, pred_l, pred, cfop);
	    if (found) add_region_in(b, NULL);  /* placeholder */
      }
      if (!found) {
        found = find_previous_loop(l, pred_l, b, pred, cfop);
      }
      if (!found) {
	    DDMG(current_ir_graph);
	    DDMN(b);
	    DDMN(pred);
	    assert(is_backedge(b, i));
	    assert(found && "backedge from inner loop");
      }
    }

    if (b != get_loop_element(l, 0).node) {
      /* Check for improper region */
      if (has_backedges(b)) {
	    printf("Improper Region!!!!!!\n");
	    DDMG(current_ir_graph);
	    DDMN(b);
	    DDML(l);
      }
    }
  }
}

static void construct_interval_edges(ir_loop *l) {
  int i, n_elems = get_loop_n_elements(l);
  for (i = 0; i < n_elems; ++i) {
    loop_element e = get_loop_element(l, i);
    switch (*e.kind) {
    case k_ir_node: {
      construct_interval_block(e.node, l);
    } break;
    case k_ir_loop: {
      construct_interval_edges(e.son);
    } break;
    default: break;
    }
  }
}

void construct_intervals(ir_graph *irg) {
  ir_loop *l;
  ir_graph *rem = current_ir_graph;
  current_ir_graph = irg;

  if (!region_attr_set)
    region_attr_set = new_set(region_attr_cmp, 256);

  construct_cf_backedges(current_ir_graph);

  l = get_irg_loop(current_ir_graph);

  construct_interval_edges(l);

  current_ir_graph = rem;
}

void free_intervals(void) {
  //void **ins;
  if (!region_attr_set) return;
  /* @@@ mem leak
  for (ins = (void **)pmap_first(region_in_map);
       ins;
       ins = (void **)pmap_next(region_in_map)) {
    //DEL_ARR_F(ins);
  }
  */
  del_set(region_attr_set);
  region_attr_set = NULL;
}

/*------------------------------------------------------------------*/
/* A vcg dumper showing an interval decomposition of a cfg.         */
/*                                                                  */
/*------------------------------------------------------------------*/

void dump_region_edges(FILE *F, void *reg) {
  int i, n_ins = get_region_n_ins(reg);

  if (is_ir_node(reg) && get_Block_n_cfgpreds((ir_node *)reg) > get_region_n_ins(reg)) {
    for (i = n_ins; i < get_Block_n_cfgpreds((ir_node *)reg); ++i) {
      if (is_backedge((ir_node *)reg, i))
	fprintf (F, "backedge: { sourcename: \"");
      else
	fprintf (F, "edge: { sourcename: \"");
      PRINT_NODEID(((ir_node *)reg));
      fprintf (F, "\" targetname: \"");
      PRINT_NODEID(get_nodes_block(skip_Proj(get_Block_cfgpred((ir_node *)reg, i))));
      fprintf (F, "\" " BLOCK_EDGE_ATTR "}\n");
    }
  }

  for (i = 0; i < n_ins; ++i) {
    void *target = get_region_in(reg, i);

    if (is_ir_node(reg)) {
      if (get_Block_n_cfgpreds((ir_node *)reg) != get_region_n_ins(reg)) {
	printf("n_cfgpreds = %d, n_ins = %d\n", get_Block_n_cfgpreds((ir_node *)reg), get_region_n_ins(reg));
	DDMN((ir_node *)reg);
      }
    }

    if ((!target || (is_ir_node(reg) && !is_ir_node(target))) && i < get_Block_n_cfgpreds((ir_node *)reg)) {
      assert(is_ir_node(reg));
      if (is_backedge((ir_node *)reg, i))
	fprintf (F, "backedge: { sourcename: \"");
      else
	fprintf (F, "edge: { sourcename: \"");
      PRINT_NODEID(((ir_node *)reg));
      fprintf (F, "\" targetname: \"");
      PRINT_NODEID(get_nodes_block(skip_Proj(get_Block_cfgpred((ir_node *)reg, i))));
      fprintf (F, "\" " BLOCK_EDGE_ATTR "}\n");

      if (!target) continue;
    }

    fprintf (F, "edge: { sourcename: \"");
    if (is_ir_node(reg)) {
      PRINT_NODEID(((ir_node *)reg));
    } else {
      PRINT_LOOPID(((ir_loop *)reg));
    }
    fprintf (F, "\" targetname: \"");
    if (is_ir_node(target)) {
      PRINT_NODEID(((ir_node *)target));
    } else {
      PRINT_LOOPID(((ir_loop *)target));
    }
    fprintf (F, "\"");
    if (is_ir_node(reg) && is_fragile_op(skip_Proj(get_Block_cfgpred(reg, i))))
      fprintf(F, EXC_CF_EDGE_ATTR);
    fprintf (F, "}\n");
  }
}

#include "execution_frequency.h"

void dump_interval_block(FILE *F, ir_node *block) {
  int i, fl;
  /* This is a block. Dump a node for the block. */
  fprintf (F, "node: {title: \""); PRINT_NODEID(block);
  fprintf (F, "\" label: \"");
  if (block == get_irg_start_block(get_irn_irg(block)))
    fprintf(F, "Start ");
  if (block == get_irg_end_block(get_irn_irg(block)))
    fprintf(F, "End ");

  fprintf (F, "%s ", get_op_name(get_irn_op(block)));
  PRINT_NODEID(block);
  fprintf(F, " freq: %9.4lf", get_region_exec_freq(block));
  fprintf(F, " n_outs: %d", get_region_n_outs(block));
  fprintf(F, " n_exc_outs: %d", get_region_n_exc_outs(block));
  fprintf (F, "\" ");
  fprintf(F, "info1:\"");
  if (dump_dominator_information_flag)
    fprintf(F, "dom depth %d\n", get_Block_dom_depth(block));

  /* show arity and possible Bad predecessors of the block */
  fprintf(F, "arity: %d\n", get_Block_n_cfgpreds(block));
  for (fl = i = 0; i < get_Block_n_cfgpreds(block); ++i) {
    ir_node *pred = get_Block_cfgpred(block, i);
    if (is_Bad(pred)) {
      if (! fl)
	fprintf(F, "Bad pred at pos: ");
      fprintf(F, "%d ", i);
      fl = 1;
    }
  }
  if (fl)
    fprintf(F, "\n");

  fprintf (F, "\"");  /* closing quote of info */

  if ((block == get_irg_start_block(get_irn_irg(block))) ||
      (block == get_irg_end_block(get_irn_irg(block)))     )
    fprintf(F, " color:blue ");
  else if (fl)
    fprintf(F, " color:yellow ");

  fprintf (F, "}\n");
}

void dump_interval_loop(FILE *F, ir_loop *l) {
  int i, n_elems = get_loop_n_elements(l);

  fprintf(F, "graph: { title: \"");
  PRINT_LOOPID(l);
  fprintf(F, "\" label: \"loop %d", get_loop_loop_nr(l));
  fprintf(F, " freq: %9.4lf", get_region_exec_freq(l));
  fprintf(F, " n_outs: %d", get_region_n_outs(l));
  fprintf(F, " n_exc_outs: %d", get_region_n_exc_outs(l));
  fprintf(F, "\" status:clustered color:white \n");

  for (i = 0; i < n_elems; ++i) {
    loop_element e = get_loop_element(l, i);
    dump_region_edges(F, e.node);
    switch (*e.kind) {
    case k_ir_node: {
      dump_interval_block(F, e.node);
    } break;
    case k_ir_loop: {
      dump_interval_loop(F, e.son);
    } break;
    default: break;
    }
  }

  fprintf(F, "}\n\n");
}


void dump_interval_graph(ir_graph *irg, const char *suffix) {
  FILE *f;

  if (strncmp(get_entity_name(get_irg_entity(irg)), dump_file_filter, strlen(dump_file_filter)) != 0)
    return;

  f = vcg_open(irg, suffix, "-intervals");
  dump_vcg_header(f, get_irg_dump_name(irg), NULL);

  current_ir_graph = irg;

  dump_interval_loop(f, get_irg_loop(current_ir_graph));

  vcg_close(f);
}
