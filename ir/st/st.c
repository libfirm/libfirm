/*
 * Project:     libFIRM
 * File name:   ir/st/st.c
 * Purpose:     Provide some auxilliary structures for firm graphs.
 * Author:      Florian Liekweg
 * Modified by:
 * Created:     26.2.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
   NAME
     st.h
   PURPOSE
     provide some auxilliary structures for firm graphs.
   S
     not quite complete
***/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

# include <stdio.h>
# include <stdlib.h>

# include "st.h"
# include "irgwalk.h"
# include "xmalloc.h"

/* init globals: */
/*static*/ dtree_t *trees = 0;
/*
static dtree_t *last  = 0;
*/

/* --------------------------------------------------------------------
* Helper Functions
* -------------------------------------------------------------------- */
/*
  Helper function for get_n_blocks
*/
static void count_block (ir_node *block, void *env)
{
  int *n = (int*) env;

# ifdef DEBUG_libfirm
  assert (block && "no block");
  assert (env   && "no env");
# endif /* def DEBUG_libfirm */

  fprintf (stdout, "%s: Block(%p) has # (%i)\n",
		   __FILE__ ":count_block", (void *)block, *n);

  (*n) ++;
}

/*
  Count the number of blocks in the given graph
*/
static int get_n_blocks (ir_graph *graph)
{
  int n = 0;

  ir_node *end_block = get_irg_end (graph);

# ifdef DEBUG_libfirm
  assert (graph     && "no graph");
  assert (end_block && "no end block");
# endif /* def DEBUG_libfirm */

  irg_block_walk (end_block, count_block, NULL, &n);

  fprintf (stdout, "%s: Graph(%p) has (%i) blocks\n",
		   __FILE__ ":get_n_blocks", (void *)graph, n);

  return (n);
}

/*
  Build an empty dominator relation entry
*/
static dtree_t *new_dtree (dt_t *tree, ir_graph *graph)
{
  dtree_t *res = (dtree_t*) malloc (sizeof (dtree_t));

# ifdef DEBUG_libfirm
  assert (res      && "no memory for dtree");
# endif /* def DEBUG_libfirm */

  res->tree  = tree;
  res->graph = graph;
  res->next  = 0;

  return (res);
}

/*
  Build an empty dominator relation
*/
static dt_t *new_dt (ir_graph *graph)
{
  int n_blocks = get_n_blocks (graph);

  dt_t *res = (dt_t*) malloc (sizeof (dt_t));

# ifdef DEBUG_libfirm
  assert (n_blocks && "no blocks for dt");
  assert (res      && "no memory for dt");
# endif /* def DEBUG_libfirm */

  res->n_blocks = n_blocks;
  res->graph    = graph;
  res->blocks   = (ir_node**) calloc (n_blocks, sizeof (ir_node*));
  res->idoms    = (ir_node**) calloc (n_blocks, sizeof (ir_node*));
  res->masks    = (bs_t*)     calloc (n_blocks, sizeof (bs_t));

  assert (res && "no dt");

  return (res);
}
/*
static void free_dt (dt_t *dt)
{
  free (dt->blocks);	dt->blocks = 0;
  free (dt->masks);		dt->masks  = 0;
  free (dt);
}
*/

/* --------------------------------------------------------------------
* Private Functions
* -------------------------------------------------------------------- */

/*
  Given a graph, find its dominator tree in the global list:

  @return	The tree, if it exists, and 0 else
*/
static dt_t *get_dominator_tree   (ir_graph *graph)
{
  dtree_t *iter = trees;

# ifdef DEBUG_libfirm
  assert (graph && "no graph");
# endif /* def DEBUG_libfirm */

  while ((0 != iter) && (graph != iter->graph))
	iter = iter->next;

  if (0 != iter)
	{
# ifdef DEBUG_libfirm
	  assert ((graph == iter->tree->graph) && "wrong graph");
# endif /* def DEBUG_libfirm */

	  return (iter->tree);
	}
  else
	{
	  return (0);
	}
}

/*
  Given a dominator tree and a graph, enter the tree into the global list.
*/
static void add_dominator_tree (dt_t *tree)
{
  dtree_t  *dtree = 0;
  ir_graph *graph = tree->graph;

# ifdef DEBUG_libfirm
  assert (tree  && "no tree" );
  assert (graph && "no graph");
# endif /* def DEBUG_libfirm */

  dtree = new_dtree (tree, graph);

# ifdef VERBOSE_libfirm
  fprintf (stdout, "%s: Adding dtree(%p) into trees(%p)\n",
		   __FILE__ ":" __PRETTY_FUNCTION__, dtree, trees);
# endif /* def VERBOSE_libfirm */

  /* enter in global list:  */
  dtree->next = trees;
  trees = dtree;
}

/*
  Get (or create) the index for a given block
*/
static int get_index (dt_t *dt, ir_node *block)
{
  int i;

# ifdef DEBUG_libfirm
  assert (dt    && "no dt");
  assert (block && "no block");
# endif /* def DEBUG_libfirm */

  for (i = 0; (i < dt->n_blocks) && (0 != dt->blocks [i]); i ++)
	if (block == dt->blocks [i])
	  return (i);

  /* not found . . . enter new one: */
  dt->blocks [i] = block;
  if (block == get_irg_start_block (dt->graph))
	{
	  dt->masks  [i] = 0x00000001 << i;

# ifdef VERBOSE_libfirm
	  fprintf (stdout, "%s: Adding block(%p)[%i] with [%#010lx]\n",
			   __FILE__ ":" __PRETTY_FUNCTION__, block, i, dt->masks [i]);
# endif /* def VERBOSE_libfirm */
	}
  else
	{
	  dt->masks  [i] = ~0x00000000;

# ifdef VERBOSE_libfirm
	  fprintf (stdout, "%s: Adding block(%p)[%i] with [%#010lx]\n",
			   __FILE__ ":" __PRETTY_FUNCTION__, block, i, dt->masks [i]);
# endif /* def VERBOSE_libfirm */
	}

  return (i);
}

/*
  Get the bit mask for a block
*/
static bs_t _get_mask (dt_t*, int);
static bs_t get_mask (dt_t *dt, ir_node *block)
{
  int index = get_index (dt, block);

# ifdef DEBUG_libfirm
  assert (dt    && "no dt");
  assert (block && "no block");
# endif /* def DEBUG_libfirm */

  return (_get_mask (dt, index));
}

/*
  Get the bit mask for a block index
*/
static bs_t _get_mask (dt_t *dt, int index)
{
# ifdef DEBUG_libfirm
  assert (dt && "no dt");
# endif /* def DEBUG_libfirm */

  return (dt->masks [index]);
}

/*
  Set the bit mask for a block
*/
#if 0
static void _set_mask (dt_t*, int, bs_t);
static void set_mask (dt_t *dt, ir_node *block, bs_t mask)
{
  int index = get_index (dt, block);

# ifdef DEBUG_libfirm
  assert (dt    && "no dt");
  assert (block && "no block");
  # endif /* def DEBUG_libfirm */

  _set_mask (dt, index, mask);
}
#endif
/*
  Set the bit mask for a block index
*/
static void _set_mask (dt_t *dt, int index, bs_t mask)
{
# ifdef DEBUG_libfirm
  assert (dt && "no dt");
# endif /* def DEBUG_libfirm */

  dt->masks [index] = mask;
}

/*
  Update the list of dominators of a given block
*/
typedef struct dt_walk_env_t
{
  dt_t    *dt;		/* the dominator relation we're building */
  ir_node *start_block;	/* need to know the start block of this graph */
  bool     changed;	/* wether the relation has changed recently */
}
dt_walk_env_t;

/*
  Helper function for build_dominator_tree
*/
static void update_dominators (ir_node *block, void *env)
{
  int i;
  dt_walk_env_t *w  = (dt_walk_env_t*) env;
  dt_t          *dt = w->dt;

  int block_index = get_index     (dt, block);
  int n_ins       = get_irn_arity (block);

  bs_t old_mask   = _get_mask     (dt, block_index);
  bs_t new_mask   = ~0x00000000;

  /* Special handling of Start Block: */
  if (block == w->start_block)
	return;

  /* check preds: */
  for (i = 0; i < n_ins; i ++)
    {
      ir_node *in  = get_nodes_block (get_irn_n (block, i));
      bs_t in_mask = get_mask  (dt, in);

      new_mask &= in_mask;
    }

  /* and remember ourselves: */
  new_mask |= (0x00000001 << block_index);

  if (new_mask != old_mask)
	{
	  w->changed = true;
	  _set_mask (dt, block_index, new_mask);

# ifdef VERBOSE_libfirm
	  fprintf (stdout, "%s: Updating block(%p)[%i] from [%#010lx] to [%#010lx]\n",
			   __FILE__ ":" __PRETTY_FUNCTION__,
			   block, block_index, old_mask, new_mask);
# endif /* def VERBOSE_libfirm */
	}
}

/*
  Say wether a dominates b
*/

static bool _dominates (dt_t *dt, ir_node *a, ir_node *b)
{
  int index_a = get_index (dt, a);
  bs_t b_mask = get_mask  (dt, b);

  return (0 != (b_mask & (0x00000001 << index_a)));
}

/*
  Return the immediate dominator of block a
*/
static ir_node *_get_idom (dt_t *dt, ir_node *block)
{
  ir_node *idom   = 0;
  int block_index = get_index (dt, block);

  if (0 != (idom = dt->idoms [block_index]))
	return (idom);

  /* check all CFG preds:
     Question: Shouldn't it be good enough to just ask our first CFG
     predecessor?  */
  {
	int i         = 0;
	int n         = get_irn_arity (block);
	ir_node *pred = 0;

	idom = block;					/* prime the loop: */

	for (i = 0; i < n; i ++)
	  {
		ir_node *ndom = 0;

		pred = get_nodes_block (get_irn_n (block, i));
		ndom = _get_idom (dt, pred);

		if (ndom != idom)
		  if (_dominates (dt, idom, ndom))
			idom = ndom;
	  }
  }

  assert (idom && "Something terribly wrong in _get_idom");

# ifdef VERBOSE_libfirm
  fprintf (stdout, "%s: idom(%p) = %p\n",
		   __FILE__ ":" __PRETTY_FUNCTION__,
		   block, idom);
# endif /* def VERBOSE_libfirm */

  /*  remember it: */
  dt->idoms [block_index] = idom;

  return (idom);
}

/* --------------------------------------------------------------------
* Public Functions
* -------------------------------------------------------------------- */

/*
  Say wether a dominates b
*/
bool dominates (ir_graph *g, ir_node *a, ir_node *b)
{
  dt_t *dt    = get_dominator_tree (g);

  return (_dominates (dt, a, b));
}

/*
  Return the immediate dominator of block a
*/
ir_node *get_idom (ir_graph *g, ir_node *a)
{
  dt_t *dt = get_dominator_tree (g);

  return (_get_idom (dt, a));
}

/*
  Allow for precomputation of necessary data
  This will allow for a slightly faster lookup of the dominator
  relation if one node is checked for dominance against many other nodes.
*/
dom_env_t *get_dom_env (ir_graph *graph, ir_node *a)
{
  dom_env_t *env = (dom_env_t*) calloc (1, sizeof (dom_env_t));

  env->graph   = graph;
  env->dt      = get_dominator_tree (graph);
  env->a       = a;
  env->index_a = get_index (env->dt, a);
  return env;
}

/*
  Dispose a dominator environment
*/
void delete_dom_env (dom_env_t *env)
{
  env->dt      = 0;
  env->a       = 0;
  env->graph   = 0;
  env->index_a = -1;

  free (env);
}

/*
  Say wether the node in env dominates b

  see get_dom_env
*/
bool dominates_l (dom_env_t *env, ir_node *b)
{
  int index_a = env->index_a;
  dt_t *dt = env->dt;
  bs_t b_mask = get_mask  (dt, b);

  return (0 != (b_mask & (0x00000001 << index_a)));
}

/*
  Build a new dominator tree for the given graph
*/
void build_dominator_tree (ir_graph *graph)
{
  /*
  dt_walk_env_t *w   = (dt_walk_env_t*) alloca (sizeof (dt_walk_env_t));

  ir_node *end_block   = get_irg_end_block   (graph);
  ir_node *start_block = get_irg_start_block (graph);
  int      n_blocks    = get_n_blocks        (graph);
  dt_t    *dt          = get_dominator_tree  (graph);
  */

  dt_walk_env_t *w     = 0;
  ir_node *end_block   = 0;
  ir_node *start_block = 0;
  int      n_blocks    = 0;
  dt_t    *dt          = 0;

  w           = (dt_walk_env_t*) alloca (sizeof (dt_walk_env_t));
  end_block   = get_irg_end_block   (graph);
  start_block = get_irg_start_block (graph);
  n_blocks    = get_n_blocks        (graph);
  dt          = get_dominator_tree  (graph);

# ifdef DEBUG_libfirm
  assert (graph && "no graph");
# endif /* def DEBUG_libfirm */

# ifdef VERBOSE_libfirm
  fprintf (stdout, "%s: for graph(%p)\n", __FILE__ ":" __PRETTY_FUNCTION__, graph);
# endif /* def VERBOSE_libfirm */

  /*if (0 != dt)
    free_dt (dt); */

  dt = new_dt (graph);

  w->dt          = dt;
  w->start_block = start_block;
  w->changed     = true;	/* at least one walk */

  /* do the walk: */
  {
	int walks = 0;

	while (w->changed)
	  {
		w->changed = false;
		irg_block_walk (end_block, update_dominators, update_dominators, (void*) w);
		walks ++;
	  }

# ifdef VERBOSE_libfirm
	fprintf (stdout, "%s: for graph(%p):  %i passes\n",
			 __FILE__ ":" __PRETTY_FUNCTION__, graph, walks);
# endif /* def VERBOSE_libfirm */
  }

  /* ok, now remember it: */
  add_dominator_tree (dt);
}
