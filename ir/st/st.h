/* Copyright (c) 2002 by Universität Karlsruhe (TH).  All Rights Reserved  */

/***
   NAME
     st.h
   PURPOSE
     provide some auxilliary structures for firm graphs.
   NOTES
     not quite complete
   HISTORY
     liekweg - Feb 26, 2002: Created.
   CVS:
     $Id$
***/

# ifndef _ST_H_
# define _ST_H_

/* Includes:  */
#include "irgraph.h"
#include "irnode.h"

#include "bs.h"

#include "bool.h"


/* Data Types: */

/*   One dominator tree */
typedef struct
{
  int n_blocks;
  ir_graph *graph;	/* PRE */
  ir_node **blocks;
  ir_node **idoms;	/* idom [n] == immediate dominator of blocks [n] */
  bs_t *masks;
}
dt_t;

/* List entry:  */
typedef struct dtree_t
{
  dt_t *tree;
  ir_graph *graph;

  struct dtree_t *next;
}
dtree_t;

/* dominator environment for a node @a in graph @graph */
typedef struct dom_env_t
{
  dt_t     *dt;
  ir_graph *graph;
  ir_node  *a;
  int       index_a;
  bs_t      mask;
} dom_env_t;

/* Forwards for Globals:  */
extern dtree_t *trees;
extern dtree_t *last;

/* Prototypes: */
void     st_build_dominator_tree (ir_graph*);
bool     dominates            (ir_graph*, ir_node*, ir_node*);
ir_node *get_idom             (ir_graph*, ir_node*);

dom_env_t *get_dom_env (ir_graph*, ir_node*);
void delete_dom_env (dom_env_t*);
bool dominates_l (dom_env_t*, ir_node*);

# endif /* defined _ST_H_ */
