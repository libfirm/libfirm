/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/pto.c
 * Purpose:     Pto
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# ifdef HAVE_CONFIG_H
#  include <config.h>
# endif

# include "pto.h"

# include "entity.h"


# include "irnode_t.h"
# include "irprog_t.h"

/* # include "eset.h" */
# include "irgraph.h"
# include "irgwalk.h"
# include "irgmod.h"
# include "irvrfy.h"
# include "trvrfy.h"
# include "xmalloc.h"

# include "irmemwalk.h"

# ifndef TRUE
#  define TRUE 1
#  define FALSE 0
# endif /* not defined TRUE */

/* BEGIN TEST */
/*
static void pto_node_pre (ir_node *node, void *__unused)
{
  fprintf (stdout, "PRE  MEM Node (0x%08x) (%s)\n",
           (int)  node,
           get_op_name (get_irn_op (node)));
           }
*/

static void pto_node_post (ir_node *node, void *__unused)
{
  const opcode op = get_irn_opcode (node);


  if (iro_Call == op) {
    entity *ent = NULL;
    ir_graph *graph = NULL;
    fprintf (stdout, "POST MEM Call Node (0x%08x)\n",
             (int) node);

    ir_node *ptr = get_Call_ptr (node);

    if (iro_Sel == get_irn_opcode (ptr)) {
      ent = get_Sel_entity (ptr);
    } else if (iro_SymConst == get_irn_opcode (ptr)) {
      if (get_SymConst_kind(ptr) == symconst_addr_ent) {
        ent = get_SymConst_entity (ptr);
      }
    }

    if (NULL != ent) {
      graph = get_entity_irg (ent);
      if (NULL != graph) {
        if (! get_irg_is_mem_visited (graph)) {

          fprintf (stdout, " -> visit  graph (0x%08x) of \"%s.%s\"\n",
                   (int) graph,
                   get_type_name (get_entity_owner (get_irg_entity (graph))),
                   get_entity_name (get_irg_entity (graph)));

          /* irg_walk_mem (graph, pto_node_pre, pto_node_post, NULL); */
          irg_walk_mem (graph, NULL, pto_node_post, NULL);
        }
      }
    }
  } else {
    fprintf (stdout, "POST MEM Node (0x%08x) (%s)\n",
             (int)  node,
             get_op_name (get_irn_op (node)));
  }
}


/* END TEST */

/*
   Test irg_walk_mem
*/
void pto_test_mem ()
{
  int i;

  fprintf (stdout, "START PTO TEST\n");

  for (i = 0; i < get_irp_n_irgs(); i++) {
    ir_graph *graph = get_irp_irg (i);

    fprintf (stdout, "START GRAPH (0x%08x) of \"%s.%s\"\n",
             (int) graph,
             get_type_name (get_entity_owner (get_irg_entity (graph))),
             get_entity_name (get_irg_entity (graph)));
    /* irg_walk_mem (graph, pto_node_pre, pto_node_post, NULL); */
    irg_walk_mem (graph, NULL, pto_node_post, NULL);
    fprintf (stdout, "END   GRAPH (0x%08x)\n", (int) graph);
  }

  fprintf (stdout, "END   PTO TEST\n");
}


/*
 * $Log$
 * Revision 1.2  2004/10/21 11:09:37  liekweg
 * Moved memwalk stuf into irmemwalk
 * Moved lset stuff into lset
 * Moved typalise stuff into typalise
 *
 * Revision 1.1  2004/10/20 14:59:42  liekweg
 * Added ana2, added ecg and pto
 *
 */
