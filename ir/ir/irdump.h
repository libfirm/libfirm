/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** dump an ir graph, for further use with xvcg
*/

/* $Id$ */

# ifndef _IRDUMP_H_
# define _IRDUMP_H_

# include "irnode.h"
# include "irgraph.h"

/****h* libfirm/irdump
 *
 * NAME
 *   irdump -- dump routines for the graph and all type information
 * NOTES
 *   The dump format of most functions is vcg.  This is a text based graph
 *   representation. Some use the original format,
 *   but most generate an extended format that is only read by some special
 *   versions of xvcg or by the comercialized version now calles aiSee.
 *   A test version of aiSee is available at http://www.absint.de/aisee/download/index.htm.
 *
 *   Most routines use the name of the passed entity as the name of the
 *   file dumped to.
 *
 ******
 */

#if 0
/* The following routines use a global variable that is not external.
   Therefore removed from interface. */
/* dump a simple node */
void dump_ir_node (ir_node *node);
/* dump the edge to the block this node belongs to */
void dump_ir_block_edge(ir_node *n);
/* dump edges to our inputs */
void dump_ir_data_edges(ir_node *n);
#endif

/****m* irdump/dump_ir_graph
 *
 * NAME
 *   dump_ir_graph  -- dump a firm graph
 * SYNOPSIS
 *  void dump_ir_graph (ir_graph *irg);
 * FUNCTION
 *  Dumps all Firm nodes of a single graph for a single procedure in
 *  standard xvcg format.
 *  Dumps the graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending .vcg.  Eventually overwrites existing files.
 * INPUTS
 *   irg: The firm graph to be dumped.
 * RESULT
 *   A file containing the firm graph in vcg format.
 * SEE ALSO
 *  turn_of_edge_labels
 ***
 */
void dump_ir_graph (ir_graph *irg);

/****m* irdump/dump_ir_block_graph
 *
 * NAME
 *   dump_ir_block_graph -- dump a firm graph without explicit block nodes.
 * SYNOPSIS
 *   void dump_ir_block_graph (ir_graph *irg);
 * FUNCTION
 *  Dumps all Firm nodes of a single graph for a single procedure in
 *  extended xvcg format.
 *  Dumps the graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending .vcg.  Eventually overwrites existing files.
 * INPUTS
 *   irg: The firm graph to be dumped.
 * RESULT
 *   A file containing the firm graph in vcg format.
 * SEE ALSO
 *  turn_of_edge_labels
 ***
 */
void dump_ir_block_graph (ir_graph *irg);

/****m* irdump/dump_cfg
 *
 * NAME
 *   dump_cfg -- Dump the control flow graph of a procedure
 * SYNOPSIS
 *   void dump_cfg (ir_graph *irg);
 * FUNCTION
 *   Dumps the control flow graph of a procedure in standard xvcg format.
 *   Dumps the graph to a file.  The file name is constructed from the
 *   name of the entity describing the procedure (irg->entity) and the
 *   ending -cfg.vcg.  Eventually overwrites existing files.
 * INPUTS
 *   irg: The firm graph whose CFG shall be dumped.
 * RESULT
 *   A file containing the CFG in vcg format.
 * SEE ALSO
 *  turn_of_edge_labels
 ***
 */
void dump_cfg (ir_graph *irg);

/****m* irdump/dump_type_graph
 *
 * NAME
 *   dump_type_graph --
 * SYNOPSIS
 *   void dump_type_graph (ir_graph *irg);
 * FUNCTION
 *  Dumps all the type information needed for Calls, Sels, ... in this graph.
 *  Dumps this graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending -type.vcg.  Eventually overwrites existing files.
 * INPUTS
 *  irg: The firm graph whose type information is to be dumped.
 * RESULT
 *  A file containing the type information of the firm graph in vcg format.
 * SEE ALSO
 *  turn_of_edge_labels
 ***
 */
void dump_type_graph (ir_graph *irg);

/****m* irdump/dump_all_types
 *
 * NAME
 *   dump_all_types -- Dumps all type information
 * SYNOPSIS
 *   void dump_all_types (void);
 * FUNCTION
 *   Dumps all type information that is somehow reachable in standard vcg
 *   format.
 *   Dumps the graph to a file named All_types.vcg.
 * INPUTS
 *   No inputs.
 * RESULT
 *   A file containing all type information for the program in standard
 *   vcg format.
 * SEE ALSO
 *  turn_of_edge_labels
 ***
 */
void dump_all_types (void);

/****m* irdump/dump_ir_graph_w_types
 *
 * NAME
 *   dump_ir_graph_w_types
 * SYNOPSIS
 *   void dump_ir_graph_w_types (ir_graph *irg);
 * FUNCTION
 *  Dumps a firm graph and  all the type information needed for Calls,
 *  Sels, ... in this graph.
 *  Dumps the graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending -all.vcg.  Eventually overwrites existing files.
 * INPUTS
 *   irg: The firm graph to be dumped with its type information.
 * RESULT
 *   A file containing the firm graph and the type information of the firm graph in vcg format.
 * SEE ALSO
 *  turn_of_edge_labels
 ***
 */

void dump_ir_graph_w_types (ir_graph *irg);
 /****m* irdump/dump_ir_block_graph_w_types
 *
 * NAME
 *   dump_ir_block_graph_w_types
 * SYNOPSIS
 *   void dump_ir_block_graph_w_types (ir_graph *irg);
 * FUNCTION
 *  Dumps a firm graph and  all the type information needed for Calls,
 *  Sels, ... in this graph.  The graph is in blocked format.
 *  Dumps the graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending -all.vcg.  Eventually overwrites existing files.
 * INPUTS
 *   irg: The firm graph to be dumped with its type information.
 * RESULT
 *   A file containing the firm graph and the type information of the firm graph in vcg format.
 * SEE ALSO
 *  turn_of_edge_labels
 ***
 */
void dump_ir_block_graph_w_types (ir_graph *irg);

/****m* irdump/dump_all_ir_graphs
 *
 * NAME
 *   dump_all_ir_graphs -- a walker that calls a dumper for each graph
 * SYNOPSIS
 *   void dump_all_ir_graphs (void dump_graph(ir_graph*));
 * FUNCTION
 *   Walks over all firm graphs and  calls a dumper for each graph.
 *   The following dumpers can be passed as arguments:
 *   dump_ir_graph
 *   dump_ir_block_graph
 *   dump_cfg
 *   dump_type_graph
 *   dump_ir_graph_w_types
 * INPUTS
 *   The dumper to be used for dumping.
 * RESULT
 *   Whatever the dumper creates.
 * SEE ALSO
 *  turn_of_edge_labels
 ***
 */
void dump_all_ir_graphs (void dump_graph(ir_graph*));

/****m* irdump/turn_of_edge_labels
 *
 * NAME
 *   turn_of_edge_labels
 * SYNOPSIS
 *   void turn_of_edge_labels();
 * FUNCTION
 *   Sets the vcg flag "display_edge_labels" to no.  This is necessary
 *   as xvcg and aisee both fail to display graphs with self-edges if these
 *   edges have labes.
 * INPUTS
 *   No inputs
 * RESULT
 *   dumpers will generate vcg flags with a different header.
 * SEE ALSO
 *
 ***
 */
void turn_of_edge_labels();


/****m* irdump/dump_constant_entity_values
 *
 * NAME
 *   dump_constant_entity_values
 * SYNOPSIS
 *   void dump_constant_entity_values()
 * FUNCTION
 *   Turns off dumping the values of constant entities. Makes type graphs
 *   better readable.
 * INPUTS
 *   No inputs
 * RESULT
 * SEE ALSO
 *
 ***
 */
void dump_constant_entity_values();


/****m* irdump/dump_constant_entity_values
 *
 * NAME
 *   dump_keepalive_edges
 * SYNOPSIS
 *   void dump_keepalive_edges()
 * FUNCTION
 *   Turns on dumping the edges from the End node to nodes to be kept
 *   alive
 * INPUTS
 *   No inputs
 * RESULT
 * SEE ALSO
 *
 ***
 */
void dump_keepalive_edges();


# endif /* _IRDUMP_H_ */
