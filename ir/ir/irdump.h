/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
** dump an ir graph, for further use with xvcg
*/

# ifndef _IRDUMP_H_
# define _IRDUMP_H_

# include "irnode.h"
# include "irgraph.h"

/* dump a simple node */
void dump_ir_node (ir_node *node);
/* dump the edge to the block this node belongs to */
void dump_ir_block_edge(ir_node *n);
/* dump edges to our inputs */
void dump_ir_data_edges(ir_node *n);

/* dump a hole graph */
void dump_ir_graph (ir_graph *irg);

/* dump a graph blockwise */
void dump_ir_block_graph (ir_graph *irg);

/* dump a control flow graph */
void dump_cfg (ir_graph *irg);

/* dumps the type information reachable from an ir graph. */
void dump_type_graph (ir_graph *irg);

/* dumps all type information (reachable from ir prog). */
void dump_all_types (void);

/* dumps a graph and the type inforamtion. */
void dump_ir_graph_w_types (ir_graph *irg);

/* dumps all graphs with the graph-dumper passed. Possible dumpers:
 * dump_ir_graph
 * dump_ir_block_graph
 * dump_cfg
 * dump_type_graph
 * dump_ir_graph_w_types                                            */
void dump_all_ir_graphs (void dump_graph(ir_graph*));

/* To turn off display of edge labels.  Edge labels offen cause xvcg to
   abort with a segmentation fault. */
void turn_of_edge_labels();


# endif /* _IRDUMP_H_ */
