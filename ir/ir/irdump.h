/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Martin Trapp, Christian Schaefer
*
* dump an ir graph, for further use with xvcg
*/

/* $Id$ */

# ifndef _IRDUMP_H_
# define _IRDUMP_H_

# include "irnode.h"
# include "irgraph.h"

/**
 *
 *   - dump routines for the graph and all type information
 *   The dump format of most functions is vcg.  This is a text based graph
 *   representation. Some use the original format,
 *   but most generate an extended format that is only read by some special
 *   versions of xvcg or by the comercialized version now calles aiSee.
 *   A test version of aiSee is available at
 *   http://www.absint.de/aisee/download/index.htm.
 *
 *   Most routines use the name of the passed entity as the name of the
 *   file dumped to.
 *
 */

/* @@@ GL: A hack */
extern char *dump_file_suffix;

/**
 *
 *   - dump a firm graph
 *  Dumps all Firm nodes of a single graph for a single procedure in
 *  standard xvcg format.
 *  Dumps the graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending .vcg.  Eventually overwrites existing files.
 *   @param irg: The firm graph to be dumped.
 *   @return A file containing the firm graph in vcg format.
 * @see turn_of_edge_labels
 * @see
 */
void dump_ir_graph (ir_graph *irg);

/**
 *
 *   - dump a firm graph without explicit block nodes.
 *  Dumps all Firm nodes of a single graph for a single procedure in
 *  extended xvcg format.
 *  Dumps the graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending .vcg.  Eventually overwrites existing files.
 *   @param irg: The firm graph to be dumped.
 *   @return A file containing the firm graph in vcg format.
 * @see turn_of_edge_labels
 * @see
 */
void dump_ir_block_graph (ir_graph *irg);

/**
 *
 *   - Dump the control flow graph of a procedure
 *   Dumps the control flow graph of a procedure in standard xvcg format.
 *   Dumps the graph to a file.  The file name is constructed from the
 *   name of the entity describing the procedure (irg->entity) and the
 *   ending -cfg.vcg.  Eventually overwrites existing files.
 *   @param irg: The firm graph whose CFG shall be dumped.
 *   @return A file containing the CFG in vcg format.
 * @see turn_of_edge_labels
 * @see
 */
void dump_cfg (ir_graph *irg);

/**
 *
 *   -
 *  Dumps all the type information needed for Calls, Sels, ... in this graph.
 *  Dumps this graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending -type.vcg.  Eventually overwrites existing files.
 *  @param irg: The firm graph whose type information is to be dumped.
 *  @return A file containing the type information of the firm graph in vcg format.
 * @see turn_of_edge_labels
 * @see
 */
void dump_type_graph (ir_graph *irg);

/**
 *
 *   - Dumps all type information
 *   Dumps all type information that is somehow reachable in standard vcg
 *   format.
 *   Dumps the graph to a file named All_types.vcg.
 *   @param No inputs.
 *   @return A file containing all type information for the program in standard
 *   @return vcg format.
 * @see turn_of_edge_labels
 * @see
 */
void dump_all_types (void);

/**
 *
 *    - Dumps the class hierarchy with or without entities.
 *   Dumps a node for all classes and the sub/supertype relations.  If
 *   entities is set to true also dumps the entities of classes, but without
 *   any additional information as the entities type.  The overwrites relation
 *   is dumped along with the entities.
 *   Dumps to a file class_hierarchy.vcg
 *   @param Flag whether to dump the entities.
 *   @return A file containing the class hierarchy tree for the program in standard
 *   @return vcg format.
 * @see
 * @see
 */
void dump_class_hierarchy (bool entities);

/**
 *
 *   dump_ir_graph_w_types
 *  Dumps a firm graph and  all the type information needed for Calls,
 *  Sels, ... in this graph.
 *  Dumps the graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending -all.vcg.  Eventually overwrites existing files.
 *   @param irg: The firm graph to be dumped with its type information.
 *   @return A file containing the firm graph and the type information of the firm graph in vcg format.
 * @see turn_of_edge_labels
 * @see
 */

void dump_ir_graph_w_types (ir_graph *irg);
/**
 *
 *   dump_ir_block_graph_w_types
 *  Dumps a firm graph and  all the type information needed for Calls,
 *  Sels, ... in this graph.  The graph is in blocked format.
 *  Dumps the graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending -all.vcg.  Eventually overwrites existing files.
 *   @param irg: The firm graph to be dumped with its type information.
 *   @return A file containing the firm graph and the type information of the firm graph in vcg format.
 * @see turn_of_edge_labels
 * @see
 */
void dump_ir_block_graph_w_types (ir_graph *irg);



/**
 *
 *   dump_cg_graph
 *  Dumps a interprocedural firm graph as dump_ir_graph.
 *   @param irg: The firm graph to be dumped.
 *   @return A file containing the firm graph in vcg format.
 * @see
 */
void dump_cg_graph(ir_graph * irg);

/**
 *
 *   dump_cg_block_graph
 *  Dumps a interprocedural firm graph as dump_ir_block_graph.
 *   @param irg: The firm graph to be dumped.
 *   @return A file containing the firm graph in vcg format.
 * @see
 */
void dump_cg_block_graph(ir_graph * irg);


void dump_all_cg_block_graph();

/**
 *
 *   - a walker that calls a dumper for each graph
 *   Walks over all firm graphs and  calls a dumper for each graph.
 *   The following dumpers can be passed as arguments:
 *   dump_ir_graph
 *   dump_ir_block_graph
 *   dump_cfg
 *   dump_type_graph
 *   dump_ir_graph_w_types
 *   @param The dumper to be used for dumping.
 *   @return Whatever the dumper creates.
 * @see turn_of_edge_labels
 * @see
 */
typedef void (dump_graph_func)(ir_graph *);
void dump_all_ir_graphs (dump_graph_func *dump_graph);

/**
 *
 *   turn_off_edge_labels
 *   Sets the vcg flag "display_edge_labels" to no.  This is necessary
 *   as xvcg and aisee both fail to display graphs with self-edges if these
 *   edges have labes.
 *   @param No inputs
 *   @return dumpers will generate vcg flags with a different header.
 * @see
 * @see
 */
void turn_off_edge_labels();

/**
 *
 *   dump_consts_local
 *   If set to true constants will be replicated for every use. In non blocked
 *   view edges from constant to block are scipped.  Vcg
 *   then layouts the graphs more compact, this makes them better readable.
 *   The flag is automatically and temporarily set to false if other
 *   edges are dumped, as outs, loop, ...
 *   Default setting: false.
 * @see
 * @see
 */
void dump_consts_local(bool b);


/**
 *
 *   turn_off_constant_entity_values
 *   Turns off dumping the values of constant entities. Makes type graphs
 *   better readable.
 *   @param No inputs
 * @see
 * @see
 */
void turn_off_constant_entity_values();


/**
 *
 *   dump_keepalive_edges
 *   Turns on dumping the edges from the End node to nodes to be kept
 *   alive
 *   @param No inputs
 * @see
 * @see
 */
void dump_keepalive_edges(bool b);


/**
 *
 *   dump_out_edges
 *   Turns on dumping the out edges starting from the Start block in
 *   dump_ir_graph.  To test the consistency of the out datastructure.
 *   @param No inputs
 * @see
 * @see
 */
void dump_out_edges();


/**
 *
 *   dump_dominator_information
 *   If this flag is set the dumper dumps edges to immediate dominator in cfg.
 *   @param No inputs
 * @see
 * @see
 */
void dump_dominator_information();


/**
 *
 *   dump_loop_information
 *   If this flag is set the dumper dumps loop nodes and edges from
 *   these nodes to the contained ir nodes.
 *   Can be turned off with dont_dump_loop_information().
 *   If the loops are interprocedural nodes can be missing.
 *   @param No inputs
 * @see
 * @see
 */
void dump_loop_information();
void dont_dump_loop_information();

# endif /* _IRDUMP_H_ */
