/*
 * Project:     libFIRM
 * File name:   ir/ir/irdump.h
 * Purpose:     Write vcg representation of firm to file.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier, Hubert Schmidt
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


/**
 * @file irdump.h
 *
 * Dump routines for the ir graph and all type information.
 *
 * @author Martin Trapp, Christian Schaefer
 *
 * The dump format of most functions is vcg.  This is a text based graph
 * representation. Some use the original format,
 * but most generate an extended format that is only read by some special
 * versions of xvcg or by the comercialized version now calles aiSee.
 * A test version of aiSee is available at
 * http://www.absint.de/aisee/download/index.htm.
 *
 * Most routines use the name of the passed entity as the name of the
 * file dumped to.
 */


# ifndef _IRDUMP_H_
# define _IRDUMP_H_

# include "irnode.h"
# include "irgraph.h"

/**
 * The value of this string will be added to the file name before .vcg
 *
 * @todo  GL: A hack -- add parameter to dumper function.
 */
extern char *dump_file_suffix;

/**
 *  Dump a firm graph.
 *
 *  @param irg  The firm graph to be dumped.
 *
 *  @return
 *     A file containing the firm graph in vcg format.
 *
 *  Dumps all Firm nodes of a single graph for a single procedure in
 *  standard xvcg format.
 *  Dumps the graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending .vcg.  Eventually overwrites existing files.
 *
 * @see turn_off_edge_labels()
 */
void dump_ir_graph (ir_graph *irg);

/**
 *  Dump a firm graph without explicit block nodes.
 *
 *  @param irg   The firm graph to be dumped.
 *
 *  @return
 *     A file containing the firm graph in vcg format.
 *
 *  Dumps all Firm nodes of a single graph for a single procedure in
 *  extended xvcg format.
 *  Dumps the graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending .vcg.  Eventually overwrites existing files.
 *
 * @see turn_off_edge_labels()
 */
void dump_ir_block_graph (ir_graph *irg);

/**
 *   Dump the control flow graph of a procedure.
 *
 *   @param irg  The firm graph whose CFG shall be dumped.
 *
 *   @return
 *      A file containing the CFG in vcg format.
 *
 *   Dumps the control flow graph of a procedure in standard xvcg format.
 *   Dumps the graph to a file.  The file name is constructed from the
 *   name of the entity describing the procedure (irg->entity) and the
 *   ending -cfg.vcg.  Eventually overwrites existing files.
 *
 * @see turn_off_edge_labels()
 */
void dump_cfg (ir_graph *irg);

/**
 *  Dumps all the type information needed for Calls, Sels, ... in this graph.
 *
 *  @param irg   The firm graph whose type information is to be dumped.
 *  @return
 *      A file containing the type information of the firm graph in vcg format.
 *
 *  Dumps this graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending -type.vcg.  Eventually overwrites existing files.
 *
 * @see turn_off_edge_labels()
 */
void dump_type_graph (ir_graph *irg);

/**
 *   Dumps all type information.
 *
 *   @return
 *      A file containing all type information for the program in standard
 *      vcg format.
 *
 *   Dumps all type information that is somehow reachable in standard vcg
 *   format.
 *   Dumps the graph to a file named All_types.vcg.
 *
 * @see turn_off_edge_labels()
 */
void dump_all_types (void);

/**
 *   Dumps the class hierarchy with or without entities.
 *
 *   @param entities    Flag whether to dump the entities.
 *
 *   @return
 *      A file containing the class hierarchy tree for the program in standard
 *      vcg format.
 *
 *   Does not dump the global type.
 *   Dumps a node for all classes and the sub/supertype relations.  If
 *   entities is set to true also dumps the entities of classes, but without
 *   any additional information as the entities type.  The overwrites relation
 *   is dumped along with the entities.
 *   Dumps to a file class_hierarchy.vcg
 */
void dump_class_hierarchy (bool entities);

/**
 *  Dumps a firm graph and  all the type information needed for Calls,
 *  Sels, ... in this graph.
 *
 *  @param irg   The firm graph to be dumped with its type information.
 *
 *  @return
 *      A file containing the firm graph and the type information of the firm graph in vcg format.
 *
 *  Dumps the graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending -all.vcg.  Eventually overwrites existing files.
 *
 * @see turn_off_edge_labels()
 */
void dump_ir_graph_w_types (ir_graph *irg);

/**
 *  Dumps a firm graph and  all the type information needed for Calls,
 *  Sels, ... in this graph.
 *
 *  @param irg   The firm graph to be dumped with its type information.
 *
 *  @return
 *      A file containing the firm graph and the type information of the firm graph in vcg format.
 *
 *  The graph is in blocked format.
 *  Dumps the graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending -all.vcg.  Eventually overwrites existing files.
 *
 * @see turn_off_edge_labels()
 */
void dump_ir_block_graph_w_types (ir_graph *irg);

/**
 *  Dumps a interprocedural firm graph as dump_ir_graph.
 *
 *  @param irg   The firm graph to be dumped.
 *
 *  @return
 *      A file containing the firm graph in vcg format.
 */
void dump_cg_graph(ir_graph * irg);

/**
 *  Dumps a interprocedural firm graph as dump_ir_block_graph.
 *
 *  @param irg   The firm graph to be dumped.
 *
 *  @return
 *      A file containing the firm graph in vcg format.
 */
void dump_cg_block_graph(ir_graph * irg);


void dump_all_cg_block_graph(void);

/**
 *   The type of a walker function that is called for each graph.
 *
 *   @param irg   current visited graph
 */
typedef void dump_graph_func(ir_graph *irg);

/**
 *   A walker that calls a dumper for each graph.
 *
 *   @param dump_graph    The dumper to be used for dumping.
 *
 *   @return
 *      Whatever the dumper creates.
 *
 *   Walks over all firm graphs and  calls a dumper for each graph.
 *   The following dumpers can be passed as arguments:
 *   - dump_ir_graph()
 *   - dump_ir_block_graph()
 *   - dump_cfg()
 *   - dump_type_graph()
 *   - dump_ir_graph_w_types()
 *
 * @see turn_off_edge_labels()
 */
void dump_all_ir_graphs (dump_graph_func *dump_graph);

/**
 *   Sets the vcg flag "display_edge_labels" to no.
 *
 *   This is necessary
 *   as xvcg and aisee both fail to display graphs with self-edges if these
 *   edges have labes.
 *   Dumpers will generate vcg flags with a different header.
 */
void turn_off_edge_labels(void);

/**
 *   If set to true constants will be replicated for every use. In non blocked
 *   view edges from constant to block are scipped.  Vcg
 *   then layouts the graphs more compact, this makes them better readable.
 *   The flag is automatically and temporarily set to false if other
 *   edges are dumped, as outs, loop, ...
 *   Default setting: false.
 */
void dump_consts_local(bool b);
/**
 * Returns false if dump_out_edge_flag or dump_loop_information_flag
 * are set, else returns dump_const_local_flag.
 */
bool get_opt_dump_const_local(void);

/**
 *   Turns off dumping the values of constant entities. Makes type graphs
 *   better readable.
 */
void turn_off_constant_entity_values(void);

/**
 *   Turns on dumping the edges from the End node to nodes to be kept
 *   alive
 */
void dump_keepalive_edges(bool b);
bool get_opt_dump_keepalive_edges(void);

/**
 *   Turns on dumping the out edges starting from the Start block in
 *   dump_ir_graph.  To test the consistency of the out datastructure.
 */
void dump_out_edges(void);

/**
 *   If this flag is set the dumper dumps edges to immediate dominator in cfg.
 */
void dump_dominator_information(void);

/**
 *   If this flag is set the dumper dumps loop nodes and edges from
 *   these nodes to the contained ir nodes.
 *   Can be turned off with dont_dump_loop_information().
 *   If the loops are interprocedural nodes can be missing.
 */
void dump_loop_information(void);

/**
 * @see dump_loop_information()
 */
void dont_dump_loop_information(void);

# endif /* _IRDUMP_H_ */
