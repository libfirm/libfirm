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
# include "irloop.h"


/* **************************************************************************** */
/*                                 GRAPH DUMPERS                                */
/* **************************************************************************** */

/**
 * This hook is called to dump the vcg attributes of a node to a file.
 * If this function returns zero, the default attributes are added, else
 * removed.
 */
typedef int (*DUMP_NODE_VCGATTR_FUNC)(FILE *F, ir_node *n);

/** Set the node_vcgattr hook. */
void set_dump_node_vcgattr_hook(DUMP_NODE_VCGATTR_FUNC hook);

/** Dump a firm graph.
 *
 *  @param irg  The firm graph to be dumped.
 *
 *  @return
 *     A file containing the firm graph in vcg format.
 *
 *  Dumps all Firm nodes of a single graph for a single procedure in
 *  standard xvcg format.  Dumps the graph to a file.  The file name
 *  is constructed from the name of the entity describing the
 *  procedure (irg->entity) and the ending -pure<-ip>.vcg.  Eventually
 *  overwrites existing files.  Visits all nodes in
 *  interprocedural_view.
 *
 * @see turn_off_edge_labels()
 */
void dump_ir_graph (ir_graph *irg, const char *suffix);

/** Dump a firm graph without explicit block nodes.
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
 *  ending <-ip>.vcg.  Eventually overwrites existing files.  Dumps several
 *  procedures in boxes if interprocedural_view.
 *
 * @see turn_off_edge_labels()
 */
void dump_ir_block_graph (ir_graph *irg, const char *suffix);

/** Dumps all graphs in interprocedural view to a file named All_graphs.vcg.
 */
void dump_all_cg_block_graph(const char *suffix);

/** Dumps a firm graph and  all the type information needed for Calls,
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
void dump_ir_graph_w_types (ir_graph *irg, const char *suffix);

/** Dumps a firm graph and  all the type information needed for Calls,
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
void dump_ir_block_graph_w_types (ir_graph *irg, const char *suffix);

/** The type of a walker function that is called for each graph.
 *
 *  @param irg   current visited graph
 */
typedef void dump_graph_func(ir_graph *irg, const char *suffix);

/**  A walker that calls a dumper for each graph.
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
void dump_all_ir_graphs (dump_graph_func *dump_graph, const char *suffix);


/**  Dump the control flow graph of a procedure.
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
void dump_cfg (ir_graph *irg, const char *suffix);


/* **************************************************************************** */
/*                              CALLGRAPH DUMPERS                               */
/* **************************************************************************** */


/** Dump the call graph.
 *
 * Dumps the callgraph to a file "Callgraph"<suffix>".vcg".
 */
void dump_callgraph(const char *suffix);

/* **************************************************************************** */
/*                              TYPEGRAPH DUMPERS                               */
/* **************************************************************************** */

/**
 *  Dumps all the type information needed for Calls, Sels, ... in this graph.
 *  Does not dump the graph!
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
void dump_type_graph (ir_graph *irg, const char *suffix);

/**  Dumps all type information.
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
void dump_all_types (const char *suffix);

/**  Dumps the class hierarchy with or without entities.
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
void dump_class_hierarchy (bool entities, const char *suffix);

/* **************************************************************************** */
/*                              LOOPTREE DUMPERS                                */
/* **************************************************************************** */

/**
 * Dump a standalone loop tree, which contains the loop nodes and the firm nodes
 * belonging to one loop packed together in one subgraph.  Dumps to file
 * <name of irg><suffix>-looptree.vcg
 * Turns on edge labels by default.
 *
 * Implementing this dumper was stimulated by Florian Liekwegs similar dumper.
 *
 * @arg irg     Dump the loop tree for this graph.
 * @arg suffix  Suffix to filename.
 */
void dump_loop_tree(ir_graph *irg, const char *suffix);

/** Dumps the firm nodes in the sub-loop-tree of loop to a graph.
 *
 *  Dumps the loop nodes if dump_loop_information() is set.
 *  The name of the file is loop_<loop_nr><suffix>.vcg.
 *
 *  @arg loop    Dump the loop tree for this loop.
 *  @arg suffix  Suffix to filename.
 */
void dump_loop (ir_loop *l, const char *suffix);

/** Dumps the loop tree over the call graph.
 *
 *  See for yourself what you can use this for.
 *  The filename is "Callgraph_looptree<suffix>.vcg".
 *
 *  @arg suffix  Suffix to filename.
 */
void dump_callgraph_loop_tree(const char *suffix);


/* **************************************************************************** */
/*                                TEXT DUMPERS                                  */
/* **************************************************************************** */

/** Verbosity for text dumpers */
typedef enum {
  dump_verbosity_onlynames         = 0x00000001,   /**< only dump names. turns off all other
						      flags up to 0x00010000. */
  dump_verbosity_fields            = 0x00000002,   /**< dump types and fields (like a type declaration) */
  dump_verbosity_methods           = 0x00000004,   /**< dump types and methods (like a type declaration) */
  dump_verbosity_typeattrs         = 0x00000008,   /**< dump all type attributes */
  dump_verbosity_entattrs          = 0x00000010,   /**< dump all entity attributes */
  dump_verbosity_entconsts         = 0x00000020,   /**< dump entity constants */

  dump_verbosity_accessStats       = 0x00000100,   /**< dump entity access statistics */
  dump_verbosity_csv               = 0x00000200,   /**< dump access statistics as comma separated list */

  dump_verbosity_noClassTypes      = 0x00001000,   /**< dump no class       types */
  dump_verbosity_noStructTypes     = 0x00002000,   /**< dump no struct      types */
  dump_verbosity_noUnionTypes      = 0x00004000,   /**< dump no union       types */
  dump_verbosity_noArrayTypes      = 0x00008000,   /**< dump no array       types */
  dump_verbosity_noPointerTypes    = 0x00010000,   /**< dump no pointer     types */
  dump_verbosity_noMethodTypes     = 0x00020000,   /**< dump no method      types */
  dump_verbosity_noPrimitiveTypes  = 0x00040000,   /**< dump no primitive   types */
  dump_verbosity_noEnumerationTypes= 0x00080000,   /**< dump no enumeration types */

  dump_verbosity_onlyClassTypes     = 0x000FE000,  /**< dump only class     types */
  dump_verbosity_onlyStructTypes    = 0x000FD000,  /**< dump only struct    types */
  dump_verbosity_onlyUnionTypes     = 0x000FB000,  /**< dump only union     types */
  dump_verbosity_onlyArrayTypes     = 0x000F7000,  /**< dump only array     types */
  dump_verbosity_onlyPointerTypes   = 0x000EF000,  /**< dump only pointer   types */
  dump_verbosity_onlyMethodTypes    = 0x000DF000,  /**< dump only method    types */
  dump_verbosity_onlyPrimitiveTypes = 0x000BF000,  /**< dump only primitive types */
  dump_verbosity_onlyEnumerationTypes=0x0007F000,  /**< dump only enumeration types */

  dump_verbosity_max                = 0x4FF00FFE   /**< turn on all verbosity.
						        @@@ Because of a bug in gcc 3.2 we can not set the first two bits. */
} dump_verbosity;


/** Write the entity and all its attributes to the passed file.
 *  */
void    dump_entity_to_file (FILE *F, entity *ent, unsigned verbosity);

/** Write the entity and all its attributes to the stdout.
 *
 *  Calls dump_entity_to_file().  */
void    dump_entity (entity *ent);

/** Write the type and all its attributes to the file passed.
 * */
void    dump_type_to_file (FILE *f, type *tp, unsigned verbosity);

/** Write the type and all its attributes to stdout.
 *  */
void    dump_type (type *tp);


/** Dump type information as text.
 *
 *  Often type graphs are unhandy in their vcg representation.  The text
 *  dumper represents the information for a single type more compact, but
 *  the relations between the types only implicitly.
 *  Dumps only 'real' types, i.e., those in the type list.  Does not dump
 *  the global type nor frame types or the like.
 *
 *  The file name is the program name (get_irp_name()), or 'TextTypes'
 *  if the program name is not set, appended by <suffix>-types.txt.
 *  For verbosity see the documentation of the verbosity flags above.
 */
void dump_types_as_text(unsigned verbosity, const char *suffix);

/* **************************************************************************** */
/*                                    FLAGS                                     */
/* **************************************************************************** */

/** Output a selected graph.
 *
 *  All graph dumpers check this name.  If the name is != "" and
 *  not a prefix of the graph to be dumped, the dumper does not
 *  dump the graph.
 *
 *  @param name The prefix of the name (not the ld_name) of the method
 *              entity to be dumped.
 */
void only_dump_method_with_name(ident *name);

/**  Sets the vcg flag "display_edge_labels" to no.
 *
 *   This is necessary as xvcg and aisee both fail to display graphs
 *   with self-edges if these edges have labes.
 */
void turn_off_edge_labels(void);

/**
 *  If set to true constants will be replicated for every use. In non
 *  blocked view edges from constant to block are skipped.  Vcg then
 *  layouts the graphs more compact, this makes them better readable.
 *  The flag is automatically and temporarily set to false if other
 *  edges are dumped, as outs, loop, ...
 *  Default setting: false.
 */
void dump_consts_local(bool b);

/**
 * Returns false if dump_out_edge_flag or dump_loop_information_flag
 * are set, else returns dump_const_local_flag.
 */
bool get_opt_dump_const_local(void);

/**  Turns off dumping the values of constant entities. Makes type graphs
 *   better readable.
 */
void turn_off_constant_entity_values(void);

/**  Turns on dumping the edges from the End node to nodes to be kept
 *   alive.
 */
void dump_keepalive_edges(bool b);
bool get_opt_dump_keepalive_edges(void);

/** Turns on dumping the out edges starting from the Start block in
 *  dump_ir_graph.
 *
 *  To test the consistency of the out datastructure.
 */
void dump_out_edges(void);

/** If this flag is set the dumper dumps edges to immediate dominator in cfg.
 */
void dump_dominator_information(void);

/** If this flag is set the dumper dumps loop nodes and edges from
 *  these nodes to the contained ir nodes.
 *
 *  Can be turned off with dont_dump_loop_information().
 *  If the loops are interprocedural nodes can be missing.
 */
void dump_loop_information(void);

/**
 * @see dump_loop_information()
 */
void dont_dump_loop_information(void);

/** If set and backedge info is computed, backedges are dumped dashed
 *  and as vcg 'backedge' construct.
 *
 *  Default: set.
 */
void dump_backedge_information(bool b);

/** Dump the information of type field specified in ana/irtypeinfo.h.
 *
 *  If the flag is set, the type name is output in [] in the node label,
 *  else it is output as info.
 */
void dump_analysed_type_info(bool b);

/** Write the address of a node into the vcg info.
 *
 *  This is off per default for automatic comparisons of
 *  vcg graphs -- these will differ in the pointer values!
 */
void dump_pointer_values_to_info(bool b);


# endif /* _IRDUMP_H_ */
