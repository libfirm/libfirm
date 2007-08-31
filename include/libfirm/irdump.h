/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Write vcg representation of firm to file.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Hubert Schmidt
 * @version $Id$
 * @summary
 *  Dump routines for the ir graph and all type information.
 *
 *  The dump format of most functions is vcg.  This is a text based graph
 *  representation. Some use the original format,
 *  but most generate an extended format that is only read by some special
 *  versions of xvcg or by the comercialized version now calles aiSee.
 *  A test version of aiSee is available at
 *   http://www.absint.de/aisee/download/index.htm.
 *
 *  We have developed an own advanced viewer called ycomp:
 *    http://www.info.uni-karlsruhe.de/software.php/id=6&lang=en
 *
 *  Most routines use the name of the passed entity as the name of the
 *  file dumped to.
 */
#ifndef FIRM_IR_IRDUMP_H
#define FIRM_IR_IRDUMP_H

#include "irnode.h"
#include "irgraph.h"
#include "irloop.h"

/**
 * Names of the 32 supported colors
 */
typedef enum {
  ird_color_default      = -1,
  ird_color_white        = 0,
  ird_color_blue         = 1,
  ird_color_red          = 2,
  ird_color_green        = 3,
  ird_color_yellow       = 4,
  ird_color_magenta      = 5,
  ird_color_cyan         = 6,
  ird_color_darkgray     = 7,
  ird_color_darkblue     = 8,
  ird_color_darkred      = 9,
  ird_color_darkgreen    = 10,
  ird_color_darkyellow   = 11,
  ird_color_darkmagenta  = 12,
  ird_color_darkcyan     = 13,
  ird_color_gold         = 14,
  ird_color_lightgray    = 15,
  ird_color_lightblue    = 16,
  ird_color_lightred     = 17,
  ird_color_lightgreen   = 18,
  ird_color_lightyellow  = 19,
  ird_color_lightmagenta = 20,
  ird_color_lightcyan    = 21,
  ird_color_lilac        = 22,
  ird_color_turquoise    = 23,
  ird_color_aquamarine   = 24,
  ird_color_khaki        = 25,
  ird_color_purple       = 26,
  ird_color_yellowgreen  = 27,
  ird_color_pink         = 28,
  ird_color_orange       = 29,
  ird_color_orchid       = 30,
  ird_color_black        = 31
} dumper_colors;

/**
 * Edge kinds
 */
typedef enum {
  data_edge           = 0x01,   /**< a data edge between two basic blocks */
  block_edge          = 0x02,   /**< an edge from a node to its basic block */
  cf_edge             = 0x03,   /**< regularly control flow edge */
  exc_cf_edge         = 0x04,   /**< exceptional control flow edge */
  mem_edge            = 0x05,   /**< memory edge */
  dominator_edge      = 0x06,   /**< dominator edge */
  node2type_edge      = 0x07,   /**< an edge from an IR node to a type */

  ent_type_edge       = 0x11,   /**< an edge from an entity to its type */
  ent_own_edge        = 0x12,   /**< an edge from an entity to its owner type */
  ent_overwrites_edge = 0x13,   /**< an edge from an entity to the entity it overwrites */
  ent_value_edge      = 0x14,   /**< an edge from an entity to its value entity */
  ent_corr_edge       = 0x15,   /**< an edge from an entity to the member entity its initializes */

  meth_par_edge       = 0x21,   /**< an edge from a method type to one of its parameter types */
  meth_res_edge       = 0x22,   /**< an edge from a method type to one of its result types */
  type_super_edge     = 0x23,   /**< an edge from a class type to its super/basis type */
  union_edge          = 0x24,   /**< an edge from a union type to its member types */
  ptr_pts_to_edge     = 0x25,   /**< an edge from a pointer type to its points-to type */
  arr_elt_type_edge   = 0x26,   /**< an edge from an array type to its element type */
  arr_ent_edge        = 0x27,   /**< an edge from a array type to its element entity */
  type_member_edge    = 0x28,   /**< an edge from a compound type to its member entities */

  /* additional flags */
  intra_edge          = 0,      /**< intra edge flag: edge do not cross basic block boundaries */
  inter_edge          = 0x40,   /**< inter edge flag: edge cross basic block boundaries */
  back_edge           = 0x80    /**< backwards edge flag */
} edge_kind;

/* **************************************************************************** */
/*                                 GRAPH DUMPERS                                */
/* **************************************************************************** */

/**
 * This hook is called to insert some special nodes into dumped graph
 */
typedef int (*DUMP_IR_GRAPH_FUNC)(FILE *F, ir_graph *irg);
/**
 * This hook is called to dump the vcg attributes of a node to a file.
 * If this function returns zero, the default attributes are added, else
 * removed.
 */
typedef int (*DUMP_NODE_VCGATTR_FUNC)(FILE *F, ir_node *node, ir_node *local);
/**
 * This hook is called to dump the vcg attributes of an edge to a file.
 * If this function returns zero, the default attributes are added, else
 * removed.
 */
typedef int (*DUMP_EDGE_VCGATTR_FUNC)(FILE *F, ir_node *node, int to);

/** Set the ir graph dump hook. */
void set_dump_ir_graph_hook(DUMP_IR_GRAPH_FUNC hook);
/** Set the node_vcgattr hook. */
void set_dump_node_vcgattr_hook(DUMP_NODE_VCGATTR_FUNC hook);
/** Set the edge_vcgattr hook. */
void set_dump_edge_vcgattr_hook(DUMP_EDGE_VCGATTR_FUNC hook);

typedef int (*DUMP_NODE_EDGE_FUNC)(FILE *f, ir_node *node);

/**
 * Set the hook to be called to dump additional edges to a node.
 * @param func The hook to be called.
 */
void set_dump_node_edge_hook(DUMP_NODE_EDGE_FUNC func);

/**
 * Get the additional edge dump hook.
 * @return The current additional edge dump hook.]
 */
DUMP_NODE_EDGE_FUNC get_dump_node_edge_hook(void);

/**
 * Set the hook to be called to dump additional edges to a block.
 * @param func The hook to be called.
 */
void set_dump_block_edge_hook(DUMP_NODE_EDGE_FUNC func);

/**
 * Get the additional block edge dump hook.
 * @return The current additional block edge dump hook.
 */
DUMP_NODE_EDGE_FUNC get_dump_block_edge_hook(void);

/** Dump a firm graph.
 *
 *  @param irg     The firm graph to be dumped.
 *  @param suffix  A suffix for the file name.
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
 *  @param irg     The firm graph to be dumped.
 *  @param suffix  A suffix for the file name.
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

/** Dump a firm graph without explicit block nodes but grouped in extended blocks.
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
void dump_ir_extblock_graph (ir_graph *irg, const char *suffix);

/** Dumps all graphs in interprocedural view to a file named All_graphs<suffix>.vcg.
 *
 * @param suffix  A suffix for the file name.
 */
void dump_all_cg_block_graph(const char *suffix);

/** Dumps a firm graph and  all the type information needed for Calls,
 *  Sels, ... in this graph.
 *
 *  @param irg     The firm graph to be dumped with its type information.
 *  @param suffix  A suffix for the file name.
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
 *  @param irg     The firm graph to be dumped with its type information.
 *  @param suffix  A suffix for the file name.
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

/** The type of a dump function that is called for each graph.
 *
 *  @param irg     current visited graph
 *  @param suffix  A suffix for the file name.
 */
typedef void dump_graph_func(ir_graph *irg, const char *suffix);

/**
 * A walker that calls a dumper for each graph.
 *
 * @param dump_graph    The dumper to be used for dumping.
 * @param suffix        A suffix for the file name.
 *
 * @return
 *      Whatever the dumper creates.
 *
 *  Walks over all firm graphs and  calls a dumper for each graph.
 *  The following dumpers can be passed as arguments:
 *   - dump_ir_graph()
 *   - dump_ir_block_graph()
 *   - dump_cfg()
 *   - dump_type_graph()
 *   - dump_ir_graph_w_types()
 *
 * @see turn_off_edge_labels()
 */
void dump_all_ir_graphs (dump_graph_func *dump_graph, const char *suffix);


/**
 * Dump the control flow graph of a procedure.
 *
 * @param irg     The firm graph whose CFG shall be dumped.
 * @param suffix  A suffix for the file name.
 *
 * @return
 *      A file containing the CFG in vcg format.
 *
 * Dumps the control flow graph of a procedure in standard xvcg format.
 * Dumps the graph to a file.  The file name is constructed from the
 * name of the entity describing the procedure (irg->entity) and the
 * ending -cfg.vcg.  Eventually overwrites existing files.
 *
 * @see turn_off_edge_labels()
 */
void dump_cfg (ir_graph *irg, const char *suffix);

/**
 * Dump a node and its predecessors forming a subgraph to a vcg file.
 *
 * @param root   The node serving as root for the subgraph.
 * @param depth  Dump nodes on paths starting at root with length depth.
 * @param suffix A suffix for the file name.
 *
 * Dumps the graph to a file.  The file name is constructed from the
 * name of the entity describing the procedure the passed node is
 * in, suffix and the ending -subg_<nr>.vcg.  nr is a unique number
 * for each graph dumped. Eventually overwrites existing files.
 *
 * @return
 *      A file containing the subgraph in vcg format.
 */
void dump_subgraph (ir_node *root, int depth, const char *suffix);

/* **************************************************************************** */
/*                              CALLGRAPH DUMPERS                               */
/* **************************************************************************** */


/** Dump the call graph.
 *
 * Dumps the callgraph to a file "Callgraph"<suffix>".vcg".
 *
 * @param suffix A suffix for the file name.
 *
 * @see dump_callgraph_loop_tree(const char *suffix)
 */
void dump_callgraph(const char *suffix);

/* **************************************************************************** */
/*                              TYPEGRAPH DUMPERS                               */
/* **************************************************************************** */

/**
 * Dumps all the type information needed for Calls, Sels, ... in this graph.
 * Does not dump the graph!
 *
 * @param irg    The firm graph whose type information is to be dumped.
 * @param suffix A suffix for the file name.
 *
 * @return
 *      A file containing the type information of the firm graph in vcg format.
 *
 *  Dumps this graph to a file.  The file name is constructed from the
 *  name of the entity describing the procedure (irg->entity) and the
 *  ending -type.vcg.  Eventually overwrites existing files.
 *
 * @see turn_off_edge_labels()
 */
void dump_type_graph (ir_graph *irg, const char *suffix);

/**
 * Dumps all type information.
 *
 * @param suffix A suffix for the file name.
 *
 * @return
 *      A file containing all type information for the program in standard
 *      vcg format.
 *
 * Dumps all type information that is somehow reachable in standard vcg
 * format.
 * Dumps the graph to a file named All_types.vcg.
 *
 * @see turn_off_edge_labels()
 */
void dump_all_types (const char *suffix);

/**
 * Dumps the class hierarchy with or without entities.
 *
 * @param entities    Flag whether to dump the entities.
 * @param suffix      A suffix for the file name.
 *
 * @return
 *      A file containing the class hierarchy tree for the program in standard
 *      vcg format.
 *
 * Does not dump the global type.
 * Dumps a node for all classes and the sub/supertype relations.  If
 * entities is set to true also dumps the entities of classes, but without
 * any additional information as the entities type.  The overwrites relation
 * is dumped along with the entities.
 * Dumps to a file class_hierarchy.vcg
 */
void dump_class_hierarchy (int entities, const char *suffix);

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
 * @param irg     Dump the loop tree for this graph.
 * @param suffix  A suffix for the file name.
 */
void dump_loop_tree(ir_graph *irg, const char *suffix);

/** Dumps the firm nodes in the sub-loop-tree of loop to a graph.
 *
 * Dumps the loop nodes if dump_loop_information() is set.
 * The name of the file is loop_<loop_nr><suffix>.vcg.
 *
 * @param l       Dump the loop tree for this loop.
 * @param suffix  A suffix for the file name.
 */
void dump_loop (ir_loop *l, const char *suffix);

/** Dumps the loop tree over the call graph.
 *
 * See for yourself what you can use this for.
 * The filename is "Callgraph_looptree<suffix>.vcg".
 *
 * @param suffix  A suffix for the file name.
 */
void dump_callgraph_loop_tree(const char *suffix);


/* **************************************************************************** */
/*                                TEXT DUMPERS                                  */
/* **************************************************************************** */


/** Write the irnode and all its attributes to the file passed.
 * */
int dump_irnode_to_file (FILE *f, ir_node *n);

/** Write the irnode and all its attributes to stdout.
 *  */
void dump_irnode (ir_node *n);

/** Write the graph and all its attributes to the file passed.
 *  Does not write the nodes.
 * */
void dump_graph_to_file(FILE *F, ir_graph *irg);

/** Write the graph and all its attributes to stdout.
 *  Does not write the nodes.
 *  */
void dump_graph(ir_graph *g);


/** Dump graph information as text.
 *
 *  Often graphs are unhandy in their vcg representation.  The text
 *  dumper represents the information for the firm nodes more compact,
 *  but the relations between the nodes only implicitly.
 *
 *  The file name is the graph name (get_entity_name()), appended by
 *  <suffix>.txt.
 */
void dump_graph_as_text(ir_graph *irg, const char *suffix);


/** Verbosity for text dumpers */
typedef enum {
  dump_verbosity_onlynames         = 0x00000001,   /**< only dump names. turns off all other
                                                        flags up to 0x00010000. */
  dump_verbosity_fields            = 0x00000002,   /**< dump types and fields (like a type declaration) */
  dump_verbosity_methods           = 0x00000004,   /**< dump types and methods (like a type declaration) */
  dump_verbosity_nostatic          = 0x00000040,   /**< dump types and dynamic allocated fields (like a
                                                        type declaration). This excludes methods and
                                                        static, polymorphic fields. */
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

  dump_verbosity_max                = 0x4FF00FBE   /**< turn on all verbosity.
                                                        Do not turn on negative flags!
                                                        @@@ Because of a bug in gcc 3.2 we can not set the
                                                        first two bits. */
} dump_verbosity;


/** Write the entity and all its attributes to the passed file.
 *  */
void    dump_entity_to_file (FILE *F, ir_entity *ent, unsigned verbosity);

/** Write the entity and all its attributes to the stdout.
 *
 *  Calls dump_entity_to_file().  */
void    dump_entity (ir_entity *ent);

/** Write the type and all its attributes to the file passed.
 * */
void    dump_type_to_file (FILE *f, ir_type *tp, dump_verbosity verbosity);

/** Write the type and all its attributes to stdout.
 *  */
void    dump_type (ir_type *tp);


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

/** Dumps all global variables as text.
 *
 * @param suffix  A suffix for the file name.
 *
 * Dumps a text representation of the entities in the global type.
 *
 * The file name is the program name (get_irp_name()), or 'TextTypes'
 * if the program name is not set, appended by <suffix>-globals.txt.
 * For verbosity see the documentation of the verbosity flags above.
 */
void dump_globals_as_text(unsigned verbosity, const char *suffix);

/* **************************************************************************** */
/*                                    FLAGS                                     */
/* **************************************************************************** */

/** Set a prefix filter for output functions.
 *
 * All graph dumpers check this name.  If the name is != "" and
 * not a prefix of the graph to be dumped, the dumper does not
 * dump the graph.
 *
 * @param name The prefix of the name (not the ld_name) of the method
 *              entity to be dumped.
 */
void   only_dump_method_with_name(ident *name);

/** Returns the prefix filter set with only_dump_method_with_name(). */
ident *get_dump_file_filter_ident(void);

/** Returns true if dump file filter is not set, or if it is a
 *  prefix of name. */
int is_filtered_dump_name(ident *name);

/** Sets the vcg flag "display_edge_labels" to no.
 *
 * This is necessary as xvcg and aisee both fail to display graphs
 * with self-edges if these edges have labels.
 */
void turn_off_edge_labels(void);

/**
 * If set to non-zero constants will be replicated for every use. In non
 * blocked view edges from constant to block are skipped.  Vcg then
 * layouts the graphs more compact, this makes them better readable.
 * The flag is automatically and temporarily set to false if other
 * edges are dumped, as outs, loop, ...
 * Default setting: false.
 */
void dump_consts_local(int flag);

/**
 * if set to non-zero node idx will be added to node labels
 */
void dump_node_idx_label(int flag);

/**
 * Returns 0 if dump_out_edge_flag or dump_loop_information_flag
 * are set, else returns dump_const_local_flag.
 */
int get_opt_dump_const_local(void);

/**  Turns off dumping the values of constant entities. Makes type graphs
 *   better readable.
 */
void dump_constant_entity_values(int flag);

/**  Turns on dumping the edges from the End node to nodes to be kept
 *   alive.
 */
void dump_keepalive_edges(int flag);
int get_opt_dump_keepalive_edges(void);

/** Turns on dumping the out edges starting from the Start block in
 *  dump_ir_graph.
 *
 *  To test the consistency of the out data structure.
 */
void dump_out_edges(int flag);

/** If this flag is set the dumper dumps edges to immediate dominator in cfg.
 */
void dump_dominator_information(int flag);

/** If this flag is set the dumper dumps loop nodes and edges from
 *  these nodes to the contained ir nodes.
 *
 *  If the loops are interprocedural nodes can be missing.
 */
void dump_loop_information(int flag);

/** If set and backedge info is computed, backedges are dumped dashed
 *  and as vcg 'backedge' construct.
 *
 *  Default: set.
 */
void dump_backedge_information(int flag);

/** Dump the information of type field specified in ana/irtypeinfo.h.
 *
 *  If the flag is set, the type name is output in [] in the node label,
 *  else it is output as info.
 */
void set_opt_dump_analysed_type_info(int flag);

/** Write the address of a node into the vcg info.
 *
 *  This is off per default for automatic comparisons of
 *  vcg graphs -- these will differ in the pointer values!
 */
void dump_pointer_values_to_info(int flag);

/** Dumps ld_names of entities instead of there names.
 *
 * This option is on per default.
 */
void dump_ld_names(int flag);

/** Dumps all graph anchor nodes, even if they
 * are dead.
 *
 * This option is off per default.
 */
void dump_all_anchors(int flag);

/** A node info dumper callback. */
typedef void (dump_node_info_cb_t)(void *data, FILE *f, const ir_node *n);

/**
 * Adds a new node info dumper callback. It is possible to add an unlimited
 * number of callbacks. The callbacks are called at the end of the default
 * info dumper.
 *
 * @param cb    the callback function to be called
 * @param data  a context parameter
 *
 * @return A callback handle.
 *
 * @note This functionality is only available, if Firm hooks are enabled (default).
 */
void *dump_add_node_info_callback(dump_node_info_cb_t *cb, void *data);

/**
 * Remove a previously added info dumper callback.
 *
 * @param handle  the callback handle returned from dump_add_node_info_callback()
 */
void dump_remv_node_info_callback(void *handle);

#endif
