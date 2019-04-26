/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Write vcg representation of firm to file.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Hubert Schmidt
 * @brief   Dump routines for the ir graph and all type information.
 *
 */
#ifndef FIRM_IR_IRDUMP_H
#define FIRM_IR_IRDUMP_H

#include <stdio.h>
#include "firm_types.h"

#include "begin.h"

/**
 * @ingroup printing
 * @defgroup ir_dump Visualisation
 *
 * Dumps information so it can be visualised. The dump format of most functions
 * is vcg.  This is a text based graph representation. Some use the original
 * format, but most generate an extended format that is only read by some
 * special versions of xvcg.
 *
 * We have developed an own advanced viewer called ycomp:
 *   http://www.info.uni-karlsruhe.de/software/ycomp/
 *@{
 */

/** @defgroup convenience Convenience Interface
 * @{
 */

/**
 * Convenience interface for dumping a graph as vcg file.
 *
 * For details on how the filename is constructed see #dump_ir_graph_ext
 */
FIRM_API void dump_ir_graph(ir_graph *graph, const char *suffix);

/**
 * type for dumpers that dump information about the whole program
 */
typedef void (*ir_prog_dump_func)(FILE *out);

/**
 * Convenience interface for dumping the whole compilation-unit/program.
 *
 * The filename is constructed by combining a counter, the name of the current
 * ir_prog and the given @p suffix. The file-extensions is determined by looking
 * at @p mime_type.
 * The file is stored into the directory specified by #ir_set_dump_path
 *
 * @param func       Dumper. Usually one of #dump_callgraph, #dump_typegraph,
 *                   #dump_class_hierarchy, #dump_types_as_text,
 *                   #dump_globals_as_text
 * @param suffix     Suffix to append to the name
 */
FIRM_API void dump_ir_prog_ext(ir_prog_dump_func func, const char *suffix);

/**
 * type for graph dumpers
 */
typedef void (*ir_graph_dump_func)(FILE *out, ir_graph *graph);

/**
 * Convenience interface for dumping graphs.
 * The filename is constructed by combining a counter, the name of the graphs
 * entity and the given @p suffix. The file-extensions is determined by looking
 * at @p mime_type.
 * The file is stored into the directory specified by #ir_set_dump_path
 *
 * @param func      Dumper. Usually one of #dump_cfg, #dump_loop_tree,
 *                  #dump_ir_graph_file
 * @param graph     the graph to dump
 * @param suffix    suffix
 */
FIRM_API void dump_ir_graph_ext(ir_graph_dump_func func, ir_graph *graph,
                                const char *suffix);

/**
 * A walker that calls a dumper for each graph in the program
 *
 * @param suffix        A suffix for the file name.
 */
FIRM_API void dump_all_ir_graphs(const char *suffix);

/**
 * Specifies output path for the dump_ir_graph function
 */
FIRM_API void ir_set_dump_path(const char *path);

/**
 * Sets a prefix filter for output functions.
 *
 * All graph dumpers check this name.  If the name is != "" and
 * not a substring of the graph to be dumped, the dumper does not
 * dump the graph.
 *
 * @param name The prefix of the name of the method entity to be dumped.
 */
FIRM_API void ir_set_dump_filter(const char *name);

/** Returns the substring filter set with #ir_set_dump_filter */
FIRM_API const char *ir_get_dump_filter(void);

/*@}*/

/**
 * Dumps all Firm nodes of a single graph for a single procedure in
 * standard xvcg format.
 *
 * @param graph  The firm graph to be dumped.
 * @param out    Output stream the graph is written to
 */
FIRM_API void dump_ir_graph_file(FILE *out, ir_graph *graph);

/**
 * Dump the control flow graph of a procedure.
 *
 * @param graph   The firm graph whose CFG shall be dumped.
 * @param out     Output stream the CFG is written to
 *
 * Dumps the control flow graph of a procedure in standard xvcg format.
 */
FIRM_API void dump_cfg(FILE *out, ir_graph *graph);

/**
 * Dump the call graph.
 *
 * @param out    Output stream the callgraph is written to
 */
FIRM_API void dump_callgraph(FILE *out);

/**
 * Dumps all type information.
 *
 * @param out     Output stream the typegraph is written to
 *
 * Dumps all type information that is somehow reachable in standard vcg
 * format.
 */
FIRM_API void dump_typegraph(FILE *out);

/**
 * Dumps the class hierarchy with or without entities.
 *
 * @param out         Output stream
 *
 * Does not dump the global type.
 * Dumps a node for all classes and the sub/supertype relations.  If
 * entities is set to true also dumps the entities of classes, but without
 * any additional information as the entities type.  The overwrites relation
 * is dumped along with the entities.
 */
FIRM_API void dump_class_hierarchy(FILE *out);

/**
 * Dump a standalone loop tree, which contains the loop nodes and the firm nodes
 * belonging to one loop packed together in one subgraph.
 *
 * @param out     Output stream
 * @param graph   Dump the loop tree for this graph.
 */
FIRM_API void dump_loop_tree(FILE *out, ir_graph *graph);

/**
 * Dumps the loop tree over the call graph.
 *
 * @param out   Output stream
 */
FIRM_API void dump_callgraph_loop_tree(FILE *out);

/**
 * Dump type information as text.
 *
 * Often type graphs are unhandy in their vcg representation.  The text dumper
 * represents the information for a single type more compact, but the relations
 * between the types only implicitly. Dumps only 'real' types, i.e., those in
 * the type list.  Does not dump the global type nor frame types or the like.
 */
FIRM_API void dump_types_as_text(FILE *out);

/**
 * Dumps all global variables as text.
 *
 * @param out         Output stream
 *
 * Dumps a text representation of the entities in the global type.
 */
FIRM_API void dump_globals_as_text(FILE *out);

/**
 * Dumps the firm nodes in the sub-loop-tree of loop to a vcg file.
 *
 * @param out     Output stream
 * @param loop    Dump the loop tree for this loop.
 */
FIRM_API void dump_loop(FILE *out, ir_loop *loop);

/** Write the graph and all its attributes to the file passed.
 *  Does not write the nodes. */
FIRM_API void dump_graph_as_text(FILE *out, const ir_graph *graph);

/** Write the entity and all its attributes to the passed file. */
FIRM_API void dump_entity_to_file(FILE *out, const ir_entity *entity);

/** Write the type and all its attributes to the file passed. */
FIRM_API void dump_type_to_file(FILE *out, const ir_type *type);

/** Verbosity for text dumpers */
typedef enum {
	dump_verbosity_onlynames         = 0x00000001,   /**< Only dump names. Turns off all other
	                                                      flags up to 0x00010000. */
	dump_verbosity_fields            = 0x00000002,   /**< Dump types and fields (like a type declaration). */
	dump_verbosity_methods           = 0x00000004,   /**< Dump types and methods (like a type declaration). */
	dump_verbosity_nostatic          = 0x00000040,   /**< Dump types and dynamic allocated fields (like a
	                                                      type declaration). This excludes methods and
	                                                      static, polymorphic fields. */
	dump_verbosity_typeattrs         = 0x00000008,   /**< Dump all type attributes. */
	dump_verbosity_entattrs          = 0x00000010,   /**< Dump all entity attributes. */
	dump_verbosity_entconsts         = 0x00000020,   /**< Dump entity constants. */

	dump_verbosity_accessStats       = 0x00000100,   /**< Dump entity access statistics. */

	dump_verbosity_max                = 0x4FF00FBE   /**< Turn everything on */
} ir_dump_verbosity_t;
ENUM_BITSET(ir_dump_verbosity_t)

/** override currently set text dump flags with new ones */
FIRM_API void ir_set_dump_verbosity(ir_dump_verbosity_t verbosity);
/** return currently set text dump flags */
FIRM_API ir_dump_verbosity_t ir_get_dump_verbosity(void);

/**
 * A bitset indicating various options that affect what information is dumped
 * and how exactly it is dumped. This affects the dumpers that produce vcg
 * graphs.
 */
typedef enum {
	/** dump basic blocks as subgraphs which contain the nodes in the block */
	ir_dump_flag_blocks_as_subgraphs   = 1U <<  0,
	/** dump (parts of) typegraph along with nodes */
	ir_dump_flag_with_typegraph        = 1U <<  1,
	/** Sets the vcg flag "display_edge_labels" to no.
	 * This is necessary as xvcg fails to display graphs
	 * with self-edges if these edges have labels. */
	ir_dump_flag_disable_edge_labels   = 1U <<  2,
	/** If set constants will be replicated for every use. In non blocked view
	 * edges from constant to block are skipped.  Vcg then layouts the graphs
	 * more compact, this makes them better readable. */
	ir_dump_flag_consts_local          = 1U <<  3,
	/** if set node idx will be added to node labels */
	ir_dump_flag_idx_label             = 1U <<  4,
	/** if set node number will be added to node labels */
	ir_dump_flag_number_label          = 1U <<  5,
	/** show keepalive edges from the end node */
	ir_dump_flag_keepalive_edges       = 1U <<  6,
	/** dump out edges */
	ir_dump_flag_out_edges             = 1U <<  7,
	/** if set dumps edges from blocks to their immediate dominator */
	ir_dump_flag_dominance             = 1U <<  8,
	/** If set the dumper dumps loop nodes and edges from these nodes to the
	 * contained ir nodes. */
	ir_dump_flag_loops                 = 1U <<  9,
	/** if set (and backedge info is computed) dump backedges */
	ir_dump_flag_back_edges            = 1U << 10,
	/** dump backedges from iredges.h */
	ir_dump_flag_iredges               = 1U << 11,
	/** dump all anchor nodes, even the unused ones */
	ir_dump_flag_all_anchors           = 1U << 12,
	/** dumps marked blocks with an asterisk in the label */
	ir_dump_flag_show_marks            = 1U << 13,

	/** turns of dumping of constant entity values in typegraphs */
	ir_dump_flag_no_entity_values      = 1U << 14,
	/** dumps ld_names of entities instead of their names */
	ir_dump_flag_ld_names              = 1U << 15,
	/** dump entities in class hierarchies */
	ir_dump_flag_entities_in_hierarchy = 1U << 16,
} ir_dump_flags_t;
ENUM_BITSET(ir_dump_flags_t)

/** override currently set dump flags with new ones */
FIRM_API void ir_set_dump_flags(ir_dump_flags_t flags);
/** add flags to the currently set dump flags */
FIRM_API void ir_add_dump_flags(ir_dump_flags_t flags);
/** disable certain dump flags */
FIRM_API void ir_remove_dump_flags(ir_dump_flags_t flags);
/** return currently set dump flags */
FIRM_API ir_dump_flags_t ir_get_dump_flags(void);

/**
 * This hook is called to dump the vcg attributes of a node to a file.
 * If this function returns zero, the default attributes are added, else
 * removed.
 */
typedef int (*dump_node_vcgattr_func)(FILE *out, const ir_node *node, const ir_node *local);

/**
 * This hook is called to dump the vcg attributes of an edge to a file.
 * If this function returns zero, the default attributes are added, else
 * removed.
 */
typedef int (*dump_edge_vcgattr_func)(FILE *out, const ir_node *node, int to);

/**
 * This hook allows dumping of additional edges (it is called outside a node: {}
 * environment)
 */
typedef void (*dump_node_edge_func)(FILE *out, const ir_node *node);

/** Sets the node_vcgattr hook. */
FIRM_API void set_dump_node_vcgattr_hook(dump_node_vcgattr_func hook);
/** Sets the edge_vcgattr hook. */
FIRM_API void set_dump_edge_vcgattr_hook(dump_edge_vcgattr_func hook);

/**
 * Sets the hook to be called to dump additional edges to a node.
 * @param func The hook to be called.
 */
FIRM_API void set_dump_node_edge_hook(dump_node_edge_func func);

/**
 * Returns the additional edge dump hook.
 * @return The current additional edge dump hook.]
 */
FIRM_API dump_node_edge_func get_dump_node_edge_hook(void);

/**
 * Sets the hook to be called to dump additional edges to a block.
 * @param func The hook to be called.
 */
FIRM_API void set_dump_block_edge_hook(dump_node_edge_func func);

/**
 * Returns the additional block edge dump hook.
 * @return The current additional block edge dump hook.
 */
FIRM_API dump_node_edge_func get_dump_block_edge_hook(void);

/** A node info dumper callback. */
typedef void (dump_node_info_cb_t)(void *data, FILE *out, const ir_node *n);

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
 * @note This functionality is only available, if Firm hooks are enabled.
 */
FIRM_API hook_entry_t *dump_add_node_info_callback(dump_node_info_cb_t *cb, void *data);

/**
 * Removes a previously added info dumper callback.
 *
 * @param handle  the callback handle returned from
 *                dump_add_node_info_callback()
 */
FIRM_API void dump_remove_node_info_callback(hook_entry_t *handle);

/** @defgroup lowlevel Low Level Interface
 * This interface exists to enable some custom vcg formatting/slicing
 * used to produce example images for the homepage.
 * @{
 */

/**
 * Prints the typical header used in libfirm .vcg files to file \p out.
 */
void dump_vcg_header(FILE *out, const char *name, const char *layout,
                     const char *orientation);

/**
 * Prints the typical footer used in libfirm .vcg files to file \p out.
 */
void dump_vcg_footer(FILE *out);

/**
 * Prints vcg node for a single firm graph node \p node to \p out.
 */
void dump_node(FILE *out, const ir_node *node);

/**
 * Prints vcg edges for firm graph node \p node to \p out.
 */
void dump_ir_data_edges(FILE *out, const ir_node *node);

/**
 * Prints the vcg identifier for firm graph node \p node to \p out.
 */
void print_nodeid(FILE *out, const ir_node *node);

/**
 * Prints the commands to begin of a new vcg subgraph intended for firm block
 * \p block to \p out.
 */
void dump_begin_block_subgraph(FILE *out, const ir_node *block);

/**
 * Prints the commands to end a vcg subgraph of firm block \p block to \p out.
 */
void dump_end_block_subgraph(FILE *out, const ir_node *block);

/**
 * Prints vcg edges for predecessors of firm block \p block to \p out.
 */
void dump_block_edges(FILE *out, const ir_node *block);

/**
 * Prints vcg nodes+edges for all node in firm graph \p irg to \p out.
 * Block nodes are turned into subgraphs containing all nodes using them as a
 * parent.
 */
void dump_blocks_as_subgraphs(FILE *out, ir_graph *irg);

/*@}*/

/*@}*/

#include "end.h"

#endif
