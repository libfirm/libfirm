/*
 * Project:     libFIRM
 * File name:   ir/common/firmwalk.h
 * Purpose:     Walker that touches all Firm data structures
 * Author:      Sebastian Felis
 * Modified by:
 * Created:     7.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 *  @file firmwalk.h
 *
 *  Firm walker over intermediate representation.
 *
 *  To initialize the walker, call firm_walk_init(). This function
 *  collects all specific data from the firm represenation. After
 *  building the walker information firm_walk() could be called
 *  serveral times with different flags (options) from specific walker
 *  or dumper. At least firm_walk_finalizer() should be called to free
 *  the stored data.
 *
 *  This walker could be used for a dumper e.g. a vcg or xml dumper.
 *
 *  @note If a specific walker or dumper which uses the link field
 *        of any firm node, the the wrapper functions set_firm_walk_link()
 *        and get_firm_walk_link() should be used, because the firm walker
 *        make use of the link field to store its own data.
 */
#ifndef _FIRM_WALK_H_
#define _FIRM_WALK_H_

#include "type.h"
#include "irgraph.h"
#include "typewalk.h"

/** Returns the link of a firm node.
 *  Possible firm structures are: entity, type, ir_graph, ir_node and
 *  ir_mode. Otherwise this function has no effect
 *
 *  Derived walker or dumper have to call this function to store data
 *  to a firm structure. The real link field of firm structure is used
 *  by this firm walker to collect walking data.
 *
 *  @param thing Pointer to a firm structure
 *  @retrun Link pointer
 *
 *  @note After calling firm_walk_finalize() the stored link
 *        information may be invalid. */
void *get_firm_walk_link(void *thing);

/** Set the link field of a firm structure.
 *  Possible firm structures are: entity, type, ir_graph, ir_node and
 *  ir_mode. Otherwise this function has no effect
 *
 *  Derived walker or dumper have to call this function to store data
 *  to a firm structure. The real link field of firm structure is used
 *  by this firm walker to collect walking data.
 *
 *  @param thing firm structur
 *  @param link Pointer to link field
 *
 *  @note After calling firm_walk_finalize() the stored link
 *        information may be invalid. */
void set_firm_walk_link(void *thing, void *link);

/** Initialisation function for firm walker callbacks */
typedef void firm_walk_init_func(void *env);
/** Finalisation function for firm walker callbacks */
typedef void firm_walk_finalize_func(void *env);

/** Mode callback function definition */
typedef void firm_walk_mode_func(ir_mode *mode, void *env);
/** Type callback function definition */
typedef void firm_walk_type_func(type *tp, void *env);
/** Entity callback function definition */
typedef void firm_walk_entity_func(entity *ent, void *env);
/** Graph callback function definition */
typedef void firm_walk_graph_func(ir_graph *irg, void *env);
//@{
/** Block callback function definition */
typedef void firm_walk_block_init_func(ir_graph *irg, void *env);
typedef void firm_walk_block_func(ir_node *block, void *env);
typedef void firm_walk_block_finalize_func(ir_graph *irg, void *env);
//@}
/** Node callback function definition */
typedef void firm_walk_node_func (ir_node *irn, void *env);

/** @enum firm_walk_flags
 *
 *  Flags for the firm walker to modify some dumping behavior
 */
typedef enum
{
  FW_WITH_ALL_TYPES     = 1<<0, /**< Collect and dump all types, especially
                                      unused types.
                                      @note This flag could be set in
                                      firm_dumper_init() and is unused in
                                      firm_dump() */
  FW_WITH_DOMINATOR     = 1<<1, /**< nyi */
  FW_WITH_OUTEDGES      = 1<<2, /**< nyi */
  FW_WITH_LOOPS         = 1<<3, /**< nyi */
  FW_DUMP_BLOCK_AS_IRN  = 1<<4, /**< Dump all block nodes as irn nodes
                                      additionally */
  FW_DUMP_IRN_IN_PREFIX = 1<<5  /**< Dumps all ir nodes in prefix order
                                      according to the internal firm graph
                                      structure */
} firm_walk_flags;

/** Interface of the firm walker */
typedef struct
{
  //@{
  /** Interface function to dump all used and internal modes.
      Internal modes are: BB, X, M and T */
  firm_walk_init_func *do_mode_init;
  firm_walk_mode_func *do_mode;
  firm_walk_finalize_func *do_mode_finalize;
  //@}

  //@{
  /** Interface to dump all collected types.
   *
   *  @node To dump all (not only used types by default) a special walk
   *        flag must be set for the walker initializer */
  firm_walk_init_func *do_type_init;
  firm_walk_type_func *do_type;
  firm_walk_finalize_func *do_type_finalize;
  //@}

  //@{
  /** Dumping interface for entities */
  firm_walk_init_func *do_entity_init;
  firm_walk_entity_func *do_entity;
  firm_walk_finalize_func *do_entity_finalize;
  //@}

  /** Dumps all graphs and subnodes.
   *
   *  The firm walker dump a graph with its blocks and nodes nested.
   *  Fist do_graph_init will be called (if defined). For each graph
   *  do_graph will be call in a loop. After dumped all graphs,
   *  do_graph_finalize will be called.
   *
   *  Within do_graph each block will be dumped. First do_block_init,
   *  for each block do_block and after all dumped blocks
   *  do_block_finalize.
   *
   *  The ir nodes are dumped nested in their blocks as well. Within
   *  do_block, for each ir node do_node is called in postfix order
   *  according to the internal firm representation. By changing the
   *  walking flag, a prefix order is also possible. */
  firm_walk_init_func *do_graph_init;
  firm_walk_graph_func *do_graph;
  firm_walk_finalize_func *do_graph_finalize;

  //@{
  /** Dumping interface for blocks. If blocks should be handled like
   *  like a normal ir node, a special walker flag could be set.
   *  @see do_graph */
  firm_walk_block_init_func *do_block_init;
  firm_walk_block_func *do_block;
  firm_walk_block_finalize_func *do_block_finalize;
  //@}

  /** Dumping interface for ir nodes
   *  @see do_graph */
  firm_walk_node_func *do_node;
  /* dominator */
  /* procedures */
  /* loop */
  firm_walk_flags flags;
  /* pointer to environment of interface */
  void *env;
} firm_walk_interface;


/** Initialize the dumper und collect all data from the firm intermediate
 *  representation
 *
 *  @param flags flags */
void firm_walk_init(firm_walk_flags flags);

/** Walker of the firm intermediate representation.
 *
 *  The callback functions of the interface will be called nested, e.g. for
 *  each block: init function of block, do_block, nested function of nodes,
 *  finalize function of block.
 *
 *  - modes
 *  - types
 *  - entities
 *  - ir graphs
 *    - procedures
 *    - blocks
 *    - nodes. Options: dominator, outedges
 *
 *  @param wif Stucture of walker interface. In this struct the used callback
 *             functions are defined.
 */
void firm_walk(firm_walk_interface *wif);

/** Finalize the walker and frees all stored data for dumping */
void firm_walk_finalize(void);

#endif /* _FIRM_WALK_H_ */
