/*
 * Project:     libFIRM
 * File name:   ir/common/firmwalk.c
 * Purpose:     Walker that touches all Firm data structures
 * Author:      Sebastian Felis
 * Modified by:
 * Created:     7.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "firmwalk.h"
#include "pmap.h"
#include "entity.h"
#include "irnode_t.h"
#include "irprog.h"
#include "irgwalk.h"
#include "array.h"
#include "obst.h"
#include <string.h>

/** obstack for firm walker */
static struct obstack fw_obst;

/** This map stores all types of firm */
static pmap *mode_map = NULL;
/** This map stores all types of firm */
static pmap *type_map = NULL;
/** This map stores all entities of firm */
static pmap *entity_map = NULL;

/** Internal data structure of firm walker to collect
 *  some information of firm ir. The collected data will be stored
 *  into the link field of ir node. All graphs have a list of its
 *  blocks and all blocks have a list of their nodes. */
typedef struct {
  ir_node **list; /**< List of blocks or nodes */
  void *link;     /**< Public link field. The public link field of firm nodes
                       is wrapped by set_firm_walk_link and
                       get_firm_walk_link. */
} fw_data;

//@{
/** Access macros to fw_data structure */
#define FW_GET_DATA_LIST(s)     ((s)->list)
#define FW_SET_DATA_LIST(s, t)  ((s)->list = (t))
#define FW_GET_DATA_LINK(s)     ((s)->link)
#define FW_SET_DATA_LINK(s, t)  ((s)->link = (t))
//@}

/** Returns own data struct of the firm walker.
 *
 *  If no structure defined (firm link is NULL) allocate a new
 *  struct to the fgd obstack. Only ir graph and block nodes
 *  will allocate this structure.
 *  - ir graph collect its block
 *  - block collect its nodes
 */
static
fw_data *fw_get_data(void *thing)
{
  fw_data *data = NULL;

  assert(thing);
  switch (get_kind(thing)) {
  case k_ir_graph:
    data = get_irg_link(thing);
    /* init block list of graph */
    if (NULL == data)
    {
      /* allocate new firm walk structure */
      data = obstack_alloc(&fw_obst, sizeof(fw_data));
      memset(data, 0, sizeof(fw_data));
      set_irg_link(thing, data);
      /* allocate block list */
      FW_GET_DATA_LIST(data) = NEW_ARR_F(ir_node *, 0);
    }
    break;
  case k_ir_node:
    /* init node list of block */
    if (is_Block(thing))
    {
      data = get_irn_link(thing);
      if (NULL == data)
      {
        /* allocate new firm walk structure */
        data = obstack_alloc(&fw_obst, sizeof(fw_data));
        memset(data, 0, sizeof(fw_data));
        set_irn_link(thing, data);
        /* allocate node list */
        FW_GET_DATA_LIST(data) = NEW_ARR_F(ir_node *, 0);
      }
    }
    break;
  default: {} // other kinds of firm nodes
  }

  return data;
}

/** Free all collected data in ir graphs and nodes.
 *  An ir graph or an ir block node has a list as a
 *  dynamic array, which will be deleted here.  */
static
void fw_free_data(void *thing)
{
  fw_data *data = NULL;

  assert(thing);

  switch (get_kind(thing)) {
  case k_ir_graph:
    data = get_irg_link(thing);
    /* delete block list of graph */
    if (NULL != data)
    {
      DEL_ARR_F(FW_GET_DATA_LIST(data));
      set_irg_link(thing, NULL);
    }
    break;
  case k_ir_node:
    /* delete node list of block */
    if (is_Block(thing))
    {
      data = get_irn_link(thing);
      if (NULL != data)
      {
        DEL_ARR_F(FW_GET_DATA_LIST(data));
        set_irn_link(thing, NULL);
      }
    }
    break;
  default: {} // other kinds of firm nodes
  }
}

// documentation in header file
void set_firm_walk_link(void *thing, void *link)
{
  fw_data *data;

  assert(thing);
  switch (get_kind(thing)) {
  case k_entity:
    set_entity_link(thing, link);
    break;
  case k_type:
    set_type_link(thing, link);
    break;
  case k_ir_graph:
    data = fw_get_data(thing);
    FW_SET_DATA_LINK(data, link);
    break;
  case k_ir_node:
    if (is_Block(thing))
    {
      data = fw_get_data(thing);
      FW_SET_DATA_LINK(data, link);
    }
    else
      set_irn_link(thing, link);
    break;
  case k_ir_mode:
    set_mode_link(thing, link);
    break;
  default: {} // other kinds of firm nodes
  }
}

// documentation in header file
void *get_firm_walk_link(void *thing)
{
  fw_data *data;
  assert(thing);
  switch (get_kind(thing)) {
  case k_entity:
    return get_entity_link(thing);
  case k_type:
    return get_type_link(thing);
  case k_ir_graph:
    data = fw_get_data(thing);
    return FW_GET_DATA_LINK(data);
  case k_ir_node:
    if (is_Block(thing))
    {
      data = fw_get_data(thing);
      return FW_GET_DATA_LINK(data);
    }
    else
      return get_irn_link(thing);
  case k_ir_mode:
    return get_mode_link(thing);
  default:
    return NULL;
  }
}

/** Set link field of a ir node to NULL */
static
void fw_clear_link(ir_node * node, void * env)
{
  set_irn_link(node, NULL);
}

/** Fill maps of type and entity.
 *  This function will be called by the firm walk initializer
 *  to collect all types and entities of program's firm ir.
 *  All types will be colleced in the hash table type_map
 *  and all entity are stored in entity_map. The mode of an
 *  type will be collected as well.
 *
 *  @param tore Type or entity
 *  @param env Environment pointer (currently unused)
 */
static
void fw_collect_tore(type_or_ent *tore, void *env)
{
  ir_mode *mode;
  type *tp;
  entity *ent;

  switch (get_kind(tore)) {
  case k_entity:
    ent = (entity *)tore;
    // append entity to list
    set_entity_link(ent, NULL);
    if (!pmap_contains(entity_map, ent))
      pmap_insert(entity_map, ent, env);
    break;
  case k_type:
    tp = (type *)tore;
    mode = get_type_mode(tp);
    // append type to list
    set_type_link(tp, NULL);
    if (!pmap_contains(type_map, tp))
      pmap_insert(type_map, tp, env);

    /* insert only modes (non atomic types, i.e. class, array or struct
       have no mode. The link field will be cleared in the walk_do_mode()
       callback function. */
    if ((NULL != mode) && (!pmap_contains(mode_map, mode)))
      pmap_insert(mode_map, mode, env);
    break;
  default: break;
  }
}

/** Collect all data from nodes. Block appends itself to
 *  the corresponding ir graph and other nodes appends itself
 *  to block list. Collects also the modes of each node to get
 *  non-type modes.
 *
 *  @param irn IR node pointer.
 *  @param env Environment pointer (currently unused)
 */
static
void fw_collect_irn(ir_node *irn, void *env)
{
  fw_data *data;
  ir_mode* mode = intern_get_irn_mode(irn);

  /* The link field will be cleared in the walk_do_mode()
    callback function. */
  if ((NULL != mode) && (!pmap_contains(mode_map, mode)))
    pmap_insert(mode_map, mode, env);

  /* block nodes. */
  if (is_Block(irn))
  {
    /* add this block to ir graph's block list */
    data = fw_get_data(get_current_ir_graph());
    ARR_APP1(ir_node *, FW_GET_DATA_LIST(data), irn);
  }
  /* non block nodes */
  else
  {
    /* add this node to block's node list */
    ir_node *block = get_nodes_Block(irn);
    data = fw_get_data(block);
    ARR_APP1(ir_node *, FW_GET_DATA_LIST(data), irn);
  }
}

/** Irg walker function to free all collected data from nodes */
static
void fw_free_colleted_data(ir_node *irn, void *env)
{
  /* Free node list from blocks */
  if (is_Block(irn))
  {
    fw_free_data(irn);
  }
}

/** Initialize all walking data.
 *
 *  Collect all specific data like types, entities, ir graphs, blocks, nodes
 *  from current firm structures.
 */
void firm_walk_init(firm_walk_flags flags)
{
  int i;

  /* init obstack */
  obstack_init(&fw_obst);

  /*  Init map of modes and lists of type and entity. If map or list
      allready exists, free it. */
  if (mode_map)
  {
    pmap_destroy(mode_map);
  }
  mode_map = pmap_create();

  if (type_map)
  {
    pmap_destroy(type_map);
  }
  type_map = pmap_create();

  if (entity_map)
  {
    pmap_destroy(entity_map);
  }
  entity_map = pmap_create();

  /* insert internal modes to mode hash. The link field will be cleared
     in the walk_do_mode() callback function.
     Other used modes are added by collecting types */

  /*
     ### RG: should be done by inspection the mode of all irn

     pmap_insert(mode_map, mode_BB, NULL);
     pmap_insert(mode_map, mode_T, NULL);
     pmap_insert(mode_map, mode_ANY, NULL);
     pmap_insert(mode_map, mode_BAD, NULL);
     pmap_insert(mode_map, mode_X, NULL);
     pmap_insert(mode_map, mode_M, NULL);
     pmap_insert(mode_map, mode_b, NULL);
  */

  // Collect all types (also unused types) if flag is set
  if (FW_WITH_ALL_TYPES & flags)
    type_walk(fw_collect_tore, NULL, NULL);

  // for each ir graph
  for (i = 0; i < get_irp_n_irgs(); i++)
  {
    ir_graph *irg = get_irp_irg(i);
    set_irg_link(irg, NULL);

    type_walk_irg(irg, fw_collect_tore, NULL, NULL);

    irg_walk_graph(irg, fw_clear_link, fw_collect_irn, NULL);
  }
}

/** This function should call after using the firm walker to free
 *  all collected data and frees the used obstack */
void firm_walk_finalize(void)
{
  int i;

  /* free all used maps and lists */
  pmap_destroy(mode_map);
  mode_map = NULL;
  pmap_destroy(type_map);
  type_map = NULL;
  pmap_destroy(entity_map);
  entity_map = NULL;

  // free all collected data from ir graphs and nodes
  for (i = 0; i < get_irp_n_irgs(); i++)
  {
    ir_graph *irg = get_irp_irg(i);
    fw_free_data(irg);
    irg_walk_graph(get_irp_irg(i), NULL, fw_free_colleted_data, NULL);
  }

  /* free obstack */
  obstack_free(&fw_obst, NULL);
}

/** Dumps the firm ir.
 *
 *  After initializing the firm walker by calling firm_walk_init()
 *  the firm structure could be accessed by definign the firm walk interface
 *  wif. This function could be called serveral times to customize the
 *  walk order or definitions.
 *
 *  @param wif Walk interface which contains the callback function
 *
 *  @see firm_walk_interface */
void firm_walk(firm_walk_interface *wif)
{
  int irg_i, block_i, block_list_len, irn_i, irn_list_len;
  pmap_entry *entry;
  fw_data *data;
  ir_node *block, **block_list, **irn_list;
  ir_graph *saved_irg = current_ir_graph;

  assert(wif && "firm_walk() in firmwalk.c: No walking interface defined!");

  /* walk over all modes */
  if (wif->do_mode_init) wif->do_mode_init(wif->env);
  if (wif->do_mode)
  {
    for (entry = pmap_first(mode_map); entry; entry = pmap_next(mode_map))
    {
      set_mode_link((ir_mode*)entry->key, NULL);
      wif->do_mode(entry->key, wif->env);
    }
  }
  if (wif->do_mode_finalize) wif->do_mode_finalize(wif->env);

  /* walk over all types */
  if (wif->do_type_init) wif->do_type_init(wif->env);
  if (wif->do_type)
  {
    for (entry = pmap_first(type_map); entry; entry = pmap_next(type_map))
      wif->do_type((type *)entry->key, wif->env);
  }
  if (wif->do_type_finalize) wif->do_type_finalize(wif->env);

  /* walk over all entities */
  if (wif->do_entity_init) wif->do_entity_init(wif->env);
  if (wif->do_entity)
  {
    for (entry = pmap_first(entity_map); entry; entry = pmap_next(entity_map))
      wif->do_entity((entity *)entry->key, wif->env);
  }
  if (wif->do_entity_finalize) wif->do_entity_finalize(wif->env);


  /* Dump graphs ================================================= */
  if (wif->do_graph_init) wif->do_graph_init(wif->env);

  for (irg_i = 0; irg_i < get_irp_n_irgs(); irg_i++)
  {
    current_ir_graph = get_irp_irg(irg_i);

    /* walk over all ir graph */
    if (wif->do_graph) wif->do_graph(current_ir_graph, wif->env);

    /* walk over all irg's block nested ========================== */
    data = fw_get_data(current_ir_graph);
    block_list = FW_GET_DATA_LIST(data);
    block_list_len = ARR_LEN(block_list);
    for (block_i = 0; block_i < block_list_len; block_i++)
    {
      if (wif->do_block_init) wif->do_block_init(current_ir_graph, wif->env);

      block = (ir_node *)block_list[block_i];
      if (wif->do_block) wif->do_block(block, wif->env);

      /* walk over all block's ir nodes nested =================== */
      data = fw_get_data(block);
      irn_list = FW_GET_DATA_LIST(data);
      irn_list_len = ARR_LEN(irn_list);

      // call block as prefix ir node
      if ((wif->do_node) &&
          (wif->flags & FW_DUMP_BLOCK_AS_IRN & !FW_DUMP_IRN_IN_PREFIX))
        wif->do_node(block, wif->env);

      // do ir nodes in prefix or postfix order?
      if (wif->flags & FW_DUMP_IRN_IN_PREFIX)
        irn_i = irn_list_len-1;
      else
        irn_i = 0;

      while (irn_i >= 0 && irn_i < irn_list_len)
      {
        if (wif->do_node) wif->do_node((ir_node *)irn_list[irn_i], wif->env);

        // do ir nodes in prefix or postfix order?
        if (wif->flags & FW_DUMP_IRN_IN_PREFIX)
          irn_i--;
        else
          irn_i++;
      }
      // call block as postfix ir node
      if ((wif->do_node) &&
          (wif->flags & (FW_DUMP_BLOCK_AS_IRN | FW_DUMP_IRN_IN_PREFIX)))
        wif->do_node(block, wif->env);

      /* wall over all block's ir nodes nested end =============== */

      if (wif->do_block_finalize) wif->do_block_finalize(current_ir_graph, wif->env);
    } // for each block

    /* walk over all irg's block nested end ====================== */

  } // for each ir graph irg
  if (wif->do_graph_finalize) wif->do_graph_finalize(wif->env);

  /** ### ToDo: Dump const_code_irg ?? No! Dump const code with entities, types etc. */

  /* restore the state of current_ir_graph */
  current_ir_graph = saved_irg;
}
