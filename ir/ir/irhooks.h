/*
 * Project:     libFIRM
 * File name:   ir/ir/irhooks.h
 * Purpose:     Generic hooks for various libFirm functions.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (C) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irhooks.h
 *
 * Generic hooks for various libFirm functions.
 *
 * @author Michael Beck
 */
#ifndef __IRHOOKS_H__
#define __IRHOOKS_H__

#include "firm_config.h"
#include "irop.h"
#include "irnode.h"
#include "irgraph.h"

/**
 * options for the hook_merge_nodes hook
 */
typedef enum {
  HOOK_OPT_STG,         /**< straightening optimization */
  HOOK_OPT_IFSIM,       /**< if simplification */
  HOOK_OPT_CONST_EVAL,  /**< constant evaluation */
  HOOK_OPT_ALGSIM,      /**< algebraic simplification */
  HOOK_OPT_PHI,         /**< Phi optmization */
  HOOK_OPT_WAW,         /**< Write-After-Write optimization */
  HOOK_OPT_WAR,         /**< Write-After-Read optimization */
  HOOK_OPT_RAW,         /**< Read-After-Write optimization */
  HOOK_OPT_RAR,         /**< Read-After-Read optimization */
  HOOK_OPT_RC,          /**< Read-a-Const optimization */
  HOOK_OPT_TUPLE,	/**< Tuple optimization */
  HOOK_OPT_ID,          /**< ID optimization */
  HOOK_OPT_CSE,         /**< common subexpression elimination */
  HOOK_OPT_STRENGTH_RED,/**< strength reduction */
  HOOK_OPT_ARCH_DEP,    /**< architecture dependent optimization */
  HOOK_OPT_REASSOC,     /**< reassociation */
  HOOK_OPT_POLY_CALL,   /**< polymorphic call optimization */
  HOOK_LOWERED,         /**< lowered */

  HOOK_OPT_LAST
} hook_opt_kind;

/**
 * a hook entry
 */
typedef struct hook_entry {
  /** a union of all possible hook types */
  union {
    void (*_hook_new_ir_op)(void *context, ir_op *op);
    void (*_hook_free_ir_op)(void *context, ir_op *op);
    void (*_hook_new_node)(void *context, ir_node *node);
    void (*_hook_turn_into_id)(void *context, ir_node *node);
    void (*_hook_new_graph)(void *context, ir_graph *irg, entity *ent);
    void (*_hook_free_graph)(void *context, ir_graph *irg);
    void (*_hook_irg_walk)(void *context, ir_graph *irg, void *pre, void *post);
    void (*_hook_irg_walk_blkwise)(void *context, ir_graph *irg, void *pre, void *post);
    void (*_hook_irg_block_walk)(void *context, ir_graph *irg, ir_node *node, void *pre, void *post);
    void (*_hook_merge_nodes)(void *context, ir_node **new_node_array, int new_num_entries, ir_node **old_node_array, int old_num_entries, hook_opt_kind opt);
    void (*_hook_reassociate)(void *context, int start);
    void (*_hook_lower)(void *context, ir_node *node);
    void (*_hook_inline)(void *context, ir_node *call, ir_graph *irg);
    void (*_hook_tail_rec)(void *context, ir_graph *irg);
    void (*_hook_strength_red)(void *context, ir_graph *irg, ir_node *strong, ir_node *cmp);
    void (*_hook_dead_node_elim_start)(void *context, ir_graph *irg);
    void (*_hook_dead_node_elim_stop)(void *context, ir_graph *irg);
    void (*_hook_arch_dep_replace_mul_with_shifts)(void *context, ir_node *irn);
    void (*_hook_arch_dep_replace_div_by_const)(void *context, ir_node *irn);
    void (*_hook_arch_dep_replace_mod_by_const)(void *context, ir_node *irn);
    void (*_hook_arch_dep_replace_DivMod_by_const)(void *context, ir_node *irn);
  } hook;

  /** the context for every hook */
  void *context;

  /** needed for chaining */
  struct hook_entry *next;
} hook_entry_t;

/**
 * possible hooks
 */
typedef enum {
  hook_new_ir_op,
  hook_free_ir_op,
  hook_new_node,
  hook_turn_into_id,
  hook_new_graph,
  hook_free_graph,
  hook_irg_walk,
  hook_irg_walk_blkwise,
  hook_irg_block_walk,
  hook_merge_nodes,
  hook_reassociate,
  hook_lower,
  hook_inline,
  hook_tail_rec,
  hook_strength_red,
  hook_dead_node_elim_start,
  hook_dead_node_elim_stop,
  hook_arch_dep_replace_mul_with_shifts,
  hook_arch_dep_replace_div_by_const,
  hook_arch_dep_replace_mod_by_const,
  hook_arch_dep_replace_DivMod_by_const,
  hook_last,
} hook_type_t;

/**
 * register the hook entry.
 *
 * @param hook   the hook type
 * @rapam entry  the hook entry
 */
void register_hook(hook_type_t hook, hook_entry_t *entry);

#ifdef FIRM_ENABLE_HOOKS

extern hook_entry_t *hooks[hook_last];

/**
 * execute the hook what with the args args
 * Do not use this macro directly.
 */
#define hook_exec(what, args) do {       \
  hook_entry_t *p;                       \
  for (p = hooks[what]; p; p = p->next){ \
    void *ctx = p->context;              \
    p->hook._##what args;                \
  }                                      \
} while (0)

#else

#define hook_exec(what, args)

#endif /* FIRM_ENABLE_HOOKS */

#define hook_new_ir_op(op)                hook_exec(hook_new_ir_op, (ctx, op))
#define hook_free_ir_op(op)               hook_exec(hook_free_ir_op, (ctx, op))
#define hook_new_node(node)               hook_exec(hook_new_node, (ctx, node))
#define hook_turn_into_id(node)           hook_exec(hook_turn_into_id, (ctx, node))
#define hook_new_graph(irg, ent)          hook_exec(hook_new_graph, (ctx, irg, ent))
#define hook_free_graph(irg)              hook_exec(hook_free_graph, (ctx, irg))
#define hook_irg_walk(irg, pre, post)     hook_exec(hook_irg_walk, (ctx, irg, pre, post))
#define hook_irg_walk_blkwise(irg, pre, post) \
  hook_exec(hook_irg_walk_blkwise, (ctx, irg, pre, post))
#define hook_irg_block_walk(irg, node, pre, post) \
  hook_exec(hook_irg_block_walk, (ctx, irg, node, pre, post))
#define hook_merge_nodes(new_node_array, new_num_entries, old_node_array, old_num_entries, opt) \
  hook_exec(hook_merge_nodes, (ctx, new_node_array, new_num_entries, old_node_array, old_num_entries, opt))
#define hook_reassociate(start)           hook_exec(hook_reassociate, (ctx, start))
#define hook_lower(node)                  hook_exec(hook_lower, (ctx, node))
#define hook_inline(call, irg)            hook_exec(hook_inline, (ctx, call, irg))
#define hook_tail_rec(irg)                hook_exec(hook_tail_rec, (ctx, irg))
#define hook_strength_red(irg, strong, cmp) \
  hook_exec(hook_strength_red, (ctx, irg, strong, cmp))
#define hook_dead_node_elim_start(irg)    hook_exec(hook_dead_node_elim_start, (ctx, irg))
#define hook_dead_node_elim_stop(irg)     hook_exec(hook_dead_node_elim_stop, (ctx, irg))
#define hook_arch_dep_replace_mul_with_shifts(irn) \
  hook_exec(hook_arch_dep_replace_mul_with_shifts, (ctx, irn))
#define hook_arch_dep_replace_div_by_const(irn) \
  hook_exec(hook_arch_dep_replace_div_by_const, (ctx, irn))
#define hook_arch_dep_replace_mod_by_const(irn) \
  hook_exec(hook_arch_dep_replace_mod_by_const, (ctx, irn))
#define hook_arch_dep_replace_DivMod_by_const(irn) \
  hook_exec(hook_arch_dep_replace_DivMod_by_const, (ctx, irn))

#endif /* __IRHOOKS_H__ */
