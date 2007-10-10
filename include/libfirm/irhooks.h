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
 * @brief   Generic hooks for various libFirm functions.
 * @author  Michael Beck
 * @version $Id$
 */
#ifndef FIRM_IR_IRHOOKS_H
#define FIRM_IR_IRHOOKS_H

#include "firm_config.h"
#include "irop.h"
#include "irnode.h"
#include "irgraph.h"

/**
 * options for the hook_merge_nodes hook
 */
typedef enum {
	HOOK_OPT_DEAD_BLOCK,  /**< a block was removed because it's dead */
	HOOK_OPT_STG,         /**< straightening optimization */
	HOOK_OPT_IFSIM,       /**< if simplification */
	HOOK_OPT_CONST_EVAL,  /**< constant evaluation */
	HOOK_OPT_ALGSIM,      /**< algebraic simplification */
	HOOK_OPT_PHI,         /**< Phi optimization */
	HOOK_OPT_SYNC,        /**< Sync optimization */
	HOOK_OPT_WAW,         /**< Write-After-Write optimization */
	HOOK_OPT_WAR,         /**< Write-After-Read optimization */
	HOOK_OPT_RAW,         /**< Read-After-Write optimization */
	HOOK_OPT_RAR,         /**< Read-After-Read optimization */
	HOOK_OPT_RC,          /**< Read-a-Const optimization */
	HOOK_OPT_TUPLE,       /**< Tuple optimization */
	HOOK_OPT_ID,          /**< ID optimization */
	HOOK_OPT_CSE,         /**< common subexpression elimination */
	HOOK_OPT_STRENGTH_RED,/**< strength reduction */
	HOOK_OPT_ARCH_DEP,    /**< architecture dependent optimization */
	HOOK_OPT_REASSOC,     /**< reassociation */
	HOOK_OPT_POLY_CALL,   /**< polymorphic call optimization */
	HOOK_OPT_IF_CONV,     /**< an if conversion was tried */
	HOOK_OPT_FUNC_CALL,   /**< a real function call was removed */
	HOOK_OPT_CONFIRM,     /**< a value was substituted by another due to a Confirm */
	HOOK_OPT_CONFIRM_C,   /**< a value was substituted by a const due to a Confirm */
	HOOK_OPT_CONFIRM_E,   /**< a value was evaluated due to a Confirm */
	HOOK_OPT_EXC_REM,     /**< a exception edge was removed due to a Confirmation prove */
	HOOK_LOWERED,         /**< lowered */
	HOOK_BACKEND,         /**< a backend transformation */
	HOOK_OPT_LAST
} hook_opt_kind;

typedef enum _if_result_t {
	IF_RESULT_SUCCESS         = 0,  /**< if conversion could be done */
	IF_RESULT_SIDE_EFFECT     = 1,  /**< if conversion failed because of side effect */
	IF_RESULT_SIDE_EFFECT_PHI = 2,  /**< if conversion failed because of Phi node found */
	IF_RESULT_TOO_DEEP        = 3,  /**< if conversion failed because of to deep DAG's */
	IF_RESULT_BAD_CF          = 4,  /**< if conversion failed because of bad control flow */
	IF_RESULT_DENIED          = 5,  /**< if conversion failed because of architecture deny */
	IF_RESULT_LAST
} if_result_t;

/**
 * A generic function type.
 */
typedef void (generic_func)(void);

/**
 * a hook entry
 */
typedef struct hook_entry {
	/** A union of all possible hook types. */
	union {
		/** This hook is called, after a new ir_op was created. */
		void (*_hook_new_ir_op)(void *context, ir_op *op);

		/** This hook is called, before am ir_op is destroyed. */
		void (*_hook_free_ir_op)(void *context, ir_op *op);

		/** This hook is called, after a new IR-node was created and before it is optimized. */
		void (*_hook_new_node)(void *context, ir_graph *graph, ir_node *node);

		/** This hook is called, before a node input was changed. */
		void (*_hook_set_irn_n)(void *context, ir_node *src,
		                        int pos, ir_node *tgt, ir_node *old_tgt);

		/** This hook is called, before a node is replaced (exchange()) by another. */
		void (*_hook_replace)(void *context, ir_node *old_node, ir_node *new_node);

		/** This hook is called, before a node is changed into an Id node. */
		void (*_hook_turn_into_id)(void *context, ir_node *node);

		/** This hook is called, after a new graph was created and before the first block
		*  on this graph is build. */
		void (*_hook_new_graph)(void *context, ir_graph *irg, ir_entity *ent);

		/** This hook is called before a graph is freed. */
		void (*_hook_free_graph)(void *context, ir_graph *irg);

		/** This hook is called before an irg walk is started. */
		void (*_hook_irg_walk)(void *context, ir_graph *irg, generic_func *pre, generic_func *post);

		/** This hook is called before an block wise irg walk is started. */
		void (*_hook_irg_walk_blkwise)(void *context, ir_graph *irg, generic_func *pre, generic_func *post);

		/** This hook is called before an block walk is started. */
		void (*_hook_irg_block_walk)(void *context, ir_graph *irg, ir_node *node, generic_func *pre, generic_func *post);

		/** This hook is called, when debug info must be merged. */
		void (*_hook_merge_nodes)(void *context, ir_node **new_node_array, int new_num_entries,
		                          ir_node **old_node_array, int old_num_entries, hook_opt_kind opt);

		/** This hook is called, when reassociation is started/stopped. */
		void (*_hook_reassociate)(void *context, int start);

		/** This hook is called, before a node is lowered. */
		void (*_hook_lower)(void *context, ir_node *node);

		/** This hook is called, before a graph is inlined. */
		void (*_hook_inline)(void *context, ir_node *call, ir_graph *irg);

		/** This hook is called, before tail recursion is applied to a graph. */
		void (*_hook_tail_rec)(void *context, ir_graph *irg, int n_calls);

		/** This hook is called, before a node is replaced due to strength reduction */
		void (*_hook_strength_red)(void *context, ir_graph *irg, ir_node *node);

		/** This hook is called, when dead node elimination is started/stopped. */
		void (*_hook_dead_node_elim)(void *context, ir_graph *irg, int start);

		/** This hook is called, when a node is substituted during dead node elimination. */
		void (*_hook_dead_node_elim_subst)(void *context, ir_graph *irg, ir_node *old, ir_node *nw);

		/** This hook is called after if conversion has run. */
		void (*_hook_if_conversion)(void *context, ir_graph *irg, ir_node *phi, int pos, ir_node *mux, if_result_t reason);

		/** This hook is called after a call was detected as const call */
		void (*_hook_func_call)(void *context, ir_graph *irg, ir_node *call);

		/** This hook is called after a Mul was replaced by a series of Shift and Add/Sub operations. */
		void (*_hook_arch_dep_replace_mul_with_shifts)(void *context, ir_node *irn);

		/** This hook is called after a Div/Mod/DivMod by a constant value was replaced. */
		void (*_hook_arch_dep_replace_division_by_const)(void *context, ir_node *irn);

		/** This hook is called after a new mode was registered. */
		void (*_hook_new_mode)(void *context, const ir_mode *tmpl, ir_mode *mode);

		/** This hook is called after a new entity was created. */
		void (*_hook_new_entity)(void *context, ir_entity *ent);

		/** This hook is called after a new type was created. */
		void (*_hook_new_type)(void *context, ir_type *tp);

		/** This hook is called at the end of the node info dumper to dump additional node info. */
		void (*_hook_node_info)(void *context, FILE *f, const ir_node *n);
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
	hook_set_irn_n,
	hook_replace,
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
	hook_dead_node_elim,
	hook_dead_node_elim_subst,
	hook_if_conversion,
	hook_func_call,
	hook_arch_dep_replace_mul_with_shifts,
	hook_arch_dep_replace_division_by_const,
	hook_new_mode,
	hook_new_entity,
	hook_new_type,
	hook_node_info,
	hook_last
} hook_type_t;

/**
 * register a hook entry.
 *
 * @param hook   the hook type
 * @param entry  the hook entry
 */
void register_hook(hook_type_t hook, hook_entry_t *entry);

/**
 * unregister a hook entry.
 *
 * @param hook   the hook type
 * @param entry  the hook entry
 */
void unregister_hook(hook_type_t hook, hook_entry_t *entry);

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
#define hook_new_node(graph, node)        hook_exec(hook_new_node, (ctx, graph, node))
#define hook_set_irn_n(src, pos, tgt, old_tgt) \
  hook_exec(hook_set_irn_n, (ctx, src, pos, tgt, old_tgt))
#define hook_replace(old, nw)             hook_exec(hook_replace, (ctx, old, nw))
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
#define hook_tail_rec(irg, n_calls)       hook_exec(hook_tail_rec, (ctx, irg, n_calls))
#define hook_strength_red(irg, node) \
  hook_exec(hook_strength_red, (ctx, irg, node))
#define hook_dead_node_elim(irg, start)   hook_exec(hook_dead_node_elim, (ctx, irg, start))
#define hook_dead_node_elim_subst(irg, old, nw) \
   hook_exec(hook_dead_node_elim_subst, (ctx, irg, old, nw))
#define hook_if_conversion(irg, phi, pos, mux, reason) \
  hook_exec(hook_if_conversion, (ctx, irg, phi, pos, mux, reason))
#define hook_func_call(irg, call) \
  hook_exec(hook_func_call, (ctx, irg, call))
#define hook_arch_dep_replace_mul_with_shifts(irn) \
  hook_exec(hook_arch_dep_replace_mul_with_shifts, (ctx, irn))
#define hook_arch_dep_replace_division_by_const(irn) \
  hook_exec(hook_arch_dep_replace_division_by_const, (ctx, irn))
#define hook_new_mode(tmpl, mode)         hook_exec(hook_new_mode, (ctx, tmpl, mode))
#define hook_new_entity(ent)              hook_exec(hook_new_entity, (ctx, ent))
#define hook_new_type(tp)                 hook_exec(hook_new_type, (ctx, tp))
#define hook_node_info(F, node)           hook_exec(hook_node_info, (ctx, F, node))

/* the initializer, move to hooks_t.h some day */
int firm_init_hooks(void);

#endif
