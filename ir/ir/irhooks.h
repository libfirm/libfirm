/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Generic hooks for various libFirm functions.
 * @author  Michael Beck
 */
#ifndef FIRM_IR_IRHOOKS_H
#define FIRM_IR_IRHOOKS_H

#include "irop.h"
#include "irnode.h"

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
	HOOK_OPT_NORMALIZE,   /**< a commutative node was normalized */
	HOOK_LOWERED,         /**< lowered */
	HOOK_BACKEND,         /**< a backend transformation */
	HOOK_OPT_LAST
} hook_opt_kind;

/** Result of an if-conversion attempt */
typedef enum if_result_t {
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
struct hook_entry {
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

		/** This hook is called, after a commutative node was normalized. */
		void (*_hook_normalize)(void *context, ir_node *node);

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

		/** This hook is called after if conversion has run. */
		void (*_hook_if_conversion)(void *context, ir_graph *irg, ir_node *phi, int pos, ir_node *mux, if_result_t reason);

		/** This hook is called after a call was detected as const call */
		void (*_hook_func_call)(void *context, ir_graph *irg, ir_node *call);

		/** This hook is called after a Mul was replaced by a series of Shift and Add/Sub operations. */
		void (*_hook_arch_dep_replace_mul_with_shifts)(void *context, ir_node *irn);

		/** This hook is called after a Div/Mod by a constant value was replaced. */
		void (*_hook_arch_dep_replace_division_by_const)(void *context, ir_node *irn);

		/** This hook is called after a new mode was registered. */
		void (*_hook_new_mode)(void *context, ir_mode *mode);

		/** This hook is called after a new entity was created. */
		void (*_hook_new_entity)(void *context, ir_entity *ent);

		/** This hook is called after a new type was created. */
		void (*_hook_new_type)(void *context, ir_type *tp);

		/** This hook is called at the end of the node info dumper to dump additional node info. */
		void (*_hook_node_info)(void *context, FILE *f, const ir_node *n);
	} hook; /**< hook */

	/** the context for every hook */
	void *context;

	/** needed for chaining */
	struct hook_entry *next;
};

/**
 * possible hooks
 */
typedef enum {
	hook_new_ir_op,            /**< type for hook_new_ir_op() hook */
	hook_free_ir_op,           /**< type for hook_free_ir_op() hook */
	hook_new_node,             /**< type for hook_new_node() hook */
	hook_set_irn_n,            /**< type for hook_set_irn_n() hook */
	hook_replace,              /**< type for hook_replace() hook */
	hook_turn_into_id,         /**< type for hook_turn_into_id() hook */
	hook_normalize,            /**< type for hook_normalize() hook */
	hook_new_graph,            /**< type for hook_new_graph() hook */
	hook_free_graph,           /**< type for hook_free_graph() hook */
	hook_irg_walk,             /**< type for hook_irg_walk() hook */
	hook_irg_walk_blkwise,     /**< type for hook_irg_walk_blkwise() hook */
	hook_irg_block_walk,       /**< type for hook_irg_block_walk() hook */
	hook_merge_nodes,          /**< type for hook_merge_nodes() hook */
	hook_reassociate,          /**< type for hook_reassociate() hook */
	hook_lower,                /**< type for hook_lower() hook */
	hook_inline,               /**< type for hook_inline() hook */
	hook_tail_rec,             /**< type for hook_tail_rec() hook */
	hook_strength_red,         /**< type for hook_strength_red() hook */
	hook_dead_node_elim,       /**< type for hook_dead_node_elim() hook */
	hook_if_conversion,        /**< type for hook_if_conversion() hook */
	hook_func_call,            /**< type for hook_func_call() hook */
	/** type for hook_arch_dep_replace_mul_with_shifts() hook */
	hook_arch_dep_replace_mul_with_shifts,
	/** type for hook_arch_dep_replace_division_by_const() hook */
	hook_arch_dep_replace_division_by_const,
	hook_new_mode,             /**< type for hook_new_mode() hook */
	hook_new_entity,           /**< type for hook_new_entity() hook */
	hook_new_type,             /**< type for hook_new_type() hook */
	hook_node_info,            /**< type for hook_node_info() hook */
	hook_last                  /**< last hook type */
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

/** Global list of registerd hooks. */
extern hook_entry_t *hooks[hook_last];

/**
 * Executes the hook @p what with the args @p args
 * Do not use this macro directly.
 */
#define hook_exec(what, args) do {           \
  hook_entry_t *_p;                          \
  for (_p = hooks[what]; _p; _p = _p->next){ \
    void *hook_ctx_ = _p->context;           \
    _p->hook._##what args;                   \
  }                                          \
} while (0)

/** Called when a new node opcode has been created */
#define hook_new_ir_op(op)                hook_exec(hook_new_ir_op, (hook_ctx_, op))
/** Called when a node opcode has been freed */
#define hook_free_ir_op(op)               hook_exec(hook_free_ir_op, (hook_ctx_, op))
/** Called after a new node has been created */
#define hook_new_node(graph, node)        hook_exec(hook_new_node, (hook_ctx_, graph, node))
/** Called when a nodes input is changed */
#define hook_set_irn_n(src, pos, tgt, old_tgt) \
  hook_exec(hook_set_irn_n, (hook_ctx_, src, pos, tgt, old_tgt))
/** Called when a node is replaced */
#define hook_replace(old, nw)             hook_exec(hook_replace, (hook_ctx_, old, nw))
/** Called when a node is turned into an Id node */
#define hook_turn_into_id(node)           hook_exec(hook_turn_into_id, (hook_ctx_, node))
/** Called when a node is normalized */
#define hook_normalize(node)              hook_exec(hook_normalize, (hook_ctx_, node))
/** Called after a new graph has been created */
#define hook_new_graph(irg, ent)          hook_exec(hook_new_graph, (hook_ctx_, irg, ent))
/** Called after a graph has been freed */
#define hook_free_graph(irg)              hook_exec(hook_free_graph, (hook_ctx_, irg))
/** Called before a graph walk is started */
#define hook_irg_walk(irg, pre, post)     hook_exec(hook_irg_walk, (hook_ctx_, irg, pre, post))
/** Called before a blockwise graph walk is started */
#define hook_irg_walk_blkwise(irg, pre, post) \
  hook_exec(hook_irg_walk_blkwise, (hook_ctx_, irg, pre, post))
/** Called before a block walk is started */
#define hook_irg_block_walk(irg, node, pre, post) \
  hook_exec(hook_irg_block_walk, (hook_ctx_, irg, node, pre, post))
/** Called before 2 nodes get merged */
#define hook_merge_nodes(new_node_array, new_num_entries, old_node_array, old_num_entries, opt) \
  hook_exec(hook_merge_nodes, (hook_ctx_, new_node_array, new_num_entries, old_node_array, old_num_entries, opt))
/** Called before node inputs get reassociated */
#define hook_reassociate(start)           hook_exec(hook_reassociate, (hook_ctx_, start))
/** Called before a node gets lowered */
#define hook_lower(node)                  hook_exec(hook_lower, (hook_ctx_, node))
/** Called before a graph is inlined */
#define hook_inline(call, irg)            hook_exec(hook_inline, (hook_ctx_, call, irg))
/** Called before tail recursion is performed */
#define hook_tail_rec(irg, n_calls)       hook_exec(hook_tail_rec, (hook_ctx_, irg, n_calls))
/** Called before strength reduction is performed */
#define hook_strength_red(irg, node) \
  hook_exec(hook_strength_red, (hook_ctx_, irg, node))
/** Called before dead node elimination is performed */
#define hook_dead_node_elim(irg, start)   hook_exec(hook_dead_node_elim, (hook_ctx_, irg, start))
/** Called when if-conversion creates a Mux node */
#define hook_if_conversion(irg, phi, pos, mux, reason) \
  hook_exec(hook_if_conversion, (hook_ctx_, irg, phi, pos, mux, reason))
/** Called when a function call is optimized */
#define hook_func_call(irg, call) \
  hook_exec(hook_func_call, (hook_ctx_, irg, call))
/** Called when a mul is replaced with shifts */
#define hook_arch_dep_replace_mul_with_shifts(irn) \
  hook_exec(hook_arch_dep_replace_mul_with_shifts, (hook_ctx_, irn))
/** Called when a dvision by constant is replaced */
#define hook_arch_dep_replace_division_by_const(irn) \
  hook_exec(hook_arch_dep_replace_division_by_const, (hook_ctx_, irn))
/** Called when a new mode has been created */
#define hook_new_mode(mode)               hook_exec(hook_new_mode, (hook_ctx_, mode))
/** Called when a new entity has been created */
#define hook_new_entity(ent)              hook_exec(hook_new_entity, (hook_ctx_, ent))
/** Called when a new type has been created */
#define hook_new_type(tp)                 hook_exec(hook_new_type, (hook_ctx_, tp))
/** Called at the end of the node info dumper to dump additional node info. */
#define hook_node_info(F, node)           hook_exec(hook_node_info, (hook_ctx_, F, node))

#endif
