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

#include <stdio.h>
#include "firm_types.h"

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
		/** This hook is called, after a new IR-node was created and before it is optimized. */
		void (*_hook_new_node)(void *context, ir_node *node);

		/** This hook is called, before a node is replaced (exchange()) by another. */
		void (*_hook_replace)(void *context, ir_node *old_node, ir_node *new_node);

		/** This hook is called, after a new graph was created and before the first block
		 * on this graph is built. */
		void (*_hook_new_graph)(void *context, ir_graph *irg, ir_entity *ent);

		/** This hook is called, before a node is lowered. */
		void (*_hook_lower)(void *context, ir_node *node);

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
	hook_new_node,             /**< type for hook_new_node() hook */
	hook_replace,              /**< type for hook_replace() hook */
	hook_new_graph,            /**< type for hook_new_graph() hook */
	hook_lower,                /**< type for hook_lower() hook */
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
#define hook_exec(what, args) do {             \
	hook_entry_t *_p;                          \
	for (_p = hooks[what]; _p; _p = _p->next){ \
		void *hook_ctx_ = _p->context;         \
		_p->hook._##what args;                 \
	}                                          \
} while (0)

/** Called after a new node has been created */
#define hook_new_node(node)               hook_exec(hook_new_node, (hook_ctx_, node))
/** Called when a node is replaced */
#define hook_replace(old, nw)             hook_exec(hook_replace, (hook_ctx_, old, nw))
/** Called after a new graph has been created */
#define hook_new_graph(irg, ent)          hook_exec(hook_new_graph, (hook_ctx_, irg, ent))
/** Called before a node gets lowered */
#define hook_lower(node)                  hook_exec(hook_lower, (hook_ctx_, node))
/** Called when a new mode has been created */
#define hook_new_mode(mode)               hook_exec(hook_new_mode, (hook_ctx_, mode))
/** Called when a new entity has been created */
#define hook_new_entity(ent)              hook_exec(hook_new_entity, (hook_ctx_, ent))
/** Called when a new type has been created */
#define hook_new_type(tp)                 hook_exec(hook_new_type, (hook_ctx_, tp))
/** Called at the end of the node info dumper to dump additional node info. */
#define hook_node_info(F, node)           hook_exec(hook_node_info, (hook_ctx_, F, node))

#endif
