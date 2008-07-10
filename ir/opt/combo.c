/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   Cliff Click's Combined Analysis/Optimization
 * @author  Michael Beck
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "iroptimize.h"
#include <assert.h>
#include "list.h"
#include "obstack.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irgwalk.h"
#include "irop.h"
#include "irouts.h"
#include "irgmod.h"
#include "debug.h"

typedef struct partition_entry_t partition_entry_t;
typedef struct partition_t       partition_t;

/**
 * A partition entry.
 */
struct partition_entry_t {
	ir_node           *node;         /**< The node itself. */
	list_head         list;          /**< double-linked list */
	partition_t       *part;         /**< points to the partition this entry belongs to */
	partition_entry_t *touched_next; /**< Next entry on partition.touched set. */
	unsigned          on_touched:1;  /**< Set, if this entry is on the partition.touched set. */
};

/**
 * A partition containing congruent nodes.
 */
struct partition_t {
	list_head         entries;       /**< The partition entries. */
	partition_t       *wl_next;      /**< Next entry in the work list if any. */
	partition_t       *touched_next; /**< points to the next partition in the touched set. */
	partition_entry_t *touched;      /**< the partition.touched set of this partition. */
	unsigned          n_entries;     /**< number of entries in this partition. */
	unsigned          n_touched;     /**< number of entries in the partition.touched. */
	int               n_inputs;      /**< Maximum number of inputs of all entries. */
	unsigned          on_worklist:1; /**< Set, if this partition is in the work list. */
	unsigned          on_touched:1;  /**< Set, if this partition is on the touched set. */
};

typedef struct environment_t {
	struct obstack  obst;      /**< obstack to allocate data structures. */
	partition_t     *worklist; /**< The work list. */
	partition_t     *touched;  /**< the touched set. */
	partition_t     *opcode_map[iro_Last];  /**< The initial partition set. */
} environment_t;

#define get_irn_entry(irn)         ((partition_entry_t *)get_irn_link(irn))
#define set_irn_entry(irn, entry)  set_irn_link(irn, entry)

/** The debug module handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

#ifdef DEBUG_libfirm
static void dump_partition(partition_t *part) {
	partition_entry_t *entry;

	DB((dbg, LEVEL_2, "{ "));
	list_for_each_entry(partition_entry_t, entry, &part->entries, list) {
		DB((dbg, LEVEL_2, "%+F, ", entry->node));
	}
	DB((dbg, LEVEL_2, "}\n"));
}
#else
#define dump_partition(part)
#endif

/**
 * Create a new empty partition.
 */
static INLINE partition_t *new_partition(environment_t *env) {
	partition_t *part = obstack_alloc(&env->obst, sizeof(*part));

	INIT_LIST_HEAD(&part->entries);
	part->wl_next      = env->worklist;
	part->touched_next = NULL;
	part->touched      = NULL;
	part->n_entries    = 0;
	part->n_touched    = 0;
	part->n_inputs     = 0;
	part->on_worklist  = 0;
	part->on_touched   = 0;

	return part;
}

/**
 * Get the partition for a given opcode.
 */
static INLINE partition_t *get_partition(ir_opcode code, environment_t *env) {
	partition_t *part = env->opcode_map[code];

	if (part == NULL) {
		/* create a new partition and place it on the wait queue */
		part = new_partition(env);

		part->on_worklist     = 1;
		env->worklist         = part;
		env->opcode_map[code] = part;
	}
	return part;
}

/**
 * Walker, creates the initial partitions, one for every opcode and place them
 * on the worklist.
 */
static void create_initial_partitions(ir_node *irn, void *ctx) {
	environment_t *env  = ctx;
	ir_opcode     code  = get_irn_opcode(irn);
	partition_t   *part = get_partition(code, env);
	int           arity;

	/* create a partition entry and place it in the partition */
	partition_entry_t *entry = obstack_alloc(&env->obst, sizeof(*entry));

	INIT_LIST_HEAD(&entry->list);
	entry->node         = irn;
	entry->part         = part;
	entry->touched_next = NULL;
	entry->on_touched   = 0;
	set_irn_entry(irn, entry);

	list_add_tail(&entry->list, &part->entries);
	++part->n_entries;

	arity = get_irn_arity(irn);
	if (arity > part->n_inputs)
		part->n_inputs = arity;
}

/**
 * Add a partition to the touched set if not already there.
 */
static INLINE void add_to_touched(partition_t *part, environment_t *env) {
	if (part->on_touched == 0) {
		part->touched_next = env->touched;
		env->touched       = part;
		part->on_touched   = 1;
	}
}

/**
 * Add an entry to the entry.partition.touched set if not already there..
 */
static INLINE void add_to_partition_touched(partition_entry_t *y) {
	if (y->on_touched == 0) {
		partition_t *part = y->part;

		y->touched_next = part->touched;
		part->touched   = y;
		++part->n_touched;
		y->on_touched   = 1;
	}
}

/**
 * Split a partition by the touched set.
 */
static partition_t *split(partition_t *Z, partition_entry_t *g, environment_t *env) {
	partition_t       *Z_prime;
	partition_entry_t *entry;
	unsigned          n = 0;

	/* Remove g from Z. */
	for (entry = g; entry != NULL; entry = entry->touched_next) {
		list_del(&entry->list);
		entry->on_touched = 0;
		++n;
	}
	Z->n_entries -= n;

	/* Move g to a new partition, Z’. */
	Z_prime = new_partition(env);
	for (entry = g; entry != NULL; entry = entry->touched_next) {
		list_add(&entry->list, &Z_prime->entries);
		entry->part = Z_prime;
	}
	Z_prime->n_entries = n;

	/* If Z is on worklist then add Z’ to worklist.
	   Else add the smaller of Z and Z’ to worklist. */
	if (Z->on_worklist || Z_prime->n_entries < Z->n_entries) {
		Z_prime->on_worklist = 1;
		Z_prime->wl_next     = env->worklist;
		env->worklist        = Z_prime;
	} else {
		Z->on_worklist = 1;
		Z->wl_next     = env->worklist;
		env->worklist  = Z;
	}
	return Z_prime;
}

/**
 * Split the partitions if caused by the first entry on the worklist.
 */
static void cause_splits(environment_t *env) {
	partition_t       *X, *Z;
	partition_entry_t *x, *y, *e;
	int               i;

	/* remove the first partition from the worklist */
	X = env->worklist;
	env->worklist  = X->wl_next;
	X->on_worklist = 0;

	for (i = X->n_inputs - 1; i >= -1; --i) {
		/* empty the touched set: already done, just clear the list */
		env->touched = NULL;

		list_for_each_entry(partition_entry_t, x, &X->entries, list) {
			if (i < get_irn_arity(x->node) && (!is_Block(x->node) || i >= 0)) {
				y = get_irn_entry(get_irn_n(x->node, i));

				add_to_touched(y->part, env);
				add_to_partition_touched(y);
			}
		}

		for (Z = env->touched; Z != NULL; Z = Z->touched_next) {
			/* remove it from the touched set */
			Z->on_touched = 0;

			if (Z->n_entries != Z->n_touched) {
				split(Z, Z->touched, env);
			}
			/* Empty Z.touched. */
			for (e = Z->touched; e != NULL; e = e->touched_next) {
				e->on_touched = 0;
			}
			Z->touched = NULL;
		}

	}
}

/**
 * Get the leader for a given node from its congruence class.
 *
 * @param irn  the node
 */
static ir_node *get_leader(ir_node *irn) {
	partition_t *part = get_irn_entry(irn)->part;

	if (part->n_entries > 1) {
		DB((dbg, LEVEL_2, "Found congruence class for %+F ", irn));
		dump_partition(part);
	}
	return irn;
}

/**
 * Post-Walker, apply the analysis results;
 */
static void apply_result(ir_node *irn, void *ctx) {
	environment_t *env = ctx;

	ir_node *leader = get_leader(irn);

	if (leader != irn) {
		exchange(irn, leader);
	}
}

void combo(ir_graph *irg) {
	environment_t env;

	/* register a debug mask */
	FIRM_DBG_REGISTER(dbg, "firm.opt.combo");
	firm_dbg_set_mask(dbg, SET_LEVEL_2);

	obstack_init(&env.obst);
	env.worklist = NULL;
	env.touched  = NULL;
	memset(env.opcode_map, 0, sizeof(env.opcode_map));

	assure_irg_outs(irg);

	/* create the initial partitions */
	irg_walk_graph(irg, NULL, create_initial_partitions, &env);

	while (env.worklist != NULL)
		cause_splits(&env);

	/* apply the result */
	irg_walk_graph(irg, NULL, apply_result, &env);

	obstack_free(&env.obst, NULL);
}
