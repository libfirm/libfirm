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
 *
 * Note that we use the terminology from Click's work here, which is different
 * in some cases from Firm terminology.  Especially, Click's type is a
 * Firm tarval/entity, nevertheless we call it type here for "maximum compatibility".
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>

#include "iroptimize.h"
#include "irflag.h"
#include "ircons.h"
#include "list.h"
#include "array.h"
#include "set.h"
#include "pmap.h"
#include "obstack.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "irgwalk.h"
#include "irop.h"
#include "irouts.h"
#include "irgmod.h"
#include "debug.h"
#include "error.h"

#include "tv_t.h"

#include "irprintf.h"
#include "irdump.h"

/* define this to check that all type translations are monotone */
#define VERIFY_MONOTONE

typedef struct node_t            node_t;
typedef struct partition_t       partition_t;
typedef struct opcode_key_t      opcode_key_t;
typedef struct listmap_entry_t   listmap_entry_t;

/** The type of the compute function. */
typedef void (*compute_func)(node_t *node);

/**
 * An opcode map key.
 */
struct opcode_key_t {
	ir_opcode   code;   /**< The Firm opcode. */
	ir_mode     *mode;  /**< The mode of all nodes in the partition. */
	union {
		long      proj;   /**< For Proj nodes, its proj number */
		ir_entity *ent;   /**< For Sel Nodes, its entity */
	} u;
};

/**
 * An entry in the list_map.
 */
struct listmap_entry_t {
	void            *id;    /**< The id. */
	node_t          *list;  /**< The associated list for this id. */
	listmap_entry_t *next;  /**< Link to the next entry in the map. */
};

/** We must map id's to lists. */
typedef struct listmap_t {
	set             *map;    /**< Map id's to listmap_entry_t's */
	listmap_entry_t *values; /**< List of all values in the map. */
} listmap_t;

/**
 * A lattice element. Because we handle constants and symbolic constants different, we
 * have to use this union.
 */
typedef union {
	tarval          *tv;
	symconst_symbol sym;
} lattice_elem_t;

/**
 * A node.
 */
struct node_t {
	ir_node         *node;          /**< The IR-node itself. */
	list_head       node_list;      /**< Double-linked list of entries. */
	list_head       cprop_list;     /**< Double-linked partition.cprop list. */
	partition_t     *part;          /**< points to the partition this node belongs to */
	node_t          *next;          /**< Next node on local list (partition.touched, fallen). */
	lattice_elem_t  type;           /**< The associated lattice element "type". */
	int             max_user_input; /**< Maximum input number of Def-Use edges. */
	int             next_edge;      /**< Index of the next Def-Use edge to use. */
	unsigned        on_touched:1;   /**< Set, if this node is on the partition.touched set. */
	unsigned        on_cprop:1;     /**< Set, if this node is on the partition.cprop list. */
	unsigned        on_fallen:1;    /**< Set, if this node is on the fallen list. */
};

/**
 * A partition containing congruent nodes.
 */
struct partition_t {
	list_head         entries;         /**< The head of partition node list. */
	list_head         cprop;           /**< The head of partition.cprop list. */
	partition_t       *wl_next;        /**< Next entry in the work list if any. */
	partition_t       *touched_next;   /**< Points to the next partition in the touched set. */
	partition_t       *cprop_next;     /**< Points to the next partition in the cprop list. */
	node_t            *touched;        /**< The partition.touched set of this partition. */
	unsigned          n_nodes;         /**< Number of entries in this partition. */
	unsigned          n_touched;       /**< Number of entries in the partition.touched. */
	int               max_arity;       /**< Maximum arity of all entries. */
	int               max_user_inputs; /**< Maximum number of user inputs of all entries. */
	unsigned          on_worklist:1;   /**< Set, if this partition is in the work list. */
	unsigned          on_touched:1;    /**< Set, if this partition is on the touched set. */
	unsigned          on_cprop:1;      /**< Set, if this partition is on the cprop list. */
#ifdef DEBUG_libfirm
	partition_t       *dbg_next;       /**< Link all partitions for debugging */
	unsigned          nr;              /**< A unique number for (what-)mapping, >0. */
#endif
};

typedef struct environment_t {
	struct obstack  obst;           /**< obstack to allocate data structures. */
	partition_t     *worklist;      /**< The work list. */
	partition_t     *cprop;         /**< The constant propagation list. */
	partition_t     *touched;       /**< the touched set. */
	partition_t     *initial;       /**< The initial partition. */
	set             *opcode2id_map; /**< The opcodeMode->id map. */
	pmap            *type2id_map;   /**< The type->id map. */
	int             end_idx;        /**< -1 for local and 0 for global congruences. */
	int             lambda_input;   /**< Captured argument for lambda_partition(). */
#ifdef DEBUG_libfirm
	partition_t     *dbg_list;      /**< List of all partitions. */
#endif
} environment_t;

/** Type of the what function. */
typedef void *(*what_func)(const node_t *node, environment_t *env);

#define get_irn_node(irn)         ((node_t *)get_irn_link(irn))
#define set_irn_node(irn, node)   set_irn_link(irn, node)

/** The debug module handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/** Next partition number. */
DEBUG_ONLY(static unsigned part_nr = 0);

#ifdef DEBUG_libfirm
static INLINE lattice_elem_t get_partition_type(const partition_t *X);

/**
 * Dump partition to output.
 */
static void dump_partition(const char *msg, const partition_t *part) {
	const node_t   *node;
	int            first = 1;
	lattice_elem_t type = get_partition_type(part);

	DB((dbg, LEVEL_2, "%s part%u (%u, %+F) {\n  ", msg, part->nr, part->n_nodes, type));
	list_for_each_entry(node_t, node, &part->entries, node_list) {
		DB((dbg, LEVEL_2, "%s%+F", first ? "" : ", ", node->node));
		first = 0;
	}
	DB((dbg, LEVEL_2, "\n}\n"));
}

/**
 * Dump all partitions.
 */
static void dump_all_partitions(const environment_t *env) {
	const partition_t *P;

	DB((dbg, LEVEL_2, "All partitions\n===============\n"));
	for (P = env->dbg_list; P != NULL; P = P->dbg_next)
		dump_partition("", P);
}

#else
#define dump_partition(msg, part)
#define dump_all_partitions(env)
#endif

#if defined(VERIFY_MONOTONE) && defined (DEBUG_libfirm)
/**
 * Verify that a type transition is monotone
 */
static void verify_type(const lattice_elem_t old_type, const lattice_elem_t new_type) {
	if (old_type.tv == new_type.tv) {
		/* no change */
		return;
	}
	if (old_type.tv == tarval_top) {
		/* from Top down-to is always allowed */
		return;
	}
	if (old_type.tv == tarval_unreachable) {
		if (new_type.tv == tarval_reachable) {
			/* U -> R */
			return;
		}
		panic("verify_type(): wrong translation from %+F to %+F", old_type, new_type);
	}
	if (new_type.tv == tarval_bottom) {
		/* bottom reached */
		return;
	}
	panic("verify_type(): wrong translation from %+F to %+F", old_type, new_type);
}
#else
#define verify_type(old_type, new_type)
#endif

/**
 * Return the "top" value depending on the mode
 */
static tarval *get_top_value(const ir_mode *mode) {
	return (mode == mode_X || mode == mode_BB) ? tarval_unreachable : tarval_top;
}

/**
 * Compare two pointer values of a listmap.
 */
static int listmap_cmp_ptr(const void *elt, const void *key, size_t size) {
	const listmap_entry_t *e1 = elt;
	const listmap_entry_t *e2 = key;

	(void) size;
	return e1->id != e2->id;
}  /* listmap_cmp_ptr */

/**
 * Initializes a listmap.
 *
 * @param map  the listmap
 */
static void listmap_init(listmap_t *map) {
	map->map    = new_set(listmap_cmp_ptr, 16);
	map->values = NULL;
}  /* listmap_init */

/**
 * Terminates a listmap.
 *
 * @param map  the listmap
 */
static void listmap_term(listmap_t *map) {
	del_set(map->map);
}  /* listmap_term */

/**
 * Return the associated listmap entry for a given id.
 *
 * @param map  the listmap
 * @param id   the id to search for
 *
 * @return the asociated listmap entry for the given id
 */
static listmap_entry_t *listmap_find(listmap_t *map, void *id) {
	listmap_entry_t key, *entry;

	key.id   = id;
	key.list = NULL;
	key.next = NULL;
	entry = set_insert(map->map, &key, sizeof(key), HASH_PTR(id));

	if (entry->list == NULL) {
		/* a new entry, put into the list */
		entry->next = map->values;
		map->values = entry;
	}
	return entry;
}  /* listmap_find */

/**
 * Calculate the hash value for an opcode map entry.
 *
 * @param entry  an opcode map entry
 *
 * @return a hash value for the given opcode map entry
 */
static unsigned opcode_hash(const opcode_key_t *entry) {
	return (entry->mode - (ir_mode *)0) * 9 + entry->code + entry->u.proj * 3 + HASH_PTR(entry->u.ent);
}  /* opcode_hash */

/**
 * Compare two entries in the opcode map.
 */
static int cmp_opcode(const void *elt, const void *key, size_t size) {
	const opcode_key_t *o1 = elt;
	const opcode_key_t *o2 = key;

	(void) size;
	return o1->code != o2->code || o1->mode != o2->mode ||
	       o1->u.proj != o2->u.proj || o1->u.ent != o2->u.ent;
}  /* cmp_opcode */

/**
 * Compare two Def-Use edges for input position.
 */
static int cmp_def_use_edge(const void *a, const void *b) {
	const ir_def_use_edge *ea = a;
	const ir_def_use_edge *eb = b;

	/* no overrun, because range is [-1, MAXINT] */
	return ea->pos - eb->pos;
}  /* cmp_def_use_edge */

/**
 * We need the Def-Use edges sorted.
 */
static void sort_irn_outs(node_t *node) {
	ir_node *irn = node->node;
	int n_outs = get_irn_n_outs(irn);

	if (n_outs > 1) {
		qsort(&irn->out[1], n_outs, sizeof(irn->out[0]), cmp_def_use_edge);
	}
	node->max_user_input = irn->out[n_outs + 1].pos;
}  /* sort_irn_outs */

/**
 * Return the type of a node.
 *
 * @param irn  an IR-node
 *
 * @return the associated type of this node
 */
static INLINE lattice_elem_t get_node_type(const ir_node *irn) {
	return get_irn_node(irn)->type;
}  /* get_node_type */

/**
 * Return the tarval of a node.
 *
 * @param irn  an IR-node
 *
 * @return the associated type of this node
 */
static INLINE tarval *get_node_tarval(const ir_node *irn) {
	lattice_elem_t type = get_node_type(irn);

	if (is_tarval(type.tv))
		return type.tv;
	return tarval_bottom;
}  /* get_node_type */

/**
 * Add a partition to the worklist.
 */
static INLINE void add_to_worklist(partition_t *X, environment_t *env) {
	assert(X->on_worklist == 0);
	X->wl_next     = env->worklist;
	X->on_worklist = 1;
	env->worklist  = X;
}

/**
 * Create a new empty partition.
 *
 * @param env   the environment
 *
 * @return a newly allocated partition
 */
static INLINE partition_t *new_partition(environment_t *env) {
	partition_t *part = obstack_alloc(&env->obst, sizeof(*part));

	INIT_LIST_HEAD(&part->entries);
	INIT_LIST_HEAD(&part->cprop);
	part->wl_next         = NULL;
	part->touched_next    = NULL;
	part->cprop_next      = NULL;
	part->touched         = NULL;
	part->n_nodes         = 0;
	part->n_touched       = 0;
	part->max_arity       = 0;
	part->max_user_inputs = 0;
	part->on_worklist     = 0;
	part->on_touched      = 0;
	part->on_cprop        = 0;
#ifdef DEBUG_libfirm
	part->dbg_next        = env->dbg_list;
	env->dbg_list         = part;
	part->nr              = part_nr++;
#endif

	return part;
}  /* new_partition */

/**
 * Get the first node from a partition.
 */
static INLINE node_t *get_first_node(const partition_t *X) {
	return list_entry(X->entries.next, node_t, node_list);
}

/**
 * Return the type of a partition (assuming partition is non-empty and
 * all elements have the same type).
 *
 * @param X  a partition
 *
 * @return the type of the first element of the partition
 */
static INLINE lattice_elem_t get_partition_type(const partition_t *X) {
	const node_t *first = get_first_node(X);
	return first->type;
}  /* get_partition_type */

/**
 * Creates a partition node for the given IR-node and place it
 * into the given partition.
 *
 * @param irn   an IR-node
 * @param part  a partition to place the node in
 * @param env   the environment
 *
 * @return the created node
 */
static node_t *create_partition_node(ir_node *irn, partition_t *part, environment_t *env) {
	/* create a partition node and place it in the partition */
	node_t *node = obstack_alloc(&env->obst, sizeof(*node));
	ir_mode *mode = get_irn_mode(irn);

	INIT_LIST_HEAD(&node->node_list);
	INIT_LIST_HEAD(&node->cprop_list);
	node->node           = irn;
	node->part           = part;
	node->next           = NULL;
	node->type.tv        = get_top_value(mode);
	node->max_user_input = 0;
	node->next_edge      = 0;
	node->on_touched     = 0;
	node->on_cprop       = 0;
	node->on_fallen      = 0;
	set_irn_node(irn, node);

	list_add_tail(&node->node_list, &part->entries);
	++part->n_nodes;

	return node;
}  /* create_partition_node */

/**
 * Pre-Walker, init all Block-Phi lists.
 */
static void init_block_phis(ir_node *irn, void *env) {
	(void) env;

	if (is_Block(irn)) {
		set_Block_phis(irn, NULL);
	}
}

/**
 * Post-Walker, initialize all Nodes' type to U or top and place
 * all nodes into the TOP partition.
 */
static void create_initial_partitions(ir_node *irn, void *ctx) {
	environment_t *env  = ctx;
	partition_t   *part = env->initial;
	node_t        *node;
	int           arity;

	node = create_partition_node(irn, part, env);
	sort_irn_outs(node);
	arity = get_irn_arity(irn);
	if (arity > part->max_arity)
		part->max_arity = arity;
	if (node->max_user_input > part->max_user_inputs)
		part->max_user_inputs = node->max_user_input;

	if (is_Phi(irn)) {
		add_Block_phi(get_nodes_block(irn), irn);
	}
}  /* create_initial_partitions */

/**
 * Add a partition to the touched set if not already there.
 *
 * @param part  the partition
 * @param env   the environment
 */
static INLINE void add_to_touched(partition_t *part, environment_t *env) {
	if (part->on_touched == 0) {
		part->touched_next = env->touched;
		env->touched       = part;
		part->on_touched   = 1;
	}
}  /* add_to_touched */

/**
 * Add a node to the entry.partition.touched set if not already there.
 *
 * @param y  a node
 */
static INLINE void add_to_partition_touched(node_t *y) {
	if (y->on_touched == 0) {
		partition_t *part = y->part;

		y->next       = part->touched;
		part->touched = y;
		y->on_touched = 1;
		++part->n_touched;
	}
}  /* add_to_partition_touched */

/**
 * Update the worklist: If Z is on worklist then add Z' to worklist.
 * Else add the smaller of Z and Z' to worklist.
 *
 * @param Z        the Z partition
 * @param Z_prime  the Z' partition, a previous part of Z
 * @param env      the environment
 */
static void update_worklist(partition_t *Z, partition_t *Z_prime, environment_t *env) {
	if (Z->on_worklist || Z_prime->n_nodes < Z->n_nodes) {
		add_to_worklist(Z_prime, env);
	} else {
		add_to_worklist(Z, env);
	}
}  /* update_worklist */

/**
 * Split a partition by a local list.
 *
 * @param Z    the Z partition to split
 * @param g    a (non-empty) node list
 * @param env  the environment
 *
 * @return  a new partition containing the nodes of g
 */
static partition_t *split(partition_t *Z, node_t *g, environment_t *env) {
	partition_t *Z_prime;
	node_t      *node;
	unsigned    n = 0;
	int         max_input, max_arity, arity;

	dump_partition("Splitting ", Z);

	assert(g != NULL);

	/* Remove g from Z. */
	for (node = g; node != NULL; node = node->next) {
		list_del(&node->node_list);
		++n;
	}
	assert(n < Z->n_nodes);
	Z->n_nodes -= n;

	/* Move g to a new partition, Z’. */
	Z_prime = new_partition(env);
	max_arity = max_input = 0;
	for (node = g; node != NULL; node = node->next) {
		list_add(&node->node_list, &Z_prime->entries);
		node->part = Z_prime;
		arity = get_irn_arity(node->node);
		if (arity > max_arity)
			max_arity = arity;
		if (node->max_user_input > max_input)
			max_input = node->max_user_input;
	}
	Z_prime->max_arity       = max_arity;
	Z_prime->max_user_inputs = max_input;
	Z_prime->n_nodes         = n;

	update_worklist(Z, Z_prime, env);

	dump_partition("Now ", Z);
	dump_partition("Created new ", Z_prime);
	return Z_prime;
}  /* split */

/**
 * Returns non-zero if the i'th input of a Phi node is live.
 *
 * @param phi  a Phi-node
 * @param i    an input number
 *
 * @return non-zero if the i'th input of the given Phi node is live
 */
static int is_live_input(ir_node *phi, int i) {
	if (i >= 0) {
		ir_node        *block = get_nodes_block(phi);
		ir_node        *pred  = get_Block_cfgpred(block, i);
		lattice_elem_t type   = get_node_type(pred);

		return type.tv != tarval_unreachable;
	}
	/* else it's the control input, always live */
	return 1;
}  /* is_live_input */

/**
 * Return non-zero if a type is a constant.
 */
static int is_constant_type(lattice_elem_t type) {
	if (type.tv != tarval_bottom && type.tv != tarval_top)
		return 1;
	return 0;
}  /* is_constant_type */

/**
 * Place a node on the cprop list.
 *
 * @param y    the node
 * @param env  the environment
 */
static void add_node_to_cprop(node_t *y, environment_t *env) {
	/* Add y to y.partition.cprop. */
	if (y->on_cprop == 0) {
		partition_t *Y = y->part;

		list_add_tail(&y->cprop_list, &Y->cprop);
		y->on_cprop   = 1;

		DB((dbg, LEVEL_3, "Add %+F to part%u.cprop\n", y->node, Y->nr));

		/* place its partition on the cprop list */
		if (Y->on_cprop == 0) {
			Y->cprop_next = env->cprop;
			env->cprop    = Y;
			Y->on_cprop   = 1;
		}
	}
	if (get_irn_mode(y->node) == mode_T) {
		/* mode_T nodes always produce tarval_bottom, so we must explicitly
		   add it's Proj's to get constant evaluation to work */
		int i;

		for (i = get_irn_n_outs(y->node) - 1; i >= 0; --i) {
			node_t *proj = get_irn_node(get_irn_out(y->node, i));

			add_node_to_cprop(proj, env);
		}
	}

	if (is_Block(y->node)) {
		/* Due to the way we handle Phi's, we must place all Phis of a block on the list
		 * if someone placed the block. The Block is only placed if the reachability
		 * changes, and this must be re-evaluated in compute_Phi(). */
		ir_node *phi;
		for (phi = get_Block_phis(y->node); phi != NULL; phi = get_Phi_next(phi)) {
			node_t *p = get_irn_node(phi);
			add_node_to_cprop(p, env);
		}
	}
}  /* add_node_to_cprop */

/**
 * Check whether a type is neither Top or a constant.
 * Note: U, R are NOT constants!
 *
 * @param type  the type to check
 */
static int type_is_neither_top_nor_const(const lattice_elem_t type) {
	if (is_tarval(type.tv)) {
		if (type.tv == tarval_top)
			return 0;
		if (tarval_is_constant(type.tv))
			return 0;
	} else {
		/* is a symconst */
		return 0;
	}
	return 1;
}

/**
 * Split the partitions if caused by the first entry on the worklist.
 *
 * @param env  the environment
 */
static void cause_splits(environment_t *env) {
	partition_t *X, *Y, *Z;
	node_t      *x, *y, *e;
	int         i, end_idx;
	ir_opcode   code;
	ir_node     *succ;

	/* remove the first partition from the worklist */
	X = env->worklist;
	env->worklist  = X->wl_next;
	X->on_worklist = 0;

	dump_partition("Cause_split: ", X);
	end_idx = env->end_idx;
	for (i = -1; i <= X->max_user_inputs; ++i) {
		/* empty the touched set: already done, just clear the list */
		env->touched = NULL;

		list_for_each_entry(node_t, x, &X->entries, node_list) {
			int num_edges;

			if (i == -1) {
				x->next_edge = 1;
			}
			num_edges = get_irn_n_outs(x->node);

			while (x->next_edge <= num_edges) {
				ir_def_use_edge *edge = &x->node->out[x->next_edge];

				/* check if we have necessary edges */
				if (edge->pos > i)
					break;

				++x->next_edge;

				succ = edge->use;

				/* ignore the "control input" for non-pinned nodes
				   if we are running in GCSE mode */
				if (i < end_idx && get_irn_pinned(succ) != op_pin_state_pinned)
					continue;

				y = get_irn_node(succ);
				if (is_constant_type(y->type)) {
					code = get_irn_opcode(succ);
					if (code == iro_Sub || (code == iro_Proj && is_Cmp(get_Proj_pred(succ))))
						add_node_to_cprop(y, env);
				}

				/* Partitions of constants should not be split simply because their Nodes have unequal
				   functions or incongruent inputs. */
				if (type_is_neither_top_nor_const(y->type) &&
					(! is_Phi(y->node) || is_live_input(y->node, i))) {
					Y = y->part;
					add_to_touched(Y, env);
					add_to_partition_touched(y);
				}
			}
		}

		for (Z = env->touched; Z != NULL; Z = Z->touched_next) {
			/* remove it from the touched set */
			Z->on_touched = 0;

			if (Z->n_nodes != Z->n_touched) {
				DB((dbg, LEVEL_2, "Split part%d by touched\n", Z->nr));
				split(Z, Z->touched, env);
			}
			/* Empty local Z.touched. */
			for (e = Z->touched; e != NULL; e = e->next) {
				e->on_touched = 0;
			}
			Z->touched   = NULL;
			Z->n_touched = 0;
		}
	}
}  /* cause_splits */

/**
 * Implements split_by_what(): Split a partition by characteristics given
 * by the what function.
 *
 * @param X     the partition to split
 * @param What  a function returning an Id for every node of the partition X
 * @param P     an flexible array to store the result partitions or NULL
 * @param env   the environment
 *
 * @return if P != NULL P will be filled with the resulting partitions and returned
 */
static partition_t **split_by_what(partition_t *X, what_func What,
                                   partition_t **P, environment_t *env) {
	node_t          *x, *S;
	listmap_t       map;
	listmap_entry_t *iter;
	partition_t     *R;

	/* Let map be an empty mapping from the range of What to (local) list of Nodes. */
	listmap_init(&map);
	list_for_each_entry(node_t, x, &X->entries, node_list) {
		void            *id = What(x, env);
		listmap_entry_t *entry;

		if (id == NULL) {
			/* input not allowed, ignore */
			continue;
		}
		/* Add x to map[What(x)]. */
		entry = listmap_find(&map, id);
		x->next     = entry->list;
		entry->list = x;
	}
	/* Let P be a set of Partitions. */

	/* for all sets S except one in the range of map do */
	for (iter = map.values; iter != NULL; iter = iter->next) {
		if (iter->next == NULL) {
			/* this is the last entry, ignore */
			break;
		}
		S = iter->list;

		/* Add SPLIT( X, S ) to P. */
		DB((dbg, LEVEL_2, "Split part%d by what\n", X->nr));
		R = split(X, S, env);
		if (P != NULL) {
			ARR_APP1(partition_t *, P, R);
		}
	}
	/* Add X to P. */
	if (P != NULL) {
		ARR_APP1(partition_t *, P, X);
	}

	listmap_term(&map);
	return P;
}  /* split_by_what */

/** lambda n.(n.type) */
static void *lambda_type(const node_t *node, environment_t *env) {
	(void)env;
	return node->type.tv;
}  /* lambda_type */

/** lambda n.(n.opcode) */
static void *lambda_opcode(const node_t *node, environment_t *env) {
	opcode_key_t key, *entry;
	ir_node      *irn = node->node;

	key.code   = get_irn_opcode(irn);
	key.mode   = get_irn_mode(irn);
	key.u.proj = 0;
	key.u.ent  = NULL;

	switch (get_irn_opcode(irn)) {
	case iro_Proj:
		key.u.proj = get_Proj_proj(irn);
		break;
	case iro_Sel:
		key.u.ent = get_Sel_entity(irn);
		break;
	default:
		break;
	}

	entry = set_insert(env->opcode2id_map, &key, sizeof(key), opcode_hash(&key));
	return entry;
}  /* lambda_opcode */

/** lambda n.(n[i].partition) */
static void *lambda_partition(const node_t *node, environment_t *env) {
	ir_node *skipped = skip_Proj(node->node);
	ir_node *pred;
	node_t  *p;
	int     i = env->lambda_input;

	if (i >= get_irn_arity(node->node)) {
		/* we are outside the allowed range */
		return NULL;
	}

	/* ignore the "control input" for non-pinned nodes
	   if we are running in GCSE mode */
	if (i < env->end_idx && get_irn_pinned(skipped) != op_pin_state_pinned)
		return NULL;

	pred = i == -1 ? get_irn_n(skipped, i) : get_irn_n(node->node, i);
	p    = get_irn_node(pred);

	return p->part;
}  /* lambda_partition */

/**
 * Checks whether a type is a constant.
 */
static int is_type_constant(lattice_elem_t type) {
	if (is_tarval(type.tv))
		return tarval_is_constant(type.tv);
	/* else it is a symconst */
	return 1;
}

/**
 * Implements split_by().
 *
 * @param X    the partition to split
 * @param env  the environment
 */
static void split_by(partition_t *X, environment_t *env) {
	partition_t **P = NEW_ARR_F(partition_t *, 0);
	int         i, j, k;

	DB((dbg, LEVEL_2, "WHAT = lambda n.(n.type) on part%d\n", X->nr));
	P = split_by_what(X, lambda_type, P, env);
	for (i = ARR_LEN(P) - 1; i >= 0; --i) {
		partition_t *Y = P[i];

		if (Y->n_nodes > 1) {
			lattice_elem_t type = get_partition_type(Y);

			/* we do not want split the TOP, unreachable or constant partitions */
			if (type.tv != tarval_top && type.tv != tarval_unreachable && !is_type_constant(type)) {
				partition_t **Q = NEW_ARR_F(partition_t *, 0);

				DB((dbg, LEVEL_2, "WHAT = lambda n.(n.opcode) on part%d\n", Y->nr));
				Q = split_by_what(Y, lambda_opcode, Q, env);

				for (j = ARR_LEN(Q) - 1; j >= 0; --j) {
					partition_t *Z = Q[j];

					for (k = Z->max_arity - 1; k >= -1; --k) {
						if (Z->n_nodes > 1) {
							env->lambda_input = k;
							DB((dbg, LEVEL_2, "WHAT = lambda n.(n[%d].partition) on part%d\n", k, Z->nr));
							split_by_what(Z, lambda_partition, NULL, env);
						}
					}
				}
				DEL_ARR_F(Q);
			}
		}
	}
	DEL_ARR_F(P);
}  /* split_by */

/**
 * (Re-)compute the type for a given node.
 *
 * @param node  the node
 */
static void default_compute(node_t *node) {
	int     i;
	ir_node *irn = node->node;
	tarval  *top = tarval_top;

	if (get_irn_mode(node->node) == mode_X)
		top = tarval_unreachable;

	if (get_irn_pinned(irn) == op_pin_state_pinned) {
		node_t *block = get_irn_node(get_nodes_block(irn));

		if (block->type.tv == tarval_unreachable) {
			node->type.tv = top;
			return;
		}
	}

	/* if any of the data inputs have type top, the result is type top */
	for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(irn, i);
		node_t  *p    = get_irn_node(pred);

		if (p->type.tv == tarval_top) {
			node->type.tv = top;
			return;
		}
	}

	if (get_irn_mode(node->node) == mode_X)
		node->type.tv = tarval_reachable;
	else
		node->type.tv = computed_value(irn);
}  /* default_compute */

/**
 * (Re-)compute the type for a Block node.
 *
 * @param node  the node
 */
static void compute_Block(node_t *node) {
	int     i;
	ir_node *block = node->node;

	for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
		node_t *pred = get_irn_node(get_Block_cfgpred(block, i));

		if (pred->type.tv == tarval_reachable) {
			/* A block is reachable, if at least of predecessor is reachable. */
			node->type.tv = tarval_reachable;
			return;
		}
	}
	node->type.tv = tarval_unreachable;
}  /* compute_Block */

/**
 * (Re-)compute the type for a Jmp node.
 *
 * @param node  the node
 */
static void compute_Jmp(node_t *node) {
	node_t *block = get_irn_node(get_nodes_block(node->node));

	node->type = block->type;
}  /* compute_Jmp */

/**
 * (Re-)compute the type for the End node.
 *
 * @param node  the node
 */
static void compute_End(node_t *node) {
	/* the End node is NOT dead of course */
	node->type.tv = tarval_reachable;
}

/**
 * (Re-)compute the type for a SymConst node.
 *
 * @param node  the node
 */
static void compute_SymConst(node_t *node) {
	ir_node *irn = node->node;
	node_t  *block = get_irn_node(get_nodes_block(irn));

	if (block->type.tv == tarval_unreachable) {
		node->type.tv = tarval_top;
		return;
	}
	switch (get_SymConst_kind(irn)) {
	case symconst_addr_ent:
	/* case symconst_addr_name: cannot handle this yet */
		node->type.sym = get_SymConst_symbol(irn);
		break;
	default:
		node->type.tv = computed_value(irn);
	}
}  /* compute_SymConst */

/**
 * (Re-)compute the type for a Phi node.
 *
 * @param node  the node
 */
static void compute_Phi(node_t *node) {
	int            i;
	ir_node        *phi = node->node;
	lattice_elem_t type;

	/* if a Phi is in a unreachable block, its type is TOP */
	node_t *block = get_irn_node(get_nodes_block(phi));

	if (block->type.tv == tarval_unreachable) {
		node->type.tv = tarval_top;
		return;
	}

	/* Phi implements the Meet operation */
	type.tv = tarval_top;
	for (i = get_Phi_n_preds(phi) - 1; i >= 0; --i) {
		node_t *pred   = get_irn_node(get_Phi_pred(phi, i));
		node_t *pred_X = get_irn_node(get_Block_cfgpred(block->node, i));

		if (pred_X->type.tv == tarval_unreachable || pred->type.tv == tarval_top) {
			/* ignore TOP inputs: We must check here for unreachable blocks,
			   because Firm constants live in the Start Block are NEVER Top.
			   Else, a Phi (1,2) will produce Bottom, even if the 2 for instance
			   comes from a unreachable input. */
			continue;
		}
		if (pred->type.tv == tarval_bottom) {
			node->type.tv = tarval_bottom;
			return;
		} else if (type.tv == tarval_top) {
			/* first constant found */
			type = pred->type;
		} else if (type.tv != pred->type.tv) {
			/* different constants or tarval_bottom */
			node->type.tv = tarval_bottom;
			return;
		}
		/* else nothing, constants are the same */
	}
	node->type = type;
}  /* compute_Phi */

/**
 * (Re-)compute the type for an Add. Special case: one nodes is a Zero Const.
 *
 * @param node  the node
 */
static void compute_Add(node_t *node) {
	ir_node        *sub = node->node;
	node_t         *l   = get_irn_node(get_Add_left(sub));
	node_t         *r   = get_irn_node(get_Add_right(sub));
	lattice_elem_t a    = l->type;
	lattice_elem_t b    = r->type;
	node_t         *block = get_irn_node(get_nodes_block(sub));
	ir_mode        *mode;

	if (block->type.tv == tarval_unreachable) {
		node->type.tv = tarval_top;
		return;
	}

	if (a.tv == tarval_top || b.tv == tarval_top) {
		node->type.tv = tarval_top;
	} else if (a.tv == tarval_bottom || b.tv == tarval_bottom) {
		node->type.tv = tarval_bottom;
	} else {
		/* x + 0 = 0 + x = x, but beware of floating point +0 + -0, so we
		   must call tarval_add() first to handle this case! */
		if (is_tarval(a.tv)) {
			if (is_tarval(b.tv)) {
				node->type.tv = tarval_add(a.tv, b.tv);
				return;
			}
			mode = get_tarval_mode(a.tv);
			if (a.tv == get_mode_null(mode)) {
				node->type = b;
				return;
			}
		} else if (is_tarval(b.tv)) {
			mode = get_tarval_mode(b.tv);
			if (b.tv == get_mode_null(mode)) {
				node->type = a;
				return;
			}
		}
		node->type.tv = tarval_bottom;
	}
}  /* compute_Add */

/**
 * Returns true if a type is a constant.
 */
static int is_con(const lattice_elem_t type) {
	return is_entity(type.sym.entity_p) || tarval_is_constant(type.tv);
}

/**
 * (Re-)compute the type for a Sub. Special case: both nodes are congruent.
 *
 * @param node  the node
 */
static void compute_Sub(node_t *node) {
	ir_node        *sub = node->node;
	node_t         *l   = get_irn_node(get_Sub_left(sub));
	node_t         *r   = get_irn_node(get_Sub_right(sub));
	lattice_elem_t a    = l->type;
	lattice_elem_t b    = r->type;
	node_t         *block = get_irn_node(get_nodes_block(sub));

	if (block->type.tv == tarval_unreachable) {
		node->type.tv = tarval_top;
		return;
	}
	if (a.tv == tarval_top || b.tv == tarval_top) {
		node->type.tv = tarval_top;
	} else if (is_con(a) && is_con(b)) {
		if (is_tarval(a.tv) && is_tarval(b.tv)) {
			node->type.tv = tarval_sub(a.tv, b.tv);
		} else if (is_tarval(a.tv) && tarval_is_null(a.tv)) {
			node->type = b;
		} else if (is_tarval(b.tv) && tarval_is_null(b.tv)) {
			node->type = a;
		} else {
			node->type.tv = tarval_bottom;
		}
	} else if (r->part == l->part &&
	           (!mode_is_float(get_irn_mode(l->node)))) {
		/*
		 * BEWARE: a - a is NOT always 0 for floating Point values, as
		 * NaN op NaN = NaN, so we must check this here.
		 */
		ir_mode *mode = get_irn_mode(sub);
		node->type.tv = get_mode_null(mode);
	} else {
		node->type.tv = tarval_bottom;
	}
}  /* compute_Sub */

/**
 * (Re-)compute the type for a Proj(Cmp).
 *
 * @param node  the node
 * @param cond  the predecessor Cmp node
 */
static void compute_Proj_Cmp(node_t *node, ir_node *cmp) {
	ir_node        *proj = node->node;
	node_t         *l    = get_irn_node(get_Cmp_left(cmp));
	node_t         *r    = get_irn_node(get_Cmp_right(cmp));
	lattice_elem_t a     = l->type;
	lattice_elem_t b     = r->type;
	pn_Cmp         pnc   = get_Proj_proj(proj);

	if (a.tv == tarval_top || b.tv == tarval_top) {
		node->type.tv = tarval_top;
	} else if (is_con(a) && is_con(b)) {
		default_compute(node);
	} else if (r->part == l->part &&
	           (!mode_is_float(get_irn_mode(l->node)) || pnc == pn_Cmp_Lt || pnc == pn_Cmp_Gt)) {
		/*
		 * BEWARE: a == a is NOT always True for floating Point values, as
		 * NaN != NaN is defined, so we must check this here.
		 */
		node->type.tv = new_tarval_from_long(pnc & pn_Cmp_Eq, mode_b);
	} else {
		node->type.tv = tarval_bottom;
	}
}  /* compute_Proj_Cmp */

/**
 * (Re-)compute the type for a Proj(Cond).
 *
 * @param node  the node
 * @param cond  the predecessor Cond node
 */
static void compute_Proj_Cond(node_t *node, ir_node *cond) {
	ir_node *proj     = node->node;
	long    pnc       = get_Proj_proj(proj);
	ir_node *sel      = get_Cond_selector(cond);
	node_t  *selector = get_irn_node(sel);

	if (get_irn_mode(sel) == mode_b) {
		/* an IF */
		if (pnc == pn_Cond_true) {
			if (selector->type.tv == tarval_b_false) {
				node->type.tv = tarval_unreachable;
			} else if (selector->type.tv == tarval_b_true) {
				node->type.tv = tarval_reachable;
			} else if (selector->type.tv == tarval_bottom) {
				node->type.tv = tarval_reachable;
			} else {
				assert(selector->type.tv == tarval_top);
				node->type.tv = tarval_unreachable;
			}
		} else {
			assert(pnc == pn_Cond_false);

			if (selector->type.tv == tarval_b_false) {
				node->type.tv = tarval_reachable;
			} else if (selector->type.tv == tarval_b_true) {
				node->type.tv = tarval_unreachable;
			} else if (selector->type.tv == tarval_bottom) {
				node->type.tv = tarval_reachable;
			} else {
				assert(selector->type.tv == tarval_top);
				node->type.tv = tarval_unreachable;
			}
		}
	} else {
		/* an SWITCH */
		if (selector->type.tv == tarval_bottom) {
			node->type.tv = tarval_reachable;
		} else if (selector->type.tv == tarval_top) {
			node->type.tv = tarval_unreachable;
		} else {
			long value = get_tarval_long(selector->type.tv);
			if (pnc == get_Cond_defaultProj(cond)) {
				/* default switch, have to check ALL other cases */
				int i;

				for (i = get_irn_n_outs(cond) - 1; i >= 0; --i) {
					ir_node *succ = get_irn_out(cond, i);

					if (succ == proj)
						continue;
					if (value == get_Proj_proj(succ)) {
						/* we found a match, will NOT take the default case */
						node->type.tv = tarval_unreachable;
						return;
					}
				}
				/* all cases checked, no match, will take default case */
				node->type.tv = tarval_reachable;
			} else {
				/* normal case */
				node->type.tv = value == pnc ? tarval_reachable : tarval_unreachable;
			}
		}
	}
}  /* compute_Proj_Cond */

/**
 * (Re-)compute the type for a Proj-Nodes.
 *
 * @param node  the node
 */
static void compute_Proj(node_t *node) {
	ir_node *proj = node->node;
	ir_mode *mode = get_irn_mode(proj);
	node_t  *block = get_irn_node(get_nodes_block(skip_Proj(proj)));
	ir_node *pred  = get_Proj_pred(proj);

	if (block->type.tv == tarval_unreachable) {
		/* a Proj node in an unreachable block computes Top
		   except if it's the initial_exec node. */
		if (get_Proj_proj(proj) != pn_Start_X_initial_exec ||
			! is_Start(pred)) {
			node->type.tv = get_top_value(mode);
			return;
		}
	}

	if (mode == mode_M) {
		/* mode M is always bottom */
		node->type.tv = tarval_bottom;
		return;
	}
	if (mode != mode_X) {
		if (is_Cmp(pred))
			compute_Proj_Cmp(node, pred);
		else
			default_compute(node);
		return;
	}
	/* handle mode_X nodes */

	switch (get_irn_opcode(pred)) {
	case iro_Start:
		/* the Proj_X from the Start is always reachable */
		node->type.tv = tarval_reachable;
		break;
	case iro_Cond:
		compute_Proj_Cond(node, pred);
		break;
	default:
		default_compute(node);
	}
}  /* compute_Proj */

/**
 * (Re-)compute the type for a given node.
 *
 * @param node  the node
 */
static void compute(node_t *node) {
	compute_func func = (compute_func)node->node->op->ops.generic;

	if (func != NULL)
		func(node);
}  /* compute */

/**
 * Propagate constant evaluation.
 *
 * @param env  the environment
 */
static void propagate(environment_t *env) {
	partition_t    *X, *Y;
	node_t         *x;
	lattice_elem_t old_type;
	node_t         *fallen;
	unsigned       n_fallen;
	int            i;

	while (env->cprop != NULL) {
		/* remove the first partition X from cprop */
		X          = env->cprop;
		X->on_cprop = 0;
		env->cprop = X->cprop_next;

		DB((dbg, LEVEL_2, "Propagate type on part%d\n", X->nr));
		fallen   = NULL;
		n_fallen = 0;
		while (! list_empty(&X->cprop)) {
			/* remove the first Node x from X.cprop */
			x = list_entry(X->cprop.next, node_t, cprop_list);
			list_del(&x->cprop_list);
			x->on_cprop = 0;

			/* compute a new type for x */
			old_type = x->type;
			DB((dbg, LEVEL_3, "computing type of %+F\n", x->node));
			compute(x);
			if (x->type.tv != old_type.tv) {
				verify_type(old_type, x->type);
				DB((dbg, LEVEL_2, "node %+F has changed type from %+F to %+F\n", x->node, old_type, x->type));

				if (x->on_fallen == 0) {
					/* Add x to fallen. Nodes might fall from T -> const -> _|_, so check that they are
					   not already on the list. */
					x->next      = fallen;
					x->on_fallen = 1;
					fallen       = x;
					++n_fallen;
					DB((dbg, LEVEL_2, "Add node %+F to fallen\n", x->node));
				}
				for (i = get_irn_n_outs(x->node) - 1; i >= 0; --i) {
					ir_node *succ = get_irn_out(x->node, i);
					node_t  *y    = get_irn_node(succ);

					/* Add y to y.partition.cprop. */
					add_node_to_cprop(y, env);
				}
			}
		}

		if (n_fallen > 0 && n_fallen != X->n_nodes) {
			DB((dbg, LEVEL_2, "Splitting part%d by fallen\n", X->nr));
			Y = split(X, fallen, env);
		} else {
			Y = X;
		}
		/* remove the nodes from the fallen list */
		for (x = fallen; x != NULL; x = x->next)
			x->on_fallen = 0;

		if (Y->n_nodes > 1)
			split_by(Y, env);
	}
}  /* propagate */

/**
 * Get the leader for a given node from its congruence class.
 *
 * @param irn  the node
 */
static ir_node *get_leader(node_t *node) {
	partition_t *part = node->part;

	if (part->n_nodes > 1) {
		DB((dbg, LEVEL_2, "Found congruence class for %+F\n", node->node));

		return get_first_node(part)->node;
	}
	return node->node;
}

/**
 * Post-Walker, apply the analysis results;
 */
static void apply_result(ir_node *irn, void *ctx) {
	node_t *node = get_irn_node(irn);

	(void) ctx;
	if (is_Block(irn)) {
		if (irn == get_irg_end_block(current_ir_graph)) {
			/* the EndBlock is always reachable even if the analysis
			   finds out the opposite :-) */
			return;
		}

		if (node->type.tv == tarval_unreachable) {
			/* mark dead blocks */
			set_Block_dead(irn);
		}
	} else if (is_End(irn)) {
		/* do not touch the End node */
	} else {
		node_t *block = get_irn_node(get_nodes_block(irn));

		if (block->type.tv == tarval_unreachable) {
			if (! is_Bad(irn)) {
				ir_node *bad = get_irg_bad(current_ir_graph);

				/* here, bad might already have a node, but this can be safely ignored
				   as long as bad has at least ONE valid node */
				set_irn_node(bad, node);
				node->node = bad;
				DB((dbg, LEVEL_1, "%+F is unreachable\n", irn));
				exchange(irn, bad);
			}
		}
		else if (get_irn_mode(irn) == mode_X) {
			if (node->type.tv == tarval_unreachable) {
				ir_node *bad = get_irg_bad(current_ir_graph);

				/* see comment above */
				set_irn_node(bad, node);
				node->node = bad;
				DB((dbg, LEVEL_1, "%+F is unreachable\n", irn));
				exchange(irn, bad);
			}
			else if (is_Proj(irn)) {
				/* leave or Jmp */
				ir_node *cond = get_Proj_pred(irn);

				if (is_Cond(cond)) {
					node_t *sel = get_irn_node(get_Cond_selector(cond));

					if (is_tarval(sel->type.tv) && tarval_is_constant(sel->type.tv)) {
						/* Cond selector is a constant, make a Jmp */
						ir_node *jmp = new_r_Jmp(current_ir_graph, block->node);
						set_irn_node(jmp, node);
						node->node = jmp;
						DB((dbg, LEVEL_1, "%+F is replaced by %+F\n", irn, jmp));
						exchange(irn, jmp);
					}
				}
			}
		} else {
			/* normal data node */
			if (is_tarval(node->type.tv) && tarval_is_constant(node->type.tv)) {
				tarval *tv = node->type.tv;

				if (! is_Const(irn)) {
					/* can be replaced by a constant */
					ir_node *c = new_r_Const(current_ir_graph, block->node, get_tarval_mode(tv), tv);
					set_irn_node(c, node);
					node->node = c;
					DB((dbg, LEVEL_1, "%+F is replaced by %+F\n", irn, c));
					exchange(irn, c);
				}
			} else if (is_entity(node->type.sym.entity_p)) {
				if (! is_SymConst(irn)) {
					/* can be replaced by a Symconst */
					ir_node *symc = new_r_SymConst(current_ir_graph, block->node, get_irn_mode(irn), node->type.sym, symconst_addr_ent);
					set_irn_node(symc, node);
					node->node = symc;

					DB((dbg, LEVEL_1, "%+F is replaced by %+F\n", irn, symc));
					exchange(irn, symc);
				}
			} else {
				ir_node *leader = get_leader(node);

				if (leader != irn) {
					DB((dbg, LEVEL_1, "%+F from part%d is replaced by %+F\n", irn, node->part->nr, leader));
					exchange(irn, leader);
				}
			}
		}
	}
}  /* static void apply_result(ir_node *irn, void *ctx) {
 */

#define SET(code) op_##code->ops.generic = (op_func)compute_##code

/**
 * sets the generic functions to compute.
 */
static void set_compute_functions(void) {
	int i;

	/* set the default compute function */
	for (i = get_irp_n_opcodes() - 1; i >= 0; --i) {
		ir_op *op = get_irp_opcode(i);
		op->ops.generic = (op_func)default_compute;
	}

	/* set specific functions */
	SET(Block);
	SET(Jmp);
	SET(Phi);
	SET(Add);
	SET(Sub);
	SET(SymConst);
	SET(Proj);
	SET(End);
}  /* set_compute_functions */

static int dump_partition_hook(FILE *F, ir_node *n, ir_node *local) {
	ir_node *irn = local != NULL ? local : n;
	node_t *node = get_irn_node(irn);

	ir_fprintf(F, "info2 : \"partition %u type %+F\"\n", node->part->nr, node->type);
	return 1;
}

void combo(ir_graph *irg) {
	environment_t env;
	ir_node       *initial_X;
	node_t        *start;
	ir_graph      *rem = current_ir_graph;

	current_ir_graph = irg;

	/* register a debug mask */
	FIRM_DBG_REGISTER(dbg, "firm.opt.combo");
	//firm_dbg_set_mask(dbg, SET_LEVEL_3);

	DB((dbg, LEVEL_1, "Doing COMBO for %+F\n", irg));

	obstack_init(&env.obst);
	env.worklist       = NULL;
	env.cprop          = NULL;
	env.touched        = NULL;
	env.initial        = NULL;
#ifdef DEBUG_libfirm
	env.dbg_list       = NULL;
#endif
	env.opcode2id_map  = new_set(cmp_opcode, iro_Last * 4);
	env.type2id_map    = pmap_create();
	env.end_idx        = get_opt_global_cse() ? 0 : -1;
	env.lambda_input   = 0;

	assure_irg_outs(irg);

	/* we have our own value_of function */
	set_value_of_func(get_node_tarval);

	set_compute_functions();
	DEBUG_ONLY(part_nr = 0);

	/* create the initial partition and place it on the work list */
	env.initial = new_partition(&env);
	add_to_worklist(env.initial, &env);
	irg_walk_graph(irg, init_block_phis, create_initial_partitions, &env);

	/* Place the START Node's partition on cprop.
	   Place the START Node on its local worklist. */
	initial_X = get_irg_initial_exec(irg);
	start     = get_irn_node(initial_X);
	add_node_to_cprop(start, &env);

	do {
		propagate(&env);
		if (env.worklist != NULL)
			cause_splits(&env);
	} while (env.cprop != NULL || env.worklist != NULL);

	dump_all_partitions(&env);

	set_dump_node_vcgattr_hook(dump_partition_hook);
	dump_ir_block_graph(irg, "-partition");
	set_dump_node_vcgattr_hook(NULL);


	/* apply the result */
	irg_walk_graph(irg, NULL, apply_result, &env);

	pmap_destroy(env.type2id_map);
	del_set(env.opcode2id_map);
	obstack_free(&env.obst, NULL);

	/* restore value_of() default behavior */
	set_value_of_func(NULL);
	current_ir_graph = rem;
}  /* combo */
