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
 * Note further that we use the terminology from Click's work here, which is different
 * in some cases from Firm terminology.  Especially, Click's type is a
 * Firm tarval/entity, nevertheless we call it type here for "maximum compatibility".
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>

#include "iroptimize.h"
#include "archop.h"
#include "irflag.h"
#include "ircons.h"
#include "list.h"
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
#include "iropt_dbg.h"
#include "debug.h"
#include "array_t.h"
#include "error.h"

#include "tv_t.h"

#include "irprintf.h"
#include "irdump.h"

/* define this to check that all type translations are monotone */
#undef VERIFY_MONOTONE

/* define this to check the consistency of partitions */
#define CHECK_PARTITIONS

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
	int         arity;  /**< The arity of this opcode (needed for Phi etc. */
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
	list_head       node_list;      /**< Double-linked list of leader/follower entries. */
	list_head       cprop_list;     /**< Double-linked partition.cprop list. */
	partition_t     *part;          /**< points to the partition this node belongs to */
	node_t          *next;          /**< Next node on local list (partition.touched, fallen). */
	node_t          *race_next;     /**< Next node on race list. */
	lattice_elem_t  type;           /**< The associated lattice element "type". */
	int             max_user_input; /**< Maximum input number of Def-Use edges. */
	int             next_edge;      /**< Index of the next Def-Use edge to use. */
	int             n_followers;    /**< Number of Follower in the outs set. */
	unsigned        on_touched:1;   /**< Set, if this node is on the partition.touched set. */
	unsigned        on_cprop:1;     /**< Set, if this node is on the partition.cprop list. */
	unsigned        on_fallen:1;    /**< Set, if this node is on the fallen list. */
	unsigned        is_follower:1;  /**< Set, if this node is a follower. */
	unsigned        by_all_const:1; /**< Set, if this node was once evaluated by all constants. */
	unsigned        flagged:2;      /**< 2 Bits, set if this node was visited by race 1 or 2. */
};

/**
 * A partition containing congruent nodes.
 */
struct partition_t {
	list_head         Leader;          /**< The head of partition Leader node list. */
	list_head         Follower;        /**< The head of partition Follower node list. */
	list_head         cprop;           /**< The head of partition.cprop list. */
	partition_t       *wl_next;        /**< Next entry in the work list if any. */
	partition_t       *touched_next;   /**< Points to the next partition in the touched set. */
	partition_t       *cprop_next;     /**< Points to the next partition in the cprop list. */
	partition_t       *split_next;     /**< Points to the next partition in the list that must be split by split_by(). */
	node_t            *touched;        /**< The partition.touched set of this partition. */
	unsigned          n_leader;        /**< Number of entries in this partition.Leader. */
	unsigned          n_touched;       /**< Number of entries in the partition.touched. */
	int               max_user_inputs; /**< Maximum number of user inputs of all entries. */
	unsigned          on_worklist:1;   /**< Set, if this partition is in the work list. */
	unsigned          on_touched:1;    /**< Set, if this partition is on the touched set. */
	unsigned          on_cprop:1;      /**< Set, if this partition is on the cprop list. */
	unsigned          type_is_T_or_C:1;/**< Set, if all nodes in this partition have type Top or Constant. */
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
	char            nonstd_cond;    /**< Set, if a Condb note has a non-Cmp predecessor. */
	char            modified;       /**< Set, if the graph was modified. */
#ifdef DEBUG_libfirm
	partition_t     *dbg_list;      /**< List of all partitions. */
#endif
} environment_t;

/** Type of the what function. */
typedef void *(*what_func)(const node_t *node, environment_t *env);

#define get_irn_node(follower)         ((node_t *)get_irn_link(follower))
#define set_irn_node(follower, node)   set_irn_link(follower, node)

/* we do NOT use tarval_unreachable here, instead we use Top for this purpose */
#undef tarval_unreachable
#define tarval_unreachable tarval_top


/** The debug module handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/** Next partition number. */
DEBUG_ONLY(static unsigned part_nr = 0);

/** The tarval returned by Unknown nodes. */
static tarval *tarval_UNKNOWN;

/* forward */
static node_t *identity(node_t *node);

#ifdef CHECK_PARTITIONS
/**
 * Check a partition.
 */
static void check_partition(const partition_t *T) {
	node_t   *node;
	unsigned n = 0;

	list_for_each_entry(node_t, node, &T->Leader, node_list) {
		assert(node->is_follower == 0);
		assert(node->flagged == 0);
		assert(node->part == T);
		++n;
	}
	assert(n == T->n_leader);

	list_for_each_entry(node_t, node, &T->Follower, node_list) {
		assert(node->is_follower == 1);
		assert(node->flagged == 0);
		assert(node->part == T);
	}
}  /* check_partition */

static void check_all_partitions(environment_t *env) {
	partition_t *P;
	node_t      *node;

#ifdef DEBUG_libfirm
	for (P = env->dbg_list; P != NULL; P = P->dbg_next) {
		check_partition(P);
		list_for_each_entry(node_t, node, &P->Follower, node_list) {
			node_t *leader = identity(node);

			assert(leader != node && leader->part == node->part);
		}
	}
#endif
}

/**
 * Check list.
 */
static void do_check_list(const node_t *list, int ofs, const partition_t *Z) {
	const node_t *e;

#define NEXT(e)  *((const node_t **)((char *)(e) + (ofs)))
	for (e = list; e != NULL; e = NEXT(e)) {
		assert(e->part == Z);
	}
#undef NEXT
}  /* ido_check_list */

/**
 * Check a local list.
 */
static void check_list(const node_t *list, const partition_t *Z) {
	do_check_list(list, offsetof(node_t, next), Z);
}  /* check_list */

#else
#define check_partition(T)
#define check_list(list, Z)
#define check_all_partitions(env)
#endif /* CHECK_PARTITIONS */

#ifdef DEBUG_libfirm
static INLINE lattice_elem_t get_partition_type(const partition_t *X);

/**
 * Dump partition to output.
 */
static void dump_partition(const char *msg, const partition_t *part) {
	const node_t   *node;
	int            first = 1;
	lattice_elem_t type = get_partition_type(part);

	DB((dbg, LEVEL_2, "%s part%u%s (%u, %+F) {\n  ",
		msg, part->nr, part->type_is_T_or_C ? "*" : "",
		part->n_leader, type));
	list_for_each_entry(node_t, node, &part->Leader, node_list) {
		DB((dbg, LEVEL_2, "%s%+F", first ? "" : ", ", node->node));
		first = 0;
	}
	if (! list_empty(&part->Follower)) {
		DB((dbg, LEVEL_2, "\n---\n  "));
		first = 1;
		list_for_each_entry(node_t, node, &part->Follower, node_list) {
			DB((dbg, LEVEL_2, "%s%+F", first ? "" : ", ", node->node));
			first = 0;
		}
	}
	DB((dbg, LEVEL_2, "\n}\n"));
}  /* dump_partition */

/**
 * Dumps a list.
 */
static void do_dump_list(const char *msg, const node_t *node, int ofs) {
	const node_t *p;
	int          first = 1;

#define GET_LINK(p, ofs)  *((const node_t **)((char *)(p) + (ofs)))

	DB((dbg, LEVEL_3, "%s = {\n  ", msg));
	for (p = node; p != NULL; p = GET_LINK(p, ofs)) {
		DB((dbg, LEVEL_3, "%s%+F", first ? "" : ", ", p->node));
		first = 0;
	}
	DB((dbg, LEVEL_3, "\n}\n"));

#undef GET_LINK
}

/**
 * Dumps a race list.
 */
static void dump_race_list(const char *msg, const node_t *list) {
	do_dump_list(msg, list, offsetof(node_t, race_next));
}

/**
 * Dumps a local list.
 */
static void dump_list(const char *msg, const node_t *list) {
	do_dump_list(msg, list, offsetof(node_t, next));
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
#define dump_race_list(msg, list)
#define dump_list(msg, list)
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
	if (old_type.tv == tarval_reachable) {
		panic("verify_type(): wrong translation from %+F to %+F", old_type, new_type);
	}
	if (new_type.tv == tarval_bottom || new_type.tv == tarval_reachable) {
		/* bottom reached */
		return;
	}
	panic("verify_type(): wrong translation from %+F to %+F", old_type, new_type);
}
#else
#define verify_type(old_type, new_type)
#endif

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
	       o1->arity != o2->arity ||
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
	node->max_user_input = irn->out[n_outs].pos;
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
	DB((dbg, LEVEL_2, "Adding part%d to worklist\n", X->nr));
	X->wl_next     = env->worklist;
	X->on_worklist = 1;
	env->worklist  = X;
}  /* add_to_worklist */

/**
 * Create a new empty partition.
 *
 * @param env   the environment
 *
 * @return a newly allocated partition
 */
static INLINE partition_t *new_partition(environment_t *env) {
	partition_t *part = obstack_alloc(&env->obst, sizeof(*part));

	INIT_LIST_HEAD(&part->Leader);
	INIT_LIST_HEAD(&part->Follower);
	INIT_LIST_HEAD(&part->cprop);
	part->wl_next         = NULL;
	part->touched_next    = NULL;
	part->cprop_next      = NULL;
	part->split_next      = NULL;
	part->touched         = NULL;
	part->n_leader        = 0;
	part->n_touched       = 0;
	part->max_user_inputs = 0;
	part->on_worklist     = 0;
	part->on_touched      = 0;
	part->on_cprop        = 0;
	part->type_is_T_or_C  = 0;
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
	return list_entry(X->Leader.next, node_t, node_list);
}  /* get_first_node */

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

	INIT_LIST_HEAD(&node->node_list);
	INIT_LIST_HEAD(&node->cprop_list);
	node->node           = irn;
	node->part           = part;
	node->next           = NULL;
	node->race_next      = NULL;
	node->type.tv        = tarval_top;
	node->max_user_input = 0;
	node->next_edge      = 0;
	node->n_followers    = 0;
	node->on_touched     = 0;
	node->on_cprop       = 0;
	node->on_fallen      = 0;
	node->is_follower    = 0;
	node->by_all_const   = 0;
	node->flagged        = 0;
	set_irn_node(irn, node);

	list_add_tail(&node->node_list, &part->Leader);
	++part->n_leader;

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
}  /* init_block_phis */

/**
 * Post-Walker, initialize all Nodes' type to U or top and place
 * all nodes into the TOP partition.
 */
static void create_initial_partitions(ir_node *irn, void *ctx) {
	environment_t *env  = ctx;
	partition_t   *part = env->initial;
	node_t        *node;

	node = create_partition_node(irn, part, env);
	sort_irn_outs(node);
	if (node->max_user_input > part->max_user_inputs)
		part->max_user_inputs = node->max_user_input;

	if (is_Phi(irn)) {
		add_Block_phi(get_nodes_block(irn), irn);
	} else if (is_Cond(irn)) {
		/* check if all Cond's have a Cmp predecessor. */
		if (get_irn_mode(irn) == mode_b && !is_Cmp(skip_Proj(get_Cond_selector(irn))))
			env->nonstd_cond = 1;

	}
}  /* create_initial_partitions */

/**
 * Add a node to the entry.partition.touched set and
 * node->partition to the touched set if not already there.
 *
 * @param y    a node
 * @param env  the environment
 */
static INLINE void add_to_touched(node_t *y, environment_t *env) {
	if (y->on_touched == 0) {
		partition_t *part = y->part;

		y->next       = part->touched;
		part->touched = y;
		y->on_touched = 1;
		++part->n_touched;

		if (part->on_touched == 0) {
			part->touched_next = env->touched;
			env->touched       = part;
			part->on_touched   = 1;
		}

		check_list(part->touched, part);
	}
}  /* add_to_touched */

/**
 * Place a node on the cprop list.
 *
 * @param y    the node
 * @param env  the environment
 */
static void add_to_cprop(node_t *y, environment_t *env) {
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

			add_to_cprop(proj, env);
		}
	} else if (is_Block(y->node)) {
		/* Due to the way we handle Phi's, we must place all Phis of a block on the list
		 * if someone placed the block. The Block is only placed if the reachability
		 * changes, and this must be re-evaluated in compute_Phi(). */
		ir_node *phi;
		for (phi = get_Block_phis(y->node); phi != NULL; phi = get_Phi_next(phi)) {
			node_t *p = get_irn_node(phi);
			add_to_cprop(p, env);
		}
	}
}  /* add_to_cprop */

/**
 * Update the worklist: If Z is on worklist then add Z' to worklist.
 * Else add the smaller of Z and Z' to worklist.
 *
 * @param Z        the Z partition
 * @param Z_prime  the Z' partition, a previous part of Z
 * @param env      the environment
 */
static void update_worklist(partition_t *Z, partition_t *Z_prime, environment_t *env) {
	if (Z->on_worklist || Z_prime->n_leader < Z->n_leader) {
		add_to_worklist(Z_prime, env);
	} else {
		add_to_worklist(Z, env);
	}
}  /* update_worklist */

/**
 * Make all inputs to x no longer be F.def_use edges.
 *
 * @param x  the node
 */
static void move_edges_to_leader(node_t *x) {
	ir_node     *irn = x->node;
	int         i, j, k;

	for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
		node_t  *pred = get_irn_node(get_irn_n(irn, i));
		ir_node *p;
		int     n;

		p = pred->node;
		n = get_irn_n_outs(p);
		for (j = 1; j <= pred->n_followers; ++j) {
			if (p->out[j].pos == i && p->out[j].use == irn) {
				/* found a follower edge to x, move it to the Leader */
				ir_def_use_edge edge = p->out[j];

				/* remove this edge from the Follower set */
				p->out[j] = p->out[pred->n_followers];
				--pred->n_followers;

				/* sort it into the leader set */
				for (k = pred->n_followers + 2; k <= n; ++k) {
					if (p->out[k].pos >= edge.pos)
						break;
					p->out[k - 1] = p->out[k];
				}
				/* place the new edge here */
				p->out[k - 1] = edge;

				/* edge found and moved */
				break;
			}
		}
	}
}  /* move_edges_to_leader */

/**
 * Split a partition that has NO followers by a local list.
 *
 * @param Z    partition to split
 * @param g    a (non-empty) node list
 * @param env  the environment
 *
 * @return  a new partition containing the nodes of g
 */
static partition_t *split_no_followers(partition_t *Z, node_t *g, environment_t *env) {
	partition_t *Z_prime;
	node_t      *node;
	unsigned    n = 0;
	int         max_input;

	dump_partition("Splitting ", Z);
	dump_list("by list ", g);

	assert(g != NULL);

	/* Remove g from Z. */
	for (node = g; node != NULL; node = node->next) {
		assert(node->part == Z);
		list_del(&node->node_list);
		++n;
	}
	assert(n < Z->n_leader);
	Z->n_leader -= n;

	/* Move g to a new partition, Z'. */
	Z_prime = new_partition(env);
	max_input = 0;
	for (node = g; node != NULL; node = node->next) {
		list_add_tail(&node->node_list, &Z_prime->Leader);
		node->part = Z_prime;
		if (node->max_user_input > max_input)
			max_input = node->max_user_input;
	}
	Z_prime->max_user_inputs = max_input;
	Z_prime->n_leader        = n;

	check_partition(Z);
	check_partition(Z_prime);

	/* for now, copy the type info tag, it will be adjusted in split_by(). */
	Z_prime->type_is_T_or_C = Z->type_is_T_or_C;

	update_worklist(Z, Z_prime, env);

	dump_partition("Now ", Z);
	dump_partition("Created new ", Z_prime);
	return Z_prime;
}  /* split_no_followers */

/**
 * Make the Follower -> Leader transition for a node.
 *
 * @param n  the node
 */
static void follower_to_leader(node_t *n) {
	assert(n->is_follower == 1);

	DB((dbg, LEVEL_2, "%+F make the follower -> leader transition\n", n->node));
	n->is_follower = 0;
	move_edges_to_leader(n);
	list_del(&n->node_list);
	list_add_tail(&n->node_list, &n->part->Leader);
	++n->part->n_leader;
}  /* follower_to_leader */

/**
 * The environment for one race step.
 */
typedef struct step_env {
	node_t   *initial;    /**< The initial node list. */
	node_t   *unwalked;   /**< The unwalked node list. */
	node_t   *walked;     /**< The walked node list. */
	int      index;       /**< Next index of Follower use_def edge. */
	unsigned side;        /**< side number. */
} step_env;

/**
 * Return non-zero, if a input is a real follower
 *
 * @param irn    the node to check
 * @param input  number of the input
 */
static int is_real_follower(const ir_node *irn, int input) {
	node_t *pred;

	switch (get_irn_opcode(irn)) {
	case iro_Confirm:
		if (input == 1) {
			/* ignore the Confirm bound input */
			return 0;
		}
		break;
	case iro_Mux:
		if (input == 0) {
			/* ignore the Mux sel input */
			return 0;
		}
		break;
	case iro_Phi: {
		/* dead inputs are not follower edges */
		ir_node *block = get_nodes_block(irn);
		node_t  *pred  = get_irn_node(get_Block_cfgpred(block, input));

		if (pred->type.tv == tarval_unreachable)
			return 0;
		break;
	}
	case iro_Sub:
	case iro_Shr:
	case iro_Shl:
	case iro_Shrs:
	case iro_Rotl:
		if (input == 1) {
			/* only a Sub x,0 / Shift x,0 might be a follower */
			return 0;
		}
		break;
	case iro_Add:
	case iro_Or:
	case iro_Eor:
		pred = get_irn_node(get_irn_n(irn, input));
		if (is_tarval(pred->type.tv) && tarval_is_null(pred->type.tv))
			return 0;
		break;
	case iro_Mul:
		pred = get_irn_node(get_irn_n(irn, input));
		if (is_tarval(pred->type.tv) && tarval_is_one(pred->type.tv))
			return 0;
		break;
	case iro_And:
		pred = get_irn_node(get_irn_n(irn, input));
		if (is_tarval(pred->type.tv) && tarval_is_all_one(pred->type.tv))
			return 0;
		break;
	case iro_Min:
	case iro_Max:
		/* all inputs are followers */
		return 1;
	default:
		assert(!"opcode not implemented yet");
		break;
	}
	return 1;
}

/**
 * Do one step in the race.
 */
static int step(step_env *env) {
	node_t *n;

	if (env->initial != NULL) {
		/* Move node from initial to unwalked */
		n             = env->initial;
		env->initial  = n->race_next;

		n->race_next  = env->unwalked;
		env->unwalked = n;

		return 0;
	}

	while (env->unwalked != NULL) {
		/* let n be the first node in unwalked */
		n = env->unwalked;
		while (env->index < n->n_followers) {
			const ir_def_use_edge *edge = &n->node->out[1 + env->index];

			/* let m be n.F.def_use[index] */
			node_t *m = get_irn_node(edge->use);

			assert(m->is_follower);
			/*
			 * Some inputs, like the get_Confirm_bound are NOT
			 * real followers, sort them out.
			 */
			if (! is_real_follower(m->node, edge->pos)) {
				++env->index;
				continue;
			}
			++env->index;

			/* only followers from our partition */
			if (m->part != n->part)
				continue;

			if ((m->flagged & env->side) == 0) {
				m->flagged |= env->side;

				if (m->flagged != 3) {
					/* visited the first time */
					/* add m to unwalked not as first node (we might still need to
					   check for more follower node */
					m->race_next = n->race_next;
					n->race_next = m;
					return 0;
				}
				/* else already visited by the other side and on the other list */
			}
		}
		/* move n to walked */
		env->unwalked = n->race_next;
		n->race_next  = env->walked;
		env->walked   = n;
		env->index    = 0;
	}
	return 1;
}  /* step */

/**
 * Clear the flags from a list and check for
 * nodes that where touched from both sides.
 *
 * @param list  the list
 */
static int clear_flags(node_t *list) {
	int    res = 0;
	node_t *n;

	for (n = list; n != NULL; n = n->race_next) {
		if (n->flagged == 3) {
			/* we reach a follower from both sides, this will split congruent
			 * inputs and make it a leader. */
			follower_to_leader(n);
			res = 1;
		}
		n->flagged = 0;
	}
	return res;
}  /* clear_flags */

/**
 * Split a partition by a local list using the race.
 *
 * @param pX   pointer to the partition to split, might be changed!
 * @param gg   a (non-empty) node list
 * @param env  the environment
 *
 * @return  a new partition containing the nodes of gg
 */
static partition_t *split(partition_t **pX, node_t *gg, environment_t *env) {
	partition_t *X = *pX;
	partition_t *X_prime;
	list_head   tmp;
	step_env    env1, env2, *winner;
	node_t      *g, *h, *node, *t;
	int         max_input, transitions;
	unsigned    n;
	DEBUG_ONLY(static int run = 0;)

	DB((dbg, LEVEL_2, "Run %d ", run++));
	if (list_empty(&X->Follower)) {
		/* if the partition has NO follower, we can use the fast
		   splitting algorithm. */
		return split_no_followers(X, gg, env);
	}
	/* else do the race */

	dump_partition("Splitting ", X);
	dump_list("by list ", gg);

	INIT_LIST_HEAD(&tmp);

	/* Remove gg from X.Leader and put into g */
	g = NULL;
	for (node = gg; node != NULL; node = node->next) {
		assert(node->part == X);
		assert(node->is_follower == 0);

		list_del(&node->node_list);
		list_add_tail(&node->node_list, &tmp);
		node->race_next = g;
		g               = node;
	}
	/* produce h */
	h = NULL;
	list_for_each_entry(node_t, node, &X->Leader, node_list) {
		node->race_next = h;
		h               = node;
	}
	/* restore X.Leader */
	list_splice(&tmp, &X->Leader);

	env1.initial       = g;
	env1.unwalked      = NULL;
	env1.walked        = NULL;
	env1.index         = 0;
	env1.side          = 1;

	env2.initial       = h;
	env2.unwalked      = NULL;
	env2.walked        = NULL;
	env2.index         = 0;
	env2.side          = 2;

	for (;;) {
		if (step(&env1)) {
			winner = &env1;
			break;
		}
		if (step(&env2)) {
			winner = &env2;
			break;
		}
	}
	assert(winner->initial == NULL);
	assert(winner->unwalked == NULL);

	/* clear flags from walked/unwalked */
	transitions  = clear_flags(env1.unwalked);
	transitions |= clear_flags(env1.walked);
	transitions |= clear_flags(env2.unwalked);
	transitions |= clear_flags(env2.walked);

	dump_race_list("winner ", winner->walked);

	/* Move walked_{winner} to a new partition, X'. */
	X_prime   = new_partition(env);
	max_input = 0;
	n         = 0;
	for (node = winner->walked; node != NULL; node = node->race_next) {
		list_del(&node->node_list);
		node->part = X_prime;
		if (node->is_follower) {
			list_add_tail(&node->node_list, &X_prime->Follower);
		} else {
			list_add_tail(&node->node_list, &X_prime->Leader);
			++n;
		}
		if (node->max_user_input > max_input)
			max_input = node->max_user_input;
	}
	X_prime->n_leader        = n;
	X_prime->max_user_inputs = max_input;
	X->n_leader             -= X_prime->n_leader;

	/* for now, copy the type info tag, it will be adjusted in split_by(). */
	X_prime->type_is_T_or_C = X->type_is_T_or_C;

	/*
	 * Even if a follower was not checked by both sides, it might have
	 * loose its congruence, so we need to check this case for all follower.
	 */
	list_for_each_entry_safe(node_t, node, t, &X_prime->Follower, node_list) {
		if (identity(node) == node) {
			follower_to_leader(node);
			transitions = 1;
		}
	}

	check_partition(X);
	check_partition(X_prime);

	/* X' is the smaller part */
	add_to_worklist(X_prime, env);

	/*
	 * If there where follower to leader transitions, ensure that the nodes
	 * can be split out if necessary.
	 */
	if (transitions) {
		/* place partitions on the cprop list */
                if (X_prime->on_cprop == 0) {
			X_prime->cprop_next = env->cprop;
			env->cprop          = X_prime;
			X_prime->on_cprop   = 1;
		}
	}

	dump_partition("Now ", X);
	dump_partition("Created new ", X_prime);

	/* we have to ensure that the partition containing g is returned */
	if (winner == &env2) {
		*pX = X_prime;
		return X;
	}

	return X_prime;
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
 * Check whether a type is neither Top or a constant.
 * Note: U is handled like Top here, R is a constant.
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
 * Collect nodes to the touched list.
 *
 * @param list  the list which contains the nodes that must be evaluated
 * @param idx   the index of the def_use edge to evaluate
 * @param env   the environment
 */
static void collect_touched(list_head *list, int idx, environment_t *env) {
	node_t  *x, *y;
	int     end_idx = env->end_idx;

	list_for_each_entry(node_t, x, list, node_list) {
		int num_edges;

		if (idx == -1) {
			/* leader edges start AFTER follower edges */
			x->next_edge = x->n_followers + 1;
		}
		num_edges = get_irn_n_outs(x->node);

		/* for all edges in x.L.def_use_{idx} */
		while (x->next_edge <= num_edges) {
			const ir_def_use_edge *edge = &x->node->out[x->next_edge];
			ir_node               *succ;

			/* check if we have necessary edges */
			if (edge->pos > idx)
				break;

			++x->next_edge;

			succ = edge->use;

			/* ignore the "control input" for non-pinned nodes
			if we are running in GCSE mode */
			if (idx < end_idx && get_irn_pinned(succ) != op_pin_state_pinned)
				continue;

			y = get_irn_node(succ);
			assert(get_irn_n(succ, idx) == x->node);

			/* ignore block edges touching followers */
			if (idx == -1 && y->is_follower)
				continue;

			if (is_constant_type(y->type)) {
				ir_opcode code = get_irn_opcode(succ);
				if (code == iro_Sub || code == iro_Eor || code == iro_Cmp)
					add_to_cprop(y, env);
			}

			/* Partitions of constants should not be split simply because their Nodes have unequal
			   functions or incongruent inputs. */
			if (type_is_neither_top_nor_const(y->type) &&
				(! is_Phi(y->node) || is_live_input(y->node, idx))) {
					add_to_touched(y, env);
			}
		}
	}
}  /* collect_touched */

/**
 * Split the partitions if caused by the first entry on the worklist.
 *
 * @param env  the environment
 */
static void cause_splits(environment_t *env) {
	partition_t *X, *Z, *N;
	int         idx;

	/* remove the first partition from the worklist */
	X = env->worklist;
	env->worklist  = X->wl_next;
	X->on_worklist = 0;

	dump_partition("Cause_split: ", X);

	/* combine temporary leader and follower list */
	for (idx = -1; idx <= X->max_user_inputs; ++idx) {
		/* empty the touched set: already done, just clear the list */
		env->touched = NULL;

		collect_touched(&X->Leader, idx, env);
		collect_touched(&X->Follower, idx, env);

		for (Z = env->touched; Z != NULL; Z = N) {
			node_t   *e;
			node_t   *touched  = Z->touched;
			unsigned n_touched = Z->n_touched;

			assert(Z->touched != NULL);

			/* beware, split might change Z */
			N = Z->touched_next;

			/* remove it from the touched set */
			Z->on_touched = 0;

			/* Empty local Z.touched. */
			for (e = touched; e != NULL; e = e->next) {
				assert(e->is_follower == 0);
				e->on_touched = 0;
			}
			Z->touched   = NULL;
			Z->n_touched = 0;

			if (0 < n_touched && n_touched < Z->n_leader) {
				DB((dbg, LEVEL_2, "Split part%d by touched\n", Z->nr));
				split(&Z, touched, env);
			} else
				assert(n_touched <= Z->n_leader);
		}
	}
}  /* cause_splits */

/**
 * Implements split_by_what(): Split a partition by characteristics given
 * by the what function.
 *
 * @param X     the partition to split
 * @param What  a function returning an Id for every node of the partition X
 * @param P     a list to store the result partitions
 * @param env   the environment
 *
 * @return *P
 */
static partition_t *split_by_what(partition_t *X, what_func What,
                                  partition_t **P, environment_t *env) {
	node_t          *x, *S;
	listmap_t       map;
	listmap_entry_t *iter;
	partition_t     *R;

	/* Let map be an empty mapping from the range of What to (local) list of Nodes. */
	listmap_init(&map);
	list_for_each_entry(node_t, x, &X->Leader, node_list) {
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
		R = split(&X, S, env);
		R->split_next = *P;
		*P            = R;
	}
	/* Add X to P. */
	X->split_next = *P;
	*P            = X;

	listmap_term(&map);
	return *P;
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
	key.arity  = get_irn_arity(irn);
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
 * Returns true if a type is a constant.
 */
static int is_con(const lattice_elem_t type) {
	/* be conservative */
	if (is_tarval(type.tv))
		return tarval_is_constant(type.tv);
	return is_entity(type.sym.entity_p);
}  /* is_con */

/**
 * Implements split_by().
 *
 * @param X    the partition to split
 * @param env  the environment
 */
static void split_by(partition_t *X, environment_t *env) {
	partition_t *I, *P = NULL;
	int         input;

	dump_partition("split_by", X);

	if (X->n_leader == 1) {
		/* we have only one leader, no need to split, just check it's type */
		node_t *x = get_first_node(X);
		X->type_is_T_or_C = x->type.tv == tarval_top || is_con(x->type);
		return;
	}

	DB((dbg, LEVEL_2, "WHAT = lambda n.(n.type) on part%d\n", X->nr));
	P = split_by_what(X, lambda_type, &P, env);

	/* adjust the type tags, we have split partitions by type */
	for (I = P; I != NULL; I = I->split_next) {
		node_t *x = get_first_node(I);
		I->type_is_T_or_C = x->type.tv == tarval_top || is_con(x->type);
	}

	do {
		partition_t *Y = P;

		P = P->split_next;
		if (Y->n_leader > 1) {
			/* we do not want split the TOP or constant partitions */
			if (! Y->type_is_T_or_C) {
				partition_t *Q = NULL;

				DB((dbg, LEVEL_2, "WHAT = lambda n.(n.opcode) on part%d\n", Y->nr));
				Q = split_by_what(Y, lambda_opcode, &Q, env);

				do {
					partition_t *Z = Q;

					Q = Q->split_next;
					if (Z->n_leader > 1) {
						const node_t *first = get_first_node(Z);
						int          arity  = get_irn_arity(first->node);
						partition_t  *R, *S;

						/*
						 * BEWARE: during splitting by input 2 for instance we might
						 * create new partitions which are different by input 1, so collect
						 * them and split further.
						 */
						Z->split_next = NULL;
						R             = Z;
						S             = NULL;
						for (input = arity - 1; input >= -1; --input) {
							do {
								partition_t *Z_prime = R;

								R = R->split_next;
								if (Z_prime->n_leader > 1) {
									env->lambda_input = input;
									DB((dbg, LEVEL_2, "WHAT = lambda n.(n[%d].partition) on part%d\n", input, Z_prime->nr));
									S = split_by_what(Z_prime, lambda_partition, &S, env);
								} else {
									Z_prime->split_next = S;
									S                   = Z_prime;
								}
							} while (R != NULL);
							R = S;
							S = NULL;
						}
					}
				} while (Q != NULL);
			}
		}
	} while (P != NULL);
}  /* split_by */

/**
 * (Re-)compute the type for a given node.
 *
 * @param node  the node
 */
static void default_compute(node_t *node) {
	int     i;
	ir_node *irn = node->node;
	node_t  *block = get_irn_node(get_nodes_block(irn));

	if (block->type.tv == tarval_unreachable) {
		node->type.tv = tarval_top;
		return;
	}

	/* if any of the data inputs have type top, the result is type top */
	for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(irn, i);
		node_t  *p    = get_irn_node(pred);

		if (p->type.tv == tarval_top) {
			node->type.tv = tarval_top;
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

	if (block == get_irg_start_block(current_ir_graph)) {
		/* start block is always reachable */
		node->type.tv = tarval_reachable;
		return;
	}

	for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
		node_t *pred = get_irn_node(get_Block_cfgpred(block, i));

		if (pred->type.tv == tarval_reachable) {
			/* A block is reachable, if at least of predecessor is reachable. */
			node->type.tv = tarval_reachable;
			return;
		}
	}
	node->type.tv = tarval_top;
}  /* compute_Block */

/**
 * (Re-)compute the type for a Bad node.
 *
 * @param node  the node
 */
static void compute_Bad(node_t *node) {
	/* Bad nodes ALWAYS compute Top */
	node->type.tv = tarval_top;
}  /* compute_Bad */

/**
 * (Re-)compute the type for an Unknown node.
 *
 * @param node  the node
 */
static void compute_Unknown(node_t *node) {
	/* While Unknown nodes should compute Top this is dangerous:
	 * a Top input to a Cond would lead to BOTH control flows unreachable.
	 * While this is correct in the given semantics, it would destroy the Firm
	 * graph.
	 *
	 * It would be safe to compute Top IF it can be assured, that only Cmp
	 * nodes are inputs to Conds. We check that first.
	 * This is the way Frontends typically build Firm, but some optimizations
	 * (cond_eval for instance) might replace them by Phib's...
	 */
	node->type.tv = tarval_UNKNOWN;
}  /* compute_Unknown */

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
	ir_mode        *mode;

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
	tarval         *tv;

	if (a.tv == tarval_top || b.tv == tarval_top) {
		node->type.tv = tarval_top;
	} else if (is_con(a) && is_con(b)) {
		if (is_tarval(a.tv) && is_tarval(b.tv)) {
			node->type.tv = tarval_sub(a.tv, b.tv, get_irn_mode(sub));
		} else if (is_tarval(a.tv) && tarval_is_null(a.tv)) {
			node->type = b;
		} else if (is_tarval(b.tv) && tarval_is_null(b.tv)) {
			node->type = a;
		} else {
			node->type.tv = tarval_bottom;
		}
		node->by_all_const = 1;
	} else if (r->part == l->part &&
	           (!mode_is_float(get_irn_mode(l->node)))) {
		/*
		 * BEWARE: a - a is NOT always 0 for floating Point values, as
		 * NaN op NaN = NaN, so we must check this here.
		 */
		ir_mode *mode = get_irn_mode(sub);
		tv = get_mode_null(mode);

		/* if the node was ONCE evaluated by all constants, but now
		   this breakes AND we cat by partition a different result, switch to bottom.
		   This happens because initially all nodes are in the same partition ... */
		if (node->by_all_const && node->type.tv != tv)
			tv = tarval_bottom;
		node->type.tv = tv;
	} else {
		node->type.tv = tarval_bottom;
	}
}  /* compute_Sub */

/**
 * (Re-)compute the type for an Eor. Special case: both nodes are congruent.
 *
 * @param node  the node
 */
static void compute_Eor(node_t *node) {
	ir_node        *eor = node->node;
	node_t         *l   = get_irn_node(get_Eor_left(eor));
	node_t         *r   = get_irn_node(get_Eor_right(eor));
	lattice_elem_t a    = l->type;
	lattice_elem_t b    = r->type;
	tarval         *tv;

	if (a.tv == tarval_top || b.tv == tarval_top) {
		node->type.tv = tarval_top;
	} else if (is_con(a) && is_con(b)) {
		if (is_tarval(a.tv) && is_tarval(b.tv)) {
			node->type.tv = tarval_eor(a.tv, b.tv);
		} else if (is_tarval(a.tv) && tarval_is_null(a.tv)) {
			node->type = b;
		} else if (is_tarval(b.tv) && tarval_is_null(b.tv)) {
			node->type = a;
		} else {
			node->type.tv = tarval_bottom;
		}
		node->by_all_const = 1;
	} else if (r->part == l->part) {
		ir_mode *mode = get_irn_mode(eor);
		tv = get_mode_null(mode);

		/* if the node was ONCE evaluated by all constants, but now
		   this breakes AND we cat by partition a different result, switch to bottom.
		   This happens because initially all nodes are in the same partition ... */
		if (node->by_all_const && node->type.tv != tv)
			tv = tarval_bottom;
		node->type.tv = tv;
	} else {
		node->type.tv = tarval_bottom;
	}
}  /* compute_Eor */

/**
 * (Re-)compute the type for Cmp.
 *
 * @param node  the node
 */
static void compute_Cmp(node_t *node) {
	ir_node        *cmp  = node->node;
	node_t         *l    = get_irn_node(get_Cmp_left(cmp));
	node_t         *r    = get_irn_node(get_Cmp_right(cmp));
	lattice_elem_t a     = l->type;
	lattice_elem_t b     = r->type;

	if (a.tv == tarval_top || b.tv == tarval_top) {
#ifdef WITH_UNKNOWN
		/*
		 * Top is congruent to any other value, we can
		 * calculate the compare result.
		 */
		node->type.tv = tarval_b_true;
#else
		node->type.tv = tarval_top;
#endif
	} else if (is_con(a) && is_con(b)) {
		/* both nodes are constants, we can probably do something */
		node->type.tv = tarval_b_true;
	} else if (r->part == l->part) {
		/* both nodes congruent, we can probably do something */
		node->type.tv = tarval_b_true;
	} else {
		node->type.tv = tarval_bottom;
	}
}  /* compute_Proj_Cmp */

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
	tarval         *tv;

	if (a.tv == tarval_top || b.tv == tarval_top) {
#ifdef WITH_UNKNOWN
		/* see above */
		tv = new_tarval_from_long((pnc & pn_Cmp_Eq) ^ pn_Cmp_Eq, mode_b);
		goto not_equal;
#else
		node->type.tv = tarval_top;
#endif
	} else if (is_con(a) && is_con(b)) {
		default_compute(node);
		node->by_all_const = 1;
	} else if (r->part == l->part &&
	           (!mode_is_float(get_irn_mode(l->node)) || pnc == pn_Cmp_Lt || pnc == pn_Cmp_Gt)) {
		/*
		 * BEWARE: a == a is NOT always True for floating Point values, as
		 * NaN != NaN is defined, so we must check this here.
		 */
		tv = new_tarval_from_long(pnc & pn_Cmp_Eq, mode_b);
#ifdef WITH_UNKNOWN
not_equal:
#endif

		/* if the node was ONCE evaluated by all constants, but now
		   this breakes AND we cat by partition a different result, switch to bottom.
		   This happens because initially all nodes are in the same partition ... */
		if (node->by_all_const && node->type.tv != tv)
			tv = tarval_bottom;
		node->type.tv = tv;
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
 * (Re-)compute the type for a Proj-Node.
 *
 * @param node  the node
 */
static void compute_Proj(node_t *node) {
	ir_node *proj = node->node;
	ir_mode *mode = get_irn_mode(proj);
	node_t  *block = get_irn_node(get_nodes_block(skip_Proj(proj)));
	ir_node *pred  = get_Proj_pred(proj);

	if (block->type.tv == tarval_unreachable) {
		/* a Proj in a unreachable Block stay Top */
		node->type.tv = tarval_top;
		return;
	}
	if (get_irn_node(pred)->type.tv == tarval_top) {
		/* if the predecessor is Top, its Proj follow */
		node->type.tv = tarval_top;
		return;
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
		/* the Proj_X from the Start is always reachable.
		   However this is already handled at the top. */
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
 * (Re-)compute the type for a Confirm.
 *
 * @param node  the node
 */
static void compute_Confirm(node_t *node) {
	ir_node *confirm = node->node;
	node_t  *pred = get_irn_node(get_Confirm_value(confirm));

	if (get_Confirm_cmp(confirm) == pn_Cmp_Eq) {
		node_t *bound = get_irn_node(get_Confirm_bound(confirm));

		if (is_con(bound->type)) {
			/* is equal to a constant */
			node->type = bound->type;
			return;
		}
	}
	/* a Confirm is a copy OR a Const */
	node->type = pred->type;
}  /* compute_Confirm */

/**
 * (Re-)compute the type for a Max.
 *
 * @param node  the node
 */
static void compute_Max(node_t *node) {
	ir_node        *op   = node->node;
	node_t         *l    = get_irn_node(get_binop_left(op));
	node_t         *r    = get_irn_node(get_binop_right(op));
	lattice_elem_t a     = l->type;
	lattice_elem_t b     = r->type;

	if (a.tv == tarval_top || b.tv == tarval_top) {
		node->type.tv = tarval_top;
	} else if (is_con(a) && is_con(b)) {
		/* both nodes are constants, we can probably do something */
		if (a.tv == b.tv) {
			/* this case handles symconsts as well */
			node->type = a;
		} else {
			ir_mode *mode   = get_irn_mode(op);
			tarval  *tv_min = get_mode_min(mode);

			if (a.tv == tv_min)
				node->type = b;
			else if (b.tv == tv_min)
				node->type = a;
			else if (is_tarval(a.tv) && is_tarval(b.tv)) {
				if (tarval_cmp(a.tv, b.tv) & pn_Cmp_Gt)
					node->type.tv = a.tv;
				else
					node->type.tv = b.tv;
			} else {
				node->type.tv = tarval_bad;
			}
		}
	} else if (r->part == l->part) {
		/* both nodes congruent, we can probably do something */
		node->type = a;
	} else {
		node->type.tv = tarval_bottom;
	}
}  /* compute_Max */

/**
 * (Re-)compute the type for a Min.
 *
 * @param node  the node
 */
static void compute_Min(node_t *node) {
	ir_node        *op   = node->node;
	node_t         *l    = get_irn_node(get_binop_left(op));
	node_t         *r    = get_irn_node(get_binop_right(op));
	lattice_elem_t a     = l->type;
	lattice_elem_t b     = r->type;

	if (a.tv == tarval_top || b.tv == tarval_top) {
		node->type.tv = tarval_top;
	} else if (is_con(a) && is_con(b)) {
		/* both nodes are constants, we can probably do something */
		if (a.tv == b.tv) {
			/* this case handles symconsts as well */
			node->type = a;
		} else {
			ir_mode *mode   = get_irn_mode(op);
			tarval  *tv_max = get_mode_max(mode);

			if (a.tv == tv_max)
				node->type = b;
			else if (b.tv == tv_max)
				node->type = a;
			else if (is_tarval(a.tv) && is_tarval(b.tv)) {
				if (tarval_cmp(a.tv, b.tv) & pn_Cmp_Gt)
					node->type.tv = a.tv;
				else
					node->type.tv = b.tv;
			} else {
				node->type.tv = tarval_bad;
			}
		}
	} else if (r->part == l->part) {
		/* both nodes congruent, we can probably do something */
		node->type = a;
	} else {
		node->type.tv = tarval_bottom;
	}
}  /* compute_Min */

/**
 * (Re-)compute the type for a given node.
 *
 * @param node  the node
 */
static void compute(node_t *node) {
	compute_func func;

	if (is_no_Block(node->node)) {
		node_t *block = get_irn_node(get_nodes_block(node->node));

		if (block->type.tv == tarval_unreachable) {
			node->type.tv = tarval_top;
			return;
		}
	}

	func = (compute_func)node->node->op->ops.generic;
	if (func != NULL)
		func(node);
}  /* compute */

/*
 * Identity functions: Note that one might thing that identity() is just a
 * synonym for equivalent_node(). While this is true, we cannot use it for the algorithm
 * here, because it expects that the identity node is one of the inputs, which is NOT
 * always true for equivalent_node() which can handle (and does sometimes) DAGs.
 * So, we have our own implementation, which copies some parts of equivalent_node()
 */

/**
 * Calculates the Identity for Phi nodes
 */
static node_t *identity_Phi(node_t *node) {
	ir_node *phi    = node->node;
	ir_node *block  = get_nodes_block(phi);
	node_t  *n_part = NULL;
	int     i;

	for (i = get_Phi_n_preds(phi) - 1; i >= 0; --i) {
		node_t *pred_X = get_irn_node(get_Block_cfgpred(block, i));

		if (pred_X->type.tv == tarval_reachable) {
			node_t *pred = get_irn_node(get_Phi_pred(phi, i));

			if (n_part == NULL)
				n_part = pred;
			else if (n_part->part != pred->part) {
				/* incongruent inputs, not a follower */
				return node;
			}
		}
	}
	/* if n_part is NULL here, all inputs path are dead, the Phi computes
	 * tarval_top, is in the TOP partition and should NOT being split! */
	assert(n_part != NULL);
	return n_part;
}  /* identity_Phi */

/**
 * Calculates the Identity for commutative 0 neutral nodes.
 */
static node_t *identity_comm_zero_binop(node_t *node) {
	ir_node *op   = node->node;
	node_t  *a    = get_irn_node(get_binop_left(op));
	node_t  *b    = get_irn_node(get_binop_right(op));
	ir_mode *mode = get_irn_mode(op);
	tarval  *zero;

	/* for FP these optimizations are only allowed if fp_strict_algebraic is disabled */
	if (mode_is_float(mode) && (get_irg_fp_model(current_ir_graph) & fp_strict_algebraic))
		return node;

	/* node: no input should be tarval_top, else the binop would be also
	 * Top and not being split. */
	zero = get_mode_null(mode);
	if (a->type.tv == zero)
		return b;
	if (b->type.tv == zero)
		return a;
	return node;
}  /* identity_comm_zero_binop */

/**
 * Calculates the Identity for Shift nodes.
 */
static node_t *identity_shift(node_t *node) {
	ir_node *op   = node->node;
	node_t  *b    = get_irn_node(get_binop_right(op));
	ir_mode *mode = get_irn_mode(b->node);
	tarval  *zero;

	/* node: no input should be tarval_top, else the binop would be also
	 * Top and not being split. */
	zero = get_mode_null(mode);
	if (b->type.tv == zero)
		return get_irn_node(get_binop_left(op));
	return node;
}  /* identity_shift */

/**
 * Calculates the Identity for Mul nodes.
 */
static node_t *identity_Mul(node_t *node) {
	ir_node *op   = node->node;
	node_t  *a    = get_irn_node(get_Mul_left(op));
	node_t  *b    = get_irn_node(get_Mul_right(op));
	ir_mode *mode = get_irn_mode(op);
	tarval  *one;

	/* for FP these optimizations are only allowed if fp_strict_algebraic is disabled */
	if (mode_is_float(mode) && (get_irg_fp_model(current_ir_graph) & fp_strict_algebraic))
		return node;

	/* node: no input should be tarval_top, else the binop would be also
	 * Top and not being split. */
	one = get_mode_one(mode);
	if (a->type.tv == one)
		return b;
	if (b->type.tv == one)
		return a;
	return node;
}  /* identity_Mul */

/**
 * Calculates the Identity for Sub nodes.
 */
static node_t *identity_Sub(node_t *node) {
	ir_node *sub  = node->node;
	node_t  *b    = get_irn_node(get_Sub_right(sub));
	ir_mode *mode = get_irn_mode(sub);

	/* for FP these optimizations are only allowed if fp_strict_algebraic is disabled */
	if (mode_is_float(mode) && (get_irg_fp_model(current_ir_graph) & fp_strict_algebraic))
		return node;

	/* node: no input should be tarval_top, else the binop would be also
	 * Top and not being split. */
	if (b->type.tv == get_mode_null(mode))
		return get_irn_node(get_Sub_left(sub));
	return node;
}  /* identity_Mul */

/**
 * Calculates the Identity for And nodes.
 */
static node_t *identity_And(node_t *node) {
	ir_node *and = node->node;
	node_t  *a   = get_irn_node(get_And_left(and));
	node_t  *b   = get_irn_node(get_And_right(and));
	tarval  *neutral = get_mode_all_one(get_irn_mode(and));

	/* node: no input should be tarval_top, else the And would be also
	 * Top and not being split. */
	if (a->type.tv == neutral)
		return b;
	if (b->type.tv == neutral)
		return a;
	return node;
}  /* identity_And */

/**
 * Calculates the Identity for Confirm nodes.
 */
static node_t *identity_Confirm(node_t *node) {
	ir_node *confirm = node->node;

	/* a Confirm is always a Copy */
	return get_irn_node(get_Confirm_value(confirm));
}  /* identity_Confirm */

/**
 * Calculates the Identity for Mux nodes.
 */
static node_t *identity_Mux(node_t *node) {
	ir_node *mux = node->node;
	node_t  *t   = get_irn_node(get_Mux_true(mux));
	node_t  *f   = get_irn_node(get_Mux_false(mux));
	/*node_t  *sel; */

	if (t->part == f->part)
		return t;

	/* for now, the 1-input identity is not supported */
#if 0
	sel = get_irn_node(get_Mux_sel(mux));

	/* Mux sel input is mode_b, so it is always a tarval */
	if (sel->type.tv == tarval_b_true)
		return t;
	if (sel->type.tv == tarval_b_false)
		return f;
#endif
	return node;
}  /* identity_Mux */

/**
 * Calculates the Identity for Min nodes.
 */
static node_t *identity_Min(node_t *node) {
	ir_node *op   = node->node;
	node_t  *a    = get_irn_node(get_binop_left(op));
	node_t  *b    = get_irn_node(get_binop_right(op));
	ir_mode *mode = get_irn_mode(op);
	tarval  *tv_max;

	if (a->part == b->part) {
		/* leader of multiple predecessors */
		return a;
	}

	/* works even with NaN */
	tv_max = get_mode_max(mode);
	if (a->type.tv == tv_max)
		return b;
	if (b->type.tv == tv_max)
		return a;
	return node;
}  /* identity_Min */

/**
 * Calculates the Identity for Max nodes.
 */
static node_t *identity_Max(node_t *node) {
	ir_node *op   = node->node;
	node_t  *a    = get_irn_node(get_binop_left(op));
	node_t  *b    = get_irn_node(get_binop_right(op));
	ir_mode *mode = get_irn_mode(op);
	tarval  *tv_min;

	if (a->part == b->part) {
		/* leader of multiple predecessors */
		return a;
	}

	/* works even with NaN */
	tv_min = get_mode_min(mode);
	if (a->type.tv == tv_min)
		return b;
	if (b->type.tv == tv_min)
		return a;
	return node;
}  /* identity_Max */

/**
 * Calculates the Identity for nodes.
 */
static node_t *identity(node_t *node) {
	ir_node *irn = node->node;

	switch (get_irn_opcode(irn)) {
	case iro_Phi:
		return identity_Phi(node);
	case iro_Mul:
		return identity_Mul(node);
	case iro_Add:
	case iro_Or:
	case iro_Eor:
		return identity_comm_zero_binop(node);
	case iro_Shr:
	case iro_Shl:
	case iro_Shrs:
	case iro_Rotl:
		return identity_shift(node);
	case iro_And:
		return identity_And(node);
	case iro_Sub:
		return identity_Sub(node);
	case iro_Confirm:
		return identity_Confirm(node);
	case iro_Mux:
		return identity_Mux(node);
	case iro_Min:
		return identity_Min(node);
	case iro_Max:
		return identity_Max(node);
	default:
		return node;
	}
}  /* identity */

/**
 * Node follower is a (new) follower of leader, segregate Leader
 * out edges.
 */
static void segregate_def_use_chain_1(const ir_node *follower, node_t *leader) {
	ir_node *l   = leader->node;
	int     j, i, n = get_irn_n_outs(l);

	DB((dbg, LEVEL_2, "%+F is a follower of %+F\n", follower, leader->node));
	/* The leader edges must remain sorted, but follower edges can
	   be unsorted. */
	for (i = leader->n_followers + 1; i <= n; ++i) {
		if (l->out[i].use == follower) {
			ir_def_use_edge t = l->out[i];

			for (j = i - 1; j >= leader->n_followers + 1; --j)
				l->out[j + 1] = l->out[j];
			++leader->n_followers;
			l->out[leader->n_followers] = t;
			break;
		}
	}
}  /* segregate_def_use_chain_1 */

/**
 * Node follower is a (new) follower of leader, segregate Leader
 * out edges. If follower is a n-congruent Input identity, all follower
 * inputs congruent to follower are also leader.
 *
 * @param follower  the follower IR node
 */
static void segregate_def_use_chain(const ir_node *follower) {
	int i;

	for (i = get_irn_arity(follower) - 1; i >= 0; --i) {
		node_t *pred = get_irn_node(get_irn_n(follower, i));

		segregate_def_use_chain_1(follower, pred);
	}
}  /* segregate_def_use_chain */

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
	unsigned       n_fallen, old_type_was_T_or_C;
	int            i;

	while (env->cprop != NULL) {
		void *oldopcode = NULL;

		/* remove the first partition X from cprop */
		X           = env->cprop;
		X->on_cprop = 0;
		env->cprop  = X->cprop_next;

		old_type_was_T_or_C = X->type_is_T_or_C;

		DB((dbg, LEVEL_2, "Propagate type on part%d\n", X->nr));
		fallen   = NULL;
		n_fallen = 0;
		while (! list_empty(&X->cprop)) {
			/* remove the first Node x from X.cprop */
			x = list_entry(X->cprop.next, node_t, cprop_list);
			//assert(x->part == X);
			list_del(&x->cprop_list);
			x->on_cprop = 0;

			if (x->is_follower && identity(x) == x) {
				/* check the opcode first */
				if (oldopcode == NULL) {
					oldopcode = lambda_opcode(get_first_node(X), env);
				}
				if (oldopcode != lambda_opcode(x, env)) {
					if (x->on_fallen == 0) {
						/* different opcode -> x falls out of this partition */
						x->next      = fallen;
						x->on_fallen = 1;
						fallen       = x;
						++n_fallen;
						DB((dbg, LEVEL_2, "Add node %+F to fallen\n", x->node));
					}
				}

				/* x will make the follower -> leader transition */
				follower_to_leader(x);
			}

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
					add_to_cprop(y, env);
				}
			}
		}

		if (n_fallen > 0 && n_fallen != X->n_leader) {
			DB((dbg, LEVEL_2, "Splitting part%d by fallen\n", X->nr));
			Y = split(&X, fallen, env);
			/*
			 * We have split out fallen node. The type of the result
			 * partition is NOT set yet.
			 */
			Y->type_is_T_or_C = 0;
		} else {
			Y = X;
		}
		/* remove the flags from the fallen list */
		for (x = fallen; x != NULL; x = x->next)
			x->on_fallen = 0;

		if (old_type_was_T_or_C) {
			node_t *y, *tmp;

			/* check if some nodes will make the leader -> follower transition */
			list_for_each_entry_safe(node_t, y, tmp, &Y->Leader, node_list) {
				if (y->type.tv != tarval_top && ! is_con(y->type)) {
					node_t *eq_node = identity(y);

					if (eq_node != y && eq_node->part == y->part) {
						DB((dbg, LEVEL_2, "Node %+F is a follower of %+F\n", y->node, eq_node->node));
						/* move to Follower */
						y->is_follower = 1;
						list_del(&y->node_list);
						list_add_tail(&y->node_list, &Y->Follower);
						--Y->n_leader;

						segregate_def_use_chain(y->node);
					}
				}
			}
		}
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

	if (part->n_leader > 1 || node->is_follower) {
		if (node->is_follower) {
			DB((dbg, LEVEL_2, "Replacing follower %+F\n", node->node));
		}
		else
			DB((dbg, LEVEL_2, "Found congruence class for %+F\n", node->node));

		return get_first_node(part)->node;
	}
	return node->node;
}  /* get_leader */

/**
 * Return non-zero if the control flow predecessor node pred
 * is the only reachable control flow exit of its block.
 *
 * @param pred  the control flow exit
 */
static int can_exchange(ir_node *pred) {
	if (is_Start(pred))
		return 0;
	else if (is_Jmp(pred))
		return 1;
	else if (get_irn_mode(pred) == mode_T) {
		int i, k;

		/* if the predecessor block has more than one
		reachable outputs we cannot remove the block */
		k = 0;
		for (i = get_irn_n_outs(pred) - 1; i >= 0; --i) {
			ir_node *proj = get_irn_out(pred, i);
			node_t  *node;

			/* skip non-control flow Proj's */
			if (get_irn_mode(proj) != mode_X)
				continue;

			node = get_irn_node(proj);
			if (node->type.tv == tarval_reachable) {
				if (++k > 1)
					return 0;
			}
		}
		return 1;
	}
	return 0;
}  /* can_exchange */

/**
 * Block Post-Walker, apply the analysis results on control flow by
 * shortening Phi's and Block inputs.
 */
static void apply_cf(ir_node *block, void *ctx) {
	environment_t *env = ctx;
	node_t        *node = get_irn_node(block);
	int           i, j, k, n;
	ir_node       **ins, **in_X;
	ir_node       *phi, *next;

	n = get_Block_n_cfgpreds(block);

	if (node->type.tv == tarval_unreachable) {
		env->modified = 1;

		for (i = n - 1; i >= 0; --i) {
			ir_node *pred = get_Block_cfgpred(block, i);

			if (! is_Bad(pred)) {
				node_t *pred_bl = get_irn_node(get_nodes_block(skip_Proj(pred)));

				if (pred_bl->flagged == 0) {
					pred_bl->flagged = 3;

					if (pred_bl->type.tv == tarval_reachable) {
						/*
						 * We will remove an edge from block to its pred.
						 * This might leave the pred block as an endless loop
						 */
						if (! is_backedge(block, i))
							keep_alive(pred_bl->node);
					}
				}
			}
		}

		/* the EndBlock is always reachable even if the analysis
		   finds out the opposite :-) */
		if (block != get_irg_end_block(current_ir_graph)) {
			/* mark dead blocks */
			set_Block_dead(block);
			DB((dbg, LEVEL_1, "Removing dead %+F\n", block));
		} else {
			/* the endblock is unreachable */
			set_irn_in(block, 0, NULL);
		}
		return;
	}

	if (n == 1) {
		/* only one predecessor combine */
		ir_node *pred = skip_Proj(get_Block_cfgpred(block, 0));

		if (can_exchange(pred)) {
			ir_node *new_block = get_nodes_block(pred);
			DB((dbg, LEVEL_1, "Fuse %+F with %+F\n", block, new_block));
			DBG_OPT_COMBO(block, new_block, FS_OPT_COMBO_CF);
			exchange(block, new_block);
			node->node = new_block;
			env->modified = 1;
		}
		return;
	}

	NEW_ARR_A(ir_node *, in_X, n);
	k = 0;
	for (i = 0; i < n; ++i) {
		ir_node *pred = get_Block_cfgpred(block, i);
		node_t  *node = get_irn_node(pred);

		if (node->type.tv == tarval_reachable) {
			in_X[k++] = pred;
		} else {
			DB((dbg, LEVEL_1, "Removing dead input %d from %+F (%+F)\n", i, block, pred));
			if (! is_Bad(pred)) {
				node_t *pred_bl = get_irn_node(get_nodes_block(skip_Proj(pred)));

				if (pred_bl->flagged == 0) {
					pred_bl->flagged = 3;

					if (pred_bl->type.tv == tarval_reachable) {
						/*
						 * We will remove an edge from block to its pred.
						 * This might leave the pred block as an endless loop
						 */
						if (! is_backedge(block, i))
							keep_alive(pred_bl->node);
					}
				}
			}
		}
	}
	if (k >= n)
		return;

	NEW_ARR_A(ir_node *, ins, n);
	for (phi = get_Block_phis(block); phi != NULL; phi = next) {
		node_t *node = get_irn_node(phi);

		next = get_Phi_next(phi);
		if (is_tarval(node->type.tv) && tarval_is_constant(node->type.tv)) {
			/* this Phi is replaced by a constant */
			tarval  *tv = node->type.tv;
			ir_node *c  = new_r_Const(current_ir_graph, block, get_tarval_mode(tv), tv);

			set_irn_node(c, node);
			node->node = c;
			DB((dbg, LEVEL_1, "%+F is replaced by %+F\n", phi, c));
			DBG_OPT_COMBO(phi, c, FS_OPT_COMBO_CONST);
			exchange(phi, c);
			env->modified = 1;
		} else {
			j = 0;
			for (i = 0; i < n; ++i) {
				node_t *pred = get_irn_node(get_Block_cfgpred(block, i));

				if (pred->type.tv == tarval_reachable) {
					ins[j++] = get_Phi_pred(phi, i);
				}
			}
			if (j == 1) {
				/* this Phi is replaced by a single predecessor */
				ir_node *s = ins[0];
				node_t *phi_node = get_irn_node(phi);

				node->node = s;
				DB((dbg, LEVEL_1, "%+F is replaced by %+F because of cf change\n", phi, s));
				DBG_OPT_COMBO(phi, s, FS_OPT_COMBO_FOLLOWER);
				exchange(phi, s);
				phi_node->node = s;
				env->modified = 1;
			} else {
				set_irn_in(phi, j, ins);
				env->modified = 1;
			}
		}
	}

	if (k == 1) {
		/* this Block has only one live predecessor */
		ir_node *pred = skip_Proj(in_X[0]);

		if (can_exchange(pred)) {
			ir_node *new_block = get_nodes_block(pred);
			DBG_OPT_COMBO(block, new_block, FS_OPT_COMBO_CF);
			exchange(block, new_block);
			node->node = new_block;
			env->modified = 1;
		}
	} else {
		set_irn_in(block, k, in_X);
		env->modified = 1;
	}
}

/**
 * Post-Walker, apply the analysis results;
 */
static void apply_result(ir_node *irn, void *ctx) {
	environment_t *env = ctx;
	node_t        *node = get_irn_node(irn);

	if (is_Block(irn) || is_End(irn) || is_Bad(irn)) {
		/* blocks already handled, do not touch the End node */
	} else {
		node_t *block = get_irn_node(get_nodes_block(irn));

		if (block->type.tv == tarval_unreachable) {
			ir_node *bad = get_irg_bad(current_ir_graph);

			/* here, bad might already have a node, but this can be safely ignored
			   as long as bad has at least ONE valid node */
			set_irn_node(bad, node);
			node->node = bad;
			DB((dbg, LEVEL_1, "%+F is unreachable\n", irn));
			exchange(irn, bad);
			env->modified = 1;
		}
		else if (node->type.tv == tarval_unreachable) {
			/* don't kick away Unknown */
			if (! is_Unknown(irn)) {
				ir_node *bad = get_irg_bad(current_ir_graph);

				/* see comment above */
				set_irn_node(bad, node);
				node->node = bad;
				DB((dbg, LEVEL_1, "%+F is unreachable\n", irn));
				exchange(irn, bad);
				env->modified = 1;
			}
		}
		else if (get_irn_mode(irn) == mode_X) {
			if (is_Proj(irn)) {
				/* leave or Jmp */
				ir_node *cond = get_Proj_pred(irn);

				if (is_Cond(cond)) {
					node_t *sel = get_irn_node(get_Cond_selector(cond));

					if (is_tarval(sel->type.tv) && tarval_is_constant(sel->type.tv)) {
						/* Cond selector is a constant and the Proj is reachable, make a Jmp */
						ir_node *jmp  = new_r_Jmp(current_ir_graph, block->node);
						set_irn_node(jmp, node);
						node->node = jmp;
						DB((dbg, LEVEL_1, "%+F is replaced by %+F\n", irn, jmp));
						DBG_OPT_COMBO(irn, jmp, FS_OPT_COMBO_CF);
						exchange(irn, jmp);
						env->modified = 1;
					}
				}
			}
		} else {
			/* normal data node */
			if (is_tarval(node->type.tv) && tarval_is_constant(node->type.tv)) {
				tarval *tv = node->type.tv;

				/*
				 * Beware: never replace mode_T nodes by constants. Currently we must mark
				 * mode_T nodes with constants, but do NOT replace them.
				 */
				if (! is_Const(irn) && get_irn_mode(irn) != mode_T) {
					/* can be replaced by a constant */
					ir_node *c = new_r_Const(current_ir_graph, block->node, get_tarval_mode(tv), tv);
					set_irn_node(c, node);
					node->node = c;
					DB((dbg, LEVEL_1, "%+F is replaced by %+F\n", irn, c));
					DBG_OPT_COMBO(irn, c, FS_OPT_COMBO_CONST);
					exchange(irn, c);
					env->modified = 1;
				}
			} else if (is_entity(node->type.sym.entity_p)) {
				if (! is_SymConst(irn)) {
					/* can be replaced by a Symconst */
					ir_node *symc = new_r_SymConst(current_ir_graph, block->node, get_irn_mode(irn), node->type.sym, symconst_addr_ent);
					set_irn_node(symc, node);
					node->node = symc;

					DB((dbg, LEVEL_1, "%+F is replaced by %+F\n", irn, symc));
					DBG_OPT_COMBO(irn, symc, FS_OPT_COMBO_CONST);
					exchange(irn, symc);
					env->modified = 1;
				}
			} else if (is_Confirm(irn)) {
				/* Confirms are always follower, but do not kill them here */
			} else {
				ir_node *leader = get_leader(node);

				if (leader != irn) {
					DB((dbg, LEVEL_1, "%+F from part%d is replaced by %+F\n", irn, node->part->nr, leader));
					if (node->is_follower)
						DBG_OPT_COMBO(irn, leader, FS_OPT_COMBO_FOLLOWER);
					else
						DBG_OPT_COMBO(irn, leader, FS_OPT_COMBO_CONGRUENT);
					exchange(irn, leader);
					env->modified = 1;
				}
			}
		}
	}
}  /* apply_result */

/**
 * Fix the keep-alives by deleting unreachable ones.
 */
static void apply_end(ir_node *end, environment_t *env) {
	int i, j,  n = get_End_n_keepalives(end);
	ir_node **in;

	if (n > 0)
		NEW_ARR_A(ir_node *, in, n);

	/* fix the keep alive */
	for (i = j = 0; i < n; i++) {
		ir_node *ka   = get_End_keepalive(end, i);
		node_t  *node = get_irn_node(ka);

		if (! is_Block(ka))
			node = get_irn_node(get_nodes_block(ka));

		if (node->type.tv != tarval_unreachable && !is_Bad(ka))
			in[j++] = ka;
	}
	if (j != n) {
		set_End_keepalives(end, j, in);
		env->modified = 1;
	}
}  /* apply_end */

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
	SET(Unknown);
	SET(Bad);
	SET(Jmp);
	SET(Phi);
	SET(Add);
	SET(Sub);
	SET(Eor);
	SET(SymConst);
	SET(Cmp);
	SET(Proj);
	SET(Confirm);
	SET(End);

	if (op_Max != NULL)
		SET(Max);
	if (op_Min != NULL)
		SET(Min);

}  /* set_compute_functions */

static int dump_partition_hook(FILE *F, ir_node *n, ir_node *local) {
#ifdef DEBUG_libfirm
	ir_node *irn = local != NULL ? local : n;
	node_t *node = get_irn_node(irn);

	ir_fprintf(F, "info2 : \"partition %u type %+F\"\n", node->part->nr, node->type);
	return 1;
#endif
}

void combo(ir_graph *irg) {
	environment_t env;
	ir_node       *initial_bl;
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
	env.nonstd_cond    = 0;
	env.modified       = 0;

	assure_irg_outs(irg);
	assure_cf_loop(irg);

	/* we have our own value_of function */
	set_value_of_func(get_node_tarval);

	set_compute_functions();
	DEBUG_ONLY(part_nr = 0);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	/* create the initial partition and place it on the work list */
	env.initial = new_partition(&env);
	add_to_worklist(env.initial, &env);
	irg_walk_graph(irg, init_block_phis, create_initial_partitions, &env);

#ifdef WITH_UNKNOWN
	tarval_UNKNOWN = env.nonstd_cond ? tarval_bad : tarval_top;
#else
	tarval_UNKNOWN = tarval_bad;
#endif

	/* all nodes on the initial partition have type Top */
	env.initial->type_is_T_or_C = 1;

	/* Place the START Node's partition on cprop.
	   Place the START Node on its local worklist. */
	initial_bl = get_irg_start_block(irg);
	start      = get_irn_node(initial_bl);
	add_to_cprop(start, &env);

	do {
		propagate(&env);
		if (env.worklist != NULL)
			cause_splits(&env);
	} while (env.cprop != NULL || env.worklist != NULL);

	dump_all_partitions(&env);
	check_all_partitions(&env);

#if 0
	set_dump_node_vcgattr_hook(dump_partition_hook);
	dump_ir_block_graph(irg, "-partition");
	set_dump_node_vcgattr_hook(NULL);
#else
	(void)dump_partition_hook;
#endif

	/* apply the result */
	irg_block_walk_graph(irg, NULL, apply_cf, &env);
	irg_walk_graph(irg, NULL, apply_result, &env);
	apply_end(get_irg_end(irg), &env);

	if (env.modified) {
		/* control flow might changed */
		set_irg_outs_inconsistent(irg);
		set_irg_extblk_inconsistent(irg);
		set_irg_doms_inconsistent(irg);
		set_irg_loopinfo_inconsistent(irg);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	pmap_destroy(env.type2id_map);
	del_set(env.opcode2id_map);
	obstack_free(&env.obst, NULL);

	/* restore value_of() default behavior */
	set_value_of_func(NULL);
	current_ir_graph = rem;
}  /* combo */
