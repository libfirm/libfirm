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
 * Firm tarval, nevertheless we call it type here for "maximum compatibility".
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>

#include "iroptimize.h"
#include "irflag.h"
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

/* we need the tarval_R and tarval_U */
#define tarval_U  tarval_undefined
#define tarval_R  tarval_bad

typedef struct node_t            node_t;
typedef struct partition_t       partition_t;
typedef struct opcode_key_t      opcode_key_t;
typedef struct opcode_entry_t    opcode_entry_t;
typedef struct listmap_entry_t   listmap_entry_t;

/** The type of the compute function. */
typedef void (*compute_func)(node_t *node);

/**
 * An opcode map key.
 */
struct opcode_key_t {
	ir_opcode   code;   /**< The Firm opcode. */
	ir_mode     *mode;  /**< The mode of all nodes in the partition. */
};

/**
 * An entry in the opcode map.
 */
struct opcode_entry_t {
	opcode_key_t key;    /**< The key. */
	partition_t  *part;  /**< The associated partition. */
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
 * A node.
 */
struct node_t {
	ir_node     *node;         /**< The IR-node itself. */
	list_head   node_list;     /**< Double-linked list of entries. */
	partition_t *part;         /**< points to the partition this node belongs to */
	node_t      *cprop_next;   /**< Next node on partition.cprop list. */
	node_t      *next;         /**< Next node on local list (partition.touched, fallen). */
	tarval      *type;         /**< The associated lattice element "type". */
	unsigned    on_touched:1;  /**< Set, if this node is on the partition.touched set. */
	unsigned    on_cprop:1;    /**< Set, if this node is on the partition.cprop list. */
};

/**
 * A partition containing congruent nodes.
 */
struct partition_t {
	list_head         entries;       /**< The head of partition node list. */
	node_t            *cprop;        /**< The partition.cprop list. */
	partition_t       *wl_next;      /**< Next entry in the work list if any. */
	partition_t       *touched_next; /**< Points to the next partition in the touched set. */
	partition_t       *cprop_next;   /**< Points to the next partition in the cprop list. */
	node_t            *touched;      /**< The partition.touched set of this partition. */
	unsigned          n_nodes;       /**< Number of entries in this partition. */
	unsigned          n_touched;     /**< Number of entries in the partition.touched. */
	int               n_inputs;      /**< Maximum number of inputs of all entries. */
	unsigned          on_worklist:1; /**< Set, if this partition is in the work list. */
	unsigned          on_touched:1;  /**< Set, if this partition is on the touched set. */
#ifdef DEBUG_libfirm
	partition_t       *dbg_next;     /**< Link all partitions for debugging */
	unsigned          nr;            /**< A unique number for (what-)mapping, >0. */
#endif
};

typedef struct environment_t {
	struct obstack  obst;           /**< obstack to allocate data structures. */
	partition_t     *worklist;      /**< The work list. */
	partition_t     *cprop;         /**< The constant propagation list. */
	partition_t     *touched;       /**< the touched set. */
	partition_t     *TOP;           /**< The TOP partition. */
#ifdef DEBUG_libfirm
	partition_t     *dbg_list;      /**< List of all partitions. */
#endif
	set             *opcode_map;    /**< The initial opcode->partition map. */
	set             *opcode2id_map; /**< The opcodeMode->id map. */
	pmap            *type2id_map;   /**< The type->id map. */
	int             end_idx;        /**< -1 for local and 0 for global congruences. */
	int             lambda_input;   /**< Captured argument for lambda_partition(). */
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
/**
 * Dump partition to output.
 */
static void dump_partition(const char *msg, partition_t *part) {
	node_t *node;
	int    first = 1;

	DB((dbg, LEVEL_2, "%s part%u (%u) {\n  ", msg, part->nr, part->n_nodes));
	list_for_each_entry(node_t, node, &part->entries, node_list) {
		DB((dbg, LEVEL_2, "%s%+F", first ? "" : ", ", node->node));
		first = 0;
	}
	DB((dbg, LEVEL_2, "\n}\n"));
}

/**
 * Dump all partitions.
 */
static void dump_all_partitions(environment_t *env) {
	partition_t *P;

	DB((dbg, LEVEL_2, "All partitions\n===============\n"));
	for (P = env->dbg_list; P != NULL; P = P->dbg_next)
		dump_partition("", P);
}

#else
#define dump_partition(msg, part)
#define dump_all_partitions(env)
#endif

/**
 * compare two pointer values.
 */
static int cmp_ptr(const void *elt, const void *key, size_t size) {
	const listmap_entry_t *e1 = elt;
	const listmap_entry_t *e2 = key;

	return e1->id != e2->id;
}

/**
 * Creates a new listmap.
 */
static void new_listmap(listmap_t *map) {
	map->map    = new_set(cmp_ptr, 16);
	map->values = NULL;
}

/**
 * Deletes a listmap.
 */
static void del_listmap(listmap_t *map) {
	del_set(map->map);
}

/**
 * Return the associated listmap entry for a given id.
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
}

/**
 * calculate the hash value for an opcode map entry.
 */
static unsigned opcode_hash(const opcode_key_t *entry) {
	return (entry->mode - (ir_mode *)0) * 9 + entry->code;
}

/**
 * Compare two entries in the opcode map.
 */
static int cmp_opcode(const void *elt, const void *key, size_t size) {
	const opcode_key_t *o1 = elt;
	const opcode_key_t *o2 = key;

	return o1->code != o2->code || o1->mode != o2->mode;
}

/** Return the type of a node. */
static INLINE tarval *get_node_type(const ir_node *irn) {
	return get_irn_node(irn)->type;
}

/**
 * Create a new empty partition.
 */
static INLINE partition_t *new_partition(environment_t *env) {
	partition_t *part = obstack_alloc(&env->obst, sizeof(*part));

	INIT_LIST_HEAD(&part->entries);
	part->cprop        = NULL;
	part->wl_next      = env->worklist;
	part->touched_next = NULL;
	part->cprop_next   = NULL;
	part->touched      = NULL;
	part->n_nodes      = 0;
	part->n_touched    = 0;
	part->n_inputs     = 0;
	part->on_worklist  = 0;
	part->on_touched   = 0;
#ifdef DEBUG_libfirm
	part->dbg_next     = env->dbg_list;
	env->dbg_list      = part;
	part->nr           = part_nr++;
#endif

	return part;
}

/**
 * Get the partition for a given opcode.
 */
static INLINE partition_t *get_partition_for_irn(const ir_node *irn, environment_t *env) {
	opcode_entry_t key, *entry;
	unsigned       hash;

	key.key.code = get_irn_opcode(irn);
	key.key.mode = get_irn_mode(irn);
	hash         = opcode_hash(&key.key);

	entry = set_find(env->opcode_map, &key, sizeof(key), hash);
	if (entry == NULL) {
		/* create a new partition and place it on the wait queue */
		partition_t *part = new_partition(env);

		part->on_worklist = 1;
		env->worklist     = part;

		key.part = part;
		set_insert(env->opcode_map, &key, sizeof(key), hash);
		entry = &key;
	}
	return entry->part;
}

/**
 * Creates a partition node for the given IR-node and place it
 * into the given partition.
 */
static void create_partition_node(ir_node *irn, partition_t *part, environment_t *env) {
	/* create a partition node and place it in the partition */
	node_t *node = obstack_alloc(&env->obst, sizeof(*node));

	INIT_LIST_HEAD(&node->node_list);
	node->node         = irn;
	node->part         = part;
	node->cprop_next   = NULL;
	node->next         = NULL;
	node->type         = tarval_top; /* == tarval_U */
	node->on_touched   = 0;
	node->on_cprop     = 0;
	set_irn_node(irn, node);

	list_add_tail(&node->node_list, &part->entries);
	++part->n_nodes;

	DB((dbg, LEVEL_2, "Placing %+F in partition %u\n", irn, part->nr));
}

/**
 * Walker, initialize all Nodes' type to U or top and place
 * all nodes into the TOP partition.
 */
static void create_initial_partitions(ir_node *irn, void *ctx) {
	environment_t *env  = ctx;
	partition_t   *part = env->TOP;
	int           arity;

	create_partition_node(irn, part, env);
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
 * Add a node to the entry.partition.touched set if not already there..
 */
static INLINE void add_to_partition_touched(node_t *y) {
	if (y->on_touched == 0) {
		partition_t *part = y->part;

		y->next       = part->touched;
		part->touched = y;
		y->on_touched = 1;
		++part->n_touched;
	}
}

/**
 * update the worklist
 */
static void update_worklist(partition_t *Z, partition_t *Z_prime, environment_t *env) {
	/* If Z is on worklist then add Z' to worklist.
	   Else add the smaller of Z and Z' to worklist. */
	if (Z->on_worklist || Z_prime->n_nodes < Z->n_nodes) {
		Z_prime->on_worklist = 1;
		Z_prime->wl_next     = env->worklist;
		env->worklist        = Z_prime;
	} else {
		Z->on_worklist = 1;
		Z->wl_next     = env->worklist;
		env->worklist  = Z;
	}
}

/**
 * Split a partition by a local list.
 */
static partition_t *split(partition_t *Z, node_t *g, environment_t *env) {
	partition_t *Z_prime;
	node_t      *node;
	unsigned    n = 0;
	int         n_inputs;

	dump_partition("Splitting ", Z);

	/* Remove g from Z. */
	for (node = g; node != NULL; node = node->next) {
		list_del(&node->node_list);
		++n;
	}
	Z->n_nodes -= n;

	/* Move g to a new partition, Z’. */
	Z_prime = new_partition(env);
	n_inputs = 0;
	for (node = g; node != NULL; node = node->next) {
		int arity = get_irn_arity(node->node);
		list_add(&node->node_list, &Z_prime->entries);
		node->part = Z_prime;
		if (arity > n_inputs)
			n_inputs = arity;
	}
	Z_prime->n_inputs = n_inputs;
	Z_prime->n_nodes  = n;

	update_worklist(Z, Z_prime, env);

	dump_partition("Now ", Z);
	dump_partition("Created new ", Z_prime);
	return Z_prime;
}

/**
 * Returns non-zero if the i'th input of a Phi node is live.
 */
static int is_live_input(ir_node *phi, int i) {
	ir_node *block = get_nodes_block(phi);
	ir_node *pred  = get_Block_cfgpred(block, i);
	tarval *type   = get_node_type(pred);

	return type != tarval_U;
}

/**
 * Split the partitions if caused by the first entry on the worklist.
 */
static void cause_splits(environment_t *env) {
	partition_t *X, *Y, *Z;
	node_t      *x, *y, *e;
	int         i, end_idx;

	/* remove the first partition from the worklist */
	X = env->worklist;
	env->worklist  = X->wl_next;
	X->on_worklist = 0;

	dump_partition("Cause_split: ", X);
	end_idx = env->end_idx;
	for (i = X->n_inputs - 1; i >= -1; --i) {
		/* empty the touched set: already done, just clear the list */
		env->touched = NULL;

		list_for_each_entry(node_t, x, &X->entries, node_list) {
			/* ignore the "control input" for non-pinned nodes
			   if we are running in GCSE mode */
			if (i < end_idx && get_irn_pinned(x->node) != op_pin_state_pinned)
				continue;

			/* non-existing input */
			if (i >= get_irn_arity(x->node))
				continue;

			y = get_irn_node(get_irn_n(x->node, i));
			Y = y->part;
			if (Y != env->TOP && (! is_Phi(x->node) || is_live_input(x->node, i))) {
				add_to_touched(Y, env);
				add_to_partition_touched(y);
			}
		}

		for (Z = env->touched; Z != NULL; Z = Z->touched_next) {
			/* remove it from the touched set */
			Z->on_touched = 0;

			if (Z->n_nodes != Z->n_touched) {
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
}

/**
 * Implements split_by_what(): Split a partition by characteristics given
 * by the what function.
 *
 * @return list of partitions
 */
static partition_t **split_by_what(partition_t *X, what_func What,
								  partition_t**P, environment_t *env) {
	node_t          *x, *S;
	listmap_t       map;
	listmap_entry_t *iter;
	partition_t     *R;

	/* Let map be an empty mapping from the range of What to (local) list of Nodes. */
	new_listmap(&map);
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
		R = split(X, S, env);
		if (P != NULL) {
			ARR_APP1(partition_t *, P, R);
		}
	}
	/* Add X to P. */
	if (P != NULL) {
		ARR_APP1(partition_t *, P, X);
	}

	del_listmap(&map);
	return P;
}

/** lambda n.(n.type) */
static void *lambda_type(const node_t *node, environment_t *env) {
	(void)env;
	return node->type;
}

/** lambda n.(n.opcode) */
static void *lambda_opcode(const node_t *node, environment_t *env) {
	opcode_key_t key, *entry;

	key.code = get_irn_opcode(node->node);
	key.mode = get_irn_mode(node->node);
	entry = set_insert(env->opcode2id_map, &key, sizeof(&key), opcode_hash(&key));
	return entry;
}

/** lambda n.(n[i].partition) */
static void *lambda_partition(const node_t *node, environment_t *env) {
	ir_node *pred;
	node_t  *p;
	int     i = env->lambda_input;

	if (i >= get_irn_arity(node->node)) {
		/* we are outside the allowed range */
		return NULL;
	}

	/* ignore the "control input" for non-pinned nodes
	   if we are running in GCSE mode */
	if (i < env->end_idx && get_irn_pinned(node->node) != op_pin_state_pinned)
		return NULL;

	pred = get_irn_n(node->node, i);
	p    = get_irn_node(pred);

	return p->part;
}

/**
 * Implements split_by().
 */
static void split_by(partition_t *X, environment_t *env) {
	partition_t **P = NEW_ARR_F(partition_t *, 0);
	int         i, j, k;

	P = split_by_what(X, lambda_type, P, env);
	for (i = ARR_LEN(P) - 1; i >= 0; --i) {
		partition_t *Y = P[i];

		if (Y != env->TOP) {
			partition_t **Q = NEW_ARR_F(partition_t *, 0);

			Q = split_by_what(Y, lambda_opcode, Q, env);

			for (j = ARR_LEN(Q) - 1; j >= 0; --j) {
				partition_t *Z = Q[j];

				for (k = Z->n_inputs - 1; k >= -1; --k) {
					env->lambda_input = k;
					split_by_what(Z, lambda_partition, NULL, env);
				}
			}
			DEL_ARR_F(Q);
		}
	}
	DEL_ARR_F(P);
}

/**
 * (Re-)compute the type for a given node.
 */
static void default_compute(node_t *node) {
	int     i;
	ir_node *irn = node->node;
	ir_mode *mode;

	if (get_irn_pinned(irn) == op_pin_state_pinned) {
		node_t *block = get_irn_node(get_nodes_block(irn));

		if (block->type == tarval_U) {
			node->type = tarval_top;
			return;
		}
	}
	mode = get_irn_mode(irn);
	if (mode == mode_M) {
		/* mode M is always bottom for now */
		node->type = tarval_bottom;
		return;
	}
	if (! mode_is_data(mode))
		return;

	/* if any of the data inputs have type top, the result is type top */
	for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(irn, i);
		node_t  *p    = get_irn_node(pred);

		if (p->type == tarval_top) {
			node->type = tarval_top;
			return;
		}
	}
	node->type = computed_value(irn);
}

/**
 * (Re-)compute the type for a Block node.
 */
static void compute_Block(node_t *node) {
	int     i;
	ir_node *block = node->node;

	for (i = get_Block_n_cfgpreds(block) - 1; i >= 0; --i) {
		node_t *pred = get_irn_node(get_Block_cfgpred(block, i));

		if (pred->type == tarval_R) {
			/* A block is reachable, if at least of predecessor is reachable. */
			node->type = tarval_R;
			return;
		}
	}
	node->type = tarval_U;
}

/**
 * (Re-)compute the type for a Jmp node.
 */
static void compute_Jmp(node_t *node) {
	node_t *block = get_irn_node(get_nodes_block(node->node));

	node->type = block->type;
}

/**
 * (Re-)compute the type for a Phi node.
 */
static void compute_Phi(node_t *node) {
	int     i;
	ir_node *phi = node->node;
	tarval  *type = tarval_top;

	/* if a Phi is in a unreachable block, its type is TOP */
	node_t *block = get_irn_node(get_nodes_block(phi));

	if (block->type == tarval_U) {
		node->type = tarval_top;
		return;
	}

	/* if any of the data inputs have type top, the result is type top */
	for (i = get_Phi_n_preds(phi) - 1; i >= 0; --i) {
		node_t *pred = get_irn_node(get_Phi_pred(phi, i));

		if (pred->type == tarval_top) {
			/* ignore TOP inputs */
			continue;
		}
		if (pred->type == tarval_bottom) {
			node->type = tarval_bottom;
			return;
		} else if (type == tarval_top) {
			/* first constant found */
			type = pred->type;
		} else if (type == pred->type) {
			/* same constant, continue */
			continue;
		} else {
			/* different constants or tarval_bottom */
			node->type = tarval_bottom;
			return;
		}
	}
	node->type = type;
}

/**
 * (Re-)compute the type for a Sub. Special case: both nodes are congruent.
 */
static void compute_Sub(node_t *node) {
	ir_node *sub = node->node;
	node_t  *l   = get_irn_node(get_Sub_left(sub));
	node_t  *r   = get_irn_node(get_Sub_right(sub));
	tarval  *a   = l->type;
	tarval  *b   = r->type;

	if (a == tarval_top || b == tarval_top) {
		node->type = tarval_top;
	} else if (r->part == l->part) {
		ir_mode *mode = get_irn_mode(sub);
		node->type = get_mode_null(mode);
	} else if (a == tarval_bottom || b == tarval_bottom) {
		node->type = tarval_bottom;
	} else {
		node->type = tarval_sub(a, b);
	}
}

/**
 * (Re-)compute the type for a Proj(Cmp).
 */
static void compute_Proj_Cmp(node_t *node, ir_node *cmp) {
	ir_node *proj = node->node;
	node_t  *l    = get_irn_node(get_Cmp_left(cmp));
	node_t  *r    = get_irn_node(get_Cmp_right(cmp));
	tarval  *a    = l->type;
	tarval  *b    = r->type;
	pn_Cmp  pnc   = get_Proj_proj(proj);

	/*
	 * BEWARE: a == a is NOT always True for floating Point values, as
	 * NaN != NaN is defined, so we must check this here.
	 */
	if (!mode_is_float(get_irn_mode(l->node)) || pnc == pn_Cmp_Lt ||  pnc == pn_Cmp_Gt) {
		if (a == tarval_top || b == tarval_top) {
			node->type = tarval_top;
		} else if (r->part == l->part) {
			node->type = new_tarval_from_long(pnc & pn_Cmp_Eq, mode_b);
		} else if (a == tarval_bottom || b == tarval_bottom) {
			node->type = tarval_bottom;
		} else {
			default_compute(node);
		}
	} else {
		default_compute(node);
	}
}

/**
 * (Re-)compute the type for a Proj-Nodes.
 */
static void compute_Proj(node_t *node) {
	ir_node *proj = node->node;
	ir_mode *mode = get_irn_mode(proj);
	ir_node *pred;

	if (mode == mode_M) {
		/* mode M is always bottom */
		node->type = tarval_bottom;
		return;
	}
	if (mode != mode_X) {
		ir_node *cmp = get_Proj_pred(proj);
		if (is_Cmp(cmp))
			compute_Proj_Cmp(node, cmp);
		else
			default_compute(node);
		return;
	}
	/* handle mode_X nodes */
	pred = get_Proj_pred(proj);

	switch (get_irn_opcode(pred)) {
	case iro_Start:
		/* the Proj_X from the Start is always reachable */
		node->type = tarval_R;
		break;
	default:
		default_compute(node);
	}
}

/**
 * (Re-)compute the type for a given node.
 */
static void compute(node_t *node) {
	compute_func func = (compute_func)node->node->op->ops.generic;

	if (func != NULL)
		func(node);
}
/**
 * Propagate constant evaluation.
 */
static void propagate(environment_t *env) {
	partition_t *X, *Y;
	node_t      *x;
	tarval      *old_type;
	node_t      *fallen = NULL;
	unsigned    n_fallen = 0;
	int         i;

	while (env->cprop != NULL) {
		/* remove a partition X from cprop */
		X = env->cprop;
		env->cprop = X->cprop_next;

		while (X->cprop != NULL) {
			/* remove a Node x from X.cprop */
			x = X->cprop;
			x->on_cprop = 0;
			X->cprop = x->cprop_next;

			/* compute a new type for x */
			old_type = x->type;
			compute(x);
			if (x->type != old_type) {
				DB((dbg, LEVEL_2, "node %+F has changed type from %T to %T\n", x->node, old_type, x->type));
				/* Add x to fallen. */
				x->next = fallen;
				fallen  = x;
				++n_fallen;

				for (i = get_irn_n_outs(x->node) - 1; i >= 0; --i) {
					ir_node *succ = get_irn_out(x->node, i);
					node_t  *y    = get_irn_node(succ);

					/* Add y to y.partition.cprop. */
					if (y->on_cprop == 0) {
						y->cprop_next  = y->part->cprop;
						y->part->cprop = y;
						y->on_cprop    = 1;
					}
				}
			}
		}
		if (n_fallen != X->n_nodes) {
			Y = split(X, fallen, env);
		} else {
			Y = X;
		}
		split_by(Y, env);
	}
}

/**
 * Get the leader for a given node from its congruence class.
 *
 * @param irn  the node
 */
static ir_node *get_leader(ir_node *irn) {
	partition_t *part = get_irn_node(irn)->part;

	if (part->n_nodes > 1) {
		DB((dbg, LEVEL_2, "Found congruence class for %+F ", irn));
		dump_partition("", part);
	}
	return irn;
}

/**
 * Post-Walker, apply the analysis results;
 */
static void apply_result(ir_node *irn, void *ctx) {
	environment_t *env = ctx;

	if (is_no_Block(irn)) {
		ir_node *leader = get_leader(irn);

		if (leader != irn) {
			exchange(irn, leader);
		}
	}
}

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
	SET(Sub);
}

void combo(ir_graph *irg) {
	environment_t env;
	ir_node       *start_bl, *initial_X;
	node_t        *start;
	ir_graph      *rem = current_ir_graph;

	current_ir_graph = irg;

	/* register a debug mask */
	FIRM_DBG_REGISTER(dbg, "firm.opt.combo");
	firm_dbg_set_mask(dbg, SET_LEVEL_2);

	obstack_init(&env.obst);
	env.worklist       = NULL;
	env.cprop          = NULL;
	env.touched        = NULL;
	env.TOP            = NULL;
#ifdef DEBUG_libfirm
	env.dbg_list       = NULL;
#endif
	env.opcode_map     = new_set(cmp_opcode, iro_Last * 4);
	env.opcode2id_map  = new_set(cmp_opcode, iro_Last * 4);
	env.type2id_map    = pmap_create();
	env.end_idx        = get_opt_global_cse() ? 0 : -1;
	env.lambda_input   = 0;

	assure_irg_outs(irg);

	/* we have our own value_of function */
	set_value_of_func(get_node_type);

	set_compute_functions();

	/* create the initial TOP partition and place it on the work list */
	env.TOP = new_partition(&env);
	env.TOP->wl_next = env.worklist;
	env.worklist     = env.TOP;
	irg_walk_graph(irg, NULL, create_initial_partitions, &env);

	/* Place the START Node's partition on cprop.
	   Place the START Node on its local worklist. */
	start_bl = get_irg_start_block(irg);
	start    = get_irn_node(start_bl);
	start->part->cprop_next = env.cprop;
	env.cprop               = start->part;

	start->cprop_next  = start->part->cprop;
	start->part->cprop = start;

	/* set the initial exec to R */
	initial_X = get_irg_initial_exec(irg);
	get_irn_node(initial_X)->type = tarval_R;

	while (env.cprop != NULL && env.worklist != NULL) {
		propagate(&env);
		if (env.worklist != NULL)
			cause_splits(&env);
	}

	dump_all_partitions(&env);

	/* apply the result */
	irg_walk_graph(irg, NULL, apply_result, &env);

	pmap_destroy(env.type2id_map);
	del_set(env.opcode_map);
	del_set(env.opcode2id_map);
	obstack_free(&env.obst, NULL);

	/* restore value_of() default behavior */
	set_value_of_func(NULL);
	current_ir_graph = rem;
}
