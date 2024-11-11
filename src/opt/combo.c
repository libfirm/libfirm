/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Cliff Click's Combined Analysis/Optimization
 * @author  Michael Beck
 *
 * This is a slightly enhanced version of Cliff Clicks combo algorithm
 * - support for commutative nodes is added, Add(a,b) and Add(b,a) ARE congruent
 * - supports all Firm direct (by a data edge) identities except Mux
 *   (Mux can be a 2-input or 1-input identity, only 2-input is implemented yet)
 * - supports Confirm nodes (handle them like Copies but do NOT remove them)
 * - Unknown nodes are represented as Top
 * - support for global congruences is implemented but not tested yet
 *
 * Note further that we use the terminology from Click's work here, which is
 * different in some cases from Firm terminology.  Especially, Click's type is a
 * Firm tarval/entity, nevertheless we call it type here for "maximum
 * compatibility".
 */
#include "array.h"
#include "debug.h"
#include "ircons.h"
#include "irdump.h"
#include "irflag.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irnodeset.h"
#include "irop_t.h"
#include "iropt_dbg.h"
#include "iropt_t.h"
#include "iroptimize.h"
#include "irouts_t.h"
#include "irprintf.h"
#include "list.h"
#include "obstack.h"
#include "panic.h"
#include "pmap.h"
#include "set.h"
#include "tv_t.h"
#include <assert.h>

/* define this to check that all type translations are monotone */
#define VERIFY_MONOTONE

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
	ir_node *irn;    /**< An IR node representing this opcode. */
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
 * A lattice element. Because we handle constants and symbolic constants
 * different, we have to use this union.
 */
typedef union {
	ir_tarval *tv;
	ir_entity *ent;
} lattice_elem_t;

/**
 * A node.
 */
struct node_t {
	ir_node        *node;           /**< The IR-node itself. */
	list_head       node_list;      /**< Double-linked list of leader/follower entries. */
	list_head       cprop_list;     /**< Double-linked partition.cprop list. */
	partition_t    *part;           /**< points to the partition this node belongs to */
	node_t         *next;           /**< Next node on local list (partition.touched, fallen). */
	node_t         *race_next;      /**< Next node on race list. */
	lattice_elem_t  type;           /**< The associated lattice element "type". */
	int             max_user_input; /**< Maximum input number of Def-Use edges. */
	unsigned        next_edge;      /**< Index of the next Def-Use edge to use. */
	unsigned        n_followers;    /**< Number of follower in the outs set. */
	bool            on_touched:1;   /**< Set, if this node is on the partition.touched set. */
	bool            on_cprop:1;     /**< Set, if this node is on the partition.cprop list. */
	bool            on_fallen:1;    /**< Set, if this node is on the fallen list. */
	bool            is_follower:1;  /**< Set, if this node is a follower. */
	unsigned        flagged:2;      /**< 2 Bits, set if this node was visited by race 1 or 2. */
};

/**
 * A partition containing congruent nodes.
 */
struct partition_t {
	list_head    leader;          /**< The head of partition leader node list. */
	list_head    follower;        /**< The head of partition follower node list. */
	list_head    cprop;           /**< The head of partition.cprop list. */
	partition_t *wl_next;         /**< Next entry in the work list if any. */
	partition_t *touched_next;    /**< Points to the next partition in the touched set. */
	partition_t *cprop_next;      /**< Points to the next partition in the cprop list. */
	partition_t *split_next;      /**< Points to the next partition in the list that must be split by split_by(). */
	node_t      *touched;         /**< The partition.touched set of this partition. */
	unsigned     n_leaders;       /**< Number of entries in this partition.leader. */
	unsigned     n_touched;       /**< Number of entries in the partition.touched. */
	int          max_user_inputs; /**< Maximum number of user inputs of all entries. */
	bool         on_worklist:1;   /**< Set, if this partition is in the work list. */
	bool         on_touched:1;    /**< Set, if this partition is on the touched set. */
	bool         on_cprop:1;      /**< Set, if this partition is on the cprop list. */
	bool         type_is_B_or_C:1;/**< Set, if all nodes in this partition have type Bottom or Constant. */
#ifdef DEBUG_libfirm
	partition_t *dbg_next;       /**< Link all partitions for debugging */
	unsigned     nr;             /**< A unique number for (what-)mapping, >0. */
#endif
};

typedef struct environment_t {
	struct obstack  obst;          /**< obstack to allocate data structures. */
	partition_t    *worklist;      /**< The work list. */
	partition_t    *cprop;         /**< The constant propagation list. */
	partition_t    *touched;       /**< the touched set. */
	partition_t    *initial;       /**< The initial partition. */
	set            *opcode2id_map; /**< The opcodeMode->id map. */
	ir_node       **kept_memory;   /**< Array of memory nodes that must be kept. */
	int             end_idx;       /**< -1 for local and 0 for global congruences. */
	int             lambda_input;  /**< Captured argument for lambda_partition(). */
	bool            modified:1;    /**< Set, if the graph was modified. */
	bool            unopt_cf:1;    /**< If set, control flow is not optimized due to Unknown. */
	/* options driving the optimizaion */
	bool            commutative:1; /**< Set, if commutation nodes should be handled specially. */
#ifdef DEBUG_libfirm
	partition_t    *dbg_list;      /**< List of all partitions. */
#endif
} environment_t;

/** Type of the what function. */
typedef void *(*what_func)(const node_t *node, environment_t *env);

static inline node_t *get_irn_node(const ir_node *node)
{
	return (node_t*)get_irn_link(node);
}

static void set_irn_node(ir_node *irn, node_t *node)
{
	set_irn_link(irn, node);
}

/* we use dataflow like names here */
#define tarval_top    tarval_unknown
#define tarval_bottom tarval_bad

static inline bool is_reachable(const node_t *node)
{
	assert(is_Block(node->node));
	return node->type.tv == tarval_top;
}

/** The debug module handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/** The what reason. */
DEBUG_ONLY(static const char *what_reason;)

/** Next partition number. */
DEBUG_ONLY(static unsigned part_nr = 0;)

/* forward */
static node_t *identity(node_t *node);

/**
 * Compare two opcode representatives.
 */
static int cmp_irn_opcode(const ir_node *a, const ir_node *b)
{
	if ((get_irn_op(a) != get_irn_op(b)) ||
	    (get_irn_mode(a) != get_irn_mode(b)))
		return 1;

	/* compare if a's in and b's in are of equal length */
	int arity = get_irn_arity(a);
	if (arity != get_irn_arity(b))
		return 1;

	if (is_Block(a)) {
		/*
		 * Some ugliness here: Two Blocks having the same
		 * IJmp predecessor would be congruent, which of course is wrong.
		 * We fix it by never letting blocks be congruent
		 * which cannot be detected by combo either.
		 */
		return 1;
	}

	/* here, we already know that the nodes are identical except their
	 * attributes */
	return !a->op->ops.attrs_equal(a, b);
}

#ifdef CHECK_PARTITIONS
/**
 * Check a partition.
 */
static void check_partition(const partition_t *T)
{
	unsigned n = 0;

	list_for_each_entry(node_t, node, &T->leader, node_list) {
		assert(!node->is_follower);
		assert(node->flagged == 0);
		assert(node->part == T);
		++n;
	}
	(void)n;
	assert(n == T->n_leaders);

	list_for_each_entry(node_t, node, &T->follower, node_list) {
		assert(node->is_follower);
		assert(node->flagged == 0);
		assert(node->part == T);
	}
}

#ifdef DEBUG_libfirm
/**
 * check that all leader nodes in the partition have the same opcode.
 */
static void check_opcode(const partition_t *Z)
{
	const ir_node *repr = NULL;

	list_for_each_entry(node_t, node, &Z->leader, node_list) {
		ir_node *irn = node->node;

		if (repr == NULL) {
			repr = irn;
		} else {
			assert(cmp_irn_opcode(repr, irn) == 0);
		}
	}
}
#endif

static void check_all_partitions(environment_t *env)
{
#ifdef DEBUG_libfirm
	for (partition_t *P = env->dbg_list; P != NULL; P = P->dbg_next) {
		check_partition(P);
		if (!P->type_is_B_or_C)
			check_opcode(P);
		list_for_each_entry(node_t, node, &P->follower, node_list) {
			node_t *leader = identity(node);

			assert(leader != node && leader->part == node->part);
		}
	}
#else
	(void)env;
#endif
}

/**
 * Check list.
 */
static void do_check_list(const node_t *list, int ofs, const partition_t *Z)
{

#ifndef NDEBUG
#define NEXT(e)  *((const node_t **)((char *)(e) + (ofs)))
	for (const node_t *e = list; e != NULL; e = NEXT(e)) {
		assert(e->part == Z);
	}
#undef NEXT
#else
	(void)list;
	(void)ofs;
	(void)Z;
#endif
}

/**
 * Check a local list.
 */
static void check_list(const node_t *list, const partition_t *Z)
{
	do_check_list(list, offsetof(node_t, next), Z);
}

#else
#define check_partition(T)
#define check_list(list, Z)
#define check_all_partitions(env)
#endif /* CHECK_PARTITIONS */

#ifdef DEBUG_libfirm
static inline lattice_elem_t get_partition_type(const partition_t *X);

/**
 * Dump partition to output.
 */
static void dump_partition(const char *msg, const partition_t *part)
{
	bool           first = true;
	lattice_elem_t type  = get_partition_type(part);

	DB((dbg, LEVEL_2, "%s part%u%s (%u, %+F) {\n  ",
		msg, part->nr, part->type_is_B_or_C ? "*" : "",
		part->n_leaders, type));
	list_for_each_entry(node_t, node, &part->leader, node_list) {
		DB((dbg, LEVEL_2, "%s%+F", first ? "" : ", ", node->node));
		first = false;
	}
	if (!list_empty(&part->follower)) {
		DB((dbg, LEVEL_2, "\n---\n  "));
		first = true;
		list_for_each_entry(node_t, node, &part->follower, node_list) {
			DB((dbg, LEVEL_2, "%s%+F", first ? "" : ", ", node->node));
			first = false;
		}
	}
	DB((dbg, LEVEL_2, "\n}\n"));
}

/**
 * Dumps a list.
 */
static void do_dump_list(const char *msg, const node_t *node, int ofs)
{
#define GET_LINK(p, ofs)  *((const node_t **)((char *)(p) + (ofs)))

	DB((dbg, LEVEL_3, "%s = {\n  ", msg));
	bool first = true;
	for (const node_t *p = node; p != NULL; p = GET_LINK(p, ofs)) {
		DB((dbg, LEVEL_3, "%s%+F", first ? "" : ", ", p->node));
		first = false;
	}
	DB((dbg, LEVEL_3, "\n}\n"));

#undef GET_LINK
}

/**
 * Dumps a race list.
 */
static void dump_race_list(const char *msg, const node_t *list)
{
	do_dump_list(msg, list, offsetof(node_t, race_next));
}

/**
 * Dumps a local list.
 */
static void dump_list(const char *msg, const node_t *list)
{
	do_dump_list(msg, list, offsetof(node_t, next));
}

/**
 * Dump all partitions.
 */
static void dump_all_partitions(const environment_t *env)
{
	DB((dbg, LEVEL_2, "All partitions\n===============\n"));
	for (const partition_t *P = env->dbg_list; P != NULL; P = P->dbg_next)
		dump_partition("", P);
}

/**
 * Sump a split list.
 */
static void dump_split_list(const partition_t *list)
{
	DB((dbg, LEVEL_2, "Split by %s produced = {\n", what_reason));
	char split = ' ';
	for (const partition_t *p = list; p != NULL; p = p->split_next) {
		DB((dbg, LEVEL_2, "%c part%u", split, p->nr));
		split = ',';
	}
	DB((dbg, LEVEL_2, "\n}\n"));
}

/**
 * Dump partition and type for a node.
 */
static int dump_partition_hook(FILE *F, const ir_node *n, const ir_node *local)
{
	const ir_node *irn  = local != NULL ? local : n;
	const node_t  *node = get_irn_node(irn);

	ir_fprintf(F, "info2 : \"partition %u type %+F\"\n", node->part->nr,
	           node->type);
	return 1;
}

#else
#define dump_partition(msg, part) (void)(msg), (void)(part)
#define dump_race_list(msg, list) (void)(msg), (void)(list)
#define dump_list(msg, list) (void)(msg), (void)(list)
#define dump_all_partitions(env) (void)(env)
#define dump_split_list(list) (void)(list)
#endif

#if defined(VERIFY_MONOTONE) && defined (DEBUG_libfirm)
/**
 * Verify that a type transition is monotone
 */
static void verify_type(const lattice_elem_t old_type, node_t *node)
{
	if (old_type.tv == node->type.tv) {
		/* no change */
		return;
	}
	if (old_type.tv == tarval_bottom) {
		/* from Bottom up-to is always allowed */
		return;
	}
	if (node->type.tv == tarval_top) {
		/* top reached */
		return;
	}
	panic("wrong translation from %+F to %+F on node %+F", old_type,
	      node->type, node->node);
}

#else
#define verify_type(old_type, node) (void)(old_type), (void)node
#endif

/**
 * Compare two pointer values of a listmap.
 */
static int listmap_cmp_ptr(const void *elt, const void *key, size_t size)
{
	(void)size;
	const listmap_entry_t *e1 = (listmap_entry_t*)elt;
	const listmap_entry_t *e2 = (listmap_entry_t*)key;
	return e1->id != e2->id;
}

/**
 * Initializes a listmap.
 *
 * @param map  the listmap
 */
static void listmap_init(listmap_t *map)
{
	map->map    = new_set(listmap_cmp_ptr, 16);
	map->values = NULL;
}

/**
 * Terminates a listmap.
 *
 * @param map  the listmap
 */
static void listmap_term(listmap_t *map)
{
	del_set(map->map);
}

/**
 * Return the associated listmap entry for a given id.
 *
 * @param map  the listmap
 * @param id   the id to search for
 *
 * @return the associated listmap entry for the given id
 */
static listmap_entry_t *listmap_find(listmap_t *map, void *id)
{
	listmap_entry_t  key   = { .id = id, .list = NULL, .next = NULL };
	listmap_entry_t *entry = set_insert(listmap_entry_t, map->map, &key,
	                                    sizeof(key), hash_ptr(id));

	if (entry->list == NULL) {
		/* a new entry, put into the list */
		entry->next = map->values;
		map->values = entry;
	}
	return entry;
}

/**
 * Calculate the hash value for an opcode map entry.
 *
 * @param entry  an opcode map entry
 *
 * @return a hash value for the given opcode map entry
 */
static unsigned opcode_hash(const opcode_key_t *entry)
{
	/* we cannot use the ir ops hash function here, because it hashes the
	 * predecessors. */
	const ir_node *n     = entry->irn;
	ir_opcode      code  = (ir_opcode)get_irn_opcode(n);
	ir_mode       *mode  = get_irn_mode(n);
	int            arity = get_irn_arity(n);
	unsigned       hash  = ((unsigned)PTR_TO_INT(mode))*9 + code + arity;

	if (code == iro_Const)
		hash ^= hash_ptr(get_Const_tarval(n));
	else if (code == iro_Proj)
		hash += get_Proj_num(n);
	return hash;
}

/**
 * Compare two entries in the opcode map.
 */
static int cmp_opcode(const void *elt, const void *key, size_t size)
{
	(void)size;
	const opcode_key_t *o1 = (opcode_key_t*)elt;
	const opcode_key_t *o2 = (opcode_key_t*)key;
	return cmp_irn_opcode(o1->irn, o2->irn);
}

/**
 * Compare two Def-Use edges for input position.
 */
static int cmp_def_use_edge(const void *a, const void *b)
{
	const ir_def_use_edge *ea = (const ir_def_use_edge*)a;
	const ir_def_use_edge *eb = (const ir_def_use_edge*)b;

	/* no overrun, because range is [-1, MAXINT] */
	return ea->pos - eb->pos;
}

/**
 * We need the Def-Use edges sorted.
 */
static void sort_irn_outs(node_t *node)
{
	ir_node  *irn    = node->node;
	unsigned  n_outs = get_irn_n_outs(irn);
	QSORT(irn->o.out->edges, n_outs, cmp_def_use_edge);
	node->max_user_input = n_outs > 0 ? irn->o.out->edges[n_outs-1].pos : -1;
}

/**
 * Return the type of a node.
 *
 * @param irn  an IR-node
 *
 * @return the associated type of this node
 */
static inline lattice_elem_t get_node_type(const ir_node *irn)
{
	return get_irn_node(irn)->type;
}

/**
 * Return the tarval of a node.
 *
 * @param irn  an IR-node
 *
 * @return the associated type of this node
 */
static inline ir_tarval *get_node_tarval(const ir_node *irn)
{
	lattice_elem_t type = get_node_type(irn);
	return is_tarval(type.tv) ? type.tv : tarval_top;
}

/**
 * Add a partition to the worklist.
 */
static inline void add_to_worklist(partition_t *X, environment_t *env)
{
	assert(!X->on_worklist);
	DB((dbg, LEVEL_2, "Adding part%d to worklist\n", X->nr));
	X->wl_next     = env->worklist;
	X->on_worklist = true;
	env->worklist  = X;
}

/**
 * Create a new empty partition.
 *
 * @param env   the environment
 *
 * @return a newly allocated partition
 */
static inline partition_t *new_partition(environment_t *env)
{
	partition_t *part = OALLOCZ(&env->obst, partition_t);

	INIT_LIST_HEAD(&part->leader);
	INIT_LIST_HEAD(&part->follower);
	INIT_LIST_HEAD(&part->cprop);
#ifdef DEBUG_libfirm
	part->dbg_next = env->dbg_list;
	env->dbg_list  = part;
	part->nr       = part_nr++;
#endif

	return part;
}

/**
 * Get the first node from a partition.
 */
static inline node_t *get_first_node(const partition_t *X)
{
	return list_entry(X->leader.next, node_t, node_list);
}

#ifdef DEBUG_libfirm
/**
 * Return the type of a partition (assuming partition is non-empty and
 * all elements have the same type).
 *
 * @param X  a partition
 *
 * @return the type of the first element of the partition
 */
static inline lattice_elem_t get_partition_type(const partition_t *X)
{
	const node_t *first = get_first_node(X);
	return first->type;
}
#endif

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
static node_t *create_partition_node(ir_node *irn, partition_t *part,
                                     environment_t *env)
{
	/* create a partition node and place it in the partition */
	node_t *node = OALLOCZ(&env->obst, node_t);

	INIT_LIST_HEAD(&node->node_list);
	INIT_LIST_HEAD(&node->cprop_list);
	node->node    = irn;
	node->part    = part;
	node->type.tv = tarval_bottom;
	set_irn_node(irn, node);

	list_add_tail(&node->node_list, &part->leader);
	++part->n_leaders;

	return node;
}

/**
 * Pre-Walker, initialize all Nodes' type to U or bottom and place
 * all nodes into the Bottom partition.
 */
static void create_initial_partitions(ir_node *irn, void *ctx)
{
	environment_t *env  = (environment_t*)ctx;
	partition_t   *part = env->initial;
	node_t        *node = create_partition_node(irn, part, env);
	sort_irn_outs(node);
	if (node->max_user_input > part->max_user_inputs)
		part->max_user_inputs = node->max_user_input;

	if (is_Block(irn)) {
		set_Block_phis(irn, NULL);
	}
}

/**
 * Post-Walker, collect  all Block-Phi lists, set Cond.
 */
static void init_block_phis(ir_node *irn, void *ctx)
{
	(void)ctx;

	if (is_Phi(irn)) {
		ir_node *block = get_nodes_block(irn);
		add_Block_phi(block, irn);
	}
}

/**
 * Add a node to the entry.partition.touched set and
 * node->partition to the touched set if not already there.
 *
 * @param y    a node
 * @param env  the environment
 */
static inline void add_to_touched(node_t *y, environment_t *env)
{
	if (!y->on_touched) {
		partition_t *part = y->part;

		y->next       = part->touched;
		part->touched = y;
		y->on_touched = true;
		++part->n_touched;

		if (!part->on_touched) {
			part->touched_next = env->touched;
			env->touched       = part;
			part->on_touched   = true;
		}

		check_list(part->touched, part);
	}
}

/**
 * Place a node on the cprop list.
 *
 * @param y    the node
 * @param env  the environment
 */
static void add_to_cprop(node_t *y, environment_t *env)
{
	/* Add y to y.partition.cprop. */
	if (!y->on_cprop) {
		partition_t *Y = y->part;
		list_add_tail(&y->cprop_list, &Y->cprop);
		y->on_cprop = true;

		DB((dbg, LEVEL_3, "Add %+F to part%u.cprop\n", y->node, Y->nr));

		/* place its partition on the cprop list */
		if (!Y->on_cprop) {
			Y->cprop_next = env->cprop;
			env->cprop    = Y;
			Y->on_cprop   = true;
		}
	}
	ir_node *irn = y->node;
	if (get_irn_mode(irn) == mode_T) {
		/* mode_T nodes always produce tarval_top, so we must explicitly
		 * add its Projs to get constant evaluation to work */
		foreach_irn_out_r(irn, i, succ) {
			node_t *const proj = get_irn_node(succ);
			add_to_cprop(proj, env);
		}
	} else if (is_Block(irn)) {
		/* Due to the way we handle Phi's, we must place all Phis of a block on the list
		 * if someone placed the block. The Block is only placed if the reachability
		 * changes, and this must be re-evaluated in compute_Phi(). */
		for (ir_node *phi = get_Block_phis(irn); phi != NULL; phi = get_Phi_next(phi)) {
			node_t *p = get_irn_node(phi);
			add_to_cprop(p, env);
		}
	}
}

/**
 * Update the worklist: If Z is on worklist then add Z' to worklist.
 * Else add the smaller of Z and Z' to worklist.
 *
 * @param Z        the Z partition
 * @param Z_prime  the Z' partition, a previous part of Z
 * @param env      the environment
 */
static void update_worklist(partition_t *Z, partition_t *Z_prime, environment_t *env)
{
	if (Z->on_worklist || Z_prime->n_leaders < Z->n_leaders) {
		add_to_worklist(Z_prime, env);
	} else {
		add_to_worklist(Z, env);
	}
}

/**
 * Make all inputs to x no longer be F.def_use edges.
 *
 * @param x  the node
 */
static void move_edges_to_leader(node_t *x)
{
	ir_node *irn = x->node;
	foreach_irn_in_r(irn, i, pred_irn) {
		node_t  *pred = get_irn_node(pred_irn);
		ir_node *p    = pred->node;
		unsigned n    = get_irn_n_outs(p);
		for (unsigned j = 0; j < pred->n_followers; ++j) {
			ir_def_use_edge edge = p->o.out->edges[j];
			if (edge.pos == i && edge.use == irn) {
				/* found a follower edge to x, move it to the leader */
				/* remove this edge from the follower set */
				--pred->n_followers;
				p->o.out->edges[j] = p->o.out->edges[pred->n_followers];

				/* sort it into the leader set */
				unsigned k;
				for (k = pred->n_followers+1; k < n; ++k) {
					if (p->o.out->edges[k].pos >= edge.pos)
						break;
					p->o.out->edges[k-1] = p->o.out->edges[k];
				}
				/* place the new edge here */
				p->o.out->edges[k-1] = edge;

				/* edge found and moved */
				break;
			}
		}
	}
}

/**
 * Split a partition that has NO followers by a local list.
 *
 * @param Z    partition to split
 * @param g    a (non-empty) node list
 * @param env  the environment
 *
 * @return  a new partition containing the nodes of g
 */
static partition_t *split_no_followers(partition_t *Z, node_t *g, environment_t *env)
{
	dump_partition("Splitting ", Z);
	dump_list("by list ", g);

	assert(g != NULL);

	/* Remove g from Z. */
	unsigned n = 0;
	for (node_t *node = g; node != NULL; node = node->next) {
		assert(node->part == Z);
		list_del(&node->node_list);
		++n;
	}
	assert(n < Z->n_leaders);
	Z->n_leaders -= n;

	/* Move g to a new partition, Z'. */
	partition_t *Z_prime   = new_partition(env);
	int          max_input = 0;
	for (node_t *node = g; node != NULL; node = node->next) {
		list_add_tail(&node->node_list, &Z_prime->leader);
		node->part = Z_prime;
		if (node->max_user_input > max_input)
			max_input = node->max_user_input;
	}
	Z_prime->max_user_inputs = max_input;
	Z_prime->n_leaders       = n;

	check_partition(Z);
	check_partition(Z_prime);

	/* for now, copy the type info tag, it will be adjusted in split_by(). */
	Z_prime->type_is_B_or_C = Z->type_is_B_or_C;

	dump_partition("Now ", Z);
	dump_partition("Created new ", Z_prime);

	update_worklist(Z, Z_prime, env);

	return Z_prime;
}

/**
 * Make the follower -> leader transition for a node.
 *
 * @param n  the node
 */
static void follower_to_leader(node_t *n)
{
	assert(n->is_follower);

	DB((dbg, LEVEL_2, "%+F make the follower -> leader transition\n", n->node));
	n->is_follower = false;
	move_edges_to_leader(n);
	list_del(&n->node_list);
	list_add_tail(&n->node_list, &n->part->leader);
	++n->part->n_leaders;
}

/**
 * The environment for one race step.
 */
typedef struct step_env {
	node_t   *initial;  /**< The initial node list. */
	node_t   *unwalked; /**< The unwalked node list. */
	node_t   *walked;   /**< The walked node list. */
	unsigned  index;    /**< Next index of follower use_def edge. */
	unsigned  side;     /**< side number. */
} step_env;

/**
 * Return non-zero, if a input is a real follower
 *
 * @param irn    the node to check
 * @param input  number of the input
 */
static bool is_real_follower(const ir_node *irn, int input)
{
	switch (get_irn_opcode(irn)) {
	case iro_Confirm:
		if (input == 1) {
			/* ignore the Confirm bound input */
			return false;
		}
		break;
	case iro_Mux:
		if (input == 0) {
			/* ignore the Mux sel input */
			return false;
		}
		break;
	case iro_Phi: {
		/* dead inputs are not follower edges */
		ir_node *block = get_nodes_block(irn);
		node_t  *pred  = get_irn_node(get_Block_cfgpred(block, input));

		if (pred->type.tv == tarval_bottom)
			return false;
		break;
	}
	case iro_Sub:
	case iro_Shr:
	case iro_Shl:
	case iro_Shrs:
		if (input == 1) {
			/* only a Sub x,0 / Shift x,0 might be a follower */
			return false;
		}
		break;
	case iro_Add:
	case iro_Or:
	case iro_Eor: {
		node_t *pred = get_irn_node(get_irn_n(irn, input));
		if (is_tarval(pred->type.tv) && tarval_is_null(pred->type.tv))
			return false;
		break;
	}
	case iro_Mul: {
		node_t *pred = get_irn_node(get_irn_n(irn, input));
		if (is_tarval(pred->type.tv) && tarval_is_one(pred->type.tv))
			return false;
		break;
	}
	case iro_And: {
		node_t *pred = get_irn_node(get_irn_n(irn, input));
		if (is_tarval(pred->type.tv) && tarval_is_all_one(pred->type.tv))
			return false;
		break;
	}
	default:
		panic("opcode not implemented yet");
	}
	return true;
}

/**
 * Do one step in the race.
 */
static bool step(step_env *env)
{
	if (env->initial != NULL) {
		/* Move node from initial to unwalked */
		node_t *n = env->initial;
		env->initial = n->race_next;

		n->race_next  = env->unwalked;
		env->unwalked = n;

		return false;
	}

	while (env->unwalked != NULL) {
		/* let n be the first node in unwalked */
		node_t *n = env->unwalked;
		while (env->index < n->n_followers) {
			const ir_def_use_edge *edge = &n->node->o.out->edges[env->index];

			/* let m be n.F.def_use[index] */
			node_t *m = get_irn_node(edge->use);

			assert(m->is_follower);
			/*
			 * Some inputs, like the get_Confirm_bound are NOT
			 * real followers, sort them out.
			 */
			if (!is_real_follower(m->node, edge->pos)) {
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
					return false;
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
	return true;
}

/**
 * Clear the flags from a list and check for
 * nodes that where touched from both sides.
 *
 * @param list  the list
 */
static bool clear_flags(node_t *list)
{
	bool res = false;

	for (node_t *n = list; n != NULL; n = n->race_next) {
		if (n->flagged == 3) {
			/* we reach a follower from both sides, this will split congruent
			 * inputs and make it a leader. */
			follower_to_leader(n);
			res = true;
		}
		n->flagged = 0;
	}
	return res;
}

/**
 * Split a partition by a local list using the race.
 *
 * @param pX   pointer to the partition to split, might be changed!
 * @param gg   a (non-empty) node list
 * @param env  the environment
 *
 * @return  a new partition containing the nodes of gg
 */
static partition_t *split(partition_t **pX, node_t *gg, environment_t *env)
{
	partition_t *X = *pX;
	DEBUG_ONLY(static int run = 0;)

	DB((dbg, LEVEL_2, "Run %d ", run++));
	if (list_empty(&X->follower)) {
		/* if the partition has NO follower, we can use the fast
		   splitting algorithm. */
		return split_no_followers(X, gg, env);
	}
	/* else do the race */

	dump_partition("Splitting ", X);
	dump_list("by list ", gg);

	list_head tmp;
	INIT_LIST_HEAD(&tmp);

	/* Remove gg from X.leader and put into g */
	node_t *g = NULL;
	for (node_t *node = gg; node != NULL; node = node->next) {
		assert(node->part == X);
		assert(!node->is_follower);

		list_del(&node->node_list);
		list_add_tail(&node->node_list, &tmp);
		node->race_next = g;
		g               = node;
	}
	/* produce h */
	node_t *h = NULL;
	list_for_each_entry(node_t, node, &X->leader, node_list) {
		node->race_next = h;
		h               = node;
	}
	/* restore X.leader */
	list_splice(&tmp, &X->leader);

	step_env senv[2];
	senv[0].initial   = g;
	senv[0].unwalked  = NULL;
	senv[0].walked    = NULL;
	senv[0].index     = 0;
	senv[0].side      = 1;

	senv[1].initial   = h;
	senv[1].unwalked  = NULL;
	senv[1].walked    = NULL;
	senv[1].index     = 0;
	senv[1].side      = 2;

	/*
	 * Some informations on the race that are not stated clearly in Click's
	 * thesis.
	 * 1) A follower stays on the side that reach him first.
	 * 2) If the other side reaches a follower, if will be converted to
	 *    a leader. /This must be done after the race is over, else the
	 *    edges we are iterating on are renumbered./
	 * 3) /New leader might end up on both sides./
	 * 4) /If one side ends up with new Leaders, we must ensure that
	 *    they can split out by opcode, hence we have to put _every_
	 *    partition with new leader nodes on the cprop list, as
	 *    opcode splitting is done by split_by() at the end of
	 *    constant propagation./
	 */
	int winner;
	for (;;) {
		if (step(&senv[0])) {
			winner = 0;
			break;
		}
		if (step(&senv[1])) {
			winner = 1;
			break;
		}
	}
	assert(senv[winner].initial == NULL);
	assert(senv[winner].unwalked == NULL);

	/* clear flags from walked/unwalked */
	int shf         = winner;
	int transitions = clear_flags(senv[0].unwalked) << shf;
	transitions |= clear_flags(senv[0].walked)   << shf;
	shf ^= 1;
	transitions |= clear_flags(senv[1].unwalked) << shf;
	transitions |= clear_flags(senv[1].walked)   << shf;

	dump_race_list("winner ", senv[winner].walked);

	/* Move walked_{winner} to a new partition, X'. */
	partition_t *X_prime   = new_partition(env);
	int          max_input = 0;
	unsigned     n         = 0;
	for (node_t *node = senv[winner].walked; node != NULL; node = node->race_next) {
		list_del(&node->node_list);
		node->part = X_prime;
		if (node->is_follower) {
			list_add_tail(&node->node_list, &X_prime->follower);
		} else {
			list_add_tail(&node->node_list, &X_prime->leader);
			++n;
		}
		if (node->max_user_input > max_input)
			max_input = node->max_user_input;
	}
	X_prime->n_leaders       = n;
	X_prime->max_user_inputs = max_input;
	X->n_leaders            -= X_prime->n_leaders;

	/* for now, copy the type info tag, it will be adjusted in split_by(). */
	X_prime->type_is_B_or_C = X->type_is_B_or_C;

	/*
	 * Even if a follower was not checked by both sides, it might have
	 * loose its congruence, so we need to check this case for all follower.
	 */
	list_for_each_entry_safe(node_t, node, t, &X_prime->follower, node_list) {
		if (identity(node) == node) {
			follower_to_leader(node);
			transitions |= 1;
		}
	}

	check_partition(X);
	check_partition(X_prime);

	dump_partition("Now ", X);
	dump_partition("Created new ", X_prime);

	/* X' is the smaller part */
	add_to_worklist(X_prime, env);

	/*
	 * If there where follower to leader transitions, ensure that the nodes
	 * can be split out if necessary.
	 */
	if ((transitions & 1) && !X_prime->on_cprop) {
		/* place winner partition on the cprop list */
		X_prime->cprop_next = env->cprop;
		env->cprop          = X_prime;
		X_prime->on_cprop   = true;
	}
	if ((transitions & 2) && !X->on_cprop) {
		/* place other partition on the cprop list */
		X->cprop_next = env->cprop;
		env->cprop    = X;
		X->on_cprop   = true;
	}

	/* we have to ensure that the partition containing g is returned */
	if (winner != 0) {
		*pX = X_prime;
		return X;
	}

	return X_prime;
}

/**
 * Returns non-zero if the i'th input of a Phi node is live.
 *
 * @param phi  a Phi-node
 * @param i    an input number
 *
 * @return non-zero if the i'th input of the given Phi node is live
 */
static bool is_live_input(ir_node *phi, int i)
{
	if (i >= 0) {
		const ir_node *block     = get_nodes_block(phi);
		const ir_node *pred      = get_Block_cfgpred(block, i);
		const node_t  *pred_node = get_irn_node(pred);
		return pred_node->type.tv == tarval_top;
	}
	/* else it's the control input, always live */
	return true;
}

/**
 * Check whether a type is neither Bottom or a constant.
 * Note: U is handled like Bottom here, R is a constant.
 *
 * @param type  the type to check
 */
static bool type_is_neither_bottom_nor_const(const lattice_elem_t type)
{
	if (is_tarval(type.tv)) {
		if (type.tv == tarval_bottom)
			return false;
		if (tarval_is_constant(type.tv))
			return false;
	} else {
		/* is an entity */
		return false;
	}
	return true;
}

/**
 * Collect nodes to the touched list.
 *
 * @param list  the list which contains the nodes that must be evaluated
 * @param idx   the index of the def_use edge to evaluate
 * @param env   the environment
 */
static void collect_touched(list_head *list, int idx, environment_t *env)
{
	int end_idx = env->end_idx;

	list_for_each_entry(node_t, x, list, node_list) {
		if (idx == -1) {
			/* leader edges start AFTER follower edges */
			x->next_edge = x->n_followers;
		}
		unsigned num_edges = get_irn_n_outs(x->node);

		/* for all edges in x.L.def_use_{idx} */
		while (x->next_edge < num_edges) {
			const ir_def_use_edge *edge = &x->node->o.out->edges[x->next_edge];

			/* check if we have necessary edges */
			if (edge->pos > idx)
				break;

			++x->next_edge;

			ir_node *succ = edge->use;

			/* only non-commutative nodes */
			if (env->commutative &&
			    (idx == 0 || idx == 1) && is_op_commutative(get_irn_op(succ)))
				continue;

			/* ignore the "control input" for non-pinned nodes
			if we are running in GCSE mode */
			if (idx < end_idx && !get_irn_pinned(succ))
				continue;

			node_t *y = get_irn_node(succ);
			assert(get_irn_n(succ, idx) == x->node);

			/* ignore block edges touching followers */
			if (idx == -1 && y->is_follower)
				continue;

			if (tarval_is_constant(y->type.tv)) {
				unsigned  code = get_irn_opcode(succ);
				if (code == iro_Sub || code == iro_Cmp)
					add_to_cprop(y, env);
			}

			/* Partitions of constants should not be split simply because their Nodes have unequal
			   functions or incongruent inputs. */
			if (type_is_neither_bottom_nor_const(y->type) &&
				(!is_Phi(y->node) || is_live_input(y->node, idx))) {
					add_to_touched(y, env);
			}
		}
	}
}

/**
 * Collect commutative nodes to the touched list.
 *
 * @param list  the list which contains the nodes that must be evaluated
 * @param env   the environment
 */
static void collect_commutative_touched(list_head *list, environment_t *env)
{
	list_for_each_entry(node_t, x, list, node_list) {
		unsigned num_edges = get_irn_n_outs(x->node);

		x->next_edge = x->n_followers;

		/* for all edges in x.L.def_use_{idx} */
		while (x->next_edge < num_edges) {
			const ir_def_use_edge *edge = &x->node->o.out->edges[x->next_edge];
			ir_node               *succ;

			/* check if we have necessary edges */
			if (edge->pos > 1)
				break;

			++x->next_edge;
			if (edge->pos < 0)
				continue;

			succ = edge->use;

			/* only commutative nodes */
			if (!is_op_commutative(get_irn_op(succ)))
				continue;

			node_t *y = get_irn_node(succ);
			if (tarval_is_constant(y->type.tv)) {
				unsigned code = get_irn_opcode(succ);
				if (code == iro_Eor)
					add_to_cprop(y, env);
			}

			/* Partitions of constants should not be split simply because their Nodes have unequal
			   functions or incongruent inputs. */
			if (type_is_neither_bottom_nor_const(y->type)) {
				add_to_touched(y, env);
			}
		}
	}
}

/**
 * Split the partitions if caused by the first entry on the worklist.
 *
 * @param env  the environment
 */
static void cause_splits(environment_t *env)
{
	/* remove the first partition from the worklist */
	partition_t *X = env->worklist;
	env->worklist  = X->wl_next;
	X->on_worklist = false;

	dump_partition("Cause_split: ", X);

	if (env->commutative) {
		/* handle commutative nodes first */

		/* empty the touched set: already done, just clear the list */
		env->touched = NULL;

		collect_commutative_touched(&X->leader, env);
		collect_commutative_touched(&X->follower, env);

		for (partition_t *N, *Z = env->touched; Z != NULL; Z = N) {
			node_t   *touched      = Z->touched;
			node_t   *touched_aa   = NULL;
			node_t   *touched_ab   = NULL;
			unsigned  n_touched_aa = 0;
			unsigned  n_touched_ab = 0;

			assert(Z->touched != NULL);

			/* beware, split might change Z */
			N = Z->touched_next;

			/* remove it from the touched set */
			Z->on_touched = false;

			/* Empty local Z.touched. */
			for (node_t *n, *e = touched; e != NULL; e = n) {
				node_t *left  = get_irn_node(get_irn_n(e->node, 0));
				node_t *right = get_irn_node(get_irn_n(e->node, 1));

				assert(!e->is_follower);
				e->on_touched = false;
				n = e->next;

				/*
				 * Note: op(a, a) is NOT congruent to op(a, b).
				 * So, we must split the touched list.
				 */
				if (left->part == right->part) {
					e->next = touched_aa;
					touched_aa = e;
					++n_touched_aa;
				} else {
					e->next = touched_ab;
					touched_ab = e;
					++n_touched_ab;
				}
			}
			assert(n_touched_aa + n_touched_ab == Z->n_touched);
			Z->touched   = NULL;
			Z->n_touched = 0;

			if (0 < n_touched_aa && n_touched_aa < Z->n_leaders) {
				partition_t *Z_prime = Z;
				DB((dbg, LEVEL_2, "Split part%d by touched_aa\n", Z_prime->nr));
				split(&Z_prime, touched_aa, env);
			} else
				assert(n_touched_aa <= Z->n_leaders);

			if (0 < n_touched_ab && n_touched_ab < Z->n_leaders) {
				partition_t *Z_prime = Z;
				DB((dbg, LEVEL_2, "Split part%d by touched_ab\n", Z_prime->nr));
				split(&Z_prime, touched_ab, env);
			} else
				assert(n_touched_ab <= Z->n_leaders);
		}
	}

	/* combine temporary leader and follower list */
	for (int idx = -1; idx <= X->max_user_inputs; ++idx) {
		/* empty the touched set: already done, just clear the list */
		env->touched = NULL;

		collect_touched(&X->leader, idx, env);
		collect_touched(&X->follower, idx, env);

		for (partition_t *N, *Z = env->touched; Z != NULL; Z = N) {
			node_t   *touched   = Z->touched;
			unsigned  n_touched = Z->n_touched;

			assert(Z->touched != NULL);

			/* beware, split might change Z */
			N = Z->touched_next;

			/* remove it from the touched set */
			Z->on_touched = false;

			/* Empty local Z.touched. */
			for (node_t *e = touched; e != NULL; e = e->next) {
				assert(!e->is_follower);
				e->on_touched = false;
			}
			Z->touched   = NULL;
			Z->n_touched = 0;

			if (0 < n_touched && n_touched < Z->n_leaders) {
				DB((dbg, LEVEL_2, "Split part%d by touched\n", Z->nr));
				split(&Z, touched, env);
			} else
				assert(n_touched <= Z->n_leaders);
		}
	}
}

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
                                  partition_t **P, environment_t *env)
{
	/* Let map be an empty mapping from the range of What to (local) list of Nodes. */
	listmap_t map;
	listmap_init(&map);
	list_for_each_entry(node_t, x, &X->leader, node_list) {
		void *id = What(x, env);
		if (id == NULL) {
			/* input not allowed, ignore */
			continue;
		}
		/* Add x to map[What(x)]. */
		listmap_entry_t *entry = listmap_find(&map, id);
		x->next     = entry->list;
		entry->list = x;
	}
	/* Let P be a set of Partitions. */

	/* for all sets S except one in the range of map do */
	for (listmap_entry_t *iter = map.values; iter != NULL; iter = iter->next) {
		if (iter->next == NULL) {
			/* this is the last entry, ignore */
			break;
		}
		node_t *S = iter->list;

		/* Add SPLIT( X, S ) to P. */
		DB((dbg, LEVEL_2, "Split part%d by WHAT = %s\n", X->nr, what_reason));
		partition_t *R = split(&X, S, env);
		R->split_next = *P;
		*P            = R;
	}
	/* Add X to P. */
	X->split_next = *P;
	*P            = X;

	listmap_term(&map);
	return *P;
}

/** lambda n.(n.type) */
static void *lambda_type(const node_t *node, environment_t *env)
{
	(void)env;
	return node->type.tv;
}

/** lambda n.(n.opcode) */
static void *lambda_opcode(const node_t *node, environment_t *env)
{
	opcode_key_t  key   = { .irn = node->node };
	opcode_key_t *entry = set_insert(opcode_key_t, env->opcode2id_map, &key, sizeof(key), opcode_hash(&key));
	return entry;
}

/** lambda n.(n[i].partition) */
static void *lambda_partition(const node_t *node, environment_t *env)
{
	int      i   = env->lambda_input;
	ir_node *irn = node->node;
	if (i >= get_irn_arity(irn) || (i == -1 && is_Block(irn))) {
		/*
		 * We are outside the allowed range: This can happen even
		 * if we have split by opcode first: doing so might move follower
		 * to Leaders and those will have a different opcode!
		 * Note that in this case the partition is on the cprop list and will be
		 * split again.
		 */
		return NULL;
	}

	/* ignore the "control input" for non-pinned nodes
	   if we are running in GCSE mode */
	ir_node *skipped = skip_Proj(irn);
	if (i < env->end_idx && !get_irn_pinned(skipped))
		return NULL;

	ir_node *pred = i == -1 ? get_irn_n(skipped, i) : get_irn_n(irn, i);
	node_t  *p    = get_irn_node(pred);
	return p->part;
}

/** lambda n.(n[i].partition) for commutative nodes */
static void *lambda_commutative_partition(const node_t *node, environment_t *env)
{
	int i = env->lambda_input;
	if (i >= get_irn_arity(node->node)) {
		/*
		 * We are outside the allowed range: This can happen even
		 * if we have split by opcode first: doing so might move follower
		 * to Leaders and those will have a different opcode!
		 * Note that in this case the partition is on the cprop list and will be
		 * split again.
		 */
		return NULL;
	}

	/* ignore the "control input" for non-pinned nodes
	   if we are running in GCSE mode */
	ir_node *irn     = node->node;
	ir_node *skipped = skip_Proj(irn);
	if (i < env->end_idx && !get_irn_pinned(skipped))
		return NULL;

	if (i == -1) {
		ir_node *pred = get_irn_n(skipped, i);
		node_t  *p    = get_irn_node(pred);
		return p->part;
	}

	if (is_op_commutative(get_irn_op(irn))) {
		/* normalize partition order by returning the "smaller" on input 0,
		   the "bigger" on input 1. */
		ir_node     *left  = get_binop_left(irn);
		partition_t *pl    = get_irn_node(left)->part;
		ir_node     *right = get_binop_right(irn);
		partition_t *pr    = get_irn_node(right)->part;

		if (i == 0)
			return MIN(pl, pr);
		else
			return MAX(pl, pr);
	} else {
		/* a not split out follower */
		ir_node *pred = get_irn_n(irn, i);
		node_t  *p    = get_irn_node(pred);

		return p->part;
	}
}

/**
 * Returns true if a type is a constant (and NOT Top or Bottom).
 */
static bool is_con(const lattice_elem_t type)
{
	/* be conservative */
	if (is_tarval(type.tv))
		return tarval_is_constant(type.tv);
	assert(is_entity(type.ent));
	return true;
}

/**
 * Implements split_by().
 *
 * @param X    the partition to split
 * @param env  the environment
 */
static void split_by(partition_t *X, environment_t *env)
{
	dump_partition("split_by", X);

	if (X->n_leaders == 1) {
		/* we have only one leader, no need to split, just check its type */
		node_t *x = get_first_node(X);
		X->type_is_B_or_C = x->type.tv == tarval_bottom || is_con(x->type);
		return;
	}

	DEBUG_ONLY(what_reason = "lambda n.(n.type)";)
	partition_t *P = NULL;
	P = split_by_what(X, lambda_type, &P, env);
	dump_split_list(P);

	/* adjust the type tags, we have split partitions by type */
	for (partition_t *I = P; I != NULL; I = I->split_next) {
		node_t *x = get_first_node(I);
		I->type_is_B_or_C = x->type.tv == tarval_bottom || is_con(x->type);
	}

	do {
		partition_t *Y = P;

		P = P->split_next;
		if (Y->n_leaders > 1) {
			/* we do not want split the Bottom or constant partitions */
			if (!Y->type_is_B_or_C) {
				partition_t *Q = NULL;

				DEBUG_ONLY(what_reason = "lambda n.(n.opcode)";)
				Q = split_by_what(Y, lambda_opcode, &Q, env);
				dump_split_list(Q);

				do {
					partition_t *Z = Q;

					Q = Q->split_next;
					if (Z->n_leaders > 1) {
						const node_t *first = get_first_node(Z);
						int          arity  = get_irn_arity(first->node);
						what_func    what = lambda_partition;
						DEBUG_ONLY(char buf[64];)

						if (env->commutative && is_op_commutative(get_irn_op(first->node)))
							what = lambda_commutative_partition;

						/*
						 * BEWARE: during splitting by input 2 for instance we might
						 * create new partitions which are different by input 1, so collect
						 * them and split further.
						 */
						Z->split_next = NULL;
						partition_t *R = Z;
						partition_t *S = NULL;

						for (int input = arity - 1; input >= -1; --input) {
							do {
								partition_t *Z_prime = R;

								R = R->split_next;
								if (Z_prime->n_leaders > 1) {
									env->lambda_input = input;
									DEBUG_ONLY(snprintf(buf, sizeof(buf), "lambda n.(n[%d].partition)", input);)
									DEBUG_ONLY(what_reason = buf;)
									S = split_by_what(Z_prime, what, &S, env);
									dump_split_list(S);
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
}

/**
 * (Re-)compute the type for a given node.
 *
 * @param node  the node
 */
static void default_compute(node_t *node)
{
	ir_node *irn = node->node;
	if (get_irn_mode(irn) == mode_X)
		node->type.tv = tarval_top; /* reachable */

	/* if any of the data inputs have type bottom, the result is type bottom */
	if (!is_memop(irn)) {
		foreach_irn_in_r(irn, i, pred) {
			node_t *const p = get_irn_node(pred);
			if (p->type.tv == tarval_bottom) {
				node->type.tv = tarval_bottom;
				return;
			}
		}
	}

	ir_tarval *value = computed_value(irn);
	if (!tarval_is_constant(value)) {
		/* In case of undefined behavior (e.g. division by zero) computed_value
		 * returns tarval_bottom. However, we handle the node like an Unknown node
		 * and set its type to tarval_top (see compute_Unknown for details).
		 */
		node->type.tv = tarval_top;
	} else {
		node->type.tv = value;
	}
}

/**
 * (Re-)compute the type for a Block node.
 *
 * @param node  the node
 */
static void compute_Block(node_t *node)
{
	ir_node  *const block = node->node;
	ir_graph *const irg   = get_irn_irg(block);

	if (block == get_irg_start_block(irg)) {
		/* The start block is always reachable. */
		node->type.tv = tarval_top; /* reachable */
		return;
	}

	for (int i = get_Block_n_cfgpreds(block); i-- > 0; ) {
		node_t *pred = get_irn_node(get_Block_cfgpred(block, i));

		/* A block is reachable, if at least one predecessor is reachable. */
		if (pred->type.tv == tarval_top) {
			node->type.tv = tarval_top; /* reachable */
			return;
		}
	}
	node->type.tv = tarval_bottom; /* unreachable */
}

/**
 * (Re-)compute the type for a Bad node.
 *
 * @param node  the node
 */
static void compute_Bad(node_t *node)
{
	/* Bad nodes ALWAYS compute Bottom */
	node->type.tv = tarval_bottom;
}

/**
 * (Re-)compute the type for an Unknown node.
 *
 * @param node  the node
 */
static void compute_Unknown(node_t *node)
{
	/* If we would return tarval_bottom, a Cond with an Unknown
	 * predecessor would lose both Projs and the graph would break apart. */
	node->type.tv = tarval_top;
}

/**
 * (Re-)compute the type for a Jmp node.
 *
 * @param node  the node
 */
static void compute_Jmp(node_t *node)
{
	node_t *block = get_irn_node(get_nodes_block(node->node));
	node->type = block->type;
}

static void join(lattice_elem_t* const res, lattice_elem_t const* const a, lattice_elem_t const* const b)
{
	if (a->tv == b->tv) {
		*res = *a;
	} else if (a->tv == tarval_bottom) {
		*res = *b;
	} else if (b->tv == tarval_bottom) {
		*res = *a;
	} else {
		res->tv = tarval_top;
	}
}

/**
 * (Re-)compute the type for a Mux.
 *
 * This should match the value computed for the Phi
 * that was if-converted to the Mux.
 *
 * @param node  the node
 */
static void compute_Mux(node_t *node)
{
	ir_node   *mux    = node->node;
	node_t    *sel    = get_irn_node(get_Mux_sel(mux));
	node_t    *f      = get_irn_node(get_Mux_false(mux));
	node_t    *t      = get_irn_node(get_Mux_true(mux));
	ir_tarval *sel_tv = sel->type.tv;

	if (sel_tv == tarval_bottom) {
		node->type.tv = tarval_bottom;
	} else if (sel_tv == tarval_b_false) {
		node->type.tv = f->type.tv;
	} else if (sel_tv == tarval_b_true) {
		node->type.tv = t->type.tv;
	} else {
		assert(sel_tv == tarval_top);
		/* Join of false and true operands. */
		join(&node->type, &f->type, &t->type);
	}
}

/**
 * (Re-)compute the type for the Return node.
 *
 * @param node  the node
 */
static void compute_Return(node_t *node)
{
	/* The Return node is NOT dead if it is in a reachable block.
	 * This is already checked in compute(). so we can return
	 * Reachable here. */
	node->type.tv = tarval_top; /* reachable */
}

/**
 * (Re-)compute the type for the End node.
 *
 * @param node  the node
 */
static void compute_End(node_t *node)
{
	/* the End node is NOT dead of course */
	node->type.tv = tarval_top; /* reachable */
}

/**
 * (Re-)compute the type for an Address node.
 *
 * @param node  the node
 */
static void compute_Address(node_t *node)
{
	node->type.ent = get_Address_entity(node->node);
}

/**
 * (Re-)compute the type for an Offset node.
 *
 * @param node  the node
 */
static void compute_Offset(node_t *node)
{
	node->type.tv = computed_value(node->node);
}

/**
 * (Re-)compute the type for an Align node.
 *
 * @param node  the node
 */
static void compute_Align(node_t *node)
{
	node->type.tv = computed_value(node->node);
}

/**
 * (Re-)compute the type for a Size node.
 *
 * @param node  the node
 */
static void compute_Size(node_t *node)
{
	node->type.tv = computed_value(node->node);
}

/**
 * (Re-)compute the type for a Phi node.
 *
 * @param node  the node
 */
static void compute_Phi(node_t *node)
{
	ir_node *phi   = node->node;
	node_t  *block = get_irn_node(get_nodes_block(phi));

	/* Phi implements the Join operation */
	lattice_elem_t type = { .tv = tarval_bottom };
	for (int i = get_Phi_n_preds(phi); i-- > 0; ) {
		/* Ignore values coming from unreachable control flow. */
		node_t *pred_X = get_irn_node(get_Block_cfgpred(block->node, i));
		if (pred_X->type.tv == tarval_bottom)
			continue;

		node_t *const pred = get_irn_node(get_Phi_pred(phi, i));
		join(&type, &type, &pred->type);
		if (type.tv == tarval_top) /* Early out. */
			break;
	}
	node->type = type;
}

/**
 * (Re-)compute the type for an Add. Special case: one nodes is a Zero Const.
 *
 * @param node  the node
 */
static void compute_Add(node_t *node)
{
	ir_node        *add = node->node;
	node_t         *l   = get_irn_node(get_Add_left(add));
	node_t         *r   = get_irn_node(get_Add_right(add));
	lattice_elem_t  a   = l->type;
	lattice_elem_t  b   = r->type;

	if (a.tv == tarval_bottom || b.tv == tarval_bottom) {
		node->type.tv = tarval_bottom;
	} else if (a.tv == tarval_top || b.tv == tarval_top) {
		node->type.tv = tarval_top;
	} else {
		/* x + 0 = 0 + x = x, but beware of floating point +0 + -0, so we
		   must call tarval_add() first to handle this case! */
		if (is_tarval(a.tv)) {
			if (is_tarval(b.tv)) {
				node->type.tv = tarval_add(a.tv, b.tv);
				return;
			}
			ir_mode *mode = get_tarval_mode(a.tv);
			if (a.tv == get_mode_null(mode)) {
				node->type = b;
				return;
			}
		} else if (is_tarval(b.tv)) {
			ir_mode *mode = get_tarval_mode(b.tv);
			if (b.tv == get_mode_null(mode)) {
				node->type = a;
				return;
			}
		}
		node->type.tv = tarval_top;
	}
}

/**
 * (Re-)compute the type for a Sub. Special case: both nodes are congruent.
 *
 * @param node  the node
 */
static void compute_Sub(node_t *node)
{
	ir_node        *sub = node->node;
	node_t         *l   = get_irn_node(get_Sub_left(sub));
	node_t         *r   = get_irn_node(get_Sub_right(sub));
	lattice_elem_t  a   = l->type;
	lattice_elem_t  b   = r->type;

	if (a.tv == tarval_bottom || b.tv == tarval_bottom) {
		node->type.tv = tarval_bottom;
	} else if (is_con(a) && is_con(b)) {
		if (is_tarval(a.tv) && is_tarval(b.tv)) {
			node->type.tv = tarval_sub(a.tv, b.tv);
		} else if (is_tarval(b.tv) && tarval_is_null(b.tv)) {
			node->type = a;
		} else {
			node->type.tv = tarval_top;
		}
	} else if (r->part == l->part &&
	           (!mode_is_float(get_irn_mode(l->node)))) {
		/*
		 * BEWARE: a - a is NOT always 0 for floating Point values, as
		 * NaN - NaN = NaN, so we must check this here.
		 */
		ir_mode   *mode = get_irn_mode(sub);
		ir_tarval *tv   = get_mode_null(mode);

		/* if the node was ONCE evaluated by all constants, but now
		   this breaks AND we get from the argument partitions a different
		   result, switch to top.
		   This happens because initially all nodes are in the same partition ... */
		if (node->type.tv != tv)
			tv = tarval_top;
		node->type.tv = tv;
	} else {
		node->type.tv = tarval_top;
	}
}

/**
 * (Re-)compute the type for an Eor. Special case: both nodes are congruent.
 *
 * @param node  the node
 */
static void compute_Eor(node_t *node)
{
	ir_node        *eor = node->node;
	node_t         *l   = get_irn_node(get_Eor_left(eor));
	node_t         *r   = get_irn_node(get_Eor_right(eor));
	lattice_elem_t  a   = l->type;
	lattice_elem_t  b   = r->type;

	if (a.tv == tarval_bottom || b.tv == tarval_bottom) {
		node->type.tv = tarval_bottom;
	} else if (is_con(a) && is_con(b)) {
		if (is_tarval(a.tv) && is_tarval(b.tv)) {
			node->type.tv = tarval_eor(a.tv, b.tv);
		} else if (is_tarval(a.tv) && tarval_is_null(a.tv)) {
			node->type = b;
		} else if (is_tarval(b.tv) && tarval_is_null(b.tv)) {
			node->type = a;
		} else {
			node->type.tv = tarval_top;
		}
	} else if (r->part == l->part) {
		ir_mode   *mode = get_irn_mode(eor);
		ir_tarval *tv   = get_mode_null(mode);

		/* if the node was ONCE evaluated by all constants, but now
		   this breaks AND we get from the argument partitions a different
		   result, switch to top.
		   This happens because initially all nodes are in the same partition ... */
		if (node->type.tv != tv)
			tv = tarval_top;
		node->type.tv = tv;
	} else {
		node->type.tv = tarval_top;
	}
}

/**
 * (Re-)compute the type for Cmp.
 *
 * @param node  the node
 */
static void compute_Cmp(node_t *node)
{
	ir_node        *cmp = node->node;
	node_t         *l   = get_irn_node(get_Cmp_left(cmp));
	node_t         *r   = get_irn_node(get_Cmp_right(cmp));
	lattice_elem_t  a   = l->type;
	lattice_elem_t  b   = r->type;

	if (a.tv == tarval_bottom || b.tv == tarval_bottom) {
		node->type.tv = tarval_bottom;
	} else if (is_con(a) && is_con(b)) {
		node->type.tv = computed_value(cmp);

	/*
	 * BEWARE: a == a is NOT always True for floating Point values, as
	 * NaN != NaN is defined, so we must check this here.
	 * (while for some pnc we could still optimize we have to stay
	 *  consistent with compute_Cmp, so do not do anything for floats)
	 */
	} else if (r->part == l->part && !mode_is_float(get_irn_mode(l->node))) {
		ir_relation  relation = get_Cmp_relation(cmp);
		ir_tarval   *tv       = relation &ir_relation_equal ? tarval_b_true : tarval_b_false;

		/* if the node was ONCE evaluated to a constant, but now
		   this breaks AND we get from the argument partitions a different
		   result, ensure monotony by switching to top.
		   This happens because initially all nodes are in the same partition ... */
		if (node->type.tv == tarval_top)
			tv = tarval_top;
		else if (node->type.tv != tv && tarval_is_constant(node->type.tv))
			tv = tarval_top;
		node->type.tv = tv;
	} else {
		node->type.tv = tarval_top;
	}
}

/**
 * (Re-)compute the type for a Proj(Cond).
 *
 * @param node  the node
 * @param cond  the predecessor Cond node
 */
static void compute_Proj_Cond(node_t *node, ir_node *cond)
{
	ir_node *sel      = get_Cond_selector(cond);
	node_t  *selector = get_irn_node(sel);
	if (selector->type.tv == tarval_bottom) {
		node->type.tv = tarval_bottom;
		return;
	}

	ir_node *proj = node->node;
	unsigned pn   = get_Proj_num(proj);
	if (pn == pn_Cond_true) {
		if (selector->type.tv == tarval_b_false) {
			node->type.tv = tarval_bottom; /* unreachable */
		} else {
			node->type.tv = tarval_top; /* reachable */
		}
	} else {
		assert(pn == pn_Cond_false);
		if (selector->type.tv == tarval_b_true) {
			node->type.tv = tarval_bottom; /* unreachable */
		} else {
			node->type.tv = tarval_top; /* reachable */
		}
	}
}

static void compute_Proj_Switch(node_t *node, ir_node *switchn)
{
	ir_node *sel      = get_Switch_selector(switchn);
	node_t  *selector = get_irn_node(sel);
	if (selector->type.tv == tarval_bottom) {
		node->type.tv = tarval_bottom; /* unreachable */
	} else if (selector->type.tv == tarval_top) {
		node->type.tv = tarval_top; /* reachable */
	} else {
		ir_node               *proj      = node->node;
		unsigned               pn        = get_Proj_num(proj);
		const ir_switch_table *table     = get_Switch_table(switchn);
		size_t                 n_entries = ir_switch_table_get_n_entries(table);

		for (size_t e = 0; e < n_entries; ++e) {
			ir_switch_table_entry const *const entry = ir_switch_table_get_entry_const(table, e);
			if (tarval_in_range(entry->min, selector->type.tv, entry->max)) {
				node->type.tv = entry->pn == pn ? tarval_top : tarval_bottom;
				return;
			}
		}

		/* no entry matched: default */
		node->type.tv
			= pn == pn_Switch_default ? tarval_top : tarval_bottom;
	}
}

/**
 * (Re-)compute the type for a Proj-Node.
 *
 * @param node  the node
 */
static void compute_Proj(node_t *node)
{
	ir_node *proj  = node->node;
	node_t  *block = get_irn_node(get_nodes_block(proj));

	if (!is_reachable(block)) {
		/* a Proj in an unreachable Block stays Bottom */
		node->type.tv = tarval_bottom;
		return;
	}

	ir_mode *mode = get_irn_mode(proj);
	if (mode == mode_M) {
		/* mode M is always top */
		node->type.tv = tarval_top;
		return;
	}

	ir_node *pred = get_Proj_pred(proj);
	if (mode == mode_X) {
		/* handle mode_X nodes */
		switch (get_irn_opcode(pred)) {
		case iro_Cond:
			compute_Proj_Cond(node, pred);
			return;
		case iro_Switch:
			compute_Proj_Switch(node, pred);
			return;
		default:
			break;
		}
	}

	node_t *pred_node = get_irn_node(pred);
	if (pred_node->type.tv == tarval_bottom) {
		/* if the predecessor is Bottom, its Proj follow */
		node->type.tv = tarval_bottom;
		return;
	}

	default_compute(pred_node);
	node->type.tv = pred_node->type.tv;
}

/**
 * (Re-)compute the type for a Confirm.
 *
 * @param node  the node
 */
static void compute_Confirm(node_t *node)
{
	const ir_node *confirm = node->node;

	if (get_Confirm_relation(confirm) == ir_relation_equal) {
		node_t *bound = get_irn_node(get_Confirm_bound(confirm));

		if (is_con(bound->type)) {
			/* is equal to a constant */
			node->type = bound->type;
			return;
		}
	}
	/* a Confirm is a copy OR a Const */
	node_t *pred = get_irn_node(get_Confirm_value(confirm));
	node->type = pred->type;
}

/**
 * (Re-)compute the type for a given node.
 *
 * @param node  the node
 */
static void compute(node_t *node)
{
#ifndef VERIFY_MONOTONE
	/*
	 * Once a node reaches top, the type cannot rise further
	 * in the lattice and we can stop computation.
	 * Do not take this exit if the monotony verifier is
	 * enabled to catch errors.
	 */
	if (node->type.tv == tarval_top)
		return;
#endif

	/* for pinned nodes, check its control input */
	ir_node *irn = node->node;
	if (!is_Block(irn) && get_irn_pinned(skip_Proj(irn))) {
		node_t *block = get_irn_node(get_nodes_block(irn));

		if (!is_reachable(block)) {
			node->type.tv = tarval_bottom;
			return;
		}
	}

	compute_func func = (compute_func)node->node->op->ops.generic;
	if (func != NULL)
		func(node);
}

/*
 * Identity functions: Note that one might think that identity() is just a
 * synonym for equivalent_node(). While this is true, we cannot use it for the algorithm
 * here, because it expects that the identity node is one of the inputs, which is NOT
 * always true for equivalent_node() which can handle (and does sometimes) DAGs.
 * So, we have our own implementation, which copies some parts of equivalent_node()
 */

/**
 * Calculates the Identity for Phi nodes
 */
static node_t *identity_Phi(node_t *node)
{
	ir_node *phi    = node->node;
	ir_node *block  = get_nodes_block(phi);
	node_t  *n_part = NULL;

	/* special rule: kept PhiM nodes have to create their own partition
	 * (as they represent the observable behaviour of a loop running endless) */
	if (get_Phi_loop(phi)) {
		assert(get_irn_mode(phi) == mode_M);
		return node;
	}

	for (int i = get_Phi_n_preds(phi); i-- > 0; ) {
		node_t *pred_X = get_irn_node(get_Block_cfgpred(block, i));
		if (pred_X->type.tv == tarval_bottom)
			continue;

		node_t *pred = get_irn_node(get_Phi_pred(phi, i));
		if (n_part == NULL) {
			n_part = pred;
		} else if (n_part->part != pred->part) {
			/* incongruent inputs, not a follower */
			return node;
		}
	}
	/* if n_part is NULL here, all inputs path are dead, the Phi computes
	 * tarval_bottom, is in the Bottom partition and should NOT being split! */
	assert(n_part != NULL);
	return n_part;
}

/**
 * Calculates the Identity for commutative 0 neutral nodes.
 */
static node_t *identity_comm_zero_binop(node_t *node)
{
	ir_node *op   = node->node;
	ir_mode *mode = get_irn_mode(op);

	/* for FP these optimizations are only allowed if fp_strict_algebraic is disabled */
	if (mode_is_float(mode) && !ir_imprecise_float_transforms_allowed())
		return node;

	/* node: no input should be tarval_bottom, else the binop would be also
	 * Bottom and not being split. */
	node_t    *a    = get_irn_node(get_binop_left(op));
	node_t    *b    = get_irn_node(get_binop_right(op));
	ir_tarval *zero = get_mode_null(mode);
	if (a->type.tv == zero)
		return b;
	if (b->type.tv == zero)
		return a;
	return node;
}

/**
 * Calculates the Identity for Shift nodes.
 */
static node_t *identity_shift(node_t *node)
{
	ir_node   *op   = node->node;
	node_t    *b    = get_irn_node(get_binop_right(op));
	ir_mode   *mode = get_irn_mode(b->node);
	ir_tarval *zero = get_mode_null(mode);

	/* node: no input should be tarval_bottom, else the binop would be also
	 * Bottom and not being split. */
	if (b->type.tv == zero)
		return get_irn_node(get_binop_left(op));
	return node;
}

/**
 * Calculates the Identity for Mul nodes.
 */
static node_t *identity_Mul(node_t *node)
{
	ir_node *op   = node->node;
	ir_mode *mode = get_irn_mode(op);

	/* for FP these optimizations are only allowed if fp_strict_algebraic is disabled */
	if (mode_is_float(mode) && !ir_imprecise_float_transforms_allowed())
		return node;

	/* node: no input should be tarval_bottom, else the binop would be also
	 * Bottom and not being split. */
	node_t    *a   = get_irn_node(get_Mul_left(op));
	node_t    *b   = get_irn_node(get_Mul_right(op));
	ir_tarval *one = get_mode_one(mode);
	if (a->type.tv == one)
		return b;
	if (b->type.tv == one)
		return a;
	return node;
}

/**
 * Calculates the Identity for Sub nodes.
 */
static node_t *identity_Sub(node_t *node)
{
	ir_node *sub  = node->node;
	ir_mode *mode = get_irn_mode(sub);

	/* for FP these optimizations are only allowed if fp_strict_algebraic is disabled */
	if (mode_is_float(mode) && !ir_imprecise_float_transforms_allowed())
		return node;

	/* node: no input should be tarval_bottom, else the binop would be also
	 * Bottom and not being split. */
	node_t *b = get_irn_node(get_Sub_right(sub));
	if (b->type.tv == get_mode_null(mode))
		return get_irn_node(get_Sub_left(sub));
	return node;
}

/**
 * Calculates the Identity for And nodes.
 */
static node_t *identity_And(node_t *node)
{
	ir_node   *andnode = node->node;
	node_t    *a       = get_irn_node(get_And_left(andnode));
	node_t    *b       = get_irn_node(get_And_right(andnode));
	ir_tarval *neutral = get_mode_all_one(get_irn_mode(andnode));

	/* node: no input should be tarval_bottom, else the And would be also
	 * Bottom and not being split. */
	if (a->type.tv == neutral)
		return b;
	if (b->type.tv == neutral)
		return a;
	return node;
}

/**
 * Calculates the Identity for Confirm nodes.
 */
static node_t *identity_Confirm(node_t *node)
{
	ir_node *confirm = node->node;

	/* a Confirm is always a Copy */
	return get_irn_node(get_Confirm_value(confirm));
}

/**
 * Calculates the Identity for Mux nodes.
 */
static node_t *identity_Mux(node_t *node)
{
	ir_node *mux = node->node;
	node_t  *t   = get_irn_node(get_Mux_true(mux));
	node_t  *f   = get_irn_node(get_Mux_false(mux));
	/*node_t  *sel; */

	if (t->part == f->part)
		return t;

	/* for now, the 1-input identity is not supported */
	return node;
}

/**
 * Calculates the Identity for nodes.
 */
static node_t *identity(node_t *node)
{
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
		return identity_shift(node);
	case iro_And:
		return identity_And(node);
	case iro_Sub:
		return identity_Sub(node);
	case iro_Confirm:
		return identity_Confirm(node);
	case iro_Mux:
		return identity_Mux(node);
	default:
		return node;
	}
}

/**
 * Node follower is a (new) follower of leader, segregate leader
 * out edges.
 */
static void segregate_def_use_chain_1(const ir_node *follower, node_t *leader)
{
	DB((dbg, LEVEL_2, "%+F is a follower of %+F\n", follower, leader->node));
	/* The leader edges must remain sorted, but follower edges can
	   be unsorted. */
	ir_node *l = leader->node;
	unsigned n = get_irn_n_outs(l);
	for (unsigned i = leader->n_followers; i < n; ++i) {
		if (l->o.out->edges[i].use == follower) {
			ir_def_use_edge t = l->o.out->edges[i];

			for (unsigned j = i; j-- > leader->n_followers; )
				l->o.out->edges[j+1] = l->o.out->edges[j];
			l->o.out->edges[leader->n_followers] = t;
			++leader->n_followers;
			break;
		}
	}
}

/**
 * Node follower is a (new) follower segregate its leader
 * out edges.
 *
 * @param follower  the follower IR node
 */
static void segregate_def_use_chain(const ir_node *follower)
{
	foreach_irn_in_r(follower, i, pred) {
		node_t *const node = get_irn_node(pred);
		segregate_def_use_chain_1(follower, node);
	}
}

/**
 * Propagate constant evaluation.
 *
 * @param env  the environment
 */
static void propagate(environment_t *env)
{
	while (env->cprop != NULL) {
		void *oldopcode = NULL;

		/* remove the first partition X from cprop */
		partition_t *X = env->cprop;
		X->on_cprop = false;
		env->cprop  = X->cprop_next;

		bool old_type_was_B_or_C = X->type_is_B_or_C;

		DB((dbg, LEVEL_2, "Propagate type on part%d\n", X->nr));
		node_t   *fallen   = NULL;
		unsigned  n_fallen = 0;
		for (;;) {
			if (list_empty(&X->cprop))
				break;

			/* remove the first Node x from X.cprop */
			node_t *x = list_entry(X->cprop.next, node_t, cprop_list);

			//assert(x->part == X);
			list_del(&x->cprop_list);
			x->on_cprop = false;

			if (x->is_follower && identity(x) == x) {
				/* check the opcode first */
				if (oldopcode == NULL) {
					oldopcode = lambda_opcode(get_first_node(X), env);
				}
				if (oldopcode != lambda_opcode(x, env)) {
					if (!x->on_fallen) {
						/* different opcode -> x falls out of this partition */
						x->next      = fallen;
						x->on_fallen = true;
						fallen       = x;
						++n_fallen;
						DB((dbg, LEVEL_2, "Add node %+F to fallen\n", x->node));
					}
				}

				/* x will make the follower -> leader transition */
				follower_to_leader(x);

				/* In case of a follower -> leader transition of a Phi node
				 * we have to ensure that the current partition will be split
				 * by lambda n.(n[i].partition).
				 *
				 * This split may already happened before when some predecessors
				 * of the Phi's Block are unreachable. Thus, we have to put the
				 * current partition in the worklist to repeat the check.
				 */
				if (is_Phi(x->node) && !x->part->on_worklist)
					add_to_worklist(x->part, env);
			}

			/* compute a new type for x */
			lattice_elem_t old_type = x->type;
			DB((dbg, LEVEL_3, "computing type of %+F\n", x->node));
			compute(x);
			if (x->type.tv != old_type.tv) {
				DB((dbg, LEVEL_2, "node %+F has changed type from %+F to %+F\n", x->node, old_type, x->type));
				verify_type(old_type, x);

				if (!x->on_fallen) {
					/* Add x to fallen. Nodes might fall from T -> const -> _|_, so check that they are
					   not already on the list. */
					x->next      = fallen;
					x->on_fallen = true;
					fallen       = x;
					++n_fallen;
					DB((dbg, LEVEL_2, "Add node %+F to fallen\n", x->node));
				}
				foreach_irn_out_r(x->node, i, succ) {
					node_t *const y = get_irn_node(succ);
					/* Add y to y.partition.cprop. */
					add_to_cprop(y, env);
				}
			}
		}

		partition_t *Y;
		if (n_fallen > 0 && n_fallen != X->n_leaders) {
			DB((dbg, LEVEL_2, "Splitting part%d by fallen\n", X->nr));
			Y = split(&X, fallen, env);
			/*
			 * We have split out fallen node. The type of the result
			 * partition is NOT set yet.
			 */
			Y->type_is_B_or_C = false;
		} else {
			Y = X;
		}
		/* remove the flags from the fallen list */
		for (node_t *x = fallen; x != NULL; x = x->next)
			x->on_fallen = false;

		if (old_type_was_B_or_C) {
			/* check if some nodes will make the leader -> follower transition */
			list_for_each_entry_safe(node_t, y, tmp, &Y->leader, node_list) {
				if (y->type.tv != tarval_bottom && !is_con(y->type)) {
					node_t *eq_node = identity(y);

					if (eq_node != y && eq_node->part == y->part) {
						DB((dbg, LEVEL_2, "Node %+F is a follower of %+F\n",
						    y->node, eq_node->node));
						/* move to follower */
						y->is_follower = true;
						list_del(&y->node_list);
						list_add_tail(&y->node_list, &Y->follower);
						--Y->n_leaders;

						segregate_def_use_chain(y->node);
					}
				}
			}
		}
		split_by(Y, env);
	}
}

/**
 * Get the leader for a given node from its congruence class.
 *
 * @param irn  the node
 */
static ir_node *get_leader(node_t *node)
{
	partition_t *part = node->part;

	if (node->is_follower) {
		DB((dbg, LEVEL_2, "Replacing follower %+F\n", node->node));
		return get_first_node(part)->node;
	}

	if (part->n_leaders > 1) {
		ir_node *irn = node->node;
		DB((dbg, LEVEL_2, "Found congruence class for %+F\n", irn));

		ir_node *block       = get_nodes_block(irn);
		ir_node *first       = get_first_node(part)->node;
		ir_node *first_block = get_nodes_block(first);

		/* Ensure that the leader dominates the node. */
		if (block_dominates(first_block, block))
			return first;
		else
			return irn;
	}
	return node->node;
}

/**
 * Returns non-zero if a mode_T node has only one reachable output.
 */
static bool only_one_reachable_proj(ir_node *n)
{
	unsigned k = 0;
	foreach_irn_out_r(n, i, proj) {
		/* skip non-control flow Proj's */
		if (get_irn_mode(proj) != mode_X)
			continue;

		const node_t *node = get_irn_node(proj);
		if (node->type.tv == tarval_top) {
			if (++k > 1)
				return false;
		}
	}
	return true;
}

/**
 * Return non-zero if the control flow predecessor node pred
 * is the only reachable control flow exit of its block.
 *
 * @param pred   the control flow exit
 * @param block  the destination block
 */
static bool can_exchange(ir_node *pred, ir_node *block)
{
	if (get_Block_entity(block) != NULL) {
		return false;
	} else if (is_Jmp(pred)) {
		return true;
	} else if (is_Raise(pred)) {
		/* Raise is a tuple and usually has only one reachable ProjX,
		 * but it must not be eliminated like a Jmp */
		return false;
	} else if (get_irn_mode(pred) == mode_T) {
		/* if the predecessor block has more than one
		   reachable outputs we cannot remove the block */
		return only_one_reachable_proj(pred);
	}
	return false;
}

/**
 * Block Post-Walker, apply the analysis results on control flow by
 * shortening Phi's and Block inputs.
 */
static void apply_cf(ir_node *block, void *ctx)
{
	environment_t *env  = (environment_t *)ctx;
	node_t        *node = get_irn_node(block);
	int            n    = get_Block_n_cfgpreds(block);

	if (!is_reachable(node)) {
		/* Nothing to do for unreachable blocks. Their control-flow successors cut
		 * them off anyway.  The only exception is the end block. */
		ir_graph *const irg = get_irn_irg(block);
		if (block == get_irg_end_block(irg)) {
			/* Analysis found out that the end block is unreachable,
			 * hence we remove all its control flow predecessors. */
			set_irn_in(block, 0, NULL);
			env->modified = true;
		}
		return;
	}

	if (n == 1) {
		/* only one predecessor combine */
		ir_node *pred = skip_Proj(get_Block_cfgpred(block, 0));

		if (can_exchange(pred, block)) {
			ir_node *new_block = get_nodes_block(pred);
			DB((dbg, LEVEL_1, "Fuse %+F with %+F\n", block, new_block));
			DBG_OPT_COMBO(block, new_block);
			exchange(block, new_block);
			node->node = new_block;
			env->modified = true;
		}
		return;
	}

	ir_node **in_X = ALLOCAN(ir_node*, n);
	int       k    = 0;
	for (int i = 0; i < n; ++i) {
		ir_node *pred = get_Block_cfgpred(block, i);
		node_t  *node = get_irn_node(pred);

		if (node->type.tv == tarval_top) {
			in_X[k++] = pred;
		} else {
			DB((dbg, LEVEL_1, "Removing dead input %d from %+F (%+F)\n", i, block, pred));
		}
	}
	if (k >= n)
		return;

	/* fix Phi's */
	ir_graph *irg = get_irn_irg(block);
	ir_node **ins = ALLOCAN(ir_node*, n);
	for (ir_node *next, *phi = get_Block_phis(block); phi != NULL; phi = next) {
		node_t   *node = get_irn_node(phi);

		next = get_Phi_next(phi);
		if (is_tarval(node->type.tv) && tarval_is_constant(node->type.tv)) {
			/* this Phi is replaced by a constant */
			ir_tarval *tv  = node->type.tv;
			ir_node   *c   = new_r_Const(irg, tv);

			set_irn_node(c, node);
			node->node = c;
			DB((dbg, LEVEL_1, "%+F is replaced by %+F\n", phi, c));
			DBG_OPT_COMBO(phi, c);
			exchange(phi, c);
			env->modified = true;
		} else {
			int j = 0;
			for (int i = 0; i < n; ++i) {
				node_t *pred = get_irn_node(get_Block_cfgpred(block, i));

				if (pred->type.tv == tarval_top) {
					ins[j++] = get_Phi_pred(phi, i);
				}
			}
			if (j == 1) {
				/* this Phi is replaced by a single predecessor */
				ir_node *s        = ins[0];
				node_t  *phi_node = get_irn_node(phi);

				if (get_Phi_loop(phi))
					remove_keep_alive(phi);

				node->node = s;
				DB((dbg, LEVEL_1, "%+F is replaced by %+F because of cf change\n", phi, s));
				DBG_OPT_COMBO(phi, s);
				exchange(phi, s);
				phi_node->node = s;
				env->modified = true;
			} else {
				set_irn_in(phi, j, ins);
				env->modified = true;
			}
		}
	}

	/* fix block */
	if (k == 1) {
		/* this Block has only one live predecessor */
		ir_node *pred = skip_Proj(in_X[0]);

		if (can_exchange(pred, block)) {
			ir_node *new_block = get_nodes_block(pred);
			DBG_OPT_COMBO(block, new_block);
			exchange(block, new_block);
			node->node = new_block;
			env->modified = true;
			return;
		}
	}
	set_irn_in(block, k, in_X);
	env->modified = true;
}

/**
 * Exchange a node by its leader.
 * Beware: in rare cases the mode might be wrong here, for instance
 * AddP(x, NULL) is a follower of x, but with different mode.
 * Fix it here.
 */
static void exchange_leader(ir_node *irn, ir_node *leader)
{
	ir_mode *mode = get_irn_mode(irn);
	if (mode != get_irn_mode(leader)) {
		/* The conv is a no-op, so we are free to place it
		 * either in the block of the leader OR in irn's block.
		 * Probably placing it into leaders block might reduce
		 * the number of Conv due to CSE. */
		ir_node  *block = get_nodes_block(leader);
		dbg_info *dbg   = get_irn_dbg_info(irn);
		ir_node  *nlead = new_rd_Conv(dbg, block, leader, mode);

		if (nlead != leader) {
			/* Note: this newly create irn has no node info because
			 * it is created after the analysis. However, this node
			 * replaces the node irn and should not be visited again,
			 * so set its visited count to the count of irn.
			 * Otherwise we might visited this node more than once if
			 * irn had more than one user.
			 */
			set_irn_node(nlead, NULL);
			set_irn_visited(nlead, get_irn_visited(irn));
			leader = nlead;
		}
	}
	exchange(irn, leader);
}

/**
 * Check, if all users of a mode_M node are dead. Use
 * the Def-Use edges for this purpose, as they still
 * reflect the situation.
 */
static bool all_users_are_dead(const ir_node *irn)
{
	foreach_irn_out(irn, i, succ) {
		node_t const *const block = get_irn_node(get_nodes_block(succ));

		if (!is_reachable(block)) {
			/* block is unreachable */
			continue;
		}
		const node_t *node = get_irn_node(succ);
		if (node->type.tv != tarval_bottom) {
			/* found a reachable user */
			return false;
		}
	}
	/* all users are unreachable */
	return true;
}

/**
 * Walker: Find reachable mode_M nodes that have only
 * unreachable users. These nodes must be kept later.
 */
static void find_kept_memory(ir_node *irn, void *ctx)
{
	if (get_irn_mode(irn) != mode_M)
		return;

	node_t *block = get_irn_node(get_nodes_block(irn));
	if (!is_reachable(block))
		return;

	node_t *node = get_irn_node(irn);
	if (node->type.tv == tarval_bottom)
		return;

	/* ok, we found a live memory node. */
	if (all_users_are_dead(irn) && is_Phi(irn)) {
		environment_t *env = (environment_t*)ctx;
		DB((dbg, LEVEL_1, "%+F must be kept\n", irn));
		ARR_APP1(ir_node *, env->kept_memory, irn);
	}
}

/**
 * Post-Walker, apply the analysis results;
 */
static void apply_result(ir_node *irn, void *ctx)
{
	/* Blocks already handled, do not touch the End node. */
	if (is_Block(irn) || is_End(irn) || is_Bad(irn))
		return;

	environment_t *env   = (environment_t *)ctx;
	node_t        *node  = get_irn_node(irn);
	node_t        *block = get_irn_node(get_nodes_block(irn));
	assert(is_reachable(block));

	if (node->type.tv == tarval_bottom) {
		ir_mode *mode = get_irn_mode(irn);

		if (mode == mode_M) {
			/* never kill a mode_M node */
			if (is_Proj(irn)) {
				ir_node *pred  = get_Proj_pred(irn);
				node_t  *pnode = get_irn_node(pred);

				if (pnode->type.tv == tarval_bottom) {
					/* skip the predecessor */
					ir_node *mem = get_memop_mem(pred);
					node->node = mem;
					DB((dbg, LEVEL_1, "%+F computes Bottom, replaced by %+F\n", irn, mem));
					exchange(irn, mem);
					env->modified = true;
				}
			}
			/* leave other nodes, especially PhiM */
		} else if (mode == mode_T) {
			/* Do not kill mode_T nodes, kill their Projs */
		} else if (!is_Unknown(irn)) {
			/* do not kick away Unknown's, they might be still needed */
			ir_graph *irg = get_irn_irg(irn);
			ir_node  *unk = new_r_Unknown(irg, mode);

			/* control flow should already be handled at apply_cf() */
			assert(mode != mode_X);

			/* see comment above */
			set_irn_node(unk, node);
			node->node = unk;
			DB((dbg, LEVEL_1, "%+F computes Bottom\n", irn));
			exchange(irn, unk);
			env->modified = true;
		}
	} else if (get_irn_mode(irn) == mode_X) {
		if (is_Proj(irn)) {
			/* leave or Jmp */
			ir_node *cond = get_Proj_pred(irn);

			if (is_Cond(cond) || is_Switch(cond)) {
				if (only_one_reachable_proj(cond)) {
					ir_node *jmp = new_r_Jmp(block->node);
					set_irn_node(jmp, node);
					node->node = jmp;
					DB((dbg, LEVEL_1, "%+F is replaced by %+F\n", irn, jmp));
					DBG_OPT_COMBO(irn, jmp);
					exchange(irn, jmp);
					env->modified = true;
				} else if (is_Switch(cond)) {
					node_t    *sel = get_irn_node(get_Switch_selector(cond));
					ir_tarval *tv  = sel->type.tv;

					if (is_tarval(tv) && tarval_is_constant(tv)) {
						/* The selector is a constant, but more
						 * than one output is active: An unoptimized
						 * case found. */
						env->unopt_cf = true;
					}
				}
			}
		}
	/* normal data node */
	} else if (is_tarval(node->type.tv) && tarval_is_constant(node->type.tv)) {
		/*
		 * Beware: never replace mode_T nodes by constants. Currently we must mark
		 * mode_T nodes with constants, but do NOT replace them.
		 */
		if (!is_Const(irn) && get_irn_mode(irn) != mode_T) {
			/* can be replaced by a constant */
			ir_graph  *irg = get_irn_irg(irn);
			ir_tarval *tv  = node->type.tv;
			ir_node   *c   = new_r_Const(irg, tv);
			set_irn_node(c, node);
			node->node = c;
			DB((dbg, LEVEL_1, "%+F is replaced by %+F\n", irn, c));
			DBG_OPT_COMBO(irn, c);
			exchange_leader(irn, c);
			env->modified = true;
		}
	} else if (is_entity(node->type.ent)) {
		if (!is_Address(irn)) {
			/* can be replaced by an Address */
			ir_graph *irg  = get_irn_irg(irn);
			ir_node  *addr = new_r_Address(irg, node->type.ent);
			set_irn_node(addr, node);
			node->node = addr;

			DB((dbg, LEVEL_1, "%+F is replaced by %+F\n", irn, addr));
			DBG_OPT_COMBO(irn, addr);
			exchange_leader(irn, addr);
			env->modified = true;
		}
	} else if (is_Confirm(irn)) {
		/* Confirms are always follower, but do not kill them here */
	} else {
		ir_node *leader = get_leader(node);

		if (leader != irn) {
			bool non_strict_phi = false;

			/*
			 * Beware: Do not remove Phi(Unknown, ..., x, ..., Unknown)
			 * as this might create non-strict programs.
			 */
			if (node->is_follower && is_Phi(irn) && !is_Unknown(leader)) {
				for (int i = get_Phi_n_preds(irn); i-- > 0; ) {
					ir_node *pred = get_Phi_pred(irn, i);

					if (is_Unknown(pred)) {
						non_strict_phi = true;
						break;
					}
				}
			}
			if (!non_strict_phi) {
				DB((dbg, LEVEL_1, "%+F from part%d is replaced by %+F\n", irn, node->part->nr, leader));
				DBG_OPT_COMBO(irn, leader);
				exchange_leader(irn, leader);
				env->modified = true;
			}
		}
	}
}

/**
 * Fix the keep-alives by deleting unreachable ones.
 */
static void apply_end(ir_node *end, environment_t *env)
{
	int const n  = get_End_n_keepalives(end);
	if (n > 0) {
		int             j  = 0;
		ir_node **const in = ALLOCAN(ir_node*, n);
		for (int i = 0; i < n; i++) {
			ir_node *ka = get_End_keepalive(end, i);

			if (is_Bad(ka))
				continue;

			ir_node *const block = get_block(ka);
			node_t  *const node  = get_irn_node(block);
			if (is_reachable(node))
				in[j++] = ka;
		}
		if (j != n) {
			set_End_keepalives(end, j, in);
			env->modified = true;
		}
	}
}

static void set_compute_func(ir_op *op, compute_func func)
{
	op->ops.generic = (op_func)func;
}

/**
 * sets the generic functions to compute.
 */
static void set_compute_functions(void)
{
	/* set the default compute function */
	for (size_t i = 0, n = ir_get_n_opcodes(); i < n; ++i) {
		ir_op *op = ir_get_opcode(i);
		op->ops.generic = (op_func)default_compute;
	}

	/* set specific functions */
	set_compute_func(op_Add,     compute_Add);
	set_compute_func(op_Address, compute_Address);
	set_compute_func(op_Align,   compute_Align);
	set_compute_func(op_Bad,     compute_Bad);
	set_compute_func(op_Block,   compute_Block);
	set_compute_func(op_Cmp,     compute_Cmp);
	set_compute_func(op_Confirm, compute_Confirm);
	set_compute_func(op_End,     compute_End);
	set_compute_func(op_Eor,     compute_Eor);
	set_compute_func(op_Jmp,     compute_Jmp);
	set_compute_func(op_Mux,     compute_Mux);
	set_compute_func(op_Offset,  compute_Offset);
	set_compute_func(op_Phi,     compute_Phi);
	set_compute_func(op_Proj,    compute_Proj);
	set_compute_func(op_Return,  compute_Return);
	set_compute_func(op_Size,    compute_Size);
	set_compute_func(op_Sub,     compute_Sub);
	set_compute_func(op_Unknown, compute_Unknown);
}

/**
 * Add memory keeps.
 */
static void add_memory_keeps(ir_graph *irg, ir_node **kept_memory, size_t len)
{
	ir_nodeset_t set;
	ir_nodeset_init(&set);

	/* check, if those nodes are already kept */
	ir_node *end = get_irg_end(irg);
	for (int i = get_End_n_keepalives(end); i-- > 0; )
		ir_nodeset_insert(&set, get_End_keepalive(end, i));

	for (size_t idx = 0; idx < len; ++idx) {
		ir_node *ka = kept_memory[idx];
		if (!ir_nodeset_contains(&set, ka)) {
			add_End_keepalive(end, ka);
		}
	}
	ir_nodeset_destroy(&set);
}

void combo(ir_graph *irg)
{
	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_NO_TUPLES
		| IR_GRAPH_PROPERTY_CONSISTENT_OUTS
		| IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO);

	/* register a debug mask */
	FIRM_DBG_REGISTER(dbg, "firm.opt.combo");

	DB((dbg, LEVEL_1, "Doing COMBO for %+F\n", irg));

	environment_t env;
	memset(&env, 0, sizeof(env));
	obstack_init(&env.obst);
	env.opcode2id_map  = new_set(cmp_opcode, iro_last * 4);
	env.kept_memory    = NEW_ARR_F(ir_node *, 0);
	env.end_idx        = get_opt_global_cse() ? 0 : -1;
	/* options driving the optimization */
	env.commutative    = true;

	/* we have our own value_of function */
	set_value_of_func(get_node_tarval);

	set_compute_functions();
	DEBUG_ONLY(part_nr = 0;)

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	/* create the initial partition and place it on the work list */
	env.initial = new_partition(&env);
	add_to_worklist(env.initial, &env);
	irg_walk_graph(irg, create_initial_partitions, init_block_phis, &env);

	/* set the hook: from now, every node has a partition and a type */
	DEBUG_ONLY(set_dump_node_vcgattr_hook(dump_partition_hook);)

	/* all nodes on the initial partition have type Bottom */
	env.initial->type_is_B_or_C = true;

	/* Place the START Node's partition on cprop.
	   Place the START Node on its local worklist. */
	ir_node *initial_bl = get_irg_start_block(irg);
	node_t  *start      = get_irn_node(initial_bl);
	add_to_cprop(start, &env);

	do {
		propagate(&env);
		if (env.worklist != NULL)
			cause_splits(&env);
	} while (env.cprop != NULL || env.worklist != NULL);

	dump_all_partitions(&env);
	check_all_partitions(&env);

	/* apply the result */

	/* check, which nodes must be kept */
	irg_walk_graph(irg, NULL, find_kept_memory, &env);

	/* kill unreachable control flow */
	irg_block_walk_graph(irg, NULL, apply_cf, &env);
	/* Kill keep-alives of dead blocks: this speeds up apply_result()
	 * and fixes assertion because dead cf to dead blocks is NOT removed by
	 * apply_cf(). */
	ir_node *end = get_irg_end(irg);
	apply_end(end, &env);

	/* need a freshly computed dominance tree (after killing unreachable code
	 * it is not valid anymore) */
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);

	irg_walk_graph(irg, NULL, apply_result, &env);

	size_t len = ARR_LEN(env.kept_memory);
	if (len > 0)
		add_memory_keeps(irg, env.kept_memory, len);

	if (env.unopt_cf) {
		DB((dbg, LEVEL_1, "Unoptimized Control Flow left"));
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	/* remove the partition hook */
	DEBUG_ONLY(set_dump_node_vcgattr_hook(NULL);)

	DEL_ARR_F(env.kept_memory);
	del_set(env.opcode2id_map);
	obstack_free(&env.obst, NULL);

	/* restore value_of() default behavior */
	set_value_of_func(NULL);

	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
}
