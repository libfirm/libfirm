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
 * @brief   Combining congruent blocks
 * @author  Michael Beck
 * @version $Id$
 *
 * This phase find congruent blocks. Works currently for
 * predecessors of the end block only.
 * Two block are congruent, if they contains only equal calculations.
 */
#include "config.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "set.h"
#include "debug.h"

typedef struct partition_t     partition_t;
typedef struct block_t         block_t;
typedef struct node_t          node_t;
typedef struct opcode_key_t    opcode_key_t;
typedef struct listmap_entry_t listmap_entry_t;
typedef struct environment_t   environment_t;

/** An opcode map key. */
struct opcode_key_t {
	ir_opcode   code;   /**< The Firm opcode. */
	ir_mode     *mode;  /**< The mode of all nodes in the partition. */
	int         arity;  /**< The arity of this opcode (needed for Phi etc. */
	union {
		long      proj;   /**< For Proj nodes, its proj number */
		ir_entity *ent;   /**< For Sel Nodes, its entity */
	} u;
};

/** A partition contains all congruent blocks. */
struct partition_t {
	list_head part_list;     /**< Double linked list of partitions. */
	list_head blocks;        /**< List of blocks in this partition. */
	unsigned  n_blocks;      /**< Number of block in this partition. */
#ifdef DEBUG_libfirm
	unsigned  nr;            /**< For debugging: number of this partition. */
#endif
};

/** A block. */
struct block_t {
	list_head  block_list;   /**< Double linked list of block inside a partition. */
	list_head  nodes;        /**< Wait-queue of nodes that must be checked for congruence. */
	block_t    *next;        /**< Next block of a split list. */
	ir_node    *block;       /**< Pointer to the associated IR-node block. */
	node_t     **roots;      /**< The array of the root nodes. */
};

/** A node. */
struct node_t {
	list_head  node_list;    /**< Double linked list of block inside a partition. */
	ir_node    *node;        /**< Pointer to the associated IR-node or NULL for block inputs. */
	char       is_input;     /**< Set if this node is an input from other block. */
};

/** The environment. */
typedef struct environment_t {
	list_head       partitions;     /**< list of partitions. */
	list_head       ready;          /**< list of ready partitions. */
	set             *opcode2id_map; /**< The opcodeMode->id map. */
	struct obstack  obst;           /** obstack for temporary data */
};

/**
 * An entry in the list_map.
 */
struct listmap_entry_t {
	void            *id;    /**< The id. */
	block_t         *list;  /**< The associated list for this id. */
	listmap_entry_t *next;  /**< Link to the next entry in the map. */
};

/** We must map id's to lists. */
typedef struct listmap_t {
	set             *map;    /**< Map id's to listmap_entry_t's */
	listmap_entry_t *values; /**< List of all values in the map. */
} listmap_t;

#define get_irn_node(irn)         ((node_t *)get_irn_link(irn))
#define set_irn_node(irn, node)   set_irn_link(irn, node)
#define get_irn_block(irn)        ((block_t *)get_irn_link(irn))
#define set_irn_block(irn, block) set_irn_link(irn, block)

/** The debug module handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/** Next partition number. */
DEBUG_ONLY(static unsigned part_nr = 0);

#ifdef DEBUG_libfirm
/**
 * Dump partition to output.
 */
static void dump_partition(const char *msg, const partition_t *part) {
	const block_t *block;
	int           first = 1;

	DB((dbg, LEVEL_2, "%s part%u (%u) {\n  ", msg, part->nr, part->n_blocks));
	list_for_each_entry(block_t, block, &part->blocks, block_list) {
		DB((dbg, LEVEL_2, "%s%+F", first ? "" : ", ", block->block));
		first = 0;
	}
	DB((dbg, LEVEL_2, "\n}\n"));
}  /* dump_partition */

/**
 * Dumps a list.
 */
static void dump_list(const char *msg, const block_t *block) {
	const block_t *p;
	int           first = 1;

	DB((dbg, LEVEL_3, "%s = {\n  ", msg));
	for (p = block; p != NULL; p = p->next) {
		DB((dbg, LEVEL_3, "%s%+F", first ? "" : ", ", p->block));
		first = 0;
	}
	DB((dbg, LEVEL_3, "\n}\n"));
}  /* do_dump_list */
#else
#define dump_partition(msg, part)
#define dump_list(msg, block)
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
 * @return the associated listmap entry for the given id
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
	return (entry->mode - (ir_mode *)0) * 9 + entry->code + entry->u.proj * 3 + HASH_PTR(entry->u.ent) + entry->arity;
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
 * Creates a new empty partition.
 */
static partition_t *create_partition(environment_t *env) {
	partition_t *part = obstack_alloc(&env->obst, sizeof(*part));

	INIT_LIST_HEAD(&part->blocks);
	part->n_blocks = 0;
	DEBUG_ONLY(part->nr = part_nr++);
	list_add_tail(&part->part_list, &env->partitions);
	return part;
}  /* create_partition */

/**
 * Allocate a new block.
 *
 * @param block  the IR-node
 * @param env    the environment
 */
static block_t *create_block(ir_node *block, environment_t *env) {
	block_t *bl = obstack_alloc(&env->obst, sizeof(*bl));

	INIT_LIST_HEAD(&bl->nodes);
	bl->next  = NULL;
	bl->block = block;
	bl->roots = NULL;
	set_irn_block(block, bl);
	return bl;
}  /* create_block */

/**
 * Adds a block to a partition.
 *
 * @param partition  the partition to add to
 * @param block      the block to add
 */
static void add_block(partition_t *partition, block_t *block) {
	list_add_tail(&block->block_list, &partition->blocks);
	++partition->n_blocks;
}  /* add_block */

/**
 * Allocate a new node.
 *
 * @param irn    the IR-node
 * @param env    the environment
 */
static node_t *create_node(ir_node *irn, environment_t *env) {
	node_t *node = obstack_alloc(&env->obst, sizeof(*node));

	node->node     = irn;
	node->is_input = 0;
	return node;
}  /* create_node */

/**
 * Adds a node to a block wait queue.
 *
 * @param block  the block to add to
 * @param node   the node to add
 */
static void add_node(block_t *block, node_t *node) {
	list_add_tail(&node->node_list, &block->nodes);
}  /* add_node */

/** creates an opcode */
static opcode_key_t *opcode(const node_t *node, environment_t *env) {
	opcode_key_t key, *entry;
	ir_node      *irn = node->node;

	if (node->is_input) {
		/* Node: as Block nodes are never propagated, it is safe to
		   use its code for "input" node */
		key.code   = iro_Block;
		key.arity  = 0;
	} else {
		key.code   = get_irn_opcode(irn);
		key.arity  = get_irn_arity(irn);
	}
	key.mode   = get_irn_mode(node->node);
	key.u.proj = 0;
	key.u.ent  = NULL;

	switch (key.code) {
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
}  /* opcode */

/**
 * Split a partition by a local list.
 *
 * @param Z    partition to split
 * @param g    a (non-empty) block list
 * @param env  the environment
 *
 * @return  a new partition containing the nodes of g
 */
static partition_t *split(partition_t *Z, block_t *g, environment_t *env) {
	partition_t *Z_prime;
	block_t     *block;
	unsigned    n = 0;

	dump_partition("Splitting ", Z);
	dump_list("by list ", g);

	assert(g != NULL);

	/* Remove g from Z. */
	for (block = g; block != NULL; block = block->next) {
		list_del(&block->block_list);
		++n;
	}
	assert(n < Z->n_blocks);
	Z->n_blocks -= n;

	/* Move g to a new partition, Z'. */
	Z_prime = create_partition(env);
	for (block = g; block != NULL; block = block->next) {
		list_add_tail(&block->block_list, &Z_prime->blocks);
	}
	Z_prime->n_blocks = n;

	dump_partition("Now ", Z);
	dump_partition("Created new ", Z_prime);
	return Z_prime;
}  /* split */

/**
 * Propagate nodes on all wait queues of the given partition.
 *
 * @param part  the partition
 * @param env    the environment
 */
void propagate_blocks(partition_t *part, environment_t *env) {
	block_t         *ready_blocks = NULL;
	unsigned        n_ready       = 0;
	block_t         *bl, *next;
	listmap_t       map;
	listmap_entry_t *iter;

	DB((dbg, LEVEL_2, "Propagate blocks on part%u\n", part->nr));

	/* Let map be an empty mapping from the range of Opcodes to (local) list of Nodes. */
	listmap_init(&map);
	list_for_each_entry_safe(block_t, bl, next, &part->blocks, block_list) {
		opcode_key_t    *id;
		listmap_entry_t *entry;
		node_t          *node;

		if (list_empty(&bl->nodes)) {
			bl->next     = ready_blocks;
			ready_blocks = bl;
			++n_ready;
			DB((dbg, LEVEL_1, "Block %+F ready\n", bl->block));
			continue;
		}

		/* get the first node from the wait queue */
		node = list_entry(bl->nodes.next, node_t, node_list);
		list_del(&node->node_list);

		/* put all not-visited predecessors to the wait queue */
		if (! node->is_input) {
			ir_node *irn = node->node;
			int     i;

			DB((dbg, LEVEL_3, "  propagate %+F\n", node->node));
			ir_normalize_node(node->node);
			for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
				ir_node *pred  = get_irn_n(irn, i);
				ir_node *block = get_nodes_block(skip_Proj(pred));
				node_t *p_node;

				if (block != bl->block) {
					p_node = create_node(pred, env);
					p_node->is_input = 1;
					add_node(bl, p_node);
				} else if (! irn_visited_else_mark(pred)) {
					/* not yet visited, ok */
					p_node = create_node(pred, env);
					set_irn_node(pred, p_node);
					add_node(bl, p_node);
				}
			}
		} else {
			DB((dbg, LEVEL_3, "  propagate Input %+F\n", node->node));
		}

		/* Add bl to map[opcode(bl)]. */
		id          = opcode(node, env);
		entry       = listmap_find(&map, id);
		bl->next    = entry->list;
		entry->list = bl;
	}

	/* split out ready blocks */
	if (n_ready > 0) {
		partition_t *Z;

		if (n_ready < part->n_blocks)
			Z = split(part, ready_blocks, env);
		else
			Z = part;
		list_del(&Z->part_list);

		if (Z->n_blocks > 1) {
			DB((dbg, LEVEL_1, "Partition %u is ready\n", Z->nr));
			list_add(&Z->part_list, &env->ready);
		} else {
			DB((dbg, LEVEL_1, "Partition %u contains only one block, killed\n", Z->nr));
		}
	}

	/* for all sets S except one in the range of map do */
	for (iter = map.values; iter != NULL; iter = iter->next) {
		block_t *S;

		if (iter->next == NULL) {
			/* this is the last entry, ignore */
			break;
		}
		S = iter->list;

		/* Add SPLIT( X, S ) to P. */
		split(part, S, env);
	}
	listmap_term(&map);
}  /* propagate_blocks */

/**
 * Propagate nodes on all wait queues.
 *
 * @param env    the environment
 */
void propagate(environment_t *env) {
	partition_t *part, *next;

	list_for_each_entry_safe(partition_t, part, next, &env->partitions, part_list) {
		if (part->n_blocks == 1) {
			/* only one block left, move to ready */
			list_del(&part->part_list);
			DB((dbg, LEVEL_1, "Partition %u contains only one block, killed\n", part->nr));
		} else
			propagate_blocks(part, env);
	}
}  /* propagate */

void melt_end_blocks(ir_graph *irg) {
	ir_graph      *rem;
	ir_node       *end;
	environment_t env;
	partition_t   *part;
	int           i;

	rem = current_ir_graph;
	current_ir_graph = irg;

	/* register a debug mask */
	FIRM_DBG_REGISTER(dbg, "firm.opt.blocks");
	firm_dbg_set_mask(dbg, SET_LEVEL_3);

	DB((dbg, LEVEL_1, "Melting end blocks for %+F\n", irg));

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);

	obstack_init(&env.obst);
	INIT_LIST_HEAD(&env.partitions);
	INIT_LIST_HEAD(&env.ready);
	env.opcode2id_map  = new_set(cmp_opcode, iro_Last * 4);

	part = create_partition(&env);

	/* collect all no-return blocks */
	end = get_irg_end(irg);
	for (i = get_End_n_keepalives(end) - 1; i >= 0; --i) {
		ir_node *ka    = get_End_keepalive(end, i);
		ir_node *block;
		block_t *bl;
		node_t  *node;

		if (! is_Call(ka))
			continue;
		mark_irn_visited(ka);

		/* found one */
		block = get_nodes_block(ka);
		bl    = create_block(block, &env);
		add_block(part, bl);
		node  = create_node(ka, &env);
		add_node(bl, node);

		/* currently we support only one root */
		bl->roots = NEW_ARR_D(node_t*, &env.obst, 1);
		bl->roots[0] = node;
	}

	/* collect normal blocks */
	end = get_irg_end_block(irg);
	for (i = get_Block_n_cfgpreds(end) - 1; i >= 0; --i) {
		ir_node *pred = get_Block_cfgpred(end, i);
		ir_node *block;
		block_t *bl;
		node_t  *node;

		mark_irn_visited(pred);

		block = get_nodes_block(pred);
		bl    = create_block(block, &env);
		add_block(part, bl);
		node  = create_node(pred, &env);
		add_node(bl, node);

		/* currently we support only one root */
		bl->roots = NEW_ARR_D(node_t*, &env.obst, 1);
		bl->roots[0] = node;
	}

	while (! list_empty(&env.partitions))
		propagate(&env);

	list_for_each_entry(partition_t, part, &env.ready, part_list) {
		dump_partition("Ready Partition", part);
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_IRN_VISITED);
	del_set(env.opcode2id_map);
	obstack_free(&env.obst, NULL);
	current_ir_graph = rem;
}  /* melt_end_blocks */
