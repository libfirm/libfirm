/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Combining congruent blocks
 * @author  Michael Beck
 *
 * This phase find congruent blocks.
 * Two block are congruent, if they contains only equal calculations.
 */
#include "array.h"
#include "debug.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "iroptimize.h"
#include "set.h"
#include "util.h"

/* define this for general block shaping: congruent blocks
   are found not only before the end block but anywhere in the graph */
#define GENERAL_SHAPE

typedef struct partition_t     partition_t;
typedef struct block_t         block_t;
typedef struct node_t          node_t;
typedef struct pair_t          pair_t;
typedef struct phi_t           phi_t;
typedef struct opcode_key_t    opcode_key_t;
typedef struct listmap_entry_t listmap_entry_t;
typedef struct environment_t   environment_t;
typedef struct pred_t          pred_t;

/** An opcode map key. */
struct opcode_key_t {
	unsigned    code;   /**< The Firm opcode. */
	ir_mode     *mode;  /**< The mode of all nodes in the partition. */
	int         arity;  /**< The arity of this opcode (needed for Phi etc. */
	union {
		unsigned         proj;   /**< For Proj nodes, its proj number */
		ir_entity       *ent;   /**< For Address/Offset/Sel nodes, its entity */
		ir_tarval       *tv;    /**< For Const nodes, its tarval */
		ir_type         *type;  /**< For TypeConst nodes, its type. */
		void            *addr;  /**< Alias all addresses. */
		int             intVal; /**< For Conv/Div nodes: strict/remainderless. */
	} u;
};

/** A partition contains all congruent blocks. */
struct partition_t {
	list_head part_list;     /**< Double linked list of partitions. */
	list_head blocks;        /**< List of blocks in this partition. */
	unsigned  n_blocks;      /**< Number of block in this partition. */
	ir_node   *meet_block;   /**< The control flow meet block of this partition. */
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
	ir_node    **roots;      /**< An array of all root nodes. */
	node_t     *cf_root;     /**< The control flow root node of this block. */
	pair_t     *input_pairs; /**< The list of inputs to this block. */
	phi_t      *phis;        /**< The list of Phis in this block. */
	block_t    *all_next;    /**< Links all created blocks. */
	int        meet_input;   /**< Input number of this block in the meet-block. */
};

/** A node. */
struct node_t {
	list_head  node_list;    /**< Double linked list of block inside a partition. */
	ir_node    *node;        /**< Pointer to the associated IR-node or NULL for block inputs. */
	char       is_input;     /**< Set if this node is an input from other block. */
};

/** The environment. */
struct environment_t {
	list_head       partitions;     /**< list of partitions. */
	list_head       ready;          /**< list of ready partitions. */
	set             *opcode2id_map; /**< The opcodeMode->id map. */
	ir_node         **live_outs;    /**< Live out only nodes. */
	block_t         *all_blocks;    /**< List of all created blocks. */
	struct obstack  obst;           /** obstack for temporary data */
};

/** A (node, input index) pair. */
struct pair_t {
	pair_t  *next;    /**< Points to the next pair entry. */
	ir_node *irn;     /**< The IR-node. */
	int     index;    /**< An input index. */
	ir_node **ins;    /**< A new in array once allocated. */
};

/** A Phi, inputs pair. */
struct phi_t {
	phi_t   *next;    /**< Points to the next Phi pair entry. */
	ir_node *phi;     /**< The Phi node. */
	ir_node **ins;    /**< A new in array once allocated. */
};

/** Describes a predecessor input. */
struct pred_t {
	ir_node *pred;  /**< The predecessor. */
	int     index;  /**< Its input index. */
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

#define get_Block_entry(block)  ((block_t *)get_irn_link(block))

/** The debug module handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/** Next partition number. */
DEBUG_ONLY(static unsigned part_nr = 0;)

#ifdef DEBUG_libfirm
/**
 * Dump partition to output.
 */
static void dump_partition(const char *msg, const partition_t *part)
{
	int first = 1;

	DB((dbg, LEVEL_2, " %s part%u (%u blocks) {\n  ", msg, part->nr, part->n_blocks));
	list_for_each_entry(block_t, block, &part->blocks, block_list) {
		DB((dbg, LEVEL_2, "%s%+F", first ? "" : ", ", block->block));
		first = 0;
	}
	DB((dbg, LEVEL_2, "\n }\n"));
}

/**
 * Dumps a list.
 */
static void dump_list(const char *msg, const block_t *block)
{
	const block_t *p;
	int           first = 1;

	DB((dbg, LEVEL_3, "  %s = {\n   ", msg));
	for (p = block; p != NULL; p = p->next) {
		DB((dbg, LEVEL_3, "%s%+F", first ? "" : ", ", p->block));
		first = 0;
	}
	DB((dbg, LEVEL_3, "\n  }\n"));
}
#else
#define dump_partition(msg, part) (void)(msg), (void)(part)
#define dump_list(msg, list) (void)(msg), (void)(list)
#endif

/**
 * Compare two pointer values of a listmap.
 */
static int listmap_cmp_ptr(const void *elt, const void *key, size_t size)
{
	(void)size;
	const listmap_entry_t *e1 = (const listmap_entry_t*)elt;
	const listmap_entry_t *e2 = (const listmap_entry_t*)key;
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
	listmap_entry_t key, *entry;

	key.id   = id;
	key.list = NULL;
	key.next = NULL;
	entry    = set_insert(listmap_entry_t, map->map, &key, sizeof(key), hash_ptr(id));

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
	/* assume long >= int */
	return (unsigned)(PTR_TO_INT(entry->mode) * 9 + entry->code + entry->u.proj * 3 + hash_ptr(entry->u.addr) + entry->arity);
}

/**
 * Compare two entries in the opcode map.
 */
static int cmp_opcode(const void *elt, const void *key, size_t size)
{
	(void)size;
	const opcode_key_t *o1 = (opcode_key_t*)elt;
	const opcode_key_t *o2 = (opcode_key_t*)key;
	return o1->code != o2->code || o1->mode != o2->mode ||
	       o1->arity != o2->arity ||
	       o1->u.proj != o2->u.proj || o1->u.addr != o2->u.addr;
}

/**
 * Creates a new empty partition and put in on the
 * partitions list.
 *
 * @param meet_block  the control flow meet block of this partition
 * @param env         the environment
 */
static partition_t *create_partition(ir_node *meet_block, environment_t *env)
{
	partition_t *part = OALLOC(&env->obst, partition_t);

	INIT_LIST_HEAD(&part->blocks);
	part->meet_block = meet_block;
	part->n_blocks   = 0;
	DEBUG_ONLY(part->nr = part_nr++;)
	list_add_tail(&part->part_list, &env->partitions);
	return part;
}

/**
 * Allocate a new block in the given partition.
 *
 * @param block      the IR-node
 * @param meet_input Input number of this block in the meet-block
 * @param partition  the partition to add to
 * @param env        the environment
 */
static block_t *create_block(ir_node *block, int meet_input, partition_t *partition, environment_t *env)
{
	block_t *bl = OALLOC(&env->obst, block_t);

	set_irn_link(block, bl);

	INIT_LIST_HEAD(&bl->nodes);
	bl->next        = NULL;
	bl->block       = block;
	bl->roots       = NEW_ARR_F(ir_node *, 0);
	bl->cf_root     = NULL;
	bl->input_pairs = NULL;
	bl->phis        = NULL;
	bl->meet_input  = meet_input;

	/* put it into the list of partition blocks */
	list_add_tail(&bl->block_list, &partition->blocks);
	++partition->n_blocks;

	/* put in into the list of all blocks */
	bl->all_next    = env->all_blocks;
	env->all_blocks = bl;

	return bl;
}

/**
 * Allocate a new node and add it to a blocks wait queue.
 *
 * @param irn    the IR-node
 * @param block  the block to add to
 * @param env    the environment
 */
static node_t *create_node(ir_node *irn, block_t *block, environment_t *env)
{
	node_t *node = OALLOC(&env->obst, node_t);

	node->node     = irn;
	node->is_input = 0;

	list_add_tail(&node->node_list, &block->nodes);

	return node;
}

/**
 * Add an input pair to a block.
 *
 * @param block  the block
 * @param irn    the IR-node that has an block input
 * @param idx    the index of the block input in node's predecessors
 * @param env    the environment
 */
static void add_pair(block_t *block, ir_node *irn, int idx, environment_t *env)
{
	pair_t *pair = OALLOC(&env->obst, pair_t);

	pair->next  = block->input_pairs;
	pair->irn   = irn;
	pair->index = idx;
	pair->ins   = NULL;

	block->input_pairs = pair;
}

/**
 * Add a Phi to a block.
 *
 * @param block  the block
 * @param phi    the Phi node
 * @param env    the environment
 */
static void add_phi(block_t *block, ir_node *phi, environment_t *env)
{
	phi_t *node = OALLOC(&env->obst, phi_t);

	node->next = block->phis;
	node->phi  = phi;
	node->ins  = NULL;

	block->phis = node;
}

/**
 * Creates an opcode from a node.
 */
static opcode_key_t *opcode(const node_t *node, environment_t *env)
{
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
	key.u.addr = NULL;

	switch (key.code) {
	case iro_Address:
		key.u.ent = get_Address_entity(irn);
		break;
	case iro_Align:
		key.u.type = get_Align_type(irn);
		break;
	case iro_Offset:
		key.u.ent = get_Offset_entity(irn);
		break;
	case iro_Proj:
		key.u.proj = get_Proj_num(irn);
		break;
	case iro_Sel:
		key.u.type = get_Sel_type(irn);
		break;
	case iro_Member:
		key.u.ent = get_Member_entity(irn);
		break;
	case iro_Const:
		key.u.tv  = get_Const_tarval(irn);
		break;
	case iro_Load:
		key.mode = get_Load_mode(irn);
		break;
	case iro_Div:
		key.u.intVal = get_Div_no_remainder(irn);
		break;
	case iro_Builtin:
		key.u.intVal = get_Builtin_kind(irn);
		break;
	case iro_Size:
		key.u.type = get_Size_type(irn);
		break;
	default:
		break;
	}

	entry = set_insert(opcode_key_t, env->opcode2id_map, &key, sizeof(key), opcode_hash(&key));
	return entry;
}

/**
 * Split a partition by a local list.
 *
 * @param Z    partition to split
 * @param g    a (non-empty) block list
 * @param env  the environment
 *
 * @return  a new partition containing the nodes of g
 */
static partition_t *split(partition_t *Z, block_t *g, environment_t *env)
{
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
	Z_prime = create_partition(Z->meet_block, env);
	for (block = g; block != NULL; block = block->next) {
		list_add_tail(&block->block_list, &Z_prime->blocks);
	}
	Z_prime->n_blocks = n;

	dump_partition("Now ", Z);
	dump_partition("Created new ", Z_prime);
	return Z_prime;
}

/**
 * Return non-zero if pred should be treated as a input node.
 */
static int is_input_node(ir_node *pred, ir_node *irn, int index)
{
	/* for now, do NOT turn direct calls into indirect one */
	if (index != 1)
		return 1;
	if (!is_Address(pred))
		return 1;
	if (! is_Call(irn))
		return 1;
	return 0;
}

/**
 * Propagate nodes on all wait queues of the given partition.
 *
 * @param part  the partition
 * @param env   the environment
 */
static void propagate_blocks(partition_t *part, environment_t *env)
{
	block_t         *ready_blocks = NULL;
	unsigned        n_ready       = 0;
	listmap_t       map;
	listmap_entry_t *iter;

	DB((dbg, LEVEL_2, " Propagate blocks on part%u\n", part->nr));

	/* Let map be an empty mapping from the range of Opcodes to (local) list of blocks. */
	listmap_init(&map);
	list_for_each_entry_safe(block_t, bl, next, &part->blocks, block_list) {
		opcode_key_t    *id;
		listmap_entry_t *entry;
		node_t          *node;

		if (list_empty(&bl->nodes)) {
			bl->next     = ready_blocks;
			ready_blocks = bl;
			++n_ready;
			DB((dbg, LEVEL_2, " Block %+F completely processed\n", bl->block));
			continue;
		}

		/* get the first node from the wait queue */
		node = list_entry(bl->nodes.next, node_t, node_list);
		list_del(&node->node_list);

		/* put all not-visited predecessors to the wait queue */
		if (! node->is_input) {
			ir_node *irn = node->node;

			DB((dbg, LEVEL_3, "  propagate %+F\n", irn));
			ir_normalize_node(node->node);
			foreach_irn_in_r(irn, i, pred) {
				ir_node *block = get_nodes_block(skip_Proj(pred));

				if (block != bl->block) {
					node_t *p_node = create_node(pred, bl, env);
					if (is_input_node(pred, irn, i)) {
						/* is a block live input */
						p_node->is_input = 1;
						if (! is_Phi(irn))
							add_pair(bl, irn, i, env);
					} else if (is_Phi(pred)) {
						/* update the Phi list */
						add_phi(bl, pred, env);
					}
				} else if (! irn_visited_else_mark(pred)) {
					/* not yet visited, ok */
					create_node(pred, bl, env);

					if (is_Phi(pred)) {
						/* update the Phi list */
						add_phi(bl, pred, env);
					}
				}
			}
		} else {
			DB((dbg, LEVEL_3, "  propagate Input %+F\n", node->node));
		}

		/* Add bl to map[opcode(n)]. */
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
			DB((dbg, LEVEL_2, " Partition %u is ready\n", Z->nr));
			list_add(&Z->part_list, &env->ready);
		} else {
			DB((dbg, LEVEL_2, " Partition %u contains only one block, killed\n", Z->nr));
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
}

/**
 * Propagate nodes on all wait queues.
 *
 * @param env    the environment
 */
static void propagate(environment_t *env)
{
	list_for_each_entry_safe(partition_t, part, next, &env->partitions, part_list) {
		if (part->n_blocks < 2) {
			/* zero or one block left, kill this partition */
			list_del(&part->part_list);
			DB((dbg, LEVEL_2, " Partition %u contains less than 2 blocks, killed\n", part->nr));
		} else
			propagate_blocks(part, env);
	}
}

/**
 * Map a block to the phi[block->input] live-trough.
 */
static void *live_throughs(const block_t *bl, const ir_node *phi)
{
	ir_node *input = get_Phi_pred(phi, bl->meet_input);

	/* If this input is inside our block, this
	   is a live-out and not a live trough.
	   Live-outs are tested inside propagate, so map all of
	   them to the "general" value NULL */
	if (get_nodes_block(input) == bl->block)
		return NULL;
	return input;
}

/**
 * Split partition by live-outs and live-troughs.
 *
 * @param part  the partition
 * @param env   the environment
 */
static void propagate_blocks_live_troughs(partition_t *part, environment_t *env)
{
	const ir_node   *meet_block = part->meet_block;
	listmap_t       map;
	listmap_entry_t *iter;
	const ir_node   *phi;

	DB((dbg, LEVEL_2, " Propagate live-troughs on part%u\n", part->nr));

	for (phi = get_Block_phis(meet_block); phi != NULL; phi = get_Phi_next(phi)) {
		/* propagate on all Phis of the meet-block */

		if (part->n_blocks < 2) {
			/* zero or one block left, kill this partition */
			list_del(&part->part_list);
			DB((dbg, LEVEL_2, " Partition %u contains less than 2 blocks, killed\n", part->nr));
			return;
		}

		/* Let map be an empty mapping from the range of live-troughs to (local) list of blocks. */
		listmap_init(&map);
		list_for_each_entry_safe(block_t, bl, next, &part->blocks, block_list) {
			opcode_key_t    *id;
			listmap_entry_t *entry;

			/* Add bl to map[live_trough(bl)]. */
			id          = (opcode_key_t*)live_throughs(bl, phi);
			entry       = listmap_find(&map, id);
			bl->next    = entry->list;
			entry->list = bl;
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
	}
}

/**
 * Propagate live-troughs on all partitions on the partition list.
 *
 * @param env    the environment
 */
static void propagate_live_troughs(environment_t *env)
{
	list_for_each_entry_safe(partition_t, part, next, &env->partitions, part_list) {
		propagate_blocks_live_troughs(part, env);
	}
}

/**
 * Apply analysis results by replacing all blocks of a partition
 * by one representative.
 *
 * Route all inputs from all block of the partition to the one
 * representative.
 * Enhance all existing Phis by combining them.
 * Create new Phis for all previous input nodes.
 *
 * @param part  the partition to process
 */
static void apply(ir_graph *irg, partition_t *part)
{
	block_t *repr = list_entry(part->blocks.next, block_t, block_list);
	ir_node *block, *end, *meet_block, *p, *next;
	ir_node **ins, **phi_ins;
	phi_t   *repr_phi, *phi;
	pair_t  *repr_pair, *pair;
	int     i, j, k, n, n_phis;

	list_del(&repr->block_list);

	/* prepare new in arrays for the block ... */
	block = repr->block;
	n     = get_Block_n_cfgpreds(block);
	ins   = NEW_ARR_F(ir_node *, n);

	for (i = 0; i < n; ++i) {
		ins[i] = get_Block_cfgpred(block, i);
	}

	/* ... for all existing Phis ... */
	for (repr_phi = repr->phis; repr_phi != NULL; repr_phi = repr_phi->next) {
		repr_phi->ins = NEW_ARR_F(ir_node *, n);

		for (i = 0; i < n; ++i)
			repr_phi->ins[i] = get_Phi_pred(repr_phi->phi, i);
	}

	/* ... and all newly created Phis */
	for (repr_pair = repr->input_pairs; repr_pair != NULL; repr_pair = repr_pair->next) {
		ir_node *input = get_irn_n(repr_pair->irn, repr_pair->index);

		repr_pair->ins = NEW_ARR_F(ir_node *, n);
		for (i = 0; i < n; ++i)
			repr_pair->ins[i] = input;
	}

	DB((dbg, LEVEL_1, "Replacing "));

	/* collect new in arrays */
	end = get_irg_end(irg);
	list_for_each_entry(block_t, bl, &part->blocks, block_list) {
		block = bl->block;

		DB((dbg, LEVEL_1, "%+F, ", block));

		/* first step: kill any keep-alive from this block */
		for (i = get_End_n_keepalives(end) - 1; i >= 0; --i) {
			ir_node *ka = get_End_keepalive(end, i);

			if (is_Block(ka)) {
				if (ka == block)
					remove_End_keepalive(end, ka);
			} else {
				if (get_nodes_block(ka) == block)
					remove_End_keepalive(end, ka);
			}
		}

		/* second step: update control flow */
		n = get_Block_n_cfgpreds(block);
		for (i = 0; i < n; ++i) {
			ir_node *pred = get_Block_cfgpred(block, i);
			ARR_APP1(ir_node *, ins, pred);
		}

		/* third step: update Phis */
		for (repr_phi = repr->phis, phi = bl->phis;
		     repr_phi != NULL;
		     repr_phi = repr_phi->next, phi = phi->next) {
			for (i = 0; i < n; ++i) {
				ir_node *pred = get_Phi_pred(phi->phi, i);
				ARR_APP1(ir_node *, repr_phi->ins, pred);
			}
		}

		/* fourth step: update inputs for new Phis */
		for (repr_pair = repr->input_pairs, pair = bl->input_pairs;
		     repr_pair != NULL;
		     repr_pair = repr_pair->next, pair = pair->next) {
			ir_node *input = get_irn_n(pair->irn, pair->index);

			for (i = 0; i < n; ++i)
				ARR_APP1(ir_node *, repr_pair->ins, input);
		}
	}

	DB((dbg, LEVEL_1, "by %+F\n", repr->block));

	/* rewire block input ... */
	n = ARR_LEN(ins);

	/*
	 * Some problem here. For:
	 * if (x) y = 1; else y = 2;
	 *
	 * the following code is constructed:
	 *
	 * b0: if (x) goto b1; else goto b1;
	 * b1: y = Phi(1,2)
	 *
	 * However, both predecessors of b1 are b0, making the Phi
	 * "wrong".
	 *
	 * We solve this by fixing critical edges.
	 */
	for (i = 0; i < n; ++i) {
		ir_node     *pred = ins[i];
		const ir_op *cfop;

		if (is_Bad(pred))
			continue;

		cfop = get_irn_op(skip_Proj(pred));
		if (is_op_fragile(cfop)) {
			/* ignore exception flow */
			continue;
		}
		if (is_op_forking(cfop)) {
			/* a critical edge */
			ir_node *block = new_r_Block(irg, 1, &ins[i]);
			ir_node *jmp   = new_r_Jmp(block);
			ins[i] = jmp;
		}
	}

	block = repr->block;
	set_irn_in(block, n, ins);
	DEL_ARR_F(ins);

	/* ... existing Phis ... */
	for (repr_phi = repr->phis; repr_phi != NULL; repr_phi = repr_phi->next) {
		set_irn_in(repr_phi->phi, n, repr_phi->ins);
		DEL_ARR_F(repr_phi->ins);
	}

	/* ... and all inputs by creating new Phis ... */
	for (repr_pair = repr->input_pairs; repr_pair != NULL; repr_pair = repr_pair->next) {
		ir_node *input = get_irn_n(repr_pair->irn, repr_pair->index);
		ir_mode *mode  = get_irn_mode(input);
		ir_node *phi   = new_r_Phi(block, n, repr_pair->ins, mode);

		set_irn_n(repr_pair->irn, repr_pair->index, phi);
		DEL_ARR_F(repr_pair->ins);

		/* might be optimized away */
		if (is_Phi(phi))
			add_Block_phi(block, phi);
	}

	/* ... finally rewire the meet block and fix its Phi-nodes */
	meet_block = part->meet_block;
	n         = get_Block_n_cfgpreds(meet_block);

	ins = NEW_ARR_F(ir_node *, n);

	n_phis = 0;
	for (p = get_Block_phis(meet_block); p != NULL; p = get_Phi_next(p)) {
		++n_phis;
	}

	phi_ins = NEW_ARR_F(ir_node *, n_phis * n);

	for (i = j = 0; i < n; ++i) {
		ir_node *pred = get_Block_cfgpred(meet_block, i);

		list_for_each_entry(block_t, bl, &part->blocks, block_list) {
			if (bl->cf_root->node == pred)
				goto continue_outer;
		}
		ins[j] = pred;

		for (k = 0, p = get_Block_phis(meet_block); p != NULL; p = get_Phi_next(p), ++k) {
			phi_ins[k * n + j] = get_Phi_pred(p, i);
		}
		++j;

continue_outer:
		;
	}

	/* fix phis */
	if (j == 1) {
		for (k = 0, p = get_Block_phis(meet_block); p != NULL; p = next, ++k) {
			next = get_Phi_next(p);

			exchange(p, phi_ins[k * n]);
		}
		/* all Phis killed */
		set_Block_phis(meet_block, NULL);
	} else {
		for (k = 0, p = get_Block_phis(meet_block); p != NULL; p = next, ++k) {
			next = get_Phi_next(p);

			set_irn_in(p, j, &phi_ins[k * n]);
		}
	}
	DEL_ARR_F(phi_ins);

	/* fix inputs of the meet block */
	set_irn_in(meet_block, j, ins);
	DEL_ARR_F(ins);
}

/**
 * Create a partition for a the end block.
 *
 * @param end_block  the end block
 * @param env        the environment
 */
static void partition_for_end_block(ir_node *end_block, environment_t *env)
{
	partition_t *part = create_partition(end_block, env);
	ir_node     *end;
	int         i;

	/* collect normal blocks */
	for (i = get_Block_n_cfgpreds(end_block) - 1; i >= 0; --i) {
		ir_node *pred = get_Block_cfgpred(end_block, i);
		ir_node *block;
		block_t *bl;
		node_t  *node;

		mark_irn_visited(pred);

		block = get_nodes_block(pred);
		bl    = create_block(block, i, part, env);
		node  = create_node(pred, bl, env);

		bl->cf_root = node;
	}

	/* collect all no-return blocks */
	end = get_irg_end(get_irn_irg(end_block));
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
		bl    = create_block(block, -1, part, env);
		node  = create_node(ka, bl, env);

		bl->cf_root = node;
	}

	dump_partition("Created", part);
}

#ifdef GENERAL_SHAPE
/**
 * Create a partition for a given meet block.
 *
 * @param block    the meet block
 * @param preds    array of candidate predecessors
 * @param n_preds  number of elements in preds
 * @param env      the environment
 */
static void partition_for_block(ir_node *block, pred_t preds[], int n_preds, environment_t *env)
{
	partition_t *part = create_partition(block, env);
	int         i;

	for (i = n_preds - 1; i >= 0; --i) {
		ir_node *pred = preds[i].pred;
		ir_node *block;
		block_t *bl;
		node_t  *node;

		mark_irn_visited(pred);

		block = get_nodes_block(pred);
		bl    = create_block(block, preds[i].index, part, env);
		node  = create_node(pred, bl, env);

		bl->cf_root = node;
	}

	dump_partition("Created", part);
}

/**
 * Walker: clear the links of all block phi lists and normal
 * links.
 */
static void clear_phi_links(ir_node *irn, void *env)
{
	(void)env;
	if (is_Block(irn)) {
		set_Block_phis(irn, NULL);
		set_irn_link(irn, NULL);
	}
}

/**
 * Walker, detect live-out nodes.
 */
static void find_liveouts(ir_node *irn, void *ctx)
{
	environment_t *env        = (environment_t*)ctx;
	ir_node       **live_outs = env->live_outs;
	ir_node       *this_block;

	if (is_Block(irn))
		return;

	/* ignore Keep-alives */
	if (is_End(irn))
		return;

	this_block = get_nodes_block(irn);

	if (is_Phi(irn)) {
		/* update the Phi list */
		add_Block_phi(this_block, irn);
	}

	foreach_irn_in_r(irn, i, pred) {
		int const idx = get_irn_idx(pred);
		if (live_outs[idx] != NULL) {
			/* already marked as live-out */
			return;
		}

		ir_node *const pred_block = get_nodes_block(pred);
		/* Phi nodes always refer to live-outs */
		if (is_Phi(irn) || this_block != pred_block) {
			/* pred is a live-out */
			live_outs[idx] = pred_block;
		}
	}
}

/**
 * Check if the current block is the meet block of its predecessors.
 */
static void check_for_cf_meet(ir_node *block, void *ctx)
{
	environment_t *env = (environment_t*)ctx;
	int           i, k, n;

	if (block == get_irg_end_block(get_irn_irg(block))) {
		/* always create a partition for the end block */
		partition_for_end_block(block, env);
		return;
	}

	n = get_Block_n_cfgpreds(block);
	if (n <= 1) {
		/* Must have at least two predecessors */
		return;
	}

	pred_t *preds = ALLOCAN(pred_t, n);
	k = 0;
	for (i = n - 1; i >= 0; --i) {
		ir_node *pred = get_Block_cfgpred(block, i);

		/* pred must be a direct jump to us */
		if (! is_Jmp(pred) && ! is_Raise(pred) && !is_Bad(pred))
			continue;

		preds[k].pred  = pred;
		preds[k].index = i;
		++k;
	}

	if (k > 1)
		partition_for_block(block, preds, k, env);
}

/**
 * Compare two nodes for root ordering.
 */
static int cmp_nodes(const void *a, const void *b)
{
	const ir_node *const *pa = (const ir_node*const*)a;
	const ir_node *const *pb = (const ir_node*const*)b;
	const ir_node  *irn_a  = *pa;
	const ir_node  *irn_b  = *pb;
	unsigned       code_a  = get_irn_opcode(irn_a);
	unsigned       code_b  = get_irn_opcode(irn_b);
	ir_mode        *mode_a, *mode_b;
	unsigned       idx_a, idx_b;

	/* try opcode first */
	if (code_a != code_b)
		return code_a - code_b;

	/* try mode */
	mode_a = get_irn_mode(irn_a);
	mode_b = get_irn_mode(irn_b);

	if (mode_a != mode_b)
		return mode_a < mode_b ? -1 : +1;

	/* last resort: index */
	idx_a = get_irn_idx(irn_a);
	idx_b = get_irn_idx(irn_b);

	return (idx_a > idx_b) - (idx_a < idx_b);
}

/**
 * Add the roots to all blocks.
 */
static void add_roots(ir_graph *irg, environment_t *env)
{
	unsigned idx, n      = get_irg_last_idx(irg);
	ir_node  **live_outs = env->live_outs;
	block_t  *bl;

	for (idx = 0; idx < n; ++idx) {
		ir_node *block = live_outs[idx];

		if (block != NULL && is_Block(block)) {
			block_t *bl = get_Block_entry(block);

			if (bl != NULL) {
				ir_node *irn = get_idx_irn(irg, idx);

				if (!irn_visited_else_mark(irn)) {
					ARR_APP1(ir_node *, bl->roots, irn);
				}
			}
		}
	}
	/*
	 * Now sort the roots to normalize them as good as possible.
	 * Else, we will split identical blocks if we start with different roots.
	 */
	for (bl = env->all_blocks; bl != NULL; bl = bl->all_next) {
		/* TODO: is this really needed? The roots are already in
		   idx-order by construction, which might be good enough. */
		QSORT_ARR(bl->roots, cmp_nodes);

		DB((dbg, LEVEL_2, " Adding Roots for block %+F\n  ", bl->block));
		/* ok, add them sorted */
		for (size_t i = 0, n = ARR_LEN(bl->roots); i < n; ++i) {
			DB((dbg, LEVEL_2, "%+F, ", bl->roots[i]));
			create_node(bl->roots[i], bl, env);
		}
		DB((dbg, LEVEL_2, "\n"));
		DEL_ARR_F(bl->roots);
		bl->roots = NULL;
	}
}
#endif /* GENERAL_SHAPE */

/* Combines congruent end blocks into one. */
void shape_blocks(ir_graph *irg)
{
	environment_t env;
	block_t       *bl;
	int           res, n;

	/* register a debug mask */
	FIRM_DBG_REGISTER(dbg, "firm.opt.blocks");

	DEBUG_ONLY(part_nr = 0;)
	DB((dbg, LEVEL_1, "Shaping blocks for %+F\n", irg));

	/* works better, when returns are placed at the end of the blocks */
	normalize_n_returns(irg);

	obstack_init(&env.obst);
	INIT_LIST_HEAD(&env.partitions);
	INIT_LIST_HEAD(&env.ready);
	env.opcode2id_map  = new_set(cmp_opcode, iro_last * 4);

	n             = get_irg_last_idx(irg);
	env.live_outs = NEW_ARR_FZ(ir_node*, n);

	env.all_blocks = NULL;

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

#ifdef GENERAL_SHAPE
	/*
	 * Detect, which nodes are live-out only: these are the roots of our blocks.
	 * Build phi lists.
	 */
	irg_walk_graph(irg, clear_phi_links, find_liveouts, &env);
#endif

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);

	inc_irg_visited(irg);
#ifdef GENERAL_SHAPE
	/*
	 * Detect all control flow meets and create partitions.
	 */
	irg_block_walk_graph(irg, NULL, check_for_cf_meet, &env);

	/* add root nodes to the partition blocks */
	add_roots(irg, &env);
#else
	partition_for_end_block(get_irg_end_block(irg), &env);
#endif

	propagate_live_troughs(&env);
	while (! list_empty(&env.partitions))
		propagate(&env);

	res = !list_empty(&env.ready);
	//if (res) dump_ir_block_graph(irg, "-before");


	list_for_each_entry(partition_t, part, &env.ready, part_list) {
		dump_partition("Ready Partition", part);
		apply(irg, part);
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED | IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	if (res) {
		/* control flow changed */
		clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE);
	}

	for (bl = env.all_blocks; bl != NULL; bl = bl->all_next) {
		if (bl->roots) {
			DEL_ARR_F(bl->roots);
		}
	}

	DEL_ARR_F(env.live_outs);
	del_set(env.opcode2id_map);
	obstack_free(&env.obst, NULL);
}
