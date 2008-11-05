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
#include "ircons.h"
#include "iroptimize.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "iropt_t.h"
#include "trouts.h"
#include "set.h"
#include "debug.h"

typedef struct partition_t     partition_t;
typedef struct block_t         block_t;
typedef struct node_t          node_t;
typedef struct pair_t          pair_t;
typedef struct phi_t           phi_t;
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
	node_t     *roots;       /**< The list of all root nodes. */
	pair_t     *input_pairs; /**< The list of inputs to this block. */
	phi_t      *phis;        /**< The list of Phis in this block. */
};

/** A node. */
struct node_t {
	list_head  node_list;    /**< Double linked list of block inside a partition. */
	ir_node    *node;        /**< Pointer to the associated IR-node or NULL for block inputs. */
	node_t     *next;        /**< Link to the next node in the root set. */
	char       is_input;     /**< Set if this node is an input from other block. */
};

/** The environment. */
struct environment_t {
	list_head       partitions;     /**< list of partitions. */
	list_head       ready;          /**< list of ready partitions. */
	set             *opcode2id_map; /**< The opcodeMode->id map. */
	struct obstack  obst;           /** obstack for temporary data */
};

/** A node, input index pair. */
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

	DB((dbg, LEVEL_2, " %s part%u (%u blocks) {\n  ", msg, part->nr, part->n_blocks));
	list_for_each_entry(block_t, block, &part->blocks, block_list) {
		DB((dbg, LEVEL_2, "%s%+F", first ? "" : ", ", block->block));
		first = 0;
	}
	DB((dbg, LEVEL_2, "\n }\n"));
}  /* dump_partition */

/**
 * Dumps a list.
 */
static void dump_list(const char *msg, const block_t *block) {
	const block_t *p;
	int           first = 1;

	DB((dbg, LEVEL_3, "  %s = {\n   ", msg));
	for (p = block; p != NULL; p = p->next) {
		DB((dbg, LEVEL_3, "%s%+F", first ? "" : ", ", p->block));
		first = 0;
	}
	DB((dbg, LEVEL_3, "\n  }\n"));
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
	bl->next        = NULL;
	bl->block       = block;
	bl->roots       = NULL;
	bl->input_pairs = NULL;
	bl->phis        = NULL;
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
	node->next     = NULL;
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

/**
 * Add an input pair to a block.
 *
 * @param block  the block
 * @param irn    the IR-node that has an block input
 * @param idx    the index of the block input in node's predecessors
 * @param env    the environment
 */
static void add_pair(block_t *block, ir_node *irn, int idx, environment_t *env) {
	pair_t *pair = obstack_alloc(&env->obst, sizeof(*pair));

	pair->next  = block->input_pairs;
	pair->irn   = irn;
	pair->index = idx;
	pair->ins   = NULL;

	block->input_pairs = pair;
}  /* add_pair */

/**
 * Add a Phi to a block.
 *
 * @param block  the block
 * @param phi    the Phi node
 * @param env    the environment
 */
static void add_phi(block_t *block, ir_node *phi, environment_t *env) {
	phi_t *node = obstack_alloc(&env->obst, sizeof(*node));

	node->next = block->phis;
	node->phi  = phi;
	node->ins  = NULL;

	block->phis = node;
}  /** add_phi */

/**
 * Creates an opcode from a node.
 */
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

	DB((dbg, LEVEL_2, " Propagate blocks on part%u\n", part->nr));

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
			DB((dbg, LEVEL_2, " Block %+F completely processed\n", bl->block));
			continue;
		}

		/* get the first node from the wait queue */
		node = list_entry(bl->nodes.next, node_t, node_list);
		list_del(&node->node_list);

		/* put all not-visited predecessors to the wait queue */
		if (! node->is_input) {
			ir_node *irn = node->node;
			int     i;

			DB((dbg, LEVEL_3, "  propagate %+F\n", irn));
			ir_normalize_node(node->node);
			for (i = get_irn_arity(irn) - 1; i >= 0; --i) {
				ir_node *pred  = get_irn_n(irn, i);
				ir_node *block = get_nodes_block(skip_Proj(pred));
				node_t *p_node;

				if (block != bl->block) {
					p_node = create_node(pred, env);
					add_node(bl, p_node);
					/* do not threat Constants like live-ins */
					if (! is_irn_constlike(irn)) {
						p_node->is_input = 1;
						if (! is_Phi(irn))
							add_pair(bl, irn, i, env);
					}
				} else if (! irn_visited_else_mark(pred)) {
					/* not yet visited, ok */
					p_node = create_node(pred, env);
					add_node(bl, p_node);

					if (is_Phi(pred)) {
						/* update the Phi list */
						add_phi(bl, pred, env);
					}
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
}  /* propagate_blocks */

/**
 * Propagate nodes on all wait queues.
 *
 * @param env    the environment
 */
void propagate(environment_t *env) {
	partition_t *part, *next;

	list_for_each_entry_safe(partition_t, part, next, &env->partitions, part_list) {
		if (part->n_blocks < 2) {
			/* zero or one block left, kill this partition */
			list_del(&part->part_list);
			DB((dbg, LEVEL_2, " Partition %u contains less than 2 blocks, killed\n", part->nr));
		} else
			propagate_blocks(part, env);
	}
}  /* propagate */

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
static void apply(ir_graph *irg, partition_t *part) {
	block_t *repr = list_entry(part->blocks.next, block_t, block_list);
	block_t *bl;
	ir_node *block, *end, *end_block;
	ir_node **ins;
	phi_t   *repr_phi, *phi;
	pair_t  *repr_pair, *pair;
	int     i, j, n, block_nr;

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
	block_nr = 0;
	list_for_each_entry(block_t, bl, &part->blocks, block_list) {
		block = bl->block;
		++block_nr;

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
			ir_node *jmp   = new_r_Jmp(irg, block);
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
		ir_node *phi   = new_r_Phi(current_ir_graph, block, n, repr_pair->ins, mode);

		set_irn_n(repr_pair->irn, repr_pair->index, phi);
		DEL_ARR_F(repr_pair->ins);
	}

	/* ... finally rewire the end block */
	end_block = get_irg_end_block(irg);
	n         = get_Block_n_cfgpreds(end_block);

	ins = NEW_ARR_F(ir_node *, n);

	for (i = j = 0; i < n; ++i) {
		ir_node *out = get_Block_cfgpred(end_block, i);

		list_for_each_entry(block_t, bl, &part->blocks, block_list) {
			node_t *root;

			for (root = bl->roots; root != NULL; root = root->next) {
				if (root->node == out)
					goto found;
			}
		}
		ins[j++] = out;
found:
		;
	}

	set_irn_in(end_block, j, ins);
	DEL_ARR_F(ins);

	/* control flow changed */
	set_irg_outs_inconsistent(irg);
	set_irg_extblk_inconsistent(irg);
	set_irg_doms_inconsistent(irg);
	/* Hmm, only the root loop is inconsistent */
	set_irg_loopinfo_inconsistent(irg);

	/* Calls might be removed. */
	set_trouts_inconsistent();
}  /* apply */

/* Combines congruent end blocks into one. */
int melt_end_blocks(ir_graph *irg) {
	ir_graph      *rem;
	ir_node       *end;
	environment_t env;
	partition_t   *part;
	int           i, res;

	rem = current_ir_graph;
	current_ir_graph = irg;

	/* register a debug mask */
	FIRM_DBG_REGISTER(dbg, "firm.opt.blocks");

	DEBUG_ONLY(part_nr = 0);
	DB((dbg, LEVEL_1, "Shaping blocks for %+F\n", irg));

	/* works better, when returns are placed at the end of the blocks */
	normalize_n_returns(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
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

		node->next = bl->roots;
		bl->roots  = node;
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

		node->next = bl->roots;
		bl->roots  = node;
	}
	dump_partition("Created", part);

	while (! list_empty(&env.partitions))
		propagate(&env);

	res = !list_empty(&env.ready);

	list_for_each_entry(partition_t, part, &env.ready, part_list) {
		dump_partition("Ready Partition", part);
		apply(irg, part);
	}
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
	del_set(env.opcode2id_map);
	obstack_free(&env.obst, NULL);
	current_ir_graph = rem;

	return res;
}  /* melt_end_blocks */
