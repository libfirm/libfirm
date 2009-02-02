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
 * @brief   Dataflow driven Load/Store optimizations, uses some ideas from
 *          VanDrunen's LEPRE
 * @author  Michael Beck
 * @version $Id$
 */
#include "config.h"

#include "irnode_t.h"
#include "irflag_t.h"
#include "array_t.h"
#include "ircons.h"
#include "irdom.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irouts.h"
#include "irgraph.h"
#include "irgopt.h"
#include "irnodemap.h"
#include "raw_bitset.h"
#include "debug.h"
#include "error.h"

/**
 * Mapping an address to an densed ID.
 */
typedef struct address_entry_t {
	unsigned id;          /**< The ID */
} address_entry;

/**
 * Memop-flags.
 */
enum memop_flags {
	FLAG_KILL_LOADS  =  1, /**< KILL all Loads */
	FLAG_KILL_STORES =  2, /**< KILL all Stores */
	FLAG_KILLED_NODE =  4, /**< this node was killed */
	FLAG_EXCEPTION   =  8, /**< this node has exception flow */
	FLAG_IGNORE      = 16, /**< ignore this node (volatile or other) */
	/** this memop KILLS all addresses */
	FLAG_KILL_ALL    = FLAG_KILL_LOADS|FLAG_KILL_STORES
};

/**
 * A value: This represents a value stored at a given address in
 * memory. Do not confuse with values from value numbering.
 */
typedef struct value_t value_t;
struct value_t {
	ir_node  *address;    /**< the address of this value */
	ir_node  *value;      /**< the value itself */
	ir_mode  *mode;       /**< the mode of the value */
	unsigned id;          /**< address id */
};

/**
 * A memop describes an memory-related operation.
 * These are Loads/Store and all other ops that might modify
 * memory (Calls, CopyB) or causing exceptions.
 */
typedef struct memop_t memop_t;
struct memop_t {
	value_t  value;      /**< the value of this memop: only defined for Load/Store */
	ir_node  *node;      /**< the memory op itself */
	ir_node  *mem;       /**< the memory FROM this node */
	ir_node  *replace;   /**< the replacement node if this memop is replaced */
	memop_t  *next;      /**< links to the next memory op in the block in forward order. */
	memop_t  *prev;      /**< links to the previous memory op in the block in forward order. */
	unsigned flags;      /**< memop flags */
};

/**
 * Additional data for every basic block.
 */
typedef struct block_t block_t;
struct block_t {
	memop_t  *memop_forward;     /**< topologically sorted list of memory ops in this block */
	memop_t  *memop_backward;    /**< last memop in the list */
	unsigned *avail_out;         /**< out-set of available addresses */
	memop_t  **id_2_memop_avail; /**< maps avail address ids to memops */
	unsigned *anticL_in;         /**< in-set of anticipated Load addresses */
	memop_t  **id_2_memop;       /**< maps address ids to memops */
	ir_node  *block;             /**< the associated block */
	block_t  *forward_next;      /**< next block entry for forward iteration */
	block_t  *backward_next;     /**< next block entry for backward iteration */
	memop_t  *avail;             /**< used locally for the avail map */
};

#define get_block_entry(block) ((block_t *)get_irn_link(block))

/**
 * Metadata for this pass.
 */
typedef struct ldst_env_t {
	struct obstack  obst;         /**< obstack for temporary data */
	ir_nodemap_t    adr_map;      /**< Map addresses to */
	block_t         *forward;     /**< Inverse post-order list of all blocks Start->End */
	block_t         *backward;    /**< Inverse post-order list of all blocks End->Start */
	ir_node         *start_bl;    /**< start block of the current graph */
	unsigned        *curr_set;    /**< current set of addresses */
	unsigned        curr_adr_id;  /**< number for address mapping */
	unsigned        n_mem_ops;    /**< number of memory operations (Loads/Stores) */
	unsigned        rbs_size;     /**< size of all bitsets in bytes */
	int             changed;      /**< Flags for changed graph state */
} ldst_env;

#ifdef DEBUG_libfirm

static firm_dbg_module_t *dbg;

/* the one and only environment */
static ldst_env env;

/**
 * Dumps the block list.
 *
 * @param ldst environment
 */
static void dump_block_list(ldst_env *env) {
	block_t *entry;
	memop_t *op;
	int     i;

	for (entry = env->forward; entry != NULL; entry = entry->forward_next) {
		DB((dbg, LEVEL_2, "%+F {", entry->block));

		i = 0;
		for (op = entry->memop_forward; op != NULL; op = op->next) {
			if (i == 0) {
				DB((dbg, LEVEL_2, "\n\t"));
			}			DB((dbg, LEVEL_2, "%+F", op->node));
			if ((op->flags & FLAG_KILL_ALL) == FLAG_KILL_ALL)
				DB((dbg, LEVEL_2, "X"));
			else if (op->flags & FLAG_KILL_LOADS)
				DB((dbg, LEVEL_2, "L"));
			else if (op->flags & FLAG_KILL_STORES)
				DB((dbg, LEVEL_2, "S"));
			DB((dbg, LEVEL_2, ", "));

			i = (i + 1) & 3;
		}
		DB((dbg, LEVEL_2, "\n}\n\n"));
	}
}

/**
 * Dumps the current set.
 *
 * @param bl   current block
 * @param s    name of the set
 */
static void dump_curr(block_t *bl, const char *s) {
	unsigned pos = 0;
	unsigned end = env.n_mem_ops * 2 - 1;
	int      i;

	DB((dbg, LEVEL_2, "%s[%+F] = {", s, bl->block));
	i = 0;
	for (pos = rbitset_next(env.curr_set, pos, 1); pos != end; pos = rbitset_next(env.curr_set, pos + 1, 1)) {
		memop_t *op = bl->id_2_memop[pos];

		if (i == 0) {
			DB((dbg, LEVEL_2, "\n\t"));
		}
		DB((dbg, LEVEL_2, "<%+F, %+F>, ", op->value.address, op->value.value));
		i = (i + 1) & 3;
	}
	DB((dbg, LEVEL_2, "\n}\n"));
}

#else
#define dump_block_list()
#define dump_curr(bl, s)
#endif /* DEBUG_libfirm */

/**
 * Walk over the memory edges from definition to users.
 *
 * @param irn   start node
 * @param pre   pre walker function
 * @param post  post walker function
 * @param ctx   context parameter for the walker functions
 */
static void walk_memory(ir_node *irn, irg_walk_func *pre, irg_walk_func *post, void *ctx) {
	int     i;
	ir_mode *mode;

	mark_irn_visited(irn);

	if (pre)
		pre(irn, ctx);

	mode = get_irn_mode(irn);
	if (mode == mode_M) {
		/* every successor uses memory */
		for (i = get_irn_n_outs(irn) - 1; i >= 0; --i) {
			ir_node *succ = get_irn_out(irn, i);

			if (! irn_visited(succ))
				walk_memory(succ, pre, post, ctx);
		}
	} else if (mode == mode_T) {
		/* only some Proj's uses memory */
		for (i = get_irn_n_outs(irn) - 1; i >= 0; --i) {
			ir_node *proj = get_irn_out(irn, i);

			if (get_irn_mode(proj) == mode_M && ! irn_visited(proj))
				walk_memory(proj, pre, post, ctx);
		}
	}
	if (post)
		post(irn, ctx);
}

/**
 * Walks over all memory nodes of a graph.
 *
 * @param irg   a graph
 * @param pre   pre walker function
 * @param post  post walker function
 * @param ctx   context parameter for the walker functions
 */
static void walk_memory_irg(ir_graph *irg, irg_walk_func pre, irg_walk_func post, void *ctx) {
	inc_irg_visited(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);

	/*
	 * there are two possible sources for memory: initial_mem and nomem
	 * we ignore nomem as this should NOT change the memory
	 */
	walk_memory(get_irg_initial_mem(irg), pre, post, ctx);

	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
}

/**
 * Block walker: allocate an block entry for every block.
 */
static void prepare_blocks(ir_node *block, void *ctx) {
	block_t *entry = obstack_alloc(&env.obst, sizeof(*entry));

	(void) ctx;

	entry->memop_forward    = NULL;
	entry->memop_backward   = NULL;
	entry->avail_out        = NULL;
	entry->id_2_memop_avail = NULL;
	entry->anticL_in        = NULL;
	entry->id_2_memop       = NULL;
	entry->block            = block;
	entry->forward_next     = env.forward;
	entry->backward_next    = NULL;
	entry->avail            = NULL;
	set_irn_link(block, entry);

	/* create the list in inverse order */
	env.forward = entry;
}

/**
 * Block walker: create backward links for the memops of a block.
 */
static void collect_backward(ir_node *block, void *ctx) {
	block_t *entry = get_block_entry(block);
	memop_t *last, *op;

	(void) ctx;
	entry->backward_next = env.backward;

	/* create the list in inverse order */
	env.backward = entry;

	/* create backward links for all memory ops */
	last = NULL;
	for (op = entry->memop_forward; op != NULL; op = op->next) {
		op->prev = last;
		last     = op;
	}
	entry->memop_backward = last;
}

/**
 * Allocate a memop.
 *
 * @param irn  the IR-node representing the memop
 */
static memop_t *alloc_memop(ir_node *irn) {
	memop_t *m = obstack_alloc(&env.obst, sizeof(*m));

	m->value.address = NULL;
	m->value.value   = NULL;
	m->value.mode    = NULL;

	m->node          = irn;
	m->replace       = NULL;
	m->next          = NULL;
	m->flags         = 0;

	return m;
}

/**
 * Register an address and allocate an ID for it.
 *
 * @param adr  the IR-node representing the address
 */
static unsigned register_address(ir_node *adr) {
	address_entry *entry = ir_nodemap_get(&env.adr_map, adr);

	if (entry == NULL) {
		/* new address */
		entry = obstack_alloc(&env.obst, sizeof(*entry));

		entry->id = env.curr_adr_id++;
		ir_nodemap_insert(&env.adr_map, adr, entry);
	}
	return entry->id;
}

/**
 * Return the memory properties of a call node.
 *
 * @param call  the call node
 *
 * return a bitset of mtp_property_const and mtp_property_pure
 */
static unsigned get_Call_memory_properties(ir_node *call) {
	ir_type *call_tp = get_Call_type(call);
	unsigned prop = get_method_additional_properties(call_tp);

	/* check first the call type */
	if ((prop & (mtp_property_const|mtp_property_pure)) == 0) {
		/* try the called entity */
		ir_node *ptr = get_Call_ptr(call);

		if (is_Global(ptr)) {
			ir_entity *ent = get_Global_entity(ptr);

			prop = get_entity_additional_properties(ent);
		}
	}
	return prop & (mtp_property_const|mtp_property_pure);
}

/**
 * Update a memop for a Load.
 *
 * @param m  the memop
 */
static void update_Load_memop(memop_t *m) {
	int     i;
	ir_node *load = m->node;
	ir_node *adr  = get_Load_ptr(load);

	if (get_Load_volatility(load) == volatility_is_volatile)
		m->flags |= FLAG_IGNORE;

	m->value.address = adr;

	for (i = get_irn_n_outs(load) - 1; i >= 0; --i) {
		ir_node *proj = get_irn_out(load, i);

		switch (get_Proj_proj(proj)) {
		case pn_Load_res:
			m->value.value = proj;
			m->value.mode  = get_irn_mode(proj);
			break;
		case pn_Load_X_except:
			m->flags |= FLAG_EXCEPTION;
			break;
		case pn_Load_M:
			m->mem = proj;
			break;
		}
	}

	if (m->value.value != NULL && !(m->flags & FLAG_IGNORE)) {
		/* only create an address if this node is NOT killed immediately or ignored */
		m->value.id = register_address(adr);
		++env.n_mem_ops;
	} else {
		/* no user, KILL it */
		m->flags |= FLAG_KILLED_NODE;
	}
}

/**
 * Update a memop for a Store.
 *
 * @param m  the memop
 */
static void update_Store_memop(memop_t *m) {
	int     i;
	ir_node *store = m->node;
	ir_node *adr   = get_Store_ptr(store);

	if (get_Store_volatility(store) == volatility_is_volatile) {
		m->flags |= FLAG_IGNORE;
	} else {
		/* only create an address if this node is NOT ignored */
		m->value.id = register_address(adr);
		++env.n_mem_ops;
	}

	m->value.address = adr;

	for (i = get_irn_n_outs(store) - 1; i >= 0; --i) {
		ir_node *proj = get_irn_out(store, i);

		switch (get_Proj_proj(proj)) {
		case pn_Store_X_except:
			m->flags |= FLAG_EXCEPTION;
			break;
		case pn_Store_M:
			m->mem = proj;
			break;
		}
	}
	m->value.value = get_Store_value(store);
	m->value.mode  = get_irn_mode(m->value.value);
}

/**
 * Update a memop for a Call.
 *
 * @param m  the memop
 */
static void update_Call_memop(memop_t *m) {
	ir_node  *call = m->node;
	unsigned prop  = get_Call_memory_properties(call);
	int      i;

	if (prop & mtp_property_const) {
		/* A constant call did NOT use memory at all, we
		   can kick it from the list. */
	} else if (prop & mtp_property_pure) {
		/* pure calls READ memory */
		m->flags = FLAG_KILL_STORES;
	} else
		m->flags = FLAG_KILL_ALL;

	for (i = get_irn_n_outs(call) - 1; i >= 0; --i) {
		ir_node *proj = get_irn_out(call, i);

		switch (get_Proj_proj(proj)) {
		case pn_Call_X_except:
			m->flags |= FLAG_EXCEPTION;
			break;
		case pn_Call_M_regular:
			m->mem = proj;
			break;
		}
	}
}

/**
 * Update a memop for a Div/Mod/Quot/DivMod.
 *
 * @param m  the memop
 */
static void update_DivOp_memop(memop_t *m) {
	ir_node *div = m->node;
	int     i;

	for (i = get_irn_n_outs(div) - 1; i >= 0; --i) {
		ir_node *proj = get_irn_out(div, i);

		switch (get_Proj_proj(proj)) {
		case pn_Generic_X_except:
			m->flags |= FLAG_EXCEPTION;
			break;
		case pn_Generic_M_regular:
			m->mem = proj;
			break;
		}
	}
}

/**
 * Memory walker: collect all memory ops and build topological lists.
 */
static void collect_memops(ir_node *irn, void *ctx) {
	memop_t  *op;
	ir_node  *block;
	block_t  *entry;

	(void) ctx;
	if (is_Proj(irn)) {
		/* we can safely ignore ProjM's except the initial memory */
		if (irn != get_irg_initial_mem(current_ir_graph))
			return;
	}

	op    = alloc_memop(irn);
	block = get_nodes_block(irn);
	entry = get_block_entry(block);

	if (is_Phi(irn)) {
		/* Phis must be always placed first */
		op->next = entry->memop_forward;
		entry->memop_forward = op;
		if (entry->memop_backward == NULL)
			entry->memop_backward = op;
	} else {
		switch (get_irn_opcode(irn)) {
		case iro_Load:
			update_Load_memop(op);
			break;
		case iro_Store:
			update_Store_memop(op);
			break;
		case iro_Call:
			update_Call_memop(op);
			break;
		case iro_Sync:
		case iro_Pin:
			op->mem = irn;
			break;
		case iro_Proj:
			/* initial memory */
			op->mem = irn;
			break;
		case iro_Return:
		case iro_End:
			/* we can those to find the memory edge */
			break;
		case iro_Div:
		case iro_DivMod:
		case iro_Quot:
		case iro_Mod:
			update_DivOp_memop(op);
			break;

		case iro_Builtin:
			/* TODO: handle some builtins */
		default:
			/* unsupported operation */
			op->flags = FLAG_KILL_ALL;
		}


		/* all other should be placed last */
		if (entry->memop_backward == NULL) {
			entry->memop_forward = entry->memop_backward = op;
		} else {
			entry->memop_backward->next = op;
			entry->memop_backward       = op;
		}
	}
}

/**
 * Find an address in the current set.
 *
 * @param bl     the block
 * @param value  the value to be searched for
 */
static memop_t *find_address(const block_t *bl, const value_t *value) {
	if (rbitset_is_set(env.curr_set, value->id)) {
		memop_t *res = bl->id_2_memop[value->id];

		if (res->value.mode == value->mode)
			return res;
		/* allow hidden casts */
		if (get_mode_arithmetic(res->value.mode) == irma_twos_complement &&
		    get_mode_arithmetic(value->mode) == irma_twos_complement &&
		    get_mode_size_bits(res->value.mode) == get_mode_size_bits(value->mode))
			return res;
	}
	return NULL;
}

/**
 * Find an address in the avail_out set.
 *
 * @param bl     the block
 * @param value  the value to be searched for
 */
static memop_t *find_address_avail(const block_t *bl, const value_t *value) {
	if (rbitset_is_set(bl->avail_out, value->id)) {
		memop_t *res = bl->id_2_memop_avail[value->id];

		if (res->value.mode == value->mode)
			return res;
		/* allow hidden casts */
		if (get_mode_arithmetic(res->value.mode) == irma_twos_complement &&
		    get_mode_arithmetic(value->mode) == irma_twos_complement &&
		    get_mode_size_bits(res->value.mode) == get_mode_size_bits(value->mode))
			return res;
	}
	return NULL;
}

/**
 * Kill all Loads from the current set.
 *
 * @param bl  the current block
 */
static void kill_all_loads(block_t *bl) {
	unsigned pos = 0;
	unsigned end = env.n_mem_ops * 2 - 1;

	for (pos = rbitset_next(env.curr_set, pos, 1); pos != end; pos = rbitset_next(env.curr_set, pos + 1, 1)) {
		memop_t *op = bl->id_2_memop[pos];

		if (! is_Store(op->node))
			rbitset_clear(env.curr_set, pos);
	}
}

/**
 * Kill all Stores from the current set.
 *
 * @param bl  the current block
 */
static void kill_all_stores(block_t *bl) {
	unsigned pos = 0;
	unsigned end = env.n_mem_ops * 2 - 1;

	for (pos = rbitset_next(env.curr_set, pos, 1); pos != end; pos = rbitset_next(env.curr_set, pos + 1, 1)) {
		memop_t *op = bl->id_2_memop[pos];

		if (is_Store(op->node))
			rbitset_clear(env.curr_set, pos);
	}
}

/**
 * Kill all addresses from the current set.
 */
static void kill_all(void) {
	rbitset_clear_all(env.curr_set, env.rbs_size);

	/* set sentinel */
	rbitset_set(env.curr_set, env.rbs_size - 1);
}


/**
 * Kill Stores that are not alias free due to a Load value from the current set.
 *
 * @param bl     the block
 * @param value  the Load value
 */
static void kill_stores(block_t *bl, const value_t *value) {
	unsigned pos = 0;
	unsigned end = env.n_mem_ops * 2 - 1;

	for (pos = rbitset_next(env.curr_set, pos, 1); pos != end; pos = rbitset_next(env.curr_set, pos + 1, 1)) {
		memop_t *op = bl->id_2_memop[pos];

		if (is_Store(op->node)) {
			if (ir_no_alias != get_alias_relation(current_ir_graph, value->address, value->mode,
			                                      op->value.address, op->value.mode)) {
				rbitset_clear(env.curr_set, pos);
				bl->id_2_memop[pos] = NULL;
			}
		}
	}
}

/**
 * Kill memops that are not alias free due to a Store value from the current set.
 *
 * @param bl     the block
 * @param value  the Store value
 */
static void kill_memops(block_t *bl, const value_t *value) {
	unsigned pos = 0;
	unsigned end = env.n_mem_ops * 2 - 1;

	for (pos = rbitset_next(env.curr_set, pos, 1); pos != end; pos = rbitset_next(env.curr_set, pos + 1, 1)) {
		memop_t *op = bl->id_2_memop[pos];

		if (ir_no_alias != get_alias_relation(current_ir_graph, value->address, value->mode,
			                                  op->value.address, op->value.mode)) {
			rbitset_clear(env.curr_set, pos);
			bl->id_2_memop[pos] = NULL;
		}
	}
}

/**
 * Add the value of a memop to the current set.
 *
 * @param bl  the block
 * @param op  the memory op
 */
static void add_memop(block_t *bl, memop_t *op) {
	rbitset_set(env.curr_set, op->value.id);
	bl->id_2_memop[op->value.id] = op;
}

/**
 * Add the value of a memop to the avail_out set.
 *
 * @param bl  the block
 * @param op  the memory op
 */
static void add_memop_avail(block_t *bl, memop_t *op) {
	rbitset_set(bl->avail_out, op->value.id);
	bl->id_2_memop_avail[op->value.id] = op;
}

/**
 * find a definition for a value in the given block.
 *
 * @param block  the block
 * @param value  the value
 *
 * @return the definition
 */
static ir_node *find_definition(ir_node *block, const value_t *value) {
	ir_node *def_bl = get_nodes_block(value->value);
	ir_node **in, *phi;
	int     i, n;
	memop_t *mop;

	if (block_dominates(def_bl, block))
		return value->value;

	/* no: we need a new Phi */
	n = get_Block_n_cfgpreds(block);

	while (n == 1) {
		block = get_Block_cfgpred_block(block, 0);
		n     = get_Block_n_cfgpreds(block);
		mop   = find_address(get_block_entry(block), value);

		assert(mop != NULL);
		value = &mop->value;
	}

	NEW_ARR_A(ir_node *, in, n);
	for (i = n - 1; i >= 0; --i) {
		ir_node *pred_bl = get_Block_cfgpred_block(block, i);
		mop   = find_address(get_block_entry(pred_bl), value);
		in[i] = find_definition(pred_bl, &mop->value);
	}

	phi = new_r_Phi(current_ir_graph, block, n, in, value->mode);
	DB((dbg, LEVEL_2, "Created new Phi %+F for value %+F in block %+F\n", phi, value->value, block));
	return phi;
}

/**
 * Mark a Load memop to be replace by a definition
 *
 * @param op  the Load memop
 */
static void mark_replace_load(memop_t *op, ir_node *def) {
	op->replace = def;
	op->flags |= FLAG_KILLED_NODE;
	env.changed = 1;
}

/**
 * Mark a Store memop to be removed.
 *
 * @param op  the Store memop
 */
static void mark_remove_store(memop_t *op) {
	op->flags |= FLAG_KILLED_NODE;
	env.changed = 1;
}

#define BYTE_SIZE(x)  (((x) + 7) >> 3)

/**
 * Do forward dataflow analysis on a given block to calculate the avail_out set.
 *
 * @param bl  the block
 *
 * @return non-zero if the set has changed since last iteration
 */
static int forward_avail(block_t *bl) {
	memop_t *op;
	int     n;
	ir_node *def;
	ir_node *pred    = get_Block_cfgpred_block(bl->block, 0);
	block_t *pred_bl = get_block_entry(pred);

	rbitset_cpy(env.curr_set, pred_bl->avail_out, env.rbs_size);

	n = get_Block_n_cfgpreds(bl->block);
	if (n > 1) {
		int i, pos;

		/* more than one predecessors, calculate the join */
		for (i = n - 1; i > 0; --i) {
			ir_node *pred    = get_Block_cfgpred_block(bl->block, i);
			block_t *pred_bl = get_block_entry(pred);

			rbitset_and(env.curr_set, pred_bl->avail_out, env.rbs_size);

		}
		/* sure that all values are in the map */
		for (pos = env.rbs_size - 1; pos >= 0; --pos) {
			if (! rbitset_is_set(env.curr_set, pos))
				bl->id_2_memop[pos] = NULL;
			else {
				for (i = n - 1; i >= 0; --i) {
					ir_node *pred    = get_Block_cfgpred_block(bl->block, i);
					block_t *pred_bl = get_block_entry(pred);

					if (pred_bl->id_2_memop[pos] != NULL) {
						bl->id_2_memop[pos] = pred_bl->id_2_memop[pos];
						break;
					}
				}
			}
		}
	} else {
		/* only one predecessor, simply copy the map */
		memcpy(bl->id_2_memop, pred_bl->id_2_memop, env.rbs_size * sizeof(bl->id_2_memop[0]));
	}

	dump_curr(bl, "Avail_in");

	for (op = bl->memop_forward; op != NULL; op = op->next) {
		switch (get_irn_opcode(op->node)) {
		case iro_Phi:
			/* meet */
			break;
		case iro_Sync:
			/* join */
			break;
		case iro_Load:
			if (! (op->flags & (FLAG_KILLED_NODE|FLAG_IGNORE))) {
				/* do we have this already? */
				memop_t *other = find_address(bl, &op->value);
				if (other != NULL) {
					if (is_Store(other->node)) {
						/* RAW */
						DB((dbg, LEVEL_1, "RAW %+F %+F\n", op->node, other->node));
					} else {
						/* RAR */
						DB((dbg, LEVEL_1, "RAR %+F %+F\n", op->node, other->node));
					}
					def = find_definition(bl->block, &other->value);
					mark_replace_load(op, def);
				} else {
					/* add this value */
					kill_stores(bl, &op->value);
					add_memop(bl, op);
				}
			}
			break;
		case iro_Store:
			if (! (op->flags & (FLAG_KILLED_NODE|FLAG_IGNORE))) {
				/* do we have this store already */
				memop_t *other = find_address(bl, &op->value);
				if (other != NULL) {
					if (is_Store(other->node)) {
						/* a WAW */
						DB((dbg, LEVEL_1, "WAW %+F %+F\n", op->node, other->node));
						mark_remove_store(other);
						/* FIXME: a Load might be get freed due to this killed store */
					} else if (other->value.value == op->value.value) {
						/* WAR */
						DB((dbg, LEVEL_1, "WAR %+F %+F\n", op->node, other->node));
						mark_remove_store(op);
					} else {
						/* we overwrite the value that was loaded */
						add_memop(bl, op);
					}
				} else {
					/* add this value */
					kill_memops(bl, &op->value);
					add_memop(bl, op);
				}
			}
			break;
		default:
			switch (op->flags & (FLAG_KILL_LOADS|FLAG_KILL_STORES)) {
			case FLAG_KILL_LOADS|FLAG_KILL_STORES:
				kill_all();
				break;
			case FLAG_KILL_LOADS:
				kill_all_loads(bl);
				break;
			case FLAG_KILL_STORES:
				kill_all_stores(bl);
				break;
			case 0:
				break;
			}
		}
	}
	dump_curr(bl, "Avail_out");
	if (!rbitset_equal(bl->avail_out, env.curr_set, env.rbs_size)) {
		/* changed */
		rbitset_cpy(bl->avail_out, env.curr_set, env.rbs_size);
		return 1;
	}
	return 0;
}

/**
 * Do backward dataflow analysis on a given block to calculate the antic set
 * of Loaded addresses.
 *
 * @param bl  the block
 *
 * @return non-zero if the set has changed since last iteration
 */
static int backward_antic(block_t *bl) {
	memop_t *op;
	ir_node *succ    = get_Block_cfg_out(bl->block, 0);
	block_t *succ_bl = get_block_entry(succ);
	int     n;

	rbitset_cpy(env.curr_set, succ_bl->anticL_in, env.rbs_size);
	memcpy(bl->id_2_memop, succ_bl->id_2_memop, env.rbs_size * sizeof(bl->id_2_memop[0]));

	n = get_Block_n_cfg_outs(bl->block);
	if (n > 1) {
		int i;

		for (i = n - 1; i > 0; --i) {
			ir_node *succ    = get_Block_cfg_out(bl->block, i);
			block_t *succ_bl = get_block_entry(succ);

			rbitset_and(env.curr_set, succ_bl->anticL_in, env.rbs_size);
		}
	}

#if 0
	/* cleanup: kill those Loads which address is not available */
	for (pos = rbitset_next(env.curr_set, pos, 1); pos != end; pos = rbitset_next(env.curr_set, pos + 1, 1)) {
		memop_t *op     = succ_bl->id_2_memop[pos];
		ir_node *ptr    = get_Load_ptr(op->node);
		ir_node *ptr_bl = get_nodes_block(ptr);

		if (!block_dominates(ptr_bl, bl->block))
			rbitset_clear(env.curr_set, pos);
	}
#endif

	dump_curr(bl, "AnticL_out");

	for (op = bl->memop_backward; op != NULL; op = op->prev) {
		switch (get_irn_opcode(op->node)) {
		case iro_Phi:
			/* meet */
			break;
		case iro_Sync:
			/* join */
			break;
		case iro_Load:
			if (! (op->flags & (FLAG_KILLED_NODE|FLAG_IGNORE))) {
				/* always add it */
				add_memop(bl, op);
			}
			break;
		case iro_Store:
			if (! (op->flags & (FLAG_KILLED_NODE|FLAG_IGNORE))) {
				/* a Store: check which memops must be killed */
				kill_memops(bl, &op->value);
			}
			break;
		default:
			switch (op->flags & (FLAG_KILL_LOADS|FLAG_KILL_STORES)) {
			case FLAG_KILL_LOADS|FLAG_KILL_STORES:
				kill_all();
				break;
			case FLAG_KILL_LOADS:
				kill_all_loads(bl);
				break;
			case FLAG_KILL_STORES:
				kill_all_stores(bl);
				break;
			case 0:
				break;
			}
		}
	}
	dump_curr(bl, "AnticL_in");
	if (! rbitset_equal(bl->anticL_in, env.curr_set, env.rbs_size)) {
		/* changed */
		rbitset_cpy(bl->anticL_in, env.curr_set, env.rbs_size);
		return 1;
	}
	return 0;
}

/**
 * Replace a Load memop by a already known value.
 *
 * @param op  the Load memop
 */
static void replace_load(memop_t *op) {
	ir_node *load = op->node;
	ir_node *def  = skip_Id(op->replace);
	int     i;
	ir_mode *mode;

	if (def != NULL)
		DB((dbg, LEVEL_1, "Replacing %+F by definition %+F\n", load, is_Proj(def) ? get_Proj_pred(def) : def));
	else {
		if (op->flags & FLAG_EXCEPTION) {
			/* bad: this node is unused and executed for exception only */
			DB((dbg, LEVEL_1, "Unused %+F executed for exception only ...\n", load));
			return;
		}
		DB((dbg, LEVEL_1, "Killing unused %+F\n", load));
	}
	for (i = get_irn_n_outs(load) - 1; i >= 0; --i) {
		ir_node *proj = get_irn_out(load, i);

		switch (get_Proj_proj(proj)) {
		case pn_Load_M:
			exchange(proj, get_Load_mem(load));
			break;
		case pn_Load_res:
			mode = get_irn_mode(proj);
			if (get_irn_mode(def) != mode) {
				/* a hidden cast */
				dbg_info *db    = get_irn_dbg_info(load);
				ir_node  *block = get_nodes_block(proj);
				def = new_rd_Conv(db, current_ir_graph, block, def, mode);
			}
			exchange(proj, def);
			break;
		case pn_Load_X_except:
			exchange(proj, new_Bad());
			break;
		case pn_Load_X_regular:
			exchange(proj, new_r_Jmp(current_ir_graph, get_nodes_block(load)));
			break;
		default:
			panic("Unknown Proj from Load");
		}
	}
}

/**
 * Remove a Store memop.
 *
 * @param op  the Store memop
 */
static void remove_store(memop_t *op) {
	ir_node *store = op->node;
	int     i;

	DB((dbg, LEVEL_1, "Removing %+F\n", store));
	for (i = get_irn_n_outs(store) - 1; i >= 0; --i) {
		ir_node *proj = get_irn_out(store, i);

		switch (get_Proj_proj(proj)) {
		case pn_Store_M:
			exchange(proj, get_Store_mem(store));
			break;
		case pn_Store_X_except:
			exchange(proj, new_Bad());
			break;
		case pn_Store_X_regular:
			exchange(proj, new_r_Jmp(current_ir_graph, get_nodes_block(store)));
			break;
		default:
			panic("Unknown Proj from Store");
		}
	}
}


/**
 * Do all necessary replacements for a given block.
 *
 * @param bl  the block
 */
static void do_replacements(block_t *bl) {
	memop_t *op;

	for (op = bl->memop_forward; op != NULL; op = op->next) {
		if (op->flags & FLAG_KILLED_NODE) {
			switch (get_irn_opcode(op->node)) {
			case iro_Load:
				replace_load(op);
				break;
			case iro_Store:
				remove_store(op);
				break;
			}
		}
	}
}

/**
 * Calculate the Avail_out sets for all basic blocks.
 */
static void calcAvail(void) {
	int i, need_iter;
	block_t *bl;

	/* calculate avail_out */
	DB((dbg, LEVEL_2, "Calculate Avail_out\n"));
	i = 0;
	do {
		DB((dbg, LEVEL_2, "Iteration %d:\n=========\n", i));

		need_iter = 0;

		/* over all blocks in reverse post order, skip the start block */
		for (bl = env.forward->forward_next; bl != NULL; bl = bl->forward_next) {
			need_iter |= forward_avail(bl);
		}
		++i;
	} while (need_iter);

	/* copy the content of the id_2_memop map into the id_2_memop_avail map
	   as it must be preserved for later use */
	for (bl = env.forward->forward_next; bl != NULL; bl = bl->forward_next) {
		memop_t **t = bl->id_2_memop_avail;

		bl->id_2_memop_avail = bl->id_2_memop;
		bl->id_2_memop       = t;
	}
	DB((dbg, LEVEL_2, "Get avail set after %d iterations\n\n", i));
}

/**
 * Calculate the Antic_in sets for all basic blocks.
 */
static void calcAntic(void) {
	int i, need_iter;

	/* calculate antic_out */
	DB((dbg, LEVEL_2, "Calculate Antic_in\n"));
	i = 0;
	do {
		block_t *bl;

		DB((dbg, LEVEL_2, "Iteration %d:\n=========\n", i));

		need_iter = 0;

		/* over all blocks in reverse post order */
		for (bl = env.backward->backward_next; bl != NULL; bl = bl->backward_next) {
			need_iter |= backward_antic(bl);
		}
		++i;
	} while (need_iter);
	DB((dbg, LEVEL_2, "Get anticipated Load set after %d iterations\n", i));
}

/**
 * Return the node representing the last memory in a block.
 *
 * @param bl  the block
 */
static ir_node *find_first_memory(block_t *bl) {
	for (;;) {
		if (bl->memop_forward != NULL) {
			return bl->memop_forward->node;
		}
		/* if there is NO memory in this block, go to the post dominator */
		bl = get_block_entry(get_Block_ipostdom(bl->block));
	}
}

/**
 * Return the node representing the last memory in a block.
 *
 * @param bl  the block
 */
static ir_node *find_last_memory(block_t *bl) {
	for (;;) {
		if (bl->memop_backward != NULL) {
			return bl->memop_backward->mem;
		}
		/* if there is NO memory in this block, go to the dominator */
		bl = get_block_entry(get_Block_idom(bl->block));
	}
}

/**
 * Reroute the memory. Reroute all users of old memory
 * to a new memory IR-node.
 *
 * @param omem  the old memory IR-node
 * @param nmem  the new memory IR-node
 */
static void reroute_mem(ir_node *omem, ir_node *nmem) {
	int i;

	for (i = get_irn_n_outs(omem) - 1; i >= 0; --i) {
		int     n_pos;
		ir_node *succ = get_irn_out_ex(omem, i, &n_pos);

		set_irn_n(succ, n_pos, nmem);
	}

	/* all edges formally point to omem now point to nmem */
	nmem->out = omem->out;
}

/**
 * insert
 */
static void insert_Load(ir_node *block, void *ctx) {
	int     *new_stuff = ctx;
	block_t *bl;
	int     i, n = get_Block_n_cfgpreds(block);
	unsigned pos = 0;
	unsigned end = env.n_mem_ops * 2 - 1;

	if (n <= 1)
		return;

	bl = get_block_entry(block);

	for (pos = rbitset_next(bl->anticL_in, pos, 1); pos != end; pos = rbitset_next(bl->anticL_in, pos + 1, 1)) {
		memop_t *op = bl->id_2_memop[pos];
		int     have_some;

		assert(is_Load(op->node));

		if (op->flags & FLAG_KILLED_NODE)
			continue;

		have_some  = 0;
		for (i = n - 1; i >= 0; --i) {
			ir_node *pred    = get_Block_cfgpred_block(block, i);
			block_t *pred_bl = get_block_entry(pred);
			memop_t *e       = find_address_avail(pred_bl, &op->value);

			if (e == NULL) {
				ir_node *block = get_nodes_block(op->value.address);
				if (! block_dominates(block, pred)) {
					/* cannot place a copy here */
					have_some = 0;
					break;
				}
				pred_bl->avail = NULL;
			} else {
				pred_bl->avail = e;
				have_some      = 1;
			}
		}
		if (have_some) {
			ir_mode *mode = op->value.mode;
			ir_node **in, *phi;

			NEW_ARR_A(ir_node *, in, n);

			for (i = n - 1; i >= 0; --i) {
				ir_node *pred    = get_Block_cfgpred_block(block, i);
				block_t *pred_bl = get_block_entry(pred);

				if (pred_bl->avail == NULL) {
					/* create a new Load here and make to make it fully redundant */
					dbg_info *db       = get_irn_dbg_info(op->node);
					ir_node  *last_mem = find_last_memory(pred_bl);
					ir_node  *load, *def;
					memop_t  *new_op;

					load = new_rd_Load(db, current_ir_graph, pred, last_mem, op->value.address, mode, cons_none);
					def  = new_r_Proj(current_ir_graph, pred, load, mode, pn_Load_res);
					DB((dbg, LEVEL_1, "Created new %+F for party redundant %+F\n", load, op->node));

					new_op                = alloc_memop(load);
					new_op->mem           = new_r_Proj(current_ir_graph, pred, load, mode_M, pn_Load_M);
					new_op->value.address = op->value.address;
					new_op->value.id      = op->value.id;
					new_op->value.mode    = mode;
					new_op->value.value   = def;

					new_op->prev            = pred_bl->memop_backward;
					pred_bl->memop_backward = new_op;

					/* We have add a new last memory op in pred block.
					   If pred had already a last mem, reroute all memory
					   users. */
					if (get_nodes_block(last_mem) == pred) {
						reroute_mem(last_mem, new_op->mem);
					}

					/* we added this load at the end, so it will be avail anyway */
					add_memop_avail(pred_bl, new_op);
					pred_bl->avail = new_op;
				}
				in[i] = pred_bl->avail->value.value;
			}
			phi = new_r_Phi(current_ir_graph, block, n, in, mode);
			mark_replace_load(op, phi);
			*new_stuff = 1;

			if (get_nodes_block(op->node) == block) {
				/* The redundant node was in the current block:
				   In that case, DO NOT update avail_out. If it was NOT
				   avail although it is executed in this bLock, it is killed by a later
				   instruction.
				 */
			} else {
				/* The redundant node is NOT in the current block and anticipated.
				   This can only happen IFF nothings kills the Load in the current block,
				   so it will be avail in the next iteration.
				 */
				add_memop_avail(bl, op);

				/* TODO propagate it downwards */
			}
		}
	}
}

/**
 * Insert Loads upwards.
 */
static void insert_Loads_upwards(void) {
	int i, need_iter;

	/* calculate antic_out */
	DB((dbg, LEVEL_2, "Inserting Loads"));
	i = 0;
	do {
		DB((dbg, LEVEL_2, "Iteration %d:\n=========\n", i));

		need_iter = 0;
		dom_tree_walk_irg(current_ir_graph, insert_Load, NULL, &need_iter);
		++i;
	} while (need_iter);
	DB((dbg, LEVEL_2, "Finished Load inserting after %d iterations\n", i));
}

int opt_ldst(ir_graph *irg) {
	block_t  *bl;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;

	FIRM_DBG_REGISTER(dbg, "firm.opt.ldst");

	DB((dbg, LEVEL_1, "\nDoing Load/Store optimization on %+F\n", irg));

	/* we need landing pads */
	remove_critical_cf_edges(irg);

	if (get_opt_alias_analysis()) {
		assure_irg_entity_usage_computed(irg);
		assure_irp_globals_entity_usage_computed();
	}

	obstack_init(&env.obst);
	ir_nodemap_init(&env.adr_map);

	env.forward     = NULL;
	env.backward    = NULL;
	env.curr_adr_id = 0;
	env.n_mem_ops   = 0;
	env.changed     = 0;
	env.start_bl    = get_irg_start_block(irg);

	assure_doms(irg);
	assure_irg_outs(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	/* first step: allocate block entries: produces an
	   inverse post-order list for the CFG */
	irg_out_block_walk(get_irg_start_block(irg), NULL, prepare_blocks, NULL);

	/* second step: find and sort all memory ops */
	walk_memory_irg(irg, collect_memops, NULL, NULL);

	if (env.n_mem_ops == 0) {
		/* no memory ops */
		goto end;
	}

	/* create the backward links */
	irg_block_walk_graph(irg, NULL, collect_backward, NULL);

	/* check that we really start with the start / end block */
	assert(env.forward->block  == env.start_bl);
	assert(env.backward->block == get_irg_end_block(irg));

	/* create address sets: we know that 2 * n_mem_ops - 1 is an upper bound for all possible addresses */
	env.rbs_size = 2 * env.n_mem_ops;

	/* create the current set */
	env.curr_set = rbitset_obstack_alloc(&env.obst, env.rbs_size);
	rbitset_set(env.curr_set, env.rbs_size - 1);

	for (bl = env.forward; bl != NULL; bl = bl->forward_next) {
		/* set sentinel bits */
		bl->avail_out  = rbitset_obstack_alloc(&env.obst, env.rbs_size);
		rbitset_set(bl->avail_out, env.rbs_size - 1);

		bl->id_2_memop_avail = NEW_ARR_D(memop_t *, &env.obst, env.rbs_size);
		memset(bl->id_2_memop_avail, 0, env.rbs_size * sizeof(bl->id_2_memop[0]));

		bl->anticL_in  = rbitset_obstack_alloc(&env.obst, env.rbs_size);
		rbitset_set(bl->anticL_in, env.rbs_size - 1);

		bl->id_2_memop = NEW_ARR_D(memop_t *, &env.obst, env.rbs_size);
		memset(bl->id_2_memop, 0, env.rbs_size * sizeof(bl->id_2_memop[0]));
	}

//	dump_block_list(&env);

	calcAvail();
	calcAntic();

	insert_Loads_upwards();

	if (env.changed) {
		/* over all blocks in reverse post order */
		for (bl = env.forward; bl != NULL; bl = bl->forward_next) {
			do_replacements(bl);
		}

		set_irg_outs_inconsistent(irg);
		set_irg_entity_usage_state(irg, ir_entity_usage_not_computed);
	}
end:

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	ir_nodemap_destroy(&env.adr_map);
	obstack_free(&env.obst, NULL);

	current_ir_graph = rem;

	return env.changed != 0;
}
