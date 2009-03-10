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

/* maximum number of output Proj's */
#define MAX_PROJ (pn_Load_max > pn_Store_max ? pn_Load_max : pn_Store_max)

/**
 * Mapping an address to an dense ID.
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
	ir_node  *projs[MAX_PROJ]; /**< Projs of this memory op */
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
	memop_t  **id_2_memop_antic; /**< maps anticipated address ids to memops */
	ir_node  *block;             /**< the associated block */
	block_t  *forward_next;      /**< next block entry for forward iteration */
	block_t  *backward_next;     /**< next block entry for backward iteration */
	memop_t  *avail;             /**< used locally for the avail map */
};

/**
 * Metadata for this pass.
 */
typedef struct ldst_env_t {
	struct obstack  obst;              /**< obstack for temporary data */
	ir_nodemap_t    adr_map;           /**< Map addresses to */
	block_t         *forward;          /**< Inverse post-order list of all blocks Start->End */
	block_t         *backward;         /**< Inverse post-order list of all blocks End->Start */
	ir_node         *start_bl;         /**< start block of the current graph */
	ir_node         *end_bl;           /**< end block of the current graph */
	unsigned        *curr_set;         /**< current set of addresses */
	memop_t         **curr_id_2_memop; /**< current map of address ids to memops */
	unsigned        curr_adr_id;       /**< number for address mapping */
	unsigned        n_mem_ops;         /**< number of memory operations (Loads/Stores) */
	unsigned        rbs_size;          /**< size of all bitsets in bytes */
	int             changed;           /**< Flags for changed graph state */
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
		memop_t *op = env.curr_id_2_memop[pos];

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

/** Get the block entry for a block node */
static block_t *get_block_entry(const ir_node *block) {
	assert(is_Block(block));

	return get_irn_link(block);
}

/** Get the memop entry for a memory operation node */
static memop_t *get_irn_memop(const ir_node *irn) {
	assert(! is_Block(irn));
	return get_irn_link(irn);
}

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
	entry->id_2_memop_antic = NULL;
	entry->block            = block;
	entry->forward_next     = env.forward;
	entry->backward_next    = NULL;
	entry->avail            = NULL;
	set_irn_link(block, entry);

	/* create the list in inverse order */
	env.forward  = entry;
	/* remember temporary the last one */
	env.backward = entry;
}

/**
 * Block walker: create backward links for the memops of a block.
 */
static void collect_backward(ir_node *block, void *ctx) {
	block_t *entry = get_block_entry(block);
	memop_t *last, *op;

	(void)ctx;

	/*
	 * Do NOT link in the end block yet. We want it to be
	 * the first in the list. This is NOT guaranteed by the walker
	 * if we have endless loops.
	 */
	if (block != env.end_bl) {
		entry->backward_next = env.backward;

		/* create the list in inverse order */
		env.backward = entry;
	}

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

	memset(m->projs, 0, sizeof(m->projs));

	set_irn_link(irn, m);
	return m;
}

/**
 * Create a memop for a Phi-replacement.
 *
 * @param op   the memop to clone
 * @param phi  the Phi-node representing the new value
 */
static memop_t *clone_memop_phi(memop_t *op, ir_node *phi) {
	memop_t *m = obstack_alloc(&env.obst, sizeof(*m));

	m->value         = op->value;
	m->value.value   = phi;

	m->node          = phi;
	m->replace       = NULL;
	m->next          = NULL;
	m->flags         = 0;

	set_irn_link(phi, m);
	return m;
}

/**
 * Register an address and allocate an ID for it.
 *
 * @param adr  the IR-node representing the address
 */
static unsigned register_address(ir_node *adr) {
	address_entry *entry;

	/* skip Confirms and Casts */
restart:
	if (is_Confirm(adr)) {
		adr = get_Confirm_value(adr);
		goto restart;
	}
	if (is_Cast(adr)) {
		adr = get_Cast_op(adr);
		goto restart;
	}

	entry = ir_nodemap_get(&env.adr_map, adr);

	if (entry == NULL) {
		/* new address */
		entry = obstack_alloc(&env.obst, sizeof(*entry));

		entry->id = env.curr_adr_id++;
		ir_nodemap_insert(&env.adr_map, adr, entry);

		DB((dbg, LEVEL_3, "ADDRESS %+F has ID %u\n", adr, entry->id));
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
		long    pn;

		/* beware of keep edges */
		if (is_End(proj))
			continue;

		pn = get_Proj_proj(proj);
		m->projs[pn] = proj;
		switch (pn) {
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
		case pn_Load_X_regular:
			break;
		default:
			panic("Unsupported Proj from Load %+F", proj);
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
		long    pn;

		/* beware of keep edges */
		if (is_End(proj))
			continue;

		pn = get_Proj_proj(proj);
		m->projs[pn] = proj;
		switch (pn) {
		case pn_Store_X_except:
			m->flags |= FLAG_EXCEPTION;
			break;
		case pn_Store_M:
			m->mem = proj;
			break;
		case pn_Store_X_regular:
			break;
		default:
			panic("Unsupported Proj from Store %+F", proj);
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

		/* beware of keep edges */
		if (is_End(proj))
			continue;

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

		/* beware of keep edges */
		if (is_End(proj))
			continue;

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
 * Update a memop for a Phi.
 *
 * @param m  the memop
 */
static void update_Phi_memop(memop_t *m) {
	/* the Phi is it's own mem */
	m->mem = m->node;
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
		update_Phi_memop(op);
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
 * @param value  the value to be searched for
 */
static memop_t *find_address(const value_t *value) {
	if (rbitset_is_set(env.curr_set, value->id)) {
		memop_t *res = env.curr_id_2_memop[value->id];

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
 */
static void kill_all_loads(void) {
	unsigned pos = 0;
	unsigned end = env.n_mem_ops * 2 - 1;

	for (pos = rbitset_next(env.curr_set, pos, 1); pos != end; pos = rbitset_next(env.curr_set, pos + 1, 1)) {
		memop_t *op = env.curr_id_2_memop[pos];

		if (! is_Store(op->node))
			rbitset_clear(env.curr_set, pos);
	}
}

/**
 * Kill all Stores from the current set.
 */
static void kill_all_stores(void) {
	unsigned pos = 0;
	unsigned end = env.n_mem_ops * 2 - 1;

	for (pos = rbitset_next(env.curr_set, pos, 1); pos != end; pos = rbitset_next(env.curr_set, pos + 1, 1)) {
		memop_t *op = env.curr_id_2_memop[pos];

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
 * @param value  the Load value
 */
static void kill_stores(const value_t *value) {
	unsigned pos = 0;
	unsigned end = env.n_mem_ops * 2 - 1;

	for (pos = rbitset_next(env.curr_set, pos, 1); pos != end; pos = rbitset_next(env.curr_set, pos + 1, 1)) {
		memop_t *op = env.curr_id_2_memop[pos];

		if (is_Store(op->node)) {
			if (ir_no_alias != get_alias_relation(current_ir_graph, value->address, value->mode,
			                                      op->value.address, op->value.mode)) {
				rbitset_clear(env.curr_set, pos);
				env.curr_id_2_memop[pos] = NULL;
			}
		}
	}
}

/**
 * Kill memops that are not alias free due to a Store value from the current set.
 *
 * @param value  the Store value
 */
static void kill_memops(const value_t *value) {
	unsigned pos = 0;
	unsigned end = env.n_mem_ops * 2 - 1;

	for (pos = rbitset_next(env.curr_set, pos, 1); pos != end; pos = rbitset_next(env.curr_set, pos + 1, 1)) {
		memop_t *op = env.curr_id_2_memop[pos];

		if (ir_no_alias != get_alias_relation(current_ir_graph, value->address, value->mode,
			                                  op->value.address, op->value.mode)) {
			rbitset_clear(env.curr_set, pos);
			env.curr_id_2_memop[pos] = NULL;
		}
	}
}

/**
 * Add the value of a memop to the current set.
 *
 * @param op  the memory op
 */
static void add_memop(memop_t *op) {
	rbitset_set(env.curr_set, op->value.id);
	env.curr_id_2_memop[op->value.id] = op;
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
 * Update a value of a memop to the avail_out set.
 *
 * @param bl  the block
 * @param op  the memory op
 */
static void update_memop_avail(block_t *bl, memop_t *op) {
	if (rbitset_is_set(bl->avail_out, op->value.id))
		bl->id_2_memop_avail[op->value.id] = op;
}

/**
 * Add a Conv if needed.
 */
static ir_node *conv_to(ir_node *irn, ir_mode *mode) {
	ir_mode *other = get_irn_mode(irn);
	if (other != mode) {
		/* different modes: check if conversion is possible without changing the bits */
		if (get_mode_arithmetic(mode) == irma_twos_complement &&
		    get_mode_arithmetic(other) == irma_twos_complement &&
			get_mode_size_bits(mode) == get_mode_size_bits(other)) {
			ir_node *block = get_nodes_block(irn);
			return new_r_Conv(current_ir_graph, block, irn, mode);
		}
		/* otherwise not possible ... yet */
		return NULL;
	}
	return irn;
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

/**
 * Do forward dataflow analysis on the given block and calculate the
 * GEN and KILL in the current (avail) set.
 *
 * @param bl  the block
 */
static void calc_gen_kill_avail(block_t *bl) {
	memop_t *op;
	ir_node *def;

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
				memop_t *other = find_address(&op->value);
				if (other != NULL && other != op) {
					def = conv_to(other->value.value, op->value.mode);
					if (def != NULL) {
#ifdef DEBUG_libfirm
						if (is_Store(other->node)) {
							/* RAW */
							DB((dbg, LEVEL_1, "RAW %+F <- %+F(%+F)\n", op->node, def, other->node));
						} else {
							/* RAR */
							DB((dbg, LEVEL_1, "RAR %+F <- %+F(%+F)\n", op->node, def, other->node));
						}
#endif
						mark_replace_load(op, def);
					} else {
						/* overwrite it */
						add_memop(op);
					}
				} else {
					/* add this value */
					kill_stores(&op->value);
					add_memop(op);
				}
			}
			break;
		case iro_Store:
			if (! (op->flags & (FLAG_KILLED_NODE|FLAG_IGNORE))) {
				/* do we have this store already */
				memop_t *other = find_address(&op->value);
				if (other != NULL) {
					if (is_Store(other->node)) {
						if (op != other && get_nodes_block(other->node) == get_nodes_block(op->node)) {
							/*
							 * A WAW in the same block we can kick the first store.
							 * This is a shortcut: we know that the second Store will be anticipated
							 * then in an case.
							 */
							DB((dbg, LEVEL_1, "WAW %+F <- %+F\n", op->node, other->node));
							mark_remove_store(other);
							/* FIXME: a Load might be get freed due to this killed store */
						}
					} else if (other->value.value == op->value.value) {
						/* WAR */
						DB((dbg, LEVEL_1, "WAR %+F <- %+F\n", op->node, other->node));
						mark_remove_store(op);
					} else {
						/* we overwrite the value that was loaded */
						add_memop(op);
					}
				} else {
					/* add this value */
					kill_memops(&op->value);
					add_memop(op);
				}
			}
			break;
		default:
			switch (op->flags & (FLAG_KILL_LOADS|FLAG_KILL_STORES)) {
			case FLAG_KILL_LOADS|FLAG_KILL_STORES:
				kill_all();
				break;
			case FLAG_KILL_LOADS:
				kill_all_loads();
				break;
			case FLAG_KILL_STORES:
				kill_all_stores();
				break;
			case 0:
				break;
			}
		}
	}
}

#define BYTE_SIZE(x)  (((x) + 7) >> 3)

/**
 * Do forward dataflow analysis on a given block to calculate the avail_out set
 * for this block only.
 *
 * @param block  the block
 */
static void forward_avail(block_t *bl) {
	/* fill the data from the current block */
	env.curr_id_2_memop = bl->id_2_memop_avail;
	env.curr_set        = bl->avail_out;

	calc_gen_kill_avail(bl);
	dump_curr(bl, "Avail_out");
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
	int     n = get_Block_n_cfg_outs(bl->block);

	if (n >= 1) {
		ir_node *succ    = get_Block_cfg_out(bl->block, 0);
		block_t *succ_bl = get_block_entry(succ);
		int i;

		rbitset_cpy(env.curr_set, succ_bl->anticL_in, env.rbs_size);
		memcpy(env.curr_id_2_memop, succ_bl->id_2_memop_antic, env.rbs_size * sizeof(env.curr_id_2_memop[0]));

		for (i = n - 1; i > 0; --i) {
			ir_node *succ    = get_Block_cfg_out(bl->block, i);
			block_t *succ_bl = get_block_entry(succ);

			rbitset_and(env.curr_set, succ_bl->anticL_in, env.rbs_size);
		}
	} else {
		/* block ends with a noreturn call */
		kill_all();
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
				add_memop(op);
			}
			break;
		case iro_Store:
			if (! (op->flags & (FLAG_KILLED_NODE|FLAG_IGNORE))) {
				/* a Store: check which memops must be killed */
				kill_memops(&op->value);
			}
			break;
		default:
			switch (op->flags & (FLAG_KILL_LOADS|FLAG_KILL_STORES)) {
			case FLAG_KILL_LOADS|FLAG_KILL_STORES:
				kill_all();
				break;
			case FLAG_KILL_LOADS:
				kill_all_loads();
				break;
			case FLAG_KILL_STORES:
				/*kill_all_stores();*/
				break;
			case 0:
				break;
			}
		}
	}

	memcpy(bl->id_2_memop_antic, env.curr_id_2_memop, env.rbs_size * sizeof(env.curr_id_2_memop[0]));
	if (! rbitset_equal(bl->anticL_in, env.curr_set, env.rbs_size)) {
		/* changed */
		rbitset_cpy(bl->anticL_in, env.curr_set, env.rbs_size);
		dump_curr(bl, "AnticL_in*");
		return 1;
	}
	dump_curr(bl, "AnticL_in");
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
	ir_node *proj;
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

	if (op->mem != NULL) {
		/* in rare cases a Load might have NO memory */
		exchange(op->mem, get_Load_mem(load));
	}
	proj = op->projs[pn_Load_res];
	if (proj != NULL) {
		mode = get_irn_mode(proj);
		if (get_irn_mode(def) != mode) {
			/* a hidden cast */
			dbg_info *db    = get_irn_dbg_info(load);
			ir_node  *block = get_nodes_block(proj);
			def = new_rd_Conv(db, current_ir_graph, block, def, mode);
		}
		exchange(proj, def);
	}
	proj = op->projs[pn_Load_X_except];
	if (proj != NULL) {
		exchange(proj, new_Bad());
	}
	proj = op->projs[pn_Load_X_regular];
	if (proj != NULL) {
		exchange(proj, new_r_Jmp(current_ir_graph, get_nodes_block(load)));
	}
}

/**
 * Remove a Store memop.
 *
 * @param op  the Store memop
 */
static void remove_store(memop_t *op) {
	ir_node *store = op->node;
	ir_node *proj;

	DB((dbg, LEVEL_1, "Removing %+F\n", store));

	if (op->mem != NULL) {
		/* in rare cases a Store might have no memory */
		exchange(op->mem, get_Store_mem(store));
	}
	proj = op->projs[pn_Store_X_except];
	if (proj != NULL) {
		exchange(proj, new_Bad());
	}
	proj = op->projs[pn_Store_X_regular];
	if (proj != NULL) {
		exchange(proj, new_r_Jmp(current_ir_graph, get_nodes_block(store)));
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
	memop_t  **tmp_memop = env.curr_id_2_memop;
	unsigned *tmp_set    = env.curr_set;
	block_t  *bl;

	/* calculate avail_out */
	DB((dbg, LEVEL_2, "Calculate Avail_out\n"));

	/* iterate over all blocks in in any order, skip the start block */
	for (bl = env.forward->forward_next; bl != NULL; bl = bl->forward_next) {
		forward_avail(bl);
	}

	/* restore the current sets */
	env.curr_id_2_memop = tmp_memop;
	env.curr_set        = tmp_set;
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
 * Reroute all memory users of old memory
 * to a new memory IR-node.
 *
 * @param omem  the old memory IR-node
 * @param nmem  the new memory IR-node
 */
static void reroute_all_mem_users(ir_node *omem, ir_node *nmem) {
	int i;

	for (i = get_irn_n_outs(omem) - 1; i >= 0; --i) {
		int     n_pos;
		ir_node *user = get_irn_out_ex(omem, i, &n_pos);

		set_irn_n(user, n_pos, nmem);
	}

	/* all edges previously point to omem now point to nmem */
	nmem->out = omem->out;
}

/**
 * Reroute memory users of old memory that are dominated by a given block
 * to a new memory IR-node.
 *
 * @param omem     the old memory IR-node
 * @param nmem     the new memory IR-node
 * @param pass_bl  the block the memory must pass
 */
static void reroute_mem_through(ir_node *omem, ir_node *nmem, ir_node *pass_bl) {
	int             i, j, n = get_irn_n_outs(omem);
	ir_def_use_edge *edges = NEW_ARR_D(ir_def_use_edge, &env.obst, n + 1);

	for (i = j = 0; i < n; ++i) {
		int     n_pos;
		ir_node *user   = get_irn_out_ex(omem, i, &n_pos);
		ir_node *use_bl = get_nodes_block(user);


		if (is_Phi(user)) {
			use_bl = get_Block_cfgpred_block(use_bl, n_pos);
		}
		if (block_dominates(pass_bl, use_bl)) {
			/* found an user that is dominated */
			++j;
			edges[j].pos = n_pos;
			edges[j].use = user;

			set_irn_n(user, n_pos, nmem);
		}
	}

	/* Modify the out structure: we create a new out edge array on our
	   temporary obstack here. This should be no problem, as we invalidate the edges
	   at the end either. */
	/* first entry is used for the length */
	edges[0].pos = j;
	nmem->out = edges;
}

/**
 * insert
 */
static int insert_Load(block_t *bl) {
	ir_node  *block = bl->block;
	int      i, n = get_Block_n_cfgpreds(block);
	unsigned pos = 0;
	unsigned end = env.n_mem_ops * 2 - 1;
	int      res = 0;
	ir_node  *pred    = get_Block_cfgpred_block(bl->block, 0);
	block_t  *pred_bl = get_block_entry(pred);

	DB((dbg, LEVEL_3, "processing %+F\n", block));

	rbitset_cpy(env.curr_set, pred_bl->avail_out, env.rbs_size);

	if (n > 1) {
		int     i, pos;
		ir_node **ins;

		NEW_ARR_A(ir_node *, ins, n);

		/* more than one predecessors, calculate the join for all avail_outs */
		for (i = n - 1; i > 0; --i) {
			ir_node *pred    = skip_Proj(get_Block_cfgpred(block, i));
			block_t *pred_bl = get_block_entry(get_nodes_block(pred));

			rbitset_and(env.curr_set, pred_bl->avail_out, env.rbs_size);

			if (is_Load(pred) || is_Store(pred)) {
				/* We reached this block by an exception from a Load or Store:
				 * the memop creating the exception was NOT completed than, kill it
				 */
				memop_t *exc_op = get_irn_memop(pred);
				rbitset_clear(env.curr_set, exc_op->value.id);
			}

		}
		/*
		 * Ensure that all values are in the map: build Phi's if necessary:
		 * Note: the last bit is the sentinel and ALWAYS set, so start with -2.
		 */
		for (pos = env.rbs_size - 2; pos >= 0; --pos) {
			if (! rbitset_is_set(env.curr_set, pos))
				env.curr_id_2_memop[pos] = NULL;
			else {
				ir_node *pred    = get_Block_cfgpred_block(bl->block, 0);
				block_t *pred_bl = get_block_entry(pred);
				int     need_phi = 0;
				ir_mode *mode;
				memop_t *first;

				first  = pred_bl->id_2_memop_avail[pos];
				ins[0] = first->value.value;
				mode = get_irn_mode(ins[0]);

				for (i = 1; i < n; ++i) {
					pred    = get_Block_cfgpred_block(bl->block, i);
					pred_bl = get_block_entry(pred);

					ins[i] = conv_to(pred_bl->id_2_memop_avail[pos]->value.value, mode);
					if (ins[i] != ins[0]) {
						need_phi = 1;
						if (ins[i] == NULL) {
							/* conversion failed */
							need_phi = 2;
							break;
						}
					}

				}
				switch (need_phi) {
				case 0:
					/* no Phi needed */
					env.curr_id_2_memop[pos] = first;
					break;
				case 1: {
					/* build a Phi  */
					ir_node *phi = new_r_Phi(current_ir_graph, bl->block, n, ins, mode);
					memop_t *phiop = alloc_memop(phi);

					phiop->value = first->value;
					phiop->value.value = phi;

					/* no need to link it in, as it is a DATA phi */

					env.curr_id_2_memop[pos] = phiop;

					DB((dbg, LEVEL_3, "Created new %+F on merging value for address %+F\n", phi, first->value.address));
					break;
				}
				default:
					/* not possible because of different modes, delete the entry */
					rbitset_clear(env.curr_set, pos);
					break;
				}
			}
		}
	} else {
		/* only one predecessor, simply copy the map */
		memcpy(env.curr_id_2_memop, pred_bl->id_2_memop_avail, env.rbs_size * sizeof(bl->id_2_memop_avail[0]));
	}


	/* recalculate avail by gen and kill */
	calc_gen_kill_avail(bl);

	if (!rbitset_equal(bl->avail_out, env.curr_set, env.rbs_size)) {
		/* the avail set has changed */
		rbitset_cpy(bl->avail_out, env.curr_set, env.rbs_size);
		memcpy(bl->id_2_memop_avail, env.curr_id_2_memop, env.rbs_size * sizeof(env.curr_id_2_memop[0]));
		dump_curr(bl, "Avail_out*");
		res = 1;
	}

	if (n <= 1)
		return res;

	/* check for partly redundant values */
	for (pos = rbitset_next(bl->anticL_in, pos, 1); pos != end; pos = rbitset_next(bl->anticL_in, pos + 1, 1)) {
		memop_t *op = bl->id_2_memop_antic[pos];
		int     have_some, all_same;
		ir_node *first;

		assert(is_Load(op->node));

		if (op->flags & FLAG_KILLED_NODE)
			continue;
		DB((dbg, LEVEL_3, "anticipated %+F\n", op->node));

		have_some  = 0;
		all_same   = 1;
		first      = 0;
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
				DB((dbg, LEVEL_3, "%+F is not available in predecessor %+F\n", op->node, pred));
				pred_bl->avail = NULL;
				all_same       = 0;
			} else {
				pred_bl->avail = e;
				have_some      = 1;
				DB((dbg, LEVEL_3, "%+F is available for %+F in predecessor %+F\n", e->node, op->node, pred));
				if (first == NULL)
					first = e->node;
				else if (first != e->node)
					all_same = 0;
			}
		}
		if (have_some && !all_same) {
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

					assert(last_mem != NULL);
					load = new_rd_Load(db, current_ir_graph, pred, last_mem, op->value.address, mode, cons_none);
					def  = new_r_Proj(current_ir_graph, pred, load, mode, pn_Load_res);
					DB((dbg, LEVEL_1, "Created new %+F in %+F for party redundant %+F\n", load, pred, op->node));

					new_op                = alloc_memop(load);
					new_op->mem           = new_r_Proj(current_ir_graph, pred, load, mode_M, pn_Load_M);
					new_op->value.address = op->value.address;
					new_op->value.id      = op->value.id;
					new_op->value.mode    = mode;
					new_op->value.value   = def;

					new_op->projs[pn_Load_M]   = new_op->mem;
					new_op->projs[pn_Load_res] = def;

					new_op->prev            = pred_bl->memop_backward;
					pred_bl->memop_backward = new_op;

					if (pred_bl->memop_forward == NULL)
						pred_bl->memop_forward = new_op;

					if (get_nodes_block(last_mem) == pred) {
						/* We have add a new last memory op in pred block.
						   If pred had already a last mem, reroute all memory
						   users. */
						reroute_all_mem_users(last_mem, new_op->mem);
					} else {
						/* reroute only those memory going through the pre block */
						reroute_mem_through(last_mem, new_op->mem, pred);
					}

					/* we added this load at the end, so it will be avail anyway */
					add_memop_avail(pred_bl, new_op);
					pred_bl->avail = new_op;
				}
				in[i] = pred_bl->avail->value.value;
			}
			phi = new_r_Phi(current_ir_graph, block, n, in, mode);
			DB((dbg, LEVEL_1, "Created new %+F in %+F for now redundant %+F\n", phi, block, op->node));

			if (get_nodes_block(op->node) == block) {
				/* The redundant node was in the current block:
				   In that case, DO NOT update avail_out. If it was NOT
				   avail although it is executed in this bLock, it is killed by a later
				   instruction.
				 */
				memop_t *phi_op = clone_memop_phi(op, phi);

				update_memop_avail(bl, phi_op);

				mark_replace_load(op, phi);
			} else {
				/* The redundant node is NOT in the current block and anticipated. */
				memop_t *phi_op = clone_memop_phi(op, phi);

				add_memop_avail(bl, phi_op);

				/* propagate it downwards */
				res = 1;
			}
			/* clear it so we do not found it the next iteration */
			rbitset_clear(bl->anticL_in, pos);
		}
	}
	return res;
}

/**
 * Insert Loads upwards.
 */
static void insert_Loads_upwards(void) {
	int i, need_iter;
	block_t *bl;

	/* recalculate antic_out and insert Loads */
	DB((dbg, LEVEL_2, "Inserting Loads\n"));

	i = 0;
	do {
		DB((dbg, LEVEL_2, "Iteration %d:\n=========\n", i));

		need_iter = 0;

		/* over all blocks in reverse post order, skip the start block */
		for (bl = env.forward->forward_next; bl != NULL; bl = bl->forward_next) {
			need_iter |= insert_Load(bl);
		}
		++i;
	} while (need_iter);

	DB((dbg, LEVEL_2, "Finished Load inserting after %d iterations\n", i));
}

int opt_ldst(ir_graph *irg) {
	block_t  *bl;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;

	FIRM_DBG_REGISTER(dbg, "firm.opt.ldst");
//	firm_dbg_set_mask(dbg, -1);

	DB((dbg, LEVEL_1, "\nDoing Load/Store optimization on %+F\n", irg));

	/* we need landing pads */
	remove_critical_cf_edges(irg);

	dump_ir_block_graph(irg, "-XXX");

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
	env.end_bl      = get_irg_end_block(irg);

	assure_doms(irg);
	assure_irg_outs(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);

	/* first step: allocate block entries: produces an
	   inverse post-order list for the CFG */
	set_irn_link(env.end_bl, NULL);
	irg_out_block_walk(get_irg_start_block(irg), NULL, prepare_blocks, NULL);

	if (get_block_entry(env.end_bl) == NULL) {
		/*
		 * The end block is NOT reachable due to endless loops
		 * or no_return calls. Ensure that it is initialized.
		 * Note that this places the entry for the end block first, so we must fix this.
		 * env.backwards points to th last block for this purpose.
		 */
		prepare_blocks(env.end_bl, NULL);

		bl               = env.forward;
		env.forward      = bl->forward_next;
		bl->forward_next = NULL;

		env.backward->forward_next = bl;
	}

	/* second step: find and sort all memory ops */
	walk_memory_irg(irg, collect_memops, NULL, NULL);

	if (env.n_mem_ops == 0) {
		/* no memory ops */
		goto end;
	}

	/* create the backward links */
	env.backward = NULL;
	irg_block_walk_graph(irg, NULL, collect_backward, NULL);

	/* link the end block in */
	bl = get_block_entry(env.end_bl);
	bl->backward_next = env.backward;
	env.backward      = bl;

	/* check that we really start with the start / end block */
	assert(env.forward->block  == env.start_bl);
	assert(env.backward->block == env.end_bl);

	/* create address sets: we know that 2 * n_mem_ops - 1 is an upper bound for all possible addresses */
	env.rbs_size = 2 * env.n_mem_ops;

	/* create the current set */
	env.curr_set = rbitset_obstack_alloc(&env.obst, env.rbs_size);
	rbitset_set(env.curr_set, env.rbs_size - 1);
	env.curr_id_2_memop = NEW_ARR_D(memop_t *, &env.obst, env.rbs_size);
	memset(env.curr_id_2_memop, 0, env.rbs_size * sizeof(env.curr_id_2_memop[0]));

	for (bl = env.forward; bl != NULL; bl = bl->forward_next) {
		/* set sentinel bits */
		bl->avail_out  = rbitset_obstack_alloc(&env.obst, env.rbs_size);
		rbitset_set(bl->avail_out, env.rbs_size - 1);

		bl->id_2_memop_avail = NEW_ARR_D(memop_t *, &env.obst, env.rbs_size);
		memset(bl->id_2_memop_avail, 0, env.rbs_size * sizeof(bl->id_2_memop_avail[0]));

		bl->anticL_in  = rbitset_obstack_alloc(&env.obst, env.rbs_size);
		rbitset_set(bl->anticL_in, env.rbs_size - 1);

		bl->id_2_memop_antic = NEW_ARR_D(memop_t *, &env.obst, env.rbs_size);
		memset(bl->id_2_memop_antic, 0, env.rbs_size * sizeof(bl->id_2_memop_antic[0]));
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

		/* not only invalidate but free them. We might allocate new out arrays
		   on our obstack which will be deleted yet. */
		free_irg_outs(irg);
		set_irg_entity_usage_state(irg, ir_entity_usage_not_computed);
	}
end:

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	ir_nodemap_destroy(&env.adr_map);
	obstack_free(&env.obst, NULL);

	dump_ir_block_graph(irg, "-YYY");

	current_ir_graph = rem;

	return env.changed != 0;
}
