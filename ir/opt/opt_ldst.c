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
#include "iropt.h"
#include "iroptimize.h"
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
	FLAG_KILL_ALL    = 1, /**< KILL all addresses */
	FLAG_KILLED_NODE = 2, /**< this node was killed */
	FLAG_EXCEPTION   = 4, /**< this node has exception flow */
	FLAG_IGNORE      = 8, /**< ignore this node (volatile or other) */
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
	memop_t  **trans_results;    /**< used to cached translated nodes due antic calculation. */
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
	int             max_cfg_preds;     /**< maximum number of block cfg predecessors */
	int             changed;           /**< Flags for changed graph state */
#ifdef DEBUG_libfirm
	ir_node         **id_2_address;    /**< maps an id to the used address */
#endif
} ldst_env;

/* the one and only environment */
static ldst_env env;

#ifdef DEBUG_libfirm

static firm_dbg_module_t *dbg;

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
			else if (op->flags & FLAG_KILL_ALL)
				DB((dbg, LEVEL_2, "K"));
			DB((dbg, LEVEL_2, ", "));

			i = (i + 1) & 3;
		}
		DB((dbg, LEVEL_2, "\n}\n\n"));
	}
}  /* dump_block_list */

/**
 * Dumps the current set.
 *
 * @param bl   current block
 * @param s    name of the set
 */
static void dump_curr(block_t *bl, const char *s) {
	unsigned end = env.rbs_size - 1;
	unsigned pos;
	int      i;

	DB((dbg, LEVEL_2, "%s[%+F] = {", s, bl->block));
	i = 0;
	for (pos = rbitset_next(env.curr_set, 0, 1); pos < end; pos = rbitset_next(env.curr_set, pos + 1, 1)) {
		memop_t *op = env.curr_id_2_memop[pos];

		if (i == 0) {
			DB((dbg, LEVEL_2, "\n\t"));
		}

		DB((dbg, LEVEL_2, "<%+F, %+F>, ", op->value.address, op->value.value));
		i = (i + 1) & 3;
	}
	DB((dbg, LEVEL_2, "\n}\n"));
}  /* dump_curr */

#else
static void dump_block_list(ldst_env *env) {
	(void) env;
}
static void dump_curr(block_t *bl, const char *s) {
	(void) bl;
	(void) s;
}
#endif /* DEBUG_libfirm */

/** Get the block entry for a block node */
static block_t *get_block_entry(const ir_node *block) {
	assert(is_Block(block));

	return get_irn_link(block);
}  /* get_block_entry */

/** Get the memop entry for a memory operation node */
static memop_t *get_irn_memop(const ir_node *irn) {
	assert(! is_Block(irn));
	return get_irn_link(irn);
}  /* get_irn_memop */

/**
 * Walk over the memory edges from definition to users.
 * This ensures, that even operation without memory output are found.
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
}  /* walk_memory */

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
}  /* walk_memory_irg */

/**
 * Register an address and allocate a (sparse, 0..n) ID for it.
 *
 * @param adr  the IR-node representing the address
 *
 * @return the allocated id
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
#ifdef DEBUG_libfirm
		ARR_APP1(ir_node *, env.id_2_address, adr);
#endif
	}
	return entry->id;
}  /* register_address */


/**
 * translate an address through a Phi node into a given predecessor
 * block.
 *
 * @param address  the address
 * @param block    the block
 * @param pos      the position of the predecessor in block
 */
static ir_node *phi_translate(ir_node *address, const ir_node *block, int pos) {
	if (is_Phi(address) && get_nodes_block(address) == block)
		address = get_Phi_pred(address, pos);
	return address;
}  /* phi_translate */

/**
 * Walker: allocate an block entry for every block
 * and register all potential addresses.
 */
static void prepare_blocks(ir_node *irn, void *ctx) {
	(void)ctx;

	if (is_Block(irn)) {
		block_t *entry = obstack_alloc(&env.obst, sizeof(*entry));
		int     n;

		entry->memop_forward    = NULL;
		entry->memop_backward   = NULL;
		entry->avail_out        = NULL;
		entry->id_2_memop_avail = NULL;
		entry->anticL_in        = NULL;
		entry->id_2_memop_antic = NULL;
		entry->block            = irn;
		entry->forward_next     = NULL;
		entry->backward_next    = NULL;
		entry->avail            = NULL;
		entry->trans_results    = NULL;
		set_irn_link(irn, entry);

		set_Block_phis(irn, NULL);

		/* use block marks to track unreachable blocks */
		set_Block_mark(irn, 0);

		n = get_Block_n_cfgpreds(irn);
		if (n > env.max_cfg_preds)
			env.max_cfg_preds = n;
	} else {
		ir_mode *mode = get_irn_mode(irn);

		if (mode_is_reference(mode)) {
			/*
			 * Register ALL possible addresses: this is overkill yet but
			 * simpler then doing it for all possible translated addresses
			 * (which would be sufficient in the moment.
			 */
			(void)register_address(irn);
		}
	}
}  /* prepare_blocks */

/**
 * Post-Walker, link in all Phi's
 */
static void link_phis(ir_node *irn, void *ctx) {
	(void)ctx;

	if (is_Phi(irn)) {
		ir_node *block = get_nodes_block(irn);
		add_Block_phi(block, irn);
	}
}  /* link_phis */

/**
 * Block walker: creates the inverse post-order list for the CFG.
 */
static void inverse_post_order(ir_node *block, void *ctx) {
	block_t *entry = get_block_entry(block);

	(void)ctx;

	/* mark this block IS reachable from start */
	set_Block_mark(block, 1);

	/* create the list in inverse order */
	entry->forward_next = env.forward;
	env.forward         = entry;

	/* remember the first visited (last in list) entry, needed for later */
	if (env.backward == NULL)
		env.backward = entry;
}  /* inverse_post_order */

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
}  /* collect_backward */

/**
 * Allocate a memop.
 *
 * @param irn  the IR-node representing the memop or NULL
 *             if this is a translated (virtual) memop
 *
 * @return the allocated memop
 */
static memop_t *alloc_memop(ir_node *irn) {
	memop_t *m = obstack_alloc(&env.obst, sizeof(*m));

	m->value.address = NULL;
	m->value.value   = NULL;
	m->value.mode    = NULL;

	m->node          = irn;
	m->mem           = NULL;
	m->replace       = NULL;
	m->next          = NULL;
	m->flags         = 0;

	memset(m->projs, 0, sizeof(m->projs));

	if (irn != NULL)
		set_irn_link(irn, m);
	return m;
}  /* alloc_memop */

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
}  /* clone_memop_phi */

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
}  /* get_Call_memory_properties */

/**
 * Returns an entity if the address ptr points to a constant one.
 *
 * @param ptr  the address
 *
 * @return an entity or NULL
 */
static ir_entity *find_constant_entity(ir_node *ptr) {
	for (;;) {
		if (is_SymConst(ptr) && get_SymConst_kind(ptr) == symconst_addr_ent) {
			return get_SymConst_entity(ptr);
		} else if (is_Sel(ptr)) {
			ir_entity *ent = get_Sel_entity(ptr);
			ir_type   *tp  = get_entity_owner(ent);

			/* Do not fiddle with polymorphism. */
			if (is_Class_type(get_entity_owner(ent)) &&
				((get_entity_n_overwrites(ent)    != 0) ||
				(get_entity_n_overwrittenby(ent) != 0)   ) )
				return NULL;

			if (is_Array_type(tp)) {
				/* check bounds */
				int i, n;

				for (i = 0, n = get_Sel_n_indexs(ptr); i < n; ++i) {
					ir_node *bound;
					tarval *tlower, *tupper;
					ir_node *index = get_Sel_index(ptr, i);
					tarval *tv     = computed_value(index);

					/* check if the index is constant */
					if (tv == tarval_bad)
						return NULL;

					bound  = get_array_lower_bound(tp, i);
					tlower = computed_value(bound);
					bound  = get_array_upper_bound(tp, i);
					tupper = computed_value(bound);

					if (tlower == tarval_bad || tupper == tarval_bad)
						return NULL;

					if (tarval_cmp(tv, tlower) & pn_Cmp_Lt)
						return NULL;
					if (tarval_cmp(tupper, tv) & pn_Cmp_Lt)
						return NULL;

					/* ok, bounds check finished */
				}
			}

			if (variability_constant == get_entity_variability(ent))
				return ent;

			/* try next */
			ptr = get_Sel_ptr(ptr);
		} else if (is_Add(ptr)) {
			ir_node *l = get_Add_left(ptr);
			ir_node *r = get_Add_right(ptr);

			if (get_irn_mode(l) == get_irn_mode(ptr) && is_Const(r))
				ptr = l;
			else if (get_irn_mode(r) == get_irn_mode(ptr) && is_Const(l))
				ptr = r;
			else
				return NULL;

			/* for now, we support only one addition, reassoc should fold all others */
			if (! is_SymConst(ptr) && !is_Sel(ptr))
				return NULL;
		} else if (is_Sub(ptr)) {
			ir_node *l = get_Sub_left(ptr);
			ir_node *r = get_Sub_right(ptr);

			if (get_irn_mode(l) == get_irn_mode(ptr) &&	is_Const(r))
				ptr = l;
			else
				return NULL;
			/* for now, we support only one subtraction, reassoc should fold all others */
			if (! is_SymConst(ptr) && !is_Sel(ptr))
				return NULL;
		} else
			return NULL;
	}
}  /* find_constant_entity */

/**
 * Return the Selection index of a Sel node from dimension n
 */
static long get_Sel_array_index_long(ir_node *n, int dim) {
	ir_node *index = get_Sel_index(n, dim);
	assert(is_Const(index));
	return get_tarval_long(get_Const_tarval(index));
}  /* get_Sel_array_index_long */

/**
 * Returns the accessed component graph path for an
 * node computing an address.
 *
 * @param ptr    the node computing the address
 * @param depth  current depth in steps upward from the root
 *               of the address
 */
static compound_graph_path *rec_get_accessed_path(ir_node *ptr, int depth) {
	compound_graph_path *res = NULL;
	ir_entity           *root, *field, *ent;
	int                 path_len, pos, idx;
	tarval              *tv;
	ir_type             *tp;

	if (is_SymConst(ptr)) {
		/* a SymConst. If the depth is 0, this is an access to a global
		 * entity and we don't need a component path, else we know
		 * at least its length.
		 */
		assert(get_SymConst_kind(ptr) == symconst_addr_ent);
		root = get_SymConst_entity(ptr);
		res = (depth == 0) ? NULL : new_compound_graph_path(get_entity_type(root), depth);
	} else if (is_Sel(ptr)) {
		/* it's a Sel, go up until we find the root */
		res = rec_get_accessed_path(get_Sel_ptr(ptr), depth+1);
		if (res == NULL)
			return NULL;

		/* fill up the step in the path at the current position */
		field    = get_Sel_entity(ptr);
		path_len = get_compound_graph_path_length(res);
		pos      = path_len - depth - 1;
		set_compound_graph_path_node(res, pos, field);

		if (is_Array_type(get_entity_owner(field))) {
			assert(get_Sel_n_indexs(ptr) == 1 && "multi dim arrays not implemented");
			set_compound_graph_path_array_index(res, pos, get_Sel_array_index_long(ptr, 0));
		}
	} else if (is_Add(ptr)) {
		ir_node *l    = get_Add_left(ptr);
		ir_node *r    = get_Add_right(ptr);
		ir_mode *mode = get_irn_mode(ptr);
		tarval  *tmp;

		if (is_Const(r) && get_irn_mode(l) == mode) {
			ptr = l;
			tv  = get_Const_tarval(r);
		} else {
			ptr = r;
			tv  = get_Const_tarval(l);
		}
ptr_arith:
		mode = get_tarval_mode(tv);
		tmp  = tv;

		/* ptr must be a Sel or a SymConst, this was checked in find_constant_entity() */
		if (is_Sel(ptr)) {
			field = get_Sel_entity(ptr);
		} else {
			field = get_SymConst_entity(ptr);
		}
		idx = 0;
		for (ent = field;;) {
			unsigned size;
			tarval   *sz, *tv_index, *tlower, *tupper;
			ir_node  *bound;

			tp = get_entity_type(ent);
			if (! is_Array_type(tp))
				break;
			ent = get_array_element_entity(tp);
			size = get_type_size_bytes(get_entity_type(ent));
			sz   = new_tarval_from_long(size, mode);

			tv_index = tarval_div(tmp, sz);
			tmp      = tarval_mod(tmp, sz);

			if (tv_index == tarval_bad || tmp == tarval_bad)
				return NULL;

			assert(get_array_n_dimensions(tp) == 1 && "multiarrays not implemented");
			bound  = get_array_lower_bound(tp, 0);
			tlower = computed_value(bound);
			bound  = get_array_upper_bound(tp, 0);
			tupper = computed_value(bound);

			if (tlower == tarval_bad || tupper == tarval_bad)
				return NULL;

			if (tarval_cmp(tv_index, tlower) & pn_Cmp_Lt)
				return NULL;
			if (tarval_cmp(tupper, tv_index) & pn_Cmp_Lt)
				return NULL;

			/* ok, bounds check finished */
			++idx;
		}
		if (! tarval_is_null(tmp)) {
			/* access to some struct/union member */
			return NULL;
		}

		/* should be at least ONE array */
		if (idx == 0)
			return NULL;

		res = rec_get_accessed_path(ptr, depth + idx);
		if (res == NULL)
			return NULL;

		path_len = get_compound_graph_path_length(res);
		pos      = path_len - depth - idx;

		for (ent = field;;) {
			unsigned size;
			tarval   *sz, *tv_index;
			long     index;

			tp = get_entity_type(ent);
			if (! is_Array_type(tp))
				break;
			ent = get_array_element_entity(tp);
			set_compound_graph_path_node(res, pos, ent);

			size = get_type_size_bytes(get_entity_type(ent));
			sz   = new_tarval_from_long(size, mode);

			tv_index = tarval_div(tv, sz);
			tv       = tarval_mod(tv, sz);

			/* worked above, should work again */
			assert(tv_index != tarval_bad && tv != tarval_bad);

			/* bounds already checked above */
			index = get_tarval_long(tv_index);
			set_compound_graph_path_array_index(res, pos, index);
			++pos;
		}
	} else if (is_Sub(ptr)) {
		ir_node *l = get_Sub_left(ptr);
		ir_node *r = get_Sub_right(ptr);

		ptr = l;
		tv  = get_Const_tarval(r);
		tv  = tarval_neg(tv);
		goto ptr_arith;
	}
	return res;
}  /* rec_get_accessed_path */

/**
 * Returns an access path or NULL.  The access path is only
 * valid, if the graph is in phase_high and _no_ address computation is used.
 */
static compound_graph_path *get_accessed_path(ir_node *ptr) {
	compound_graph_path *gr = rec_get_accessed_path(ptr, 0);
	return gr;
}  /* get_accessed_path */

typedef struct path_entry {
	ir_entity         *ent;
	struct path_entry *next;
	long              index;
} path_entry;

static ir_node *rec_find_compound_ent_value(ir_node *ptr, path_entry *next) {
	path_entry       entry, *p;
	ir_entity        *ent, *field;
	ir_initializer_t *initializer;
	tarval           *tv;
	ir_type          *tp;
	unsigned         n;

	entry.next = next;
	if (is_SymConst(ptr)) {
		/* found the root */
		ent         = get_SymConst_entity(ptr);
		initializer = get_entity_initializer(ent);
		for (p = next; p != NULL;) {
			if (initializer->kind != IR_INITIALIZER_COMPOUND)
				return NULL;
			n  = get_initializer_compound_n_entries(initializer);
			tp = get_entity_type(ent);

			if (is_Array_type(tp)) {
				ent = get_array_element_entity(tp);
				if (ent != p->ent) {
					/* a missing [0] */
					if (0 >= n)
						return NULL;
					initializer = get_initializer_compound_value(initializer, 0);
					continue;
				}
			}
			if (p->index >= (int) n)
				return NULL;
			initializer = get_initializer_compound_value(initializer, p->index);

			ent = p->ent;
			p   = p->next;
		}
		tp = get_entity_type(ent);
		while (is_Array_type(tp)) {
			ent = get_array_element_entity(tp);
			tp = get_entity_type(ent);
			/* a missing [0] */
			n  = get_initializer_compound_n_entries(initializer);
			if (0 >= n)
				return NULL;
			initializer = get_initializer_compound_value(initializer, 0);
		}

		switch (initializer->kind) {
		case IR_INITIALIZER_CONST:
			return get_initializer_const_value(initializer);
		case IR_INITIALIZER_TARVAL:
		case IR_INITIALIZER_NULL:
		default:
			return NULL;
		}
	} else if (is_Sel(ptr)) {
		entry.ent = field = get_Sel_entity(ptr);
		tp = get_entity_owner(field);
		if (is_Array_type(tp)) {
			assert(get_Sel_n_indexs(ptr) == 1 && "multi dim arrays not implemented");
			entry.index = get_Sel_array_index_long(ptr, 0) - get_array_lower_bound_int(tp, 0);
		} else {
			int i, n_members = get_compound_n_members(tp);
			for (i = 0; i < n_members; ++i) {
				if (get_compound_member(tp, i) == field)
					break;
			}
			if (i >= n_members) {
				/* not found: should NOT happen */
				return NULL;
			}
			entry.index = i;
		}
		return rec_find_compound_ent_value(get_Sel_ptr(ptr), &entry);
	}  else if (is_Add(ptr)) {
		ir_node  *l = get_Add_left(ptr);
		ir_node  *r = get_Add_right(ptr);
		ir_mode  *mode;
		unsigned pos;

		if (is_Const(r)) {
			ptr = l;
			tv  = get_Const_tarval(r);
		} else {
			ptr = r;
			tv  = get_Const_tarval(l);
		}
ptr_arith:
		mode = get_tarval_mode(tv);

		/* ptr must be a Sel or a SymConst, this was checked in find_constant_entity() */
		if (is_Sel(ptr)) {
			field = get_Sel_entity(ptr);
		} else {
			field = get_SymConst_entity(ptr);
		}

		/* count needed entries */
		pos = 0;
		for (ent = field;;) {
			tp = get_entity_type(ent);
			if (! is_Array_type(tp))
				break;
			ent = get_array_element_entity(tp);
			++pos;
		}
		/* should be at least ONE entry */
		if (pos == 0)
			return NULL;

		/* allocate the right number of entries */
		NEW_ARR_A(path_entry, p, pos);

		/* fill them up */
		pos = 0;
		for (ent = field;;) {
			unsigned size;
			tarval   *sz, *tv_index, *tlower, *tupper;
			long     index;
			ir_node  *bound;

			tp = get_entity_type(ent);
			if (! is_Array_type(tp))
				break;
			ent = get_array_element_entity(tp);
			p[pos].ent  = ent;
			p[pos].next = &p[pos + 1];

			size = get_type_size_bytes(get_entity_type(ent));
			sz   = new_tarval_from_long(size, mode);

			tv_index = tarval_div(tv, sz);
			tv       = tarval_mod(tv, sz);

			if (tv_index == tarval_bad || tv == tarval_bad)
				return NULL;

			assert(get_array_n_dimensions(tp) == 1 && "multiarrays not implemented");
			bound  = get_array_lower_bound(tp, 0);
			tlower = computed_value(bound);
			bound  = get_array_upper_bound(tp, 0);
			tupper = computed_value(bound);

			if (tlower == tarval_bad || tupper == tarval_bad)
				return NULL;

			if (tarval_cmp(tv_index, tlower) & pn_Cmp_Lt)
				return NULL;
			if (tarval_cmp(tupper, tv_index) & pn_Cmp_Lt)
				return NULL;

			/* ok, bounds check finished */
			index = get_tarval_long(tv_index);
			p[pos].index = index;
			++pos;
		}
		if (! tarval_is_null(tv)) {
			/* hmm, wrong access */
			return NULL;
		}
		p[pos - 1].next = next;
		return rec_find_compound_ent_value(ptr, p);
	} else if (is_Sub(ptr)) {
		ir_node *l = get_Sub_left(ptr);
		ir_node *r = get_Sub_right(ptr);

		ptr = l;
		tv  = get_Const_tarval(r);
		tv  = tarval_neg(tv);
		goto ptr_arith;
	}
	return NULL;
}  /* rec_find_compound_ent_value */

static ir_node *find_compound_ent_value(ir_node *ptr) {
	return rec_find_compound_ent_value(ptr, NULL);
}  /* find_compound_ent_value */

/**
 * Mark a Load memop to be replace by a definition
 *
 * @param op  the Load memop
 */
static void mark_replace_load(memop_t *op, ir_node *def) {
	op->replace = def;
	op->flags |= FLAG_KILLED_NODE;
	env.changed = 1;
}  /* mark_replace_load */

/**
 * Mark a Store memop to be removed.
 *
 * @param op  the Store memop
 */
static void mark_remove_store(memop_t *op) {
	op->flags |= FLAG_KILLED_NODE;
	env.changed = 1;
}  /* mark_remove_store */

/**
 * Update a memop for a Load.
 *
 * @param m  the memop
 */
static void update_Load_memop(memop_t *m) {
	int       i;
	ir_node   *load = m->node;
	ir_node   *ptr;
	ir_entity *ent;

	if (get_Load_volatility(load) == volatility_is_volatile)
		m->flags |= FLAG_IGNORE;

	ptr = get_Load_ptr(load);

	m->value.address = ptr;

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

	/* check if we can determine the entity that will be loaded */
	ent = find_constant_entity(ptr);

	if (ent != NULL                                     &&
		allocation_static == get_entity_allocation(ent) &&
		visibility_external_allocated != get_entity_visibility(ent)) {
		/* a static allocation that is not external: there should be NO exception
		 * when loading even if we cannot replace the load itself. */
		ir_node *value = NULL;

		/* no exception, clear the m fields as it might be checked later again */
		if (m->projs[pn_Load_X_except]) {
			exchange(m->projs[pn_Load_X_except], new_Bad());
			m->projs[pn_Load_X_except] = NULL;
			m->flags &= ~FLAG_EXCEPTION;
			env.changed = 1;
		}
		if (m->projs[pn_Load_X_regular]) {
			exchange(m->projs[pn_Load_X_regular], new_r_Jmp(get_nodes_block(load)));
			m->projs[pn_Load_X_regular] = NULL;
			env.changed = 1;
		}

		if (variability_constant == get_entity_variability(ent)) {
			if (is_atomic_entity(ent)) {
				/* Might not be atomic after lowering of Sels.  In this case we
				 * could also load, but it's more complicated. */
				/* more simpler case: we load the content of a constant value:
				 * replace it by the constant itself */
				value = get_atomic_ent_value(ent);
			} else if (ent->has_initializer) {
				/* new style initializer */
				value = find_compound_ent_value(ptr);
			} else {
				/* old style initializer */
				compound_graph_path *path = get_accessed_path(ptr);

				if (path != NULL) {
					assert(is_proper_compound_graph_path(path, get_compound_graph_path_length(path)-1));

					value = get_compound_ent_value_by_path(ent, path);
					DB((dbg, LEVEL_1, "  Constant access at %F%F resulted in %+F\n", ent, path, value));
					free_compound_graph_path(path);
				}
			}
			if (value != NULL)
				value = can_replace_load_by_const(load, value);
		}

		if (value != NULL) {
			/* we completely replace the load by this value */
			DB((dbg, LEVEL_1, "Replacing Load %+F by constant %+F\n", m->node, value));
			mark_replace_load(m, value);
			return;
		}
	}

	if (m->value.value != NULL && !(m->flags & FLAG_IGNORE)) {
		/* only create an address if this node is NOT killed immediately or ignored */
		m->value.id = register_address(ptr);
		++env.n_mem_ops;
	} else {
		/* no user, KILL it */
		mark_replace_load(m, NULL);
	}
}  /* update_Load_memop */

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
}  /* update_Store_memop */

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
		m->flags = 0;
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
}  /* update_Call_memop */

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
}  /* update_DivOp_memop */

/**
 * Update a memop for a Phi.
 *
 * @param m  the memop
 */
static void update_Phi_memop(memop_t *m) {
	/* the Phi is it's own mem */
	m->mem = m->node;
}  /* update_Phi_memop */

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
}  /* collect_memops */

/**
 * Find an address in the current set.
 *
 * @param value  the value to be searched for
 *
 * @return a memop for the value or NULL if the value does
 *         not exists in the set or cannot be converted into
 *         the requested mode
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
}  /* find_address */

/**
 * Find an address in the avail_out set.
 *
 * @param bl     the block
 */
static memop_t *find_address_avail(const block_t *bl, unsigned id, const ir_mode *mode) {
	if (rbitset_is_set(bl->avail_out, id)) {
		memop_t *res = bl->id_2_memop_avail[id];

		if (res->value.mode == mode)
			return res;
		/* allow hidden casts */
		if (get_mode_arithmetic(res->value.mode) == irma_twos_complement &&
		    get_mode_arithmetic(mode) == irma_twos_complement &&
		    get_mode_size_bits(res->value.mode) == get_mode_size_bits(mode))
			return res;
	}
	return NULL;
}  /* find_address_avail */

/**
 * Kill all addresses from the current set.
 */
static void kill_all(void) {
	rbitset_clear_all(env.curr_set, env.rbs_size);

	/* set sentinel */
	rbitset_set(env.curr_set, env.rbs_size - 1);
}  /* kill_all */

/**
 * Kill memops that are not alias free due to a Store value from the current set.
 *
 * @param value  the Store value
 */
static void kill_memops(const value_t *value) {
	unsigned end = env.rbs_size - 1;
	unsigned pos;

	for (pos = rbitset_next(env.curr_set, 0, 1); pos < end; pos = rbitset_next(env.curr_set, pos + 1, 1)) {
		memop_t *op = env.curr_id_2_memop[pos];

		if (ir_no_alias != get_alias_relation(current_ir_graph, value->address, value->mode,
			                                  op->value.address, op->value.mode)) {
			rbitset_clear(env.curr_set, pos);
			env.curr_id_2_memop[pos] = NULL;
			DB((dbg, LEVEL_2, "KILLING %+F because of possible alias address %+F\n", op->node, value->address));
		}
	}
}  /* kill_memops */

/**
 * Add the value of a memop to the current set.
 *
 * @param op  the memory op
 */
static void add_memop(memop_t *op) {
	rbitset_set(env.curr_set, op->value.id);
	env.curr_id_2_memop[op->value.id] = op;
}  /* add_memop */

/**
 * Add the value of a memop to the avail_out set.
 *
 * @param bl  the block
 * @param op  the memory op
 */
static void add_memop_avail(block_t *bl, memop_t *op) {
	rbitset_set(bl->avail_out, op->value.id);
	bl->id_2_memop_avail[op->value.id] = op;
}  /* add_memop_avail */

/**
 * Check, if we can convert a value of one mode to another mode
 * without changing the representation of bits.
 *
 * @param from  the original mode
 * @param to    the destination mode
 */
static int can_convert_to(const ir_mode *from, const ir_mode *to) {
	if (get_mode_arithmetic(from) == irma_twos_complement &&
	    get_mode_arithmetic(to) == irma_twos_complement &&
	    get_mode_size_bits(from) == get_mode_size_bits(to))
		return 1;
	return 0;
}  /* can_convert_to */

/**
 * Add a Conv to the requested mode if needed.
 *
 * @param irn   the IR-node to convert
 * @param mode  the destination mode
 *
 * @return the possible converted node or NULL
 *         if the conversion is not possible
 */
static ir_node *conv_to(ir_node *irn, ir_mode *mode) {
	ir_mode *other = get_irn_mode(irn);
	if (other != mode) {
		/* different modes: check if conversion is possible without changing the bits */
		if (can_convert_to(other, mode)) {
			ir_node *block = get_nodes_block(irn);
			return new_r_Conv(block, irn, mode);
		}
		/* otherwise not possible ... yet */
		return NULL;
	}
	return irn;
}  /* conv_to */

/**
 * Update the address of an value if this address was a load result
 * and the load is killed now.
 *
 * @param value  the value whose address is updated
 */
static void update_address(value_t *value) {
	if (is_Proj(value->address)) {
		ir_node *load = get_Proj_pred(value->address);

		if (is_Load(load)) {
			const memop_t *op = get_irn_memop(load);

			if (op->flags & FLAG_KILLED_NODE)
				value->address = op->replace;
		}
	}
}  /* update_address */

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
				memop_t *other;

				update_address(&op->value);
				other = find_address(&op->value);
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
						/* do NOT change the memop table */
						continue;
					}
				}
				/* add this value */
				add_memop(op);
			}
			break;
		case iro_Store:
			if (! (op->flags & FLAG_KILLED_NODE)) {
				/* do we have this store already */
				memop_t *other;

				update_address(&op->value);
				other = find_address(&op->value);
				if (other != NULL) {
					if (is_Store(other->node)) {
						if (op != other && !(other->flags & FLAG_IGNORE) &&
						    get_nodes_block(other->node) == get_nodes_block(op->node)) {
							/*
							 * A WAW in the same block we can kick the first store.
							 * This is a shortcut: we know that the second Store will be anticipated
							 * then in an case.
							 */
							DB((dbg, LEVEL_1, "WAW %+F <- %+F\n", other->node, op->node));
							mark_remove_store(other);
							/* FIXME: a Load might be get freed due to this killed store */
						}
					} else if (other->value.value == op->value.value && !(op->flags & FLAG_IGNORE)) {
						/* WAR */
						DB((dbg, LEVEL_1, "WAR %+F <- %+F\n", op->node, other->node));
						mark_remove_store(op);
						/* do NOT change the memop table */
						continue;
					}
				}
				/* KILL all possible aliases */
				kill_memops(&op->value);
				/* add this value */
				add_memop(op);
			}
			break;
		default:
			if (op->flags & FLAG_KILL_ALL)
				kill_all();
		}
	}
}  /* calc_gen_kill_avail */

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
}  /* forward_avail */

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
	ir_node *block = bl->block;
	int     n = get_Block_n_cfg_outs(block);

	if (n == 1) {
		ir_node  *succ    = get_Block_cfg_out(block, 0);
		block_t  *succ_bl = get_block_entry(succ);
		int      pred_pos = get_Block_cfgpred_pos(succ, block);
		unsigned end      = env.rbs_size - 1;
		unsigned pos;

		kill_all();

		if (bl->trans_results == NULL) {
			/* allocate the translate cache */
			unsigned size = env.curr_adr_id * sizeof(bl->trans_results[0]);
			bl->trans_results = obstack_alloc(&env.obst, size);
			memset(bl->trans_results, 0, size);
		}

		/* check for partly redundant values */
		for (pos = rbitset_next(succ_bl->anticL_in, 0, 1);
		     pos < end;
		     pos = rbitset_next(succ_bl->anticL_in, pos + 1, 1)) {
			/*
			 * do Phi-translation here: Note that at this point the nodes are
			 * not changed, so we can safely cache the results.
			 * However: Loads of Load results ARE bad, because we have no way
			  to translate them yet ...
			 */
			memop_t *op = bl->trans_results[pos];
			if (op == NULL) {
				/* not yet translated */
				ir_node *adr, *trans_adr;

				op  = succ_bl->id_2_memop_antic[pos];
				adr = op->value.address;

				trans_adr = phi_translate(adr, succ, pred_pos);
				if (trans_adr != adr) {
					/* create a new entry for the translated one */
					memop_t *new_op;

					new_op = alloc_memop(NULL);
					new_op->value.address = trans_adr;
					new_op->value.id      = register_address(trans_adr);
					new_op->value.mode    = op->value.mode;
					new_op->node          = op->node; /* we need the node to decide if Load/Store */
					new_op->flags         = op->flags;

					bl->trans_results[pos] = new_op;
					op = new_op;
				}
			}
			env.curr_id_2_memop[op->value.id] = op;
			rbitset_set(env.curr_set, op->value.id);
		}
	} else if (n > 1) {
		ir_node *succ    = get_Block_cfg_out(block, 0);
		block_t *succ_bl = get_block_entry(succ);
		int i;

		rbitset_copy(env.curr_set, succ_bl->anticL_in, env.rbs_size);
		memcpy(env.curr_id_2_memop, succ_bl->id_2_memop_antic, env.rbs_size * sizeof(env.curr_id_2_memop[0]));

		/* Hmm: probably we want kill merges of Loads ans Stores here */
		for (i = n - 1; i > 0; --i) {
			ir_node *succ    = get_Block_cfg_out(bl->block, i);
			block_t *succ_bl = get_block_entry(succ);

			rbitset_and(env.curr_set, succ_bl->anticL_in, env.rbs_size);
		}
	} else {
		/* block ends with a noreturn call */
		kill_all();
	}

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
			if (! (op->flags & FLAG_KILLED_NODE)) {
				/* a Store: check which memops must be killed */
				kill_memops(&op->value);
			}
			break;
		default:
			if (op->flags & FLAG_KILL_ALL)
				kill_all();
		}
	}

	memcpy(bl->id_2_memop_antic, env.curr_id_2_memop, env.rbs_size * sizeof(env.curr_id_2_memop[0]));
	if (! rbitset_equal(bl->anticL_in, env.curr_set, env.rbs_size)) {
		/* changed */
		rbitset_copy(bl->anticL_in, env.curr_set, env.rbs_size);
		dump_curr(bl, "AnticL_in*");
		return 1;
	}
	dump_curr(bl, "AnticL_in");
	return 0;
}  /* backward_antic */

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
			def = new_rd_Conv(db, block, def, mode);
		}
		exchange(proj, def);
	}
	proj = op->projs[pn_Load_X_except];
	if (proj != NULL) {
		exchange(proj, new_Bad());
	}
	proj = op->projs[pn_Load_X_regular];
	if (proj != NULL) {
		exchange(proj, new_r_Jmp(get_nodes_block(load)));
	}
}  /* replace_load */

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
		exchange(proj, new_r_Jmp(get_nodes_block(store)));
	}
}  /* remove_store */


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
}  /* do_replacements */

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
}  /* calcAvail */

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
}  /* calcAntic */

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
}  /* find_last_memory */

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
}  /* reroute_all_mem_users */

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
}  /* reroute_mem_through */

/**
 * insert Loads, making partly redundant Loads fully redundant
 */
static int insert_Load(block_t *bl) {
	ir_node  *block = bl->block;
	int      i, n = get_Block_n_cfgpreds(block);
	unsigned end = env.rbs_size - 1;
	unsigned pos;

	DB((dbg, LEVEL_3, "processing %+F\n", block));

	if (n == 0) {
		/* might still happen for an unreachable block (end for instance) */
		return 0;
	}

	if (n > 1) {
		ir_node **ins;
		int     pos;

		NEW_ARR_A(ir_node *, ins, n);

		rbitset_set_all(env.curr_set, env.rbs_size);

		/* More than one predecessors, calculate the join for all avail_outs ignoring unevaluated
		   Blocks. These put in Top anyway. */
		for (i = n - 1; i >= 0; --i) {
			ir_node *pred = skip_Proj(get_Block_cfgpred(block, i));
			ir_node *blk  = get_nodes_block(pred);
			block_t *pred_bl;

			pred_bl = get_block_entry(blk);
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
				memop_t *first   = NULL;
				ir_mode *mode    = NULL;

				for (i = 0; i < n; ++i) {
					memop_t *mop;

					pred    = get_Block_cfgpred_block(bl->block, i);
					pred_bl = get_block_entry(pred);

					mop = pred_bl->id_2_memop_avail[pos];
					if (first == NULL) {
						first = mop;
						ins[0] = first->value.value;
						mode = get_irn_mode(ins[0]);

						/* no Phi needed so far */
						env.curr_id_2_memop[pos] = first;
					} else {
						ins[i] = conv_to(mop->value.value, mode);
						if (ins[i] != ins[0]) {
							if (ins[i] == NULL) {
								/* conversion failed */
								env.curr_id_2_memop[pos] = NULL;
								rbitset_clear(env.curr_set, pos);
								break;
							}
							need_phi = 1;
						}
					}
				}
				if (need_phi) {
					/* build a Phi  */
					ir_node *phi = new_r_Phi(bl->block, n, ins, mode);
					memop_t *phiop = alloc_memop(phi);

					phiop->value = first->value;
					phiop->value.value = phi;

					/* no need to link it in, as it is a DATA phi */

					env.curr_id_2_memop[pos] = phiop;

					DB((dbg, LEVEL_3, "Created new %+F on merging value for address %+F\n", phi, first->value.address));
				}
			}
		}
	} else {
		/* only one predecessor, simply copy the map */
		ir_node *pred    = get_Block_cfgpred_block(bl->block, 0);
		block_t *pred_bl = get_block_entry(pred);

		rbitset_copy(env.curr_set, pred_bl->avail_out, env.rbs_size);

		memcpy(env.curr_id_2_memop, pred_bl->id_2_memop_avail, env.rbs_size * sizeof(bl->id_2_memop_avail[0]));
	}

	if (n > 1) {
		/* check for partly redundant values */
		for (pos = rbitset_next(bl->anticL_in, 0, 1);
		     pos < end;
		     pos = rbitset_next(bl->anticL_in, pos + 1, 1)) {
			memop_t *op = bl->id_2_memop_antic[pos];
			int     have_some, all_same;
			ir_node *first;

			if (rbitset_is_set(env.curr_set, pos)) {
				/* already avail */
				continue;
			}

			assert(is_Load(op->node));

			DB((dbg, LEVEL_3, "anticipated %+F\n", op->node));

			have_some  = 0;
			all_same   = 1;
			first      = 0;
			for (i = n - 1; i >= 0; --i) {
				ir_node *pred    = get_Block_cfgpred_block(block, i);
				block_t *pred_bl = get_block_entry(pred);
				ir_mode *mode    = op->value.mode;
				memop_t *e;
				ir_node *adr;

				adr = phi_translate(op->value.address, block, i);
				DB((dbg, LEVEL_3, ".. using address %+F in pred %d\n", adr, i));
				e   = find_address_avail(pred_bl, register_address(adr), mode);
				if (e == NULL) {
					ir_node *ef_block = get_nodes_block(adr);
					if (! block_dominates(ef_block, pred)) {
						/* cannot place a copy here */
						have_some = 0;
						DB((dbg, LEVEL_3, "%+F cannot be moved into predecessor %+F\n", op->node, pred));
						break;
					}
					DB((dbg, LEVEL_3, "%+F is not available in predecessor %+F\n", op->node, pred));
					pred_bl->avail = NULL;
					all_same       = 0;
				} else {
					if (e->value.mode != mode && !can_convert_to(e->value.mode, mode)) {
						/* cannot create a Phi due to different modes */
						have_some = 0;
						break;
					}
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
				memop_t *phi_op;

				NEW_ARR_A(ir_node *, in, n);

				for (i = n - 1; i >= 0; --i) {
					ir_node *pred    = get_Block_cfgpred_block(block, i);
					block_t *pred_bl = get_block_entry(pred);

					if (pred_bl->avail == NULL) {
						/* create a new Load here and make to make it fully redundant */
						dbg_info *db       = get_irn_dbg_info(op->node);
						ir_node  *last_mem = find_last_memory(pred_bl);
						ir_node  *load, *def, *adr;
						memop_t  *new_op;

						assert(last_mem != NULL);
						adr  = phi_translate(op->value.address, block, i);
						load = new_rd_Load(db, pred, last_mem, adr, mode, cons_none);
						def  = new_r_Proj(pred, load, mode, pn_Load_res);
						DB((dbg, LEVEL_1, "Created new %+F in %+F for party redundant %+F\n", load, pred, op->node));

						new_op                = alloc_memop(load);
						new_op->mem           = new_r_Proj(pred, load, mode_M, pn_Load_M);
						new_op->value.address = adr;
						new_op->value.id      = op->value.id;
						new_op->value.mode    = mode;
						new_op->value.value   = def;

						new_op->projs[pn_Load_M]   = new_op->mem;
						new_op->projs[pn_Load_res] = def;

						new_op->prev = pred_bl->memop_backward;
						if (pred_bl->memop_backward != NULL)
							pred_bl->memop_backward->next = new_op;

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
					in[i] = conv_to(pred_bl->avail->value.value, mode);
				}
				phi = new_r_Phi(block, n, in, mode);
				DB((dbg, LEVEL_1, "Created new %+F in %+F for now redundant %+F\n", phi, block, op->node));

				phi_op = clone_memop_phi(op, phi);
				add_memop(phi_op);
			}
		}
	}

	/* recalculate avail by gen and kill */
	calc_gen_kill_avail(bl);

	/* always update the map after gen/kill, as values might have been changed due to RAR/WAR/WAW */
	memcpy(bl->id_2_memop_avail, env.curr_id_2_memop, env.rbs_size * sizeof(env.curr_id_2_memop[0]));

	if (!rbitset_equal(bl->avail_out, env.curr_set, env.rbs_size)) {
		/* the avail set has changed */
		rbitset_copy(bl->avail_out, env.curr_set, env.rbs_size);
		dump_curr(bl, "Avail_out*");
		return 1;
	}
	dump_curr(bl, "Avail_out");
	return 0;
}  /* insert_Load */

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
}  /* insert_Loads_upwards */

/**
 * Kill unreachable control flow.
 *
 * @param irg  the graph to operate on
 */
static void kill_unreachable_blocks(ir_graph *irg) {
	block_t *bl;
	ir_node **ins;
	int     changed = 0;

	NEW_ARR_A(ir_node *, ins, env.max_cfg_preds);

	for (bl = env.forward; bl != NULL; bl = bl->forward_next) {
		ir_node *block = bl->block;
		int     i, j, k, n;

		assert(get_Block_mark(block));

		n = get_Block_n_cfgpreds(block);

		for (i = j = 0; i < n; ++i) {
			ir_node *pred = get_Block_cfgpred(block, i);
			ir_node *pred_bl;

			if (is_Bad(pred))
				continue;

			pred_bl = get_nodes_block(skip_Proj(pred));
			if (! get_Block_mark(pred_bl))
				continue;

			ins[j++] = pred;
		}
		if (j != n) {
			ir_node *phi, *next;

			/* some unreachable blocks detected */
			changed = 1;

			DB((dbg, LEVEL_1, "Killing dead block predecessors on %+F\n", block));

			set_irn_in(block, j, ins);

			/* shorten all Phi nodes */
			for (phi = get_Block_phis(block); phi != NULL; phi = next) {
				next = get_Phi_next(phi);

				for (i = k = 0; i < n; ++i) {
					ir_node *pred = get_Block_cfgpred_block(block, i);

					if (is_Bad(pred))
						continue;

					if (! get_Block_mark(pred))
						continue;

					ins[k++] = get_Phi_pred(phi, i);
				}
				if (k == 1)
					exchange(phi, ins[0]);
				else
					set_irn_in(phi, k, ins);
			}
		}

	}

	if (changed) {
		/* kick keep alives */
		ir_node *end = get_irg_end(irg);
		int     i, j, n = get_End_n_keepalives(end);

		NEW_ARR_A(ir_node *, ins, n);

		for (i = j = 0; i < n; ++i) {
			ir_node *ka = get_End_keepalive(end, i);
			ir_node *ka_bl;

			if (is_Bad(ka))
				continue;
			if (is_Block(ka))
				ka_bl = ka;
			else
				ka_bl = get_nodes_block(skip_Proj(ka));
			if (get_Block_mark(ka_bl))
				ins[j++] = ka;
		}
		if (j != n)
			set_End_keepalives(end, j, ins);

		free_irg_outs(irg);

		/* this transformation do NOT invalidate the dominance */
	}
}  /* kill_unreachable_blocks */

int opt_ldst(ir_graph *irg) {
	block_t  *bl;
	ir_graph *rem = current_ir_graph;

	current_ir_graph = irg;

	FIRM_DBG_REGISTER(dbg, "firm.opt.ldst");
//	firm_dbg_set_mask(dbg, -1);

	DB((dbg, LEVEL_1, "\nDoing Load/Store optimization on %+F\n", irg));

	/* we need landing pads */
	remove_critical_cf_edges(irg);

//	dump_ir_block_graph(irg, "-XXX");

	if (get_opt_alias_analysis()) {
		assure_irg_entity_usage_computed(irg);
		assure_irp_globals_entity_usage_computed();
	}

	obstack_init(&env.obst);
	ir_nodemap_init(&env.adr_map);

	env.forward       = NULL;
	env.backward      = NULL;
	env.curr_adr_id   = 0;
	env.n_mem_ops     = 0;
	env.max_cfg_preds = 0;
	env.changed       = 0;
	env.start_bl      = get_irg_start_block(irg);
	env.end_bl        = get_irg_end_block(irg);
#ifdef DEBUG_libfirm
	env.id_2_address  = NEW_ARR_F(ir_node *, 0);
#endif

	assure_irg_outs(irg);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_BLOCK_MARK);

	/* first step: allocate block entries. Note that some blocks might be
	   unreachable here. Using the normal walk ensures that ALL blocks are initialized. */
	irg_walk_graph(irg, prepare_blocks, link_phis, NULL);

	/* produce an inverse post-order list for the CFG: this links only reachable
	   blocks */
	irg_out_block_walk(get_irg_start_block(irg), NULL, inverse_post_order, NULL);

	if (! get_Block_mark(env.end_bl)) {
		/*
		 * The end block is NOT reachable due to endless loops
		 * or no_return calls.
		 * Place the end block last.
		 * env.backward points to the last block in the list for this purpose.
		 */
		env.backward->forward_next = get_block_entry(env.end_bl);

		set_Block_mark(env.end_bl, 1);
	}

	/* KILL unreachable blocks: these disturb the data flow analysis */
	kill_unreachable_blocks(irg);

	assure_doms(irg);

	/* second step: find and sort all memory ops */
	walk_memory_irg(irg, collect_memops, NULL, NULL);

#ifdef DEBUG_libfirm
	/* check that the backward map is correct */
	assert((unsigned)ARR_LEN(env.id_2_address) == env.curr_adr_id);
#endif

	if (env.n_mem_ops == 0) {
		/* no memory ops */
		goto end;
	}

	/* create the backward links. */
	env.backward = NULL;
	irg_block_walk_graph(irg, NULL, collect_backward, NULL);

	/* link the end block in */
	bl = get_block_entry(env.end_bl);
	bl->backward_next = env.backward;
	env.backward      = bl;

	/* check that we really start with the start / end block */
	assert(env.forward->block  == env.start_bl);
	assert(env.backward->block == env.end_bl);

	/* create address sets: for now, only the existing addresses are allowed plus one
	   needed for the sentinel */
	env.rbs_size = env.curr_adr_id + 1;

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
	(void) dump_block_list;

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

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_BLOCK_MARK);
	ir_nodemap_destroy(&env.adr_map);
	obstack_free(&env.obst, NULL);

//	dump_ir_block_graph(irg, "-YYY");

#ifdef DEBUG_libfirm
	DEL_ARR_F(env.id_2_address);
#endif

	current_ir_graph = rem;
	return env.changed != 0;
}  /* opt_ldst */
