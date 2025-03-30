/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Load/Store optimizations.
 * @author  Michael Beck
 */
#include "array.h"
#include "dbginfo_t.h"
#include "debug.h"
#include "entity_t.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgopt.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irhooks.h"
#include "irmemory.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "irnodehashmap.h"
#include "iropt_dbg.h"
#include "iropt_t.h"
#include "iroptimize.h"
#include "irtools.h"
#include "panic.h"
#include "set.h"
#include "target_t.h"
#include "tv_t.h"
#include "type_t.h"
#include "util.h"
#include <string.h>

/** The debug handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg;)

#define MAX_PROJ MAX(MAX((unsigned)pn_Load_max, (unsigned)pn_Store_max), (unsigned)pn_Call_max)

typedef enum changes_t {
	NO_CHANGES = 0,
	DF_CHANGED = (1 << 0), /**< data flow changed */
	CF_CHANGED = (1 << 1), /**< control flow changed */
	/** nodes have been created but are not reachable. This is a bad hack
	 * try hard to avoid it! */
	NODES_CREATED = (1 << 2),
} changes_t;

/**
 * walker environment
 */
typedef struct walk_env_t {
	struct obstack obst;    /**< list of all stores */
	changes_t      changes; /**< a bitmask of graph changes */
} walk_env_t;

/** A Load/Store info. */
typedef struct ldst_info_t {
	ir_node  *projs[MAX_PROJ+1]; /**< list of Proj's of this node */
	ir_node  *exc_block;         /**< the exception block if available */
	int      exc_idx;            /**< predecessor index in exception block */
	unsigned visited;            /**< visited counter for breaking loops */
} ldst_info_t;

typedef struct base_offset_t {
	ir_node *base;
	long     offset;
} base_offset_t;

typedef struct track_load_env_t {
	ir_node      *load;
	base_offset_t base_offset;
	ir_node      *ptr; /* deprecated: alternative representation of
	                      base_offset */
} track_load_env_t;

/**
 * flags for control flow.
 */
typedef enum block_flags_t {
	BLOCK_HAS_COND = (1 << 0), /**< Block has conditional control flow */
	BLOCK_HAS_EXC  = (1 << 1), /**< Block has exceptional control flow */
} block_flags_t;

/**
 * a Block info.
 */
typedef struct block_info_t {
	block_flags_t flags;  /**< flags for the block */
} block_info_t;

/** the master visited flag for loop detection. */
static unsigned master_visited;

#define INC_MASTER()       ++master_visited
#define MARK_NODE(info)    (info)->visited = master_visited
#define NODE_VISITED(info) (info)->visited >= master_visited

/**
 * get the Load/Store info of a node
 */
static ldst_info_t *get_ldst_info(ir_node *node, struct obstack *obst)
{
	ldst_info_t *info = (ldst_info_t *)get_irn_link(node);
	if (info == NULL) {
		info = OALLOCZ(obst, ldst_info_t);
		set_irn_link(node, info);
	}
	return info;
}

/**
 * get the Block info of a node
 */
static block_info_t *get_block_info(ir_node *node, struct obstack *obst)
{
	block_info_t *info = (block_info_t *)get_irn_link(node);
	if (info == NULL) {
		info = OALLOCZ(obst, block_info_t);
		set_irn_link(node, info);
	}
	return info;
}

/**
 * update the projection info for a Load/Store
 */
static changes_t update_projs(ldst_info_t *info, ir_node *proj)
{
	unsigned nr = get_Proj_num(proj);
	assert(nr <= MAX_PROJ);

	if (info->projs[nr] != NULL) {
		/* there is already one, do CSE */
		exchange(proj, info->projs[nr]);
		return DF_CHANGED;
	} else {
		info->projs[nr] = proj;
		return NO_CHANGES;
	}
}

/**
 * update the exception block info for a Load/Store node.
 *
 * @param info   the load/store info struct
 * @param block  the exception handler block for this load/store
 * @param pos    the control flow input of the block
 */
static void update_exc(ldst_info_t *info, ir_node *block, int pos)
{
	assert(info->exc_block == NULL);
	info->exc_block = block;
	info->exc_idx   = pos;
}

/**
 * walker, collects all Proj/Load/Store/Call/CopyB nodes
 *
 * walks from Start -> End
 */
static void collect_nodes(ir_node *node, void *env)
{
	walk_env_t *wenv   = (walk_env_t *)env;
	unsigned    opcode = get_irn_opcode(node);

	if (opcode == iro_Proj) {
		ir_node *pred = get_Proj_pred(node);
		opcode = get_irn_opcode(pred);

		if (opcode == iro_Load || opcode == iro_Store || opcode == iro_Call) {
			ldst_info_t *ldst_info = get_ldst_info(pred, &wenv->obst);

			wenv->changes |= update_projs(ldst_info, node);

			/*
			 * Place the Proj's to the same block as the
			 * predecessor Load. This is always ok and prevents
			 * "non-SSA" form after optimizations if the Proj
			 * is in a wrong block.
			 */
			ir_node *blk      = get_nodes_block(node);
			ir_node *pred_blk = get_nodes_block(pred);
			if (blk != pred_blk) {
				wenv->changes |= DF_CHANGED;
				set_nodes_block(node, pred_blk);
			}
		}
	} else if (opcode == iro_Block) {
		for (int i = get_Block_n_cfgpreds(node); i-- > 0; ) {
			bool     is_exc = false;
			ir_node *proj   = get_Block_cfgpred(node, i);
			ir_node *pred   = proj;

			/* ignore Bad predecessors, they will be removed later */
			if (is_Bad(pred))
				continue;

			if (is_Proj(proj)) {
				pred   = get_Proj_pred(proj);
				is_exc = is_x_except_Proj(proj);
			}
			ir_node      *pred_block = get_nodes_block(pred);
			block_info_t *bl_info    = get_block_info(pred_block, &wenv->obst);

			if (is_fragile_op(pred) && is_exc)
				bl_info->flags |= BLOCK_HAS_EXC;
			else if (is_irn_forking(pred))
				bl_info->flags |= BLOCK_HAS_COND;

			opcode = get_irn_opcode(pred);
			if (is_exc && (opcode == iro_Load || opcode == iro_Store
			               || opcode == iro_Call)) {
				ldst_info_t *ldst_info = get_ldst_info(pred, &wenv->obst);

				update_exc(ldst_info, node, i);
			}
		}
	} else if (is_memop(node)) {
		/* Just initialize a ldst_info */
		(void)get_ldst_info(node, &wenv->obst);
	}
}

/* forward */
static void reduce_node_usage(ir_node *ptr);

/**
 * Update a Load that may have lost its users.
 */
static void handle_load_update(ir_node *load)
{
	/* do NOT touch volatile loads for now */
	if (get_Load_volatility(load) == volatility_is_volatile)
		return;

	ldst_info_t *info = (ldst_info_t *)get_irn_link(load);
	if (!info->projs[pn_Load_res] && !info->projs[pn_Load_X_except]) {
		ir_node *ptr = get_Load_ptr(load);
		ir_node *mem = get_Load_mem(load);

		/* a Load whose value is neither used nor exception checked, remove it */
		exchange(info->projs[pn_Load_M], mem);
		if (info->projs[pn_Load_X_regular]) {
			ir_node *jmp = new_r_Jmp(get_nodes_block(load));
			exchange(info->projs[pn_Load_X_regular], jmp);
		}
		kill_node(load);
		reduce_node_usage(ptr);
	}
}

/**
 * A use of a node has vanished. Check if this was a Proj
 * node and update the counters.
 */
static void reduce_node_usage(ir_node *ptr)
{
	if (!is_Proj(ptr))
		return;
	if (get_irn_n_edges(ptr) > 0)
		return;

	/* this Proj is dead now */
	ir_node *pred = get_Proj_pred(ptr);
	if (is_Load(pred)) {
		ldst_info_t *info = (ldst_info_t *)get_irn_link(pred);
		info->projs[get_Proj_num(ptr)] = NULL;

		/* this node lost its result proj, handle that */
		handle_load_update(pred);
	}
}

/**
 * Kill a Load or Store and all other nodes which are not needed after
 * it has been killed.
 */
static void kill_and_reduce_usage(ir_node *node)
{
	ir_node *ptr;
	ir_node *value;
	switch (get_irn_opcode(node)) {
	case iro_Load:
		ptr   = get_Load_ptr(node);
		value = NULL;
		break;
	case iro_Store:
		ptr   = get_Store_ptr(node);
		value = get_Store_value(node);
		break;
	default:
		panic("cannot handle node %+F", node);
	}

	kill_node(node);
	reduce_node_usage(ptr);
	if (value != NULL) {
		reduce_node_usage(value);
	}
}

static void get_base_and_offset(ir_node *ptr, base_offset_t *base_offset)
{
	/* TODO: long might not be enough, we should probably use some tarval
	 * thingy, or at least detect long overflows and abort */
	long     offset = 0;
	ir_mode *mode   = get_irn_mode(ptr);
	for (;;) {
		if (is_Add(ptr)) {
			ir_node *l = get_Add_left(ptr);
			ir_node *r = get_Add_right(ptr);

			if (get_irn_mode(l) != mode || !is_Const(r))
				break;

			offset += get_Const_long(r);
			ptr     = l;
		} else if (is_Sub(ptr)) {
			ir_node *r = get_Sub_right(ptr);
			if (!is_Const(r))
				break;

			offset -= get_Const_long(r);
			ptr     = get_Sub_left(ptr);
		} else if (is_Sel(ptr)) {
			ir_node *index = get_Sel_index(ptr);
			if (!is_Const(index))
				break;

			ir_type *type         = get_Sel_type(ptr);
			ir_type *element_type = get_array_element_type(type);
			if (get_type_state(element_type) != layout_fixed)
				break;

			/* TODO: may overflow here */
			int size = get_type_size(element_type);
			offset += size * get_Const_long(index);
			ptr     = get_Sel_ptr(ptr);
		} else if (is_Member(ptr)) {
			ir_entity *entity = get_Member_entity(ptr);
			ir_type   *owner  = get_entity_owner(entity);
			if (get_type_state(owner) != layout_fixed)
				break;
			offset += get_entity_offset(entity);
			ptr     = get_Member_ptr(ptr);
		} else
			break;
	}

	base_offset->offset = offset;
	base_offset->base   = ptr;
}

/**
 * Checks whether the memory of the first access
 * is contained within the second one.
 */
static bool is_contained_in(ir_mode *const load_mode, const base_offset_t *const load_bo,
                            ir_mode *const prev_mode, const base_offset_t *const prev_bo)
{
	if (load_bo->base != prev_bo->base)
		return false;

	/* ensure the load value is completely contained in the previous one */
	long delta = load_bo->offset - prev_bo->offset;
	if (delta < 0)
		return false;
	long load_mode_len = get_mode_size_bytes(load_mode);
	long prev_mode_len = get_mode_size_bytes(prev_mode);
	if (delta+load_mode_len > prev_mode_len)
		return false;

	/* simple case: previous value has the same mode */
	if (load_mode == prev_mode)
		return true;

	ir_mode_arithmetic prev_arithmetic = get_mode_arithmetic(prev_mode);
	ir_mode_arithmetic load_arithmetic = get_mode_arithmetic(load_mode);
	return (prev_arithmetic == irma_twos_complement &&
	        load_arithmetic == irma_twos_complement)
	       || (prev_arithmetic != load_arithmetic
	           && load_mode_len == prev_mode_len);
}


/**
 * This is called for load-after-load and load-after-store.
 * If possible the value of the previous load/store is transformed in a way
 * so the 2nd load can be left out/replaced by arithmetic on the previous
 * value.
 */
static ir_node *transform_previous_value(ir_mode *const load_mode,
                                         const long load_offset, ir_mode *const prev_mode,
                                         const long prev_offset, ir_node *const prev_value,
                                         ir_node *const block, ir_node *const load)
{
	/* simple case: previous value has the same mode */
	if (load_mode == prev_mode)
		return prev_value;

	/* two complement values can be transformed with bitops */
	dbg_info          *const dbgi            = get_irn_dbg_info(load);
	long               const load_mode_len   = get_mode_size_bytes(load_mode);
	long               const prev_mode_len   = get_mode_size_bytes(prev_mode);
	ir_mode_arithmetic const prev_arithmetic = get_mode_arithmetic(prev_mode);
	ir_mode_arithmetic const load_arithmetic = get_mode_arithmetic(load_mode);
	if (prev_arithmetic == irma_twos_complement &&
	    load_arithmetic == irma_twos_complement) {
		/* produce a shift to adjust offset delta */
		long           delta = load_offset - prev_offset;
		unsigned const shift = ir_target_big_endian()
			? prev_mode_len - load_mode_len - delta
			: delta;
		ir_node *new_value = prev_value;
		if (shift != 0) {
			if (mode_is_reference(prev_mode)) {
				ir_mode *const new_mode = get_reference_offset_mode(prev_mode);
				new_value = new_r_Conv(block, new_value, new_mode);
			}
			ir_graph *const irg  = get_irn_irg(block);
			ir_node  *const cnst = new_r_Const_long(irg, mode_Iu, shift * 8);
			new_value = new_rd_Shr(dbgi, block, new_value, cnst);
		}

		return new_rd_Conv(dbgi, block, new_value, load_mode);
	} else {
		assert(prev_arithmetic != load_arithmetic && load_mode_len == prev_mode_len);
		return new_rd_Bitcast(dbgi, block, prev_value, load_mode);
	}
}

static changes_t replace_load(ir_node *load, ir_node *new_value)
{
	const ldst_info_t *info = (ldst_info_t *)get_irn_link(load);
	if (info->projs[pn_Load_M])
		exchange(info->projs[pn_Load_M], get_Load_mem(load));

	changes_t res = NO_CHANGES;
	/* no exception */
	if (info->projs[pn_Load_X_except] != NULL) {
		ir_graph *irg = get_irn_irg(load);
		ir_node  *bad = new_r_Bad(irg, mode_X);
		exchange(info->projs[pn_Load_X_except], bad);
		res |= CF_CHANGED;

		assert(info->projs[pn_Load_X_regular] != NULL);
		ir_node *jmp = new_r_Jmp(get_nodes_block(load));
		exchange(info->projs[pn_Load_X_regular], jmp);
	}

	/* loads without user should already be optimized away */
	assert(info->projs[pn_Load_res] != NULL);
	exchange(info->projs[pn_Load_res], new_value);

	kill_and_reduce_usage(load);
	return res | DF_CHANGED;
}

/**
 * returns false if op cannot be reached through the X_regular proj of
 * @p prev_op or if there are no exceptions possible.
 */
static bool on_regular_path(ir_node *op, ir_node *prev_op)
{
	/* TODO: create a real test, for now we just make sure the previous node
	 * does not throw an exception. */
	(void)op;
	return !is_fragile_op(prev_op) || !ir_throws_exception(prev_op);
}

static changes_t try_load_after_store(track_load_env_t *env, ir_node *store)
{
	ir_node *const load = env->load;
	if (!on_regular_path(load, store))
		return NO_CHANGES;

	ir_node       *store_ptr        = get_Store_ptr(store);
	base_offset_t  prev_base_offset;
	get_base_and_offset(store_ptr, &prev_base_offset);
	ir_mode       *const load_mode   = get_Load_mode(load);
	ir_mode       *const store_mode  = get_irn_mode(get_Store_value(store));
	base_offset_t *const base_offset = &env->base_offset;

	/* load value completely contained in previous store? */
	if (is_contained_in(load_mode, base_offset, store_mode, &prev_base_offset)) {
		ir_node *const store_value = get_Store_value(store);
		ir_node *const block       = get_nodes_block(load);
		ir_node *const new_value
			= transform_previous_value(load_mode, base_offset->offset, store_mode,
			                           prev_base_offset.offset, store_value, block, load);
		DBG_OPT_RAW(load, new_value);
		return replace_load(load, new_value);
	}

	return NO_CHANGES;

}

static changes_t try_load_after_load(track_load_env_t *env, ir_node *prev_load)
{
	ir_node *const load = env->load;
	if (!on_regular_path(load, prev_load))
		return NO_CHANGES;

	ldst_info_t *const info       = (ldst_info_t *)get_irn_link(prev_load);
	ir_node     *const prev_value = info->projs[pn_Load_res];
	/* the other load is unused and will get removed later anyway */
	if (prev_value == NULL)
		return NO_CHANGES;

	ir_node *const prev_ptr        = get_Load_ptr(prev_load);
	base_offset_t prev_base_offset;
	get_base_and_offset(prev_ptr, &prev_base_offset);
	ir_mode       *const load_mode   = get_Load_mode(load);
	ir_mode       *const prev_mode   = get_Load_mode(prev_load);
	base_offset_t *const base_offset = &env->base_offset;

	/* load value completely contained in previous load? */
	if (is_contained_in(load_mode, base_offset, prev_mode, &prev_base_offset)) {
		ir_node *const block     = get_nodes_block(load);
		ir_node *const new_value
			= transform_previous_value(load_mode, base_offset->offset, prev_mode,
			                           prev_base_offset.offset, prev_value, block, load);
		DBG_OPT_RAR(prev_load, load);
		return replace_load(load, new_value);
	}

	/* previous load value completely contained in current load? */
	if (get_Load_type(load) == get_Load_type(prev_load) &&
	    is_contained_in(prev_mode, &prev_base_offset, load_mode, base_offset)) {
		ir_graph *const irg   = get_irn_irg(prev_load);
		ir_node  *const block = get_nodes_block(prev_load);

		/* Adapt pointer of previous load. */
		const long        delta         = base_offset->offset - prev_base_offset.offset;
		assert(delta <= 0);
		ir_mode    *const ptr_mode      = get_irn_mode(prev_ptr);
		ir_mode    *const ptr_mode_offs = get_reference_offset_mode(ptr_mode);
		ir_node    *const c             = new_r_Const_long(irg, ptr_mode_offs, delta);
		ir_node    *const add           = new_r_Add(block, prev_ptr, c);
		set_Load_ptr(prev_load, add);

		/* Change mode of previous load. */
		set_Load_mode(prev_load, load_mode);
		ir_node   *const original_proj = info->projs[pn_Load_res];
		ir_node   *const result_proj   = new_r_Proj(prev_load, load_mode, pn_Load_res);
		changes_t        changed       = replace_load(load, result_proj);
		ir_node   *const new_prev_value
			= transform_previous_value(prev_mode, prev_base_offset.offset, load_mode,
			                           base_offset->offset, result_proj, block, load);
		exchange(original_proj, new_prev_value);
		info->projs[pn_Load_res] = result_proj;
		changed |= DF_CHANGED;
		DBG_OPT_RAR(prev_load, load);
		return changed;
	}

	return NO_CHANGES;

}

static bool try_update_ptr_CopyB(track_load_env_t *env, ir_node *copyb)
{
	ir_node       *copyb_dst       = get_CopyB_dst(copyb);
	base_offset_t  dst_base_offset;
	get_base_and_offset(copyb_dst, &dst_base_offset);
	if (dst_base_offset.base != env->base_offset.base)
		return false;

	/* see if bytes loaded are fully contained in the CopyB */
	if (env->base_offset.offset < dst_base_offset.offset)
		return false;

	ir_type       *copyb_type      = get_CopyB_type(copyb);
	long           n_copy          = get_type_size(copyb_type);
	ir_node       *copyb_src       = get_CopyB_src(copyb);
	base_offset_t  src_base_offset;
	get_base_and_offset(copyb_src, &src_base_offset);

	long     delta     = env->base_offset.offset - dst_base_offset.offset;
	ir_node *load      = env->load;
	ir_mode *load_mode = get_Load_mode(load);
	long     load_size = get_mode_size_bytes(load_mode);
	if (delta + load_size > n_copy)
		return false;

	/* track src input */
	env->base_offset.base   = src_base_offset.base;
	env->base_offset.offset = src_base_offset.offset + delta;

	/*
	 * Everything is OK, we can replace
	 *   ptr = (load_base_ptr + load_offset)
	 * with
	 *   new_load_ptr = (src_base_ptr + delta)
	 */
	ir_graph *irg          = get_irn_irg(load);
	ir_node  *block        = get_nodes_block(load);
	ir_mode  *mode_ref     = get_irn_mode(src_base_offset.base);
	ir_mode  *mode_offset  = get_reference_offset_mode(mode_ref);
	ir_node  *cnst         = new_r_Const_long(irg, mode_offset,
	                                          src_base_offset.offset + delta);
	ir_node  *new_load_ptr = new_r_Add(block, src_base_offset.base, cnst);
	env->ptr = new_load_ptr;
	return true;
}

/* Note on terminology:
 * In the following functions, two types are associated with each Load
 * and Store: load_type/store_type are the types of the actual
 * operations (derived from the modes of the IR nodes), whereas
 * load_objt/store_objt are the types of the objects in memory which are
 * accessed by the Load/Store nodes.
 */

/**
 * Follow the memory chain as long as there are only Loads,
 * alias free Stores, and constant Calls and try to replace the
 * current Load by a previous ones.
 * Note that in unreachable loops it might happen that we reach
 * load again, as well as we can fall into a cycle.
 * We break such cycles using a special visited flag.
 *
 * INC_MASTER() must be called before dive into
 */
static changes_t follow_load_mem_chain(track_load_env_t *env, ir_node *start)
{
	ir_node  *load      = env->load;
	ir_type  *load_type = get_Load_type(load);
	unsigned  load_size = get_mode_size_bytes(get_Load_mode(load));

	ir_node   *node = start;
	changes_t  res  = NO_CHANGES;
	for (;;) {
		ldst_info_t *node_info = (ldst_info_t *)get_irn_link(node);

		if (is_Store(node)) {
			/* first try load-after-store */
			changes_t changes = try_load_after_store(env, node);
			if (changes != NO_CHANGES)
				return changes | res;

			/* check if we can pass through this store */
			const ir_node     *ptr        = get_Store_ptr(node);
			const ir_node     *value      = get_Store_value(node);
			const ir_type     *store_type = get_Store_type(node);
			unsigned           store_size = get_mode_size_bytes(get_irn_mode(value));
			ir_alias_relation  rel        = get_alias_relation(
				ptr, store_type, store_size,
				env->ptr, load_type, load_size);
			/* if the might be an alias, we cannot pass this Store */
			if (rel != ir_no_alias)
				break;
			node = skip_Proj(get_Store_mem(node));
		} else if (is_Load(node)) {
			/* try load-after-load */
			changes_t changes = try_load_after_load(env, node);
			if (changes != NO_CHANGES)
				return changes | res;
			/* we can skip any load */
			node = skip_Proj(get_Load_mem(node));
		} else if (is_CopyB(node)) {
			/*
			 * We cannot replace the Load with another
			 * Load from the CopyB's source directly,
			 * because there may be Stores in between,
			 * destroying the source data. However, we can
			 * use the source address from this point
			 * onwards for further optimizations.
			 */
			bool updated = try_update_ptr_CopyB(env, node);
			if (updated) {
				/* Special case: If new_ptr points to
				 * a constant, we *can* replace the
				 * Load immediately.
				 */
				res |= NODES_CREATED;
				ir_mode *load_mode = get_Load_mode(load);
				ir_node *new_value = predict_load(env->ptr, load_mode);
				if (new_value != NULL)
					return replace_load(load, new_value) | res;
			}

			/* check aliasing with the CopyB */
			ir_node           *dst  = get_CopyB_dst(node);
			ir_type           *type = get_CopyB_type(node);
			unsigned           size = get_type_size(type);
			ir_alias_relation  rel  = get_alias_relation(
				dst, type, size,
				env->ptr, load_type, load_size);
			/* possible alias => we cannot continue */
			if (rel != ir_no_alias)
				break;
			node = skip_Proj(get_CopyB_mem(node));
		} else if (is_irn_const_memory(node)) {
			node = skip_Proj(get_memop_mem(node));
		} else {
			/* be conservative about any other node and assume aliasing
			 * that changes the loaded value */
			break;
		}

		/* check for cycles */
		if (NODE_VISITED(node_info))
			break;
		MARK_NODE(node_info);
	}

	if (is_Sync(node)) {
		/* handle all Sync predecessors */
		foreach_irn_in(node, i, in) {
			ir_node *skipped = skip_Proj(in);
			res |= follow_load_mem_chain(env, skipped);
			if ((res & ~NODES_CREATED) != NO_CHANGES)
				break;
		}
	}
	return res;
}

ir_node *can_replace_load_by_const(const ir_node *load, ir_node *c)
{
	ir_mode  *c_mode = get_irn_mode(c);
	ir_mode  *l_mode = get_Load_mode(load);
	ir_node  *block  = get_nodes_block(load);
	dbg_info *dbgi   = get_irn_dbg_info(load);
	ir_node  *res    = duplicate_subgraph(dbgi, c, block);

	if (c_mode != l_mode) {
		/* check, if the mode matches OR can be easily converted info */
		if (is_reinterpret_cast(c_mode, l_mode)) {
			/* copy the value from the const code irg and cast it */
			res = new_rd_Conv(dbgi, block, res, l_mode);
		} else {
			return NULL;
		}
	}
	return res;
}

static ir_entity *find_entity(ir_node *ptr)
{
	switch (get_irn_opcode(ptr)) {
	case iro_Address:
		return get_Address_entity(ptr);

	case iro_Offset:
		return get_Offset_entity(ptr);

	case iro_Member: {
		ir_node *pred = get_Member_ptr(ptr);
		if (get_irg_frame(get_irn_irg(ptr)) == pred)
			return get_Member_entity(ptr);

		return find_entity(pred);
	}

	case iro_Add: {
		ir_node *const right = get_Add_right(ptr);
		if (mode_is_reference(get_irn_mode(right)))
			return find_entity(right);
	} /* FALLTHROUGH */
	case iro_Sub:
		return find_entity(get_binop_left(ptr));

	default:
		return NULL;
	}
}

/**
 * optimize a Load
 *
 * @param load  the Load node
 */
static changes_t optimize_load(ir_node *load)
{
	const ldst_info_t *info = (ldst_info_t *)get_irn_link(load);
	changes_t          res  = NO_CHANGES;

	/* do NOT touch volatile loads for now */
	if (get_Load_volatility(load) == volatility_is_volatile)
		return NO_CHANGES;

	/* the address of the load to be optimized */
	ir_node *ptr = get_Load_ptr(load);

	/* The mem of the Load. Must still be returned after optimization. */
	ir_node *mem = get_Load_mem(load);

	if (info->projs[pn_Load_res] == NULL
	    && info->projs[pn_Load_X_except] == NULL) {
		assert(info->projs[pn_Load_X_regular] == NULL);
		/* the value is never used and we don't care about exceptions, remove */
		exchange(info->projs[pn_Load_M], mem);
		kill_and_reduce_usage(load);
		return res | DF_CHANGED;
	}

	/* a load from an entity which is never written to can be replaced by
	 * the value of the entity's initializer. */
	ir_entity *entity = find_entity(ptr);
	if (entity != NULL && get_entity_kind(entity) == IR_ENTITY_NORMAL
	    && !(get_entity_usage(entity) & ir_usage_write)) {

		set_entity_linkage(entity, IR_LINKAGE_CONSTANT);
		ir_node *ptr  = get_Load_ptr(load);
		ir_mode *mode = get_Load_mode(load);
		ir_node *val  = predict_load(ptr, mode);
		if (val != NULL) {
			ldst_info_t *info = (ldst_info_t*)get_irn_link(load);
			exchange(info->projs[pn_Load_res], val);
			exchange(info->projs[pn_Load_M], get_Load_mem(load));
			kill_and_reduce_usage(load);
			return DF_CHANGED;
		}
	}

	track_load_env_t env = { .ptr = ptr };
	get_base_and_offset(ptr, &env.base_offset);

	/* Check, if the base address of this load is used more than once.
	 * If not, we won't find another store/load/CopyB anyway.
	 * TODO: can we miss values with multiple users in between? */
	if (get_irn_n_edges(ptr) <= 1 && get_irn_n_edges(env.base_offset.base) <= 1)
		return res;

	/*
	 * follow the memory chain as long as there are only Loads
	 * and try to replace current Load or Store by a previous one.
	 * Note that in unreachable loops it might happen that we reach
	 * load again, as well as we can fall into a cycle.
	 * We break such cycles using a special visited flag.
	 */
	INC_MASTER();
	env.load = load;
	res = follow_load_mem_chain(&env, skip_Proj(mem));
	return res;
}

/**
 * Check whether small is a part of large (starting at same address).
 */
static bool is_partially_same(ir_node *small, ir_node *large)
{
	const ir_mode *sm = get_irn_mode(small);
	const ir_mode *lm = get_irn_mode(large);

	/* FIXME: Check endianness */
	return is_Conv(small) && get_Conv_op(small) == large
	    && get_mode_size_bytes(sm) < get_mode_size_bytes(lm)
	    && get_mode_arithmetic(sm) == irma_twos_complement
	    && get_mode_arithmetic(lm) == irma_twos_complement;
}

/**
 * follow the memory chain as long as there are only Loads and alias free
 * Stores.
 * INC_MASTER() must be called before dive into
 */
static changes_t follow_store_mem_chain(ir_node *store, ir_node *start,
                                        bool had_split)
{
	changes_t    res   = NO_CHANGES;
	ldst_info_t *info  = (ldst_info_t *)get_irn_link(store);
	ir_node     *ptr   = get_Store_ptr(store);
	ir_node     *mem   = get_Store_mem(store);
	ir_node     *value = get_Store_value(store);
	ir_type     *type  = get_Store_type(store);
	unsigned     size  = get_mode_size_bytes(get_irn_mode(value));
	ir_node     *block = get_nodes_block(store);

	ir_node *node = start;
	while (node != store) {
		ldst_info_t *node_info = (ldst_info_t *)get_irn_link(node);

		/*
		 * BEWARE: one might think that checking the modes is useless, because
		 * if the pointers are identical, they refer to the same object.
		 * This is only true in strong typed languages, not is C were the
		 * following is possible *(ir_type1 *)p = a; *(ir_type2 *)p = b ...
		 * However, if the size of the mode that is written is bigger or equal
		 * the size of the old one, the old value is completely overwritten and
		 * can be killed ...
		 */
		if (is_Store(node) && !had_split && get_Store_ptr(node) == ptr &&
		    get_nodes_block(node) == block) {
			/*
			 * a Store after a Store in the same Block -- a write after write.
			 */

			/*
			 * We may remove the first Store, if the old value is completely
			 * overwritten or the old value is a part of the new value,
			 * and if it does not have an exception handler.
			 *
			 * TODO: What, if both have the same exception handler ???
			 */
			if (get_Store_volatility(node) != volatility_is_volatile
			    && !node_info->projs[pn_Store_X_except]) {
				ir_node  *predvalue = get_Store_value(node);
				ir_mode  *predmode  = get_irn_mode(predvalue);
				unsigned  predsize  = get_mode_size_bytes(predmode);

				if (predsize <= size || is_partially_same(predvalue, value)) {
					DBG_OPT_WAW(node, store);
					DB((dbg, LEVEL_1, "  killing store %+F (override by %+F)\n",
					    node, store));
					exchange(node_info->projs[pn_Store_M], get_Store_mem(node));
					kill_and_reduce_usage(node);
					return DF_CHANGED;
				}
			}

			/*
			 * We may remove the Store, if the old value already contains
			 * the new value, and if it does not have an exception handler.
			 *
			 * TODO: What, if both have the same exception handler ???
			 */
			if (get_Store_volatility(store) != volatility_is_volatile
			        && !info->projs[pn_Store_X_except]) {
				ir_node *predvalue = get_Store_value(node);

				if (is_partially_same(value, predvalue)) {
					DBG_OPT_WAW(node, store);
					DB((dbg, LEVEL_1, "  killing store %+F (override by %+F)\n",
					    node, store));
					exchange(info->projs[pn_Store_M], mem);
					kill_and_reduce_usage(store);
					return DF_CHANGED;
				}
			}
		} else if (is_Load(node) && get_Load_ptr(node) == ptr &&
		           value == node_info->projs[pn_Load_res]) {
			/*
			 * a Store of a value just loaded from the same address
			 * -- a write after read.
			 * We may remove the Store, if it does not have an exception
			 * handler.
			 */
			if (!info->projs[pn_Store_X_except]) {
				DBG_OPT_WAR(store, node);
				DB((dbg, LEVEL_1,
				    "  killing store %+F (read %+F from same address)\n",
				    store, node));
				exchange(info->projs[pn_Store_M], mem);
				kill_and_reduce_usage(store);
				return DF_CHANGED;
			}
		}

		if (is_Store(node)) {
			/* check if we can pass through this store */
			ir_node           *store_ptr   = get_Store_ptr(node);
			ir_node           *store_value = get_Store_value(node);
			unsigned           store_size  = get_mode_size_bytes(get_irn_mode(store_value));
			ir_type           *store_type  = get_Store_type(node);
			ir_alias_relation  rel         = get_alias_relation(
				store_ptr, store_type, store_size,
				ptr, type, size);
			/* if the might be an alias, we cannot pass this Store */
			if (rel != ir_no_alias)
				break;
			node = skip_Proj(get_Store_mem(node));
		} else if (is_Load(node)) {
			ir_node           *load_ptr  = get_Load_ptr(node);
			ir_type           *load_type = get_Load_type(node);
			unsigned           load_size = get_mode_size_bytes(get_Load_mode(node));
			ir_alias_relation  rel       = get_alias_relation(
				load_ptr, load_type, load_size,
				ptr, type, size);
			if (rel != ir_no_alias)
				break;

			node = skip_Proj(get_Load_mem(node));
		} else if (is_CopyB(node)) {
			ir_node           *copyb_src  = get_CopyB_src(node);
			ir_type           *copyb_type = get_CopyB_type(node);
			unsigned           copyb_size = get_type_size(copyb_type);
			ir_alias_relation  src_rel    = get_alias_relation(
				copyb_src, copyb_type, copyb_size,
				ptr, type, size);
			if (src_rel != ir_no_alias)
				break;
			ir_node           *copyb_dst = get_CopyB_dst(node);
			ir_alias_relation  dst_rel   = get_alias_relation(
				copyb_dst, copyb_type, copyb_size,
				ptr, type, size);
			if (dst_rel != ir_no_alias)
				break;
		} else {
			/* follow only Load chains */
			break;
		}

		/* check for cycles */
		if (NODE_VISITED(node_info))
			break;
		MARK_NODE(node_info);
	}

	if (is_Sync(node)) {
		/* handle all Sync predecessors */
		foreach_irn_in(node, i, in) {
			ir_node *skipped = skip_Proj(in);
			res |= follow_store_mem_chain(store, skipped, true);
			if (res != NO_CHANGES)
				break;
		}
	}
	return res;
}

/**
 * optimize a Store
 *
 * @param store  the Store node
 */
static changes_t optimize_store(ir_node *store)
{
	if (get_Store_volatility(store) == volatility_is_volatile)
		return NO_CHANGES;

	/* Check, if the address of this Store is used more than once.
	 * If not, this Store cannot be removed in any case. */
	ir_node *ptr = get_Store_ptr(store);
	if (get_irn_n_edges(ptr) <= 1)
		return NO_CHANGES;

	ir_node *mem = get_Store_mem(store);

	/* follow the memory chain as long as there are only Loads */
	INC_MASTER();

	return follow_store_mem_chain(store, skip_Proj(mem), false);
}

/**
 * Checks whether @c ptr of type @c ptr_type lies completely within an
 * object of type @c struct_type starting at @c struct_ptr;
 */
static bool ptr_is_in_struct(ir_node *ptr, ir_type *ptr_type,
                             ir_node *struct_ptr, ir_type *struct_type)
{
	base_offset_t base_offset;
	get_base_and_offset(ptr, &base_offset);
	long     ptr_offset    = base_offset.offset;
	base_offset_t struct_offset;
	get_base_and_offset(struct_ptr, &struct_offset);
	unsigned ptr_size      = get_type_size(ptr_type);
	unsigned struct_size   = get_type_size(struct_type);

	return base_offset.base == struct_offset.base &&
		ptr_offset >= struct_offset.offset &&
		ptr_offset + ptr_size <= struct_offset.offset + struct_size;
}

/**
 * Tries to optimize @c copyb. This function handles the following
 * cases:
 * - A previous Store that lies completely within @c copyb's destination
 *   will be deleted, except it modifies the @c copyb's source.
 * - If a previous CopyB writes to @c copyb's source, @c copyb will
 *   read from the previous CopyB's source if possible. Cases where
 *   the CopyB nodes are offset against each other are not handled.
 */
static changes_t follow_copyb_mem_chain(ir_node *copyb, ir_node *start,
                                        bool had_split)
{
	changes_t res   = NO_CHANGES;
	ir_node  *src   = get_CopyB_src(copyb);
	ir_node  *dst   = get_CopyB_dst(copyb);
	ir_type  *type  = get_CopyB_type(copyb);
	unsigned  size  = get_type_size(type);
	ir_node  *block = get_nodes_block(copyb);

	ir_node *node = start;
	while (node != copyb) {
		ldst_info_t *node_info = (ldst_info_t *)get_irn_link(node);

		if (is_Store(node)
		    && get_Store_volatility(node) != volatility_is_volatile
		    && !node_info->projs[pn_Store_X_except] && !had_split
		    && get_nodes_block(node) == block) {
			/* If the CopyB completely overwrites the Store,
			 * and the Store does not modify the CopyB's source memory,
			 * and the Store has no exception handler,
			 * the Store can be removed.
			 * This is basically a write-after-write. */
			ir_node           *store_ptr  = get_Store_ptr(node);
			ir_type           *store_type = get_Store_type(node);
			unsigned           store_size = get_mode_size_bytes(get_irn_mode(store_ptr));
			ir_alias_relation  src_rel    = get_alias_relation(
				src, type, size,
				store_ptr, store_type, store_size);

			if (src_rel == ir_no_alias
			    && ptr_is_in_struct(store_ptr, store_type, dst, type)) {
				DBG_OPT_WAW(node, copyb);
				DB((dbg, LEVEL_1, "  killing store %+F (override by %+F)\n",
				    node, copyb));
				exchange(node_info->projs[pn_Store_M], get_Store_mem(node));
				kill_and_reduce_usage(node);
				return DF_CHANGED;
			}
		}

		if (is_Store(node)) {
			ir_node  *store_ptr   = get_Store_ptr(node);
			ir_type  *store_type  = get_Store_type(node);
			ir_node  *store_value = get_Store_value(node);
			unsigned  store_size  = get_mode_size_bytes(get_irn_mode(store_value));
			/* check if we can pass through this store */
			ir_alias_relation src_rel = get_alias_relation(
				store_ptr, store_type, store_size,
				src, type, size);
			if (src_rel != ir_no_alias)
				break;
			ir_alias_relation dst_rel = get_alias_relation(
				store_ptr, store_type, store_size,
				dst, type, size);
			if (dst_rel != ir_no_alias)
				break;

			node = skip_Proj(get_Store_mem(node));
		} else if (is_Load(node)) {
			ir_node  *load_ptr  = get_Load_ptr(node);
			ir_type  *load_type = get_Load_type(node);
			unsigned  load_size = get_mode_size_bytes(get_Load_mode(node));
			ir_alias_relation rel = get_alias_relation(
				load_ptr, load_type, load_size,
				dst, type, size);
			if (rel != ir_no_alias)
				break;

			node = skip_Proj(get_Load_mem(node));
		} else if (is_CopyB(node)) {
			ir_node  *pred_dst  = get_CopyB_dst(node);
			ir_node  *pred_src  = get_CopyB_src(node);
			ir_type  *pred_type = get_CopyB_type(node);
			unsigned  pred_size = get_type_size(pred_type);

			if (src == pred_dst && size == pred_size
			    && get_CopyB_volatility(node) == volatility_non_volatile) {
				src = pred_src;

				/* Special case: If src now points to a constant, we *can*
				 * replace it immediately. */
				ir_entity *entity = find_entity(src);
				if (entity != NULL
				    && (get_entity_linkage(entity) & IR_LINKAGE_CONSTANT)) {
					DBG((dbg, LEVEL_1,
					     "Optimizing %+F to read from %+F's source\n",
					     copyb, node));

					set_CopyB_src(copyb, src);
					return res | DF_CHANGED;
				}
			} else {
				ir_alias_relation dst_dst_rel = get_alias_relation(
					pred_dst, pred_type, pred_size,
					dst, type, size);
				if (dst_dst_rel != ir_no_alias)
					break;
				ir_alias_relation src_dst_rel = get_alias_relation(
					pred_src, pred_type, pred_size,
					dst, type, size);
				if (src_dst_rel != ir_no_alias)
					break;
				ir_alias_relation dst_src_rel = get_alias_relation(
					pred_dst, pred_type, pred_size,
					src, type, size);
				if (dst_src_rel != ir_no_alias)
					break;
			}

			node = skip_Proj(get_CopyB_mem(node));
		} else {
			break;
		}

		/* check for cycles */
		if (NODE_VISITED(node_info))
			break;
		MARK_NODE(node_info);
	}

	if (is_Sync(node)) {
		/* handle all Sync predecessors */
		foreach_irn_in(node, i, in) {
			ir_node *skipped = skip_Proj(in);
			res |= follow_copyb_mem_chain(copyb, skipped, true);
			if (res != NO_CHANGES)
				break;
		}
	}
	return res;
}

/**
 * Optimizes a CopyB node.
 *
 * @param copyb  the CopyB node
 */
static changes_t optimize_copyb(ir_node *copyb)
{
	if (get_CopyB_volatility(copyb) == volatility_is_volatile)
		return NO_CHANGES;

	ir_node *mem = get_CopyB_mem(copyb);

	INC_MASTER();

	return follow_copyb_mem_chain(copyb, skip_Proj(mem), false);
}

/* check if a node has more than one real user. Keepalive edges do not count as
 * real users */
static bool has_multiple_users(const ir_node *node)
{
	unsigned real_users = 0;
	foreach_out_edge(node, edge) {
		ir_node *user = get_edge_src_irn(edge);
		if (is_End(user))
			continue;
		++real_users;
		if (real_users > 1)
			return true;
	}
	return false;
}

static ir_cons_flags get_store_cons_flags(ir_node *const store)
{
	ir_cons_flags cons = cons_none;
	if (get_Store_volatility(store) != volatility_non_volatile)
		cons |= cons_volatile;
	if (get_Store_unaligned(store) != align_is_aligned)
		cons |= cons_unaligned;
	if (get_irn_pinned(store) == op_pin_state_floats)
		cons |= cons_floats;
	if (ir_throws_exception(store))
		cons |= cons_throws_exception;
	return cons;
}

/**
 * walker, optimizes Phi after Stores to identical places:
 * Does the following optimization:
 * @verbatim
 *
 *   val1   val2   val3          val1  val2  val3
 *    |      |      |               \    |    /
 *  Store  Store  Store              \   |   /
 *      \    |    /                   PhiData
 *       \   |   /                       |
 *        \  |  /                      Store
 *          PhiM
 *
 * @endverbatim
 * This reduces the number of stores and allows for predicated execution.
 * Moves Stores back to the end of a function which may be bad.
 *
 * This is only possible if the predecessor blocks have only one successor.
 */
static changes_t optimize_phi(ir_node *phi, walk_env_t *wenv)
{
	/* Must be a memory Phi */
	if (get_irn_mode(phi) != mode_M)
		return NO_CHANGES;

	/* Phi must have at least one operand and it is pointless to distribute over a
	 * Phi with exactly one operand.  It would needlessly create a new Store and
	 * lose the debug info of the Store. */
	int n = get_Phi_n_preds(phi);
	if (n <= 1)
		return NO_CHANGES;

	/* must be only one user */
	ir_node *projM = get_Phi_pred(phi, 0);
	if (has_multiple_users(projM))
		return NO_CHANGES;

	ir_node *store = skip_Proj(projM);
	if (!is_Store(store))
		return NO_CHANGES;

	/* check if the block is post dominated by Phi-block
	   and has no exception exit */
	ir_node      *block   = get_nodes_block(store);
	block_info_t *bl_info = (block_info_t *)get_irn_link(block);
	if (bl_info->flags & BLOCK_HAS_EXC)
		return NO_CHANGES;

	ir_node *phi_block = get_nodes_block(phi);
	if (!block_strictly_postdominates(phi_block, block))
		return NO_CHANGES;

	/* this is the address of the store */
	ir_node      *const ptr  = get_Store_ptr(store);
	ir_mode      *const mode = get_irn_mode(get_Store_value(store));
	ir_cons_flags const cons = get_store_cons_flags(store);
	ldst_info_t        *info = (ldst_info_t *)get_irn_link(store);
	ir_node      *const exc  = info->exc_block;

	for (int i = 1; i < n; ++i) {
		ir_node *pred = get_Phi_pred(phi, i);
		if (has_multiple_users(pred))
			return NO_CHANGES;

		pred = skip_Proj(pred);
		if (!is_Store(pred))
			return NO_CHANGES;

		if (ptr != get_Store_ptr(pred)
		    || mode != get_irn_mode(get_Store_value(pred)))
			return NO_CHANGES;
		if (cons != get_store_cons_flags(pred))
			return NO_CHANGES;

		/* check, if all stores have the same exception flow */
		info = (ldst_info_t *)get_irn_link(pred);
		if (exc != info->exc_block)
			return NO_CHANGES;


		/* check if the block is post dominated by Phi-block
		   and has no exception exit. Note that block must be different from
		   Phi-block, else we would move a Store from end End of a block to its
		   Start... */
		ir_node      *block   = get_nodes_block(pred);
		block_info_t *bl_info = (block_info_t *)get_irn_link(block);
		if (bl_info->flags & BLOCK_HAS_EXC)
			return NO_CHANGES;
		if (!block_strictly_postdominates(phi_block, block))
			return NO_CHANGES;
	}

	/*
	 * ok, when we are here, we found all predecessors of a Phi that
	 * are Stores to the same address and size. That means whatever
	 * we do before we enter the block of the Phi, we do a Store.
	 * So, we can move the Store to the current block:
	 *
	 *   val1    val2    val3          val1  val2  val3
	 *    |       |       |               \    |    /
	 * | Str | | Str | | Str |             \   |   /
	 *      \     |     /                   PhiData
	 *       \    |    /                       |
	 *        \   |   /                       Str
	 *           PhiM
	 *
	 * Is only allowed if the predecessor blocks have only one successor.
	 */
	ir_node **projMs = ALLOCAN(ir_node*, n);
	ir_node **inM    = ALLOCAN(ir_node*, n);
	ir_node **inD    = ALLOCAN(ir_node*, n);
	ir_type  *type   = NULL;
	int      *idx    = ALLOCAN(int, n);

	/* Prepare: Collect all Store nodes.  We must do this
	   first because we otherwise may loose a store when exchanging its
	   memory Proj.
	 */
	for (int i = n; i-- > 0; ) {
		projMs[i] = get_Phi_pred(phi, i);

		ir_node *const store = get_Proj_pred(projMs[i]);
		info  = (ldst_info_t *)get_irn_link(store);

		inM[i] = get_Store_mem(store);
		inD[i] = get_Store_value(store);
		idx[i] = info->exc_idx;

		if (i == n - 1) {
			type = get_Store_type(store);
		} else if (get_Store_type(store) != type) {
			/* We cannot merge Stores with different types. */
			return NO_CHANGES;
		}
	}

	/* second step: remove keep edge from old Phi and create a new memory Phi */
	ir_node *phiM = get_Phi_loop(phi) ? new_r_Phi_loop(phi_block, n, inM)
	                                  : new_r_Phi(phi_block, n, inM, mode_M);

	/* third step: create a new data Phi */
	ir_node *phiD = new_r_Phi(phi_block, n, inD, mode);

	/* fourth step: create the Store */
	store = new_r_Store(phi_block, phiM, ptr, phiD, type, cons);
	projM = new_r_Proj(store, mode_M, pn_Store_M);

	/* rewire memory and kill the old nodes */
	for (int i = n; i-- > 0; ) {
		ir_node *proj = projMs[i];
		assert(is_Proj(proj));
		ir_node *store = get_Proj_pred(proj);
		exchange(proj, inM[i]);
		kill_and_reduce_usage(store);
	}

	info = get_ldst_info(store, &wenv->obst);
	info->projs[pn_Store_M] = projM;

	/* fifths step: repair exception flow */
	changes_t res = NO_CHANGES;
	if (exc) {
		ir_node *projX = new_r_Proj(store, mode_X, pn_Store_X_except);

		info->projs[pn_Store_X_except] = projX;
		info->exc_block                = exc;
		info->exc_idx                  = idx[0];

		for (int i = 0; i < n; ++i) {
			set_Block_cfgpred(exc, idx[i], projX);
		}

		if (n > 1) {
			/* the exception block should be optimized as some inputs are
			 * identical now */
		}

		res |= CF_CHANGED;
	}

	/* sixth step: replace old Phi */
	if (get_Phi_loop(phi))
		remove_keep_alive(phi);
	exchange(phi, projM);

	return res | DF_CHANGED;
}

static changes_t optimize_conv_load(ir_node *conv)
{
	ir_node *op = get_Conv_op(conv);
	if (!is_Proj(op))
		return NO_CHANGES;
	if (has_multiple_users(op))
		return NO_CHANGES;
	/* shrink mode of load if possible. */
	ir_node *load = get_Proj_pred(op);
	if (!is_Load(load))
		return NO_CHANGES;

	/* only do it if we are the only user (otherwise the risk is too
	 * great that we end up with 2 loads instead of one). */
	ir_mode *mode      = get_irn_mode(conv);
	ir_mode *load_mode = get_Load_mode(load);
	int      bits_diff
		= get_mode_size_bits(load_mode) - get_mode_size_bits(mode);
	if (mode_is_float(load_mode) || mode_is_float(mode) || bits_diff < 0)
	    return NO_CHANGES;

	if (ir_target_big_endian()) {
		if (bits_diff % 8 != 0)
			return NO_CHANGES;
		ir_graph *irg       = get_irn_irg(conv);
		ir_node  *ptr       = get_Load_ptr(load);
		ir_mode  *mode      = get_irn_mode(ptr);
		ir_mode  *mode_offs = get_reference_offset_mode(mode);
		ir_node  *delta     = new_r_Const_long(irg, mode_offs, bits_diff/8);
		ir_node  *block     = get_nodes_block(load);
		ir_node  *add       = new_r_Add(block, ptr, delta);
		set_Load_ptr(load, add);
	}
	set_Load_mode(load, mode);
	set_irn_mode(op, mode);
	exchange(conv, op);
	return DF_CHANGED;
}

/**
 * walker, do the optimizations
 */
static void do_load_store_optimize(ir_node *n, void *env)
{
	walk_env_t *wenv = (walk_env_t *)env;
	switch (get_irn_opcode(n)) {
	case iro_Load:  wenv->changes |= optimize_load(n);      break;
	case iro_Store: wenv->changes |= optimize_store(n);     break;
	case iro_CopyB: wenv->changes |= optimize_copyb(n);     break;
	case iro_Phi:   wenv->changes |= optimize_phi(n, wenv); break;
	case iro_Conv:  wenv->changes |= optimize_conv_load(n); break;
	default:
		break;
	}
}

/**
 * Check if a store is dead and eliminate it.
 *
 * @param store  the Store node
 */
static changes_t eliminate_dead_store(ir_node *store)
{
	if (get_Store_volatility(store) == volatility_is_volatile)
		return NO_CHANGES;

	ir_node   *ptr    = get_Store_ptr(store);
	ir_entity *entity = find_entity(ptr);

	/* a store to an entity which is never read is unnecessary */
	if (entity != NULL && !(get_entity_usage(entity) & ir_usage_read)) {
		ldst_info_t *info = (ldst_info_t *)get_irn_link(store);

		if (info->projs[pn_Store_X_except] == NULL) {
			DB((dbg, LEVEL_1,
			    "  Killing useless %+F to never read entity %+F\n", store,
			    entity));
			exchange(info->projs[pn_Store_M], get_Store_mem(store));
			kill_and_reduce_usage(store);
			return DF_CHANGED;
		}
	}

	return NO_CHANGES;
}

static changes_t eliminate_dead_copyb(ir_node *copyb)
{
	if (get_CopyB_volatility(copyb) == volatility_is_volatile)
		return NO_CHANGES;

	/* a CopyB whose destination is never read is unnecessary */
	ir_node   *ptr    = get_CopyB_dst(copyb);
	ir_entity *entity = find_entity(ptr);
	if (entity != NULL && !(get_entity_usage(entity) & ir_usage_read)) {
		DB((dbg, LEVEL_1, "  Killing useless %+F to never read entity %+F\n",
		    copyb, entity));
		reduce_node_usage(get_CopyB_dst(copyb));
		reduce_node_usage(get_CopyB_src(copyb));
		exchange(copyb, get_CopyB_mem(copyb));
		return DF_CHANGED;
	}

	return NO_CHANGES;
}

/**
 * walker to eliminate dead stores.
 */
static void do_eliminate_dead_stores(ir_node *n, void *env)
{
	walk_env_t *wenv = (walk_env_t *)env;
	if (is_Store(n)) {
		wenv->changes |= eliminate_dead_store(n);
	} else if (is_CopyB(n)) {
		wenv->changes |= eliminate_dead_copyb(n);
	}
}

/** A scc. */
typedef struct scc {
	ir_node *head;      /**< the head of the list */
} scc;

/** A node entry. */
typedef struct node_entry {
	unsigned DFSnum;    /**< the DFS number of this node */
	unsigned low;       /**< the low number of this node */
	bool     in_stack;  /**< flag, set if the node is on the stack */
	ir_node *next;      /**< link to the next node the the same scc */
	scc     *pscc;      /**< the scc of this node */
	unsigned POnum;     /**< the post order number for blocks */
} node_entry;

/** A loop entry. */
typedef struct loop_env {
	ir_nodehashmap_t map;
	struct obstack   obst;
	ir_node        **stack;        /**< the node stack */
	size_t           tos;          /**< tos index */
	unsigned         nextDFSnum;   /**< the current DFS number */
	unsigned         POnum;        /**< current post order number */

	changes_t        changes;      /**< a bitmask of graph changes */
} loop_env;

/**
 * Gets the node_entry of a node
 */
static node_entry *get_irn_ne(ir_node *irn, loop_env *env)
{
	node_entry *e = ir_nodehashmap_get(node_entry, &env->map, irn);

	if (e == NULL) {
		e = OALLOC(&env->obst, node_entry);
		memset(e, 0, sizeof(*e));
		ir_nodehashmap_insert(&env->map, irn, e);
	}
	return e;
}

/**
 * Push a node onto the stack.
 *
 * @param env   the loop environment
 * @param n     the node to push
 */
static void push(loop_env *env, ir_node *n)
{
	if (env->tos == ARR_LEN(env->stack)) {
		size_t nlen = ARR_LEN(env->stack) * 2;
		ARR_RESIZE(ir_node*, env->stack, nlen);
	}
	env->stack[env->tos++] = n;
	node_entry *e = get_irn_ne(n, env);
	e->in_stack = true;
}

/**
 * pop a node from the stack
 *
 * @param env   the loop environment
 *
 * @return  The topmost node
 */
static ir_node *pop(loop_env *env)
{
	ir_node    *n = env->stack[--env->tos];
	node_entry *e = get_irn_ne(n, env);
	e->in_stack = false;
	return n;
}

/**
 * Check if irn is a region constant.
 * The block or irn must strictly dominate the header block.
 *
 * @param irn           the node to check
 * @param header_block  the header block of the induction variable
 */
static bool is_rc(ir_node *irn, ir_node *header_block)
{
	ir_node *block = get_nodes_block(irn);
	return (block != header_block) && block_dominates(block, header_block);
}

typedef struct phi_entry phi_entry;
struct phi_entry {
	ir_node   *phi;    /**< A phi with a region const memory. */
	int        pos;    /**< The position of the region const memory */
	ir_node   *load;   /**< the newly created load for this phi */
	phi_entry *next;
};

/**
 * An entry in the avail set.
 */
typedef struct avail_entry_t {
	ir_node *ptr;   /**< the address pointer */
	ir_mode *mode;  /**< the load mode */
	ir_node *load;  /**< the associated Load */
} avail_entry_t;

/**
 * Compare two avail entries.
 */
static int cmp_avail_entry(const void *elt, const void *key, size_t size)
{
	(void)size;
	const avail_entry_t *a = (const avail_entry_t *)elt;
	const avail_entry_t *b = (const avail_entry_t *)key;
	return a->ptr != b->ptr || a->mode != b->mode;
}

/**
 * Calculate the hash value of an avail entry.
 */
static unsigned hash_cache_entry(const avail_entry_t *entry)
{
	return get_irn_idx(entry->ptr)*9 + hash_ptr(entry->mode);
}

/**
 * Move loops out of loops if possible.
 *
 * @param pscc   the loop described by an SCC
 * @param env    the loop environment
 */
static void move_loads_out_of_loops(scc *pscc, loop_env *env)
{
	/* collect all outer memories */
	phi_entry *phi_list = NULL;
	for (ir_node *phi = pscc->head, *next; phi != NULL; phi = next) {
		node_entry *ne = get_irn_ne(phi, env);
		next = ne->next;

		/* check all memory Phi's */
		if (!is_Phi(phi))
			continue;

		assert(get_irn_mode(phi) == mode_M && "DFS return non-memory Phi");

		foreach_irn_in_r(phi, j, pred) {
			node_entry *const pe = get_irn_ne(pred, env);

			if (pe->pscc != ne->pscc) {
				/* not in the same SCC, is region const */
				phi_entry *pe = OALLOC(&env->obst, phi_entry);

				pe->phi  = phi;
				pe->pos  = j;
				pe->next = phi_list;
				phi_list = pe;
			}
		}
	}
	/* no Phis no fun */
	assert(phi_list != NULL && "DFS found a loop without Phi");

	/* for now, we cannot handle more than one input (only reducible cf) */
	if (phi_list->next != NULL)
		return;

	set *avail = new_set(cmp_avail_entry, 8);

	for (ir_node *load = pscc->head, *next; load != NULL; load = next) {
		node_entry *ne = get_irn_ne(load, env);
		next = ne->next;

		if (is_Load(load)) {
			ldst_info_t *info = (ldst_info_t *)get_irn_link(load);
			ir_node     *ptr = get_Load_ptr(load);

			/* for now, we cannot handle Loads with exceptions */
			if (info->projs[pn_Load_res] == NULL
			    || info->projs[pn_Load_X_regular] != NULL
			    || info->projs[pn_Load_X_except] != NULL)
				continue;

			/* for now, we can only move Load(Global) */
			if (!is_Address(ptr))
				continue;
			ir_type  *load_type  = get_Load_type(load);
			ir_mode  *load_mode  = get_Load_mode(load);
			unsigned  load_size  = get_mode_size_bytes(load_mode);
			ir_node  *other;
			ir_node  *next_other;
			for (other = pscc->head; other != NULL; other = next_other) {
				node_entry *ne = get_irn_ne(other, env);
				next_other = ne->next;

				if (is_Store(other)) {
					ir_node *store_ptr   = get_Store_ptr(other);
					ir_node *store_value = get_Store_value(other);
					ir_type *store_type  = get_Store_type(other);
					unsigned store_size  = get_mode_size_bytes(get_irn_mode(store_value));

					ir_alias_relation rel = get_alias_relation(
						store_ptr, store_type, store_size,
						ptr, load_type, load_size);
					/* if the might be an alias, we cannot pass this Store */
					if (rel != ir_no_alias)
						break;
				}
				/* only Phis and pure Calls are allowed here, so ignore them */
			}
			if (other == NULL) {
				ldst_info_t *ninfo = NULL;

				/* yep, no aliasing Store found, Load can be moved */
				DB((dbg, LEVEL_1, "  Found a Load that could be moved: %+F\n",
				    load));

				dbg_info *db = get_irn_dbg_info(load);
				for (phi_entry *pe = phi_list; pe != NULL; pe = pe->next) {
					int      pos  = pe->pos;
					ir_node *phi  = pe->phi;
					ir_node *blk  = get_nodes_block(phi);
					ir_node *pred = get_Block_cfgpred_block(blk, pos);

					avail_entry_t  entry = { .ptr = ptr, .mode = load_mode };
					avail_entry_t *res
						= set_find(avail_entry_t, avail, &entry, sizeof(entry), hash_cache_entry(&entry));
					ir_node *irn;
					if (res != NULL) {
						irn = res->load;
					} else {
						irn        = new_rd_Load(db, pred, get_Phi_pred(phi, pos), ptr, load_mode, load_type, cons_none);
						entry.load = irn;
						(void)set_insert(avail_entry_t, avail, &entry, sizeof(entry), hash_cache_entry(&entry));
						DB((dbg, LEVEL_1, "  Created %+F in %+F\n", irn, pred));
					}
					pe->load = irn;
					ninfo    = get_ldst_info(irn, &env->obst);

					ir_node *mem = new_r_Proj(irn, mode_M, pn_Load_M);
					ninfo->projs[pn_Load_M] = mem;
					if (res == NULL) {
						/* irn is from cache, so do not set phi pred again.
						 * There might be other Loads between phi and irn
						 * already. */
						set_Phi_pred(phi, pos, mem);
					}

					ninfo->projs[pn_Load_res] = new_r_Proj(irn, load_mode, pn_Load_res);
				}

				/* now kill the old Load */
				exchange(info->projs[pn_Load_M], get_Load_mem(load));
				exchange(info->projs[pn_Load_res], ninfo->projs[pn_Load_res]);

				env->changes |= DF_CHANGED;
			}
		}
	}
	del_set(avail);
}

/**
 * Process a loop SCC.
 *
 * @param pscc  the SCC
 * @param env   the loop environment
 */
static void process_loop(scc *pscc, loop_env *env)
{
	/* find the header block for this scc */
	ir_node    *header = NULL;
	node_entry *h      = NULL;
	for (ir_node *irn = pscc->head, *next; irn != NULL; irn = next) {
		node_entry *e = get_irn_ne(irn, env);
		next = e->next;

		ir_node    *block = get_nodes_block(irn);
		node_entry *b     = get_irn_ne(block, env);
		if (header != NULL) {
			if (h->POnum < b->POnum) {
				header = block;
				h      = b;
			}
		} else {
			header = block;
			h      = b;
		}
	}

	/* check if this scc contains only Phi, Loads or Stores nodes */
	bool     only_phi    = true;
	unsigned num_outside = 0;
	bool     process     = false;
	ir_node *out_rc      = NULL;
	for (ir_node *irn = pscc->head, *next; irn != NULL; irn = next) {
		node_entry *e = get_irn_ne(irn, env);
		next = e->next;

		switch (get_irn_opcode(irn)) {
		case iro_Call:
			if (is_irn_const_memory(irn)) {
				/* pure calls can be treated like loads */
				only_phi = false;
				break;
			}
			/* non-pure calls must be handle like may-alias Stores */
			goto fail;
		case iro_CopyB:
			/* cannot handle CopyB yet */
			goto fail;
		case iro_Load:
			process = true;
			if (get_Load_volatility(irn) == volatility_is_volatile) {
				/* cannot handle loops with volatile Loads */
				goto fail;
			}
			only_phi = false;
			break;
		case iro_Store:
			if (get_Store_volatility(irn) == volatility_is_volatile) {
				/* cannot handle loops with volatile Stores */
				goto fail;
			}
			only_phi = false;
			break;
		default:
			only_phi = false;
			break;
		case iro_Phi:
			foreach_irn_in_r(irn, i, pred) {
				node_entry *pe = get_irn_ne(pred, env);

				if (pe->pscc != e->pscc) {
					/* not in the same SCC, must be a region const */
					if (!is_rc(pred, header)) {
						/* not a memory loop */
						goto fail;
					}
					if (out_rc == NULL) {
						/* first region constant */
						out_rc = pred;
						++num_outside;
					} else if (out_rc != pred) {
						/* another region constant */
						++num_outside;
					}
				}
			}
			break;
		}
	}
	if (!process)
		goto fail;

	/* found a memory loop */
	DB((dbg, LEVEL_2, "  Found a memory loop:\n  "));
	if (only_phi && num_outside == 1) {
		/* a phi cycle with only one real predecessor can be collapsed */
		DB((dbg, LEVEL_2, "  Found a USELESS Phi cycle:\n  "));

		for (ir_node *irn = pscc->head, *next; irn != NULL; irn = next) {
			node_entry *e = get_irn_ne(irn, env);
			next = e->next;
			exchange(irn, out_rc);
		}
		env->changes |= DF_CHANGED;
		return;
	}

#ifdef DEBUG_libfirm
	for (ir_node *irn = pscc->head, *next; irn != NULL; irn = next) {
		node_entry *e = get_irn_ne(irn, env);
		next = e->next;
		DB((dbg, LEVEL_2, " %+F,", irn));
	}
	DB((dbg, LEVEL_2, "\n"));
#endif
	move_loads_out_of_loops(pscc, env);

fail:;
}

/**
 * Process a SCC.
 *
 * @param pscc  the SCC
 * @param env   the loop environment
 */
static void process_scc(scc *pscc, loop_env *env)
{
	ir_node    *head = pscc->head;
	node_entry *e    = get_irn_ne(head, env);

#ifdef DEBUG_libfirm
	DB((dbg, LEVEL_4, " SCC at %p:\n ", pscc));
	for (ir_node *irn = pscc->head, *next; irn != NULL; irn = next) {
		node_entry *e = get_irn_ne(irn, env);
		next = e->next;

		DB((dbg, LEVEL_4, " %+F,", irn));
	}
	DB((dbg, LEVEL_4, "\n"));
#endif

	if (e->next != NULL) {
		/* this SCC has more than one member */
		process_loop(pscc, env);
	}
}

/**
 * Do Tarjan's SCC algorithm and drive load/store optimization.
 *
 * @param irn  start at this node
 * @param env  the loop environment
 */
static void dfs(ir_node *irn, loop_env *env)
{
	mark_irn_visited(irn);

	node_entry *node = get_irn_ne(irn, env);
	node->DFSnum = env->nextDFSnum++;
	node->low    = node->DFSnum;
	push(env, irn);

	/* handle preds */
	if (is_Phi(irn) || is_Sync(irn)) {
		foreach_irn_in(irn, i, pred) {
			node_entry *o = get_irn_ne(pred, env);

			if (!irn_visited(pred)) {
				dfs(pred, env);
				node->low = MIN(node->low, o->low);
			}
			if (o->DFSnum < node->DFSnum && o->in_stack)
				node->low = MIN(o->DFSnum, node->low);
		}
	} else if (is_fragile_op(irn)) {
		ir_node    *pred = get_memop_mem(irn);
		node_entry *o    = get_irn_ne(pred, env);

		if (!irn_visited(pred)) {
			dfs(pred, env);
			node->low = MIN(node->low, o->low);
		}
		if (o->DFSnum < node->DFSnum && o->in_stack)
			node->low = MIN(o->DFSnum, node->low);
	} else if (is_Proj(irn)) {
		ir_node    *pred = get_Proj_pred(irn);
		node_entry *o    = get_irn_ne(pred, env);

		if (!irn_visited(pred)) {
			dfs(pred, env);
			node->low = MIN(node->low, o->low);
		}
		if (o->DFSnum < node->DFSnum && o->in_stack)
			node->low = MIN(o->DFSnum, node->low);
	} else {
		 /* IGNORE predecessors */
	}

	if (node->low == node->DFSnum) {
		scc *pscc = OALLOC(&env->obst, scc);
		pscc->head = NULL;
		ir_node *x;
		do {
			x = pop(env);
			node_entry *e = get_irn_ne(x, env);
			e->pscc    = pscc;
			e->next    = pscc->head;
			pscc->head = x;
		} while (x != irn);

		process_scc(pscc, env);
	}
}

/**
 * Do the DFS on the memory edges a graph.
 *
 * @param irg  the graph to process
 * @param env  the loop environment
 */
static void do_dfs(ir_graph *irg, loop_env *env)
{
	inc_irg_visited(irg);

	/* visit all memory nodes */
	ir_node *endblk = get_irg_end_block(irg);
	for (int i = get_Block_n_cfgpreds(endblk); i-- > 0; ) {
		ir_node *pred = get_Block_cfgpred(endblk, i);

		pred = skip_Proj(pred);
		if (is_Return(pred)) {
			dfs(get_Return_mem(pred), env);
		} else if (is_Raise(pred)) {
			dfs(get_Raise_mem(pred), env);
		} else if (is_fragile_op(pred)) {
			dfs(get_memop_mem(pred), env);
		} else if (is_Bad(pred)) {
			/* ignore non-optimized block predecessor */
		} else {
			panic("unknown EndBlock predecessor");
		}
	}

	/* visit the keep-alives */
	ir_node *end = get_irg_end(irg);
	for (int i = get_End_n_keepalives(end); i-- > 0; ) {
		ir_node *ka = get_End_keepalive(end, i);

		if (is_Phi(ka) && !irn_visited(ka))
			dfs(ka, env);
	}
}

/**
 * Optimize Loads/Stores in loops.
 *
 * @param irg  the graph
 */
static changes_t optimize_loops(ir_graph *irg)
{
	loop_env env = { .stack = NEW_ARR_F(ir_node*, 128), .changes = NO_CHANGES };
	ir_nodehashmap_init(&env.map);
	obstack_init(&env.obst);

	/* calculate the SCC's and drive loop optimization. */
	do_dfs(irg, &env);

	DEL_ARR_F(env.stack);
	obstack_free(&env.obst, NULL);
	ir_nodehashmap_destroy(&env.map);

	return env.changes;
}

static ir_node *get_ptr(const ir_node *node)
{
	if (is_Store(node))
		return get_Store_ptr(node);
	if (is_Load(node))
		return get_Load_ptr(node);
	return NULL;
}

static int cmp_ptrs(const void *p0, const void *p1)
{
	ir_node *const *n0 = (ir_node*const *)p0;
	ir_node *const *n1 = (ir_node*const *)p1;

	ir_node *ptr0 = get_ptr(skip_Proj(*n0));
	ir_node *ptr1 = get_ptr(skip_Proj(*n1));
	if (ptr0 == NULL || ptr1 == NULL) {
		long num0 = get_irn_node_nr(*n0);
		long num1 = get_irn_node_nr(*n1);
		return (num0 > num1) - (num0 < num1);
	}

	base_offset_t bo0;
	base_offset_t bo1;
	get_base_and_offset(ptr0, &bo0);
	get_base_and_offset(ptr1, &bo1);
	if (bo0.base != bo1.base)
		return (bo0.base > bo1.base) - (bo0.base < bo1.base);
	return (bo0.offset > bo1.offset) - (bo0.offset < bo1.offset);
}

static void combine_memop(ir_node *sync, void *data)
{
	(void)data;
	if (!is_Sync(sync))
		return;

	unsigned   pointer_size = ir_target_pointer_size();
	int        n_preds      = get_Sync_n_preds(sync);
	ir_node  **new_in       = ALLOCAN(ir_node*, n_preds);
	MEMCPY(new_in, get_irn_in(sync), n_preds);

	QSORT(new_in, n_preds, cmp_ptrs);

	bool changed = false;
	for (int i = 0; i < n_preds-1; ++i) {
again:;
		ir_node *pred0 = new_in[i];
		if (pred0 == NULL)
			continue;

		ir_node *skipped = skip_Proj(pred0);
		ir_node *store0  = skipped;
		if (is_Store(store0)) {
			if (get_Store_volatility(store0) == volatility_is_volatile
			 || ir_throws_exception(store0))
				continue;
			ir_node *store_val  = get_Store_value(store0);
			ir_mode *store_mode = get_irn_mode(store_val);
			if (get_mode_arithmetic(store_mode) != irma_twos_complement)
				continue;
			unsigned store_size = get_mode_size_bytes(store_mode);
			if (store_size >= pointer_size || !is_po2_or_zero(store_size))
				continue;
			ir_mode *mode_unsigned = find_unsigned_mode(store_mode);
			if (mode_unsigned == NULL)
				continue;
			ir_mode *double_mode = find_double_bits_int_mode(mode_unsigned);
			if (double_mode == NULL)
				continue;

			ir_node *store_ptr0 = get_Store_ptr(store0);
			base_offset_t base0;
			get_base_and_offset(store_ptr0, &base0);
			ir_node *mem = get_Store_mem(store0);

			/* found a candidate, the only store we can merge with
			 * must be the next in the list */
			ir_node *pred1 = NULL;
			int i2 = i + 1;
			for ( ; i2 < n_preds; ++i2) {
				pred1 = new_in[i2];
				if (pred1 != NULL)
					break;
			}
			if (pred1 == NULL)
				continue;
			ir_node *store1 = skip_Proj(pred1);
			if (!is_Store(store1)
			 || get_Store_volatility(store1) == volatility_is_volatile
			 || ir_throws_exception(store1)
			 || get_Store_mem(store1) != mem)
				continue;
			ir_node *store_val1  = get_Store_value(store1);
			ir_mode *store_mode1 = get_irn_mode(store_val1);
			if (get_mode_arithmetic(store_mode1) != irma_twos_complement
			 || get_mode_size_bytes(store_mode1) != store_size)
				continue;

			ir_node *store_ptr1 = get_Store_ptr(store1);
			base_offset_t base1;
			get_base_and_offset(store_ptr1, &base1);
			if (base0.base   != base1.base
			 || base1.offset != base0.offset + (long)(store_size))
				continue;

			/* sort values according to endianess */
			if (ir_target_big_endian()) {
				ir_node *tmp = store_val;
				store_val  = store_val1;
				store_val1 = tmp;
			}

			/* Abort optimisation if we can't guarantee that the extra
			 * arithmetic code below will disappear. */
			if (!is_Const(store_val1)) {
				if (!is_Shr(store_val1))
					continue;
				ir_node *shiftval = get_Shr_right(store_val1);
				if (!is_Const(shiftval))
					continue;
				ir_tarval *tv = get_Const_tarval(shiftval);
				if (!tarval_is_long(tv)
				    || get_tarval_long(tv) != (long)store_size*8)
					continue;
			}

			/* Combine types if necessary */
			ir_type *type0 = get_Store_type(store0);
			ir_type *type1 = get_Store_type(store1);
			ir_type *type;
			if (type0 != type1) {
				/* Construct an anonymous struct type
				 * for the combined Store.
				 */
				ident *struct_id = id_unique("__combined_Store");
				type = new_type_struct(struct_id);
				ident *member0_id = id_unique("__Store_part");
				new_entity(type, member0_id, type0);
				ident *member1_id = id_unique("__Store_part");
				new_entity(type, member1_id, type1);
			} else {
				type = type0;
			}

			/* combine values */
			dbg_info *dbgi   = get_irn_dbg_info(store0);
			ir_node  *block  = get_nodes_block(store0);
			ir_graph *irg    = get_irn_irg(store0);
			ir_node  *convu0 = new_r_Conv(block, store_val, mode_unsigned);
			ir_node  *conv0  = new_r_Conv(block, convu0, double_mode);
			ir_node  *convu1 = new_r_Conv(block, store_val1, mode_unsigned);
			ir_node  *conv1  = new_r_Conv(block, convu1, double_mode);
			ir_node  *cnst   = new_r_Const_long(irg, mode_Iu, store_size*8);
			ir_node  *shl    = new_r_Shl(block, conv1, cnst);
			ir_node  *or     = new_r_Or(block, conv0, shl);

			/* create a new store and replace the two small stores */
			ir_cons_flags flags = cons_unaligned;
			if (!get_irn_pinned(store0) || !get_irn_pinned(store1))
				flags |= cons_floats;
			ir_node *new_store
				= new_rd_Store(dbgi, block, mem, store_ptr0, or, type, flags);
			exchange(store0, new_store);
			exchange(store1, new_store);
			new_in[i]  = pred0;
			new_in[i2] = NULL;
			/* restart detection loop */
			i       = 0;
			changed = true;
			goto again;
		}
	}

	if (changed) {
		/* compact new_in array (remove NULL entries) */
		int new_arity = 0;
		for (int i = 0; i < n_preds; ++i) {
			ir_node *in = new_in[i];
			if (in != NULL)
				new_in[new_arity++] = in;
		}

		if (new_arity == 1)
			exchange(sync, new_in[0]);
		else
			set_irn_in(sync, new_arity, new_in);
	}
}

void combine_memops(ir_graph *irg)
{
	/* We don't have code yet to test whether the address is aligned for the
	 * combined modes, so we can only do this if the backend supports unaligned
	 * stores. */
	if (!ir_target.fast_unaligned_memaccess)
		return;

	irg_walk_graph(irg, combine_memop, NULL, NULL);
}

void optimize_load_store(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
	                         | IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES
	                         | IR_GRAPH_PROPERTY_NO_CRITICAL_EDGES
	                         | IR_GRAPH_PROPERTY_NO_TUPLES
	                         | IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
	                         | IR_GRAPH_PROPERTY_CONSISTENT_POSTDOMINANCE
	                         | IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE);

	FIRM_DBG_REGISTER(dbg, "firm.opt.ldstopt");

	assert(get_irg_pinned(irg) != op_pin_state_floats);

	const ir_disambiguator_options opts =
		get_irg_memory_disambiguator_options(irg);
	if ((opts & aa_opt_always_alias) == 0) {
		assure_irp_globals_entity_usage_computed();
	}

	walk_env_t env = { .changes = NO_CHANGES };
	obstack_init(&env.obst);

	/* init the links, then collect Loads/Stores/Proj's in lists */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	master_visited = 0;
	irg_walk_graph(irg, firm_clear_link, collect_nodes, &env);

	/* now we have collected enough information, optimize */
	irg_walk_graph(irg, NULL, do_load_store_optimize, &env);

	/* optimize_load can introduce dead stores. They are
	 * eliminated now. */
	clear_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE);
	assure_irg_entity_usage_computed(irg);
	irg_walk_graph(irg, NULL, do_eliminate_dead_stores, &env);

	env.changes |= optimize_loops(irg);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);

	obstack_free(&env.obst, NULL);

	confirm_irg_properties(irg,
		env.changes == NO_CHANGES ? IR_GRAPH_PROPERTIES_ALL :
		(env.changes & CF_CHANGED) != 0 ? IR_GRAPH_PROPERTIES_NONE :
		(env.changes & DF_CHANGED) != 0 ? IR_GRAPH_PROPERTIES_CONTROL_FLOW :
		/*NODES_CREATED*/ IR_GRAPH_PROPERTIES_CONTROL_FLOW
		| IR_GRAPH_PROPERTY_NO_BADS | IR_GRAPH_PROPERTY_NO_TUPLES
		| IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE
		| IR_GRAPH_PROPERTY_MANY_RETURNS);
}
