/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Scalar replacement of compounds.
 * @author  Beyhan Veliev, Michael Beck
 */
#include <stdbool.h>
#include <string.h>

#include "array.h"
#include "be.h"
#include "debug.h"
#include "error.h"
#include "hashptr.h"
#include "ircons_t.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "irouts_t.h"
#include "opt_init.h"
#include "pset.h"
#include "scalar_replace.h"
#include "set.h"
#include "tv.h"
#include "util.h"
#include "xmalloc.h"

static unsigned get_vnum(const ir_node *node)
{
	return (unsigned)PTR_TO_INT(get_irn_link(node));
}

static void set_vnum(ir_node *node, unsigned vnum)
{
	set_irn_link(node, INT_TO_PTR(vnum));
}

/**
 * A path element entry: it is either an entity
 * or a tarval, because we evaluate only constant array
 * accesses like a.b.c[8].d
 */
typedef union {
	ir_entity *ent;
	ir_tarval *tv;
} path_elem_t;

/**
 * An access path, used to assign value numbers
 * to variables that will be scalar replaced.
 */
typedef struct path_t {
	unsigned    vnum;     /**< The value number. */
	size_t      path_len; /**< The length of the access path. */
	path_elem_t path[];   /**< The path. */
} path_t;

/** The size of a path in bytes. */
static size_t path_size(path_t *p)
{
	return sizeof(*p) + sizeof(p->path[0])*p->path_len;
}

typedef struct scalars_t {
	ir_entity *ent;              /**< A entity for scalar replacement. */
} scalars_t;

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

/**
 * Compare two pathes.
 *
 * @return 0 if they are identically
 */
static int path_cmp(const void *elt, const void *key, size_t size)
{
	(void)size;
	const path_t *p1 = (const path_t*)elt;
	const path_t *p2 = (const path_t*)key;
	/* we can use memcmp here, because identical tarvals should have identical
	 * addresses */
	return memcmp(p1->path, p2->path, p1->path_len * sizeof(p1->path[0]));
}

/**
 * Compare two elements of the scalars_t set.
 *
 * @return 0 if they are identically
 */
static int ent_cmp(const void *elt, const void *key, size_t size)
{
	(void)size;
	const scalars_t *c1 = (const scalars_t*)elt;
	const scalars_t *c2 = (const scalars_t*)key;
	return c1->ent != c2->ent;
}

/**
 * Calculate a hash value for a path.
 */
static unsigned path_hash(const path_t *path)
{
	unsigned hash = 0;
	for (unsigned i = 0; i < path->path_len; ++i)
		hash ^= (unsigned)PTR_TO_INT(path->path[i].ent);

	return hash >> 4;
}

/**
 * Returns non-zero, if all indices of a Sel node are constants.
 *
 * @param sel  the Sel node that will be checked
 */
static bool is_const_sel(ir_node *sel)
{
	for (int i = 0, n = get_Sel_n_indexs(sel); i < n; ++i) {
		ir_node *idx = get_Sel_index(sel, i);
		if (!is_Const(idx))
			return false;
	}
	return true;
}

/**
 * Return true if a Conv node converting from mode @p from to mode @p to
 * would have only bits from the source in the result.
 * Put another way, the conv produces the same value as a load/store pair:
 * Conv(val[mode_from], mode_to) == Load(Store(M, val[mode_from]), mode_to)
 */
static bool conv_is_bitcast(ir_mode *from, ir_mode *to)
{
	if (from == to)
		return true;

	if (get_mode_arithmetic(from) != irma_twos_complement
	    || get_mode_arithmetic(to) != irma_twos_complement)
		return false;
	unsigned from_size = get_mode_size_bits(from);
	unsigned to_size   = get_mode_size_bits(to);
	return from_size == to_size
	    || (to_size < from_size && !be_is_big_endian());
}

/*
 * Returns non-zero, if the address of an entity
 * represented by a Sel node (or its successor Sels) is taken.
 */
bool is_address_taken(ir_node *sel)
{
	if (!is_const_sel(sel))
		return true;

	foreach_irn_out_r(sel, i, succ) {
		switch (get_irn_opcode(succ)) {
		case iro_Load: {
			/* do not remove volatile variables */
			if (get_Load_volatility(succ) == volatility_is_volatile)
				return true;
			/* check if this load is not a hidden conversion */
			ir_mode   *mode  = get_Load_mode(succ);
			ir_entity *ent   = get_Sel_entity(sel);
			ir_mode   *emode = get_type_mode(get_entity_type(ent));
			if (emode == NULL || !conv_is_bitcast(emode, mode))
				return true;
			break;
		}

		case iro_Store: {
			/* check that Sel is not the Store's value */
			ir_node *value = get_Store_value(succ);
			if (value == sel)
				return true;
			/* do not remove volatile variables */
			if (get_Store_volatility(succ) == volatility_is_volatile)
				return true;
			/* check if this Store is not a hidden conversion */
			ir_mode   *mode  = get_irn_mode(value);
			ir_entity *ent   = get_Sel_entity(sel);
			ir_mode   *emode = get_type_mode(get_entity_type(ent));
			if (emode == NULL || !conv_is_bitcast(mode, emode))
				return true;
			break;
		}

		case iro_Sel: {
			ir_entity *entity = get_Sel_entity(succ);
			/* we can't handle unions correctly yet -> address taken */
			if (is_Union_type(get_entity_owner(entity)))
				return true;

			/* Check the Sel successor of Sel */
			bool res = is_address_taken(succ);
			if (res)
				return true;
			break;
		}

		case iro_Call:
			/* The address of an entity is given as a parameter.
			 * As long as we do not have analyses that can tell what
			 * is done with parameters, think is taken.
			 * One special case: If the Call type tells that it's a
			 * value parameter, the address is NOT taken.
			 */
			return true;

		case iro_Id: {
			bool res = is_address_taken(succ);
			if (res)
				return true;
			break;
		}

		case iro_Tuple:
			/* Non-optimized Tuple, happens in inlining */
			for (int input_nr = get_Tuple_n_preds(succ); input_nr-- > 0; ) {
				ir_node *pred = get_Tuple_pred(succ, input_nr);
				if (pred != sel)
					continue;
				/* we found one input */
				foreach_irn_out_r(succ, k, proj) {
					if (is_Proj(proj) && get_Proj_proj(proj) == input_nr) {
						bool res = is_address_taken(proj);
						if (res)
							return true;
					}
				}
			}
			break;

		default:
			/* another op, the address is taken */
			return true;
		}
	}
	return false;
}

/* we need a special address that serves as an address taken marker */
static char _x;
static void *ADDRESS_TAKEN = &_x;

typedef enum leaf_state_t {
	POSSIBLE_LEAF        = 0,
	HAS_CHILD_LOAD_STORE = (1u << 0),
	HAS_CHILD_SELS       = (1u << 1),
} leaf_state_t;

/**
 * Link all leaf Sels with the entity.
 *
 * @param ent  the entity that will be scalar replaced
 * @param sel  a Sel node that selects some fields of this entity
 */
static leaf_state_t link_all_leaf_sels(ir_entity *ent, ir_node *sel)
{
	/** A leaf Sel is a Sel that is used directly by a Load or Store. */
	leaf_state_t state = POSSIBLE_LEAF;
	foreach_irn_out_r(sel, i, succ) {
		if (is_Load(succ) || is_Store(succ)) {
			state |= HAS_CHILD_LOAD_STORE;
			continue;
		}
		if (is_Sel(succ)) {
			link_all_leaf_sels(ent, succ);
			state |= HAS_CHILD_SELS;
		} else if (is_Id(succ)) {
			state |= link_all_leaf_sels(ent, succ);
		} else {
			assert(is_End(succ));
		}
	}
	if (state & HAS_CHILD_SELS) {
		/* Sel is not a leaf, if it has load/stores bail out */
		if (state & HAS_CHILD_LOAD_STORE) {
			set_entity_link(ent, ADDRESS_TAKEN);
		}
		return state;
	}

	/* if we are here, then we have found a leaf Sel */

	/* beware of Id's */
	sel = skip_Id(sel);

	void *link = get_entity_link(ent);
	if (link == ADDRESS_TAKEN)
		return HAS_CHILD_LOAD_STORE | HAS_CHILD_SELS;

	/* we know we are at a leaf, because this function is only called if
	 * the address is NOT taken, so sel's successor(s) must be Loads or
	 * Stores */
	set_irn_link(sel, link);
	set_entity_link(ent, sel);
	return state;
}

/**
 * Find possible scalar replacements.
 *
 * @param irg  an IR graph
 *
 * This function finds variables on the (members of the) frame type
 * that can be scalar replaced, because their address is never taken.
 * If such a variable is found, its entity link will hold a list of all
 * Sel nodes, that selects the atomic fields of this entity.
 * Otherwise, the link will be ADDRESS_TAKEN or NULL.
 *
 * @return  true if at least one entity could be replaced potentially
 */
static bool find_possible_replacements(ir_graph *irg)
{
	/* First, clear the link field of all interesting entities. */
	ir_type *frame_tp = get_irg_frame_type(irg);
	for (size_t i = get_class_n_members(frame_tp); i-- > 0;) {
		ir_entity *ent = get_class_member(frame_tp, i);
		set_entity_link(ent, NULL);
	}

	/* check for inner functions:
	 * FIXME: need a way to get the argument position for the static link */
	long static_link_arg = 0;
	for (size_t i = get_class_n_members(frame_tp); i-- > 0;) {
		ir_entity *ent = get_class_member(frame_tp, i);
		if (!is_method_entity(ent))
			continue;

		ir_graph *inner_irg = get_entity_irg(ent);
		ir_node  *args;
		assure_irg_properties(inner_irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS
									   | IR_GRAPH_PROPERTY_NO_TUPLES);
		args = get_irg_args(inner_irg);
		foreach_irn_out_r(args, j, arg) {
			if (get_Proj_proj(arg) != static_link_arg)
				continue;

			foreach_irn_out_r(arg, k, succ) {
				if (!is_Sel(succ))
					continue;
				ir_entity *ent = get_Sel_entity(succ);
				if (get_entity_owner(ent) == frame_tp) {
					/* found an access to the outer frame */
					set_entity_link(ent, ADDRESS_TAKEN);
				}
			}
		}
	}

	/* Check the ir_graph for Sel nodes. If the entity of Sel isn't a scalar
	 * replacement set the link of this entity to ADDRESS_TAKEN. */
	ir_node *irg_frame = get_irg_frame(irg);
	int      res       = 0;
	foreach_irn_out_r(irg_frame, i, succ) {
		if (!is_Sel(succ))
			continue;

		/* we are only interested in entities on the frame, NOT on the value
		 * type */
		ir_entity *ent = get_Sel_entity(succ);
		if (get_entity_owner(ent) != frame_tp)
			continue;
		if (get_entity_link(ent) == ADDRESS_TAKEN)
			continue;

		/* we can handle arrays, structs and atomic types yet */
		ir_type *ent_type = get_entity_type(ent);
		if (is_aggregate_type(ent_type) || is_atomic_type(ent_type)) {
			if (is_address_taken(succ)) {
				 /* killing one */
				if (get_entity_link(ent))
					--res;
				set_entity_link(ent, ADDRESS_TAKEN);
			} else {
				/* possible found one */
				if (get_entity_link(ent) == NULL)
					++res;
				link_all_leaf_sels(ent, succ);
			}
		}
	}

	return res != 0;
}

/**
 * Return a path from the Sel node "sel" to its root.
 *
 * @param sel  the Sel node
 * @param len  the length of the path so far
 */
static path_t *find_path(ir_node *sel, size_t len)
{
	/* the current Sel node will add some path elements */
	int n = get_Sel_n_indexs(sel);
	len += n + 1;

	path_t  *res;
	ir_node *pred = get_Sel_ptr(sel);
	if (!is_Sel(pred)) {
		/* we found the root */
		res = XMALLOCF(path_t, path, len);
		res->path_len = len;
	} else {
		res = find_path(pred, len);
	}

	assert(len <= res->path_len);
	size_t pos = res->path_len - len;
	res->path[pos++].ent = get_Sel_entity(sel);
	for (int i = 0; i < n; ++i) {
		ir_node *index = get_Sel_index(sel, i);

		res->path[pos++].tv = get_Const_tarval(index);
	}
	return res;
}


/**
 * Allocate value numbers for the leafs in our found entities.
 *
 * @param sels  a set that will contain all Sels that have a value number
 * @param ent   the entity that will be scalar replaced
 * @param vnum  the first value number we can assign
 * @param modes a flexible array, containing all the modes of
 *              the value numbers.
 *
 * @return the next free value number
 */
static unsigned allocate_value_numbers(pset *sels, ir_entity *ent,
                                       unsigned vnum, ir_mode ***modes)
{
	set *pathes = new_set(path_cmp, 8);

	DB((dbg, SET_LEVEL_3, "  Visiting Sel nodes of entity %+F\n", ent));
	/* visit all Sel nodes in the chain of the entity */
	for (ir_node *sel = (ir_node*)get_entity_link(ent), *next; sel != NULL;
	     sel = next) {
		next = (ir_node*)get_irn_link(sel);

		/* we must mark this sel for later */
		pset_insert_ptr(sels, sel);

		path_t *key  = find_path(sel, 0);
		path_t *path = set_find(path_t, pathes, key, path_size(key), path_hash(key));

		if (path != NULL) {
			set_vnum(sel, path->vnum);
			DB((dbg, SET_LEVEL_3, "  %+F represents value %u\n", sel, path->vnum));
		} else {
			key->vnum = vnum++;

			(void)set_insert(path_t, pathes, key, path_size(key), path_hash(key));

			set_vnum(sel, key->vnum);
			DB((dbg, SET_LEVEL_3, "  %+F represents value %u\n", sel, key->vnum));

			ARR_EXTO(ir_mode *, *modes, (key->vnum + 15) & ~15);

			(*modes)[key->vnum] = get_type_mode(get_entity_type(get_Sel_entity(sel)));

			assert((*modes)[key->vnum] && "Value is not atomic");

#ifdef DEBUG_libfirm
			/* Debug output */
			DB((dbg, SET_LEVEL_2, "  %s", get_entity_name(key->path[0].ent)));
			for (unsigned i = 1; i < key->path_len; ++i) {
				if (is_entity(key->path[i].ent))
					DB((dbg, SET_LEVEL_2, ".%s", get_entity_name(key->path[i].ent)));
				else
					DB((dbg, SET_LEVEL_2, "[%ld]", get_tarval_long(key->path[i].tv)));
			}
			DB((dbg, SET_LEVEL_2, " = %u (%s)\n", PTR_TO_INT(get_irn_link(sel)), get_mode_name((*modes)[key->vnum])));
#endif /* DEBUG_libfirm */
		}
		free(key);
	}

	del_set(pathes);
	set_entity_link(ent, NULL);
	return vnum;
}

/**
 * environment for memory walker
 */
typedef struct env_t {
	unsigned  nvals;   /**< number of values */
	ir_mode **modes;   /**< the modes of the values */
	pset     *sels;    /**< A set of all Sel nodes that have a value number */
} env_t;

/**
 * topological post-walker.
 */
static void walker(ir_node *node, void *ctx)
{
	env_t    *env = (env_t*)ctx;

	if (is_Load(node)) {
		/* a load, check if we can resolve it */
		ir_node *addr = get_Load_ptr(node);
		if (!is_Sel(addr))
			return;
		if (!pset_find_ptr(env->sels, addr))
			return;

		/* ok, we have a Load that will be replaced */
		unsigned vnum = get_vnum(addr);
		assert(vnum < env->nvals);

		DB((dbg, SET_LEVEL_3, "replacing %+F by value %u\n", node, vnum));

		ir_node  *block = get_nodes_block(node);
		ir_graph *irg   = get_Block_irg(block);
		set_r_cur_block(irg, block);

		/* check, if we can replace this Load */
		ir_node *val = get_r_value(irg, vnum, env->modes[vnum]);

		/* Beware: A Load can contain a hidden conversion in Firm.
		This happens for instance in the following code:

		 int i;
		 unsigned j = *(unsigned *)&i;

		Handle this here. */
		ir_mode *load_mode = get_Load_mode(node);
		ir_mode *val_mode  = get_irn_mode(val);
		if (load_mode != val_mode)
			val = new_rd_Conv(get_irn_dbg_info(node), block, val, load_mode);

		ir_node *mem = get_Load_mem(node);
		ir_node *in[pn_Load_max+1] = {
			[pn_Load_M]         = mem,
			[pn_Load_res]       = val,
		};
		assert(pn_Load_M == 0 && pn_Load_res == 1);
		int n_in = 2;
		if (ir_throws_exception(node)) {
			in[pn_Load_X_regular] = new_r_Jmp(block);
			in[pn_Load_X_except]  = new_r_Bad(irg, mode_X);
			n_in = 4;
		}
		turn_into_tuple(node, n_in, in);
	} else if (is_Store(node)) {
		/* a Store always can be replaced */
		ir_node *addr = get_Store_ptr(node);
		if (!is_Sel(addr))
			return;
		if (!pset_find_ptr(env->sels, addr))
			return;

		unsigned vnum = get_vnum(addr);
		assert(vnum < env->nvals);

		DB((dbg, SET_LEVEL_3, "replacing %+F by value %u\n", node, vnum));

		ir_node *block = get_nodes_block(node);
		ir_graph *irg  = get_Block_irg(block);
		set_r_cur_block(irg, block);

		/* Beware: A Store can contain a hidden conversion in Firm. */
		ir_node *val = get_Store_value(node);
		ir_mode *store_mode = get_irn_mode(val);
		ir_mode *val_mode   = env->modes[vnum];
		if (store_mode != val_mode)
			val = new_rd_Conv(get_irn_dbg_info(node), block, val, val_mode);

		set_r_value(irg, vnum, val);

		ir_node *mem = get_Store_mem(node);
		ir_node *in[pn_Store_max+1] = {
			[pn_Store_M]         = mem,
		};
		assert(pn_Store_M == 0);
		int n_in = 1;
		if (ir_throws_exception(node)) {
			in[pn_Store_X_regular] = new_r_Jmp(block);
			in[pn_Store_X_except]  = new_r_Bad(irg, mode_X);
			n_in = 3;
		}
		turn_into_tuple(node, n_in, in);
	}
}

/**
 * Make scalar replacement.
 *
 * @param sels    A set containing all Sel nodes that have a value number
 * @param nvals   The number of scalars.
 * @param modes   A flexible array, containing all the modes of
 *                the value numbers.
 */
static void do_scalar_replacements(ir_graph *irg, pset *sels, unsigned nvals,
                                   ir_mode **modes)
{
	ssa_cons_start(irg, (int)nvals);

	/*
	 * second step: walk over the graph blockwise in topological order
	 * and fill the array as much as possible.
	 */
	DB((dbg, SET_LEVEL_3, "Substituting Loads and Stores in %+F\n", irg));
	env_t env;
	env.nvals = nvals;
	env.modes = modes;
	env.sels  = sels;
	irg_walk_blkwise_graph(irg, NULL, walker, &env);

	ssa_cons_finish(irg);
}

/*
 * Find possible scalar replacements
 *
 * @param irg  The current ir graph.
 */
void scalar_replacement_opt(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
	                         | IR_GRAPH_PROPERTY_CONSISTENT_OUTS
	                         | IR_GRAPH_PROPERTY_NO_TUPLES);

	/* we use the link field to store the VNUM */
	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK);
	irp_reserve_resources(irp, IRP_RESOURCE_ENTITY_LINK);

	/* Find possible scalar replacements */
	bool changed = false;
	if (find_possible_replacements(irg)) {
		DB((dbg, SET_LEVEL_1, "Scalar Replacement: %+F\n", irg));

		/* Insert in set the scalar replacements. */
		ir_node  *irg_frame = get_irg_frame(irg);
		unsigned  nvals     = 0;
		ir_mode **modes     = NEW_ARR_F(ir_mode *, 16);
		set      *set_ent   = new_set(ent_cmp, 8);
		pset     *sels      = pset_new_ptr(8);
		ir_type  *frame_tp  = get_irg_frame_type(irg);

		foreach_irn_out_r(irg_frame, i, succ) {
			if (!is_Sel(succ))
				continue;

			/* we are only interested in entities on the frame, NOT
			   parameters */
			ir_entity *ent = get_Sel_entity(succ);
			if (get_entity_owner(ent) != frame_tp
				|| is_parameter_entity(ent))
				continue;

			if (get_entity_link(ent) == NULL
				|| get_entity_link(ent) == ADDRESS_TAKEN)
				continue;

			scalars_t key;
			key.ent = ent;
			(void)set_insert(scalars_t, set_ent, &key, sizeof(key), hash_ptr(key.ent));

#ifdef DEBUG_libfirm
			ir_type *ent_type = get_entity_type(ent);

			if (is_Array_type(ent_type)) {
				DB((dbg, SET_LEVEL_1, "  found array %s\n", get_entity_name(ent)));
			} else if (is_compound_type(ent_type)) {
				DB((dbg, SET_LEVEL_1, "  found struct %s\n", get_entity_name(ent)));
			} else if (is_atomic_type(ent_type))
				DB((dbg, SET_LEVEL_1, "  found atomic value %s\n", get_entity_name(ent)));
			else {
				panic("Neither an array nor a struct or atomic value found in scalar replace");
			}
#endif

			nvals = allocate_value_numbers(sels, ent, nvals, &modes);
		}

		DB((dbg, SET_LEVEL_1, "  %u values will be needed\n", nvals));

		/* If scalars were found. */
		if (nvals > 0) {
			do_scalar_replacements(irg, sels, nvals, modes);

			foreach_set(set_ent, scalars_t, value) {
				free_entity(value->ent);
			}

			changed = true;
		}
		del_pset(sels);
		del_set(set_ent);
		DEL_ARR_F(modes);
	}

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	irp_free_resources(irp, IRP_RESOURCE_ENTITY_LINK);

	confirm_irg_properties(irg, changed ? IR_GRAPH_PROPERTIES_NONE
	                                    : IR_GRAPH_PROPERTIES_ALL);
}

void firm_init_scalar_replace(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.opt.scalar_replace");
}
