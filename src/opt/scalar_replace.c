/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Scalar replacement of compounds.
 * @author  Beyhan Veliev, Michael Beck
 */
#include "scalar_replace.h"

#include "array.h"
#include "debug.h"
#include "hashptr.h"
#include "ircons_t.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "irouts_t.h"
#include "opt_init.h"
#include "panic.h"
#include "pset.h"
#include "set.h"
#include "target_t.h"
#include "tv.h"
#include "util.h"
#include "xmalloc.h"
#include <stdbool.h>
#include <string.h>

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
	    || (to_size < from_size && !ir_target_big_endian());
}

static ir_type *get_addr_type(const ir_node *addr)
{
	if (is_Member(addr)) {
		ir_entity *entity = get_Member_entity(addr);
		return get_entity_type(entity);
	} else {
		assert(is_Sel(addr));
		return get_Sel_type(addr);
	}
}

/*
 * Returns non-zero, if the value represented by @p node "escapes", i.e. is used
 * by anything else than a load/store.
 */
bool is_address_taken(ir_node *node)
{
	assert(is_Member(node) || is_Sel(node));

	foreach_irn_out_r(node, i, succ) {
		switch (get_irn_opcode(succ)) {
		case iro_Load: {
			/* do not remove volatile variables */
			if (get_Load_volatility(succ) == volatility_is_volatile)
				return true;
			/* check if this load is not a hidden conversion */
			ir_mode *mode  = get_Load_mode(succ);
			ir_type *type  = get_addr_type(node);
			ir_mode *emode = get_type_mode(type);
			if (emode == NULL || !conv_is_bitcast(emode, mode))
				return true;
			break;
		}

		case iro_Store: {
			/* check that Sel is not the Store's value */
			ir_node *value = get_Store_value(succ);
			if (value == node)
				return true;
			/* do not remove volatile variables */
			if (get_Store_volatility(succ) == volatility_is_volatile)
				return true;
			/* check if this Store is not a hidden conversion */
			ir_mode *mode  = get_irn_mode(value);
			ir_type *type  = get_addr_type(node);
			ir_mode *emode = get_type_mode(type);
			if (emode == NULL || !conv_is_bitcast(mode, emode))
				return true;
			break;
		}

		case iro_Member: {
			ir_entity *entity = get_Member_entity(succ);
			/* we can't handle unions correctly yet -> address taken */
			if (is_Union_type(get_entity_owner(entity)))
				return true;
		}

			/* FALLTHROUGH */
		case iro_Sel: {
			/* Check the Member successor of Sel */
			bool res = is_address_taken(succ);
			if (res)
				return true;
			break;
		}

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
 * Link all leaf Members with the entity.
 *
 * @param ent  the entity that will be scalar replaced
 * @param sel  a Sel node that selects some fields of this entity
 */
static leaf_state_t link_all_leaf_members(ir_entity *ent, ir_node *sel)
{
	/** A leaf Sel is a Sel that is used directly by a Load or Store. */
	leaf_state_t state = POSSIBLE_LEAF;
	foreach_irn_out_r(sel, i, succ) {
		if (is_Load(succ) || is_Store(succ)) {
			state |= HAS_CHILD_LOAD_STORE;
			continue;
		}
		if (is_Member(succ)) {
			link_all_leaf_members(ent, succ);
			state |= HAS_CHILD_SELS;
		} else if (is_Id(succ)) {
			state |= link_all_leaf_members(ent, succ);
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
	for (size_t i = get_compound_n_members(frame_tp); i-- > 0;) {
		ir_entity *ent = get_compound_member(frame_tp, i);
		set_entity_link(ent, NULL);
	}

	/* Check the ir_graph for Member nodes. If the entity of Member isn't a
	 * scalar replacement set the link of this entity to ADDRESS_TAKEN. */
	ir_node *irg_frame = get_irg_frame(irg);
	int      res       = 0;
	foreach_irn_out_r(irg_frame, i, succ) {
		if (!is_Member(succ))
			continue;

		/* we are only interested in entities on the frame, NOT on the value
		 * type */
		ir_entity *ent = get_Member_entity(succ);
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
				link_all_leaf_members(ent, succ);
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
static path_t *find_path(ir_node *node, size_t len)
{
	/* the current Sel/Member node will add some path elements */
	path_t *res;
	if (is_Sel(node)) {
		ir_node *index = get_Sel_index(node);
		if (!is_Const(index))
			goto found_root;
		ir_node *pred = get_Sel_ptr(node);
		res = find_path(pred, len+1);
		size_t pos   = res->path_len - len - 1;
		res->path[pos].tv = get_Const_tarval(index);
	} else if (is_Member(node)) {
		ir_node *pred = get_Member_ptr(node);
		res = find_path(pred, len+1);
		size_t pos = res->path_len - len - 1;
		res->path[pos].ent = get_Member_entity(node);
	} else {
		/* we found the root */
found_root:
		res = XMALLOCF(path_t, path, len);
		res->path_len = len;
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
static unsigned allocate_value_numbers(pset *members, ir_entity *ent,
                                       unsigned vnum, ir_mode ***modes)
{
	set *pathes = new_set(path_cmp, 8);

	DB((dbg, SET_LEVEL_3, "  Visiting Sel nodes of entity %+F\n", ent));
	/* visit all Member nodes in the chain of the entity */
	for (ir_node *member = (ir_node*)get_entity_link(ent), *next;
	     member != NULL; member = next) {
		next = (ir_node*)get_irn_link(member);

		/* we must mark this member for later */
		pset_insert_ptr(members, member);

		path_t *key  = find_path(member, 0);
		path_t *path = set_find(path_t, pathes, key, path_size(key), path_hash(key));

		if (path != NULL) {
			set_vnum(member, path->vnum);
			DB((dbg, SET_LEVEL_3, "  %+F represents value %u\n", member,
			    path->vnum));
		} else {
			key->vnum = vnum++;

			(void)set_insert(path_t, pathes, key, path_size(key), path_hash(key));

			set_vnum(member, key->vnum);
			DB((dbg, SET_LEVEL_3, "  %+F represents value %u\n", member,
			    key->vnum));

			ARR_EXTO(ir_mode *, *modes, (key->vnum + 15) & ~15);

			ir_entity *entity = get_Member_entity(member);
			(*modes)[key->vnum] = get_type_mode(get_entity_type(entity));

			assert((*modes)[key->vnum] && "Value is not atomic");

#ifdef DEBUG_libfirm
			/* Debug output */
			DB((dbg, SET_LEVEL_2, "  %F", key->path[0].ent));
			for (unsigned i = 1; i < key->path_len; ++i) {
				if (is_entity(key->path[i].ent))
					DB((dbg, SET_LEVEL_2, ".%F", key->path[i].ent));
				else
					DB((dbg, SET_LEVEL_2, "[%ld]", get_tarval_long(key->path[i].tv)));
			}
			DB((dbg, SET_LEVEL_2, " = %u (%s)\n", PTR_TO_INT(get_irn_link(member)), get_mode_name((*modes)[key->vnum])));
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
	pset     *members; /**< A set of all Member nodes that have a value num */
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
		if (!is_Member(addr))
			return;
		if (!pset_find_ptr(env->members, addr))
			return;

		/* ok, we have a Load that will be replaced */
		unsigned vnum = get_vnum(addr);
		assert(vnum < env->nvals);

		DB((dbg, SET_LEVEL_3, "replacing %+F by value %u\n", node, vnum));

		ir_node  *block = get_nodes_block(node);
		ir_graph *irg   = get_irn_irg(node);
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
		if (!is_Member(addr))
			return;
		if (!pset_find_ptr(env->members, addr))
			return;

		unsigned vnum = get_vnum(addr);
		assert(vnum < env->nvals);

		DB((dbg, SET_LEVEL_3, "replacing %+F by value %u\n", node, vnum));

		ir_node  *block = get_nodes_block(node);
		ir_graph *irg   = get_irn_irg(node);
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
 * @param members A set containing all Member nodes that have a value number
 * @param nvals   The number of scalars.
 * @param modes   A flexible array, containing all the modes of
 *                the value numbers.
 */
static void do_scalar_replacements(ir_graph *irg, pset *members, unsigned nvals,
                                   ir_mode **modes)
{
	ssa_cons_start(irg, (int)nvals);

	/*
	 * second step: walk over the graph blockwise in topological order
	 * and fill the array as much as possible.
	 */
	DB((dbg, SET_LEVEL_3, "Substituting Loads and Stores in %+F\n", irg));
	env_t env;
	env.nvals  = nvals;
	env.modes  = modes;
	env.members = members;
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
			if (!is_Member(succ))
				continue;

			/* we are only interested in entities on the frame, NOT
			   parameters */
			ir_entity *ent = get_Member_entity(succ);
			if (get_entity_owner(ent) != frame_tp || is_parameter_entity(ent))
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
				DB((dbg, SET_LEVEL_1, "  found array %F\n", ent));
			} else if (is_compound_type(ent_type)) {
				DB((dbg, SET_LEVEL_1, "  found struct %F\n", ent));
			} else if (is_atomic_type(ent_type)) {
				DB((dbg, SET_LEVEL_1, "  found atomic value %F\n", ent));
			} else {
				panic("neither an array nor a struct or atomic value found in scalar replace");
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
