/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Memory disambiguator
 * @author   Michael Beck
 * @date     27.12.2006
 */
#include "irmemory_t.h"

#include "adt/pmap.h"
#include "debug.h"
#include "hashptr.h"
#include "irflag.h"
#include "irflag.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irouts_t.h"
#include "irprintf.h"
#include "irprog_t.h"
#include "panic.h"
#include "type_t.h"
#include "typerep.h"
#include "util.h"
#include <stdbool.h>
#include <stdlib.h>

/** The debug handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)
DEBUG_ONLY(static firm_dbg_module_t *dbgcall = NULL;)

/** The global memory disambiguator options. */
static unsigned global_mem_disamgig_opt = aa_opt_none;

const char *get_ir_alias_relation_name(ir_alias_relation rel)
{
#define X(a) case a: return #a
	switch (rel) {
	X(ir_no_alias);
	X(ir_may_alias);
	X(ir_sure_alias);
	}
#undef X
	panic("unknown alias relation");
}

ir_disambiguator_options get_irg_memory_disambiguator_options(
		ir_graph const *const irg)
{
	unsigned opt = irg->mem_disambig_opt;
	if (opt & aa_opt_inherited)
		return global_mem_disamgig_opt;
	return opt;
}

void set_irg_memory_disambiguator_options(ir_graph *irg,
                                          ir_disambiguator_options options)
{
	irg->mem_disambig_opt = options & ~aa_opt_inherited;
}

void set_irp_memory_disambiguator_options(ir_disambiguator_options options)
{
	global_mem_disamgig_opt = options;
}

ir_storage_class_class_t get_base_sc(ir_storage_class_class_t x)
{
	return x & ~ir_sc_modifiers;
}

/**
 * Find the base address and entity of an Sel/Member node.
 *
 * @param node the node
 * @param pEnt after return points to the base entity.
 *
 * @return the base address.
 */
static const ir_node *find_base_addr(const ir_node *node, ir_entity **pEnt)
{
	const ir_node *member = NULL;
	for (;;) {
		if (is_Sel(node)) {
			node = get_Sel_ptr(node);
			continue;
		} else if (is_Member(node)) {
			ir_node *pred = get_Member_ptr(node);
			/** local variables are members of the frame type, but all disjunct
			 * so we regard them as base addresses */
			if (is_Proj(pred) && pred == get_irg_frame(get_irn_irg(pred)))
				break;
			member = node;
			node   = pred;
		} else {
			break;
		}
	}
	if (member != NULL)
		*pEnt = get_Member_entity(member);
	return node;
}

/**
 * Returns true if @c compound is a compound type that contains a
 * member with type @c member, including recursively, or if @c
 * compound is an array type with a base type equal to or containing
 * @c member.
 *
 * @param compound  The compound type
 * @param member    The member type
 *
 * @return true, if @c compound contains @c member
 */
static bool type_contains(const ir_type *compound,
                          const ir_type *member)
{
	if (is_Array_type(compound)) {
		ir_type *elem = get_array_element_type(compound);
		return elem == member ||
		       type_contains(elem, member);

	} else if (is_compound_type(compound)) {
		size_t n = get_compound_n_members(compound);
		for (size_t pos = 0; pos < n; pos++) {
			ir_entity *ent = get_compound_member(compound, pos);
			ir_type *pos_type = get_entity_type(ent);
			if (pos_type == member ||
			    type_contains(pos_type, member)) {
				return true;
			}
		}
		return false;
	} else {
		return false;
	}
}

/**
 * Determine the alias relation by checking if type1 and type2 are
 * different types.
 *
 * @param addr1    The first type.
 * @param addr2    The second type.
 */
static ir_alias_relation different_types(const ir_type *type1,
                                         const ir_type *type2)
{
	if (type1 == type2) {
		return ir_may_alias;
	}
	/* do deref until no pointer types are found */
	while (is_Pointer_type(type1) && is_Pointer_type(type2)) {
		type1 = get_pointer_points_to_type(type1);
		type2 = get_pointer_points_to_type(type2);
	}
	if (type_contains(type1, type2) || type_contains(type2, type1)) {
		return ir_may_alias;
	}
	if (is_Class_type(type1) && is_Class_type(type2) &&
	    (is_SubClass_of(type1, type2) || is_SubClass_of(type2, type1))) {
		return ir_may_alias;
	}
	return ir_no_alias;
}

/**
 * Returns non-zero if a node is a result on a malloc-like routine.
 *
 * @param node  the Proj node to test
 */
static bool is_malloc_Result(const ir_node *node)
{
	node = get_Proj_pred(node);
	if (!is_Proj(node))
		return false;
	node = get_Proj_pred(node);
	if (!is_Call(node))
		return false;
	ir_entity *callee = get_Call_callee(node);
	return callee != NULL
	    && (get_entity_additional_properties(callee) & mtp_property_malloc);
}

ir_storage_class_class_t classify_pointer(const ir_node *const addr,
                                          const ir_node *const base)
{
	/* part1: determine class */
	ir_storage_class_class_t res;
	ir_entity               *entity;
	if (is_Address(base)) {
		entity = get_Address_entity(base);
		ir_type *owner = get_entity_owner(entity);
		res = owner == get_tls_type() ? ir_sc_tls : ir_sc_globalvar;
		goto analyze_entity;
	} else if (is_Member(base)) {
		/* should only happen for local vars */
		assert(get_Member_ptr(base) == get_irg_frame(get_irn_irg(base)));
		entity = get_Member_entity(base);
		res    = is_parameter_entity(entity) ? ir_sc_argument : ir_sc_localvar;
		goto analyze_entity;
	} else if (is_Proj(base)) {
		if (is_malloc_Result(base))
			res = ir_sc_malloced;
		else
			res = ir_sc_pointer;
	} else if (is_Const(base)) {
		ir_tarval *tv = get_Const_tarval(base);
		if (tarval_is_null(tv))
			res = ir_sc_null;
		else
			res = ir_sc_globaladdr;
	} else {
		/* we always should have Member(irg_frame) as base address */
		assert(base != get_irg_frame(get_irn_irg(base)));
		res = ir_sc_pointer;
	}

	/* part2: modifiers */

	/* if we select a member/array elem from addr, then it must have been
	 * a compound type */
	if (is_Sel(addr) || is_Member(addr))
		res |= ir_sc_modifier_obj_comp;
	return res;

analyze_entity:
	if (!(get_entity_usage(entity) & ir_usage_address_taken))
		res |= ir_sc_modifier_nottaken;
	const ir_type *type = get_entity_type(entity);
	res |= is_compound_type(type) ? ir_sc_modifier_obj_comp
	                              : ir_sc_modifier_obj_prim;
	return res;
}

typedef struct address_info {
	ir_node const *base;
	ir_node const *sym_offset;
	long           offset;
	bool           has_const_offset;
} address_info;

static address_info get_address_info(ir_node const *addr)
{
	ir_node *sym_offset       = NULL;
	long     offset           = 0;
	bool     has_const_offset = true;
	for (;;) {
		switch (get_irn_opcode(addr)) {
		case iro_Add: {
			ir_node       *ptr_node;
			ir_node       *int_node;
			ir_mode *const mode_left = get_irn_mode(get_Add_left(addr));
			if (mode_is_reference(mode_left)) {
				ptr_node = get_Add_left(addr);
				int_node = get_Add_right(addr);
			} else {
				ptr_node = get_Add_right(addr);
				int_node = get_Add_left(addr);
			}

			if (is_Const(int_node)) {
				ir_tarval *tv = get_Const_tarval(int_node);
				if (tarval_is_long(tv)) {
					/* TODO: check for overflow */
					offset += get_tarval_long(tv);
					goto follow_ptr;
				}
			}
			if (!sym_offset) {
				sym_offset = int_node;
			} else {
				// addr has more than one symbolic offset, give up.
				has_const_offset = false;
			}

follow_ptr:
			addr = ptr_node;
			break;
		}

		case iro_Sub:
			has_const_offset = false;
			addr             = get_Sub_left(addr);
			break;

		default:
			return (address_info){ addr, sym_offset, offset, has_const_offset };
		}
	}
}

static ir_alias_relation _get_alias_relation(const ir_node *addr1, const ir_type *const objt1, unsigned size1,
                                             const ir_node *addr2, const ir_type *const objt2, unsigned size2)
{
	if (addr1 == addr2)
		return ir_sure_alias;
	ir_graph *const irg     = get_irn_irg(addr1);
	unsigned  const options = get_irg_memory_disambiguator_options(irg);
	if (options & aa_opt_always_alias)
		return ir_may_alias;
	/* The Armageddon switch */
	if (options & aa_opt_no_alias)
		return ir_no_alias;

	/* do the addresses have constants offsets from the same base?
	 *  Note: sub X, C is normalized to add X, -C */

	/*
	 * Currently, only expressions with at most one symbolic
	 * offset can be handled.  To extend this, change
	 * sym_offset to be a set, and compare the sets.
	 */
	address_info const info1   = get_address_info(addr1);
	address_info const info2   = get_address_info(addr2);
	long               offset1 = info1.offset;
	long               offset2 = info2.offset;
	addr1 = info1.base;
	addr2 = info2.base;

	/* same base address -> compare offsets if possible.
	 * FIXME: type long is not sufficient for this task ... */
	if (addr1 == addr2 && info1.sym_offset == info2.sym_offset && info1.has_const_offset && info2.has_const_offset) {
		unsigned long first_offset;
		unsigned long last_offset;
		unsigned first_size;

		if (offset1 <= offset2) {
			first_offset = offset1;
			last_offset = offset2;
			first_size = size1;
		} else {
			first_offset = offset2;
			last_offset = offset1;
			first_size = size2;
		}

		return first_offset + first_size <= last_offset
		     ? ir_no_alias : ir_sure_alias;
	}

	/* skip Sels/Members */
	ir_entity     *ent1  = NULL;
	ir_entity     *ent2  = NULL;
	const ir_node *base1 = find_base_addr(addr1, &ent1);
	const ir_node *base2 = find_base_addr(addr2, &ent2);

	/* two struct accesses -> compare entities */
	if (ent1 != NULL && ent2 != NULL) {
		if (ent1 == ent2) {
			if (base1 == base2)
				return ir_sure_alias;
			goto check_classes;
		}
		ir_type *owner1 = get_entity_owner(ent1);
		ir_type *owner2 = get_entity_owner(ent2);
		if (owner1 != owner2) {
			/* TODO: We have to differentiate 3 cases:
			 * - owner1 or owner2 is a type used in a subtree of the other.
			 * - If there exists a union type where the first elements towards
			 *   owner1+owner2 and the fields inside owner1+owner2 are
			 *   compatible, then they may alias.
			 * - All other cases cannot alias.
			 * => for now we assume may alias
			 */
			goto check_classes;
		}
		/* same owner, different entities? They may only alias if we have a
		 * union or if one of them is a bitfield members. */
		/* TODO: can we test if the base units actually overlap in the bitfield
		 * case? */
		if (!is_Union_type(owner1) && get_entity_bitfield_size(ent1) == 0
		    && get_entity_bitfield_size(ent2) == 0)
		    return ir_no_alias;
	}

check_classes:;
	/* no alias if 1 is a primitive object and the other a compound object */
	const ir_storage_class_class_t mod1 = classify_pointer(addr1, base1);
	const ir_storage_class_class_t mod2 = classify_pointer(addr2, base2);
	if (((mod1 | mod2) & (ir_sc_modifier_obj_comp | ir_sc_modifier_obj_prim))
	    == (ir_sc_modifier_obj_comp | ir_sc_modifier_obj_prim))
		return ir_no_alias;

	const ir_storage_class_class_t class1 = mod1 & ~ir_sc_modifiers;
	const ir_storage_class_class_t class2 = mod2 & ~ir_sc_modifiers;
	ir_storage_class_class_t other_class;
	ir_storage_class_class_t other_mod;
	if (class1 == ir_sc_pointer) {
		other_class = class2;
		other_mod   = mod2;
		goto pointer;
	} else if (class2 == ir_sc_pointer) {
		other_class = class1;
		other_mod   = mod1;
pointer:
		/* a pointer and an object whose address was never taken */
		if (other_mod & ir_sc_modifier_nottaken)
			return ir_no_alias;
		/* the null pointer aliases nothing */
		if (other_class == ir_sc_null)
			return ir_no_alias;
	} else if (class1 != class2) {
		/* objects from different memory spaces cannot alias */
		return ir_no_alias;
	} else {
		/* both classes are equal */
		if (class1 == ir_sc_globalvar) {
			ir_entity *entity1 = get_Address_entity(base1);
			ir_entity *entity2 = get_Address_entity(base2);
			return entity1 != entity2 ? ir_no_alias : ir_may_alias;
		} else if (class1 == ir_sc_localvar) {
			ir_entity *entity1 = get_Member_entity(base1);
			ir_entity *entity2 = get_Member_entity(base2);
			return entity1 != entity2 ? ir_no_alias : ir_may_alias;
		} else if (class1 == ir_sc_globaladdr) {
			offset1 += get_Const_long(base1);
			offset2 += get_Const_long(base2);

			unsigned type_size = MAX(size1, size2);
			if ((unsigned long)labs(offset2 - offset1) >= type_size)
				return ir_no_alias;
			else
				return ir_sure_alias;
		} else if (class1 == ir_sc_malloced) {
			return base1 == base2 ? ir_sure_alias : ir_no_alias;
		}
	}

	/* Type based alias analysis */
	if (options & aa_opt_type_based) {
		ir_alias_relation rel;

		if (options & aa_opt_byte_type_may_alias) {
			if (get_type_size(objt1) == 1 || get_type_size(objt2) == 1) {
				/* One of the types address a byte. Assume a ir_may_alias and leave
				   the type based check. */
				goto leave_type_based_alias;
			}
		}

		/* cheap check: If the type sizes did not match, the types MUST be different */
		/* No, one might be part of the other. */
		/* if (get_type_size(objt1) != get_type_size(objt2)) */
		/*         return ir_no_alias; */

		/* cheap test: if only one is a reference type, no alias */
		if (is_Pointer_type(objt1) != is_Pointer_type(objt2)) {
			return ir_no_alias;
		}

		if (is_Primitive_type(objt1) && is_Primitive_type(objt2)) {
			const ir_mode *const mode1 = get_type_mode(objt1);
			const ir_mode *const mode2 = get_type_mode(objt2);

			/* cheap test: if arithmetic is different, no alias */
			if (get_mode_arithmetic(mode1) != get_mode_arithmetic(mode2))
				return ir_no_alias;
			/* no alias if 1 is a reference and the other isn't */
			if (mode_is_reference(mode1) != mode_is_reference(mode2))
				return ir_no_alias;
		}

		rel = different_types(objt1, objt2);
		if (rel != ir_may_alias)
			return rel;
leave_type_based_alias:;
	}

	return ir_may_alias;
}

ir_alias_relation get_alias_relation(const ir_node *const addr1, const ir_type *const type1, unsigned size1,
                                     const ir_node *const addr2, const ir_type *const type2, unsigned size2)
{
	ir_alias_relation rel = _get_alias_relation(addr1, type1, size1, addr2, type2, size2);
	DB((dbg, LEVEL_1, "alias(%+F, %+F) = %s\n", addr1, addr2,
	    get_ir_alias_relation_name(rel)));
	return rel;
}

/**
 * Check the mode of a Load/Store with the mode of the entity
 * that is accessed.
 * If the mode of the entity and the Load/Store mode do not match, we
 * have the bad reinterpret case:
 *
 * int i;
 * char b = *(char *)&i;
 *
 * We do NOT count this as one value and return address_taken
 * in that case.
 * However, we support an often used case. If the mode is two-complement
 * we allow casts between signed/unsigned.
 *
 * @param mode     the mode of the Load/Store
 * @param ent_mode the mode of the accessed entity
 *
 * @return non-zero if the Load/Store is a hidden cast, zero else
 */
static bool is_hidden_cast(const ir_mode *mode, const ir_mode *ent_mode)
{
	if (ent_mode == NULL)
		return false;

	if (ent_mode != mode &&
		(get_mode_size_bits(ent_mode) != get_mode_size_bits(mode) ||
		 get_mode_arithmetic(ent_mode) != irma_twos_complement ||
		 get_mode_arithmetic(mode) != irma_twos_complement)) {
		return true;
	}
	return false;
}

/**
 * Determine the usage state of a node (or its successor Sels).
 *
 * @param irn  the node
 */
static ir_entity_usage determine_entity_usage(const ir_node *irn,
                                              const ir_entity *entity)
{
	unsigned res = 0;
	for (int i = get_irn_n_outs(irn); i-- > 0; ) {
		int            succ_pos;
		const ir_node *succ  = get_irn_out_ex(irn, i, &succ_pos);

		switch (get_irn_opcode(succ)) {
		case iro_Load:
			/* beware: irn might be a Id node here, so irn might be not
			   equal to get_Load_ptr(succ) */
			res |= ir_usage_read;

			/* check if this load is not a hidden conversion */
			ir_mode *mode  = get_Load_mode(succ);
			ir_mode *emode = get_type_mode(get_entity_type(entity));
			if (is_hidden_cast(mode, emode))
				res |= ir_usage_reinterpret_cast;
			break;

		case iro_Store:
			/* check that the node is not the Store's value */
			if (succ_pos == n_Store_value) {
				res |= ir_usage_unknown;
			} else if (succ_pos == n_Store_ptr) {
				res |= ir_usage_write;

				/* check if this Store is not a hidden conversion */
				ir_node *value = get_Store_value(succ);
				ir_mode *mode  = get_irn_mode(value);
				ir_mode *emode = get_type_mode(get_entity_type(entity));
				if (is_hidden_cast(mode, emode))
					res |= ir_usage_reinterpret_cast;
			}
			assert(irn != get_Store_mem(succ));
			break;

		case iro_CopyB: {
			/* CopyB are like Loads/Stores */
			ir_type *tp = get_entity_type(entity);
			if (tp != get_CopyB_type(succ)) {
				/* bad, different types, might be a hidden conversion */
				res |= ir_usage_reinterpret_cast;
			}
			if (succ_pos == n_CopyB_dst) {
				res |= ir_usage_write;
			} else {
				assert(succ_pos == n_CopyB_src);
				res |= ir_usage_read;
			}
			break;
		}

		case iro_Sel:
		case iro_Add:
		case iro_Sub:
		case iro_Id:
			/* Check the successor of irn. */
			res |= determine_entity_usage(succ, entity);
			break;

		case iro_Member: {
			ir_entity *member_entity = get_Member_entity(succ);
			/* Check the successor of irn. */
			res |= determine_entity_usage(succ, member_entity);
			break;
		}

		case iro_Call:
			if (succ_pos == n_Call_ptr) {
				/* TODO: we could check for reinterpret casts here...
				 * But I doubt anyone is interested in that bit for
				 * function entities and I'm too lazy to write the code now.
				 */
				res |= ir_usage_read;
			} else {
				assert(succ_pos != n_Call_mem);
				int arg_nr = succ_pos - n_Call_max - 1;
				ir_type *type     = get_Call_type(succ);
				ir_type *arg_type = get_method_param_type(type, arg_nr);
				if (is_aggregate_type(arg_type))
					res |= ir_usage_read;
				else
					res |= ir_usage_unknown;
			}
			break;

		case iro_Builtin: {
			ir_builtin_kind kind = get_Builtin_kind(succ);
			/* the parameters of the may_alias builtin do not lead to
			 * read/write or address taken. */
			if (kind == ir_bk_may_alias)
				break;
			res |= ir_usage_unknown;
			break;
		}

		default:
			/* another op, we don't know anything (we could do more advanced
			 * things like a dataflow analysis here) */
			res |= ir_usage_unknown;
			break;
		}
	}

	return (ir_entity_usage) res;
}

/**
 * Update the usage flags of all frame entities.
 */
static void analyse_irg_entity_usage(ir_graph *irg)
{
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);

	/* set initial state to not_taken, as this is the "smallest" state */
	ir_type *frame_type = get_irg_frame_type(irg);
	for (size_t i = 0, n = get_compound_n_members(frame_type); i < n; ++i) {
		ir_entity *ent = get_compound_member(frame_type, i);
		/* methods can only be analyzed globally */
		if (is_method_entity(ent))
			continue;
		ir_entity_usage flags = ir_usage_none;
		if (get_entity_linkage(ent) & IR_LINKAGE_HIDDEN_USER)
			flags = ir_usage_unknown;
		set_entity_usage(ent, flags);
	}

	ir_node *irg_frame = get_irg_frame(irg);

	foreach_irn_out_r(irg_frame, j, succ) {
		if (!is_Member(succ))
			continue;

		ir_entity *entity = get_Member_entity(succ);
		unsigned   flags  = get_entity_usage(entity);
		flags |= determine_entity_usage(succ, entity);
		set_entity_usage(entity, (ir_entity_usage) flags);
	}

	/* now computed */
	add_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE);
}

void assure_irg_entity_usage_computed(ir_graph *irg)
{
	if (irg_has_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_ENTITY_USAGE))
		return;

	analyse_irg_entity_usage(irg);
}

/**
 * Initialize the entity_usage flag for a global type like type.
 */
static void init_entity_usage(ir_type *tp)
{
	/* We have to be conservative: All external visible entities are unknown */
	for (size_t i = 0, n = get_compound_n_members(tp); i < n; ++i) {
		ir_entity *ent   = get_compound_member(tp, i);
		unsigned   flags = ir_usage_none;

		if (entity_is_externally_visible(ent)) {
			flags |= ir_usage_unknown;
		}
		set_entity_usage(ent, (ir_entity_usage) flags);
	}
}

/**
 * Mark all entities used in the initializer's value as unknown usage.
 *
 * @param value  the value to check
 */
static void check_initializer_value(ir_node *value)
{
	/* Handle each node at most once. */
	if (irn_visited_else_mark(value))
		return;

	/* let's check if it's an address */
	if (is_Address(value)) {
		ir_entity *ent = get_Address_entity(value);
		set_entity_usage(ent, ir_usage_unknown);
	}

	foreach_irn_in(value, i, op) {
		check_initializer_value(op);
	}
}

/**
 * Mark all entities used in the initializer as unknown usage.
 *
 * @param initializer  the initializer to check
 */
static void check_initializer_nodes(ir_initializer_t *initializer)
{
	switch (initializer->kind) {
	case IR_INITIALIZER_CONST: {
		ir_node  *n   = initializer->consti.value;
		ir_graph *irg = get_irn_irg(n);
		ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
		inc_irg_visited(irg);
		check_initializer_value(n);
		ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
		return;
	}
	case IR_INITIALIZER_TARVAL:
	case IR_INITIALIZER_NULL:
		return;
	case IR_INITIALIZER_COMPOUND:
		for (size_t i = 0; i < initializer->compound.n_initializers; ++i) {
			ir_initializer_t *sub_initializer
				= initializer->compound.initializers[i];
			check_initializer_nodes(sub_initializer);
		}
		return;
	}
	panic("invalid initializer found");
}

/**
 * Mark all entities used in the initializer for the given entity as unknown
 * usage.
 *
 * @param ent  the entity
 */
static void check_initializer(ir_entity *ent)
{
	if (get_entity_kind(ent) == IR_ENTITY_NORMAL) {
		ir_initializer_t *const init = get_entity_initializer(ent);
		if (init != NULL)
			check_initializer_nodes(init);
	}
}


/**
 * Mark all entities used in initializers as unknown usage.
 *
 * @param tp  a compound type
 */
static void check_initializers(ir_type *tp)
{
	for (size_t i = 0, n = get_compound_n_members(tp); i < n; ++i) {
		ir_entity *ent = get_compound_member(tp, i);

		check_initializer(ent);
	}
}

#ifdef DEBUG_libfirm
/**
 * Print the entity usage flags of all entities of a given type for debugging.
 *
 * @param tp  a compound type
 */
static void print_entity_usage_flags(const ir_type *tp)
{
	for (size_t i = 0, n = get_compound_n_members(tp); i < n; ++i) {
		ir_entity      *ent   = get_compound_member(tp, i);
		ir_entity_usage flags = get_entity_usage(ent);

		if (flags == 0)
			continue;
		ir_printf("%+F:", ent);
		if (flags & ir_usage_address_taken)
			printf(" address_taken");
		if (flags & ir_usage_read)
			printf(" read");
		if (flags & ir_usage_write)
			printf(" write");
		if (flags & ir_usage_reinterpret_cast)
			printf(" reinterp_cast");
		printf("\n");
	}
}
#endif /* DEBUG_libfirm */

/**
 * Post-walker: check for global entity address
 */
static void check_global_address(ir_node *irn, void *data)
{
	(void)data;
	if (!is_Address(irn))
		return;

	ir_entity *entity = get_Address_entity(irn);
	unsigned   flags  = get_entity_usage(entity);
	flags |= determine_entity_usage(irn, entity);
	set_entity_usage(entity, (ir_entity_usage) flags);
}

/**
 * Update the entity usage flags of all global entities.
 */
static void analyse_irp_globals_entity_usage(void)
{
	for (ir_segment_t s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s) {
		ir_type *type = get_segment_type(s);
		init_entity_usage(type);
	}

	for (ir_segment_t s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s) {
		ir_type *type = get_segment_type(s);
		check_initializers(type);
	}

	foreach_irp_irg(i, irg) {
		assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS | IR_GRAPH_PROPERTY_NO_TUPLES);
		irg_walk_graph(irg, NULL, check_global_address, NULL);
	}

#ifdef DEBUG_libfirm
	if (firm_dbg_get_mask(dbg) & LEVEL_1) {
		for (ir_segment_t s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s) {
			print_entity_usage_flags(get_segment_type(s));
		}
	}
#endif /* DEBUG_libfirm */

	/* now computed */
	irp->globals_entity_usage_state = ir_entity_usage_computed;
}

ir_entity_usage_computed_state get_irp_globals_entity_usage_state(void)
{
	return irp->globals_entity_usage_state;
}

void set_irp_globals_entity_usage_state(ir_entity_usage_computed_state state)
{
	irp->globals_entity_usage_state = state;
}

void assure_irp_globals_entity_usage_computed(void)
{
	if (irp->globals_entity_usage_state != ir_entity_usage_not_computed)
		return;

	analyse_irp_globals_entity_usage();
}

void firm_init_memory_disambiguator(void)
{
	FIRM_DBG_REGISTER(dbg, "firm.ana.irmemory");
	FIRM_DBG_REGISTER(dbgcall, "firm.opt.cc");
}

/** Maps method types to cloned method types. */
static pmap *mtp_map;

/**
 * Clone a method type if not already cloned.
 *
 * @param tp  the type to clone
 */
static ir_type *clone_type_and_cache(ir_type *const tp, int const variadic_index)
{
	ir_type *res = pmap_get(ir_type, mtp_map, tp);
	if (res == NULL) {
		mtp_additional_properties const props = get_method_additional_properties(tp);
		res = clone_type_method(tp, variadic_index, props | mtp_property_private);
		pmap_insert(mtp_map, tp, res);
	}

	return res;
}

/**
 * Walker: clone all call types of Calls to methods having the
 * mtp_property_private property set.
 */
static void update_calls_to_private(ir_node *call, void *env)
{
	(void)env;
	if (!is_Call(call))
		return;
	ir_entity *callee = get_Call_callee(call);
	if (callee == NULL)
		return;

	ir_type *ctp = get_Call_type(call);
	if ((get_entity_additional_properties(callee) & mtp_property_private)
		&& ((get_method_additional_properties(ctp) & mtp_property_private) == 0)) {
		ir_type *const entity_ctp = get_entity_type(callee);
		/* clear mismatches in variadicity that can happen in obscure C
		 * programs and break when changing to private calling convention. */
		ctp = clone_type_and_cache(ctp, get_method_variadic_index(entity_ctp));
		set_Call_type(call, ctp);
		DB((dbgcall, LEVEL_1,
		    "changed call to private method %+F using cloned type %+F\n",
		    callee, ctp));
	}
}

void mark_private_methods(void)
{
	assure_irp_globals_entity_usage_computed();
	mtp_map = pmap_create();

	/* first step: change the calling conventions of the local non-escaped entities */
	bool changed = false;
	foreach_irp_irg(i, irg) {
		ir_entity       *ent   = get_irg_entity(irg);
		ir_entity_usage  flags = get_entity_usage(ent);

		if (!(flags & ir_usage_address_taken) && !entity_is_externally_visible(ent)) {
			ir_type *mtp = get_entity_type(ent);

			add_entity_additional_properties(ent, mtp_property_private);
			DB((dbgcall, LEVEL_1, "found private method %+F\n", ent));
			if ((get_method_additional_properties(mtp) & mtp_property_private) == 0) {
				/* need a new type */
				mtp = clone_type_and_cache(mtp, get_method_variadic_index(mtp));
				set_entity_type(ent, mtp);
				DB((dbgcall, LEVEL_2, "changed entity type of %+F to %+F\n", ent, mtp));
				changed = true;
			}
		}
	}

	if (changed)
		all_irg_walk(NULL, update_calls_to_private, NULL);

	pmap_destroy(mtp_map);
}

/**
 * Find the entity that the given pointer points to.
 *
 * This function returns the entity into which @c ptr points, ignoring
 * any offsets (it assumes that offsets always stay within the
 * entity).
 *
 * This function does *not* always return a top-level entity
 * (i.e. local/global variable), but may also return a member of
 * another entity.
 *
 * If no entity can be found (e.g. pointer is itself result of a
 * Load), NULL is returned.
 */
static ir_entity *find_entity(ir_node *ptr)
{
	switch (get_irn_opcode(ptr)) {
	case iro_Address:
		return get_Address_entity(ptr);
	case iro_Member:
		return get_Member_entity(ptr);
	case iro_Sub:
	case iro_Add: {
		ir_node *left = get_binop_left(ptr);
		if (mode_is_reference(get_irn_mode(left)))
			return find_entity(left);
		ir_node *right = get_binop_right(ptr);
		if (mode_is_reference(get_irn_mode(right)))
			return find_entity(right);
		return NULL;
	}
	default:
		return NULL;
	}
}

/**
 * Returns true, if the entity that the given pointer points to is
 * volatile itself, or if it is part of a larger volatile entity.
 *
 * If no entity can be found (@see find_entity), the functions assumes
 * volatility.
 */
static bool is_inside_volatile_entity(ir_node *ptr)
{
	ir_entity *ent = find_entity(ptr);

	// TODO Probably a pointer, follow the Load(s) to the actual entity
	if (!ent)
		return true;

	if (get_entity_volatility(ent) == volatility_is_volatile) {
		return true;
	}

	if (is_Sel(ptr)) {
		ir_node *sel_ptr = get_Sel_ptr(ptr);
		return is_inside_volatile_entity(sel_ptr);
	} else {
		return false;
	}

}

/**
 * Returns true, if the given type is compound and contains at least
 * one entity which is volatile.
 */
static bool contains_volatile_entity(ir_type *type)
{
	if (!is_compound_type(type))
		return false;

	for (size_t i = 0, n = get_compound_n_members(type); i < n; ++i) {
		ir_entity *ent = get_compound_member(type, i);
		if (get_entity_volatility(ent) == volatility_is_volatile)
			return true;

		ir_type *ent_type = get_entity_type(ent);
		if (contains_volatile_entity(ent_type))
			return true;
	}

	return false;
}

/**
 * Returns true, if the entity that the given pointer points to is...
 * - volatile itself
 * - part of a larger volatile entity
 * - of a type which contains volatile entities.
 *
 * If no entity can be found (@see find_entity), the function assumes
 * volatility.
 */
bool is_partly_volatile(ir_node *ptr)
{
	ir_entity *ent = find_entity(ptr);
	if (!ent)
		return true;

	ir_type *type = get_entity_type(ent);
	return contains_volatile_entity(type) || is_inside_volatile_entity(ptr);
}
