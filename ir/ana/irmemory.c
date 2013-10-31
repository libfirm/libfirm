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
#include <stdlib.h>
#include <stdbool.h>

#include "adt/pmap.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"
#include "irmemory_t.h"
#include "irmemory.h"
#include "irflag.h"
#include "hashptr.h"
#include "irflag.h"
#include "irouts.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "debug.h"
#include "error.h"
#include "typerep.h"
#include "type_t.h"

/** The debug handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)
DEBUG_ONLY(static firm_dbg_module_t *dbgcall = NULL;)

/** The source language specific language disambiguator function. */
static DISAMBIGUATOR_FUNC language_disambuigator = NULL;

/** The global memory disambiguator options. */
static unsigned global_mem_disamgig_opt = aa_opt_no_opt;

const char *get_ir_alias_relation_name(ir_alias_relation rel)
{
#define X(a) case a: return #a
	switch (rel) {
	X(ir_no_alias);
	X(ir_may_alias);
	X(ir_sure_alias);
	}
#undef X
	panic("UNKNOWN alias relation");
}

unsigned get_irg_memory_disambiguator_options(const ir_graph *irg)
{
	unsigned opt = irg->mem_disambig_opt;
	if (opt & aa_opt_inherited)
		return global_mem_disamgig_opt;
	return opt;
}

void set_irg_memory_disambiguator_options(ir_graph *irg, unsigned options)
{
	irg->mem_disambig_opt = options & ~aa_opt_inherited;
}

void set_irp_memory_disambiguator_options(unsigned options)
{
	global_mem_disamgig_opt = options;
}

ir_storage_class_class_t get_base_sc(ir_storage_class_class_t x)
{
	return x & ~ir_sc_modifiers;
}

/**
 * Find the base address and entity of an Sel node.
 *
 * @param sel  the node
 * @param pEnt after return points to the base entity.
 *
 * @return the base address.
 */
static ir_node *find_base_adr(const ir_node *sel, ir_entity **pEnt)
{
	ir_node *ptr = get_Sel_ptr(sel);

	while (is_Sel(ptr)) {
		sel = ptr;
		ptr = get_Sel_ptr(sel);
	}
	*pEnt = get_Sel_entity(sel);
	return ptr;
}

/**
 * Check if a given Const node is greater or equal a given size.
 *
 * @param cns   a Const node
 * @param size  a integer size
 *
 * @return ir_no_alias if the Const is greater, ir_may_alias else
 */
static ir_alias_relation check_const(const ir_node *cns, int size)
{
	ir_tarval *tv = get_Const_tarval(cns);

	if (size == 0)
		return tarval_is_null(tv) ? ir_may_alias : ir_no_alias;
	ir_tarval *tv_size = new_tarval_from_long(size, get_tarval_mode(tv));
	return tarval_cmp(tv_size, tv) & (ir_relation_less_equal) ? ir_no_alias : ir_may_alias;
}

/**
 * Treat idx1 and idx2 as integer indexes and check if they differ always more than size.
 *
 * @param idx1  a node representing the first index
 * @param idx2  a node representing the second index
 * @param size  an integer size
 *
 * @return ir_sure_alias iff idx1 == idx2
 *         ir_no_alias iff they ALWAYS differ more than size
 *         ir_may_alias else
 */
static ir_alias_relation different_index(const ir_node *idx1,
                                         const ir_node *idx2, int size)
{
	if (idx1 == idx2)
		return ir_sure_alias;
	if (is_Const(idx1) && is_Const(idx2)) {
		/* both are const, we can compare them */
		ir_tarval *tv1 = get_Const_tarval(idx1);
		ir_tarval *tv2 = get_Const_tarval(idx2);
		if (size == 0)
			return tv1 == tv2 ? ir_sure_alias : ir_no_alias;

		/* arg, modes may be different */
		ir_mode *m1 = get_tarval_mode(tv1);
		ir_mode *m2 = get_tarval_mode(tv2);
		if (m1 != m2) {
			int size = get_mode_size_bits(m1) - get_mode_size_bits(m2);

			if (size < 0) {
				/* m1 is a small mode, cast up */
				m1 = mode_is_signed(m1) ? find_signed_mode(m2) : find_unsigned_mode(m2);
				if (m1 == NULL) {
					/* should NOT happen, but if it does we give up here */
					return ir_may_alias;
				}
				tv1 = tarval_convert_to(tv1, m1);
			} else if (size > 0) {
				/* m2 is a small mode, cast up */
				m2 = mode_is_signed(m2) ? find_signed_mode(m1) : find_unsigned_mode(m1);
				if (m2 == NULL) {
					/* should NOT happen, but if it does we give up here */
					return ir_may_alias;
				}
				tv2 = tarval_convert_to(tv2, m2);
			}
			/* here the size should be identical, check for signed */
			if (get_mode_sign(m1) != get_mode_sign(m2)) {
				/* find the signed */
				if (mode_is_signed(m2)) {
					ir_tarval *t = tv1;
					tv1 = tv2;
					tv2 = t;
					ir_mode *tm = m1;
					m1 = m2;
					m2 = tm;
				}

				/* m1 is now the signed one */
				if (!tarval_is_negative(tv1)) {
					/* tv1 is signed, but >= 0, simply cast into unsigned */
					tv1 = tarval_convert_to(tv1, m2);
				} else {
					ir_tarval *tv_size = new_tarval_from_long(size, m2);
					if (tarval_cmp(tv2, tv_size) & (ir_relation_greater_equal)) {
						/* tv1 is negative and tv2 >= tv_size, so the difference is bigger than size */
						return ir_no_alias;
					}
					/* tv_size > tv2, so we can subtract without overflow */
					tv2 = tarval_sub(tv_size, tv2, NULL);

					/* tv1 is < 0, so we can negate it */
					tv1 = tarval_neg(tv1);

					/* cast it into unsigned. for two-complement it does the right thing for MIN_INT */
					tv1 = tarval_convert_to(tv1, m2);

					/* now we can compare without overflow */
					return tarval_cmp(tv1, tv2) & (ir_relation_greater_equal) ? ir_no_alias : ir_may_alias;
				}
			}
		}
		if (tarval_cmp(tv1, tv2) == ir_relation_greater) {
			ir_tarval *t = tv1;
			tv1 = tv2;
			tv2 = t;
		}
		/* tv1 is now the "smaller" one */
		ir_tarval *tv      = tarval_sub(tv2, tv1, NULL);
		ir_tarval *tv_size = new_tarval_from_long(size, get_tarval_mode(tv));
		return tarval_cmp(tv_size, tv) & (ir_relation_less_equal) ? ir_no_alias : ir_may_alias;
	}

	/* Note: we rely here on the fact that normalization puts constants on the RIGHT side */
	if (is_Add(idx1)) {
		ir_node *l1 = get_Add_left(idx1);
		ir_node *r1 = get_Add_right(idx1);

		if (l1 == idx2) {
			/* x + c == y */
			if (is_Const(r1))
				return check_const(r1, size);
		}
		if (is_Add(idx2)) {
			/* both are Adds, check if they are of x + a == x + b kind */
			ir_node *l2 = get_Add_left(idx2);
			ir_node *r2 = get_Add_right(idx2);

			if (l1 == l2)
				return different_index(r1, r2, size);
			else if (l1 == r2)
				return different_index(r1, l2, size);
			else if (r1 == r2)
				return different_index(l1, l2, size);
			else if (r1 == l2)
				return different_index(l1, r2, size);
		}
	}
	if (is_Add(idx2)) {
		ir_node *l2 = get_Add_left(idx2);
		ir_node *r2 = get_Add_right(idx2);

		if (l2 == idx1) {
			/* x + c == y */
			if (is_Const(r2))
				return check_const(r2, size);
		}
	}

	if (is_Sub(idx1)) {
		ir_node *l1 = get_Sub_left(idx1);
		ir_node *r1 = get_Sub_right(idx1);

		if (l1 == idx2) {
			/* x - c == y */
			if (is_Const(r1))
				return check_const(r1, size);
		}

		if (is_Sub(idx2)) {
			/* both are Subs, check if they are of x - a == x - b kind */
			ir_node *l2 = get_Sub_left(idx2);

			if (l1 == l2) {
				ir_node *r2 = get_Sub_right(idx2);
				return different_index(r1, r2, size);
			}
		}
	}
	if (is_Sub(idx2)) {
		ir_node *l2 = get_Sub_left(idx2);
		ir_node *r2 = get_Sub_right(idx2);

		if (l2 == idx1) {
			/* x - c == y */
			if (is_Const(r2))
				return check_const(r2, size);
		}

	}
	return ir_may_alias;
}

/**
 * Two Sel addresses have the same base address, check if their offsets are
 * different.
 *
 * @param adr1  The first address.
 * @param adr2  The second address.
 */
static ir_alias_relation different_sel_offsets(const ir_node *sel1,
                                               const ir_node *sel2)
{
	/* TODO: fix */
	(void) sel1;
	(void) sel2;
#if 0
	ir_entity *ent1 = get_Sel_entity(sel1);
	ir_entity *ent2 = get_Sel_entity(sel2);
	int i, check_arr = 0;

	if (ent1 == ent2)
		check_arr = 1;
	else {
		ir_type *tp1 = get_entity_type(ent1);
		ir_type *tp2 = get_entity_type(ent2);

		if (tp1 == tp2)
			check_arr = 1;
		else if (get_type_state(tp1) == layout_fixed && get_type_state(tp2) == layout_fixed &&
		         get_type_size_bytes(tp1) == get_type_size_bytes(tp2))
			check_arr = 1;
	}
	if (check_arr) {
		/* we select an entity of same size, check for indexes */
		int n = get_Sel_n_indexs(sel1);
		int have_no = 0;

		if (n > 0 && n == get_Sel_n_indexs(sel2)) {
			/* same non-zero number of indexes, an array access, check */
			for (i = 0; i < n; ++i) {
				ir_node *idx1 = get_Sel_index(sel1, i);
				ir_node *idx2 = get_Sel_index(sel2, i);
				ir_alias_relation res = different_index(idx1, idx2, 0); /* we can safely IGNORE the size here if it's at least >0 */

				if (res == may_alias)
					return may_alias;
				else if (res == no_alias)
					have_no = 1;
			}
			/* if we have at least one no_alias, there is no alias relation, else we have sure */
			return have_no > 0 ? no_alias : sure_alias;
		}
	}
#else
	(void) different_index;
#endif
	return ir_may_alias;
}

/**
 * Determine the alias relation by checking if adr1 and adr2 are pointer
 * to different type.
 *
 * @param adr1    The first address.
 * @param adr2    The second address.
 */
static ir_alias_relation different_types(const ir_node *adr1,
                                         const ir_node *adr2)
{
	ir_entity *ent1 = NULL;
	ir_entity *ent2 = NULL;

	if (is_SymConst_addr_ent(adr1))
		ent1 = get_SymConst_entity(adr1);
	else if (is_Sel(adr1))
		ent1 = get_Sel_entity(adr1);

	if (is_SymConst_addr_ent(adr2))
		ent2 = get_SymConst_entity(adr2);
	else if (is_Sel(adr2))
		ent2 = get_Sel_entity(adr2);

	if (ent1 != NULL && ent2 != NULL) {
		ir_type *tp1 = get_entity_type(ent1);
		ir_type *tp2 = get_entity_type(ent2);

		if (tp1 != tp2) {
			/* do deref until no pointer types are found */
			while (is_Pointer_type(tp1) && is_Pointer_type(tp2)) {
				tp1 = get_pointer_points_to_type(tp1);
				tp2 = get_pointer_points_to_type(tp2);
			}

			if (get_type_tpop(tp1) != get_type_tpop(tp2)) {
				/* different type structure */
				return ir_no_alias;
			}
			if (is_Class_type(tp1)) {
				/* check class hierarchy */
				if (!is_SubClass_of(tp1, tp2) && !is_SubClass_of(tp2, tp1))
					return ir_no_alias;
			} else {
				/* different types */
				return ir_no_alias;
			}
		}
	}
	return ir_may_alias;
}

/**
 * Returns non-zero if a node is a result on a malloc-like routine.
 *
 * @param node  the Proj node to test
 */
static int is_malloc_Result(const ir_node *node)
{
	node = get_Proj_pred(node);
	if (!is_Proj(node))
		return 0;
	node = get_Proj_pred(node);
	if (!is_Call(node))
		return 0;
	ir_entity *callee = get_Call_callee(node);
	if (callee != NULL
	    && get_entity_additional_properties(callee) & mtp_property_malloc)
		return 1;
	return 0;
}

ir_storage_class_class_t classify_pointer(const ir_node *irn,
                                          const ir_entity *ent)
{
	ir_graph                *irg = get_irn_irg(irn);
	ir_storage_class_class_t res = ir_sc_pointer;
	if (is_SymConst_addr_ent(irn)) {
		ir_entity *entity = get_SymConst_entity(irn);
		ir_type   *owner  = get_entity_owner(entity);
		res = owner == get_tls_type() ? ir_sc_tls : ir_sc_globalvar;
		if (!(get_entity_usage(entity) & ir_usage_address_taken))
			res |= ir_sc_modifier_nottaken;
	} else if (irn == get_irg_frame(irg)) {
		res = ir_sc_localvar;
		if (ent != NULL && !(get_entity_usage(ent) & ir_usage_address_taken))
			res |= ir_sc_modifier_nottaken;
	} else if (is_Proj(irn) && is_malloc_Result(irn)) {
		return ir_sc_malloced;
	} else if (is_Const(irn)) {
		return ir_sc_globaladdr;
	} else if (is_arg_Proj(irn)) {
		res |= ir_sc_modifier_argument;
	}

	return res;
}

static const ir_node *skip_bitfield_sels(const ir_node *adr)
{
	while (is_Sel(adr)) {
		ir_entity *entity = get_Sel_entity(adr);
		if (is_compound_type(get_entity_owner(entity)) &&
			get_entity_bitfield_size(entity) > 0) {
			adr = get_Sel_ptr(adr);
		} else {
			break;
		}
	}
	return adr;
}

/**
 * Determine the alias relation between two addresses.
 *
 * @param addr1  pointer address of the first memory operation
 * @param type1  the type of the accessed data through addr1
 * @param addr2  pointer address of the second memory operation
 * @param type2  the type of the accessed data through addr2
 *
 * @return found memory relation
 */
static ir_alias_relation _get_alias_relation(
	const ir_node *adr1, const ir_type *const type1,
	const ir_node *adr2, const ir_type *const type2)
{

	if (!get_opt_alias_analysis())
		return ir_may_alias;

	if (adr1 == adr2)
		return ir_sure_alias;

	ir_graph *const irg     = get_irn_irg(adr1);
	unsigned  const options = get_irg_memory_disambiguator_options(irg);

	/* The Armageddon switch */
	if (options & aa_opt_no_alias)
		return ir_no_alias;

	/* do the addresses have constants offsets from the same base?
	 *  Note: sub X, C is normalized to add X, -C
	 */
	long           offset1            = 0;
	long           offset2            = 0;
	const ir_node *sym_offset1        = NULL;
	const ir_node *sym_offset2        = NULL;
	const ir_node *orig_adr1          = adr1;
	const ir_node *orig_adr2          = adr2;
	bool           have_const_offsets = true;

	/*
	 * Currently, only expressions with at most one symbolic
	 * offset can be handled.  To extend this, change
	 * sym_offset{1,2} to be sets, and compare the sets.
	 */

	while (is_Add(adr1)) {
		ir_node *ptr_node;
		ir_node *int_node;

		ir_mode *mode_left = get_irn_mode(get_Add_left(adr1));

		if (mode_is_reference(mode_left)) {
			ptr_node = get_Add_left(adr1);
			int_node = get_Add_right(adr1);
		} else {
			ptr_node = get_Add_right(adr1);
			int_node = get_Add_left(adr1);
		}

		if (is_Const(int_node)) {
			ir_tarval *tv  = get_Const_tarval(int_node);
			offset1       += get_tarval_long(tv);
		} else if (sym_offset1 == NULL) {
			sym_offset1 = int_node;
		} else {
			// adr1 has more than one symbolic offset.
			// Give up
			have_const_offsets = false;
		}

		adr1 = ptr_node;
	}

	while (is_Add(adr2)) {
		ir_node *ptr_node;
		ir_node *int_node;

		ir_mode *mode_left = get_irn_mode(get_Add_left(adr2));

		if (mode_is_reference(mode_left)) {
			ptr_node = get_Add_left(adr2);
			int_node = get_Add_right(adr2);
		} else {
			ptr_node = get_Add_right(adr2);
			int_node = get_Add_left(adr2);
		}

		if (is_Const(int_node)) {
			ir_tarval *tv  = get_Const_tarval(int_node);
			offset2       += get_tarval_long(tv);
		} else if (sym_offset2 == NULL) {
			sym_offset2 = int_node;
		} else {
			// adr2 has more than one symbolic offset.
			// Give up
			have_const_offsets = false;
		}

		adr2 = ptr_node;
	}

	unsigned type_size = get_type_size_bytes(type1);
	if (get_type_size_bytes(type2) > type_size) {
		type_size = get_type_size_bytes(type2);
	}

	/* same base address -> compare offsets if possible.
	 * FIXME: type long is not sufficient for this task ...
	 */
	if (adr1 == adr2 && sym_offset1 == sym_offset2 && have_const_offsets) {
		unsigned long first_offset, last_offset;
		unsigned first_type_size;

		if (offset1 <= offset2) {
			first_offset = offset1;
			last_offset = offset2;
			first_type_size = get_type_size_bytes(type1);
		} else {
			first_offset = offset2;
			last_offset = offset1;
			first_type_size = get_type_size_bytes(type2);
		}

		if (first_offset + first_type_size <= last_offset)
			return ir_no_alias;
		else
			return ir_sure_alias;
	}

	 /*
	  * Bitfields can be constructed as Sels from its base address.
	  * As they have different entities, the disambiguator would find that they
	  * are alias free. While this is true for its values, it is false for the
	  * addresses (strictly speaking, the Sel's are NOT the addresses of the
	  * bitfields).
	  * So, skip those bitfield selecting Sel's.
	  */
	adr1 = skip_bitfield_sels(adr1);
	adr2 = skip_bitfield_sels(adr2);

	/* skip Sels */
	const ir_node *base1 = adr1;
	const ir_node *base2 = adr2;
	ir_entity *ent1  = NULL;
	ir_entity *ent2  = NULL;
	if (is_Sel(adr1)) {
		base1 = find_base_adr(adr1, &ent1);
	}
	if (is_Sel(adr2)) {
		base2 = find_base_adr(adr2, &ent2);
	}

	/* same base address -> compare Sel entities */
	if (base1 == base2 && ent1 != NULL && ent2 != NULL) {
		if (ent1 != ent2)
			return ir_no_alias;
		else if (have_const_offsets)
			return different_sel_offsets(adr1, adr2);
	}

	ir_storage_class_class_t mod1   = classify_pointer(base1, ent1);
	ir_storage_class_class_t mod2   = classify_pointer(base2, ent2);
	ir_storage_class_class_t class1 = get_base_sc(mod1);
	ir_storage_class_class_t class2 = get_base_sc(mod2);

	/* struct-access cannot alias with variables */
	if (ent1 == NULL && ent2 != NULL && is_compound_type(get_entity_owner(ent2))
		&& (class1 == ir_sc_globalvar || class1 == ir_sc_localvar || class1 == ir_sc_tls || class1 == ir_sc_globaladdr)) {
		return ir_no_alias;
	}
	if (ent2 == NULL && ent1 != NULL && is_compound_type(get_entity_owner(ent1))
		&& (class2 == ir_sc_globalvar || class2 == ir_sc_localvar || class2 == ir_sc_tls || class2 == ir_sc_globaladdr)) {
		return ir_no_alias;
	}

	if (class1 == ir_sc_pointer || class2 == ir_sc_pointer) {
		/* swap pointer class to class1 */
		if (class2 == ir_sc_pointer) {
			ir_storage_class_class_t temp = mod1;
			mod1 = mod2;
			mod2 = temp;
			class1 = get_base_sc(mod1);
			class2 = get_base_sc(mod2);
		}
		/* a pointer and an object whose address was never taken */
		if (mod2 & ir_sc_modifier_nottaken) {
			return ir_no_alias;
		}
		if (mod1 & ir_sc_modifier_argument) {
			if ( (options & aa_opt_no_alias_args)
					&& (mod2 & ir_sc_modifier_argument))
				return ir_no_alias;
			if ( (options & aa_opt_no_alias_args_global)
					&& (class2 == ir_sc_globalvar
						|| class2 == ir_sc_tls
						|| class2 == ir_sc_globaladdr))
				return ir_no_alias;
		}
	} else if (class1 != class2) {
		/* two objects from different memory spaces */
		return ir_no_alias;
	} else {
		/* both classes are equal */
		if (class1 == ir_sc_globalvar) {
			ir_entity *entity1 = get_SymConst_entity(base1);
			ir_entity *entity2 = get_SymConst_entity(base2);
			if (entity1 != entity2)
				return ir_no_alias;

			/* for some reason CSE didn't happen yet for the 2 SymConsts... */
			return ir_may_alias;
		} else if (class1 == ir_sc_globaladdr) {
			ir_tarval *tv = get_Const_tarval(base1);
			offset1      += get_tarval_long(tv);
			tv            = get_Const_tarval(base2);
			offset2      += get_tarval_long(tv);

			if ((unsigned long)labs(offset2 - offset1) >= type_size)
				return ir_no_alias;
			else
				return ir_sure_alias;
		}
	}

	/* Type based alias analysis */
	if (options & aa_opt_type_based) {
		ir_alias_relation rel;

		if (options & aa_opt_byte_type_may_alias) {
			if (get_type_size_bytes(type1) == 1 || get_type_size_bytes(type2) == 1) {
				/* One of the types address a byte. Assume a ir_may_alias and leave
				   the type based check. */
				goto leave_type_based_alias;
			}
		}

		/* cheap check: If the type sizes did not match, the types MUST be different */
		if (get_type_size_bytes(type1) != get_type_size_bytes(type2))
			return ir_no_alias;

		/* cheap test: if only one is a reference type, no alias */
		if (is_Pointer_type(type1) != is_Pointer_type(type2))
			return ir_no_alias;

		if (is_Primitive_type(type1) && is_Primitive_type(type2)) {
			const ir_mode *const mode1 = get_type_mode(type1);
			const ir_mode *const mode2 = get_type_mode(type2);

			/* cheap test: if arithmetic is different, no alias */
			if (get_mode_arithmetic(mode1) != get_mode_arithmetic(mode2))
				return ir_no_alias;

		}

		/* try rule R5 */
		rel = different_types(orig_adr1, orig_adr2);
		if (rel != ir_may_alias)
			return rel;
leave_type_based_alias:;
	}

	/* do we have a language specific memory disambiguator? */
	if (language_disambuigator != NULL) {
		ir_alias_relation rel = language_disambuigator(orig_adr1, type1, orig_adr2, type2);
		if (rel != ir_may_alias)
			return rel;
	}

	/* access points-to information here */
	return ir_may_alias;
}

ir_alias_relation get_alias_relation(
	const ir_node *const adr1, const ir_type *const type1,
	const ir_node *const adr2, const ir_type *const type2)
{
	ir_alias_relation rel = _get_alias_relation(adr1, type1, adr2, type2);
	DB((dbg, LEVEL_1, "alias(%+F, %+F) = %s\n", adr1, adr2, get_ir_alias_relation_name(rel)));
	return rel;
}

void set_language_memory_disambiguator(DISAMBIGUATOR_FUNC func)
{
	language_disambuigator = func;
}

/** The result cache for the memory disambiguator. */
static set *result_cache = NULL;

/** An entry in the relation cache. */
typedef struct mem_disambig_entry {
	const ir_node     *adr1;    /**< The first address. */
	const ir_type     *type1;   /**< The first address type. */
	const ir_node     *adr2;    /**< The second address. */
	const ir_type     *type2;   /**< The second address type. */
	ir_alias_relation result;   /**< The alias relation result. */
} mem_disambig_entry;

#define HASH_ENTRY(adr1, adr2)  (hash_ptr(adr1) ^ hash_ptr(adr2))

/**
 * Compare two relation cache entries.
 */
static int cmp_mem_disambig_entry(const void *elt, const void *key, size_t size)
{
	(void) size;
	const mem_disambig_entry *p1 = (const mem_disambig_entry*) elt;
	const mem_disambig_entry *p2 = (const mem_disambig_entry*) key;
	return p1->adr1 == p2->adr1 && p1->adr2 == p2->adr2 &&
	       p1->type1 == p2->type1 && p1->type2 == p2->type2;
}

void mem_disambig_init(void)
{
	result_cache = new_set(cmp_mem_disambig_entry, 8);
}

ir_alias_relation get_alias_relation_ex(
	const ir_node *adr1, const ir_type *type1,
	const ir_node *adr2, const ir_type *type2)
{
	ir_fprintf(stderr, "%+F <-> %+F\n", adr1, adr2);

	if (!get_opt_alias_analysis())
		return ir_may_alias;

	if (get_irn_opcode(adr1) > get_irn_opcode(adr2)) {
		const ir_node *t = adr1;
		adr1 = adr2;
		adr2 = t;
	}

	mem_disambig_entry key;
	key.adr1  = adr1;
	key.adr2  = adr2;
	key.type1 = type1;
	key.type2 = type2;
	mem_disambig_entry *entry = set_find(mem_disambig_entry, result_cache, &key, sizeof(key), HASH_ENTRY(adr1, adr2));
	if (entry != NULL)
		return entry->result;

	key.result = get_alias_relation(adr1, type1, adr2, type2);

	(void)set_insert(mem_disambig_entry, result_cache, &key, sizeof(key), HASH_ENTRY(adr1, adr2));
	return key.result;
}

void mem_disambig_term(void)
{
	if (result_cache != NULL) {
		del_set(result_cache);
		result_cache = NULL;
	}
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
static int is_hidden_cast(const ir_mode *mode, const ir_mode *ent_mode)
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
                                              ir_entity *entity)
{
	unsigned res = 0;
	for (int i = get_irn_n_outs(irn); i-- > 0; ) {
		ir_node *succ = get_irn_out(irn, i);

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
			if (irn == get_Store_value(succ)) {
				res |= ir_usage_unknown;
			}
			if (irn == get_Store_ptr(succ)) {
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
			ir_type *tp  = get_entity_type(entity);
			if (tp != get_CopyB_type(succ)) {
				/* bad, different types, might be a hidden conversion */
				res |= ir_usage_reinterpret_cast;
			}
			if (irn == get_CopyB_dst(succ)) {
				res |= ir_usage_write;
			} else {
				assert(irn == get_CopyB_src(succ));
				res |= ir_usage_read;
			}
			break;
		}

		case iro_Add:
		case iro_Sub:
			/* Check the successor of irn. */
			res |= determine_entity_usage(succ, entity);
			break;
		case iro_Sel: {
			ir_entity *sel_entity = get_Sel_entity(succ);
			/* this analysis can't handle unions correctly */
			if (is_Union_type(get_entity_owner(sel_entity))) {
				res |= ir_usage_unknown;
				break;
			}
			/* Check the successor of irn. */
			res |= determine_entity_usage(succ, sel_entity);
			break;
		}

		case iro_Call:
			if (irn == get_Call_ptr(succ)) {
				/* TODO: we could check for reinterpret casts here...
				 * But I doubt anyone is interested in that bit for
				 * function entities and I'm too lazy to write the code now.
				 */
				res |= ir_usage_read;
			} else {
				assert(irn != get_Call_mem(succ));
				res |= ir_usage_unknown;
			}
			break;

		/* skip identities */
		case iro_Id:
			res |= determine_entity_usage(succ, entity);
			break;

		/* skip tuples */
		case iro_Tuple: {
			int input_nr;
			for (input_nr = get_Tuple_n_preds(succ) - 1; input_nr >= 0;
					--input_nr) {
				ir_node *pred = get_Tuple_pred(succ, input_nr);
				if (pred == irn) {
					int k;
					/* we found one input */
					for (k = get_irn_n_outs(succ) - 1; k >= 0; --k) {
						ir_node *proj = get_irn_out(succ, k);

						if (is_Proj(proj) && get_Proj_proj(proj) == input_nr) {
							res |= determine_entity_usage(proj, entity);
							break;
						}
					}
				}
			}
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
	ir_type *ft = get_irg_frame_type(irg);

	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_OUTS);

	/* set initial state to not_taken, as this is the "smallest" state */
	for (size_t i = 0, n = get_class_n_members(ft); i < n; ++i) {
		ir_entity *ent = get_class_member(ft, i);

		/* methods can only be analyzed globally */
		if (!is_method_entity(ent)) {
			ir_entity_usage flags = ir_usage_none;
			if (get_entity_linkage(ent) & IR_LINKAGE_HIDDEN_USER)
				flags = ir_usage_unknown;
			set_entity_usage(ent, flags);
		}
	}

	ir_node *irg_frame = get_irg_frame(irg);

	for (int j = get_irn_n_outs(irg_frame); j-- > 0; ) {
		ir_node        *succ = get_irn_out(irg_frame, j);
		ir_entity      *entity;
		unsigned        flags;

		if (!is_Sel(succ))
			continue;

		entity = get_Sel_entity(succ);
		flags  = get_entity_usage(entity);
		flags |= determine_entity_usage(succ, entity);
		set_entity_usage(entity, (ir_entity_usage) flags);
	}

	/* check inner functions accessing outer frame */
	int static_link_arg = 0;
	for (size_t i = 0, n = get_class_n_members(ft); i < n; ++i) {
		ir_entity *ent = get_class_member(ft, i);
		if (!is_method_entity(ent))
			continue;

		ir_graph *inner_irg = get_entity_irg(ent);
		if (inner_irg == NULL)
			continue;

		assure_irg_outs(inner_irg);
		ir_node *args = get_irg_args(inner_irg);
		for (int j = get_irn_n_outs(args); j-- > 0; ) {
			ir_node *arg = get_irn_out(args, j);

			if (get_Proj_proj(arg) == static_link_arg) {
				for (int k = get_irn_n_outs(arg); k-- > 0; ) {
					ir_node *succ = get_irn_out(arg, k);

					if (is_Sel(succ)) {
						ir_entity *entity = get_Sel_entity(succ);

						if (get_entity_owner(entity) == ft) {
							/* found an access to the outer frame */
							unsigned flags;

							flags  = get_entity_usage(entity);
							flags |= determine_entity_usage(succ, entity);
							set_entity_usage(entity, (ir_entity_usage) flags);
						}
					}
				}
			}
		}
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
 * Mark all entities used in the initializer as unknown usage.
 *
 * @param initializer  the initializer to check
 */
static void check_initializer_nodes(ir_initializer_t *initializer)
{
	switch (initializer->kind) {
	case IR_INITIALIZER_CONST: {
		/* let's check if it's an address */
		ir_node *n = initializer->consti.value;
		if (is_SymConst_addr_ent(n)) {
			ir_entity *ent = get_SymConst_entity(n);
			set_entity_usage(ent, ir_usage_unknown);
		}
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
	/* Beware: Methods are always initialized with "themself". This does not
	 * count as a taken address.
	 * TODO: this initialisation with "themself" is wrong and should be removed
	 */
	if (is_Method_type(get_entity_type(ent)))
		return;

	if (ent->initializer != NULL) {
		check_initializer_nodes(ent->initializer);
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
	(void) data;
	if (!is_SymConst_addr_ent(irn))
		return;

	ir_entity *entity = get_SymConst_entity(irn);
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

	for (size_t i = 0, n = get_irp_n_irgs(); i < n; ++i) {
		ir_graph *irg = get_irp_irg(i);

		assure_irg_outs(irg);
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
static ir_type *clone_type_and_cache(ir_type *tp)
{
	ir_type *res = pmap_get(ir_type, mtp_map, tp);
	if (res == NULL) {
		res = clone_type_method(tp);
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
		ctp = clone_type_and_cache(ctp);
		add_method_additional_properties(ctp, mtp_property_private);
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
	for (size_t i = 0, n = get_irp_n_irgs(); i < n; ++i) {
		ir_graph        *irg   = get_irp_irg(i);
		ir_entity       *ent   = get_irg_entity(irg);
		ir_entity_usage  flags = get_entity_usage(ent);

		if (!(flags & ir_usage_address_taken) && !entity_is_externally_visible(ent)) {
			ir_type *mtp = get_entity_type(ent);

			add_entity_additional_properties(ent, mtp_property_private);
			DB((dbgcall, LEVEL_1, "found private method %+F\n", ent));
			if ((get_method_additional_properties(mtp) & mtp_property_private) == 0) {
				/* need a new type */
				mtp = clone_type_and_cache(mtp);
				add_method_additional_properties(mtp, mtp_property_private);
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
	case iro_SymConst:
		return get_SymConst_entity(ptr);
	case iro_Sel:
		return get_Sel_entity(ptr);
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
	if (!ent) return true;

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
	size_t n;

	switch (get_type_tpop_code(type)) {
	case tpo_class:  n = get_class_n_members(type);
		break;
	case tpo_struct: n = get_struct_n_members(type);
		break;
	case tpo_union:  n = get_union_n_members(type);
		break;
	default:
		return false;
	}

	for (size_t i = 0; i < n; i++) {
		ir_entity *ent;

		switch (get_type_tpop_code(type)) {
		case tpo_class:  ent = get_class_member(type, i);
			break;
		case tpo_struct: ent = get_struct_member(type, i);
			break;
		case tpo_union:  ent = get_union_member(type, i);
			break;
		default:         abort(); // Should never happen
		}

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
	if (!ent) return true;

	ir_type *type = get_entity_type(ent);

	return contains_volatile_entity(type) ||
		is_inside_volatile_entity(ptr);
}
