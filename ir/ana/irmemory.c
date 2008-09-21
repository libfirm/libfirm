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
 * @brief    Memory disambiguator
 * @author   Michael Beck
 * @date     27.12.2006
 * @version  $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdbool.h>

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

/** The debug handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/** The source language specific language disambiguator function. */
static DISAMBIGUATOR_FUNC language_disambuigator = NULL;

/** The global memory disambiguator options. */
static unsigned global_mem_disamgig_opt = aa_opt_no_opt;

/* Returns a human readable name for an alias relation. */
const char *get_ir_alias_relation_name(ir_alias_relation rel) {
#define X(a) case a: return #a
	switch (rel) {
	X(ir_no_alias);
	X(ir_may_alias);
	X(ir_sure_alias);
	default: assert(0); return "UNKNOWN";
	}
#undef X
}

/* Get the memory disambiguator options for a graph. */
unsigned get_irg_memory_disambiguator_options(ir_graph *irg) {
	unsigned opt = irg->mem_disambig_opt;
	if (opt & aa_opt_inherited)
		return global_mem_disamgig_opt;
	return opt;
}  /* get_irg_memory_disambiguator_options */

/*  Set the memory disambiguator options for a graph. */
void set_irg_memory_disambiguator_options(ir_graph *irg, unsigned options) {
	irg->mem_disambig_opt = options & ~aa_opt_inherited;
}  /* set_irg_memory_disambiguator_options */

/* Set the global disambiguator options for all graphs not having local options. */
void set_irp_memory_disambiguator_options(unsigned options) {
	global_mem_disamgig_opt = options;
}  /* set_irp_memory_disambiguator_options */

/**
 * Find the base address and entity of an Sel node.
 *
 * @param sel  the node
 * @param pEnt after return points to the base entity.
 *
 * @return the base address.
 */
static ir_node *find_base_adr(ir_node *sel, ir_entity **pEnt) {
	ir_node *ptr = get_Sel_ptr(sel);

	while (is_Sel(ptr)) {
		sel = ptr;
		ptr = get_Sel_ptr(sel);
	}
	*pEnt = get_Sel_entity(sel);
	return ptr;
}  /* find_base_adr */

/**
 * Check if a given Const node is greater or equal a given size.
 *
 * @param cns   a Const node
 * @param size  a integer size
 *
 * @return ir_no_alias if the Const is greater, ir_may_alias else
 */
static ir_alias_relation check_const(ir_node *cns, int size) {
	tarval *tv = get_Const_tarval(cns);
	tarval *tv_size;

	if (size == 0)
		return tarval_is_null(tv) ? ir_may_alias : ir_no_alias;
	tv_size = new_tarval_from_long(size, get_tarval_mode(tv));
	return tarval_cmp(tv_size, tv) & (pn_Cmp_Eq|pn_Cmp_Lt) ? ir_no_alias : ir_may_alias;
}  /* check_const */

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
static ir_alias_relation different_index(ir_node *idx1, ir_node *idx2, int size) {
	if (idx1 == idx2)
		return ir_sure_alias;
	if (is_Const(idx1) && is_Const(idx2)) {
		/* both are const, we can compare them */
		tarval *tv1 = get_Const_tarval(idx1);
		tarval *tv2 = get_Const_tarval(idx2);
		tarval *tv, *tv_size;
		ir_mode *m1, *m2;

		if (size == 0)
			return tv1 == tv2 ? ir_sure_alias : ir_no_alias;

		/* arg, modes may be different */
		m1 = get_tarval_mode(tv1);
		m2 = get_tarval_mode(tv2);
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
					tarval *t = tv1;
					ir_mode *tm = m1;
					tv1 = tv2; m1 = m2;
					tv2 = t;   m2 = tm;
				}

				/* m1 is now the signed one */
				if (tarval_cmp(tv1, get_tarval_null(m1)) & (pn_Cmp_Eq|pn_Cmp_Gt)) {
					/* tv1 is signed, but >= 0, simply cast into unsigned */
					tv1 = tarval_convert_to(tv1, m2);
				} else {
					tv_size = new_tarval_from_long(size, m2);

					if (tarval_cmp(tv2, tv_size) & (pn_Cmp_Eq|pn_Cmp_Gt)) {
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
					return tarval_cmp(tv1, tv2) & (pn_Cmp_Eq|pn_Cmp_Gt) ? ir_no_alias : ir_may_alias;
				}
			}
		}
		if (tarval_cmp(tv1, tv2) == pn_Cmp_Gt) {
			tarval *t = tv1;
			tv1 = tv2;
			tv2 = t;
		}
		/* tv1 is now the "smaller" one */
		tv      = tarval_sub(tv2, tv1, NULL);
		tv_size = new_tarval_from_long(size, get_tarval_mode(tv));
		return tarval_cmp(tv_size, tv) & (pn_Cmp_Eq|pn_Cmp_Lt) ? ir_no_alias : ir_may_alias;
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
}  /* different_index */

/**
 * Two Sel addresses have the same base address, check if there offsets are
 * different.
 *
 * @param adr1  The first address.
 * @param adr2  The second address.
 */
static ir_alias_relation different_sel_offsets(ir_node *sel1, ir_node *sel2) {
	/* seems to be broken */
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
		         get_type_size_bits(tp1) == get_type_size_bits(tp2))
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
#endif
	return ir_may_alias;
}  /* different_sel_offsets */

/**
 * Determine the alias relation by checking if adr1 and adr2 are pointer
 * to different type.
 *
 * @param adr1    The first address.
 * @param adr2    The second address.
 */
static ir_alias_relation different_types(ir_node *adr1, ir_node *adr2)
{
	ir_entity *ent1 = NULL, *ent2 = NULL;

	if (is_Global(adr1))
		ent1 = get_Global_entity(adr1);
	else if (is_Sel(adr1))
		ent1 = get_Sel_entity(adr1);

	if (is_Global(adr2))
		ent2 = get_Global_entity(adr2);
	else if (is_Sel(adr2))
		ent2 = get_Sel_entity(adr2);

	if (ent1 != NULL && ent2 != NULL) {
		ir_type *tp1 = get_entity_type(ent1);
		ir_type *tp2 = get_entity_type(ent2);

		if (tp1 != tp2) {
#if 0
			/* do deref until no pointer types are found */
			while (is_Pointer_type(tp1) && is_Pointer_type(tp2)) {
				tp1 = get_pointer_points_to_type(tp1);
				tp2 = get_pointer_points_to_type(tp2);
			}
#endif

			if (get_type_tpop(tp1) != get_type_tpop(tp2)) {
				/* different type structure */
				return ir_no_alias;
			}
			if (is_Class_type(tp1)) {
				/* check class hierarchy */
				if (! is_SubClass_of(tp1, tp2) &&
					! is_SubClass_of(tp2, tp1))
					return ir_no_alias;
			} else {
				/* different types */
				return ir_no_alias;
			}
		}
	}
	return ir_may_alias;
}  /* different_types */

/**
 * Returns non-zero if a node is a result on a malloc-like routine.
 *
 * @param node  the Proj node to test
 */
static int is_malloc_Result(ir_node *node) {
	node = get_Proj_pred(node);
	if (! is_Proj(node))
		return 0;
	node = get_Proj_pred(node);
	if (! is_Call(node))
		return 0;
	node = get_Call_ptr(node);
	if (is_Global(node)) {
		ir_entity *ent = get_Global_entity(node);

		if (get_entity_additional_properties(ent) & mtp_property_malloc)
			return 1;
		return 0;
	}
	return 0;
}  /* is_malloc_Result */

/**
 * Classify a base pointer.
 *
 * @param irg  the graph of the pointer
 * @param irn  the node representing the base address
 * @param ent  the base entity of the base address iff any
 */
ir_storage_class_class_t classify_pointer(ir_graph *irg, ir_node *irn, ir_entity *ent)
{
	ir_storage_class_class_t res = ir_sc_pointer;
	if (is_Global(irn)) {
		ir_entity *entity = get_Global_entity(irn);
		res = ir_sc_globalvar;
		if (! (get_entity_usage(entity) & ir_usage_address_taken))
			res |= ir_sc_modifier_nottaken;
	} else if (irn == get_irg_frame(irg)) {
		res = ir_sc_localvar;
		if (ent != NULL && !(get_entity_usage(ent) & ir_usage_address_taken))
			res |= ir_sc_modifier_nottaken;
	} else if (is_arg_Proj(irn)) {
		return ir_sc_argument;
	} else if (irn == get_irg_tls(irg)) {
		res = ir_sc_tls;
		if (ent != NULL && !(get_entity_usage(ent) & ir_usage_address_taken))
			res |= ir_sc_modifier_nottaken;
	} else if (is_Proj(irn) && is_malloc_Result(irn)) {
		return ir_sc_malloced;
	}

	return res;
}

/**
 * If adr represents a Bitfield Sel, skip it
 */
static ir_node *skip_Bitfield_Sels(ir_node *adr) {
	if (is_Sel(adr)) {
		ir_entity *ent     = get_Sel_entity(adr);
		ir_type   *bf_type = get_entity_type(ent);

		/* is it a bitfield type? */
		if (is_Primitive_type(bf_type) && get_primitive_base_type(bf_type) != NULL)
			adr = get_Sel_ptr(adr);
	}
	return adr;
}

/**
 * Determine the alias relation between two addresses.
 *
 * @param irg    the graph of both memory operations
 * @param addr1  pointer address of the first memory operation
 * @param mode1  the mode of the accessed data through addr1
 * @param addr2  pointer address of the second memory operation
 * @param mode2  the mode of the accessed data through addr2
 *
 * @return found memory relation
 */
static ir_alias_relation _get_alias_relation(
	ir_graph *irg,
	ir_node *adr1, ir_mode *mode1,
	ir_node *adr2, ir_mode *mode2)
{
	ir_entity             *ent1, *ent2;
	unsigned              options;
	long                  offset1 = 0;
	long                  offset2 = 0;
	ir_node               *base1;
	ir_node               *base2;
	ir_node               *orig_adr1 = adr1;
	ir_node               *orig_adr2 = adr2;
	unsigned              mode_size;
	ir_storage_class_class_t class1, class2;
	int                   have_const_offsets;

	if (! get_opt_alias_analysis())
		return ir_may_alias;

	if (adr1 == adr2)
		return ir_sure_alias;

	options = get_irg_memory_disambiguator_options(irg);

	/* The Armageddon switch */
	if (options & aa_opt_no_alias)
		return ir_no_alias;

	/* do the addresses have constants offsets?
	 *  Note: nodes are normalized to have constants at right inputs,
	 *        sub X, C is normalized to add X, -C
	 */
	have_const_offsets = 1;
	while (is_Add(adr1)) {
		ir_node *add_right = get_Add_right(adr1);
		if (is_Const(add_right) && !mode_is_reference(get_irn_mode(add_right))) {
			tarval *tv  = get_Const_tarval(add_right);
			offset1    += get_tarval_long(tv);
			adr1        = get_Add_left(adr1);
		} else if (mode_is_reference(get_irn_mode(add_right))) {
			adr1 = add_right;
			have_const_offsets = 0;
		} else {
			adr1 = get_Add_left(adr1);
			have_const_offsets = 0;
		}
	}
	while (is_Add(adr2)) {
		ir_node *add_right = get_Add_right(adr2);
		if (is_Const(add_right) && !mode_is_reference(get_irn_mode(add_right))) {
			tarval *tv  = get_Const_tarval(add_right);
			offset2    += get_tarval_long(tv);
			adr2        = get_Add_left(adr2);
		} else if (mode_is_reference(get_irn_mode(add_right))) {
			adr2 = add_right;
			have_const_offsets = 0;
		} else {
			adr2 = get_Add_left(adr2);
			have_const_offsets = 0;
		}
	}

	mode_size = get_mode_size_bytes(mode1);
	if (get_mode_size_bytes(mode2) > mode_size) {
		mode_size = get_mode_size_bytes(mode2);
	}

	/* same base address -> compare offsets if possible.
	 * FIXME: type long is not sufficient for this task ...
	 */
	if (adr1 == adr2 && have_const_offsets) {
		if ((unsigned long)labs(offset2 - offset1) >= mode_size)
			return ir_no_alias;
		else
			return ir_sure_alias;
	}

	/*
	 * Bitfields can be constructed as Sels from its base address.
	 * As they have different entities, the disambiguator would find that they are
	 * alias free. While this is true for it's values, it is false for the addresses
	 * (strictly speaking, the Sel's are NOT the addresses of the bitfields).
	 * So, skip those bitfield selecting Sel's.
	 */
	adr1 = skip_Bitfield_Sels(adr1);
	adr2 = skip_Bitfield_Sels(adr2);

	/* skip Sels */
	base1 = adr1;
	base2 = adr2;
	ent1  = NULL;
	ent2  = NULL;
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

	class1 = classify_pointer(irg, base1, ent1);
	class2 = classify_pointer(irg, base2, ent2);

	if (class1 == ir_sc_pointer) {
		if (class2 & ir_sc_modifier_nottaken) {
			/* a pointer and an object whose objects was never taken */
			return ir_no_alias;
		}
	} else if (class2 == ir_sc_pointer) {
		if (class1 & ir_sc_modifier_nottaken) {
			/* a pointer and an object whose objects was never taken */
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
		}
	}

	/* Type based alias analysis */
	if (options & aa_opt_type_based) {
		ir_alias_relation rel;

		if (options & aa_opt_byte_type_may_alias) {
			if (get_mode_size_bits(mode1) == 8 || get_mode_size_bits(mode2) == 8) {
				/* One of the modes address a byte. Assume a ir_may_alias and leave
				   the type based check. */
				goto leave_type_based_alias;
			}
		}
		/* cheap check: If the mode sizes did not match, the types MUST be different */
		if (get_mode_size_bits(mode1) != get_mode_size_bits(mode2))
			return ir_no_alias;

		/* cheap test: if only one is a reference mode, no alias */
		if (mode_is_reference(mode1) != mode_is_reference(mode2))
			return ir_no_alias;

		/* cheap test: if arithmetic is different, no alias */
		if (get_mode_arithmetic(mode1) != get_mode_arithmetic(mode2))
			return ir_no_alias;

		/* try rule R5 */
		rel = different_types(orig_adr1, orig_adr2);
		if (rel != ir_may_alias)
			return rel;
leave_type_based_alias:;
	}

	/* do we have a language specific memory disambiguator? */
	if (language_disambuigator) {
		ir_alias_relation rel = (*language_disambuigator)(irg, orig_adr1, mode1, orig_adr2, mode2);
		if (rel != ir_may_alias)
			return rel;
	}

	/* access points-to information here */
	return ir_may_alias;
}  /* _get_alias_relation */

/*
 * Determine the alias relation between two addresses.
 */
ir_alias_relation get_alias_relation(
	ir_graph *irg,
	ir_node *adr1, ir_mode *mode1,
	ir_node *adr2, ir_mode *mode2)
{
	ir_alias_relation rel = _get_alias_relation(irg, adr1, mode1, adr2, mode2);
	DB((dbg, LEVEL_1, "alias(%+F, %+F) = %s\n", adr1, adr2, get_ir_alias_relation_name(rel)));
	return rel;
}  /* get_alias_relation */

/* Set a source language specific memory disambiguator function. */
void set_language_memory_disambiguator(DISAMBIGUATOR_FUNC func) {
	language_disambuigator = func;
}  /* set_language_memory_disambiguator */

/** The result cache for the memory disambiguator. */
static set *result_cache = NULL;

/** An entry in the relation cache. */
typedef struct mem_disambig_entry {
	ir_node	          *adr1;    /**< The first address. */
	ir_node	          *adr2;    /**< The second address. */
	ir_alias_relation result;   /**< The alias relation result. */
} mem_disambig_entry;

#define HASH_ENTRY(adr1, adr2)	(HASH_PTR(adr1) ^ HASH_PTR(adr2))

/**
 * Compare two relation cache entries.
 */
static int cmp_mem_disambig_entry(const void *elt, const void *key, size_t size) {
	const mem_disambig_entry *p1 = elt;
	const mem_disambig_entry *p2 = key;
	(void) size;

	return p1->adr1 == p2->adr1 && p1->adr2 == p2->adr2;
}  /* cmp_mem_disambig_entry */

/**
 * Initialize the relation cache.
 */
void mem_disambig_init(void) {
	result_cache = new_set(cmp_mem_disambig_entry, 8);
}  /* mem_disambig_init */

/*
 * Determine the alias relation between two addresses.
 */
ir_alias_relation get_alias_relation_ex(
	ir_graph *irg,
	ir_node *adr1, ir_mode *mode1,
	ir_node *adr2, ir_mode *mode2)
{
	mem_disambig_entry key, *entry;

	ir_fprintf(stderr, "%+F <-> %+F\n", adr1, adr2);

	if (! get_opt_alias_analysis())
		return ir_may_alias;

	if (get_irn_opcode(adr1) > get_irn_opcode(adr2)) {
		ir_node *t = adr1;
		adr1 = adr2;
		adr2 = t;
	}

	key.adr1 = adr1;
	key.adr2 = adr2;
	entry = set_find(result_cache, &key, sizeof(key), HASH_ENTRY(adr1, adr2));
	if (entry)
		return entry->result;

	key.result = get_alias_relation(irg, adr1, mode1, adr2, mode2);

	set_insert(result_cache, &key, sizeof(key), HASH_ENTRY(adr1, adr2));
	return key.result;
}  /* get_alias_relation_ex */

/* Free the relation cache. */
void mem_disambig_term(void) {
	if (result_cache) {
		del_set(result_cache);
		result_cache = NULL;
	}
}  /* mem_disambig_term */

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
static int is_hidden_cast(ir_mode *mode, ir_mode *ent_mode) {
	if (ent_mode == NULL)
		return false;

	if (ent_mode != mode) {
		if (ent_mode == NULL ||
			get_mode_size_bits(ent_mode) != get_mode_size_bits(mode) ||
			get_mode_sort(ent_mode) != get_mode_sort(mode) ||
			get_mode_arithmetic(ent_mode) != irma_twos_complement ||
			get_mode_arithmetic(mode) != irma_twos_complement)
			return true;
	}
	return false;
}  /* is_hidden_cast */

/**
 * Determine the usage state of a node (or it's successor Sels).
 *
 * @param irn  the node
 */
static ir_entity_usage determine_entity_usage(const ir_node *irn, ir_entity *entity) {
	int       i;
	ir_mode   *emode, *mode;
	ir_node   *value;
	ir_type   *tp;
	ir_entity_usage res = 0;

	for (i = get_irn_n_outs(irn) - 1; i >= 0; --i) {
		ir_node *succ = get_irn_out(irn, i);

		switch (get_irn_opcode(succ)) {
		case iro_Load:
			assert(irn == get_Load_ptr(succ));
			res |= ir_usage_read;

			/* check if this load is not a hidden conversion */
			mode  = get_Load_mode(succ);
			emode = get_type_mode(get_entity_type(entity));
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
				value = get_Store_value(succ);
				mode  = get_irn_mode(value);
				emode = get_type_mode(get_entity_type(entity));
				if (is_hidden_cast(mode, emode))
					res |= ir_usage_reinterpret_cast;
			}
			assert(irn != get_Store_mem(succ));
			break;

		case iro_CopyB:
			/* CopyB are like Loads/Stores */
			tp  = get_entity_type(entity);
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

		case iro_Add:
		case iro_Sub:
		case iro_Sel: {
			/* Check the successor of irn. */
			res |= determine_entity_usage(succ, entity);
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

		default:
			/* another op, we don't know anything */
			res |= ir_usage_unknown;
			break;
		}
	}

	return res;
}

/**
 * Update the usage flags of all frame entities.
 */
static void analyse_irg_entity_usage(ir_graph *irg) {
	ir_type *ft = get_irg_frame_type(irg);
	ir_node *irg_frame;
	int i;

	/* set initial state to not_taken, as this is the "smallest" state */
	for (i = get_class_n_members(ft) - 1; i >= 0; --i) {
		ir_entity *ent = get_class_member(ft, i);

		set_entity_usage(ent, 0);
	}

	assure_irg_outs(irg);

	irg_frame = get_irg_frame(irg);

	for (i = get_irn_n_outs(irg_frame) - 1; i >= 0; --i) {
		ir_node        *succ = get_irn_out(irg_frame, i);
		ir_entity      *entity;
		ir_entity_usage flags;

	    if (!is_Sel(succ))
			continue;

		entity = get_Sel_entity(succ);
		flags  = get_entity_usage(entity);
		flags |= determine_entity_usage(succ, entity);
		set_entity_usage(entity, flags);
	}

	/* now computed */
	irg->entity_usage_state = ir_entity_usage_computed;
}

ir_entity_usage_computed_state get_irg_entity_usage_state(const ir_graph *irg) {
	return irg->entity_usage_state;
}

void set_irg_entity_usage_state(ir_graph *irg, ir_entity_usage_computed_state state) {
	irg->entity_usage_state = state;
}

void assure_irg_entity_usage_computed(ir_graph *irg) {
	if (irg->entity_usage_state != ir_entity_usage_not_computed)
		return;

	analyse_irg_entity_usage(irg);
}


/**
 * Initialize the entity_usage flag for a global type like type.
 */
static void init_entity_usage(ir_type * tp) {
	int i;

	/* We have to be conservative: All external visible entities are unknown */
	for (i = get_compound_n_members(tp) - 1; i >= 0; --i) {
		ir_entity       *entity = get_compound_member(tp, i);
		ir_entity_usage  flags;

		flags = get_entity_visibility(entity) == visibility_external_visible ?
				ir_usage_unknown : 0;
		set_entity_usage(entity, flags);
	}
}

static void check_initializer_nodes(ir_initializer_t *initializer)
{
	switch (initializer->kind) {
	case IR_INITIALIZER_CONST: {
		ir_node *n = initializer->consti.value;

		/* let's check if it's an address */
		if (is_Global(n)) {
			ir_entity *ent = get_Global_entity(n);
			set_entity_usage(ent, ir_usage_unknown);
		}
		return;
	}
	case IR_INITIALIZER_TARVAL:
	case IR_INITIALIZER_NULL:
		return;
	case IR_INITIALIZER_COMPOUND: {
		size_t i;

		for (i = 0; i < initializer->compound.n_initializers; ++i) {
			ir_initializer_t *sub_initializer
				= initializer->compound.initializers[i];
			check_initializer_nodes(sub_initializer);
		}
		return;
	}
	}
	panic("invalid initializer found");
}  /* check_initializer_nodes */

/**
 * Mark all entities used in the initializer for the given entity as address taken.
 *
 * @param ent  the entity
 */
static void check_initializer(ir_entity *ent) {
	ir_node *n;
	int i;

	/* do not check uninitialized values */
	if (get_entity_variability(ent) == variability_uninitialized)
		return;

	/* Beware: Methods are always initialized with "themself". This does not
	   count as a taken address. */
	if (is_Method_type(get_entity_type(ent)))
		return;

	if (ent->has_initializer) {
		check_initializer_nodes(ent->attr.initializer);
	} else if (is_atomic_entity(ent)) {
		/* let's check if it's an address */
		n = get_atomic_ent_value(ent);
		if (is_Global(n)) {
			ir_entity *ent = get_Global_entity(n);
			set_entity_usage(ent, ir_usage_unknown);
		}
	} else {
		for (i = get_compound_ent_n_values(ent) - 1; i >= 0; --i) {
			n = get_compound_ent_value(ent, i);

			/* let's check if it's an address */
			if (is_Global(n)) {
				ir_entity *ent = get_Global_entity(n);
				set_entity_usage(ent, ir_usage_unknown);
			}
		}
	}
}  /* check_initializer */


/**
 * Mark all entities used in initializers as address taken.
 *
 * @param tp  a compound type
 */
static void check_initializers(ir_type *tp) {
	int i;

	for (i = get_compound_n_members(tp) - 1; i >= 0; --i) {
		ir_entity *ent = get_compound_member(tp, i);

		check_initializer(ent);
	}
}  /* check_initializers */

#ifdef DEBUG_libfirm
/**
 * Print the entity usage flags of all entities of a given type for debugging.
 *
 * @param tp  a compound type
 */
static void print_entity_usage_flags(ir_type *tp) {
	int i;
	for (i = get_compound_n_members(tp) - 1; i >= 0; --i) {
		ir_entity *ent = get_compound_member(tp, i);
		ir_entity_usage flags = get_entity_usage(ent);

		if (flags == 0)
			continue;
		ir_printf("%+F:");
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
static void check_global_address(ir_node *irn, void *env) {
	ir_node *tls = env;
	ir_entity *ent;
	ir_entity_usage flags;

	if (is_Global(irn)) {
		/* A global. */
		ent = get_Global_entity(irn);
	} else if (is_Sel(irn) && get_Sel_ptr(irn) == tls) {
		/* A TLS variable. */
		ent = get_Sel_entity(irn);
	} else
		return;

	flags = get_entity_usage(ent);
	flags |= determine_entity_usage(irn, ent);
	set_entity_usage(ent, flags);
}  /* check_global_address */

/**
 * Update the entity usage flags of all global entities.
 */
static void analyse_irp_globals_entity_usage(void) {
	int i;
	ir_segment_t s;

	for (s = IR_SEGMENT_FIRST; s < IR_SEGMENT_COUNT; ++s) {
		ir_type *type = get_segment_type(s);
		init_entity_usage(type);
	}

	for (s = IR_SEGMENT_FIRST; s < IR_SEGMENT_COUNT; ++s) {
		ir_type *type = get_segment_type(s);
		check_initializers(type);
	}

	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg = get_irp_irg(i);

		assure_irg_outs(irg);
		irg_walk_graph(irg, NULL, check_global_address, get_irg_tls(irg));
	}

#ifdef DEBUG_libfirm
	if (firm_dbg_get_mask(dbg) & LEVEL_1) {
		ir_segment_t s;
		for (s = IR_SEGMENT_FIRST; s < IR_SEGMENT_COUNT; ++s) {
			print_entity_usage_flags(get_segment_type(s));
		}
	}
#endif /* DEBUG_libfirm */

	/* now computed */
	irp->globals_entity_usage_state = ir_entity_usage_computed;
}

/* Returns the current address taken state of the globals. */
ir_entity_usage_computed_state get_irp_globals_entity_usage_state(void) {
	return irp->globals_entity_usage_state;
}

/* Sets the current address taken state of the graph. */
void set_irp_globals_entity_usage_state(ir_entity_usage_computed_state state) {
	irp->globals_entity_usage_state = state;
}

/* Assure that the address taken flag is computed for the globals. */
void assure_irp_globals_entity_usage_computed(void) {
	if (irp->globals_entity_usage_state != ir_entity_usage_not_computed)
		return;

	analyse_irp_globals_entity_usage();
}

void firm_init_memory_disambiguator(void) {
	FIRM_DBG_REGISTER(dbg, "firm.ana.irmemory");
}


#include <adt/pmap.h>
#include "typerep.h"

DEBUG_ONLY(static firm_dbg_module_t *dbgcall = NULL;)

/** Maps method types to cloned method types. */
static pmap *mtp_map;

/**
 * Clone a method type if not already cloned.
 *
 * @param tp  the type to clone
 */
static ir_type *clone_type_and_cache(ir_type *tp) {
	static ident *prefix = NULL;
	ir_type *res;
	pmap_entry *e = pmap_find(mtp_map, tp);

	if (e)
		return e->value;

	if (prefix == NULL)
		prefix = new_id_from_chars("C", 1);

	res = clone_type_method(tp, prefix);
	pmap_insert(mtp_map, tp, res);
	DB((dbgcall, LEVEL_2, "cloned type %+F into %+F\n", tp, res));

	return res;
}  /* clone_type_and_cache */

/**
 * Walker: clone all call types of Calls to methods having the
 * mtp_property_private property set.
 */
static void update_calls_to_private(ir_node *call, void *env) {
	(void) env;
	if (is_Call(call)) {
		ir_node *ptr = get_Call_ptr(call);

		if (is_SymConst(ptr)) {
			ir_entity *ent = get_SymConst_entity(ptr);
			ir_type *ctp = get_Call_type(call);

			if (get_entity_additional_properties(ent) & mtp_property_private) {
				if ((get_method_additional_properties(ctp) & mtp_property_private) == 0) {
					ctp = clone_type_and_cache(ctp);
					set_method_additional_property(ctp, mtp_property_private);
					set_Call_type(call, ctp);
					DB((dbgcall, LEVEL_1, "changed call to private method %+F\n", ent));
				}
			}
		}
	}
}  /* update_calls_to_private */

/* Mark all private methods, i.e. those of which all call sites are known. */
void mark_private_methods(void) {
	int i;
	int changed = 0;

	FIRM_DBG_REGISTER(dbgcall, "firm.opt.cc");

	assure_irp_globals_entity_usage_computed();

	mtp_map = pmap_create();

	/* first step: change the calling conventions of the local non-escaped entities */
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph        *irg   = get_irp_irg(i);
		ir_entity       *ent   = get_irg_entity(irg);
		ir_entity_usage  flags = get_entity_usage(ent);

		/* If an entity is sticky, it might be called from external
		   places (like inline assembler), so do NOT mark it as private. */
		if (get_entity_visibility(ent) == visibility_local &&
		    !(flags & ir_usage_address_taken) &&
		    get_entity_stickyness(ent) != stickyness_sticky) {
			ir_type *mtp = get_entity_type(ent);

			set_entity_additional_property(ent, mtp_property_private);
			DB((dbgcall, LEVEL_1, "found private method %+F\n", ent));
			if ((get_method_additional_properties(mtp) & mtp_property_private) == 0) {
				/* need a new type */
				mtp = clone_type_and_cache(mtp);
				set_entity_type(ent, mtp);
				set_method_additional_property(mtp, mtp_property_private);
				changed = 1;
			}
		}
	}

	if (changed)
		all_irg_walk(NULL, update_calls_to_private, NULL);

	pmap_destroy(mtp_map);
}  /* mark_private_methods */
