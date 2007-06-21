/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irprog_t.h"
#include "irmemory.h"
#include "irflag.h"
#include "hashptr.h"
#include "irflag.h"
#include "irouts.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "debug.h"

/** The debug handle. */
DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

/** The source language specific language disambiguator function. */
static DISAMBIGUATOR_FUNC language_disambuigator = NULL;

/** The global memory disambiguator options. */
static unsigned global_mem_disamgig_opt = aa_opt_no_opt;

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
 * Check if the address can be decomposed into base PLUS offset.
 */
static int has_offset(ir_node *adr, int *offset) {
	if (is_SymConst(adr)) {
		*offset = 0;
		return 1;
	}
	if (is_Sel(adr)) {
		ir_entity *ent = get_Sel_entity(adr);
		ir_type   *owner = get_entity_owner(ent);

		if (get_type_state(owner) != layout_fixed) {
			/* The layout is NOT fixed yet, symbolic evaluation needed */
		}
	}
	return 0;
}  /* has_offset */

/**
 * Two address expressions have the same base address,
 * check if there offsets are different.
 *
 * @param adr1  The first address.
 * @param adr2  The second address.
 */
static ir_alias_relation different_offsets(ir_node *adr1, ir_node *adr2) {
	int offset1, offset2;
	if (has_offset(adr1, &offset1) && has_offset(adr2, &offset2)) {
		/* */
	}
	return may_alias;
}  /* different_offsets */

/**
 * Check if a given Const node is greater or equal a given size.
 *
 * @return no_alias if the Const is greater, may_alias else
 */
static ir_alias_relation check_const(ir_node *cns, int size) {
	tarval *tv = get_Const_tarval(cns);
	tarval *tv_size;

	if (size == 0)
		return classify_tarval(tv) != TV_CLASSIFY_NULL ? no_alias : may_alias;
	tv_size = new_tarval_from_long(size, get_tarval_mode(tv));
	return tarval_cmp(tv_size, tv) & (pn_Cmp_Eq|pn_Cmp_Lt) ? no_alias : may_alias;
}  /* check_const */

/**
 * Treat idx1 and idx2 as integer indexes and check if they differ always more than size.
 *
 * @return sure_alias iff idx1 == idx2
 *         no_alias iff they ALWAYS differ more than size
 *         may_alias else
 */
static ir_alias_relation different_index(ir_node *idx1, ir_node *idx2, int size) {
	if (idx1 == idx2)
		return sure_alias;
	if (is_Const(idx1) && is_Const(idx2)) {
		/* both are const, we can compare them */
		tarval *tv1 = get_Const_tarval(idx1);
		tarval *tv2 = get_Const_tarval(idx2);
		tarval *tv, *tv_size;

		if (size == 0)
			return tv1 == tv2 ? sure_alias : no_alias;

		if (tarval_cmp(tv1, tv2) == pn_Cmp_Gt) {
			tarval *t = tv1;
			tv1 = tv2;
			tv2 = t;
		}
		/* tv1 is now the "smaller" one */
		tv      = tarval_sub(tv2, tv1);
		tv_size = new_tarval_from_long(size, get_tarval_mode(tv));
		return tarval_cmp(tv_size, tv) & (pn_Cmp_Eq|pn_Cmp_Lt) ? no_alias : may_alias;
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
			/* both are Adds, check if they are of x + c kind */
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
			/* both are Subs, check if they are of x - c kind */
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
	return may_alias;
}  /* different_index */

/**
 * Two Sel addresses have the same base address, check if there offsets are different.
 *
 * @param adr1  The first address.
 * @param adr2  The second address.
 */
static ir_alias_relation different_sel_offsets(ir_node *sel1, ir_node *sel2) {
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
	return may_alias;
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

	if (is_SymConst(adr1) && get_SymConst_kind(adr1) == symconst_addr_ent)
		ent1 = get_SymConst_entity(adr1);
	else if (is_Sel(adr1))
		ent1 = get_Sel_entity(adr1);

	if (is_SymConst(adr2) && get_SymConst_kind(adr2) == symconst_addr_ent)
		ent2 = get_SymConst_entity(adr2);
	else if (is_Sel(adr2))
		ent2 = get_Sel_entity(adr2);

	if (ent1 != NULL && ent2 != NULL) {
		ir_type *tp1 = get_entity_type(ent1);
		ir_type *tp2 = get_entity_type(ent2);

		if (tp1 != tp2) {
			if (is_Pointer_type(tp1) && is_Pointer_type(tp2)) {
				/* do deref until no pointer types are found */
				do {
					tp1 = get_pointer_points_to_type(tp1);
					tp2 = get_pointer_points_to_type(tp2);
				} while (is_Pointer_type(tp1) && is_Pointer_type(tp2));
			}

			if (get_type_tpop(tp1) != get_type_tpop(tp2)) {
				/* different type structure */
				return no_alias;
			}
			if (is_Class_type(tp1)) {
				/* check class hierarchy */
				if (! is_SubClass_of(tp1, tp2) &&
					! is_SubClass_of(tp2, tp1))
					return no_alias;
			} else {
				/* different types */
				return no_alias;
			}
		}
	}
	return may_alias;
}  /* different_types */

/**
 * Check if an offset is a constant and these constant is bigger or equal
 * than a given size.
 */
static int check_const_offset(ir_node *offset, int size) {
	ir_mode *mode = get_irn_mode(offset);

	/* ok, we found an offset, check for constant */
	if (is_Const(offset) && mode_is_int(mode)) {
		tarval *tv = new_tarval_from_long(size, mode);

		/* size <= offset ? */
		if (tarval_cmp(tv, get_Const_tarval(offset)) & (pn_Cmp_Eq|pn_Cmp_Lt))
			return 1;
	}
	return 0;
}  /* check_const_offset */

/**
 * Check if we can determine that the two pointers always have an offset bigger then size
 */
static ir_alias_relation _different_pointer(ir_node *adr1, ir_node *adr2, int size) {
	int found = 0;

	if (is_Add(adr1)) {
		/* first address is the result of a pointer addition */
		ir_node *l1 = get_Add_left(adr1);
		ir_node *r1  = get_Add_right(adr1);

		if (l1 == adr2) {
			found = check_const_offset(r1, size);
		} else if (r1 == adr2) {
			found = check_const_offset(l1, size);
		} else if (is_Add(adr2)) {
			/* second address is the result of a pointer addition */
			ir_node *l2 = get_Add_left(adr2);
			ir_node *r2 = get_Add_right(adr2);

			if (l1 == l2) {
				return _different_pointer(r1, r2, size);
			} else if (l1 == r2) {
				return _different_pointer(r1, l2, size);
			} else if (r1 == l2) {
				return _different_pointer(l1, r2, size);
			} else if (r1 == r2) {
				return _different_pointer(l1, l2, size);
			}
		}
	} else if (is_Add(adr2)) {
		/* second address is the result of a pointer addition */
		ir_node *l2 = get_Add_left(adr2);
		ir_node *r2  = get_Add_right(adr2);

		if (l2 == adr1) {
			found = check_const_offset(r2, size);
		} else if (r2 == adr1) {
			found = check_const_offset(l2, size);
		}
	} else {
		return different_index(adr1, adr2, size);
	}
	return found ? no_alias : may_alias;
}  /* _different_pointer */

/**
 * Check if we can determine that the two pointers always have an offset bigger then the maximum size of mode1, mode2
 */
static ir_alias_relation different_pointer(ir_node *adr1, ir_mode *mode1, ir_node *adr2, ir_mode *mode2) {
	int size = get_mode_size_bytes(mode1);
	int n    = get_mode_size_bytes(mode2);

	if (n > size)
		size = n;
	return _different_pointer(adr1, adr2, size);
}  /* different_pointer */

/**
 * Returns non-zero if a node is a routine parameter.
 *
 * @param node  the node to test
 */
static int is_arg_Proj(ir_node *node) {
	if (! is_Proj(node))
		return 0;
	node = get_Proj_pred(node);
	if (! is_Proj(node))
		return 0;
	return pn_Start_T_args == get_Proj_proj(node) && is_Start(get_Proj_pred(node));
}  /* is_arg_Proj */

/**
 * Returns true if an address represents a global variable.
 */
static INLINE int is_global_var(ir_node *irn) {
	return is_SymConst(irn) && get_SymConst_kind(irn) == symconst_addr_ent;
}  /* is_global_var */

/**
 * Determine the alias relation between two addresses.
 */
static ir_alias_relation _get_alias_relation(
	ir_graph *irg,
	ir_node *adr1, ir_mode *mode1,
	ir_node *adr2, ir_mode *mode2)
{
	ir_opcode op1, op2;
	ir_entity *ent1, *ent2;
	unsigned options;

	if (! get_opt_alias_analysis())
		return may_alias;

	if (adr1 == adr2)
		return sure_alias;

	options = get_irg_memory_disambiguator_options(irg);

	/* The Armageddon switch */
	if (options & aa_opt_no_alias)
		return no_alias;

	/* Two save some code, sort the addresses by its id's. Beware, this
	   might break some things, so better check here. */
	assert(iro_SymConst < iro_Sel && iro_Sel < iro_Proj && "Code dependence breaked");
	op1 = get_irn_opcode(adr1);
	op2 = get_irn_opcode(adr2);

	if (op1 > op2) {
		ir_node *t = adr1;
		ir_mode *m = mode1;
		adr1  = adr2;
		mode1 = mode2;
		adr2  = t;
		mode2 = m;
	}

	if (is_global_var(adr1)) {
		/* first address is a global variable */

		if (is_global_var(adr2)) {
			/* both addresses are global variables and we know
			   they are different (R1 a) */
			if (get_SymConst_entity(adr1) != get_SymConst_entity(adr2))
				return no_alias;
			else {
				/* equal entity addresses */
				return sure_alias;
			}
		}
		if (is_Sel(adr2)) {
			ir_node *base2 = find_base_adr(adr2, &ent2);

			if (is_global_var(base2)) {
				/* base2 address is a global var (R1 a) */
				if (adr1 != base2)
					return no_alias;
				else
					return different_offsets(adr1, adr2);
			} else if (base2 == get_irg_frame(irg)) {
				/* the second one is a local variable so they are always
				   different (R1 b) */
				return no_alias;
			} else if (base2 == get_irg_tls(irg)) {
				/* the second one is a TLS variable so they are always
				   different (R1 c) */
				return no_alias;
			}
		}

		/* Here we are: the first is a global var, the second some pointer. */
		ent1 = get_SymConst_entity(adr1);
		if (get_entity_address_taken(ent1) == ir_address_not_taken) {
			/* The address of the global variable was never taken, so
			   the pointer cannot match (R2). */
			return no_alias;
		}
	} else if (is_Sel(adr1)) {
		/* the first address is a Sel */
		ir_node *base1 = find_base_adr(adr1, &ent1);

		if (base1 == get_irg_frame(irg)) {
			/* first is a local variable ent1 */
			if (is_Sel(adr2)) {
				/* the second address is a Sel */
				ir_node *base2 = find_base_adr(adr2, &ent2);

				if (base1 == base2) {
					/* identical bases: check for different offsets */
					return different_sel_offsets(adr1, adr2);
				} else if (base2 == get_irg_frame(irg)) {
					/* both addresses are local variables and we know
					   they are different (R1 a) */
					if (ent1 != ent2)
						return no_alias;
				} else if (base2 == get_irg_tls(irg)) {
					/* the second one is a TLS variable so they are always
				       different (R1 d) */
					return no_alias;
				} else if (is_arg_Proj(base2)) {
					/* the second one is an offset from a parameter so they are
					   always different (R1 e) */
					return no_alias;
				}
			} else if (is_arg_Proj(adr2)) {
				/* a local variable and a parameter are always different (R1 e) */
				return no_alias;
			}
		} else if (base1 == get_irg_tls(irg)) {
			/* the first is a TLS variable */
			if (is_Sel(adr2)) {
				/* the second address is a Sel */
				ir_node *base2 = find_base_adr(adr2, &ent2);

				if (base1 == base2)
					return different_sel_offsets(adr1, adr2);
				else if (base2 == get_irg_frame(irg)) {
					/* the second one is a local variable so they are always
				       different (R1 d) */
					return no_alias;
				} else if (base2 == get_irg_tls(irg)) {
					/* both addresses are TLS variables and we know
					   they are different (R1 a) */
					if (ent1 != ent2)
						return no_alias;
				}
			}
		} else if (is_arg_Proj(base1)) {
			/* the first one is an offset from a parameter */
			if (is_Sel(adr2)) {
				/* the second address is a Sel */
				ir_node *base2 = find_base_adr(adr2, &ent2);

				if (base2 == get_irg_frame(irg)) {
					/* the second one is a local variable so they are always
				       different (R1 e) */
					return no_alias;
				}
			}
		} else if (is_global_var(base1)) {
			/* the first one is a global variable */
			ent1 = get_SymConst_entity(base1);
			if (is_Sel(adr2)) {
				/* the second address is a Sel */
				ir_node *base2 = find_base_adr(adr2, &ent2);

				if (base1 == base2)
					return different_sel_offsets(adr1, adr2);
				else if (base2 == get_irg_frame(irg)) {
					/* the second one is a local variable so they are always
				       different (R1 a) */
					return no_alias;
				} else if (base2 == get_irg_tls(irg)) {
					/* the second one is a TLS variable so they are always
				       different (R1 a) */
					return no_alias;
				} else if (is_arg_Proj(base2)) {
					if (get_entity_address_taken(ent1) == ir_address_not_taken) {
						/* The address of the global variable was never taken, so
						   the pointer cannot match (R2). */
						return no_alias;
					}
				} else if (is_global_var(base2)) {
					ent2 = get_SymConst_entity(base2);
					/* both addresses are global variables and we know
					   they are different (R1 a) */
					if (ent1 != ent2)
						return no_alias;
				}
			}
		}
	} else {
		/* some pointers, check if they have the same base buf constant offset */
		ir_alias_relation rel = different_pointer(adr1, mode1, adr2, mode2);
		if (rel != may_alias)
			return rel;
	}


	if (options & aa_opt_type_based) { /* Type based alias analysis */
		ir_alias_relation rel;

		if (options & aa_opt_byte_type_may_alias) {
			if (get_mode_size_bits(mode1) == 8 || get_mode_size_bits(mode2) == 8) {
				/* One of the modes address a byte. Assume a may_alias and leave
				   the type based check. */
				goto leave_type_based_alias;
			}
		}
		/* cheap check: If the mode sizes did not match, the types MUST be different */
		if (get_mode_size_bits(mode1) != get_mode_size_bits(mode2))
			return no_alias;

		/* try rule R5 */
		rel = different_types(adr1, adr2);
		if (rel != may_alias)
			return rel;
leave_type_based_alias:;
	}

	/* do we have a language specific memory disambiguator? */
	if (language_disambuigator) {
		ir_alias_relation rel = (*language_disambuigator)(irg, adr1, mode1, adr2, mode2);
		if (rel != may_alias)
			return rel;
	}

	/* access points-to information here */
	return may_alias;
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

	if (! get_opt_alias_analysis())
		return may_alias;

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
	if (ent_mode != mode) {
		if (ent_mode == NULL ||
			get_mode_size_bits(ent_mode) != get_mode_size_bits(mode) ||
			get_mode_sort(ent_mode) != get_mode_sort(mode) ||
			get_mode_arithmetic(ent_mode) != irma_twos_complement ||
			get_mode_arithmetic(mode) != irma_twos_complement)
			return 1;
	}
	return 0;
}  /* is_hidden_cast */

/**
 * Determine the address_taken state of a node (or it's successor Sels).
 *
 * @param irn  the node
 */
static ir_address_taken_state find_address_taken_state(ir_node *irn) {
	int     i, j;
	ir_mode *emode, *mode;
	ir_node *value;
	ir_entity *ent;

	for (i = get_irn_n_outs(irn) - 1; i >= 0; --i) {
		ir_node *succ = get_irn_out(irn, i);

		switch (get_irn_opcode(succ)) {
		case iro_Load:
			/* check if this load is not a hidden conversion */
			mode = get_Load_mode(succ);
			ent = is_SymConst(irn) ? get_SymConst_entity(irn) : get_Sel_entity(irn);
			emode = get_type_mode(get_entity_type(ent));
			if (is_hidden_cast(mode, emode))
				return ir_address_taken;
			break;

		case iro_Store:
			/* check that the node is not the Store's value */
			value = get_Store_value(succ);
			if (value == irn)
				return ir_address_taken;
			/* check if this Store is not a hidden conversion */
			mode = get_irn_mode(value);
			ent = is_SymConst(irn) ? get_SymConst_entity(irn) : get_Sel_entity(irn);
			emode = get_type_mode(get_entity_type(ent));
			if (is_hidden_cast(mode, emode))
				return ir_address_taken;
			break;

		case iro_Sel: {
			/* Check the successor of irn. */
			ir_address_taken_state res = find_address_taken_state(succ);
			if (res != ir_address_not_taken)
				return res;
			break;
		}

		case iro_Call:
			/* Only the call address is not an address taker but
			   this is an uninteresting case, so we ignore it here. */
			for (j = get_Call_n_params(succ) - 1; j >= 0; --j) {
				ir_node *param = get_Call_param(succ, j);
				if (param == irn)
					return ir_address_taken;
			}
			break;

		default:
			/* another op, the address may be taken */
			return ir_address_taken_unknown;
		}
	}
	/* All successors finished, the address is not taken. */
	return ir_address_not_taken;
}  /* find_address_taken_state */

/**
 * Update the "address taken" flag of all frame entities.
 */
static void analyse_irg_address_taken(ir_graph *irg) {
	ir_type *ft = get_irg_frame_type(irg);
	ir_node *irg_frame;
	int i;

	/* set initial state to not_taken, as this is the "smallest" state */
	for (i = get_class_n_members(ft) - 1; i >= 0; --i) {
		ir_entity *ent = get_class_member(ft, i);

		set_entity_address_taken(ent, ir_address_not_taken);
	}

	assure_irg_outs(irg);

	irg_frame = get_irg_frame(irg);

	for (i = get_irn_n_outs(irg_frame) - 1; i >= 0; --i) {
		ir_node *succ = get_irn_out(irg_frame, i);
		ir_address_taken_state state;

	    if (is_Sel(succ)) {
			ir_entity *ent = get_Sel_entity(succ);

			if (get_entity_address_taken(ent) == ir_address_taken)
				continue;

			state = find_address_taken_state(succ);
			if (state > get_entity_address_taken(ent))
				set_entity_address_taken(ent, state);
		}
	}
	/* now computed */
	irg->adr_taken_state = ir_address_taken_computed;
}  /* analyse_address_taken */

/* Returns the current address taken state of the graph. */
ir_address_taken_computed_state get_irg_address_taken_state(const ir_graph *irg) {
	return irg->adr_taken_state;
}  /* get_irg_address_taken_state */

/* Sets the current address taken state of the graph. */
void set_irg_address_taken_state(ir_graph *irg, ir_address_taken_computed_state state) {
	irg->adr_taken_state = state;
}  /* set_irg_address_taken_state */

/* Assure that the address taken flag is computed for the given graph. */
void assure_irg_address_taken_computed(ir_graph *irg) {
	if (irg->adr_taken_state == ir_address_taken_not_computed)
		analyse_irg_address_taken(irg);
}  /* assure_irg_address_taken_computed */

/**
 * Initialize the address_taken flag for a global type like type.
 */
static void init_taken_flag(ir_type * tp) {
	int i;

	/* All external visible entities are at least
	   ir_address_taken_unknown. This is very conservative. */
	for (i = get_compound_n_members(tp) - 1; i >= 0; --i) {
		ir_entity *ent = get_compound_member(tp, i);
		ir_address_taken_state state;

		state = get_entity_visibility(ent) == visibility_external_visible ?
				ir_address_taken_unknown : ir_address_not_taken ;
		set_entity_address_taken(ent, state);
	}
}  /* init_taken_flag */

#ifdef DEBUG_libfirm
/**
 * Print the address taken state of all entities of a given type for debugging.
 */
static void print_address_taken_state(ir_type *tp) {
	int i;
	for (i = get_compound_n_members(tp) - 1; i >= 0; --i) {
		ir_entity *ent = get_compound_member(tp, i);
		ir_address_taken_state state = get_entity_address_taken(ent);

		if (state != ir_address_not_taken) {
			assert(ir_address_not_taken <= (int) state && state <= ir_address_taken);
			ir_printf("%+F: %s\n", ent, get_address_taken_state_name(state));
		}
	}
}  /* print_address_taken_state */
#endif /* DEBUG_libfirm */

/**
 * Post-walker: check for global entity address
 */
static void check_global_address(ir_node *irn, void *env) {
	ir_node *tls = env;
	ir_entity *ent;
	ir_address_taken_state state;

	if (is_SymConst(irn) && get_SymConst_kind(irn) == symconst_addr_ent) {
		/* A global. */
		ent = get_SymConst_entity(irn);
	} else if (is_Sel(irn) && get_Sel_ptr(irn) == tls) {
		/* A TLS variable. */
		ent = get_Sel_entity(irn);
	} else
		return;

	if (get_entity_address_taken(ent) >= ir_address_taken) {
		/* Already at the maximum. */
		return;
	}
	state = find_address_taken_state(irn);
	if (state > get_entity_address_taken(ent))
		set_entity_address_taken(ent, state);
}  /* check_global_address */

/**
 * Update the "address taken" flag of all global entities.
 */
static void analyse_irp_globals_address_taken(void) {
	int i;

	FIRM_DBG_REGISTER(dbg, "firm.ana.irmemory");

	init_taken_flag(get_glob_type());
	init_taken_flag(get_tls_type());

	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg = get_irp_irg(i);

		assure_irg_outs(irg);
		irg_walk_graph(irg, NULL, check_global_address, get_irg_tls(irg));
	}

#ifdef DEBUG_libfirm
	if (firm_dbg_get_mask(dbg) & LEVEL_1) {
		print_address_taken_state(get_glob_type());
		print_address_taken_state(get_tls_type());
	}
#endif /* DEBUG_libfirm */

	/* now computed */
	irp->globals_adr_taken_state = ir_address_taken_computed;
}  /* analyse_irp_globals_address_taken */

/* Returns the current address taken state of the globals. */
ir_address_taken_computed_state get_irp_globals_address_taken_state(void) {
	return irp->globals_adr_taken_state;
}  /* get_irp_globals_address_taken_state */

/* Sets the current address taken state of the graph. */
void set_irp_globals_address_taken_state(ir_address_taken_computed_state state) {
	irp->globals_adr_taken_state = state;
}  /* set_irg_address_taken_state */

/* Assure that the address taken flag is computed for the globals. */
void assure_irp_globals_address_taken_computed(void) {
	if (irp->globals_adr_taken_state == ir_address_taken_not_computed)
		analyse_irp_globals_address_taken();
}  /* assure_irp_globals_address_taken_computed */


DEBUG_ONLY(static firm_dbg_module_t *dbgcall = NULL;)

/**
 * Copy the calling conventions from the entities to the call type.
 */
static void update_calls(ir_node *call, void *env) {
	(void) env;
	if (is_Call(call)) {
		ir_node *ptr = get_Call_ptr(call);

		if (is_SymConst(ptr)) {
			ir_entity *ent = get_SymConst_entity(ptr);
			ir_type *ctp = get_Call_type(call);

			if ((get_method_additional_properties(ctp) & mtp_property_private) == 0) {
				set_method_additional_property(ctp, mtp_property_private);
				DB((dbgcall, LEVEL_1, "changed call to private method %+F\n", ent));
			}
		}
	}
}

/* Mark all private methods, i.e. those of which all call sites are known. */
void mark_private_methods(void) {
	int i;
	int changed = 0;

	FIRM_DBG_REGISTER(dbgcall, "firm.opt.cc");

	assure_irp_globals_address_taken_computed();

	/* first step: change the calling conventions of the local non-escaped entities */
	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph               *irg = get_irp_irg(i);
		ir_entity              *ent = get_irg_entity(irg);
		ir_address_taken_state state = get_entity_address_taken(ent);

		if (get_entity_visibility(ent) == visibility_local &&
		    state == ir_address_not_taken) {
			ir_type *mtp = get_entity_type(ent);

			set_method_additional_property(mtp, mtp_property_private);
			changed = 1;
			DB((dbgcall, LEVEL_1, "found private method %+F\n", ent));
		}
	}

	if (changed)
		all_irg_walk(NULL, update_calls, NULL);
}
