/*
 * Project:     libFIRM
 * File name:   ir/ana/irmemory.c
 * Purpose:     Memory disambiguator
 * Author:      Michael Beck
 * Modified by:
 * Created:     27.12.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 2006-2007 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
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

/** The source language specific language disambiguator function. */
static DISAMBIGUATOR_FUNC language_disambuigator = NULL;

/**
 * Find the base address of an Sel node.
 */
static ir_node *find_base_adr(ir_node *sel) {
	ir_node *ptr = get_Sel_ptr(sel);

	while (is_Sel(ptr)) {
		sel = ptr;
		ptr = get_Sel_ptr(sel);
	}
	return ptr;
}  /* find_base_adr */

/**
 * Two address expressions have the same base address,
 * check if there offsets are different.
 *
 * @param adr1  The first address.
 * @param adr2  The second address.
 */
static ir_alias_relation different_offsets(ir_node *adr1, ir_node *adr2) {
	return may_alias;
}  /* different_offsets */

/**
 * Determine the alias relation by checking if adr1 and adr2 are pointer
 * to different type.
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
 * Determine the alias relation between two addresses.
 */
static ir_alias_relation _get_alias_relation(
	ir_graph *irg,
	ir_node *adr1, ir_mode *mode1,
	ir_node *adr2, ir_mode *mode2,
	unsigned options)
{
	opcode op1, op2;
	ir_entity *ent1;

	if (! get_opt_alias_analysis())
		return may_alias;

	if (adr1 == adr2)
		return sure_alias;

	/* Two save some code, sort the addresses by its id's. Beware, this
	   might break some things, so better check here. */
	assert(iro_SymConst < iro_Sel && "Code dependence breaked");
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

	if (is_SymConst(adr1) && get_SymConst_kind(adr1) == symconst_addr_ent) {
		/* first address is a global variable */

		if (is_SymConst(adr2) && get_SymConst_kind(adr2) == symconst_addr_ent) {
			/* both addresses are global variables and we know
			   they are different (R1 a) */
			if (get_SymConst_entity(adr1) != get_SymConst_entity(adr2))
				return no_alias;
			else
				return sure_alias;
		}
		if (is_Sel(adr2)) {
			ir_node *base = find_base_adr(adr2);

			if (is_SymConst(base) && get_SymConst_kind(base) == symconst_addr_ent) {
				/* base address is a global var (R1 a) */
				if (adr1 != base)
					return no_alias;
				if (base == adr1)
					return different_offsets(adr1, adr2);
			} else if (base == get_irg_frame(irg)) {
				/* the second one is a local variable so they are always
				   different (R1 b) */
				return no_alias;
			} else if (base == get_irg_tls(irg)) {
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
		ir_node *base1 = find_base_adr(adr1);

		if (base1 == get_irg_frame(irg)) {
			/* the first is a local variable */
			if (is_Sel(adr2)) {
				/* the second address is a Sel */
				ir_node *base2 = find_base_adr(adr2);

				if (base2 == get_irg_frame(irg)) {
					/* the second one is a local variable */
				} else if (base2 == get_irg_tls(irg)) {
					/* the second one is a TLS variable so they are always
				       different (R1 d) */
					return no_alias;
				}
			}
		} else if (base1 == get_irg_tls(irg)) {
			/* the first is a TLS variable */
			if (is_Sel(adr2)) {
				/* the second address is a Sel */
				ir_node *base2 = find_base_adr(adr2);

				if (base2 == get_irg_frame(irg)) {
					/* the second one is a local variable so they are always
				       different (R1 d) */
					return no_alias;
				} else if (base2 == get_irg_tls(irg)) {
					/* the second one is a TLS variable */
				}
			}
		}
	}

	if (options & opt_strong_typed) {
		/* try rule R5 */
		ir_alias_relation rel = different_types(adr1, adr2);
		if (rel != may_alias)
			return rel;
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
	ir_node *adr2, ir_mode *mode2,
	unsigned options)
{
	ir_alias_relation rel = _get_alias_relation(irg, adr1, mode1, adr2, mode2, options);
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
	ir_node *adr2, ir_mode *mode2,
	unsigned options)
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

	key.result = get_alias_relation(irg, adr1, mode1, adr2, mode2, options);

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
	int     i;
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
			return ir_address_taken;

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
		entity *ent = get_class_member(ft, i);

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
		entity *ent = get_compound_member(tp, i);
		ir_address_taken_state state;

		state = get_entity_visibility(ent) == visibility_external_visible ?
				ir_address_taken_unknown : ir_address_not_taken ;
		set_entity_address_taken(ent, state);
	}
}  /* init_taken_flag */

/**
 * Print the address taken state of all entities of a given type for debugging.
 */
static void print_address_taken_state(ir_type *tp) {
	int i;
	for (i = get_compound_n_members(tp) - 1; i >= 0; --i) {
		entity *ent = get_compound_member(tp, i);
		ir_address_taken_state state = get_entity_address_taken(ent);

		if (state != ir_address_not_taken) {
			assert(ir_address_not_taken <= state && state <= ir_address_taken);
			ir_printf("%+F: %s\n", ent, get_address_taken_state_name(state));
		}
	}
}  /* print_address_taken_state */

/**
 * Post-walker: check for global entity address
 */
static void check_global_address(ir_node *irn, void *env) {
	ir_node *tls = env;
	entity *ent;
	ir_address_taken_state state;

	if (is_SymConst(irn) && get_SymConst_kind(irn) == symconst_addr_ent) {
		/* A global. */
		ent = get_SymConst_entity(irn);
	} else if (is_Sel(irn) && get_Sel_ptr(irn) == tls) {
		/* A TLS variable. */
		ent = get_SymConst_entity(irn);
	} else
		return;

	if (get_entity_address_taken(ent) == ir_address_not_taken) {
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

	init_taken_flag(get_glob_type());
	init_taken_flag(get_tls_type());

	for (i = get_irp_n_irgs() - 1; i >= 0; --i) {
		ir_graph *irg = get_irp_irg(i);

		assure_irg_outs(irg);
		irg_walk_graph(irg, NULL, check_global_address, get_irg_tls(irg));
	}
	print_address_taken_state(get_glob_type());
	print_address_taken_state(get_tls_type());

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
