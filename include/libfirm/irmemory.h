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
#ifndef FIRM_ANA_IRMEMORY_H
#define FIRM_ANA_IRMEMORY_H

#include "firm_types.h"

#include "begin.h"

/** @ingroup irana
 * @defgroup ir_memory  Memory Disambiguator
 *
 * A memory disambiguator checks whether 2 given SSA values representing
 * addresses alias.
 *
 * @{
 */

/** The alias relation of two memory addresses. */
typedef enum ir_alias_relation {
	ir_no_alias,       /**< No alias. */
	ir_may_alias,      /**< Unknown state, may alias. */
	ir_sure_alias      /**< Sure alias. */
} ir_alias_relation;

/** The state of the entity usage flags. */
typedef enum ir_entity_usage_computed_state {
	ir_entity_usage_not_computed,
	ir_entity_usage_computed
} ir_entity_usage_computed_state;

/** Possible options for the memory disambiguator. */
typedef enum ir_disambiguator_options {
	aa_opt_none                = 0,       /**< no options, use defaults */
	aa_opt_always_alias        = 1u << 0, /**< always assume aliasing */
	/**< use type based alias analysis: strictly typed source language */
	aa_opt_type_based          = 1u << 1,
	/**< if type based analysis is enabled bytes types may alias other types */
	aa_opt_byte_type_may_alias = 1u << 2,
	aa_opt_no_alias            = 1u << 3, /**< different addresses NEVER alias */
	/**< internal flag: options from a graph are inherited from global */
	aa_opt_inherited           = 1u << 4,
} ir_disambiguator_options;
ENUM_BITSET(ir_disambiguator_options)

/**
 * Returns a human readable name for an alias relation.
 */
FIRM_API const char *get_ir_alias_relation_name(ir_alias_relation rel);

/**
 * Determine if two memory addresses may point to the same memory location.
 * This is determined by looking at the structure of the values or language
 * rules determined by looking at the object types accessed.
 *
 * @param addr1   The first address.
 * @param type1   The type of the object found at @p addr1 ("object type").
 * @param size1   The size in bytes of the first memory access.
 * @param addr2   The second address.
 * @param type2   The type of the object found at @p addr2 ("object type").
 * @param size2   The size in bytes of the second memory access.
 */
FIRM_API ir_alias_relation get_alias_relation(
	const ir_node *addr1, const ir_type *type1, unsigned size1,
	const ir_node *addr2, const ir_type *type2, unsigned size2);

/**
 * Assure that the entity usage flags have been computed for the given graph.
 *
 * This analysis computes the entity usage state for all local variables.
 *
 * Even then the information is not cleaned from the variables, call
 * assure_irg_entity_usage_computed() again for recomputation.
 */
FIRM_API void assure_irg_entity_usage_computed(ir_graph *irg);

/**
 * Returns the current address taken state of the globals.
 */
FIRM_API ir_entity_usage_computed_state get_irp_globals_entity_usage_state(void);

/**
 * Sets the current address taken state of the globals.
 *
 * @param state  the new state
 */
FIRM_API void set_irp_globals_entity_usage_state(ir_entity_usage_computed_state state);

/**
 * Assure that the address taken flag is computed for the global and TLS entities (variables).
 *
 * This is an interprocedural analysis that computes the address_taken state
 * for all global and TLS variables.
 *
 * Note that this is a conservative estimation that by no Firm transformation
 * can be invalidated, so it's only recomputed if manually triggered by calling
 * set_irp_globals_entity_usage_state(ir_entity_usage_not_computed).
 * Even then the information is not cleaned from the variables, call
 * assure_irp_globals_entity_usage_computed() again for recomputation.
 */
FIRM_API void assure_irp_globals_entity_usage_computed(void);

/**
 * Returns the memory disambiguator options for a graph.
 *
 * @param irg  the graph
 */
FIRM_API ir_disambiguator_options get_irg_memory_disambiguator_options(const ir_graph *irg);

/**
 * Sets the memory disambiguator options for a graph.
 *
 * @param irg      the graph
 * @param options  a set of options
 */
FIRM_API void set_irg_memory_disambiguator_options(ir_graph *irg,
                                                   ir_disambiguator_options options);

/**
 * Sets the global disambiguator options for all graphs not having local
 * options.
 *
 * @param options  a set of options
 */
FIRM_API void set_irp_memory_disambiguator_options(ir_disambiguator_options options);

/**
 * Mark all private methods, i.e. those of which all call sites are known.
 * We use a very conservative estimation yet: If the address of a method is
 * never taken AND its visibility is visibility_local, then it's private.
 */
FIRM_API void mark_private_methods(void);

/** @} */

#include "end.h"

#endif
