/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Entry point to the representation of a whole program 0-- private header.
 * @author   Goetz Lindenmaier
 * @date     2000
 */
#ifndef FIRM_IR_IRPROG_T_H
#define FIRM_IR_IRPROG_T_H

#include "irprog.h"

#include "array.h"
#include "callgraph.h"
#include "irmemory.h"
#include "pmap.h"
#include "typerep.h"

/* Inline functions. */
#define get_irp_n_irgs()                      get_irp_n_irgs_()
#define get_irp_irg(pos)                      get_irp_irg_(pos)
#define get_irp_n_types()                     get_irp_n_types_()
#define get_irp_type(pos)                     get_irp_type_(pos)
#define get_const_code_irg()                  get_const_code_irg_()
#define get_segment_type(s)                   get_segment_type_(s)
#define get_glob_type()                       get_glob_type_()
#define get_tls_type()                        get_tls_type_()
#define get_irp_next_label_nr()               get_irp_next_label_nr_()
#define irp_reserve_resources(irp, resources) irp_reserve_resources_(irp, resources)
#define irp_free_resources(irp, resources)    irp_free_resources_(irp, resources)
#define irp_resources_reserved(irp)           irp_resources_reserved_(irp)

/**
 * Data structure that holds central information about a program / module /
 * translation unit.
 */
struct ir_prog {
	ident     *name;                /**< A file name or the like. */
	ir_graph  *main_irg;            /**< The entry point to the compiled program
	                                     or NULL if no point exists. */
	ir_graph **graphs;              /**< A list of all graphs in the ir. */
	pmap      *globals;             /**< Map identifiers to global entities. */
	/** This graph holds nodes for global entity initialization expressions.
	 * It is not a function. */
	ir_graph  *const_code_irg;
	ir_entity *unknown_entity;      /**< unique 'unknown'-entity */
	ir_type   *segment_types[IR_SEGMENT_LAST+1];
	ir_type  **types;               /**< A list of all types in the ir. */
	ir_type   *code_type;           /**< unique 'code'-type */
	ir_type   *unknown_type;        /**< unique 'unknown'-type */
	ir_type   *dummy_owner;         /**< owner for internal entities */
	ir_type   *byte_type;           /**< type for a 'byte' */
	ident    **global_asms;         /**< An array of global ASM insertions. */

	/** Validity of callee information. Lowest value for all irgs. */
	irg_callee_info_state          callee_info_state;
	/** State of transitive closure of inheritance relations. */
	inh_transitive_closure_state   inh_trans_closure_state;
	irp_callgraph_state            callgraph_state; /**< State of callgraph. */
	/** Outermost callgraph loop */
	ir_loop                       *outermost_cg_loop;
	/** State of loop nesting depth information. */
	loop_nesting_depth_state       lnd_state;
	ir_entity_usage_computed_state globals_entity_usage_state;

	ir_label_t last_label_nr;        /**< Highest number for unique labels. */
	size_t     max_irg_idx;          /**< highest unused irg index */
	long       max_node_nr;          /**< Highest number unique node numbers. */
	unsigned   dump_nr;              /**< number of program info dumps */
#ifndef NDEBUG
	/** Bitset for tracking used global resources. */
	irp_resources_t reserved_resources;
#endif
};

static inline ir_type *get_segment_type_(ir_segment_t segment)
{
	assert(segment <= IR_SEGMENT_LAST);
	return irp->segment_types[segment];
}

static inline ir_type *get_glob_type_(void)
{
	return get_segment_type_(IR_SEGMENT_GLOBAL);
}

static inline ir_type *get_tls_type_(void)
{
	return get_segment_type_(IR_SEGMENT_THREAD_LOCAL);
}

static inline size_t get_irp_n_irgs_(void)
{
	return ARR_LEN(irp->graphs);
}

static inline ir_graph *get_irp_irg_(size_t pos)
{
	assert(pos < ARR_LEN(irp->graphs));
	return irp->graphs[pos];
}

static inline size_t get_irp_n_types_(void)
{
	return ARR_LEN(irp->types);
}

static inline ir_type *get_irp_type_(size_t pos)
{
	assert(pos < ARR_LEN(irp->types));
	/* Don't set the skip_tid result so that no double entries are generated. */
	return irp->types[pos];
}

/** Returns a new, unique number to number nodes or the like. */
static inline long get_irp_new_node_nr(void)
{
	return irp->max_node_nr++;
}

static inline size_t get_irp_new_irg_idx(void)
{
	return irp->max_irg_idx++;
}

static inline ir_graph *get_const_code_irg_(void)
{
	return irp->const_code_irg;
}

/** Returns a new, unique label number. */
static inline ir_label_t get_irp_next_label_nr_(void)
{
	return ++irp->last_label_nr;
}

#ifndef NDEBUG
static inline void irp_reserve_resources(ir_prog *irp,
                                         irp_resources_t resources)
{
	assert((irp->reserved_resources & resources) == 0);
	irp->reserved_resources |= resources;
}

static inline void irp_free_resources(ir_prog *irp, irp_resources_t resources)
{
	assert((irp->reserved_resources & resources) == resources);
	irp->reserved_resources &= ~resources;
}

static inline irp_resources_t irp_resources_reserved(const ir_prog *irp)
{
	return irp->reserved_resources;
}
#else
static inline void irp_reserve_resources(ir_prog *irp,
                                         irp_resources_t resources)
{
	(void)irp;
	(void)resources;
}

static inline void irp_free_resources(ir_prog *irp, irp_resources_t resources)
{
	(void)irp;
	(void)resources;
}

static inline irp_resources_t irp_resources_reserved(const ir_prog *irp)
{
	(void)irp;
	return IRP_RESOURCE_NONE;
}
#endif

/** initializes ir_prog. Constructs only the basic lists */
void init_irprog_1(void);

/** Completes ir_prog. */
void init_irprog_2(void);

/** Adds type to the list of types in irp. */
void add_irp_type(ir_type *typ);

/** Removes type from the list of types, deallocates it and
    shrinks the list by one. */
void remove_irp_type(ir_type *typ);

/** Adds irg to the list of ir graphs in the current irp. */
FIRM_API void add_irp_irg(ir_graph *irg);

/** Removes irg from the list of irgs and
    shrinks the list by one. */
FIRM_API void remove_irp_irg(ir_graph *irg);

#define foreach_irp_irg(idx, irg) \
	for (bool irg##__b = true; irg##__b; irg##__b = false) \
		for (size_t idx = 0, irg##__n = get_irp_n_irgs(); irg##__b && idx != irg##__n; ++idx) \
			for (ir_graph *const irg = (irg##__b = false, get_irp_irg(idx)); !irg##__b; irg##__b = true)

#define foreach_irp_irg_r(idx, irg) \
	for (bool irg##__b = true; irg##__b;) \
		for (; irg##__b; irg##__b = false) \
			for (size_t idx = get_irp_n_irgs(); irg##__b && idx-- != 0;) \
				for (ir_graph *const irg = (irg##__b = false, get_irp_irg(idx)); !irg##__b; irg##__b = true)

#endif
