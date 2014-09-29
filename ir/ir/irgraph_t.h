/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Entry point to the representation of procedure code -- internal header.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 */
#ifndef FIRM_IR_IRGRAPH_T_H
#define FIRM_IR_IRGRAPH_T_H

#include "firm_types.h"
#include "irgraph.h"

#include "irtypes.h"
#include "irprog.h"
#include "type_t.h"
#include "entity_t.h"
#include "iredgekinds.h"
#include "irloop.h"
#include "obst.h"

#define get_irg_start_block(irg)              get_irg_start_block_(irg)
#define set_irg_start_block(irg, node)        set_irg_start_block_(irg, node)
#define get_irg_start(irg)                    get_irg_start_(irg)
#define set_irg_start(irg, node)              set_irg_start_(irg, node)
#define get_irg_end_block(irg)                get_irg_end_block_(irg)
#define set_irg_end_block(irg, node)          set_irg_end_block_(irg, node)
#define get_irg_end(irg)                      get_irg_end_(irg)
#define set_irg_end(irg, node)                set_irg_end_(irg, node)
#define get_irg_initial_exec(irg)             get_irg_initial_exec_(irg)
#define set_irg_initial_exec(irg, node)       set_irg_initial_exec_(irg, node)
#define get_irg_frame(irg)                    get_irg_frame_(irg)
#define set_irg_frame(irg, node)              set_irg_frame_(irg, node)
#define get_irg_initial_mem(irg)              get_irg_initial_mem_(irg)
#define set_irg_initial_mem(irg, node)        set_irg_initial_mem_(irg, node)
#define get_irg_args(irg)                     get_irg_args_(irg)
#define set_irg_args(irg, node)               set_irg_args_(irg, node)
#define get_irg_no_mem(irg)                   get_irg_no_mem_(irg)
#define set_irn_no_mem(irg, node)             set_irn_no_mem_(irg, node)
#define get_irg_entity(irg)                   get_irg_entity_(irg)
#define set_irg_entity(irg, ent)              set_irg_entity_(irg, ent)
#define get_irg_frame_type(irg)               get_irg_frame_type_(irg)
#define set_irg_frame_type(irg, ftp)          set_irg_frame_type_(irg, ftp)
#define get_irg_pinned(irg)                   get_irg_pinned_(irg)
#define get_irg_callee_info_state(irg)        get_irg_callee_info_state_(irg)
#define set_irg_callee_info_state(irg, s)     set_irg_callee_info_state_(irg, s)
#define get_irg_additional_properties(irg)    get_irg_additional_properties_(irg)
#define set_irg_additional_properties(irg, m) set_irg_additional_properties_(irg, m)
#define set_irg_additional_property(irg, f)   set_irg_additional_property_(irg, f)
#define set_irg_link(irg, thing)              set_irg_link_(irg, thing)
#define get_irg_link(irg)                     get_irg_link_(irg)
#define get_irg_visited(irg)                  get_irg_visited_(irg)
#define get_irg_block_visited(irg)            get_irg_block_visited_(irg)
#define set_irg_block_visited(irg, v)         set_irg_block_visited_(irg, v)
#define inc_irg_block_visited(irg)            inc_irg_block_visited_(irg)
#define dec_irg_block_visited(irg)            dec_irg_block_visited_(irg)
#define get_idx_irn(irg, idx)                 get_idx_irn_(irg, idx)
#define irg_is_constrained(irg, constraints)  irg_is_constrained_(irg, constraints)
#define add_irg_properties(irg, props)        add_irg_properties_(irg, props)
#define clear_irg_properties(irg, props)      clear_irg_properties_(irg, props)
#define irg_has_properties(irg, props)        irg_has_properties_(irg, props)
#define ir_reserve_resources(irg,resources)   ir_reserve_resources_(irg,resources)
#define ir_free_resources(irg,resources)      ir_free_resources_(irg,resources)
#define ir_resources_reserved(irg)            ir_resources_reserved_(irg)

/**
 * Set the number of locals for a given graph.
 *
 * @param irg    the graph
 * @param n_loc  number of locals
 */
void irg_set_nloc(ir_graph *res, int n_loc);

/**
 * Make a rudimentary ir graph for the constant code.
 * Must look like a correct irg, spare everything else.
 */
ir_graph *new_const_code_irg(void);

/**
 * Create a new graph that is a copy of a given one.
 * Uses the link fields of the original graphs.
 *
 * @param irg  The graph that must be copied.
 */
ir_graph *create_irg_copy(ir_graph *irg);

/**
 * Set the op_pin_state_pinned state of a graph.
 *
 * @param irg     the IR graph
 * @param p       new pin state
 */
static inline void set_irg_pinned(ir_graph *irg, op_pin_state p)
{
	irg->irg_pinned_state = p;
}

/** Returns the obstack associated with the graph. */
static inline struct obstack *get_irg_obstack(ir_graph *const irg)
{
	assert(obstack_object_size(&irg->obst) == 0);
	return &irg->obst;
}



/**
 * Returns true if the node n is allocated on the storage of graph irg.
 *
 * @param irg   the IR graph
 * @param n the IR node
 */
int node_is_in_irgs_storage(const ir_graph *irg, const ir_node *n);

/*-------------------------------------------------------------------*/
/* inline functions for graphs                                       */
/*-------------------------------------------------------------------*/

/** Returns the start block of a graph. */
static inline ir_node *get_irg_start_block_(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_start_block);
}

static inline void set_irg_start_block_(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_start_block, node);
}

static inline ir_node *get_irg_start_(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_start);
}

static inline void set_irg_start_(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_start, node);
}

static inline ir_node *get_irg_end_block_(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_end_block);
}

static inline void set_irg_end_block_(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_end_block, node);
}

static inline ir_node *get_irg_end_(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_end);
}

static inline void set_irg_end_(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_end, node);
}

static inline ir_node *get_irg_frame_(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_frame);
}

static inline void set_irg_frame_(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_frame, node);
}

static inline ir_node *get_irg_initial_mem_(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_initial_mem);
}

static inline void set_irg_initial_mem_(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_initial_mem, node);
}

static inline ir_node *get_irg_args_(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_args);
}

static inline void set_irg_args_(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_args, node);
}

static inline ir_node *get_irg_no_mem_(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_no_mem);
}

static inline void set_irg_no_mem_(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_no_mem, node);
}

static inline ir_entity *get_irg_entity_(const ir_graph *irg)
{
	return irg->ent;
}

static inline void set_irg_entity_(ir_graph *irg, ir_entity *ent)
{
	irg->ent = ent;
}

static inline ir_type *get_irg_frame_type_(ir_graph *irg)
{
	assert(irg->frame_type);
	return irg->frame_type;
}

static inline void set_irg_frame_type_(ir_graph *irg, ir_type *ftp)
{
	assert(is_frame_type(ftp));
	irg->frame_type = ftp;
}

static inline op_pin_state get_irg_pinned_(const ir_graph *irg)
{
	return irg->irg_pinned_state;
}

static inline irg_callee_info_state get_irg_callee_info_state_(const ir_graph *irg)
{
	return irg->callee_info_state;
}

static inline void set_irg_callee_info_state_(ir_graph *irg, irg_callee_info_state s)
{
	irg_callee_info_state irp_state = get_irp_callee_info_state();

	irg->callee_info_state = s;

	/* I could compare ... but who knows? */
	if ((irp_state == irg_callee_info_consistent)  ||
	    ((irp_state == irg_callee_info_inconsistent) && (s == irg_callee_info_none)))
		set_irp_callee_info_state(s);
}

static inline void set_irg_link_(ir_graph *irg, void *thing)
{
	irg->link = thing;
}

static inline void *get_irg_link_(const ir_graph *irg)
{
	return irg->link;
}

static inline ir_visited_t get_irg_visited_(const ir_graph *irg)
{
	return irg->visited;
}

static inline ir_visited_t get_irg_block_visited_(const ir_graph *irg)
{
	return irg->block_visited;
}

static inline void set_irg_block_visited_(ir_graph *irg, ir_visited_t visited)
{
	irg->block_visited = visited;
}

static inline void inc_irg_block_visited_(ir_graph *irg)
{
	++irg->block_visited;
}

static inline void dec_irg_block_visited_(ir_graph *irg)
{
	--irg->block_visited;
}

static inline int irg_is_constrained_(const ir_graph *irg,
                                      ir_graph_constraints_t constraints)
{
	return (irg->constraints & constraints) == constraints;
}

static inline void add_irg_properties_(ir_graph *irg,
                                       ir_graph_properties_t props)
{
	irg->properties |= props;
}

static inline void clear_irg_properties_(ir_graph *irg,
                                    ir_graph_properties_t props)
{
	irg->properties &= ~props;
}

static inline int irg_has_properties_(const ir_graph *irg,
                                      ir_graph_properties_t props)
{
	return (irg->properties & props) == props;
}

#ifndef NDEBUG
static inline void ir_reserve_resources_(ir_graph *irg,
                                         ir_resources_t resources)
{
	assert((irg->reserved_resources & resources) == 0);
	irg->reserved_resources |= resources;
}

static inline void ir_free_resources_(ir_graph *irg, ir_resources_t resources)
{
	assert((irg->reserved_resources & resources) == resources);
	irg->reserved_resources &= ~resources;
}
static inline ir_resources_t ir_resources_reserved_(const ir_graph *irg)
{
	return irg->reserved_resources;
}
#else
static inline void ir_reserve_resources_(ir_graph *irg,
                                         ir_resources_t resources)
{
	(void)irg;
	(void)resources;
}

static inline void ir_free_resources_(ir_graph *irg, ir_resources_t resources)
{
	(void)irg;
	(void)resources;
}
static inline ir_resources_t ir_resources_reserved_(const ir_graph *irg)
{
	(void)irg;
	return IR_RESOURCE_NONE;
}
#endif

/**
 * Allocates a new idx in the irg for the node and adds the irn to the idx -> irn map.
 * @param irg The graph.
 * @param irn The node.
 * @return    The index allocated for the node.
 */
static inline unsigned irg_register_node_idx(ir_graph *irg, ir_node *irn)
{
	unsigned idx = irg->last_node_idx++;
	if (idx >= (unsigned)ARR_LEN(irg->idx_irn_map))
		ARR_RESIZE(ir_node *, irg->idx_irn_map, idx + 1);

	irg->idx_irn_map[idx] = irn;
	return idx;
}

/**
 * Kill a node from the irg. BEWARE: this kills
 * all later created nodes.
 */
static inline void irg_kill_node(ir_graph *irg, ir_node *n)
{
	unsigned idx = get_irn_idx(n);
	assert(idx + 1 == irg->last_node_idx);

	if (idx + 1 == irg->last_node_idx)
		--irg->last_node_idx;
	irg->idx_irn_map[idx] = NULL;
	obstack_free(&irg->obst, n);
}

/**
 * Get the node for an index.
 * @param irg The graph.
 * @param idx The index you want the node for.
 * @return    The node with that index or NULL, if there is no node with that index.
 * @note      The node you got might be dead.
 */
static inline ir_node *get_idx_irn_(const ir_graph *irg, unsigned idx)
{
	assert(idx < (unsigned) ARR_LEN(irg->idx_irn_map));
	return irg->idx_irn_map[idx];
}

/**
 * Return the number of anchors in this graph.
 */
static inline int get_irg_n_anchors(const ir_graph *irg)
{
	return get_irn_arity(irg->anchor);
}

/**
 * Return anchor for given index
 */
static inline ir_node *get_irg_anchor(const ir_graph *irg, int idx)
{
	return get_irn_n(irg->anchor, idx);
}

/**
 * Set anchor for given index
 */
static inline void set_irg_anchor(ir_graph *irg, int idx, ir_node *irn)
{
	set_irn_n(irg->anchor, idx, irn);
}

#endif
