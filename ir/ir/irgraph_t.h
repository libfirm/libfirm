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
 * @brief    Entry point to the representation of procedure code -- internal header.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version  $Id$
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
#include "pset.h"
#include "set.h"

/** Suffix that is added to every frame type. */
#define FRAME_TP_SUFFIX "frame_tp"

/**
 * Initializes the graph construction module.
 */
void firm_init_irgraph(void);

/**
 * Set the number of locals for a given graph.
 *
 * @param irg    the graph
 * @param n_loc  number of locals
 */
void irg_set_nloc(ir_graph *res, int n_loc);

/**
 * Internal constructor that does not add to irp_irgs or the like.
 */
ir_graph *new_r_ir_graph(ir_entity *ent, int n_loc);

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
void set_irg_pinned(ir_graph *irg, op_pin_state p);

/** Returns the obstack associated with the graph. */
struct obstack *get_irg_obstack(const ir_graph *irg);

/**
 * Returns true if the node n is allocated on the storage of graph irg.
 *
 * @param irg   the IR graph
 * @param n the IR node
 */
int node_is_in_irgs_storage(ir_graph *irg, ir_node *n);

/*-------------------------------------------------------------------*/
/* inline functions for graphs                                       */
/*-------------------------------------------------------------------*/

static inline int _is_ir_graph(const void *thing)
{
	return (get_kind(thing) == k_ir_graph);
}

/** Returns the start block of a graph. */
static inline ir_node *_get_irg_start_block(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_start_block);
}

static inline void _set_irg_start_block(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_start_block, node);
}

static inline ir_node *_get_irg_start(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_start);
}

static inline void _set_irg_start(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_start, node);
}

static inline ir_node *_get_irg_end_block(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_end_block);
}

static inline void _set_irg_end_block(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, -1, node);
	set_irn_n(irg->anchor, anchor_end_block, node);
}

static inline ir_node *_get_irg_end(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_end);
}

static inline void _set_irg_end(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_end, node);
}

static inline ir_node *_get_irg_initial_exec(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_initial_exec);
}

static inline void _set_irg_initial_exec(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_initial_exec, node);
}

static inline ir_node *_get_irg_frame(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_frame);
}

static inline void _set_irg_frame(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_frame, node);
}

static inline ir_node *_get_irg_tls(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_tls);
}

static inline void _set_irg_tls(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_tls, node);
}

static inline ir_node *_get_irg_initial_mem(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_initial_mem);
}

static inline void _set_irg_initial_mem(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_initial_mem, node);
}

static inline ir_node *_get_irg_args(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_args);
}

static inline void _set_irg_args(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_args, node);
}

static inline ir_node *_get_irg_bad(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_bad);
}

static inline void _set_irg_bad(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_bad, node);
}

static inline ir_node * _get_irg_no_mem(const ir_graph *irg)
{
	return get_irn_n(irg->anchor, anchor_no_mem);
}

static inline void _set_irg_no_mem(ir_graph *irg, ir_node *node)
{
	set_irn_n(irg->anchor, anchor_no_mem, node);
}

static inline ir_entity *_get_irg_entity(const ir_graph *irg)
{
	assert(irg);
	return irg->ent;
}

static inline void _set_irg_entity(ir_graph *irg, ir_entity *ent)
{
	irg->ent = ent;
}

static inline ir_type *_get_irg_frame_type(ir_graph *irg)
{
	assert(irg->frame_type);
	return irg->frame_type;
}

static inline void _set_irg_frame_type(ir_graph *irg, ir_type *ftp)
{
	assert(is_frame_type(ftp));
	irg->frame_type = ftp;
}

static inline struct obstack *_get_irg_obstack(const ir_graph *irg)
{
	return irg->obst;
}


static inline irg_phase_state _get_irg_phase_state(const ir_graph *irg)
{
	return irg->phase_state;
}

static inline void _set_irg_phase_state(ir_graph *irg, irg_phase_state state)
{
	irg->phase_state = state;
}

static inline op_pin_state _get_irg_pinned(const ir_graph *irg)
{
	return irg->irg_pinned_state;
}

static inline irg_outs_state _get_irg_outs_state(const ir_graph *irg)
{
	return irg->outs_state;
}

static inline void _set_irg_outs_inconsistent(ir_graph *irg)
{
	if (irg->outs_state == outs_consistent)
		irg->outs_state = outs_inconsistent;
}

static inline irg_extblk_info_state _get_irg_extblk_state(const ir_graph *irg)
{
	return irg->extblk_state;
}

static inline void _set_irg_extblk_inconsistent(ir_graph *irg)
{
	if (irg->extblk_state == ir_extblk_info_valid)
		irg->extblk_state = ir_extblk_info_invalid;
}

static inline irg_dom_state _get_irg_dom_state(const ir_graph *irg)
{
	return irg->dom_state;
}

static inline irg_dom_state _get_irg_postdom_state(const ir_graph *irg)
{
	return irg->pdom_state;
}

static inline void _set_irg_doms_inconsistent(ir_graph *irg)
{
	if (irg->dom_state != dom_none)
		irg->dom_state = dom_inconsistent;
	if (irg->pdom_state != dom_none)
		irg->pdom_state = dom_inconsistent;
}

static inline irg_loopinfo_state _get_irg_loopinfo_state(const ir_graph *irg)
{
	return irg->loopinfo_state;
}

static inline void _set_irg_loopinfo_state(ir_graph *irg, irg_loopinfo_state s)
{
  irg->loopinfo_state = s;
}

static inline void _set_irg_loopinfo_inconsistent(ir_graph *irg)
{
	irg->loopinfo_state &= ~loopinfo_valid;
}

static inline void _set_irg_pinned(ir_graph *irg, op_pin_state p)
{
	irg->irg_pinned_state = p;
}

static inline irg_callee_info_state _get_irg_callee_info_state(const ir_graph *irg)
{
	return irg->callee_info_state;
}

static inline void _set_irg_callee_info_state(ir_graph *irg, irg_callee_info_state s)
{
	irg_callee_info_state irp_state = get_irp_callee_info_state();

	irg->callee_info_state = s;

	/* I could compare ... but who knows? */
	if ((irp_state == irg_callee_info_consistent)  ||
	    ((irp_state == irg_callee_info_inconsistent) && (s == irg_callee_info_none)))
		set_irp_callee_info_state(s);
}

static inline irg_inline_property _get_irg_inline_property(const ir_graph *irg)
{
	return irg->inline_property;
}

static inline void _set_irg_inline_property(ir_graph *irg, irg_inline_property s)
{
	irg->inline_property = s;
}

static inline unsigned _get_irg_additional_properties(const ir_graph *irg)
{
	if (irg->additional_properties & mtp_property_inherited)
		return get_method_additional_properties(get_entity_type(irg->ent));
	return irg->additional_properties;
}

static inline void _set_irg_additional_properties(ir_graph *irg, unsigned mask)
{
	irg->additional_properties = mask & ~mtp_property_inherited;
}

static inline void _set_irg_additional_property(ir_graph *irg, mtp_additional_property flag)
{
	unsigned prop = irg->additional_properties;

	if (prop & mtp_property_inherited)
		prop = get_method_additional_properties(get_entity_type(irg->ent));
	irg->additional_properties = prop | flag;
}

static inline void _set_irg_link(ir_graph *irg, void *thing)
{
	irg->link = thing;
}

static inline void *_get_irg_link(const ir_graph *irg)
{
	return irg->link;
}

static inline ir_visited_t _get_irg_visited(const ir_graph *irg)
{
	return irg->visited;
}

static inline ir_visited_t _get_irg_block_visited(const ir_graph *irg)
{
	return irg->block_visited;
}

static inline void _set_irg_block_visited(ir_graph *irg, ir_visited_t visited)
{
	irg->block_visited = visited;
}

static inline void _inc_irg_block_visited(ir_graph *irg)
{
	++irg->block_visited;
}

static inline void _dec_irg_block_visited(ir_graph *irg)
{
	--irg->block_visited;
}

static inline unsigned _get_irg_estimated_node_cnt(const ir_graph *irg)
{
	return irg->estimated_node_count;
}

/* Return the floating point model of this graph. */
static inline unsigned _get_irg_fp_model(const ir_graph *irg)
{
	return irg->fp_model;
}

static inline void _set_irg_state(ir_graph *irg, ir_graph_state_t state)
{
	irg->state |= state;
}

static inline void _clear_irg_state(ir_graph *irg, ir_graph_state_t state)
{
	irg->state &= ~state;
}

static inline int _is_irg_state(const ir_graph *irg, ir_graph_state_t state)
{
	return (irg->state & state) == state;
}

/**
 * Allocates a new idx in the irg for the node and adds the irn to the idx -> irn map.
 * @param irg The graph.
 * @param irn The node.
 * @return    The index allocated for the node.
 */
static inline unsigned irg_register_node_idx(ir_graph *irg, ir_node *irn) {
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
	obstack_free(irg->obst, n);
}

/**
 * Get the node for an index.
 * @param irg The graph.
 * @param idx The index you want the node for.
 * @return    The node with that index or NULL, if there is no node with that index.
 * @note      The node you got might be dead.
 */
static inline ir_node *_get_idx_irn(ir_graph *irg, unsigned idx)
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



/**
 * Register a phase on an irg.
 * The phase will then be managed by the irg. This means you can easily
 * access the phase when you only have a graph handle, the memory will be
 * freed when the graph is freed and some care is taken that the phase data
 * will be invalidated/preserved on events like dead code elemination and
 * code selection.
 */
void irg_register_phase(ir_graph *irg, ir_phase_id id, ir_phase *phase);

/**
 * Frees all phase infos attached to an irg
 */
void irg_invalidate_phases(ir_graph *irg);

/**
 * return phase with given id
 */
static inline ir_phase *irg_get_phase(const ir_graph *irg, ir_phase_id id)
{
	assert(id <= PHASE_LAST);
	return irg->phases[id];
}


#define is_ir_graph(thing)                    _is_ir_graph(thing)
#define get_irg_start_block(irg)              _get_irg_start_block(irg)
#define set_irg_start_block(irg, node)        _set_irg_start_block(irg, node)
#define get_irg_start(irg)                    _get_irg_start(irg)
#define set_irg_start(irg, node)              _set_irg_start(irg, node)
#define get_irg_end_block(irg)                _get_irg_end_block(irg)
#define set_irg_end_block(irg, node)          _set_irg_end_block(irg, node)
#define get_irg_end(irg)                      _get_irg_end(irg)
#define set_irg_end(irg, node)                _set_irg_end(irg, node)
#define get_irg_initial_exec(irg)             _get_irg_initial_exec(irg)
#define set_irg_initial_exec(irg, node)       _set_irg_initial_exec(irg, node)
#define get_irg_frame(irg)                    _get_irg_frame(irg)
#define set_irg_frame(irg, node)              _set_irg_frame(irg, node)
#define get_irg_tls(irg)                      _get_irg_tls(irg)
#define set_irg_tls(irg, node)                _set_irg_tls(irg, node)
#define get_irg_initial_mem(irg)              _get_irg_initial_mem(irg)
#define set_irg_initial_mem(irg, node)        _set_irg_initial_mem(irg, node)
#define get_irg_args(irg)                     _get_irg_args(irg)
#define set_irg_args(irg, node)               _set_irg_args(irg, node)
#define get_irg_bad(irg)                      _get_irg_bad(irg)
#define set_irg_bad(irg, node)                _set_irg_bad(irg, node)
#define get_irg_no_mem(irg)                   _get_irg_no_mem(irg)
#define set_irn_no_mem(irg, node)             _set_irn_no_mem(irg, node)
#define get_irg_entity(irg)                   _get_irg_entity(irg)
#define set_irg_entity(irg, ent)              _set_irg_entity(irg, ent)
#define get_irg_frame_type(irg)               _get_irg_frame_type(irg)
#define set_irg_frame_type(irg, ftp)          _set_irg_frame_type(irg, ftp)
#define get_irg_obstack(irg)                  _get_irg_obstack(irg)
#define get_irg_phase_state(irg)              _get_irg_phase_state(irg)
#define set_irg_phase_state(irg, state)       _set_irg_phase_state(irg, state)
#define get_irg_pinned(irg)                   _get_irg_pinned(irg)
#define get_irg_outs_state(irg)               _get_irg_outs_state(irg)
#define set_irg_outs_inconsistent(irg)        _set_irg_outs_inconsistent(irg)
#define get_irg_extblk_state(irg)             _get_irg_extblk_state(irg)
#define set_irg_extblk_inconsistent(irg)      _set_irg_extblk_inconsistent(irg)
#define get_irg_dom_state(irg)                _get_irg_dom_state(irg)
#define get_irg_postdom_state(irg)            _get_irg_postdom_state(irg)
#define set_irg_doms_inconsistent(irg)        _set_irg_doms_inconsistent(irg)
#define get_irg_loopinfo_state(irg)           _get_irg_loopinfo_state(irg)
#define set_irg_loopinfo_state(irg, s)        _set_irg_loopinfo_state(irg, s)
#define set_irg_loopinfo_inconsistent(irg)    _set_irg_loopinfo_inconsistent(irg)
#define set_irg_pinned(irg, p)                _set_irg_pinned(irg, p)
#define get_irg_callee_info_state(irg)        _get_irg_callee_info_state(irg)
#define set_irg_callee_info_state(irg, s)     _set_irg_callee_info_state(irg, s)
#define get_irg_inline_property(irg)          _get_irg_inline_property(irg)
#define set_irg_inline_property(irg, s)       _set_irg_inline_property(irg, s)
#define get_irg_additional_properties(irg)    _get_irg_additional_properties(irg)
#define set_irg_additional_properties(irg, m) _set_irg_additional_properties(irg, m)
#define set_irg_additional_property(irg, f)   _set_irg_additional_property(irg, f)
#define set_irg_link(irg, thing)              _set_irg_link(irg, thing)
#define get_irg_link(irg)                     _get_irg_link(irg)
#define get_irg_visited(irg)                  _get_irg_visited(irg)
#define get_irg_block_visited(irg)            _get_irg_block_visited(irg)
#define set_irg_block_visited(irg, v)         _set_irg_block_visited(irg, v)
#define inc_irg_block_visited(irg)            _inc_irg_block_visited(irg)
#define dec_irg_block_visited(irg)            _dec_irg_block_visited(irg)
#define get_irg_estimated_node_cnt(irg)       _get_irg_estimated_node_cnt(irg)
#define get_irg_fp_model(irg)                 _get_irg_fp_model(irg)
#define get_idx_irn(irg, idx)                 _get_idx_irn(irg, idx)
#define set_irg_state(irg, state)             _set_irg_state(irg, state)
#define clear_irg_state(irg, state)           _clear_irg_state(irg, state)
#define is_irg_state(irg, state)              _is_irg_state(irg, state)

#endif
