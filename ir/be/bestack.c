/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author      Sebastian Hack, Matthias Braun
 *
 * Handling of the stack frame. It is composed of three types:
 * 1) The type of the arguments which are pushed on the stack.
 * 2) The "between type" which consists of stuff the call of the
 *    function pushes on the stack (like the return address and
 *    the old base pointer for ia32).
 * 3) The Firm frame type which consists of all local variables
 *    and the spills.
 */
#include "bestack.h"

#include "beirg.h"
#include "benode.h"
#include "besched.h"
#include "bessaconstr.h"
#include "ircons_t.h"
#include "iredges_t.h"
#include "irgmod.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "util.h"

static unsigned round_up2_misaligned(unsigned const offset,
		unsigned const alignment, unsigned const misalign)
{
	return round_up2(offset + misalign, alignment) - misalign;
}

static void sim_be_IncSP(ir_node *node, stack_pointer_state_t *state)
{
	int      const ofs         = be_get_IncSP_offset(node);
	unsigned const p2align     = be_get_IncSP_no_align(node) ? 0
	                                                         : state->p2align;
	unsigned const alignment   = 1u << p2align;
	int      const prev_offset = state->offset;
	int      const new_offset  = prev_offset - state->align_padding + ofs;
	int      const aligned
		= round_up2_misaligned(new_offset, alignment, state->misalign);
	be_set_IncSP_offset(node, aligned - prev_offset);
	state->align_padding = aligned - new_offset;
	state->offset        = aligned;
}

/**
 * Simulate stack pointer offset relative to offset at function begin.
 */
static void process_stack_bias(sp_sim_func const sim, ir_node *const block,
                               unsigned const p2align, unsigned const misalign,
                               int offset, unsigned align_padding)
{
	/* TODO: We really should check that offset corresponds to the one we got
	 * last time when the block was already visited. Add a map in debug mode? */
	if (Block_block_visited(block))
		return;
	mark_Block_block_visited(block);

	stack_pointer_state_t state = {
		.misalign      = misalign,
		.p2align       = p2align,
		.offset        = offset,
		.align_padding = align_padding,
	};

	sched_foreach(block, node) {
		if (be_is_IncSP(node)) {
			sim_be_IncSP(node, &state);
		} else {
			sim(node, &state);
		}
	}

	/* Continue at our control flow successors. */
	foreach_block_succ(block, edge) {
		ir_node *succ = get_edge_src_irn(edge);
		process_stack_bias(sim, succ, p2align, misalign,
		                   state.offset, state.align_padding);
	}
}

void be_sim_stack_pointer(ir_graph *const irg, unsigned const misalign,
                          unsigned const p2align, sp_sim_func const sim)
{
	ir_node *const start_block = get_irg_start_block(irg);

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	inc_irg_block_visited(irg);
	process_stack_bias(sim, start_block, p2align, misalign, 0, 0);
	ir_free_resources(irg, IR_RESOURCE_BLOCK_VISITED);
}

typedef struct fix_stack_walker_env_t {
	const arch_register_t *sp;
	ir_node              **sp_nodes;
} fix_stack_walker_env_t;

/**
 * Walker. Collect all stack modifying nodes.
 */
static void collect_stack_nodes_walker(ir_node *node, void *data)
{
	fix_stack_walker_env_t *const env = (fix_stack_walker_env_t*)data;
	if (get_irn_mode(node) != mode_T && arch_get_irn_register(node) == env->sp)
		ARR_APP1(ir_node*, env->sp_nodes, node);
}

void be_fix_stack_nodes(ir_graph *const irg, arch_register_t const *const sp)
{
	be_irg_t                  *const birg   = be_birg_from_irg(irg);
	bool                       const ignore = !rbitset_is_set(birg->allocatable_regs, sp->global_index);
	arch_register_req_t const *const sp_req = be_create_reg_req(irg, sp, ignore);

	fix_stack_walker_env_t walker_env;
	walker_env.sp = sp;
	walker_env.sp_nodes = NEW_ARR_F(ir_node*, 0);

	irg_walk_graph(irg, collect_stack_nodes_walker, NULL, &walker_env);

	/* nothing to be done if we didn't find any node, in fact we mustn't
	 * continue, as for endless loops incsp might have had no users and is bad
	 * now.
	 */
	size_t n_sp_nodes = ARR_LEN(walker_env.sp_nodes);
	if (n_sp_nodes == 0) {
		DEL_ARR_F(walker_env.sp_nodes);
		return;
	}

	be_ssa_construction_env_t senv;
	be_ssa_construction_init(&senv, irg);
	be_ssa_construction_add_copies(&senv, walker_env.sp_nodes, n_sp_nodes);
	be_ssa_construction_fix_users_array(&senv, walker_env.sp_nodes, n_sp_nodes);

	be_lv_t *const lv = be_get_irg_liveness(irg);
	if (lv->sets_valid) {
		for (size_t i = 0; i < n_sp_nodes; ++i) {
			be_liveness_update(lv, walker_env.sp_nodes[i]);
		}
		be_ssa_construction_update_liveness_phis(&senv, lv);
	}

	ir_node **phis = be_ssa_construction_get_new_phis(&senv);

	/* set register requirements for stack phis */
	for (size_t i = 0, n_phis = ARR_LEN(phis); i < n_phis; ++i) {
		ir_node *phi = phis[i];
		be_set_phi_reg_req(phi, sp_req);
		arch_set_irn_register(phi, sp);
	}
	be_ssa_construction_destroy(&senv);
	DEL_ARR_F(walker_env.sp_nodes);

	/* when doing code with frame-pointers then often the last sp producers are
	 * not used anymore because we copy the framepointer to the stack pointer
	 * when leaving the function. Though the last sp producer is often kept
	 * (because you often don't know which sp producer is the last one and
	 * fixstack should find them all). Remove unnecessary keep edges and sp
	 * producers. */
	ir_node *end = get_irg_end(irg);
	foreach_irn_in_r(end, i, in) {
		if (get_irn_mode(in) != mode_T && arch_get_irn_register(in) == sp) {
			remove_End_n(end, i);
			if (get_irn_n_edges(in) == 0) {
				if (!is_Proj(in))
					sched_remove(in);
				kill_node(in);
			}
		}
	}
}

static int cmp_slots_last(void const *const p0, void const *const p1)
{
	ir_entity const *const e0 = *(ir_entity const**)p0;
	ir_entity const *const e1 = *(ir_entity const**)p1;
	if (e0->kind == IR_ENTITY_SPILLSLOT) {
		if (e1->kind != IR_ENTITY_SPILLSLOT)
			return -1;
	} else if (e1->kind == IR_ENTITY_SPILLSLOT)
		return 1;

	return QSORT_CMP(e1->nr, e0->nr);
}

static int cmp_slots_first(void const *const p0, void const *const p1)
{
	ir_entity const *const e0 = *(ir_entity const**)p0;
	ir_entity const *const e1 = *(ir_entity const**)p1;
	if (e0->kind == IR_ENTITY_SPILLSLOT) {
		if (e1->kind != IR_ENTITY_SPILLSLOT)
			return 1;
	} else if (e1->kind == IR_ENTITY_SPILLSLOT)
		return -1;

	return QSORT_CMP(e0->nr, e1->nr);
}

void be_sort_frame_entities(ir_type *const frame, bool spillslots_first)
{
	unsigned const n_members = get_compound_n_members(frame);
	assert(is_compound_type(frame));
	QSORT(frame->attr.compound.members, n_members,
		  spillslots_first ? cmp_slots_first : cmp_slots_last);
}

void be_layout_frame_type(ir_type *const frame, int const begin,
                          unsigned const misalign)
{
	assert(get_type_state(frame) == layout_undefined);
	/* Layout entities into negative direction. */
	int offset = begin;
	for (unsigned i = 0, n_members = get_compound_n_members(frame);
		 i < n_members; ++i) {
		ir_entity *const member        = get_compound_member(frame, i);
		int        const member_offset = get_entity_offset(member);
		if (member_offset != INVALID_OFFSET) {
			assert(member_offset >= begin);
			continue;
		}
		assert(get_entity_bitfield_size(member) == 0);

		unsigned size;
		unsigned alignment = get_entity_alignment(member);
		if (member->kind == IR_ENTITY_SPILLSLOT) {
			size = member->attr.spillslot.size;
		} else {
			ir_type const *const type           = get_entity_type(member);
			unsigned       const type_alignment = get_type_alignment(type);
			size      = get_type_size(type);
			alignment = MAX(alignment, type_alignment);
		}

		offset -= size;
		offset  = -round_up2_misaligned(-offset, alignment, misalign);
		set_entity_offset(member, offset);
	}
	set_type_size(frame, -(offset-begin));
	set_type_state(frame, layout_fixed);
}
