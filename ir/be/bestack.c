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
#include "besched.h"
#include "benode.h"
#include "bessaconstr.h"

#include "ircons_t.h"
#include "iredges_t.h"
#include "irnode_t.h"
#include "irgwalk.h"
#include "irgmod.h"

int be_get_stack_entity_offset(be_stack_layout_t *frame, ir_entity *ent,
                               int bias)
{
	ir_type *t   = get_entity_owner(ent);
	int      ofs = get_entity_offset(ent);
	/* Find the type the entity is contained in. */
	for (unsigned index = 0; index < N_FRAME_TYPES; ++index) {
		if (frame->order[index] == t)
			break;
		/* Add the size of all the types below the one of the entity to the entity's offset */
		ofs += get_type_size(frame->order[index]);
	}
	/* correct the offset by the initial position of the frame pointer */
	ofs -= frame->initial_offset;
	/* correct the offset with the current bias. */
	ofs += bias;
	return ofs;
}

/**
 * Retrieve the entity with given offset from a frame type.
 */
static ir_entity *search_ent_with_offset(ir_type *t, int offset)
{
	for (size_t i = 0, n = get_compound_n_members(t); i < n; ++i) {
		ir_entity *ent = get_compound_member(t, i);
		if (get_entity_offset(ent) == offset)
			return ent;
	}
	return NULL;
}

static void stack_frame_compute_initial_offset(be_stack_layout_t *frame)
{
	ir_type   *base = frame->between_type;
	ir_entity *ent  = search_ent_with_offset(base, 0);
	if (ent == NULL) {
		frame->initial_offset = get_type_size(frame->frame_type);
	} else {
		frame->initial_offset = be_get_stack_entity_offset(frame, ent, 0);
	}
}

/**
 * A helper struct for the bias walker.
 */
typedef struct bias_walk {
	get_sp_bias_func      get_sp_bias;
	set_frame_offset_func set_frame_offset;
	get_frame_entity_func get_frame_entity;
} bias_walk;

/**
 * Fix all stack accessing operations in the block bl.
 *
 * @param bl         the block to process
 * @param real_bias  the bias value
 */
static void process_stack_bias(bias_walk *bw, ir_node *bl, int real_bias, int wanted_bias)
{
	if (Block_block_visited(bl)) {
		return;
	}

	mark_Block_block_visited(bl);

	ir_graph          *irg         = get_irn_irg(bl);
	be_stack_layout_t *layout      = be_get_irg_stack_layout(irg);
	bool               sp_relative = layout->sp_relative;

	sched_foreach(bl, irn) {
		/* Check, if the node relates to an entity on the stack frame.
		 * If so, set the true offset (including the bias) for that
		 * node. */
		ir_entity *ent = bw->get_frame_entity(irn);
		if (ent != NULL) {
			int bias   = sp_relative ? real_bias : 0;
			int offset = be_get_stack_entity_offset(layout, ent, bias);
			bw->set_frame_offset(irn, offset);
		}

		/* If the node modifies the stack pointer by a constant offset,
		 * record that in the bias. */
		if (be_is_IncSP(irn)) {
			int      ofs   = be_get_IncSP_offset(irn);
			unsigned align = be_get_IncSP_align(irn);
			/* fill in real stack frame size */
			if (align > 0) {
				/* patch IncSP to produce an aligned stack pointer */
				int const between_size = get_type_size(layout->between_type);
				int const alignment    = 1 << align;
				int const delta        = (real_bias + ofs + between_size) & (alignment - 1);
				assert(ofs >= 0);
				if (delta > 0) {
					be_set_IncSP_offset(irn, ofs + alignment - delta);
					real_bias += alignment - delta;
				}
			} else {
				/* adjust so real_bias corresponds with wanted_bias */
				int delta = wanted_bias - real_bias;
				assert(delta <= 0);
				if (delta != 0) {
					be_set_IncSP_offset(irn, ofs + delta);
					real_bias += delta;
				}
			}
			real_bias   += ofs;
			wanted_bias += ofs;
		} else {
			int ofs = bw->get_sp_bias(irn);
			if (ofs == SP_BIAS_RESET) {
				real_bias   = 0;
				wanted_bias = 0;
			} else {
				real_bias   += ofs;
				wanted_bias += ofs;
			}
		}
	}

	assert(real_bias >= wanted_bias);

	/* Since we know our biases, we can now handle our control flow successors. */
	foreach_out_edge_kind_safe(bl, edge, EDGE_KIND_BLOCK) {
		ir_node *pred = get_edge_src_irn(edge);
		process_stack_bias(bw, pred, real_bias, wanted_bias);
	}
}

void be_abi_fix_stack_bias(ir_graph *irg, get_sp_bias_func get_sp_bias,
                           set_frame_offset_func set_frame_offset,
                           get_frame_entity_func get_frame_entity)
{
	be_stack_layout_t *stack_layout = be_get_irg_stack_layout(irg);

	stack_frame_compute_initial_offset(stack_layout);

	int      initial_bias = stack_layout->initial_bias;
	ir_node *start_block  = get_irg_start_block(irg);
	bias_walk bw = {
		.get_sp_bias      = get_sp_bias,
		.set_frame_offset = set_frame_offset,
		.get_frame_entity = get_frame_entity,
	};

	ir_reserve_resources(irg, IR_RESOURCE_BLOCK_VISITED);
	inc_irg_block_visited(irg);
	process_stack_bias(&bw, start_block, initial_bias, initial_bias);
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
	be_irg_t *const birg = be_birg_from_irg(irg);
	const arch_register_req_t *sp_req;
	if (!rbitset_is_set(birg->allocatable_regs, sp->global_index)) {
		sp_req = be_create_reg_req(irg, sp, true);
	} else {
		sp_req = sp->single_req;
	}

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
