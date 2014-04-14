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
#include "irnode_t.h"
#include "irgwalk.h"
#include "irgmod.h"

int be_get_stack_entity_offset(be_stack_layout_t *frame, ir_entity *ent,
                               int bias)
{
	ir_type *t = get_entity_owner(ent);
	int ofs    = get_entity_offset(ent);

	int index;

	/* Find the type the entity is contained in. */
	for (index = 0; index < N_FRAME_TYPES; ++index) {
		if (frame->order[index] == t)
			break;
		/* Add the size of all the types below the one of the entity to the entity's offset */
		ofs += get_type_size_bytes(frame->order[index]);
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
	int i, n;

	for (i = 0, n = get_compound_n_members(t); i < n; ++i) {
		ir_entity *ent = get_compound_member(t, i);
		if (get_entity_offset(ent) == offset)
			return ent;
	}

	return NULL;
}

static void stack_frame_compute_initial_offset(be_stack_layout_t *frame)
{
	ir_type  *base = frame->between_type;
	ir_entity *ent = search_ent_with_offset(base, 0);

	if (ent == NULL) {
		frame->initial_offset = get_type_size_bytes(frame->frame_type);
	} else {
		frame->initial_offset = be_get_stack_entity_offset(frame, ent, 0);
	}
}

/**
 * Walker: finally lower all Members of outer frame or parameter
 * entities.
 */
static void lower_outer_frame_members(ir_node *sel, void *ctx)
{
	ir_node           *ptr;
	ir_entity         *ent;
	ir_type           *owner;
	be_stack_layout_t *layout;
	ir_graph          *irg;
	(void) ctx;

	if (!is_Member(sel))
		return;

	ent    = get_Member_entity(sel);
	owner  = get_entity_owner(ent);
	ptr    = get_Member_ptr(sel);
	irg    = get_irn_irg(sel);
	layout = be_get_irg_stack_layout(irg);

	if (owner == layout->frame_type || owner == layout->arg_type) {
		/* found access to outer frame or arguments */
		int offset = be_get_stack_entity_offset(layout, ent, 0);

		if (offset != 0) {
			ir_node  *bl   = get_nodes_block(sel);
			dbg_info *dbgi = get_irn_dbg_info(sel);
			ir_mode  *mode = get_irn_mode(sel);
			ir_mode  *mode_UInt = get_reference_mode_unsigned_eq(mode);
			ir_node  *cnst = new_r_Const_long(irg, mode_UInt, offset);

			ptr = new_rd_Add(dbgi, bl, ptr, cnst, mode);
		}
		exchange(sel, ptr);
	}
}

/**
 * A helper struct for the bias walker.
 */
typedef struct bias_walk {
	int      start_block_bias; /**< The bias at the end of the start block. */
	ir_node *start_block;      /**< The start block of the current graph. */
} bias_walk;

/**
 * Fix all stack accessing operations in the block bl.
 *
 * @param bl         the block to process
 * @param real_bias  the bias value
 *
 * @return the bias at the end of this block
 */
static int process_stack_bias(ir_node *bl, int real_bias)
{
	int                wanted_bias = real_bias;
	ir_graph          *irg         = get_Block_irg(bl);
	be_stack_layout_t *layout      = be_get_irg_stack_layout(irg);
	bool               sp_relative = layout->sp_relative;
	const arch_env_t  *arch_env    = be_get_irg_arch_env(irg);

	sched_foreach(bl, irn) {
		int ofs;

		/*
		   Check, if the node relates to an entity on the stack frame.
		   If so, set the true offset (including the bias) for that
		   node.
		 */
		ir_entity *ent = arch_get_frame_entity(irn);
		if (ent != NULL) {
			int bias   = sp_relative ? real_bias : 0;
			int offset = be_get_stack_entity_offset(layout, ent, bias);
			arch_set_frame_offset(irn, offset);
		}

		/*
		 * If the node modifies the stack pointer by a constant offset,
		 * record that in the bias.
		 */
		if (be_is_IncSP(irn)) {
			ofs = be_get_IncSP_offset(irn);
			/* fill in real stack frame size */
			if (be_get_IncSP_align(irn)) {
				/* patch IncSP to produce an aligned stack pointer */
				int const between_size = get_type_size_bytes(layout->between_type);
				int const alignment    = 1 << arch_env->stack_alignment;
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
			ofs = arch_get_sp_bias(irn);
			if (ofs == SP_BIAS_RESET) {
				real_bias   = 0;
				wanted_bias = 0;
			} else {
				real_bias   += ofs;
				wanted_bias += ofs;
			}
		}
	}

	assert(real_bias == wanted_bias);
	return real_bias;
}

/**
 * Block-Walker: fix all stack offsets for all blocks
 * except the start block
 */
static void stack_bias_walker(ir_node *bl, void *data)
{
	bias_walk *bw = (bias_walk*)data;
	if (bl != bw->start_block) {
		process_stack_bias(bl, bw->start_block_bias);
	}
}

void be_abi_fix_stack_bias(ir_graph *irg)
{
	be_stack_layout_t *stack_layout = be_get_irg_stack_layout(irg);
	ir_type           *frame_tp;
	int                i;
	bias_walk          bw;

	stack_frame_compute_initial_offset(stack_layout);

	/* Determine the stack bias at the end of the start block. */
	bw.start_block      = get_irg_start_block(irg);
	bw.start_block_bias = process_stack_bias(bw.start_block, stack_layout->initial_bias);

	/* fix the bias is all other blocks */
	irg_block_walk_graph(irg, stack_bias_walker, NULL, &bw);

	/* fix now inner functions: these still have Sel node to outer
	   frame and parameter entities */
	frame_tp = get_irg_frame_type(irg);
	for (i = get_class_n_members(frame_tp) - 1; i >= 0; --i) {
		ir_entity *ent = get_class_member(frame_tp, i);
		if (!is_method_entity(ent))
			continue;
		ir_graph *irg = get_entity_irg(ent);
		if (irg != NULL)
			irg_walk_graph(irg, NULL, lower_outer_frame_members, NULL);
	}
}

typedef struct fix_stack_walker_env_t {
	ir_node **sp_nodes;
} fix_stack_walker_env_t;

/**
 * Walker. Collect all stack modifying nodes.
 */
static void collect_stack_nodes_walker(ir_node *node, void *data)
{
	fix_stack_walker_env_t *const env = (fix_stack_walker_env_t*)data;

	if (get_irn_mode(node) == mode_T)
		return;

	arch_register_req_t const *const req = arch_get_irn_register_req(node);
	if (!arch_register_req_is(req, produces_sp))
		return;

	ARR_APP1(ir_node*, env->sp_nodes, node);
}

void be_abi_fix_stack_nodes(ir_graph *irg)
{
	be_lv_t                   *lv       = be_get_irg_liveness(irg);
	const arch_env_t          *arch_env = be_get_irg_arch_env(irg);
	be_irg_t                  *birg     = be_birg_from_irg(irg);
	const arch_register_t     *sp       = arch_env->sp;
	be_ssa_construction_env_t  senv;
	int len;
	ir_node **phis;
	fix_stack_walker_env_t walker_env;

	arch_register_req_t const *sp_req = birg->sp_req;
	if (sp_req == NULL) {
		arch_register_req_type_t type = arch_register_req_type_produces_sp;
		if (!rbitset_is_set(birg->allocatable_regs, sp->global_index))
			type |= arch_register_req_type_ignore;

		struct obstack *const obst = be_get_be_obst(irg);
		birg->sp_req = sp_req = be_create_reg_req(obst, sp, type);
	}

	walker_env.sp_nodes = NEW_ARR_F(ir_node*, 0);

	irg_walk_graph(irg, collect_stack_nodes_walker, NULL, &walker_env);

	/* nothing to be done if we didn't find any node, in fact we mustn't
	 * continue, as for endless loops incsp might have had no users and is bad
	 * now.
	 */
	len = ARR_LEN(walker_env.sp_nodes);
	if (len == 0) {
		DEL_ARR_F(walker_env.sp_nodes);
		return;
	}

	be_ssa_construction_init(&senv, irg);
	be_ssa_construction_add_copies(&senv, walker_env.sp_nodes,
                                   ARR_LEN(walker_env.sp_nodes));
	be_ssa_construction_fix_users_array(&senv, walker_env.sp_nodes,
	                                    ARR_LEN(walker_env.sp_nodes));

	if (lv != NULL) {
		len = ARR_LEN(walker_env.sp_nodes);
		for (int i = 0; i < len; ++i) {
			be_liveness_update(lv, walker_env.sp_nodes[i]);
		}
		be_ssa_construction_update_liveness_phis(&senv, lv);
	}

	phis = be_ssa_construction_get_new_phis(&senv);

	/* set register requirements for stack phis */
	len = ARR_LEN(phis);
	for (int i = 0; i < len; ++i) {
		ir_node *phi = phis[i];
		be_set_phi_reg_req(phi, sp_req);
		arch_set_irn_register(phi, arch_env->sp);
	}
	be_ssa_construction_destroy(&senv);
	DEL_ARR_F(walker_env.sp_nodes);

	/* when doing code with frame-pointers then often the last incsp-nodes are
	 * not used anymore because we copy the framepointer to the stack pointer
	 * when leaving the function. Though the last incsp is often kept (because
	 * you often don't know which incsp is the last one and fixstack should find
	 * them all). Remove unnecessary keeps and IncSP nodes */
	ir_node *end = get_irg_end(irg);
	foreach_irn_in_r(end, i, in) {
		if (!be_is_IncSP(in)) {
			continue;
		}

		remove_End_keepalive(end, in);
		if (get_irn_n_edges(in) == 0) {
			sched_remove(in);
			kill_node(in);
		}
	}
}
