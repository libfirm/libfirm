/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Data-flow driven minimal fixpoint value range propagation
 * @author  Christoph Mallon
 */
#include <assert.h>
#include <stdbool.h>

#include "adt/pdeq.h"
#include "adt/obst.h"
#include "adt/xmalloc.h"
#include "debug.h"
#include "ircons.h"
#include "irdom.h"
#include "iredges.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "iroptimize.h"
#include "tv.h"
#include "irpass.h"
#include "irmemory.h"
#include "constbits.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

typedef struct environment_t {
	unsigned modified:1;     /**< Set, if the graph was modified. */
} environment_t;

static int mode_is_intb(ir_mode const* const m)
{
	return mode_is_int(m) || m == mode_b;
}

static void apply_result(ir_node* const irn, void* ctx)
{
	environment_t* env = (environment_t*)ctx;
	ir_node*       block;
	bitinfo*       block_b;
	bitinfo*       b;
	ir_tarval*     z;
	ir_tarval*     o;

	if (is_Block(irn)) {
		block_b = get_bitinfo(irn);
		/* Trivially unreachable blocks have no info. */
		if (block_b == NULL || block_b->z == get_tarval_b_false()) {
			ir_graph *irg  = get_irn_irg(irn);
			ir_node  *bad = new_r_Bad(irg, mode_BB);
			exchange(irn, bad);
			env->modified = 1;
		}
		return;
	}

	block   = get_nodes_block(irn);
	block_b = get_bitinfo(block);
	/* Trivially unreachable blocks have no info. */
	if (block_b == NULL || block_b->z == get_tarval_b_false()) {
		/* Unreachable blocks might be replaced before the nodes in them. */
		ir_mode  *mode = get_irn_mode(irn);
		ir_graph *irg  = get_irn_irg(irn);
		ir_node  *bad  = new_r_Bad(irg, mode);
		exchange(irn, bad);
		env->modified = 1;
		return;
	}

	b = get_bitinfo(irn);
	if (!b) return;
	if (is_Const(irn)) return; // It cannot get any better than a Const.

	z = b->z;
	o = b->o;
	// Only display information if we could find out anything about the value.
	DEBUG_ONLY(if (!tarval_is_all_one(z) || !tarval_is_null(o)))
		DB((dbg, LEVEL_2, "%+F: 0:%T 1:%T%s\n", irn, z, o, z == o ? " --- constant" : ""));

	// Replace node with constant value by Const.
	if (z == o) {
		ir_mode* const m = get_irn_mode(irn);
		ir_node*       n;
		if (mode_is_intb(m)) {
			ir_graph *irg = get_irn_irg(irn);
			n = new_r_Const(irg, z);
		} else if (m == mode_X) {
			ir_graph* const irg = get_Block_irg(block);
			if (z == get_tarval_b_true()) {
				n = new_r_Jmp(block);
			} else {
				n = new_r_Bad(irg, mode_X);
				/* Transferring analysis information to the bad node makes it a
				 * candidate for replacement. */
				goto exchange_only;
			}
		} else {
			return;
		}
		set_irn_link(n, b);
exchange_only:
		exchange(irn, n);
		env->modified = 1;
	}

	switch (get_irn_opcode(irn)) {
		case iro_And: {
			ir_node*       const l  = get_And_left(irn);
			ir_node*       const r  = get_And_right(irn);
			bitinfo const* const bl = get_bitinfo(l);
			bitinfo const* const br = get_bitinfo(r);
			if (tarval_is_null(tarval_andnot(br->z, bl->o))) {
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) is superfluous\n", irn, l, r));
				exchange(irn, r);
				env->modified = 1;
			} else if (tarval_is_null(tarval_andnot(bl->z, br->o))) {
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) is superfluous\n", irn, l, r));
				exchange(irn, l);
				env->modified = 1;
			}
			break;
		}

		case iro_Eor: {
			ir_node*       const l  = get_Eor_left(irn);
			ir_node*       const r  = get_Eor_right(irn);
			bitinfo const* const bl = get_bitinfo(l);
			bitinfo const* const br = get_bitinfo(r);
			/* if each bit is guaranteed to be zero on either the left or right
			 * then an Add will have the same effect as the Eor. Change it for
			 * normalisation */
			if (tarval_is_null(tarval_and(bl->z, br->z))) {
				dbg_info      *dbgi     = get_irn_dbg_info(irn);
				ir_node       *block    = get_nodes_block(irn);
				ir_mode       *mode     = get_irn_mode(irn);
				ir_node       *new_node = new_rd_Add(dbgi, block, l, r, mode);
				bitinfo const *bi       = get_bitinfo(irn);
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) normalised to Add\n", irn, l, r));
				set_bitinfo(new_node, bi->z, bi->o);
				exchange(irn, new_node);
				env->modified = 1;
			}
			break;
		}

		case iro_Minus: {
			ir_mode *mode = get_irn_mode(irn);

			/* If all bits except the highest bit are zero the Minus is superfluous. */
			if (get_mode_arithmetic(mode) == irma_twos_complement) {
				ir_node         *const op  = get_Minus_op(irn);
				bitinfo   const *const b   = get_bitinfo(op);
				ir_tarval       *const min = get_mode_min(mode);

				if (b->z == min) {
					DB((dbg, LEVEL_2, "%+F(%+F) is superfluous\n", irn, op));
					exchange(irn, op);
					env->modified = 1;
				}
			}
			break;
		}

		case iro_Or: {
			ir_node*       const l  = get_Or_left(irn);
			ir_node*       const r  = get_Or_right(irn);
			bitinfo const* const bl = get_bitinfo(l);
			bitinfo const* const br = get_bitinfo(r);
			if (tarval_is_null(tarval_andnot(bl->z, br->o))) {
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) is superfluous\n", irn, l, r));
				exchange(irn, r);
				env->modified = 1;
			} else if (tarval_is_null(tarval_andnot(br->z, bl->o))) {
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) is superfluous\n", irn, l, r));
				exchange(irn, l);
				env->modified = 1;
			}

			/* if each bit is guaranteed to be zero on either the left or right
			 * then an Add will have the same effect as the Or. Change it for
			 * normalisation */
			if (tarval_is_null(tarval_and(bl->z, br->z))) {
				dbg_info      *dbgi     = get_irn_dbg_info(irn);
				ir_node       *block    = get_nodes_block(irn);
				ir_mode       *mode     = get_irn_mode(irn);
				ir_node       *new_node = new_rd_Add(dbgi, block, l, r, mode);
				bitinfo const *bi       = get_bitinfo(irn);
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) normalised to Add\n", irn, l, r));
				set_bitinfo(new_node, bi->z, bi->o);
				exchange(irn, new_node);
				env->modified = 1;
			}

			break;
		}
	}
}

void fixpoint_vrp(ir_graph* const irg)
{
	environment_t env;
	struct obstack private_obst;

	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
		| IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		| IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	obstack_init(&private_obst);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	constbits_analyze(irg, &private_obst);

	FIRM_DBG_REGISTER(dbg, "firm.opt.fp-vrp");
	DB((dbg, LEVEL_1, "===> Performing constant propagation on %+F (optimization)\n", irg));

	DB((dbg, LEVEL_2, "---> Applying analysis results\n"));
	env.modified = 0;
	irg_walk_graph(irg, NULL, apply_result, &env);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	obstack_free(&private_obst, NULL);

	confirm_irg_properties(irg,
		env.modified ? IR_GRAPH_PROPERTIES_NONE : IR_GRAPH_PROPERTIES_ALL);
}

ir_graph_pass_t *fixpoint_vrp_irg_pass(const char *name)
{
	return def_graph_pass(name ? name : "fixpoint_vrp", fixpoint_vrp);
}
