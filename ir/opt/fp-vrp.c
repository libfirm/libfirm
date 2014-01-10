/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Data-flow driven minimal fixpoint value range propagation
 * @author  Christoph Mallon
 */
#include "adt/obst.h"
#include "constbits.h"
#include "debug.h"
#include "ircons.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "iroptimize.h"
#include "tv.h"

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

	if (is_Block(irn)) {
		bitinfo *block_b = get_bitinfo(irn);
		/* Trivially unreachable blocks have no info. */
		if (block_b == NULL || block_b->z == get_tarval_b_false()) {
			ir_graph *irg  = get_irn_irg(irn);
			ir_node  *bad = new_r_Bad(irg, mode_BB);
			exchange(irn, bad);
			env->modified = 1;
		}
		return;
	}

	ir_node *block   = get_nodes_block(irn);
	bitinfo *block_b = get_bitinfo(block);
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

	bitinfo *b = get_bitinfo(irn);
	if (!b) return;
	if (is_Const(irn)) return; // It cannot get any better than a Const.

	ir_tarval *z = b->z;
	ir_tarval *o = b->o;
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

		case iro_Cmp: {
			ir_node       *const  l = get_Cmp_left(irn);
			ir_node       *const  r = get_Cmp_right(irn);
			const bitinfo *const  bl = get_bitinfo(l);
			const bitinfo *const  br = get_bitinfo(r);
			if (bl == NULL || br == NULL)
				break;

			ir_mode   *const mode  = get_irn_mode(l);
			ir_tarval *const l_o   = bl->o;
			ir_tarval *const l_z   = bl->z;
			ir_tarval *const r_o   = br->o;
			ir_tarval *const r_z   = br->z;
			ir_tarval *      l_max;
			ir_tarval *      l_min;
			ir_tarval *      r_max;
			ir_tarval *      r_min;
			if (mode_is_signed(mode)) {
				if (!get_mode_arithmetic(mode) == irma_twos_complement)
					break;
				ir_tarval *min     = get_mode_min(mode);
				ir_tarval *not_min = tarval_not(min);
				l_max = l_z;
				l_min = l_o;
				if (tarval_is_negative(l_z)) {
					/* Value may be negative. */
					l_min = tarval_or(l_o, min);
				}
				if (!tarval_is_negative(l_o)) {
					/* Value may be positive. */
					l_max = tarval_and(l_z, not_min);
				}
				r_max = r_z;
				r_min = r_o;
				if (tarval_is_negative(r_z)) {
					/* Value may be negative. */
					r_min = tarval_or(r_o, min);
				}
				if (!tarval_is_negative(r_o)) {
					/* Value may be positive. */
					r_max = tarval_and(r_z, not_min);
				}
			} else {
				l_max = l_z;
				l_min = l_o;
				r_max = r_z;
				r_min = r_o;
			}

			const ir_relation relation     = get_Cmp_relation(irn);
			ir_relation       new_relation = relation;
			if (!(tarval_cmp(l_max, r_min) & ir_relation_greater)) {
				new_relation &= ~ir_relation_greater;
			}
			if (!(tarval_cmp(l_min, r_max) & ir_relation_less)) {
				new_relation &= ~ir_relation_less;
			}
			if (!tarval_is_null(tarval_andnot(l_o, r_z))
			    || !tarval_is_null(tarval_andnot(r_o, l_z))) {
				new_relation &= ~ir_relation_equal;
			}

			if (relation != new_relation) {
				dbg_info *dbgi = get_irn_dbg_info(irn);
				ir_node  *cmp  = new_rd_Cmp(dbgi, block, l, r, new_relation);
				mark_irn_visited(cmp);
				DB((dbg, LEVEL_2, "Simplified relation of %+F(%+F, %+F)\n", irn, l, r));
				set_bitinfo(cmp, z, o);
				exchange(irn, cmp);
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
			 * normalization */
			if (tarval_is_null(tarval_and(bl->z, br->z))) {
				dbg_info *dbgi     = get_irn_dbg_info(irn);
				ir_node  *block    = get_nodes_block(irn);
				ir_mode  *mode     = get_irn_mode(irn);
				ir_node  *new_node = new_rd_Add(dbgi, block, l, r, mode);
				mark_irn_visited(new_node);
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) normalized to Add\n", irn, l, r));
				set_bitinfo(new_node, z, o);
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
			 * normalization */
			if (tarval_is_null(tarval_and(bl->z, br->z))) {
				dbg_info *dbgi     = get_irn_dbg_info(irn);
				ir_node  *block    = get_nodes_block(irn);
				ir_mode  *mode     = get_irn_mode(irn);
				ir_node  *new_node = new_rd_Add(dbgi, block, l, r, mode);
				mark_irn_visited(new_node);
				DB((dbg, LEVEL_2, "%+F(%+F, %+F) normalized to Add\n", irn, l, r));
				set_bitinfo(new_node, z, o);
				exchange(irn, new_node);
				env->modified = 1;
			}

			break;
		}

		case iro_Shrs: {
			ir_mode *mode = get_irn_mode(irn);

			if (get_mode_arithmetic(mode) == irma_twos_complement) {
				unsigned mode_bits   = get_mode_size_bits(mode);
				unsigned highest_bit = get_tarval_highest_bit(z);
				if (highest_bit + 1U != mode_bits) {
					dbg_info *const dbgi     = get_irn_dbg_info(irn);
					ir_node  *const block    = get_nodes_block(irn);
					ir_node  *const l        = get_Shrs_left(irn);
					ir_node  *const r        = get_Shrs_right(irn);
					ir_node  *const new_node = new_rd_Shr(dbgi, block, l, r, mode);
					mark_irn_visited(new_node);
					DB((dbg, LEVEL_2, "%+F(%+F, %+F) normalized to Shr\n", irn, l, r));
					set_bitinfo(new_node, z, o);
					exchange(irn, new_node);
					env->modified = 1;
				}
			}

			break;
		}
	}
}

void fixpoint_vrp(ir_graph* const irg)
{
	assure_irg_properties(irg,
		IR_GRAPH_PROPERTY_NO_BADS
		| IR_GRAPH_PROPERTY_NO_UNREACHABLE_CODE
		| IR_GRAPH_PROPERTY_CONSISTENT_DOMINANCE
		| IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);

	struct obstack private_obst;
	obstack_init(&private_obst);

	ir_reserve_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	constbits_analyze(irg, &private_obst);

	FIRM_DBG_REGISTER(dbg, "firm.opt.fp-vrp");
	DB((dbg, LEVEL_1, "===> Performing constant propagation on %+F (optimization)\n", irg));

	DB((dbg, LEVEL_2, "---> Applying analysis results\n"));
	environment_t env = { .modified = 0 };
	irg_walk_graph(irg, NULL, apply_result, &env);

	ir_free_resources(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);

	obstack_free(&private_obst, NULL);

	confirm_irg_properties(irg,
		env.modified ? IR_GRAPH_PROPERTIES_NONE : IR_GRAPH_PROPERTIES_ALL);
}
