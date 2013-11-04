/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file contains functions for matching firm graphs for
 *              nodes that can be used as address mode for x86 instructions
 * @author      Matthias Braun
 */
#include "beirg.h"
#include "beutil.h"
#include "x86_address_mode.h"

#include "irtypes.h"
#include "irnode_t.h"
#include "irprintf.h"
#include "error.h"
#include "iredges_t.h"
#include "irgwalk.h"

#include "benode.h"
#include "belive.h"

#define AGGRESSIVE_AM

static bitset_t *non_address_mode_nodes;

/**
 * Recursive worker for checking if a DAG with root node can be represented as
 * a simple immediate.
 *
 * @param node      the node
 * @param entities  number of entities found so far
 * @param negate    if set, the immediate must be negated
 *
 * @return non-zero if the DAG represents an immediate, 0 else
 */
static bool do_is_immediate(const ir_node *node, int *entities, bool negate)
{
	switch (get_irn_opcode(node)) {
	case iro_Const:
		/* Consts are typically immediates */
		if (!tarval_is_long(get_Const_tarval(node))) {
#ifdef DEBUG_libfirm
			ir_fprintf(stderr,
			           "Optimisation warning tarval of %+F(%+F) is not a long.\n",
			           node, get_irn_irg(node));
#endif
			return false;
		}
		return true;
	case iro_Address:
		/* the first Address of a DAG can be folded into an immediate */
		/* unfortunately the assembler/linker doesn't support -entity */
		if (negate)
			return false;
		if (++*entities > 1)
			return false;
		return true;

	case iro_Unknown:
		/* we can use '0' for Unknowns */
		return true;
	case iro_Add:
	case iro_Sub:
		/* Add's and Sub's are typically supported as long as both operands are
		 * immediates */
		if (x86_is_non_address_mode_node(node))
			return false;

		ir_node *left = get_binop_left(node);
		if (!do_is_immediate(left, entities, negate))
			return false;
		ir_node *right = get_binop_right(node);
		if (!do_is_immediate(right, entities, is_Sub(node) ? !negate : negate))
			return false;

		return true;
	default:
		/* all other nodes are NO immediates */
		return false;
	}
}

/**
 * Check if a DAG starting with root node can be folded into an address mode
 * as an immediate.
 *
 * @param addr    the address mode data so far
 * @param node    the node
 * @param negate  if set, the immediate must be negated
 */
static int is_immediate(x86_address_t *addr, const ir_node *node, bool negate)
{
	int entities = addr->entity != NULL;
	return do_is_immediate(node, &entities, negate);
}

/**
 * Place a DAG with root node into an address mode.
 *
 * @param addr    the address mode data so far
 * @param node    the node
 * @param negate  if set, the immediate must be negated
 */
static void eat_immediate(x86_address_t *addr, ir_node *node, bool negate)
{
	switch (get_irn_opcode(node)) {
	case iro_Const: {
		/* simply add the value to the offset */
		ir_tarval *tv  = get_Const_tarval(node);
		long       val = get_tarval_long(tv);
		if (negate) {
			addr->offset -= val;
		} else {
			addr->offset += val;
		}
		break;
	}
	case iro_Address:
		/* place the entity into the immediate */
		if (addr->entity != NULL) {
			panic("Internal error: more than 1 entity in address calculation");
		}
		addr->entity = get_Address_entity(node);
		if (is_tls_entity(addr->entity))
			addr->tls_segment = true;
		assert(!negate);
		break;
	case iro_Unknown:
		break;
	case iro_Add: {
		assert(!x86_is_non_address_mode_node(node));
		ir_node *left = get_Add_left(node);
		eat_immediate(addr, left, negate);
		ir_node *right = get_Add_right(node);
		eat_immediate(addr, right, negate);
		break;
	}
	case iro_Sub: {
		assert(!x86_is_non_address_mode_node(node));
		ir_node *left = get_Sub_left(node);
		eat_immediate(addr, left, negate);
		ir_node *right = get_Sub_right(node);
		eat_immediate(addr, right, !negate);
		break;
	}
	default:
		panic("Internal error in immediate address calculation");
	}
}

/**
 * Place operands of node into an address mode.
 *
 * @param addr    the address mode data so far
 * @param node    the node
 * @param flags   the flags
 *
 * @return the folded node
 */
static ir_node *eat_immediates(x86_address_t *addr, ir_node *node,
                               x86_create_am_flags_t flags)
{
	if (!(flags & x86_create_am_force)
	    && x86_is_non_address_mode_node(node)
	    && (!(flags & x86_create_am_double_use) || get_irn_n_edges(node) > 2))
		return node;

	if (is_Add(node)) {
		ir_node *left  = get_Add_left(node);
		ir_node *right = get_Add_right(node);

		if (is_immediate(addr, left, 0)) {
			eat_immediate(addr, left, 0);
			return eat_immediates(addr, right, x86_create_am_normal);
		}
		if (is_immediate(addr, right, 0)) {
			eat_immediate(addr, right, 0);
			return eat_immediates(addr, left, x86_create_am_normal);
		}
	} else if (is_Sub(node)) {
		ir_node *left  = get_Sub_left(node);
		ir_node *right = get_Sub_right(node);

		if (is_immediate(addr, right, 1)) {
			eat_immediate(addr, right, 1);
			return eat_immediates(addr, left, x86_create_am_normal);
		}
	}

	return node;
}

/**
 * Try to place a Shl into an address mode.
 *
 * @param addr    the address mode data so far
 * @param node   the node to place
 * @return true on success
 */
static bool eat_shl(x86_address_t *addr, ir_node *node)
{
	ir_node *shifted_val;
	long     val;
	if (is_Shl(node)) {
		/* we can use shl with 0,1,2 or 3 shift */
		ir_node *right = get_Shl_right(node);
		if (!is_Const(right))
			return false;
		ir_tarval *tv = get_Const_tarval(right);
		if (!tarval_is_long(tv))
			return false;

		val = get_tarval_long(tv);
		if (val < 0 || val > 3)
			return false;
		if (val == 0)
			ir_fprintf(stderr,
			           "Optimisation warning: unoptimized Shl(,0) found\n");

		shifted_val = get_Shl_left(node);
	} else if (is_Add(node)) {
		/* might be an add x, x */
		ir_node *left  = get_Add_left(node);
		ir_node *right = get_Add_right(node);
		if (left != right)
			return false;
		if (is_Const(left))
			return false;

		val         = 1;
		shifted_val = left;
	} else {
		return false;
	}

	/* we can only eat a shl if we don't have a scale or index set yet */
	if (addr->scale != 0 || addr->index != NULL)
		return false;
	if (x86_is_non_address_mode_node(node))
		return false;

#ifndef AGGRESSIVE_AM
	if (get_irn_n_edges(node) > 1)
		return false;
#endif

	addr->scale = val;
	addr->index = shifted_val;
	return true;
}

static void set_frame_addr(x86_address_t *const addr, ir_node *const frame)
{
	assert(!addr->base);
	assert(!addr->frame_entity);
	addr->base         = be_get_FrameAddr_frame(frame);
	addr->frame_entity = be_get_FrameAddr_entity(frame);
	addr->use_frame    = 1;
}

static bool is_downconv(const ir_node *node)
{
	if (!is_Conv(node))
		return false;

	ir_mode *dest_mode = get_irn_mode(node);
	if (get_mode_arithmetic(dest_mode) != irma_twos_complement)
		return false;
	ir_mode *src_mode = get_irn_mode(get_Conv_op(node));
	if (get_mode_arithmetic(src_mode) != irma_twos_complement)
		return false;
	return get_mode_size_bits(dest_mode) <= get_mode_size_bits(src_mode);
}

static ir_node *skip_downconv(ir_node *node)
{
	while (is_downconv(node)) {
		if (get_irn_n_edges(node) > 1)
			break;
		node = get_Conv_op(node);
	}
	return node;
}

void x86_create_address_mode(x86_address_t *addr, ir_node *node,
                             x86_create_am_flags_t flags)
{
	if (is_immediate(addr, node, 0)) {
		eat_immediate(addr, node, 0);
		return;
	}

#ifndef AGGRESSIVE_AM
	if (!(flags & x86_create_am_force) && get_irn_n_edges(node) > 1) {
		addr->base = node;
		return;
	}
#endif

	if (!(flags & x86_create_am_force)
	    && x86_is_non_address_mode_node(node)
	    && (!(flags & x86_create_am_double_use) || get_irn_n_edges(node) > 2)) {
		addr->base = node;
		return;
	}

	ir_node *eat_imms = eat_immediates(addr, node, flags);
	if (eat_imms != node) {
		if (flags & x86_create_am_force) {
			eat_imms = skip_downconv(eat_imms);
		}

		node = eat_imms;
#ifndef AGGRESSIVE_AM
		if (get_irn_n_edges(node) > 1) {
			addr->base = node;
			return;
		}
#endif
		if (x86_is_non_address_mode_node(node)) {
			addr->base = node;
			return;
		}
	}

	/* starting point Add, Sub or Shl, FrameAddr */
	if (is_Shl(node)) {
		/* We don't want to eat add x, x as shl here, so only test for real Shl
		 * instructions, because we want the former as Lea x, x, not Shl x, 1 */
		if (eat_shl(addr, node))
			return;
	} else if (is_immediate(addr, node, 0)) {
		eat_immediate(addr, node, 0);
		return;
	} else if (be_is_FrameAddr(node)) {
		set_frame_addr(addr, node);
		return;
	} else if (is_Add(node)) {
		ir_node *left  = get_Add_left(node);
		ir_node *right = get_Add_right(node);

		if (flags & x86_create_am_force) {
			left  = skip_downconv(left);
			right = skip_downconv(right);
		}

		assert(flags & x86_create_am_force || !is_immediate(addr, left,  0));
		assert(flags & x86_create_am_force || !is_immediate(addr, right, 0));

		if (eat_shl(addr, left)) {
			left = NULL;
		} else if (eat_shl(addr, right)) {
			right = NULL;
		}
		if (left != NULL && be_is_FrameAddr(left)
		    && !x86_is_non_address_mode_node(left)) {
			set_frame_addr(addr, left);
			left = NULL;
		} else if (right != NULL && be_is_FrameAddr(right)
		           && !x86_is_non_address_mode_node(right)) {
			set_frame_addr(addr, right);
			right = NULL;
		}

		if (left != NULL) {
			if (addr->base != NULL) {
				assert(addr->index == NULL && addr->scale == 0);
				assert(right == NULL);
				addr->index = left;
			} else {
				addr->base = left;
			}
		}
		if (right != NULL) {
			if (addr->base == NULL) {
				addr->base = right;
			} else {
				assert(addr->index == NULL && addr->scale == 0);
				addr->index = right;
			}
		}
		return;
	}

	addr->base = node;
}

void x86_mark_non_am(ir_node *node)
{
	bitset_set(non_address_mode_nodes, get_irn_idx(node));
}

bool x86_is_non_address_mode_node(ir_node const *node)
{
	return bitset_is_set(non_address_mode_nodes, get_irn_idx(node));
}

/**
 * Check if a given value is last used (i.e. die after) the block of some
 * other node.
 */
static bool value_last_used_here(be_lv_t *lv, ir_node *here, ir_node *value)
{
	ir_node *block = get_nodes_block(here);

	/* If the value is live end it is for sure it does not die here */
	if (be_is_live_end(lv, block, value))
		return false;

	/* if multiple nodes in this block use the value, then we cannot decide
	 * whether the value will die here (because there is no schedule yet).
	 * Assume it does not die in this case. */
	foreach_out_edge(value, edge) {
		ir_node *user = get_edge_src_irn(edge);
		if (user != here && get_nodes_block(user) == block) {
			return false;
		}
	}

	return true;
}

static bool simple_is_immediate(const ir_node *node)
{
	int entities = 0;
	return do_is_immediate(node, &entities, false);
}

/**
 * Walker: mark those nodes that cannot be part of an address mode because
 * their value must be accessed through a register
 */
static void mark_non_address_nodes(ir_node *node, void *env)
{
	be_lv_t *lv = (be_lv_t*)env;

	ir_mode *mode = get_irn_mode(node);
	if (!mode_is_int(mode) && !mode_is_reference(mode) && mode != mode_b)
		return;

	switch (get_irn_opcode(node)) {
	case iro_Load:
		/* Nothing to do. especially do not mark the pointer, because we want to
		 * turn it into AM. */
		break;

	case iro_Store: {
		/* Do not mark the pointer, because we want to turn it into AM. */
		ir_node *val = get_Store_value(node);
		x86_mark_non_am(val);
		break;
	}

	case iro_Shl:
	case iro_Add: {
		/* only 1 user: AM folding is always beneficial */
		if (get_irn_n_edges(node) <= 1)
			break;

		/* for adds and shls with multiple users we use this heuristic:
		 * we do not fold them into address mode if their operands don't live
		 * out of the block, because in this case we will reduce register
		 * pressure. Otherwise we fold them in aggressively in the hope, that
		 * the node itself doesn't exist anymore and we were able to save the
		 * register for the result */
		ir_node *left  = get_binop_left(node);
		ir_node *right = get_binop_right(node);

		/* if any of the operands is an immediate then this will not
		 * increase register pressure */
		if (simple_is_immediate(left) || simple_is_immediate(right))
			return;

		/* Fold AM if any of the two operands does not die here. This duplicates
		 * an addition and has the same register pressure for the case that only
		 * one operand dies, but is faster (on Pentium 4).
		 * && instead of || only folds AM if both operands do not die here */
		if (!value_last_used_here(lv, node, left) ||
		    !value_last_used_here(lv, node, right)) {
			return;
		}

		/* At least one of left and right are not used by anyone else, so it is
		 * beneficial for the register pressure (if both are unused otherwise,
		 * else neutral) and ALU use to not fold AM. */
		x86_mark_non_am(node);
		break;
	}

	default:
		for (int i = 0, arity = get_irn_arity(node); i < arity; ++i) {
			ir_node *in = get_irn_n(node, i);
			x86_mark_non_am(in);
		}
		break;
	}
}

void x86_calculate_non_address_mode_nodes(ir_graph *irg)
{
	be_assure_live_chk(irg);
	be_lv_t *lv = be_get_irg_liveness(irg);

	non_address_mode_nodes = bitset_malloc(get_irg_last_idx(irg));

	irg_walk_graph(irg, NULL, mark_non_address_nodes, lv);
}

void x86_free_non_address_mode_nodes(void)
{
	free(non_address_mode_nodes);
}
