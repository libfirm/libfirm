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
#include "x86_address_mode.h"

#include "bearch.h"
#include "bediagnostic.h"
#include "beemitter.h"
#include "beirg.h"
#include "belive.h"
#include "benode.h"
#include "betranshlp.h"
#include "beutil.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irprintf.h"
#include <inttypes.h>

static bitset_t *non_address_mode_nodes;

static bool tarval_possible(ir_tarval *tv)
{
	ir_mode *mode = get_tarval_mode(tv);
	if (get_mode_size_bits(mode) <= 32) {
		assert(tarval_is_long(tv));
		return true;
	}

	if (!tarval_is_long(tv))
		return false;
	/* immediates on x86_64 are at most 32bit and get sign extended */
	long    val   = get_tarval_long(tv);
	int32_t val32 = (long)val;
	return val == (long)val32;
}

static bool eat_imm(x86_address_t *const addr, ir_node const *const node,
                    bool basereg_usable)
{
	switch (get_irn_opcode(node)) {
	case iro_Add:
		/* Add is supported as long as both operands are immediates. */
		return
			!x86_is_non_address_mode_node(node) &&
			eat_imm(addr, get_Add_left(node), basereg_usable) &&
			eat_imm(addr, get_Add_right(node), basereg_usable);

	case iro_Address:
		/* The first Address of a DAG can be folded into an immediate. */
		if (addr->imm.entity)
			return false;
		addr->imm.entity = get_Address_entity(node);
		addr->imm.kind = X86_IMM_ADDR;
		if (is_tls_entity(addr->imm.entity))
			addr->tls_segment = true;
		return true;

	case iro_Const: {
		/* Use the value for the offset. */
		if (addr->imm.offset != 0)
			return false;
		ir_tarval *const tv = get_Const_tarval(node);
		if (!tarval_possible(tv))
			return false;
		addr->imm.offset = get_tarval_long(tv);
		return true;
	}

	case iro_Unknown:
		/* Use '0' for Unknowns. */
		return true;

	default:
		if (be_is_Relocation(node)) {
			if (addr->imm.entity)
				return false;
			addr->imm.entity = be_get_Relocation_entity(node);
			x86_immediate_kind_t const kind
				= (x86_immediate_kind_t)be_get_Relocation_kind(node);
			addr->imm.kind = kind;
			if (kind == X86_IMM_GOTPCREL || kind == X86_IMM_PCREL) {
				if (!basereg_usable)
					return false;
				addr->ip_base = true;
			}
			return true;
		}
		/* All other nodes are no immediates. */
		return false;
	}
}

/**
 * Place a DAG with root @p node into an address mode.
 *
 * @param addr    the address mode data so far (only modified on success)
 * @param node    the node
 *
 * @return Whether the whole DAG at @p node could be matched as immediate.
 */
static bool eat_immediate(x86_address_t *const addr, ir_node const *const node,
                          bool basereg_usable)
{
	x86_address_t try_addr = *addr;
	if (eat_imm(&try_addr, node, basereg_usable)) {
		*addr = try_addr;
		return true;
	}
	return false;
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
                               x86_create_am_flags_t flags,
                               bool basereg_usable)
{
	if (!(flags & x86_create_am_force)
	    && x86_is_non_address_mode_node(node)
	    && (!(flags & x86_create_am_double_use) || get_irn_n_edges(node) > 2))
		return node;

	if (is_Add(node)) {
		ir_node *left  = get_Add_left(node);
		ir_node *right = get_Add_right(node);
		if (eat_immediate(addr, left, basereg_usable))
			return eat_immediates(addr, right, x86_create_am_normal,
			                      basereg_usable);
		if (eat_immediate(addr, right, basereg_usable))
			return eat_immediates(addr, left, x86_create_am_normal,
			                      basereg_usable);
	} else if (is_Member(node)) {
		assert(addr->imm.entity == NULL);
		addr->imm.entity = get_Member_entity(node);
		addr->imm.kind   = X86_IMM_FRAMEENT;
		ir_node *ptr = get_Member_ptr(node);
		assert(is_Start(get_Proj_pred(ptr)));
		return ptr;
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
	/* we can only eat a shl if we don't have a scale or index set yet */
	if (addr->scale != 0 || addr->index != NULL)
		return false;

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
			be_warningf(node, "found unoptimized Shl x,0");

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

	if (x86_is_non_address_mode_node(node))
		return false;

	addr->variant = X86_ADDR_INDEX;
	addr->scale   = val;
	addr->index   = shifted_val;
	return true;
}

static void add_addr_operand(x86_address_t *const addr, ir_node *const op)
{
	if (!op)
		return;
	ir_node *const base = addr->base;
	if (base) {
		addr->variant = X86_ADDR_BASE_INDEX;
		assert(!addr->index && addr->scale == 0);
		/* esp must be used as base */
		if (is_Proj(op) && is_Start(get_Proj_pred(op))) {
			addr->base  = op;
			addr->index = base;
		} else {
			addr->index = op;
		}
	} else {
		addr->variant = addr->index ? X86_ADDR_BASE_INDEX : X86_ADDR_BASE;
		addr->base    = op;
	}
}

void x86_create_address_mode(x86_address_t *addr, ir_node *node,
                             x86_create_am_flags_t flags)
{
	memset(addr, 0, sizeof(*addr));

	addr->imm.kind = X86_IMM_VALUE;
	if (eat_immediate(addr, node, true)) {
		addr->variant = addr->ip_base ? X86_ADDR_RIP : X86_ADDR_JUST_IMM;
		return;
	}

	assert(!addr->ip_base);
	if (!(flags & x86_create_am_force) && x86_is_non_address_mode_node(node)
	    && (!(flags & x86_create_am_double_use) || get_irn_n_edges(node) > 2)) {
		addr->variant = X86_ADDR_BASE;
		addr->base    = node;
		return;
	}

	ir_node *eat_imms = eat_immediates(addr, node, flags, false);
	if (eat_imms != node) {
		if (flags & x86_create_am_force)
			eat_imms = be_skip_downconv(eat_imms, true);

		node = eat_imms;
		if (x86_is_non_address_mode_node(node)) {
			addr->variant = X86_ADDR_BASE;
			addr->base    = node;
			return;
		}
	}

	/* starting point Add, Sub or Shl, FrameAddr */
	if (is_Shl(node)) {
		/* We don't want to eat add x, x as shl here, so only test for real Shl
		 * instructions, because we want the former as Lea x, x, not Shl x, 1 */
		if (eat_shl(addr, node)) {
			if (addr->scale == 1) {
				/* c(,x,2) -> c(x,x,1)
				 * The latter has no mandatory 4 byte offset. */
				addr->variant = X86_ADDR_BASE_INDEX;
				addr->scale   = 0;
				addr->base    = addr->index;
			}
			return;
		}
	} else if (eat_immediate(addr, node, true)) {
		/* we can hit this case in x86_create_am_force mode */
		addr->variant = addr->ip_base ? X86_ADDR_RIP : X86_ADDR_JUST_IMM;
		return;
	} else if (is_Add(node)) {
		ir_node *left  = get_Add_left(node);
		ir_node *right = get_Add_right(node);

		if (flags & x86_create_am_force) {
			left  = be_skip_downconv(left, true);
			right = be_skip_downconv(right, true);
		}
		left  = eat_immediates(addr, left, flags, false);
		right = eat_immediates(addr, right, flags, false);

		if (eat_shl(addr, left)) {
			left = NULL;
		} else if (eat_shl(addr, right)) {
			right = NULL;
		}

		/* (x & 0xFFFFFFFC) + (x >> 2) -> lea(x >> 2, x >> 2, 4) */
		if (left != NULL && right != NULL) {
			ir_node *and;
			ir_node *shr;
			if (is_And(left) && (is_Shr(right) || is_Shrs(right))) {
				and = left;
				shr = right;
				goto tryit;
			}
			if (is_And(right) && (is_Shr(left) || is_Shrs(left))) {
				and = right;
				shr = left;
tryit:
				if (get_And_left(and) == get_binop_left(shr)) {
					ir_node *and_right = get_And_right(and);
					ir_node *shr_right = get_binop_right(shr);

					if (is_Const(and_right) && is_Const(shr_right)) {
						ir_tarval *and_mask     = get_Const_tarval(and_right);
						ir_tarval *shift_amount = get_Const_tarval(shr_right);
						ir_mode   *mode         = get_irn_mode(and);
						ir_tarval *all_one      = get_mode_all_one(mode);
						ir_tarval *shift_mask   = tarval_shl(tarval_shr(all_one, shift_amount), shift_amount);
						long       val          = get_tarval_long(shift_amount);

						if (and_mask == shift_mask && val >= 0 && val <= 3) {
							addr->variant = X86_ADDR_BASE_INDEX;
							addr->base    = shr;
							addr->index   = shr;
							addr->scale   = val;
							return;
						}
					}
				}
			}
		}

		add_addr_operand(addr, left);
		add_addr_operand(addr, right);
		return;
	}

	addr->variant = X86_ADDR_BASE;
	addr->base    = node;
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

/**
 * Walker: mark those nodes that cannot be part of an address mode because
 * their value must be accessed through a register
 */
static void mark_non_address_nodes(ir_node *node, void *env)
{
	if (is_Load(node)) {
		/* Nothing to do. Especially, do not mark the pointer, because we want to
		 * turn it into AM. */
		return;
	}
	if (is_Store(node)) {
		/* Do not mark the pointer, because we want to turn it into AM. */
		ir_node *val = get_Store_value(node);
		x86_mark_non_am(val);
		return;
	}

	ir_mode *mode = get_irn_mode(node);
	if (!be_mode_needs_gp_reg(mode) && mode != mode_b)
		return;

	switch (get_irn_opcode(node)) {
	case iro_Shl:
	case iro_Add: {
		/* only 1 user: AM folding is always beneficial */
		if (get_irn_n_edges(node) <= 1)
			break;

		/* For adds and shls with multiple users we use this heuristic:
		 * we do not fold them into address mode if their operands do not live
		 * out of the block, because in this case we will reduce register
		 * pressure. Otherwise, we fold them aggressively in the hope that
		 * the node itself does not exist anymore and we were able to save the
		 * register for the result */
		ir_node *left  = get_binop_left(node);
		ir_node *right = get_binop_right(node);

		/* if any of the operands is an immediate then this will not
		 * increase register pressure */
		x86_address_t addr;
		memset(&addr, 0, sizeof(addr));
		if (eat_immediate(&addr, left, false)
		 || eat_immediate(&addr, right, false))
			return;

		/* Fold AM if any of the two operands does not die here. This duplicates
		 * an addition and has the same register pressure for the case that only
		 * one operand dies, but is faster (on Pentium 4).
		 * && instead of || only folds AM if both operands do not die here */
		be_lv_t *lv = (be_lv_t*)env;
		if (!value_last_used_here(lv, node, left)
		 || !value_last_used_here(lv, node, right)) {
			return;
		}

		/* At least one of left and right are not used by anyone else, so it is
		 * beneficial for the register pressure (if both are unused otherwise,
		 * else neutral) and ALU use to not fold AM. */
		x86_mark_non_am(node);
		break;
	}

	default:
		foreach_irn_in(node, i, in) {
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

char const *x86_get_addr_variant_str(x86_addr_variant_t const variant)
{
	switch (variant) {
	case X86_ADDR_REG:        return "reg";
	case X86_ADDR_JUST_IMM:   return "immediate";
	case X86_ADDR_BASE:       return "base";
	case X86_ADDR_BASE_INDEX: return "base+index";
	case X86_ADDR_INDEX:      return "index";
	case X86_ADDR_RIP:        return "rip";
	case X86_ADDR_INVALID:
		break;
	}
	return "<BAD>";
}

static void emit_register(arch_register_t const *const reg)
{
	be_emit_char('%');
	be_emit_string(reg->name);
	assert(!reg->is_virtual);
}

void x86_emit_addr(ir_node const *const node, x86_addr_t const *const addr)
{
	switch (addr->segment) {
	case X86_SEGMENT_DEFAULT:
		break;
	case X86_SEGMENT_CS: be_emit_cstring("%cs:"); break;
	case X86_SEGMENT_SS: be_emit_cstring("%ss:"); break;
	case X86_SEGMENT_DS: be_emit_cstring("%ds:"); break;
	case X86_SEGMENT_ES: be_emit_cstring("%es:"); break;
	case X86_SEGMENT_FS: be_emit_cstring("%fs:"); break;
	case X86_SEGMENT_GS: be_emit_cstring("%gs:"); break;
	}


	/* emit offset */
	x86_addr_variant_t const variant = addr->variant;
	int32_t            const offset  = addr->immediate.offset;
	ir_entity   const *const entity  = addr->immediate.entity;
	assert(variant != X86_ADDR_INVALID);
	if (entity) {
		x86_emit_relocation_no_offset(addr->immediate.kind, entity);
		if (offset != 0)
			be_emit_irprintf("%+"PRId32, offset);
	} else if (offset != 0 || variant == X86_ADDR_JUST_IMM) {
		assert(addr->immediate.kind == X86_IMM_VALUE);
		/* also handle special case if nothing is set */
		be_emit_irprintf("%"PRId32, offset);
	}

	if (variant != X86_ADDR_JUST_IMM) {
		be_emit_char('(');

		if (variant == X86_ADDR_RIP) {
			be_emit_cstring("%rip");
		} else {
			if (x86_addr_variant_has_base(variant)) {
				arch_register_t const *const reg
					= arch_get_irn_register_in(node, addr->base_input);
				emit_register(reg);
			}

			if (x86_addr_variant_has_index(variant)) {
				be_emit_char(',');
				arch_register_t const *const reg
					= arch_get_irn_register_in(node, addr->index_input);
				emit_register(reg);

				unsigned const log_scale = addr->log_scale;
				if (log_scale > 0)
					be_emit_irprintf(",%u", 1u << log_scale);
			}
		}
		be_emit_char(')');
	}
}
