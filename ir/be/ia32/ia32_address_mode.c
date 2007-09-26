/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief       This file contains functions for matching firm graphs for
 *              nodes that can be used as addressmode for x86 commands
 * @author      Matthias Braun
 * @version     $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "ia32_address_mode.h"
#include "ia32_transform.h"

#include "irtypes.h"
#include "irnode_t.h"
#include "irprintf.h"
#include "error.h"
#include "iredges_t.h"
#include "irgwalk.h"

#include "../benode_t.h"

#define AGGRESSIVE_AM

/* gas/ld don't support negative symconsts :-( */
#undef SUPPORT_NEGATIVE_SYMCONSTS

static bitset_t *non_address_mode_nodes;

static int do_is_immediate(const ir_node *node, int *symconsts, int negate)
{
	ir_node *left;
	ir_node *right;

	switch(get_irn_opcode(node)) {
	case iro_Const:
		if(!tarval_is_long(get_Const_tarval(node))) {
#ifdef DEBUG_libfirm
			ir_fprintf(stderr, "Optimisation warning tarval of %+F(%+F) is not "
			           "a long.\n", node, current_ir_graph);
#endif
			return 0;
		}
		return 1;
	case iro_SymConst:
#ifndef SUPPORT_NEGATIVE_SYMCONSTS
		/* unfortunately the assembler/linker doesn't support -symconst */
		if(negate)
			return 0;
#endif

		if(get_SymConst_kind(node) != symconst_addr_ent)
			return 0;
		(*symconsts)++;
		if(*symconsts > 1)
			return 0;

		return 1;
	case iro_Add:
	case iro_Sub:
		if(bitset_is_set(non_address_mode_nodes, get_irn_idx(node)))
			return 0;

		left  = get_binop_left(node);
		right = get_binop_right(node);
		if(!do_is_immediate(left, symconsts, negate))
			return 0;
		if(!do_is_immediate(right, symconsts, is_Sub(node) ? !negate : negate))
			return 0;

		return 1;
	default:
		break;
	}

	return 0;
}

static int is_immediate_simple(const ir_node *node)
{
	int symconsts = 0;
	return do_is_immediate(node, &symconsts, 0);
}

static int is_immediate(ia32_address_t *addr, const ir_node *node, int negate)
{
	int symconsts = 0;
	if(addr->symconst_ent != NULL)
		symconsts = 1;

	return do_is_immediate(node, &symconsts, negate);
}

static void eat_immediate(ia32_address_t *addr, ir_node *node, int negate)
{
	tarval  *tv;
	ir_node *left;
	ir_node *right;
  long val;

	switch(get_irn_opcode(node)) {
	case iro_Const:
		tv = get_Const_tarval(node);
		val = get_tarval_long(tv);
		if(negate) {
			addr->offset -= val;
		} else {
			addr->offset += val;
		}
		break;
	case iro_SymConst:
		if(addr->symconst_ent != NULL) {
			panic("Internal error: more than 1 symconst in address "
			      "calculation");
		}
		addr->symconst_ent  = get_SymConst_entity(node);
#ifndef SUPPORT_NEGATIVE_SYMCONSTS
		assert(!negate);
#endif
		addr->symconst_sign = negate;
		break;
	case iro_Add:
		assert(!bitset_is_set(non_address_mode_nodes, get_irn_idx(node)));
		left  = get_Add_left(node);
		right = get_Add_right(node);
		eat_immediate(addr, left, negate);
		eat_immediate(addr, right, negate);
		break;
	case iro_Sub:
		assert(!bitset_is_set(non_address_mode_nodes, get_irn_idx(node)));
		left  = get_Sub_left(node);
		right = get_Sub_right(node);
		eat_immediate(addr, left, negate);
		eat_immediate(addr, right, !negate);
		break;
	default:
		panic("Internal error in immediate address calculation");
	}
}

static ir_node *eat_immediates(ia32_address_t *addr, ir_node *node, int force)
{
	if(!force && bitset_is_set(non_address_mode_nodes, get_irn_idx(node)))
		return node;

	if(is_Add(node)) {
		ir_node *left  = get_Add_left(node);
		ir_node *right = get_Add_right(node);

		if(is_immediate(addr, left, 0)) {
			eat_immediate(addr, left, 0);
			return eat_immediates(addr, right, 0);
		}
		if(is_immediate(addr, right, 0)) {
			eat_immediate(addr, right, 0);
			return eat_immediates(addr, left, 0);
		}
	} else if(is_Sub(node)) {
		ir_node *left  = get_Sub_left(node);
		ir_node *right = get_Sub_right(node);

		if(is_immediate(addr, right, 1)) {
			eat_immediate(addr, right, 1);
			return eat_immediates(addr, left, 0);
		}
	}

	return node;
}

static int eat_shl(ia32_address_t *addr, ir_node *node)
{
	ir_node *right = get_Shl_right(node);
	tarval  *tv;
	long     val;

	/* we can only eat a shl if we don't have a scale or index set */
	if(addr->scale != 0 || addr->index != NULL)
		return 0;

	/* we can use shl with 1, 2 or 3 shift */
	if(!is_Const(right))
		return 0;
	tv = get_Const_tarval(right);
	if(!tarval_is_long(tv))
		return 0;
	val = get_tarval_long(tv);
	if(val < 0 || val > 3)
		return 0;
	if(val == 0) {
		ir_fprintf(stderr, "Optimisation warning: unoptimized Shl(,0) found\n");
	}
	if(bitset_is_set(non_address_mode_nodes, get_irn_idx(node)))
		return 0;

#ifndef AGGRESSIVE_AM
	if(get_irn_n_edges(node) > 1)
		return 0;
#endif

	addr->scale = val;
	addr->index = eat_immediates(addr, get_Shl_left(node), 0);
	return 1;
}

void ia32_create_address_mode(ia32_address_t *addr, ir_node *node, int force)
{
	int      res = 0;
	ir_node *eat_imms;

	if(is_immediate(addr, node, 0)) {
		eat_immediate(addr, node, 0);
		return;
	}

#ifndef AGGRESSIVE_AM
	if(!force && get_irn_n_edges(node) > 1) {
		addr->base = node;
		return;
	}
#endif

	if(!force && bitset_is_set(non_address_mode_nodes, get_irn_idx(node))) {
		addr->base = node;
		return;
	}

	eat_imms = eat_immediates(addr, node, force);
	if(eat_imms != node) {
		res  = 1;
		node = eat_imms;
#ifndef AGGRESSIVE_AM
		if(get_irn_n_edges(node) > 1) {
			addr->base = node;
			return;
		}
#endif
		if(bitset_is_set(non_address_mode_nodes, get_irn_idx(node))) {
			addr->base = node;
			return;
		}
	}

	/* starting point Add, Sub or Shl, FrameAddr */
	if(is_Shl(node)) {
		if(eat_shl(addr, node))
			return;
	} else if(is_immediate(addr, node, 0)) {
		eat_immediate(addr, node, 0);
		return;
	} else if(be_is_FrameAddr(node)) {
		assert(addr->base == NULL);
		assert(addr->frame_entity == NULL);
		addr->base         = be_get_FrameAddr_frame(node);
		addr->use_frame    = 1;
		addr->frame_entity = be_get_FrameAddr_entity(node);
		return;
	} else if(is_Add(node)) {
		ir_node *left  = get_Add_left(node);
		ir_node *right = get_Add_right(node);
		assert(force || !is_immediate(addr, left, 0));
		assert(force || !is_immediate(addr, right, 0));

		if(is_Shl(left) && eat_shl(addr, left)) {
			left = NULL;
		} else if(is_Shl(right) && eat_shl(addr, right)) {
			right = NULL;
		}
		if(left != NULL && be_is_FrameAddr(left)
				&& !bitset_is_set(non_address_mode_nodes, get_irn_idx(left))) {
			assert(addr->base == NULL);
			assert(addr->frame_entity == NULL);
			addr->base         = be_get_FrameAddr_frame(left);
			addr->use_frame    = 1;
			addr->frame_entity = be_get_FrameAddr_entity(left);
			left               = NULL;
		} else if(right != NULL && be_is_FrameAddr(right)
				&& !bitset_is_set(non_address_mode_nodes, get_irn_idx(right))) {
			assert(addr->base == NULL);
			assert(addr->frame_entity == NULL);
			addr->base         = be_get_FrameAddr_frame(right);
			addr->use_frame    = 1;
			addr->frame_entity = be_get_FrameAddr_entity(right);
			right              = NULL;
		}

		if(left != NULL) {
			if(addr->base != NULL) {
				assert(addr->index == NULL && addr->scale == 0);
				assert(right == NULL);
				addr->index = left;
			} else {
				addr->base = left;
			}
		}
		if(right != NULL) {
			if(addr->base == NULL) {
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



static void mark_non_address_nodes(ir_node *node, void *env)
{
	int i, arity;
	ir_node *ptr;
	ir_node *mem;
	ir_node *val;
	ir_node *left;
	ir_node *right;
	(void) env;

	switch(get_irn_opcode(node)) {
	case iro_Load:
		ptr = get_Load_ptr(node);
		mem = get_Load_mem(node);

		bitset_set(non_address_mode_nodes, get_irn_idx(mem));
		break;

	case iro_Store:
		val = get_Store_value(node);
		ptr = get_Store_ptr(node);
		mem = get_Store_mem(node);

		bitset_set(non_address_mode_nodes, get_irn_idx(val));
		bitset_set(non_address_mode_nodes, get_irn_idx(mem));
		break;

	case iro_Add:
		left  = get_Add_left(node);
		right = get_Add_right(node);
		/* if we can do source address mode then we will never fold the add
		 * into address mode */
		if(!mode_is_float(get_irn_mode(node)) && (is_immediate_simple(right) ||
			 (!use_source_address_mode(get_nodes_block(node), left, right)
		     && !use_source_address_mode(get_nodes_block(node), right, left))))
		{
		    break;
		}
		bitset_set(non_address_mode_nodes, get_irn_idx(node));
		/* fallthrough */

	default:
		arity = get_irn_arity(node);

		for(i = 0; i < arity; ++i) {
			ir_node *in = get_irn_n(node, i);
			bitset_set(non_address_mode_nodes, get_irn_idx(in));
		}
		break;
	}
}

void calculate_non_address_mode_nodes(ir_graph *irg)
{
	non_address_mode_nodes = bitset_malloc(get_irg_last_idx(irg));

	irg_walk_graph(irg, NULL, mark_non_address_nodes, NULL);
}

void free_non_address_mode_nodes(void)
{
	bitset_free(non_address_mode_nodes);
}
