/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Access function for backedges.
 * @author    Goetz Lindenmaier
 * @date      7.2002
 */
#include "irbackedge_t.h"

#include "array.h"
#include "irgraph_t.h"
#include "irnode_t.h"

/*--------------------------------------------------------------------*/
/* Backedge information.                                              */
/*--------------------------------------------------------------------*/


/**
 * Returns backarray if the node can have backedges, else returns
 * NULL.
 *
 * Does not assert whether the backarray is correct -- use
 * very careful!
 */
static bitset_t *mere_get_backarray(const ir_node *n)
{
	switch (get_irn_opcode(n)) {
	case iro_Block:
		if (!get_Block_matured(n))
			return NULL;

		assert(n->attr.block.backedge != NULL);
		return n->attr.block.backedge;
	case iro_Phi:
		assert(n->attr.phi.u.backedge != NULL);
		return n->attr.phi.u.backedge;
	default:
		break;
	}
	return NULL;
}

/**
 * Returns backarray if the node can have backedges, else returns
 * NULL.
 */
static bitset_t *get_backarray(const ir_node *n)
{
	bitset_t *ba = mere_get_backarray(n);
#ifndef NDEBUG
	if (ba != NULL) {
		size_t bal = bitset_size(ba);  /* avoid macro expansion in assertion. */
		size_t inl = get_irn_arity(n);
		assert(bal == inl);
	}
#endif

	return ba;
}

#ifndef NDEBUG
/**
 * Returns non-zero if node has no backarray, or
 *                  if size of backarray == size of in array.
 */
static int legal_backarray(const ir_node *n)
{
	bitset_t *ba = mere_get_backarray(n);
	if (ba && (bitset_size(ba) != (unsigned) get_irn_arity(n)))
		return 0;
	return 1;
}
#endif

void fix_backedges(struct obstack *obst, ir_node *n)
{
	bitset_t *arr = mere_get_backarray(n);
	if (arr == NULL)
		return;

	int arity = get_irn_arity(n);
	if (bitset_size(arr) != (unsigned) arity) {
		arr = new_backedge_arr(obst, arity);

		unsigned opc = get_irn_opcode(n);
		if (opc == iro_Phi) {
			n->attr.phi.u.backedge = arr;
		} else if (opc == iro_Block) {
			n->attr.block.backedge = arr;
		}
	}

	assert(legal_backarray(n));
}

int is_backedge(const ir_node *n, int pos)
{
	bitset_t *ba = get_backarray(n);
	if (ba != NULL)
		return bitset_is_set(ba, pos);
	return 0;
}

void set_backedge(ir_node *n, int pos)
{
	bitset_t *ba = get_backarray(n);
	assert(ba != NULL);
	bitset_set(ba, pos);
}

int has_backedges(const ir_node *n)
{
	bitset_t *ba = get_backarray(n);
	if (ba != NULL) {
		return !bitset_is_empty(ba);
	}
	return 0;
}

void clear_backedges(ir_node *n)
{
	bitset_t *ba = get_backarray(n);
	if (ba != NULL) {
		bitset_clear_all(ba);
	}
}

bitset_t *new_backedge_arr(struct obstack *obst, size_t size)
{
	return bitset_obstack_alloc(obst, size);
}
