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
 * @author    Matthias Braun
 * @date      30.03.2007
 * @brief     A nodeset. This should be prefered over a simple pset, because it
 *            tries to guarantee deterministic behavior. (and is faster)
 * @version   $Id$
 * @note      Actually the bits to make the behaviour deterministic are not
 *            implemented yet...
 */
#ifndef _FIRM_IRNODESET_H_
#define _FIRM_IRNODESET_H_

#include "firm_config.h"

#include "firm_types.h"
#include "xmalloc.h"

/*
 * sebastian experimental:
 * use ordered arrays as node sets.
 * the guys here have made good experiences with that.
 * Internally we use normal Firm arrays and binary
 * search for locating the elements. Using arrays should
 * give the sets a small footprint.
 */
#undef  IR_NODESET_USE_ORDERED_SETS

#define HashSet          ir_nodeset_t
#define HashSetIterator  ir_nodeset_iterator_t
#define ValueType        ir_node*
#define DO_REHASH

#ifdef IR_NODESET_USE_ORDERED_SETS
#include "arrayset.h"
#else
#include "hashset.h"
#endif

#undef DO_REHASH
#undef ValueType
#undef HashSetIterator
#undef HashSet

typedef struct ir_nodeset_t          ir_nodeset_t;
typedef struct ir_nodeset_iterator_t ir_nodeset_iterator_t;

/**
 * Initializes a nodeset with default size.
 *
 * @param nodeset      Pointer to allocated space for the nodeset
 */
void ir_nodeset_init(ir_nodeset_t *nodeset);

/**
 * Initializes a nodeset
 *
 * @param nodeset             Pointer to allocated space for the nodeset
 * @param expected_elements   Number of elements expected in the nodeset (roughly)
 */
void ir_nodeset_init_size(ir_nodeset_t *nodeset, size_t expected_elements);

/**
 * Destroys a nodeset and frees the memory allocated for hashtable. The memory of
 * the nodeset itself is not freed.
 *
 * @param nodeset   Pointer to the nodeset
 */
void ir_nodeset_destroy(ir_nodeset_t *nodeset);

/**
 * Allocates memory for a nodeset and initializes the set.
 *
 * @param expected_elements   Number of elements expected in the nodeset (roughly)
 * @return The initialized nodeset
 */
static INLINE ir_nodeset_t *ir_nodeset_new(size_t expected_elements) {
	ir_nodeset_t *res = xmalloc(sizeof(*res));
	ir_nodeset_init_size(res, expected_elements);
	return res;
}

/**
 * Destroys a nodeset and frees the memory of the nodeset itself.
 */
static INLINE void ir_nodeset_del(ir_nodeset_t *nodeset) {
	ir_nodeset_destroy(nodeset);
	xfree(nodeset);
}

/**
 * Inserts a node into a nodeset.
 *
 * @param nodeset   Pointer to the nodeset
 * @param node      node to insert into the nodeset
 * @returns         1 if the element has been inserted,
 *                  0 if it was already there
 */
int ir_nodeset_insert(ir_nodeset_t *nodeset, ir_node *node);


/**
 * Removes a node from a nodeset. Does nothing if the nodeset doesn't contain
 * the node.
 *
 * @param nodeset  Pointer to the nodeset
 * @param node     Node to remove from the nodeset
 */
void ir_nodeset_remove(ir_nodeset_t *nodeset, const ir_node *node);

/**
 * Tests whether a nodeset contains a specific node
 *
 * @param nodeset   Pointer to the nodeset
 * @param node      The pointer to find
 * @returns         1 if nodeset contains the node, 0 else
 */
int ir_nodeset_contains(const ir_nodeset_t *nodeset, const ir_node *node);

/**
 * Returns the number of pointers contained in the nodeset
 *
 * @param nodeset   Pointer to the nodeset
 * @returns       Number of pointers contained in the nodeset
 */
size_t ir_nodeset_size(const ir_nodeset_t *nodeset);

/**
 * Initializes a nodeset iterator. Sets the iterator before the first element in
 * the nodeset.
 *
 * @param iterator   Pointer to already allocated iterator memory
 * @param nodeset       Pointer to the nodeset
 */
void ir_nodeset_iterator_init(ir_nodeset_iterator_t *iterator,
                              const ir_nodeset_t *nodeset);

/**
 * Advances the iterator and returns the current element or NULL if all elements
 * in the nodeset have been processed.
 * @attention It is not allowed to use nodeset_insert or nodeset_remove while
 *            iterating over a nodeset.
 *
 * @param iterator  Pointer to the nodeset iterator.
 * @returns         Next element in the nodeset or NULL
 */
ir_node *ir_nodeset_iterator_next(ir_nodeset_iterator_t *iterator);

/**
 * Removes the element the iterator currently points to
 *
 * @param nodeset   Pointer to the nodeset
 * @param iterator  Pointer to the nodeset iterator.
 */
void ir_nodeset_remove_iterator(ir_nodeset_t *nodeset,
                                const ir_nodeset_iterator_t *iterator);

#define foreach_ir_nodeset(nodeset, irn, iter) \
	for(ir_nodeset_iterator_init(&iter, nodeset), \
        irn = ir_nodeset_iterator_next(&iter);    \
		irn != NULL; irn = ir_nodeset_iterator_next(&iter))


#ifdef IR_NODESET_USE_ORDERED_SETS

/**
 * Insert an element quickly into from the set.
 * This method may destroy internal invariats of the set (think of sorted arrays).
 * All calls to other routines but
 * - iteration
 * - get the number of elements in the set
 * will not work until ir_nodeset_fixup() was called.
 * @param nodeset The nodeset.
 * @param node    The node to insert.
 */
void ir_nodeset_insert_quick(ir_nodeset_t *nodeset, ir_node *node);

/**
 * Remove an element quickly from the set.
 * This method may destroy internal invariats of the set (think of sorted arrays).
 * All calls to other routines but
 * - iteration
 * - get the number of elements in the set
 * will not work until ir_nodeset_fixup() was called.
 * @param nodeset The nodeset.
 * @param node    The node to delete.
 */
void ir_nodeset_remove_quick(ir_nodeset_t *nodeset, const ir_node *node);

/**
 * Fixes up internal state of the set.
 * Is needed when one of the _quick functions was called.
 * @param nodeset The nodeset.
 */
void ir_nodeset_fixup(ir_nodeset_t *nodeset);

#else

#define ir_nodeset_remove_quick ir_nodeset_remove
#define ir_nodeset_insert_quick ir_nodeset_insert
#define ir_nodeset_fixup(set)

#endif /* IR_NODESET_USE_ORDERED_SETS */

#endif
