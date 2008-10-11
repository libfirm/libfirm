/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @author    Michael Beck
 * @brief     A linked nodemap.
 * @version   $Id$
 */
#ifndef _FIRM_IRLINKEDNODEMAP_H_
#define _FIRM_IRLINKEDNODEMAP_H_

#include "firm_types.h"
#include "xmalloc.h"
#include "list.h"

/*
 * sebastian experimental:
 * use ordered arrays as node sets.
 * the guys here have made good experiences with that.
 * Internally we use normal Firm arrays and binary
 * search for locating the elements. Using arrays should
 * give the sets a small footprint.
 */
#undef IR_nodemap_USE_ORDERED_SETS

typedef struct ir_lnk_nodemap_entry_t {
	ir_node     *node;  /**< the node itself */
	void        *data;  /**< associated data */
	list_head   list;   /**< link field for the list iterator */
} ir_lnk_nodemap_entry_t;

#define HashSet          ir_lnk_nodemap_t
#define HashSetIterator  ir_lnk_nodemap_iterator_t
#define ValueType        ir_lnk_nodemap_entry_t
#define ADDITIONAL_DATA  list_head elem_list; list_head all_iters;
#define DO_REHASH
#define NO_ITERATOR

#include "hashset.h"

#undef NO_ITERATOR
#undef DO_REHASH
#undef ADDITIONAL_DATA
#undef ValueType
#undef HashSetIterator
#undef HashSet

typedef struct ir_lnk_nodemap_t ir_lnk_nodemap_t;
typedef struct ir_lnk_nodemap_iterator_t {
	list_head              *iter;       /**< points to the list head of the last element */
	const ir_lnk_nodemap_t *nodemap;    /**< lithe nodemap of this iterator. */
} ir_lnk_nodemap_iterator_t;

/**
 * Initializes a linked nodemap with default size.
 *
 * @param nodemap      Pointer to allocated space for the nodemap
 */
void ir_lnk_nodemap_init(ir_lnk_nodemap_t *nodemap);

/**
 * Initializes a linked nodemap.
 *
 * @param nodemap             Pointer to allocated space for the nodemap
 * @param expected_elements   Number of elements expected in the nodemap (roughly)
 */
void ir_lnk_nodemap_init_size(ir_lnk_nodemap_t *nodemap, size_t expected_elements);

/**
 * Destroys a nodemap and frees the memory allocated for hashtable. The memory of
 * the nodemap itself is not freed.
 *
 * @param nodemap   Pointer to the nodemap
 */
void ir_lnk_nodemap_destroy(ir_lnk_nodemap_t *nodemap);

/**
 * Allocates memory for a linked nodemap and initializes the set.
 *
 * @param expected_elements   Number of elements expected in the nodemap (roughly)
 * @return The initialized nodemap
 */
static INLINE ir_lnk_nodemap_t *ir_lnk_nodemap_new(size_t expected_elements) {
	ir_lnk_nodemap_t *res = XMALLOC(ir_lnk_nodemap_t);
	ir_lnk_nodemap_init_size(res, expected_elements);
	return res;
}

/**
 * Destroys a linked nodemap and frees the memory of the nodemap itself.
 */
static INLINE void ir_lnk_nodemap_del(ir_lnk_nodemap_t *nodemap) {
	ir_lnk_nodemap_destroy(nodemap);
	xfree(nodemap);
}

/**
 * Inserts a node into a linked nodemap.
 *
 * @param nodemap   Pointer to the nodemap
 * @param node      node to insert into the nodemap
 * @param data      data to associate with the node
 * @returns         1 if the element has been inserted,
 *                  0 if it was already there
 */
int ir_lnk_nodemap_put(ir_lnk_nodemap_t *nodemap, ir_node *node, void *data);

/**
 * Get the associated value of a specific node
 *
 * @param nodemap   Pointer to the nodemap
 * @param node      The pointer to find
 * @returns         the associated data of the node or NULL
 */
void *ir_lnk_nodemap_get(const ir_lnk_nodemap_t *nodemap, const ir_node *node);

/**
 * Removes a node from a linked nodemap. Does nothing if the nodemap doesn't contain
 * the node.
 *
 * @param nodemap  Pointer to the nodemap
 * @param node     Node to remove from the nodemap
 */
void ir_lnk_nodemap_remove(ir_lnk_nodemap_t *nodemap, const ir_node *node);

/**
 * Returns the number of nodes contained in the linked nodemap.
 *
 * @param nodemap   Pointer to the nodemap
 * @returns         Number of nodes contained in the linked nodemap
 */
size_t ir_lnk_nodemap_size(const ir_lnk_nodemap_t *nodemap);

/**
 * Initializes a nodemap iterator. Sets the iterator before the first element in
 * the linked nodemap.
 *
 * @param iterator   Pointer to already allocated iterator memory
 * @param nodemap       Pointer to the nodemap
 */
void ir_lnk_nodemap_iterator_init(ir_lnk_nodemap_iterator_t *iterator,
                                  const ir_lnk_nodemap_t *nodemap);

/**
 * Advances the iterator and returns the current element or NULL if all elements
 * in the linked nodemap have been processed.
 * @attention It is not allowed to use ir_lnk_nodemap_insert or ir_lnk_nodemap_remove while
 *            iterating over a nodemap.
 *
 * @param iterator  Pointer to the nodemap iterator.
 * @returns         Next element in the nodemap or NULL
 */
ir_node *ir_lnk_nodemap_iterator_next(ir_lnk_nodemap_iterator_t *iterator);

/**
 * Removes the element the iterator currently points to.
 *
 * @param nodemap   Pointer to the linked nodemap
 * @param iterator  Pointer to the linked nodemap iterator.
 */
void ir_lnk_nodemap_remove_iterator(ir_lnk_nodemap_t *nodemap,
                                    ir_lnk_nodemap_iterator_t *iterator);

#define foreach_ir_lnk_nodemap(nodemap, irn, iter) \
	for (ir_lnk_nodemap_iterator_init(&iter, nodemap), \
        irn = ir_lnk_nodemap_iterator_next(&iter);    \
		irn != NULL; irn = ir_lnk_nodemap_iterator_next(&iter))

#endif
