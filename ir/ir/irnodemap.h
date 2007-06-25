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
 * @brief     A nodemap. This should be prefered over a simple pset, because it
 *            tries to guarantee deterministic behavior. (and is faster)
 * @version   $Id$
 * @note      Actually the bits to make the behaviour deterministic are not
 *            implemented yet...
 */
#ifndef _FIRM_IRNODEMAP_H_
#define _FIRM_IRNODEMAP_H_

#include "irnode.h"
#include "xmalloc.h"

typedef struct ir_nodemap_entry_t {
	const ir_node *node;
	void          *data;
} ir_nodemap_entry_t;

#define HashSet          ir_nodemap_t
#define HashSetIterator  ir_nodemap_iterator_t
#define ValueType        ir_nodemap_entry_t
#define DO_REHASH
#include "hashset.h"
#undef DO_REHASH
#undef ValueType
#undef HashSetIterator
#undef HashSet

typedef struct ir_nodemap_t           ir_nodemap_t;
typedef struct ir_nodemap_iterator_t  ir_nodemap_iterator_t;

/**
 * Initializes a nodemap with default size.
 *
 * @param nodemap      Pointer to allocated space for the nodemap
 */
void ir_nodemap_init(ir_nodemap_t *nodemap);

/**
 * Initializes a nodemap
 *
 * @param nodemap             Pointer to allocated space for the nodemap
 * @param expected_elements   Number of elements expected in the nodemap (roughly)
 */
void ir_nodemap_init_size(ir_nodemap_t *nodemap, size_t expected_elements);

/**
 * Destroys a nodemap and frees the memory allocated for hashtable. The memory of
 * the nodemap itself is not freed.
 *
 * @param nodemap   Pointer to the nodemap
 */
void ir_nodemap_destroy(ir_nodemap_t *nodemap);

/**
 * Inserts a node into a nodemap.
 *
 * @param nodemap   Pointer to the nodemap
 * @param node      node to insert into the nodemap
 * @param data      data to associate with the node
 */
void ir_nodemap_insert(ir_nodemap_t *nodemap, const ir_node *node, void *data);

/**
 * Removes a node from a nodemap. Does nothing if the nodemap doesn't contain
 * the node.
 *
 * @param nodemap  Pointer to the nodemap
 * @param node     Node to remove from the nodemap
 */
void ir_nodemap_remove(ir_nodemap_t *nodemap, const ir_node *node);

/**
 * Tests whether a nodemap contains a specific node
 *
 * @param nodemap   Pointer to the nodemap
 * @param node      The pointer to find
 * @returns         1 if nodemap contains the node, 0 else
 */
void *ir_nodemap_get(const ir_nodemap_t *nodemap, const ir_node *node);

/**
 * Returns the number of pointers contained in the nodemap
 *
 * @param nodemap   Pointer to the nodemap
 * @returns       Number of pointers contained in the nodemap
 */
size_t ir_nodemap_size(const ir_nodemap_t *nodemap);

#if 0
/**
 * Initializes a nodemap iterator. Sets the iterator before the first element in
 * the nodemap.
 *
 * @param iterator   Pointer to already allocated iterator memory
 * @param nodemap       Pointer to the nodemap
 */
void ir_nodemap_iterator_init(ir_nodemap_iterator_t *iterator,
                              const ir_nodemap_t *nodemap);

/**
 * Advances the iterator and returns the current element or NULL if all elements
 * in the nodemap have been processed.
 * @attention It is not allowed to use nodemap_insert or nodemap_remove while
 *            iterating over a nodemap.
 *
 * @param iterator  Pointer to the nodemap iterator.
 * @returns         Next element in the nodemap or NULL
 */
ir_node *ir_nodemap_iterator_next(ir_nodemap_iterator_t *iterator);

/**
 * Removes the element the iterator currently points to
 *
 * @param nodemap   Pointer to the nodemap
 * @param iterator  Pointer to the nodemap iterator.
 */
void ir_nodemap_remove_iterator(ir_nodemap_t *nodemap,
                                const ir_nodemap_iterator_t *iterator);
#endif

#endif
