/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author    Matthias Braun
 * @date      30.03.2007
 * @brief     A nodemap. This should be preferred over a simple pset, because it
 *            tries to guarantee deterministic behavior. (and is faster)
 */
#ifndef _FIRM_IRNODEHASHMAP_H_
#define _FIRM_IRNODEHASHMAP_H_

#include "firm_types.h"

typedef struct ir_nodehashmap_entry_t {
	ir_node *node;
	void    *data;
} ir_nodehashmap_entry_t;

#define HashSet          ir_nodehashmap_t
#define HashSetIterator  ir_nodehashmap_iterator_t
#define ValueType        ir_nodehashmap_entry_t
#define DO_REHASH
#include "hashset.h"
#undef DO_REHASH
#undef ValueType
#undef HashSetIterator
#undef HashSet

typedef struct ir_nodehashmap_t           ir_nodehashmap_t;
typedef struct ir_nodehashmap_iterator_t  ir_nodehashmap_iterator_t;

/**
 * Initializes a nodehashmap with default size.
 *
 * @param nodehashmap      Pointer to allocated space for the nodehashmap
 */
void ir_nodehashmap_init(ir_nodehashmap_t *nodehashmap);

/**
 * Initializes a nodehashmap
 *
 * @param nodehashmap         Pointer to allocated space for the nodehashmap
 * @param expected_elements   Number of elements expected in the nodehashmap
 *                            (roughly)
 */
void ir_nodehashmap_init_size(ir_nodehashmap_t *nodehashmap,
                              size_t expected_elements);

/**
 * Destroys a nodehashmap and frees the memory allocated for hashtable. The
 * memory of the nodehashmap itself is not freed.
 *
 * @param nodehashmap   Pointer to the nodehashmap
 */
void ir_nodehashmap_destroy(ir_nodehashmap_t *nodehashmap);

/**
 * Inserts a node into a nodehashmap.
 *
 * @param nodehashmap  Pointer to the nodehashmap
 * @param node         node to insert into the nodehashmap
 * @param data         data to associate with the node
 */
void ir_nodehashmap_insert(ir_nodehashmap_t *nodehashmap, ir_node *node,
                           void *data);

/**
 * Removes a node from a nodehashmap. Does nothing if the nodehashmap doesn't
 * contain the node.
 *
 * @param nodehashmap  Pointer to the nodehashmap
 * @param node         Node to remove from the nodehashmap
 */
void ir_nodehashmap_remove(ir_nodehashmap_t *nodehashmap, const ir_node *node);

/**
 * Tests whether a nodehashmap contains a specific node
 *
 * @param nodehashmap   Pointer to the nodehashmap
 * @param node          The pointer to find
 * @returns             the associated data of the node or NULL
 */
void *ir_nodehashmap_get(const ir_nodehashmap_t *nodehashmap,
                         const ir_node *node);

#define ir_nodehashmap_get(type, self, node) ((type*)ir_nodehashmap_get((self), (node)))

/**
 * Returns the number of pointers contained in the nodehashmap
 *
 * @param nodehashmap   Pointer to the nodehashmap
 * @returns             Number of pointers contained in the nodehashmap
 */
size_t ir_nodehashmap_size(const ir_nodehashmap_t *nodehashmap);

/**
 * Initializes a nodehashmap iterator. Sets the iterator before the first
 * element in the nodehashmap.
 *
 * @param iterator   Pointer to already allocated iterator memory
 * @param nodehashmap       Pointer to the nodehashmap
 */
void ir_nodehashmap_iterator_init(ir_nodehashmap_iterator_t *iterator,
                                  const ir_nodehashmap_t *nodehashmap);

/**
 * Advances the iterator and returns the current element or NULL if all elements
 * in the nodehashmap have been processed.
 * @attention It is not allowed to use nodehashmap_insert or nodehashmap_remove
 * while iterating over a nodehashmap.
 *
 * @param iterator  Pointer to the nodehashmap iterator.
 * @returns         Next element in the nodehashmap or NULL
 */
ir_nodehashmap_entry_t ir_nodehashmap_iterator_next(
		ir_nodehashmap_iterator_t *iterator);

/**
 * Removes the element the iterator currently points to
 *
 * @param nodehashmap  Pointer to the nodehashmap
 * @param iterator     Pointer to the nodehashmap iterator.
 */
void ir_nodehashmap_remove_iterator(ir_nodehashmap_t *nodehashmap,
                                    const ir_nodehashmap_iterator_t *iterator);

#define foreach_ir_nodehashmap(nodehashmap, entry, iter) \
	for (ir_nodehashmap_iterator_init(&iter, nodehashmap); (entry = ir_nodehashmap_iterator_next(&iter)).node;)

#endif
