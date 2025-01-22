/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author    Matthias Braun
 * @date      30.03.2007
 * @brief     A nodeset. This should be prefered over a simple pset, because it
 *            tries to guarantee deterministic behavior. (and is faster)
 * @note      Actually the bits to make the behaviour deterministic are not
 *            implemented yet...
 */
#ifndef _FIRM_IRNODESET_H_
#define _FIRM_IRNODESET_H_

#include <stdbool.h>
#include "firm_types.h"
#include "xmalloc.h"

#define HashSet          ir_nodeset_t
#define HashSetIterator  ir_nodeset_iterator_t
#define ValueType        ir_node*
#define DO_REHASH

#include "hashset.h"

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
static inline ir_nodeset_t *ir_nodeset_new(size_t expected_elements) {
	ir_nodeset_t *res = XMALLOC(ir_nodeset_t);
	ir_nodeset_init_size(res, expected_elements);
	return res;
}

/**
 * Destroys a nodeset and frees the memory of the nodeset itself.
 */
static inline void ir_nodeset_del(ir_nodeset_t *nodeset) {
	ir_nodeset_destroy(nodeset);
	free(nodeset);
}

/**
 * Inserts a node into a nodeset.
 *
 * @param nodeset   Pointer to the nodeset
 * @param node      node to insert into the nodeset
 * @returns         true if the element has been inserted,
 *                  false if it was already there
 */
bool ir_nodeset_insert(ir_nodeset_t *nodeset, ir_node *node);


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
 */
bool ir_nodeset_contains(const ir_nodeset_t *nodeset, const ir_node *node);

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

static inline ir_node *ir_nodeset_first(ir_nodeset_t const *const nodeset)
{
	ir_nodeset_iterator_t iter;
	ir_nodeset_iterator_init(&iter, nodeset);
	return ir_nodeset_iterator_next(&iter);
}

#define foreach_ir_nodeset(nodeset, irn, iter) \
	for (bool irn##__once = true; irn##__once;) \
		for (ir_nodeset_iterator_t iter; irn##__once;) \
			for (ir_node *irn; irn##__once; irn##__once = false) \
				for (ir_nodeset_iterator_init(&iter, nodeset); (irn = ir_nodeset_iterator_next(&iter));)

#endif
