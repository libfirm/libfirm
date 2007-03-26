/**
 * @file
 * @author    Matthias Braun
 * @date      30.03.2007
 * @brief     A nodeset. This should be prefered over a simple pset, because it
 *            tries to guarantee deterministic behavior. (and is faster)
 * @version   $Id$
 */
#ifndef _FIRM_IRNODESET_H_
#define _FIRM_IRNODESET_H_

#include "irnode.h"

#define HashSet          ir_nodeset_t
#define HashSetIterator  ir_nodeset_iterator_t
#define ValueType        ir_node*
#define DO_REHASH
#include "hashset.h"
#undef DO_REHASH
#undef ValueType
#undef HashSetIterator
#undef HashSet

/**
 * Initializes a nodeset
 *
 * @param nodeset      Pointer to allocated space for the nodeset
 */
void ir_nodeset_init(ir_nodeset_t *nodeset);

/**
 * Initializes a nodeset
 *
 * @param nodeset             Pointer to allocated space for the nodeset
 * @param expected_elements   Number of elements expected in the nodeset (rougly)
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

#endif
