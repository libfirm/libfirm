/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author    Matthias Braun
 * @date      12.01.2008
 * @brief     An edgeset.
 */
#ifndef _FIRM_IREDGESET_H_
#define _FIRM_IREDGESET_H_

#include "firm_types.h"

#define HashSet          ir_edgeset_t
#define HashSetIterator  ir_edgeset_iterator_t
#define ValueType        ir_edge_t*
#define DO_REHASH

#include "hashset.h"

#undef DO_REHASH
#undef ValueType
#undef HashSetIterator
#undef HashSet

typedef struct ir_edgeset_t          ir_edgeset_t;
typedef struct ir_edgeset_iterator_t ir_edgeset_iterator_t;

/**
 * Initializes a edgeset with default size.
 *
 * @param edgeset      Pointer to allocated space for the edgeset
 */
void ir_edgeset_init(ir_edgeset_t *edgeset);

/**
 * Destroys a edgeset and frees the memory allocated for hashtable. The memory of
 * the edgeset itself is not freed.
 *
 * @param edgeset   Pointer to the edgeset
 */
void ir_edgeset_destroy(ir_edgeset_t *edgeset);

/**
 * Inserts a edge into a edgeset.
 *
 * @param edgeset   Pointer to the edgeset
 * @param edge      edge to insert into the edgeset
 */
ir_edge_t *ir_edgeset_insert(ir_edgeset_t *edgeset, ir_edge_t *edge);


/**
 * Removes a edge from a edgeset. Does nothing if the edgeset doesn't contain
 * the edge.
 *
 * @param edgeset  Pointer to the edgeset
 * @param edge     Node to remove from the edgeset
 */
void ir_edgeset_remove(ir_edgeset_t *edgeset, const ir_edge_t *edge);

/**
 * Initializes a edgeset iterator. Sets the iterator before the first element in
 * the edgeset.
 *
 * @param iterator   Pointer to already allocated iterator memory
 * @param edgeset       Pointer to the edgeset
 */
void ir_edgeset_iterator_init(ir_edgeset_iterator_t *iterator,
                              const ir_edgeset_t *edgeset);

/**
 * Advances the iterator and returns the current element or NULL if all elements
 * in the edgeset have been processed.
 * @attention It is not allowed to use edgeset_insert or edgeset_remove while
 *            iterating over a edgeset.
 *
 * @param iterator  Pointer to the edgeset iterator.
 * @returns         Next element in the edgeset or NULL
 */
ir_edge_t *ir_edgeset_iterator_next(ir_edgeset_iterator_t *iterator);

/**
 * Removes the element the iterator currently points to
 *
 * @param edgeset   Pointer to the edgeset
 * @param iterator  Pointer to the edgeset iterator.
 */
void ir_edgeset_remove_iterator(ir_edgeset_t *edgeset,
                                const ir_edgeset_iterator_t *iterator);

#define foreach_ir_edgeset(edgeset, edge, iter) \
	for (ir_edgeset_iterator_init(&iter, edgeset); (edge = ir_edgeset_iterator_next(&iter)) != NULL;)

#endif
