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
 * @brief     A value set, containing expression for values.
 * @version   $Id$
 */
#ifndef _FIRM_VALUESET_H_
#define _FIRM_VALUESET_H_

#include "firm_types.h"
#include "xmalloc.h"
#include "list.h"

typedef struct ir_valueset_entry_t {
	ir_node     *value;  /**< the represented value */
	ir_node     *expr;   /**< the leader expression for the value in the current set */
	list_head   list;    /**< link field for the list iterator */
} ir_valueset_entry_t;

#define HashSet          ir_valueset_t
#define HashSetIterator  ir_valueset_iterator_t
#define ValueType        ir_valueset_entry_t
#define ADDITIONAL_DATA  list_head elem_list; list_head all_iters;
#undef DO_REHASH
#define NO_ITERATOR

#include "hashset.h"

#undef NO_ITERATOR
#undef ADDITIONAL_DATA
#undef ValueType
#undef HashSetIterator
#undef HashSet

typedef struct ir_valueset_t ir_valueset_t;
typedef struct ir_valueset_iterator_t {
	list_head           *iter;       /**< points to the list head of the last element */
	const ir_valueset_t *valueset;   /**< the value set of this iterator. */
} ir_valueset_iterator_t;

/**
 * Initializes a value set with default size.
 *
 * @param valueset      Pointer to allocated space for the value set
 */
void ir_valueset_init(ir_valueset_t *valueset);

/**
 * Initializes a value set.
 *
 * @param valueset            Pointer to allocated space for the value set
 * @param expected_elements   Number of elements expected in the value set (roughly)
 */
void ir_valueset_init_size(ir_valueset_t *valueset, size_t expected_elements);

/**
 * Destroys a value set and frees the memory allocated for hashtable. The memory of
 * the value set itself is not freed.
 *
 * @param valueset   Pointer to the value set
 */
void ir_valueset_destroy(ir_valueset_t *valueset);

/**
 * Allocates memory for a value set and initializes it.
 *
 * @param expected_elements   Number of elements expected in the value set (roughly)
 * @return The initialized value set
 */
static inline ir_valueset_t *ir_valueset_new(size_t expected_elements) {
	ir_valueset_t *res = XMALLOC(ir_valueset_t);
	ir_valueset_init_size(res, expected_elements);
	return res;
}

/**
 * Destroys a value set and frees the memory of the set itself.
 */
static inline void ir_valueset_del(ir_valueset_t *valueset) {
	ir_valueset_destroy(valueset);
	xfree(valueset);
}

/**
 * Inserts a (value, expression) pair into a valueset if the value is not already
 * known, else does nothing.
 *
 * @param valueset  Pointer to the value set
 * @param value     the value to insert into the value set
 * @param expr      the expression to associate with the value
 * @returns         1 if the value has been inserted,
 *                  0 if it was already there
 */
int ir_valueset_insert(ir_valueset_t *valueset, ir_node *value, ir_node *expr);

/**
 * Inserts a (value, expression) pair into a valueset if the value is not already
 * known, else replace the expression for the given value.
 *
 * @param valueset  Pointer to the value set
 * @param value     the value to insert into the value set
 * @param expr      the expression to associate with the value
 * @returns         1 if the value has been inserted,
 *                  0 if it was already there
 */
int ir_valueset_replace(ir_valueset_t *valueset, ir_node *value, ir_node *expr);

/**
 * Get the leader expression of a specific value from the value set.
 *
 * @param valueset  Pointer to the value set
 * @param value     The value to find
 * @returns         the associated expression of the value or NULL
 */
void *ir_valueset_lookup(const ir_valueset_t *valueset, const ir_node *value);

/**
 * Removes a value from a value set. Does nothing if the value set doesn't contain
 * the value.
 *
 * @param valueset  Pointer to the value set
 * @param value     value to remove from the values et
 */
void ir_valueset_remove(ir_valueset_t *valueset, const ir_node *value);

/**
 * Returns the number of values contained in the value set.
 *
 * @param valueset  Pointer to the value set
 * @returns         Number of values contained in the value set
 */
size_t ir_valueset_size(const ir_valueset_t *valueset);

/**
 * Initializes a value set iterator. Sets the iterator before the first element in
 * the value set.
 *
 * @param iterator   Pointer to already allocated iterator memory
 * @param valueset   Pointer to the value set
 */
void ir_valueset_iterator_init(ir_valueset_iterator_t *iterator,
                               const ir_valueset_t *valueset);

/**
 * Advances the iterator and returns the current element or NULL if all elements
 * in the value set have been processed.
 * @note It is not allowed to use ir_valueset_insert() or ir_valueset_remove() while
 *            iterating over a nodemap.
 *
 * @param iterator  Pointer to the value set iterator.
 * @param expr      After return contains the associated expression for the value or NULL
 * @returns         Next element in the value set or NULL
 */
ir_node *ir_valueset_iterator_next(ir_valueset_iterator_t *iterator, ir_node **expr);

/**
 * Removes the element the iterator currently points to.
 *
 * @param valueset  Pointer to the value set
 * @param iterator  Pointer to the value set iterator.
 */
void ir_valueset_remove_iterator(ir_valueset_t *valueset, ir_valueset_iterator_t *iterator);

#define foreach_valueset(valueset, value, expr, iter) \
	for (ir_valueset_iterator_init(&iter, valueset), \
        value = ir_valueset_iterator_next(&iter, &expr);    \
		value != NULL; value = ir_valueset_iterator_next(&iter, &expr))

#endif
