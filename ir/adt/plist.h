/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @author Kimon Hoffmann
 * @date   14.07.2005
 * @cvs-id $Id$
 * @summary Simple, non circular, double linked pointer list.
 *          Created because the properties of the standard circular list were
 *          not very well suited for the interference graph implementation.
 *          This list uses an obstack and a free-list to efficiently manage its
 *          elements.
 * @note    Until now the code is entirely untested so it probably contains
 *          plenty of errors. (Matze: Is this still true, the code seems to be
 *          used at some places....)
 */
#ifndef FIRM_ADT_PLIST_H
#define FIRM_ADT_PLIST_H

#include <stddef.h>
#include "obst.h"

typedef struct _plist_element plist_element_t;
typedef struct _plist plist_t;

/**
 * The plist data type.
 */
struct _plist {
	/**
	 * The obstack used for all allocations.
	 */
	struct obstack *obst;

	/* Set to 1 if plist uses a foreign obstack  */
	unsigned foreign_obstack : 1;

	/**
	 * First element in the list.
	 */
	plist_element_t *first_element;

	/**
	 * Last element in the list.
	 */
	plist_element_t *last_element;

	/**
	 * Current number of elements in the list.
	 */
	int element_count;

	/**
	 * First element in the free list.
	 * Please note that the free list is a single linked list and all back
	 * references are invalid.
	 */
	plist_element_t* first_free_element;
};

/**
 * An element in the pointer list.
 */
struct _plist_element {
	plist_element_t *next;
	plist_element_t *prev;
	void *data;
};

/**
 * Creates a new pointer list and initializes it.
 * @return The newly created pointer list.
 */
plist_t *plist_new(void);

/**
 * Creates a new pointer list and initializes it.
 * Uses the given obstack instead of creating one.
 * @param obst  The obstack to use
 * @return The newly created pointer list.
 */
plist_t *plist_obstack_new(struct obstack *obst);

/**
 * Frees the passed pointer list.
 * After a call to this function all references to the list and any of
 * its elements are invalid.
 */
void plist_free(plist_t *list);

/**
 * Returns the number of elements in a pointer list.
 * @param list the pointer list
 * @return The number of elements in a pointer list.
 */
#define plist_count(list) \
 	((list)->element_count)

/**
 * Inserts an element at the back of a pointer list.
 * @param list the pointer list to append the new element to.
 * @param value the element value to append.
 */
void plist_insert_back(plist_t *list, void *value);

/**
 * Inserts an element at the front of a pointer list.
 * @param list the pointer list to prepend the new element to.
 * @param value the element value to prepend.
 */
void plist_insert_front(plist_t *list, void *value);

/**
 * Inserts an element into a pointer list before the specified element,
 * which must be non null.
 * @param list the pointer list to insert the new element into.
 * @param element the list element before which the new element should
 * 		be inserted. This element must be a part of @p list.
 * @param value the element value to insert.
 */
void plist_insert_before(plist_t *list, plist_element_t *element, void *value);

/**
 * Inserts an element into a pointer list after the specified element,
 * which must be non null.
 * @param list the pointer list to insert the new element into.
 * @param element the list element after which the new element should
 * 		be inserted. This element must be a part of @p list.
 * @param value the element value to insert.
 */
void plist_insert_after(plist_t *list, plist_element_t *element, void *value);

/**
 * Checks if list has an element with the given data pointer.
 * @param list   the list to check
 * @param value  the data pointer to look for
 * @return 1 if element with data pointer found, 0 otherwise
 */
int plist_has_value(plist_t *list, void *value);

/**
 * Tries to find list element associated to the given data pointer.
 * @param list   the list to check
 * @param value  the data pointer to look for
 * @return The first list element associated to data pointer if found, NULL otherwise
 */
plist_element_t *plist_find_value(plist_t *list, void *value);

/**
 * Erases the specified element from the pointer list.
 * @param list the pointer list from which the element should be erased.
 * @param element the list element to erase. This element must be a part
 * 		of @p list.
 */
void plist_erase(plist_t *list, plist_element_t *element);

/**
 * Erases all elements from the specified pointer list.
 * @param list the pointer list that should be cleared.
 */
void plist_clear(plist_t *list);

/**
 * Returns the first element of a pointer list.
 * @param list the pointer list to iterate
 * @return a pointer to the element or NULL if the list is empty
 */
 #define plist_first(list) \
 	((list)->first_element)

/**
 * Returns the last element of a pointer list.
 * @param list the pointer list to iterate
 * @return a pointer to the element or NULL if the list is empty
 */
 #define plist_last(list) \
 	((list)->last_element)

/**
 * Checks whether a pointer list element has a successor or not.
 * @param element the list element that should be queried for existence
 * 		of a successor.
 * @return TRUE if @p element has a successor, otherwise FALSE.
 */
#define plist_element_has_next(element) \
	((element)->next != NULL)

/**
 * Checks whether a pointer list element has a predecessor or not.
 * @param element the list element that should be queried for existence
 * 		of a predecessor.
 * @return TRUE if @p element has a successor, otherwise FALSE.
 */
#define plist_element_has_prev(element) \
	((element)->prev != NULL)

/**
 * Gets the successor of the passed list element.
 * @param element the list element to return the successor of.
 * @return The successor of @p element or NULL if @p element is the last
 * 		element in the sequence.
 */
#define plist_element_get_next(element) \
	((element)->next)

/**
 * Gets the predecessor of the passed list element.
 * @param element the list element to return the predecessor of.
 * @return The predecessor of @p element or NULL if @p element is the last
 * 		element in the sequence.
 */
#define plist_element_get_prev(element) \
	((element)->prev)

/**
 * Gets the value stored in the passed list element.
 * @param element the list element to return the value of.
 * @return The value stored in @p element.
 */
#define plist_element_get_value(element) \
	((element)->data)

/**
 * Convenience macro to iterate over a plist.
 */
#define foreach_plist(list, el) \
	for (el = plist_first(list); el; el = plist_element_get_next(el))

#endif /*_PLIST_H_*/
