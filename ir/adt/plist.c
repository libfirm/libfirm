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
 * @brief Simple, non circular, double linked pointer list.
 * @note  Created because the properties of the standard circular list were not
 *        very well suited for the interference graph implementation.
 *        This list uses an obstack and a free-list to efficiently manage its
 *        elements.
 * @author  Kimon Hoffmann
 * @date    14.07.2005
 * @version $Id$
 */
#include <stdlib.h>

#include "plist.h"

/**
 * Helper macro that returns a new uninitialized list element by either
 * fetching one from the free-list or allocating a new one on the lists
 * obstack.
 * @param list the list for which to allocate the element.
 * @return the newly allocated, uninitialized element.
 */
static plist_element_t *allocate_element(plist_t* list) {
	plist_element_t *new_element;

	if (list->first_free_element != NULL) {
		new_element              = list->first_free_element;
		list->first_free_element = new_element->next;
		new_element->next        = NULL;
	}
	else {
		new_element = obstack_alloc(list->obst, sizeof(*new_element));
	}

	return new_element;
}

plist_t *plist_new(void) {
	plist_t *list = xmalloc(sizeof(*list) + sizeof(*list->obst));

	list->obst               = (struct obstack *)&list[1];
	list->foreign_obstack    = 0;
	list->first_element      = NULL;
	list->last_element       = NULL;
	list->first_free_element = NULL;
	list->element_count      = 0;

	obstack_init(list->obst);
	return list;
}

plist_t *plist_obstack_new(struct obstack *obst) {
	plist_t *list = obstack_alloc(obst, sizeof(*list));

	list->obst               = obst;
	list->foreign_obstack    = 1;
	list->first_element      = NULL;
	list->last_element       = NULL;
	list->first_free_element = NULL;
	list->element_count      = 0;

	return list;
}

void plist_free(plist_t *list) {
	list->first_element      = NULL;
	list->last_element       = NULL;
	list->first_free_element = NULL;
	list->element_count      = 0;

	if (! list->foreign_obstack) {
		obstack_free(list->obst, NULL);
		xfree(list);
	}
}

void plist_insert_back(plist_t *list, void *value) {
	if (list->last_element != NULL) {
		plist_insert_after(list, list->last_element, value);
	}
	else {
		plist_element_t *newElement = allocate_element(list);

		newElement->data    = value;
		newElement->prev    = NULL;
		newElement->next    = NULL;
		list->first_element = list->last_element = newElement;
		list->element_count = 1;
	}
}

void plist_insert_front(plist_t *list, void *value) {
	if (list->first_element != NULL) {
		plist_insert_before(list, list->first_element, value);
	}
	else {
		plist_element_t *newElement = allocate_element(list);

		newElement->data    = value;
		newElement->prev    = NULL;
		newElement->next    = NULL;
		list->first_element = list->last_element = newElement;
		list->element_count = 1;
	}
}

void plist_insert_before(plist_t *list, plist_element_t *element, void *value) {
	plist_element_t *prevElement;
	plist_element_t *newElement = allocate_element(list);

	newElement->data = value;
	newElement->next = element;
	prevElement      = element->prev;
	newElement->prev = prevElement;

	if (prevElement != NULL) {
		prevElement->next = newElement;
	}
	else {
		list->first_element = newElement;
	}

 	element->prev = newElement;
	++list->element_count;
}

void plist_insert_after(plist_t* list, plist_element_t* element, void* value) {
	plist_element_t *nextElement;
	plist_element_t *newElement = allocate_element(list);

	newElement->data = value;
	newElement->prev = element;
	nextElement      = element->next;
	newElement->next = nextElement;

	if (nextElement != NULL) {
		nextElement->prev = newElement;
	}
	else {
		list->last_element = newElement;
	}

	element->next = newElement;
	++list->element_count;
}

int plist_has_value(plist_t *list, void *value) {
	plist_element_t *iter;

	for (iter = plist_first(list); iter; iter = plist_element_get_next(iter)) {
		if (plist_element_get_value(iter) == value)
			return 1;
	}

	return 0;
}

plist_element_t *plist_find_value(plist_t *list, void *value) {
	plist_element_t *iter;

	for (iter = plist_first(list); iter; iter = plist_element_get_next(iter)) {
		if (plist_element_get_value(iter) == value)
			return iter;
	}

	return NULL;
}

void plist_erase(plist_t *list, plist_element_t *element) {
	plist_element_t *next_element = element->next;
	plist_element_t *prev_element = element->prev;

	if (next_element != NULL) {
		next_element->prev = prev_element;
	}
	else {
		list->last_element = prev_element;
	}

	if (prev_element != NULL) {
		prev_element->next = next_element;
	}
	else {
		list->first_element = next_element;
	}

	--list->element_count;

	/* Clean the element and prepend it to the free list */
	element->prev            = NULL; /* The allocation code expects prev to be NULL */
	element->next            = list->first_free_element;
	list->first_free_element = element;
}

void plist_clear(plist_t *list) {
	plist_element_t *curr_element = list->first_element;

	while (curr_element != NULL) {
		curr_element->prev = NULL;
		curr_element       = curr_element->next;
	}

	curr_element = list->last_element;

	if (curr_element != NULL) {
		curr_element->next = list->first_free_element;
	}

	list->first_free_element = list->first_element;
	list->first_element      = 0;
	list->last_element       = 0;
	list->element_count      = 0;
}
