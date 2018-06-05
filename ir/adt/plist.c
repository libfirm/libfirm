/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
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
static plist_element_t *allocate_element(plist_t* list)
{
	plist_element_t *new_element;

	if (list->first_free_element != NULL) {
		new_element              = list->first_free_element;
		list->first_free_element = new_element->next;
		new_element->next        = NULL;
	}
	else {
		new_element = OALLOC(list->obst, plist_element_t);
	}

	return new_element;
}

plist_t *plist_new(void)
{
	plist_t *list = (plist_t*) xmalloc(sizeof(*list) + sizeof(*list->obst));

	list->obst               = (struct obstack *)&list[1];
	list->foreign_obstack    = 0;
	list->first_element      = NULL;
	list->last_element       = NULL;
	list->first_free_element = NULL;
	list->element_count      = 0;

	obstack_init(list->obst);
	return list;
}

plist_t *plist_obstack_new(struct obstack *obst)
{
	plist_t *list = OALLOC(obst, plist_t);

	list->obst               = obst;
	list->foreign_obstack    = 1;
	list->first_element      = NULL;
	list->last_element       = NULL;
	list->first_free_element = NULL;
	list->element_count      = 0;

	return list;
}

void plist_free(plist_t *list)
{
	list->first_element      = NULL;
	list->last_element       = NULL;
	list->first_free_element = NULL;
	list->element_count      = 0;

	if (! list->foreign_obstack) {
		obstack_free(list->obst, NULL);
		free(list);
	}
}

void plist_insert_back(plist_t *list, void *value)
{
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

void plist_insert_front(plist_t *list, void *value)
{
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

void plist_insert_before(plist_t *list, plist_element_t *element, void *value)
{
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

void plist_insert_after(plist_t* list, plist_element_t* element, void* value)
{
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

void plist_insert_sorted(plist_t *list, void *new, int (*compare)(void* value1, void* value2) ) 
{
	plist_element_t* current;

//empty list
	if (plist_count(list) == 0) {
		plist_insert_front(list, new);
		return;
	}

//list with n > 0 elementes
	current = plist_first(list);

	do {
//  if (new < current) -> insert before
		if ( compare( plist_element_get_value(current), new) ) {
			plist_insert_before(list, current, new);
			return;
		}
	}	while (plist_element_has_next(current) && (current = plist_element_get_next(current)) );
	plist_insert_after(list, current, new);
}

int plist_has_value(plist_t *list, void *value)
{
	plist_element_t *iter;

	for (iter = plist_first(list); iter; iter = plist_element_get_next(iter)) {
		if (plist_element_get_value(iter) == value)
			return 1;
	}

	return 0;
}

plist_element_t *plist_find_value(plist_t *list, void *value)
{
	plist_element_t *iter;

	for (iter = plist_first(list); iter; iter = plist_element_get_next(iter)) {
		if (plist_element_get_value(iter) == value)
			return iter;
	}

	return NULL;
}

void plist_erase(plist_t *list, plist_element_t *element)
{
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

void plist_clear(plist_t *list)
{
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
