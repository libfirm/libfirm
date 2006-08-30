/**
 * Simple, non circular, double linked pointer list.
 * Created because the properties of the standard circular list were not
 * very well suited for the interference graph implementation.
 * This list uses an obstack and a free-list to efficiently manage its
 * elements.
 * @author Kimon Hoffmann
 * @date   14.07.2005
 * @cvs-id $Id$
 * @note Until now the code is entirely untested so it probably contains
 * 		plenty of errors.
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
static plist_element_t* allocate_element(plist_t* list) {
	plist_element_t* newElement;

	if (list->first_free_element != NULL) {
		newElement               = list->first_free_element;
		list->first_free_element = newElement->next;
		newElement->next         = NULL;
	}
	else {
		newElement = obstack_alloc(&list->obst, sizeof(*newElement));
	}

	return newElement;
}

plist_t* plist_new(void) {
	plist_t* list = xmalloc(sizeof(*list));

	obstack_init(&list->obst);
	list->first_element      = NULL;
	list->last_element       = NULL;
	list->first_free_element = NULL;
	list->element_count      = 0;

	return list;
}

void plist_free(plist_t* list) {
	obstack_free(&list->obst, NULL);

	list->first_element      = NULL;
	list->last_element       = NULL;
	list->first_free_element = NULL;
	list->element_count      = 0;
	xfree(list);
}

void plist_insert_back(plist_t* list, void* value) {
	if (list->last_element != NULL) {
		plist_insert_after(list, list->last_element, value);
	}
	else {
		plist_element_t* newElement = allocate_element(list);

		newElement->data    = value;
		newElement->prev    = NULL;
		newElement->next    = NULL;
		list->first_element = list->last_element = newElement;
		list->element_count = 1;
	}
}

void plist_insert_front(plist_t* list, void* value) {
	if (list->first_element != NULL) {
		plist_insert_before(list, list->first_element, value);
	}
	else {
		plist_element_t* newElement = allocate_element(list);

		newElement->data    = value;
		newElement->prev    = NULL;
		newElement->next    = NULL;
		list->first_element = list->last_element = newElement;
		list->element_count = 1;
	}
}

void plist_insert_before(plist_t* list, plist_element_t* element, void* value) {
	plist_element_t* prevElement;
	plist_element_t* newElement = allocate_element(list);

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
	plist_element_t* nextElement;
	plist_element_t* newElement = allocate_element(list);

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

void plist_erase(plist_t* list, plist_element_t* element) {
	plist_element_t* nextElement = element->next;
	plist_element_t* prevElement = element->prev;

	if (nextElement != NULL) {
		nextElement->prev = prevElement;
	}
	else {
		list->last_element = prevElement;
	}

	if (prevElement != NULL) {
		prevElement->next = nextElement;
	}
	else {
		list->first_element = nextElement;
	}

	--list->element_count;

	/* Clean the element and prepend it to the free list */
	element->prev            = NULL; /* The allocation code expects prev to be NULL */
	element->next            = list->first_free_element;
	list->first_free_element = element;
}

void plist_clear(plist_t* list) {
	plist_element_t* currentElement = list->first_element;

	while (currentElement != NULL) {
		currentElement->prev = NULL;
		currentElement       = currentElement->next;
	}

	currentElement = list->last_element;

	if (currentElement != NULL) {
		currentElement->next = list->first_free_element;
	}

	list->first_free_element = list->first_element;
	list->first_element      = 0;
	list->last_element       = 0;
	list->element_count      = 0;
}
