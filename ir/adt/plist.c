/**
 * Simple, non circular, double linked pointer list.
 * Created because the properties of the standard circular list were not
 * very well suited for the interference graph implementation.
 * This list uses an obstack and a free-list to efficiently manage its
 * elements.
 * @author Kimon Hoffmann
 * @date 17.07.2005
 * @note Until now the code is entirely untested so it probably contains
 * 		plenty of errors.
 */
#include <stdlib.h>
#include "obst.h"

#include "plist.h"

/**
 * Structure for one entry of the double linked pointer list.
 */
struct PListElement {
	PListElement* next;
	PListElement* prev;
	void* data;
};

/**
 * The list data type.
 */
struct PList {
	/**
	 * The obastack used for all allocations.
	 */
	struct obstack obst;
	/**
	 * First element in the list.
	 */
	PListElement* firstElement;
	/**
	 * Last element in the list.
	 */
	PListElement* lastElement;
	/**
	 * Current numner of elements in the list.
	 */
	int elementCount;
	/**
	 * First element in the free list.
	 * Please note that the free list is a single linked list and all back
	 * references are invalid.
	 */
	PListElement* firstFreeElement;
};

/**
 * Helper macro that returns a new uninitialized list element by either
 * fetching one from the free-list or allocating a new one on the lists
 * obstack.
 * @param list the list for which to allocate the element.
 * @return the newlyallocated, uninitialized element.
 */
static PListElement* allocate_element(PList* list) {
	PListElement* newElement;
	if (list->firstFreeElement != NULL) {
		newElement = list->firstFreeElement;
		list->firstFreeElement = newElement->next;
		newElement->next = NULL;
	} else {
		newElement = obstack_alloc(&list->obst, sizeof(*newElement));
	}
	return newElement;
}

PList* plist_new(void) {
	PList* list = xmalloc(sizeof(*list));
	obstack_init(&list->obst);
	list->firstElement = NULL;
	list->lastElement = NULL;
	list->firstFreeElement = NULL;
	list->elementCount = 0;
	return list;
}

void plist_free(PList* list) {
	obstack_free(&list->obst, NULL);
	list->firstElement = NULL;
	list->lastElement = NULL;
	list->firstFreeElement = NULL;
	list->elementCount = 0;
	xfree(list);
}

void plist_insert_back(PList* list, void* value) {
	if (list->lastElement != NULL) {
		plist_insert_after(list, list->lastElement, value);
	} else {
		PListElement* newElement = allocate_element(list);
		newElement->data = value;
		newElement->prev = NULL;
		newElement->next = NULL;
		list->firstElement = list->lastElement = newElement;
		list->elementCount = 1;
	}
}

void plist_insert_front(PList* list, void* value) {
	if (list->firstElement != NULL) {
		plist_insert_before(list, list->firstElement, value);
	} else {
		PListElement* newElement = allocate_element(list);
		newElement->data = value;
		newElement->prev = NULL;
		newElement->next = NULL;
		list->firstElement = list->lastElement = newElement;
		list->elementCount = 1;
	}
}

void plist_insert_before(PList* list, PListElement* element, void* value) {
	PListElement* newElement = allocate_element(list);
	newElement->data = value;
	newElement->next = element;
	PListElement* prevElement = element->prev;
	newElement->prev = prevElement;
	if (prevElement != NULL) {
		prevElement->next = newElement;
	} else {
		list->firstElement = newElement;
	}
	element->prev = newElement;
	++list->elementCount;
}

void plist_insert_after(PList* list, PListElement* element, void* value) {
	PListElement* newElement = allocate_element(list);
	newElement->data = value;
	newElement->prev = element;
	PListElement* nextElement = element->next;
	newElement->next = nextElement;
	if (nextElement != NULL) {
		nextElement->prev = newElement;
	} else {
		list->lastElement = newElement;
	}
	element->next = newElement;
	++list->elementCount;
}

void plist_erase(PList* list, PListElement* element) {
	PListElement* nextElement = element->next;
	PListElement* prevElement = element->prev;
	if (nextElement != NULL) {
		nextElement->prev = prevElement;
	} else {
		list->lastElement = prevElement;
	}
	if (prevElement != NULL) {
		prevElement->next = nextElement;
	} else {
		list->firstElement = nextElement;
	}
	--list->elementCount;
	/* Clean the element and prepend it to the free list */
	element->prev = NULL; /* The allocation code exprects prev to be NULL */
	element->next = list->firstFreeElement;
	list->firstFreeElement = element;
}

void plist_clear(PList* list) {
	PListElement* currentElement = list->firstElement;
	while (currentElement != NULL) {
		currentElement->prev = NULL;
		currentElement = currentElement->next;
	}
	currentElement = list->lastElement;
	if (currentElement != NULL) {
		currentElement->next = list->firstFreeElement;
	}
	list->firstFreeElement = list->firstElement;
	list->firstElement = list->lastElement = 0;
	list->elementCount = 0;
}
