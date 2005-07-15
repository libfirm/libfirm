/**
 * Simple, non circular, double linked pointer list.
 * Created because the properties of the standard circular list were not
 * very well suited for the interference graph implementation.
 * This list uses an obstack and a free-list to efficiently manage its
 * elements.
 * @author Kimon Hoffmann
 * @date 14.07.2005
 * @note Until now the code is entirely untested so it probably contains
 * 		plenty of errors.
 */
#ifndef _PLIST_H_
#define _PLIST_H_

#include <stddef.h>
#include "obst.h"

typedef struct PListElement PListElement;
typedef struct PList PList;

/**
 * The Plist data type.
 */
struct PList {
	/**
	 * The obstack used for all allocations.
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
	 * Current number of elements in the list.
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
 * An element in the pointer list.
 */
struct PListElement {
	PListElement* next;
	PListElement* prev;
	void* data;
};

/**
 * Creates a new pointer list and initializes it.
 * @return The newly created pointer list.
 */
PList* plist_new(void);

/**
 * Frees the passed pointer list.
 * After a call to this function all references to the list and any of
 * its elements are invalid.
 */
void plist_free(PList* list);

/**
 * Returns the number of elements in a pointer list.
 * @param list the pointer list
 * @return The number of elements in a pointer list.
 */
#define plist_count(list) \
 	((list)->elementCount)

/**
 * Inserts an element at the back of a pointer list.
 * @param list the pointer list to append the new element to.
 * @param value the element value to append.
 */
void plist_insert_back(PList* list, void* value);

/**
 * Inserts an element at the front of a pointer list.
 * @param list the pointer list to prepend the new element to.
 * @param value the element value to prepend.
 */
void plist_insert_front(PList* list, void* value);

/**
 * Inserts an element into a pointer list before the specified element,
 * which must be non null.
 * @param list the pointer list to insert the new element into.
 * @param element the list element before which the new element should
 * 		be inserted. This element must be a part of @p list.
 * @param value the element value to insert.
 */
void plist_insert_before(PList* list, PListElement* element, void* value);

/**
 * Inserts an element into a pointer list after the specified element,
 * which must be non null.
 * @param list the pointer list to insert the new element into.
 * @param element the list element after which the new element should
 * 		be inserted. This element must be a part of @p list.
 * @param value the element value to insert.
 */
void plist_insert_after(PList* list, PListElement* element, void* value);

/**
 * Erases the specified element from the pointer list.
 * @param list the pointer list from which the lement should be erased.
 * @param element the list element to erase. This element must be a part
 * 		of @p list.
 */
void plist_erase(PList* list, PListElement* element);

/**
 * Erases all elements from the specified pointer list.
 * @param list the pointer list that should be cleard.
 */
void plist_clear(PList* list);

/**
 * Returns the first element of a pointer list.
 * @param list the pointer list to iterate
 * @return a pointer to the element or NULL if the list is empty
 */
 #define plist_first(list) \
 	((list)->firstElement)

/**
 * Returns the last element of a pointer list.
 * @param list the pointer list to iterate
 * @return a pointer to the element or NULL if the list is empty
 */
 #define plist_last(list) \
 	((list)->lastElement)

/**
 * Checks whether a pointer list element has a successor or not.
 * @param element the list element that should be queried for existance
 * 		of a successor.
 * @return TRUE if @p element has a successor, otherwise FALSE.
 */
#define plist_element_has_next(element) \
	((element)->next != NULL)

/**
 * Checks whether a pointer list element has a predecessor or not.
 * @param element the list element that should be queried for existance
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

#endif /*_PLIST_H_*/
