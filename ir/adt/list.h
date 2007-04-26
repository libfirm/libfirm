
/**
 * @file
 * @brief   Doubly linked lists.
 * @version $Id$
 *
 * Simple doubly linked list implementation.
 *
 * Some of the internal functions ("__xxx") are useful when
 * manipulating whole lists rather than single entries, as
 * sometimes we already know the next/prev entries and we can
 * generate better code by using them directly rather than
 * using the generic single-entry routines.
  */
#ifndef FIRM_ADT_LIST_H
#define FIRM_ADT_LIST_H

#include "firm_config.h"

struct list_head {
	struct list_head *next, *prev;
};

#define LIST_HEAD_INIT(name) { &(name), &(name) }

#define LIST_HEAD(name) \
	struct list_head name = LIST_HEAD_INIT(name)

#define INIT_LIST_HEAD(ptr) do { \
	(ptr)->next = (ptr); (ptr)->prev = (ptr); \
} while (0)

#define _list_offsetof(type,member) \
  ((char *) &(((type *) 0)->member) - (char *) 0)

#define _list_container_of(ptr, type, member) \
	((type *) ((char *) (ptr) - _list_offsetof(type, member)))

/*
 * Insert a new entry between two known consecutive entries.
 *
 * This is only for internal list manipulation where we know
 * the prev/next entries already!
 */
static INLINE void __list_add(struct list_head *new_node,
			      struct list_head *prev,
			      struct list_head *next)
{
	next->prev = new_node;
	new_node->next = next;
	new_node->prev = prev;
	prev->next = new_node;
}

/**
 * list_add - add a new entry
 * @param new_node   new entry to be added
 * @param head       list head to add it after
 *
 * Insert a new entry after the specified head.
 * This is good for implementing stacks.
 */
static INLINE void list_add(struct list_head *new_node, struct list_head *head)
{
	__list_add(new_node, head, head->next);
}

/**
 * list_add_tail - add a new entry
 * @param new_node   new entry to be added
 * @param head       list head to add it before
 *
 * Insert a new entry before the specified head.
 * This is useful for implementing queues.
 */
static INLINE void list_add_tail(struct list_head *new_node, struct list_head *head)
{
	__list_add(new_node, head->prev, head);
}

/*
 * Delete a list entry by making the prev/next entries
 * point to each other.
 *
 * This is only for internal list manipulation where we know
 * the prev/next entries already!
 */
static INLINE void __list_del(struct list_head * prev, struct list_head * next)
{
	next->prev = prev;
	prev->next = next;
}

/**
 * list_del - deletes entry from list.
 * @param entry  the element to delete from the list.
 *
 * @Note
 *   list_empty on entry does not return true after this, the entry is
 *   in an undefined state.
 */
static INLINE void list_del(struct list_head *entry)
{
	__list_del(entry->prev, entry->next);
	entry->next = NULL;
	entry->prev = NULL;
}


/**
 * list_del_init - deletes entry from list and reinitialize it.
 * @param entry   the element to delete from the list.
 */
static INLINE void list_del_init(struct list_head *entry)
{
	__list_del(entry->prev, entry->next);
	INIT_LIST_HEAD(entry);
}

/**
 * list_move - delete from one list and add as another's head
 * @param list   the entry to move
 * @param head   the head that will precede our entry
 */
static INLINE void list_move(struct list_head *list, struct list_head *head)
{
        __list_del(list->prev, list->next);
        list_add(list, head);
}

/**
 * list_move_tail - delete from one list and add as another's tail
 * @param list   the entry to move
 * @param head   the head that will follow our entry
 */
static INLINE void list_move_tail(struct list_head *list,
				  struct list_head *head)
{
        __list_del(list->prev, list->next);
        list_add_tail(list, head);
}

/**
 * list_empty - tests whether a list is empty
 * @param head   the list to test.
 */
static INLINE int list_empty(const struct list_head *head)
{
	return head->next == head;
}

static INLINE void __list_splice(struct list_head *list,
				 struct list_head *head)
{
	struct list_head *first = list->next;
	struct list_head *last = list->prev;
	struct list_head *at = head->next;

	first->prev = head;
	head->next = first;

	last->next = at;
	at->prev = last;
}

/**
 * list_splice - join two lists
 * @param list   the new list to add.
 * @param head   the place to add it in the first list.
 */
static INLINE void list_splice(struct list_head *list, struct list_head *head)
{
	if (!list_empty(list))
		__list_splice(list, head);
}

/**
 * list_splice_init - join two lists and reinitialize the emptied list.
 * @param list   the new list to add.
 * @param head   the place to add it in the first list.
 *
 * The list at list is reinitialized
 */
static INLINE void list_splice_init(struct list_head *list,
				    struct list_head *head)
{
	if (!list_empty(list)) {
		__list_splice(list, head);
		INIT_LIST_HEAD(list);
	}
}

/**
 * list_entry - get the struct for this entry
 * @param ptr     the &struct list_head pointer.
 * @param type    the type of the struct this is embedded in.
 * @param member  the name of the list_struct within the struct.
 */
#define list_entry(ptr, type, member) \
	_list_container_of(ptr, type, member)

/**
 * list_for_each	-	iterate over a list
 * @param pos	the &struct list_head to use as a loop counter.
 * @param head	the head for your list.
 */
#define list_for_each(pos, head) \
	for (pos = (head)->next; pos != (head); pos = pos->next)

/**
 * __list_for_each	-	iterate over a list
 * @param pos	the &struct list_head to use as a loop counter.
 * @param head	the head for your list.
 *
 * This variant differs from list_for_each() in that it's the
 * simplest possible list iteration code, no ing is done.
 * Use this for code that knows the list to be very short (empty
 * or 1 entry) most of the time.
 */
#define __list_for_each(pos, head) \
	for (pos = (head)->next; pos != (head); pos = pos->next)

/**
 * list_for_each_prev	-	iterate over a list backwards
 * @param pos	the &struct list_head to use as a loop counter.
 * @param head	the head for your list.
 */
#define list_for_each_prev(pos, head) \
	for (pos = (head)->prev; pos != (head); pos = pos->prev)

/**
 * list_for_each_safe	-	iterate over a list safe against removal of list entry
 * @param pos	the &struct list_head to use as a loop counter.
 * @param n	another &struct list_head to use as temporary storage
 * @param head	the head for your list.
 */
#define list_for_each_safe(pos, n, head) \
	for (pos = (head)->next, n = pos->next; pos != (head); \
		pos = n, n = pos->next)

/**
 * list_for_each_entry	-	iterate over list of given type
 * @param type    the type of the struct where the listhead is embedded in
 * @param pos     the type * to use as a loop counter.
 * @param head    the head for your list.
 * @param member  the name of the list_struct within the struct.
 */
#define list_for_each_entry(type, pos, head, member)				\
	for (pos = list_entry((head)->next, type, member);	\
	     &pos->member != (head); 					\
	     pos = list_entry(pos->member.next, type, member))

/**
 * list_for_each_entry_reverse - iterate backwards over list of given type.
 * @param type    the type of the struct where the listhead is embedded in
 * @param pos     the type * to use as a loop counter.
 * @param head    the head for your list.
 * @param member  the name of the list_struct within the struct.
 */
#define list_for_each_entry_reverse(type, pos, head, member)			\
	for (pos = list_entry((head)->prev, type, member);	\
	     &pos->member != (head); 					\
	     pos = list_entry(pos->member.prev, type, member))


/**
 * list_for_each_entry_safe - iterate over list of given type safe against removal of list entry
 * @param type    the type of the struct where the listhead is embedded in
 * @param pos     the type * to use as a loop counter.
 * @param n       another type * to use as temporary storage
 * @param head    the head for your list.
 * @param member  the name of the list_struct within the struct.
 */
#define list_for_each_entry_safe(type, pos, n, head, member)			\
	for (pos = list_entry((head)->next, type, member),	\
		n = list_entry(pos->member.next, type, member);	\
	     &pos->member != (head); 					\
	     pos = n, n = list_entry(n->member.next, type, member))


#endif
