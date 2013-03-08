/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author Tobias Rapp
 * @brief Addressable priority queue implementation based on the pairing heap
 *        datastructure.
 */
#include <stddef.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>

#include "obst.h"
#include "apqueue.h"
#include "error.h"

/**
 * An element in the apqueue.
 */
struct apqueue_el_t {
	void                *data;          /**< The actual data.*/
	int                 priority;       /**< The priority of the element. */
	bool                is_active;      /**< If the element is contained in an apqueue.*/
	struct apqueue_el_t *right_sibling; /**< Pointer to the next element. */
	struct apqueue_el_t *child;         /**< Pointer to a child element. */
	struct apqueue_el_t *parent;        /**< Pointer to the parent. */
};

/**
 * The actual apqueue datastructure.
 */
struct apqueue_t {
	size_t         length;  /**< The number of active elements. */
	apqueue_el_t   *max;    /**< The current maximum. */
	apqueue_el_t   *root;   /**< The first element in the list of root (no parents) nodes.*/
	struct obstack obst;    /**< Obstack for memory allocation. */
};

apqueue_t *new_apqueue(void)
{
	apqueue_t *res = XMALLOC(apqueue_t);
	res->length    = 0;
	res->max       = NULL;
	res->root      = NULL;

	obstack_init(&res->obst);
	return res;
}

void del_apqueue(apqueue_t *q)
{
	obstack_free(&q->obst, NULL);
	free(q);
}

apqueue_el_t* apqueue_put(apqueue_t *q, void *data, int priority)
{
	++q->length;
	/* Create new element */
	apqueue_el_t *new_el = OALLOC(&q->obst, apqueue_el_t);
	new_el->data         = data;
	new_el->priority     = priority;
	new_el->child        = NULL;
	new_el->parent       = NULL;
	new_el->is_active    = true;

	/* Insert into roots */
	new_el->right_sibling = q->root;
	q->root               = new_el;

	/* Update max if necessary */
	if (q->max == NULL || q->max->priority < priority) {
		q->max = new_el;
	}

	return new_el;
}

/**
 * For every pair of root nodes this makes one the child of the other,
 * thereby rebalancing the set of trees.
 */
static void pairwise_union(apqueue_t *q)
{
	assert(q->root != NULL);

	apqueue_el_t *first    = q->root;
	apqueue_el_t *prev = q->root;
	apqueue_el_t *second    = first->right_sibling;

	/* Do union of first and second */
	while(first != NULL && second != NULL) {
		if (first->priority >= second->priority && second != q->max) {
			/* first >= second, so make second a child of first */
			first->right_sibling  = second->right_sibling;
			second->right_sibling = first->child;
			first->child          = second;
			second->parent        = first;
			prev                  = first;

			if (first->right_sibling == NULL) break;
			second = first->right_sibling->right_sibling;
			first  = first->right_sibling;
		} else {
			/* second > first, so make first a child of second */
			if (first == q->root) {
				q->root             = second;
			} else {
				prev->right_sibling = second;
			}
			first->right_sibling    = second->child;
			second->child           = first;
			first->parent           = second;

			prev = second;
			if (second->right_sibling == NULL) break;
			first  = second->right_sibling;
			second = second->right_sibling->right_sibling;
		}
	}
}

/**
 * Finds the new maximal element from the root nodes.
 */
static void scan_new_max(apqueue_t *q)
{
	if (q->root == NULL) {
		q->max = NULL;
		return;
	}

	apqueue_el_t *index = q->root;
	apqueue_el_t *max_temp = q->root;

	while(index->right_sibling != NULL) {
		index = index->right_sibling;
		if (index->priority > max_temp->priority) {
			max_temp = index;
		}
	}
	q->max = max_temp;
}

void *apqueue_pop_front(apqueue_t *q)
{
	assert(q->length > 0 && q->max != NULL && q->root != NULL);
	assert(q->max->is_active && q->root->is_active);

	--q->length;
	apqueue_el_t *res = q->max;

	apqueue_el_t *index;

	/* Remove max from the root queue */
	if (q->max == q->root) {
		q->root = q->root->right_sibling;
	} else {
		index = q->root;
		while(index->right_sibling != q->max) {
			index = index->right_sibling;
		}
		/* index is the element before max, now remove max */
		index->right_sibling = q->max->right_sibling;
	}

	/* Put every child of max into the root queue */
	index = q->max->child;
	while (index != NULL) {
		apqueue_el_t *right = index->right_sibling;
		index->right_sibling = q->root;
		q->root = index;
		index->parent = NULL;
		index = right;
	}

	if (q->root != NULL) {
		/* Rebalance by doing pairwise union */
		pairwise_union(q);
	}

	/* Scan root list for max element */
	scan_new_max(q);

	res->is_active = false;
	return res->data;
}

size_t apqueue_length(const apqueue_t *q)
{
	return q->length;
}

int apqueue_empty(const apqueue_t *q)
{
	return q->length == 0;
}

int apqueue_contains(apqueue_el_t* address)
{
	return address->is_active;
}

int apqueue_get_priority(apqueue_el_t* address)
{
	return address->priority;
}

static void apqueue_increase_priority(apqueue_t *q, apqueue_el_t *ptr, int priority)
{
	ptr->priority = priority;

	if (ptr->parent != NULL) {
		/* Remove ptr from the list of siblings */
		apqueue_el_t *index = ptr->parent->child;
		if (index != ptr) {
			/* Find left sibling of ptr */
			while(index->right_sibling != ptr) {
				index = index->right_sibling;
			}
			index->right_sibling = ptr->right_sibling;
		} else {
			/* index and ptr are already the same, so simply exclude ptr */
			ptr->parent->child = ptr->right_sibling;
		}

		/* Insert ptr into root */
		ptr->right_sibling = q->root;
		ptr->parent = NULL;
		q->root = ptr;
	}
	if (q->max->priority <= priority) {
		q->max = ptr;
	}
}

static void apqueue_decrease_priority(apqueue_t *q, apqueue_el_t *ptr, int priority)
{
	ptr->priority = priority;

	/* Insert all children into the root list */
	apqueue_el_t *index = ptr->child;
	while (index != NULL) {
		apqueue_el_t *right = index->right_sibling;
		index->right_sibling = q->root;
		index->parent = NULL;
		q->root = index;
		index = right;
	}
	ptr->child = NULL;

	if (ptr == q->max) {
		/* In this special case, we must scan all roots for a new max */
		scan_new_max(q);
	}

	/* Rebalance the tree by pairwise union */
	pairwise_union(q);
}

void *apqueue_remove(apqueue_t *q, apqueue_el_t* address)
{
	/* increase priority of address to max, then pop_front */
	apqueue_increase_priority(q, (apqueue_el_t*)address, INT_MAX);
	return apqueue_pop_front(q);
}

void apqueue_change_priority(apqueue_t *q, apqueue_el_t* address, int priority)
{
	assert(address->is_active && "Can't change priority of already removed element");

	if (address->priority < priority) {
		apqueue_increase_priority(q, address, priority);
	} else if (address->priority > priority) {
		apqueue_decrease_priority(q, address, priority);
	}
}
