/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Matthias Braun
 */

/**
 * @file
 * @brief  Double Ended Queue.
 */
#ifndef FIRM_ADT_DEQ_H
#define FIRM_ADT_DEQ_H

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>

/**
 * @ingroup adt
 * @defgroup deq Double Ended Queue
 * Double ended queue data structure. They allow efficient insertion and
 * deletion on both ends.
 * @{
 */

#define DEQ_BLOCK_SIZE        2048
#define DEQ_BLOCK_DATA_SIZE   DEQ_BLOCK_SIZE - sizeof(deq_block_t)

typedef struct deq_block_t deq_block_t;
struct deq_block_t {
	unsigned l;
	unsigned r;
	deq_block_t *next_left;
	deq_block_t *next_right;
	char data[];
};

typedef struct deq_t {
	deq_block_t *left_end;
	deq_block_t *right_end;
} deq_t;

/** @cond PRIVATE */
deq_block_t *deq_append_block_left_priv(deq_t *deq);
deq_block_t *deq_append_block_right_priv(deq_t *deq);
void deq_left_end_block_empty_priv(deq_t *deq);
void deq_right_end_block_empty_priv(deq_t *deq);
/** @endcond */

/** Initialize double ended queue \p deq and allocate a first data block. */
void deq_init(deq_t *deq);

/**
 * Free all data blocks allocated for double ended queue \p deq.
 * Further use of the datastructure is undefined until deq_init() is called.
 */
void deq_free(deq_t *deq);

/** Returns true if the double ended queue \p deq is empty, false otherwise. */
static inline bool deq_empty(deq_t const *const deq)
{
	deq_block_t const *const left_end = deq->left_end;
	return left_end == deq->right_end && left_end->l == left_end->r;
}

/** Return pointer to the object at the elft end of the double ended queue. */
static inline void *deq_left_end(deq_t const *const deq)
{
	deq_block_t *const left_end = deq->left_end;
	return left_end->data + left_end->l;
}

/** Return pointer to object at the right end of the double ended queue. */
static inline void *deq_right_end_obj(deq_t const *const deq, unsigned size)
{
	deq_block_t *const right_end = deq->right_end;
	return right_end->data + right_end->r - size;
}

/**
 * Remove object of size \p size from the left end of double ended queue \p deq.
 */
static inline void deq_shrink_left(deq_t *const deq, unsigned size)
{
	deq_block_t *const left_end = deq->left_end;
	left_end->l += size;
	if (left_end->l >= left_end->r)
		deq_left_end_block_empty_priv(deq);
}

/**
 * Remove object of size \p size from the right end of double ended queue
 * \p deq.
 */
static inline void deq_shrink_right(deq_t *const deq, unsigned size)
{
	deq_block_t *const right_end = deq->right_end;
	assert(right_end->r >= size);
	right_end->r -= size;
	if (right_end->r <= right_end->l)
		deq_right_end_block_empty_priv(deq);
}

/**
 * Allocate space for object of size \p size on the left end of the double ended
 * queue \p deq.  Returns a pointer to the newly allocated space.
 */
static inline void *deq_alloc_left(deq_t *const deq, unsigned size)
{
	assert(size < DEQ_BLOCK_DATA_SIZE);
	deq_block_t *left_end = deq->left_end;
	if (left_end->l < size)
		left_end = deq_append_block_left_priv(deq);
	left_end->l -= size;
	return left_end->data + left_end->l;
}

/**
 * Allocate space for object of size \p size on the right end of the double
 * ended queue \p deq.  Returns a pointer to the newly allocated space.
 */
static inline void *deq_alloc_right(deq_t *const deq, unsigned size)
{
	assert(size < DEQ_BLOCK_DATA_SIZE);
	deq_block_t *right_end = deq->right_end;
	if (right_end->r + size > DEQ_BLOCK_DATA_SIZE)
		right_end = deq_append_block_right_priv(deq);
	void *result = right_end->data + right_end->r;
	right_end->r += size;
	return result;
}

static inline deq_block_t const *deq_get_left_end_block(deq_t const *const deq)
{
	return deq->left_end;
}

static inline deq_block_t *deq_block_next_right(deq_block_t const *const block)
{
	return block->next_right;
}

static inline const void *deq_block_data_begin(deq_block_t const *const block)
{
	return block->data + block->l;
}

static inline const void *deq_block_data_end(deq_block_t const *const block)
{
	return block->data + block->r;
}

/** @} */

#endif
