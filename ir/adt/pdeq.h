/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Matthias Braun
 */

/**
 * @file
 * @brief  Double Ended Pointer Queue.
 *
 * Convenience functions to maintain a double ended queue containing pointers.
 */
#ifndef FIRM_ADT_PDEQ_NEW_H
#define FIRM_ADT_PDEQ_NEW_H

#include "deq.h"

static inline void deq_push_pointer_left(deq_t *deq, void *ptr)
{
	void **space = (void**)deq_alloc_left(deq, sizeof(void*));
	*space = ptr;
}

static inline void deq_push_pointer_right(deq_t *deq, void *ptr)
{
	void **space = (void**)deq_alloc_right(deq, sizeof(void*));
	*space = ptr;
}

static inline void *deq_pop_pointer_left(deq_t *deq)
{
	void *result = *((void**)deq_left_end(deq));
	deq_shrink_left(deq, sizeof(void*));
	return result;
}

#define deq_pop_pointer_left(type, deq) \
	((type*)deq_pop_pointer_left(deq))

static inline void *deq_pop_pointer_right(deq_t *deq)
{
	void *result = *((void**)deq_right_end_obj(deq, sizeof(void*)));
	deq_shrink_right(deq, sizeof(void*));
	return result;
}

#define deq_pop_pointer_right(type, deq) \
	((type*)deq_pop_pointer_right(deq))


typedef struct deq_pointer_iter_t {
	deq_t       const *deq;
	deq_block_t const *block;
	void        const **block_p;
	void        const **block_end;
} deq_pointer_iter_t;

static inline void deq_pointer_iter_init(deq_pointer_iter_t *const iter,
                                         deq_t const *const deq)
{
	deq_block_t const *const block = deq_get_left_end_block(deq);
	iter->deq       = deq;
	iter->block     = block;
	iter->block_p   = (void const**)deq_block_data_begin(block);
	iter->block_end = (void const**)deq_block_data_end(block);
}

static inline bool deq_pointer_iter_at_end(deq_pointer_iter_t const *const iter)
{
	return iter->block_p == iter->block_end;
}

static inline void deq_pointer_iter_next(deq_pointer_iter_t *const iter)
{
	++iter->block_p;
	if (iter->block_p == iter->block_end) {
		deq_block_t const *const next_right = deq_block_next_right(iter->block);
		if (next_right != NULL) {
			iter->block     = next_right;
			iter->block_p   = (void const**)deq_block_data_begin(next_right);
			iter->block_end = (void const**)deq_block_data_end(next_right);
		}
	}
}

#define deq_foreach_pointer(deq, type, pointer) \
	for (bool pointer##__once = true; pointer##__once;) \
		for (deq_pointer_iter_t pointer##__iter; pointer##__once; pointer##__once = false) \
			for (deq_pointer_iter_init(&pointer##__iter, deq); \
			     !deq_pointer_iter_at_end(&pointer##__iter); \
			     deq_pointer_iter_next(&pointer##__iter)) \
				for (bool pointer##__once2 = true; pointer##__once2;) \
					for (type *pointer = *((type**)pointer##__iter.block_p); pointer##__once2; pointer##__once2 = false)

#endif
