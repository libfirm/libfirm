/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Matthias Braun
 */
#include "deq.h"

#include "xmalloc.h"
#include <stdlib.h>
#include <string.h>

void deq_init(deq_t *deq)
{
	deq_block_t *block = XMALLOCF(deq_block_t, data, DEQ_BLOCK_DATA_SIZE);
	memset(block, 0, sizeof(deq_block_t));
	deq->left_end  = block;
	deq->right_end = block;
}

void deq_free(deq_t *deq)
{
	for (deq_block_t *block = deq->left_end, *next; block != NULL;
	     block = next) {
	    next = block->next_right;
		free(block);
	}
#ifndef NDEBUG
	deq->left_end = NULL;
	deq->right_end = NULL;
#endif
}

deq_block_t *deq_append_block_left_priv(deq_t *deq)
{
	deq_block_t *const left_end = deq->left_end;
	/* Special case if left and right end are in the same block and
	 * the block is empty. Leaving the right pointer at 0 would be invalid. */
	deq_block_t *newb;
	if (left_end == deq->right_end && left_end->r == left_end->l) {
		newb = left_end;
		assert(newb->next_left == NULL);
		assert(newb->next_right == NULL);
	} else {
		newb = XMALLOCF(deq_block_t, data, DEQ_BLOCK_DATA_SIZE);
		newb->next_left  = NULL;
		newb->next_right = left_end;

		left_end->next_left = newb;
		deq->left_end = newb;
	}
	newb->l = DEQ_BLOCK_DATA_SIZE;
	newb->r = DEQ_BLOCK_DATA_SIZE;
	return newb;
}

deq_block_t *deq_append_block_right_priv(deq_t *deq)
{
	deq_block_t *const right_end = deq->right_end;
	/* Special case if left and right end are in the same block and
	 * the block is empty. Leaving the right pointer at 0 would be invalid. */
	deq_block_t *newb;
	if (right_end == deq->left_end && right_end->r == right_end->l) {
		newb = right_end;
		assert(newb->next_left == NULL);
		assert(newb->next_right == NULL);
	} else {
		newb = XMALLOCF(deq_block_t, data, DEQ_BLOCK_DATA_SIZE);
		newb->next_left  = right_end;
		newb->next_right = NULL;

		right_end->next_right = newb;
		deq->right_end = newb;
	}

	newb->l = 0;
	newb->r = 0;
	return newb;
}

void deq_left_end_block_empty_priv(deq_t *deq)
{
	deq_block_t *const block      = deq->left_end;
	deq_block_t *const next_right = block->next_right;
	assert(block->l == block->r);
	if (next_right != NULL) {
		assert(block != deq->right_end);
		assert(next_right->next_left == block);
		deq->left_end = next_right;
		next_right->next_left = NULL;
		free(block);
	} else {
		assert(block == deq->right_end);
		block->l = 0;
		block->r = 0;
	}
}

void deq_right_end_block_empty_priv(deq_t *deq)
{
	deq_block_t *const block     = deq->right_end;
	deq_block_t *const next_left = block->next_left;
	assert(block->r == block->l);
	if (next_left != NULL) {
		assert(block != deq->left_end);
		assert(next_left->next_right == block);
		deq->right_end = next_left;
		next_left->next_right = NULL;
		free(block);
	} else {
		assert(block == deq->left_end);
		block->l = 0;
		block->r = 0;
	}
}
