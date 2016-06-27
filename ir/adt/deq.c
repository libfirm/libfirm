/*
 * This file is part of libFirm.
 * Copyright (C) 2016 Matthias Braun
 */
#include "deq.h"

#include <stdlib.h>
#include <string.h>

void deq_init(deq_t *deq)
{
	deq_block_t *block = malloc(DEQ_BLOCK_SIZE);
	memset(block, 0, sizeof(deq_block_t));
	deq->left_end  = block;
	deq->right_end = block;
}

void deq_free(deq_t *deq)
{
	for (deq_block_t *block = deq->left_end; block != NULL;
	     block = block->next_right) {
		free(block);
	}
#ifndef NDEBUG
	deq->left_end = NULL;
	deq->right_end = NULL;
#endif
}

deq_block_t *deq_append_block_left_priv(deq_t *deq)
{
	deq_block_t *block = malloc(DEQ_BLOCK_SIZE);
	block->l          = DEQ_BLOCK_DATA_SIZE;
	block->r          = DEQ_BLOCK_DATA_SIZE;
	block->next_left  = NULL;
	block->next_right = deq->left_end;

	deq->left_end = block;
	return block;
}

deq_block_t *deq_append_block_right_priv(deq_t *deq)
{
	deq_block_t *block = malloc(DEQ_BLOCK_SIZE);
	block->l          = 0;
	block->r          = 0;
	block->next_left  = deq->right_end;
	block->next_right = NULL;

	deq->right_end = block;
	return block;
}

void deq_free_left_end_block_priv(deq_t *deq)
{
	deq_block_t *const block = deq->left_end;
	assert(block->l == block->r);
	deq_block_t *const next_right = block->next_right;
	if (next_right != NULL) {
		deq->left_end = block->next_right;
		free(block);
	}
}

void deq_free_right_end_block_priv(deq_t *deq)
{
	deq_block_t *block = deq->right_end;
	assert(block->r == block->l);
	deq_block_t *const next_left = block->next_left;
	if (next_left != NULL) {
		deq->right_end = next_left;
		free(block);
	}
}
