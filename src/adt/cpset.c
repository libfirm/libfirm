/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Custom pointer set
 * @author  Matthias Braun
 *
 * This implements a set of pointers which allows to specify custom callbacks
 * for comparing and hashing its elements.
 */
#include "cpset.h"

#define HashSet                   cpset_t
#define HashSetIterator           cpset_iterator_t
#define HashSetEntry              cpset_hashset_entry_t
#define ValueType                 void*
#define NullValue                 NULL
#define DeletedValue              ((void*)-1)
#define Hash(this,key)            this->hash_function(key)
#define KeysEqual(this,key1,key2) this->cmp_function(key1, key2)
#define SCALAR_RETURN
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof(cpset_hashset_entry_t))

void cpset_init_(cpset_t *self);
#define hashset_init            cpset_init_
void cpset_init_size_(cpset_t *self, size_t expected_elems);
#define hashset_init_size       cpset_init_size_
#define hashset_destroy         cpset_destroy
#define hashset_insert          cpset_insert
#define hashset_remove          cpset_remove
#define hashset_find            cpset_find
#define hashset_size            cpset_size
#define hashset_iterator_init   cpset_iterator_init
#define hashset_iterator_next   cpset_iterator_next
#define hashset_remove_iterator cpset_remove_iterator

#include "hashset.c.h"

void cpset_init(cpset_t *this_, cpset_hash_function hash_function,
                cpset_cmp_function cmp_function)
{
	this_->hash_function = hash_function;
	this_->cmp_function = cmp_function;
	cpset_init_(this_);
}

void cpset_init_size(cpset_t *this_, cpset_hash_function hash_function,
                     cpset_cmp_function cmp_function, size_t expected_elems)
{
	this_->hash_function = hash_function;
	this_->cmp_function = cmp_function;
	cpset_init_size_(this_, expected_elems);
}
