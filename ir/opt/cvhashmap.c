/*
 * This file is part of libFirm.
 * Copyright (C) 2016 University of Karlsruhe.
 */

/**
 * @file
 * @author    Raphael von der Grün
 * @date      2016
 * @brief     A CV hash map.
 */
#include "cvhashmap.h"
#include "hashptr.h"

static cv_hashmap_entry_t null_cv_hashmap_entry = { NULL, NULL };

#define DO_REHASH
#define HashSet                   cv_hashmap_t
#define HashSetIterator           cv_hashmap_iterator_t
#define ValueType                 cv_hashmap_entry_t
#define NullValue                 null_cv_hashmap_entry
#define KeyType                   cloning_vector_t
#define ConstKeyType              const cloning_vector_t
#define GetKey(value)             (value).cv
#define InitData(self,value,key)  (value).cv = (key)
#define Hash(self,key)            cv_hash((key))
#define KeysEqual(self,key1,key2) cv_equal((key1), (key2))
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof((ptr)[0]))
#define EntrySetEmpty(value)      (value).cv = NULL
#define EntrySetDeleted(value)    (value).cv = (cloning_vector_t) -1
#define EntryIsEmpty(value)       ((value).cv == NULL)
#define EntryIsDeleted(value)     ((value).cv == (cloning_vector_t)-1)

void cv_hashmap_init_(cv_hashmap_t *self);
#define hashset_init            cv_hashmap_init_
#define hashset_init_size       cv_hashmap_init_size
#define hashset_destroy         cv_hashmap_destroy
cv_hashmap_entry_t *cv_hashmap_insert_(cv_hashmap_t *self,
                                               cloning_vector_t cv);
#define hashset_insert          cv_hashmap_insert_
#define hashset_remove          cv_hashmap_remove
cv_hashmap_entry_t *cv_hashmap_find_(const cv_hashmap_t *self,
                                             const cloning_vector_t cv);
#define hashset_find            cv_hashmap_find_
#define hashset_size            cv_hashmap_size
#define hashset_iterator_init   cv_hashmap_iterator_init
#define hashset_iterator_next   cv_hashmap_iterator_next
#define hashset_remove_iterator cv_hashmap_remove_iterator

#include "hashset.c.h"

void cv_hashmap_init(cv_hashmap_t *cv_hashmap)
{
	cv_hashmap_init_size(cv_hashmap, 16);
}

void *(cv_hashmap_get)(const cv_hashmap_t *self, const cloning_vector_t cv)
{
	cv_hashmap_entry_t *entry = cv_hashmap_find_(self, cv);
	return entry->data;
}

void cv_hashmap_insert(cv_hashmap_t *self, cloning_vector_t cv, void *data)
{
	cv_hashmap_entry_t *entry = cv_hashmap_insert_(self, cv);
	entry->data                   = data;
}
