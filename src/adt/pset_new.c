/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   implementation of pset_new
 * @author  Matthias Braun
 */
#include "pset_new.h"

/** probing method: quadratic probing */
#define DO_REHASH
#define ID_HASH
#define HashSet                    pset_new_t
#define HashSetIterator            pset_new_iterator_t
#define ValueType                  void*
#define NullValue                  NULL
#define DeletedValue               ((void*)-1)
#define KeysEqual(this,key1,key2)  1
#define SetRangeEmpty(ptr,size)    memset(ptr, 0, (size) * sizeof(HashSetEntry))

#define hashset_init            pset_new_init
#define hashset_init_size       pset_new_init_size
#define hashset_destroy         pset_new_destroy
#define hashset_insert          pset_new_insert
#define hashset_remove          pset_new_remove
#define hashset_find            pset_new_contains
#define hashset_size            pset_new_size
#define hashset_iterator_init   pset_new_iterator_init
#define hashset_iterator_next   pset_new_iterator_next
#define hashset_remove_iterator pset_new_remove_iterator

#include "hashset.c.h"
