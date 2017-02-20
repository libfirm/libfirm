/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @author    Matthias Braun
 * @date      30.03.2007
 * @brief     A nodeset. This should be preferred over a simple pset, because it
              tries to guarantee deterministic behavior.
 */
#include "irnodeset.h"

#include "hashptr.h"
#include "irnode_t.h"

#define DO_REHASH
#define ID_HASH
#define HashSet                   ir_nodeset_t
#define HashSetIterator           ir_nodeset_iterator_t
#define ValueType                 ir_node*
#define NullValue                 NULL
#define DeletedValue              ((ir_node*)-1)
#define Hash(this,key)            ((unsigned)((key)->node_nr))
#define KeysEqual(this,key1,key2) (key1) == (key2)
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof((ptr)[0]))

void ir_nodeset_init_(ir_nodeset_t *self);
#define hashset_init            ir_nodeset_init_
#define hashset_init_size       ir_nodeset_init_size
#define hashset_destroy         ir_nodeset_destroy
#define hashset_insert          ir_nodeset_insert
#define hashset_remove          ir_nodeset_remove
#define hashset_find            ir_nodeset_contains
#define hashset_size            ir_nodeset_size
#define hashset_iterator_init   ir_nodeset_iterator_init
#define hashset_iterator_next   ir_nodeset_iterator_next
#define hashset_remove_iterator ir_nodeset_remove_iterator

#include "hashset.c.h"

void ir_nodeset_init(ir_nodeset_t *nodeset)
{
	ir_nodeset_init_size(nodeset, 16);
}
