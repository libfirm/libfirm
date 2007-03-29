/**
 * @file
 * @author    Matthias Braun
 * @date      30.03.2007
 * @brief     A nodeset. This should be prefered over a simple pset, because it
              tries to guarantee deterministic behavior.
 * @version   $Id$
 */
#include "config.h"

#include "irnodeset.h"
#include "hashptr.h"

#define DO_REHASH
#define ID_HASH
#define HashSet                   ir_nodeset_t
#define HashSetIterator           ir_nodeset_iterator_t
#define ValueType                 ir_node*
#define NullValue                 NULL
#define DeletedValue              ((ir_node*)-1)
#ifdef FIRM_debug
#define Hash(this,value)          (value)->node_nr
#else
#define Hash(this,value)          HASH_PTR(value)
#endif
#define KeysEqual(this,key1,key2) (key1) == (key2)
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof((ptr)[0]))

#define hashset_init            ir_nodeset_init
#define hashset_init_size       ir_nodeset_init_size
#define hashset_destroy         ir_nodeset_destroy
#define hashset_insert          ir_nodeset_insert
#define hashset_remove          ir_nodeset_remove
#define hashset_find            _ir_nodeset_find
#define hashset_size            ir_nodeset_size
#define hashset_iterator_init   ir_nodeset_iterator_init
#define hashset_iterator_next   ir_nodeset_iterator_next
#define hashset_remove_iterator ir_nodeset_remove_iterator

#include "hashset.c"

int ir_nodeset_contains(const ir_nodeset_t *this, const ir_node *node)
{
	return _ir_nodeset_find(this, node) != NULL;
}
