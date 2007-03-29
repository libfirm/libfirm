#include "config.h"

#include "cpset.h"

#define HashSet                   cpset_t
#define HashSetIterator           cpset_iterator_t
#define HashSetEntry              cpset_hashset_entry_t
#define ValueType                 void*
#define NullValue                 NULL
#define DeletedValue              ((void*)-1)
#define Hash(this,value)          this->hash_function(value)
#define KeysEqual(this,key1,key2) this->cmp_function(key1, key2)
#define SetRangeEmpty(ptr,size)   memset(ptr, 0, (size) * sizeof(cpset_hashset_entry_t))

#define hashset_init            _cpset_init
#define hashset_init_size       _cpset_init_size
#define hashset_destroy         cpset_destroy
#define hashset_insert          cpset_insert
#define hashset_remove          cpset_remove
#define hashset_find            cpset_find
#define hashset_size            cpset_size
#define hashset_iterator_init   cpset_iterator_init
#define hashset_iterator_next   cpset_iterator_next
#define hashset_remove_iterator cpset_remove_iterator

#include "hashset.c"

void cpset_init(cpset_t *this, cpset_hash_function hash_function,
                cpset_cmp_function cmp_function)
{
	this->hash_function = hash_function;
	this->cmp_function = cmp_function;
	_cpset_init(this);
}

void cpset_init_size(cpset_t *this, cpset_hash_function hash_function,
                     cpset_cmp_function cmp_function, size_t expected_elems)
{
	this->hash_function = hash_function;
	this->cmp_function = cmp_function;
	_cpset_init_size(this, expected_elems);
}
