/*
 * This file is part of libFirm.
 * Copyright (C) 2014 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Generic hashset implementation
 * @author  Matthias Braun, inspiration from densehash from google sparsehash
 *          package
 * @date    17.03.2007
 *
 *
 * You have to specialize this file by defining:
 *
 * <ul>
 *  <li><b>HashSet</b>         The name of the hashset type</li>
 *  <li><b>HashSetIterator</b> The name of the hashset iterator type</li>
 *  <li><b>ValueType</b>       The type of the stored data values</li>
 *  <li><b>NullValue</b>       A special value representing no values</li>
 *  <li><b>DeletedValue</b>    A special value representing deleted entries</li>
 *  <li><b>Hash(hashset,key)</b> calculates the hash value for a given key</li>
 * </ul>
 *
 * Note that by default it is assumed that the data values themselves are used
 * as keys. However you can change that with additional defines:
 *
 * <ul>
 *  <li><b>KeyType</b>         The type of the keys identifying data values.
 *                             Defining this implies, that a data value contains
 *                             more than just the key.</li>
 *  <li><b>GetKey(value)</b>   Extracts the key from a data value</li>
 *  <li><b>KeysEqual(hashset,key1,key2)</b>  Tests whether 2 keys are equal</li>
 *  <li><b>DO_REHASH</b>       Instead of storing the hash-values, recalculate
 *                             them on demand from the datavalues. (useful if
 *                             calculating the hash-values takes less time than
 *                             a memory access)</li>
 * </ul>
 *
 * You can further fine tune your hashset by defining the following:
 *
 * <ul>
 *  <li><b>JUMP(num_probes)</b> The probing method</li>
 *  <li><b>Alloc(count)</b>     Allocates count hashset entries (NOT bytes)</li>
 *  <li><b>Free(ptr)</b>        Frees a block of memory allocated by Alloc</li>
 *  <li><b>SetRangeEmpty(ptr,count)</b> Efficiently sets a range of elements to
 *                                      the Null value</li>
 *  <li><b>ADDITIONAL_DATA<b>   Additional fields appended to the hashset struct</li>
 * </ul>
 */
#ifdef HashSet

#include <limits.h>
#include <stdlib.h>
#include <assert.h>

#include "bitfiddle.h"

/* quadratic probing */
#ifndef JUMP
#define JUMP(num_probes)      (num_probes)
#endif /* JUMP */

#ifndef Hash
#define ID_HASH
#define Hash(self,key)        ((unsigned)(((char *)key) - (char *)0))
#endif /* Hash */

#ifdef DO_REHASH
#define HashSetEntry                   ValueType
#define EntrySetHash(entry,new_hash)   ((void)0)
#define EntryGetHash(self,entry)       Hash(self, GetKey(entry))
#define EntryGetValue(entry)           (entry)
#else /* ! DO_REHASH */
#define EntryGetHash(self,entry)       (entry).hash
#define EntrySetHash(entry,new_hash)   (entry).hash = (new_hash)
#define EntryGetValue(entry)           (entry).data
#endif /* DO_REHASH */

#ifndef Alloc
#include "xmalloc.h"
#define Alloc(size) XMALLOCN(HashSetEntry, (size))
#define Free(ptr)      free(ptr)
#endif /* Alloc */

#ifdef ID_HASH
#define FindReturnValue                 bool
#define GetFindReturnValue(entry,found) (found)
#define NullReturnValue                 false
#define InsertReturnValue(findreturn)   !(findreturn)
#else /* ! ID_HASH */
#ifdef SCALAR_RETURN
#define FindReturnValue                 ValueType
#define GetFindReturnValue(entry,found) EntryGetValue(entry)
#define NullReturnValue                 NullValue
#else
#define FindReturnValue                 ValueType*
#define GetFindReturnValue(entry,found) & EntryGetValue(entry)
#define NullReturnValue                 & NullValue
#endif
#endif /* ID_HASH */

#ifndef InsertReturnValue
#define InsertReturnValue(findreturn)   findreturn
#endif

#ifndef KeyType
#define KeyType                  ValueType
#define GetKey(value)            (value)
#define InitData(self,value,key) (value) = (key)
#endif /* KeyType */

#ifndef ConstKeyType
#define ConstKeyType             const KeyType
#endif /* ConstKeyType */

#ifndef EntrySetEmpty
#define EntrySetEmpty(entry)    EntryGetValue(entry) = NullValue
#endif /* EntrySetEmpty */
#ifndef EntrySetDeleted
#define EntrySetDeleted(entry)  EntryGetValue(entry) = DeletedValue
#endif /* EntrySetDeleted */
#ifndef EntryIsEmpty
#define EntryIsEmpty(entry)     (EntryGetValue(entry) == NullValue)
#endif /* EntryIsEmpty */
#ifndef EntryIsDeleted
#define EntryIsDeleted(entry)   (EntryGetValue(entry) == DeletedValue)
#endif /* EntryIsDeleted */
#ifndef SetRangeEmpty
#define SetRangeEmpty(ptr,size)                \
{                                              \
	size_t _i;                                 \
	size_t _size = (size);                     \
	HashSetEntry *entries = (ptr);             \
	for (_i = 0; _i < _size; ++_i) {            \
		HashSetEntry *entry = & entries[_i];   \
		EntrySetEmpty(*entry);                 \
	}                                          \
}
#endif /* SetRangeEmpty */

#ifndef HT_OCCUPANCY_FLT
/** how full before we double size */
#define HT_OCCUPANCY_FLT(x) ((x)/2)
#endif /* HT_OCCUPANCY_FLT */
#ifndef HT_1_DIV_OCCUPANCY_FLT
#define HT_1_DIV_OCCUPANCY_FLT 2
#endif

#ifndef HT_EMPTY_FLT
/** how empty before we half size */
#define HT_EMPTY_FLT(x)     ((x)/5)
#endif /* HT_EMPTY_FLT */

#ifndef HT_MIN_BUCKETS
/** default smallest bucket size */
#define HT_MIN_BUCKETS    32
#endif /* HT_MIN_BUCKETS */

#define ILLEGAL_POS       ((size_t)-1)

#ifdef hashset_size
/**
 * Returns the number of elements in the hashset
 */
size_t hashset_size(const HashSet *self)
{
	return self->num_elements - self->num_deleted;
}
#else
static inline size_t hashset_size(const HashSet *self)
{
	return self->num_elements - self->num_deleted;
}
#endif

/**
 * Inserts an element into a hashset without growing the set (you have to make
 * sure there's enough room for that.
 * @returns  previous value if found, NullValue otherwise
 * @note also see comments for hashset_insert()
 * @internal
 */
static inline FindReturnValue insert_nogrow(HashSet *self, KeyType key)
{
	size_t   num_probes  = 0;
	size_t   num_buckets = self->num_buckets;
	size_t   hashmask    = num_buckets - 1;
	unsigned hash        = Hash(self, key);
	size_t   bucknum     = hash & hashmask;
	size_t   insert_pos  = ILLEGAL_POS;

	assert((num_buckets & (num_buckets - 1)) == 0);

	for (;;) {
		HashSetEntry *entry = & self->entries[bucknum];

		if (EntryIsEmpty(*entry)) {
			size_t p;
			HashSetEntry *nentry;

			if (insert_pos != ILLEGAL_POS) {
				p = insert_pos;
			} else {
				p = bucknum;
			}

			nentry = &self->entries[p];
			InitData(self, EntryGetValue(*nentry), key);
			EntrySetHash(*nentry, hash);
			self->num_elements++;
			return GetFindReturnValue(*nentry, false);
		}
		if (EntryIsDeleted(*entry)) {
			if (insert_pos == ILLEGAL_POS)
				insert_pos = bucknum;
		} else if (EntryGetHash(self, *entry) == hash) {
			if (KeysEqual(self, GetKey(EntryGetValue(*entry)), key)) {
				// Value already in the set, return it
				return GetFindReturnValue(*entry, true);
			}
		}

		++num_probes;
		bucknum = (bucknum + JUMP(num_probes)) & hashmask;
		assert(num_probes < num_buckets);
	}
}

/**
 * calculate shrink and enlarge limits
 * @internal
 */
static inline void reset_thresholds(HashSet *self)
{
	self->enlarge_threshold = (size_t) HT_OCCUPANCY_FLT(self->num_buckets);
	self->shrink_threshold  = (size_t) HT_EMPTY_FLT(self->num_buckets);
	self->consider_shrink   = 0;
}

#ifndef HAVE_OWN_RESIZE
/**
 * Inserts an element into a hashset under the assumption that the hashset
 * contains no deleted entries and the element doesn't exist in the hashset yet.
 * @internal
 */
static void insert_new(HashSet *self, unsigned hash, ValueType value)
{
	size_t num_probes  = 0;
	size_t num_buckets = self->num_buckets;
	size_t hashmask    = num_buckets - 1;
	size_t bucknum     = hash & hashmask;
	size_t insert_pos  = ILLEGAL_POS;

	//assert(value != NullValue);

	for (;;) {
		HashSetEntry *entry = & self->entries[bucknum];

		if (EntryIsEmpty(*entry)) {
			size_t        p;
			HashSetEntry *nentry;

			if (insert_pos != ILLEGAL_POS) {
				p = insert_pos;
			} else {
				p = bucknum;
			}
			nentry = &self->entries[p];

			EntryGetValue(*nentry) = value;
			EntrySetHash(*nentry, hash);
			self->num_elements++;
			return;
		}
		assert(!EntryIsDeleted(*entry));

		++num_probes;
		bucknum = (bucknum + JUMP(num_probes)) & hashmask;
		assert(num_probes < num_buckets);
	}
}

/**
 * Resize the hashset
 * @internal
 */
static inline void resize(HashSet *self, size_t new_size)
{
	size_t num_buckets = self->num_buckets;
	size_t i;
	HashSetEntry *old_entries = self->entries;
	HashSetEntry *new_entries;

	/* allocate a new array with double size */
	new_entries = Alloc(new_size);
	SetRangeEmpty(new_entries, new_size);

	/* use the new array */
	self->entries      = new_entries;
	self->num_buckets  = new_size;
	self->num_elements = 0;
	self->num_deleted  = 0;
#ifndef NDEBUG
	self->entries_version++;
#endif
	reset_thresholds(self);

	/* reinsert all elements */
	for (i = 0; i < num_buckets; ++i) {
		HashSetEntry *entry = & old_entries[i];
		if (EntryIsEmpty(*entry) || EntryIsDeleted(*entry))
			continue;

		insert_new(self, EntryGetHash(self, *entry), EntryGetValue(*entry));
	}

	/* now we can free the old array */
	Free(old_entries);
}
#else

/* resize must be defined outside */
static inline void resize(HashSet *self, size_t new_size);

#endif

/**
 * grow the hashset if adding 1 more elements would make it too crowded
 * @internal
 */
static inline void maybe_grow(HashSet *self)
{
	if (LIKELY(self->num_elements + 1 <= self->enlarge_threshold))
		return;

	size_t resize_to;
	if (self->num_elements - self->num_deleted + 2 > self->enlarge_threshold) {
		/* double table size */
		resize_to = self->num_buckets * 2;
		if (resize_to <= self->num_buckets) {
			abort();
		}
	} else {
		/* no need to resize, we just clean up the deleted entries */
		resize_to = self->num_buckets;
	}
	resize(self, resize_to);
}

/**
 * shrink the hashset if it is only sparsely filled
 * @internal
 */
static inline void maybe_shrink(HashSet *self)
{
	size_t size;
	size_t resize_to;

	if (!self->consider_shrink)
		return;

	self->consider_shrink = 0;
	size                  = hashset_size(self);
	if (size <= HT_MIN_BUCKETS)
		return;

	if (LIKELY(size > self->shrink_threshold))
		return;

	resize_to = ceil_po2(size);

	if (resize_to < 4)
		resize_to = 4;

	resize(self, resize_to);
}

#ifdef hashset_insert
/**
 * Insert an element into the hashset. If no element with the given key exists yet,
 * then a new one is created and initialized with the InitData function.
 * Otherwise the existing element is returned (for hashs where key is equal to
 * value, nothing is returned.)
 *
 * @param self   the hashset
 * @param key    the key that identifies the data
 * @returns      the existing or newly created data element (or nothing in case of hashs where keys are the while value)
 */
FindReturnValue hashset_insert(HashSet *self, KeyType key)
{
#ifndef NDEBUG
	self->entries_version++;
#endif

	maybe_shrink(self);
	maybe_grow(self);
	return InsertReturnValue(insert_nogrow(self, key));
}
#endif

#ifdef hashset_find
/**
 * Searches for an element with key @p key.
 *
 * @param self      the hashset
 * @param key       the key to search for
 * @returns         the found value or NullValue if nothing was found
 */
FindReturnValue hashset_find(const HashSet *self, ConstKeyType key)
{
	size_t   num_probes  = 0;
	size_t   num_buckets = self->num_buckets;
	size_t   hashmask    = num_buckets - 1;
	unsigned hash        = Hash(self, key);
	size_t   bucknum     = hash & hashmask;

	for (;;) {
		HashSetEntry *entry = & self->entries[bucknum];

		if (EntryIsEmpty(*entry)) {
			return NullReturnValue;
		}
		if (EntryIsDeleted(*entry)) {
			// value is deleted
		} else if (EntryGetHash(self, *entry) == hash) {
			if (KeysEqual(self, GetKey(EntryGetValue(*entry)), key)) {
				// found the value
				return GetFindReturnValue(*entry, true);
			}
		}

		++num_probes;
		bucknum = (bucknum + JUMP(num_probes)) & hashmask;
		assert(num_probes < num_buckets);
	}
}
#endif

#ifdef hashset_remove
/**
 * Removes an element from a hashset. Does nothing if the set doesn't contain
 * the element.
 *
 * @param self    the hashset
 * @param key     key that identifies the data to remove
 */
void hashset_remove(HashSet *self, ConstKeyType key)
{
	size_t   num_probes  = 0;
	size_t   num_buckets = self->num_buckets;
	size_t   hashmask    = num_buckets - 1;
	unsigned hash        = Hash(self, key);
	size_t   bucknum     = hash & hashmask;

#ifndef NDEBUG
	self->entries_version++;
#endif

	for (;;) {
		HashSetEntry *entry = & self->entries[bucknum];

		if (EntryIsEmpty(*entry)) {
			return;
		}
		if (EntryIsDeleted(*entry)) {
			// entry is deleted
		} else if (EntryGetHash(self, *entry) == hash) {
			if (KeysEqual(self, GetKey(EntryGetValue(*entry)), key)) {
				EntrySetDeleted(*entry);
				self->num_deleted++;
				self->consider_shrink = 1;
				return;
			}
		}

		++num_probes;
		bucknum = (bucknum + JUMP(num_probes)) & hashmask;
		assert(num_probes < num_buckets);
	}
}
#endif

/**
 * Initializes hashset with a specific size
 * @internal
 */
static inline void init_size(HashSet *self, size_t initial_size)
{
	if (initial_size < 4)
		initial_size = 4;

	self->entries         = Alloc(initial_size);
	SetRangeEmpty(self->entries, initial_size);
	self->num_buckets     = initial_size;
	self->consider_shrink = 0;
	self->num_elements    = 0;
	self->num_deleted     = 0;
#ifndef NDEBUG
	self->entries_version = 0;
#endif
#ifdef ADDITIONAL_INIT
	ADDITIONAL_INIT
#endif

	reset_thresholds(self);
}

#ifdef hashset_init
/**
 * Initializes a hashset with the default size. The memory for the set has to
 * already allocated.
 */
void hashset_init(HashSet *self)
{
	init_size(self, HT_MIN_BUCKETS);
}
#endif

#ifdef hashset_destroy
/**
 * Destroys a hashset, freeing all used memory (except the memory for the
 * HashSet struct itself).
 */
void hashset_destroy(HashSet *self)
{
#ifdef ADDITIONAL_TERM
	ADDITIONAL_TERM
#endif
	Free(self->entries);
#ifndef NDEBUG
	self->entries = NULL;
#endif
}
#endif

#ifdef hashset_init_size
/**
 * Initializes a hashset expecting expected_element size.
 */
void hashset_init_size(HashSet *self, size_t expected_elements)
{
	size_t needed_size;
	size_t po2size;

	if (expected_elements >= UINT_MAX/2) {
		abort();
	}

	needed_size = expected_elements * HT_1_DIV_OCCUPANCY_FLT;
	po2size     = ceil_po2(needed_size);
	init_size(self, po2size);
}
#endif

#ifdef hashset_iterator_init
/**
 * Initializes a hashset iterator. The memory for the allocator has to be
 * already allocated.
 * @note it is not allowed to remove or insert elements while iterating
 */
void hashset_iterator_init(HashSetIterator *self, const HashSet *hashset)
{
	self->current_bucket = hashset->entries - 1;
	self->end            = hashset->entries + hashset->num_buckets;
#ifndef NDEBUG
	self->set             = hashset;
	self->entries_version = hashset->entries_version;
#endif
}
#endif

#ifdef hashset_iterator_next
/**
 * Returns the next value in the iterator or NULL if no value is left
 * in the hashset.
 * @note it is not allowed to remove or insert elements while iterating
 */
ValueType hashset_iterator_next(HashSetIterator *self)
{
	HashSetEntry *current_bucket = self->current_bucket;
	HashSetEntry *end            = self->end;

	/* using hashset_insert or hashset_remove is not allowed while iterating */
	assert(self->entries_version == self->set->entries_version);

	do {
		current_bucket++;
		if (current_bucket >= end)
			return NullValue;
	} while (EntryIsEmpty(*current_bucket) || EntryIsDeleted(*current_bucket));

	self->current_bucket = current_bucket;
	return EntryGetValue(*current_bucket);
}
#endif

#ifdef hashset_remove_iterator
/**
 * Removes the element the iterator points to. Removing an element a second time
 * has no result.
 */
void hashset_remove_iterator(HashSet *self, const HashSetIterator *iter)
{
	HashSetEntry *entry = iter->current_bucket;

	/* iterator_next needs to have been called at least once */
	assert(entry >= self->entries);
	/* needs to be on a valid element */
	assert(entry < self->entries + self->num_buckets);

	if (EntryIsDeleted(*entry))
		return;

	EntrySetDeleted(*entry);
	self->num_deleted++;
	self->consider_shrink = 1;
}
#endif

#endif
