/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @date    16.03.2007
 * @brief   Generic hashset functions
 * @author  Matthias Braun
 *
 * You have to specialize this header by defining HashSet, HashSetIterator and
 * ValueType
 */
#ifdef HashSet

#include <stdlib.h>

#ifdef DO_REHASH
#define HashSetEntry ValueType
#else
typedef struct HashSetEntry {
	ValueType data;
	unsigned hash;
} HashSetEntry;
#endif

struct HashSet {
	HashSetEntry *entries;
	size_t num_buckets;
	size_t enlarge_threshold;
	size_t shrink_threshold;
	size_t num_elements;
	size_t num_deleted;
	int consider_shrink;
#ifndef NDEBUG
	unsigned entries_version;
#endif
#ifdef ADDITIONAL_DATA
	ADDITIONAL_DATA
#endif
};

#ifdef HashSetIterator
struct HashSetIterator {
	HashSetEntry *current_bucket;
	HashSetEntry *end;
#ifndef NDEBUG
	const struct HashSet *set;
	unsigned entries_version;
#endif
};
#endif

#ifdef DO_REHASH
#undef HashSetEntry
#endif

#endif
