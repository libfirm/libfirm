/**
 * Iterators for the several collection types used in firm.
 * Useful for formatted and unified dumping of collections of objects.
 * @author Sebastian Hack
 * @date 29.11.2004
 */

/**
 * Check, if some memory object appears to be an iterator.
 * @param ptr SOme memory.
 * @return 1, if that memory area appears to be an iterator, 0 if not.
 */
int is_iterator(const void *ptr);

typedef struct _iterator_t {
	char magic[4];
	void *(*start)(void *collection);
	void *(*next)(void *collection, void *curr);
	void (*finish)(void *collection, void *curr);
} iterator_t;

/**
 * An iterator implementation for psets.
 */
extern const iterator_t *it_pset;

/**
 * An iterator implementation for linked lists.
 */
extern const iterator_t *it_list;
