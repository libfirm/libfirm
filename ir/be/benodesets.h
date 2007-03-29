/**
 * A lightweight wrapper around pset to store IR nodes.
 * In some algorithms we want a more deterministic behavior
 * which the pset_ptr did not guarantee due to it's hash function
 */
#ifndef _BENODESETS_H
#define _BENODESETS_H

#include "firm_types.h"
#include "pset.h"

typedef struct pset nodeset;

/**
 * Calculates a hash value for a node.
 */
unsigned nodeset_hash(const ir_node *n);

/**
 * Creates a new nodeset.
 *
 * @param slots   Initial number of collision chains.  I.e., #slots
 *                different keys can be hashed without collisions.
 *
 * @returns
 *    created nodeset
 */
static INLINE nodeset *new_nodeset(int slots)
{
	return new_pset(pset_default_ptr_cmp, slots);
}

/*
 * Define some convenience macros.
 */
#define new_nodeset_default()    new_nodeset(64)

/**
 * Deletes a nodeset.
 *
 * @param nset   the nodeset
 *
 * @note
 *    This does NOT delete the elements of this node set, just it's pointers!
 */
static INLINE void del_nodeset(nodeset *nset)
{
	del_pset(nset);
}

/**
 * Returns the number of nodes in a nodeset.
 *
 * @param nset   the nodeset
 */
static INLINE int nodeset_count(nodeset *nset)
{
	return pset_count(nset);
}

/**
 * Searches a node in a node set.
 *
 * @param nset  the pset to search in
 * @param key   the node to search
 *
 * @return
 *    the pointer of the found node in the nodeset or NULL if it was not found
 */
static INLINE ir_node *nodeset_find(nodeset *nset, ir_node *key)
{
	return (ir_node *) pset_find(nset, key, nodeset_hash(key));
}

/**
 * Inserts a node into a pset.
 *
 * @param nset  the nodeset to insert in
 * @param key   a pointer to the element to be inserted
 *
 * @return a pointer to the inserted element
 *
 * @note
 *    It is not possible to insert an element more than once. If an element
 *    that should be inserted is already in the set, this functions does
 *    nothing but returning its already existing set_entry.
 */
static INLINE ir_node *nodeset_insert(nodeset *nset, ir_node *key)
{
	return (ir_node *) pset_insert(nset, key, nodeset_hash(key));
}

/**
 * Removes a node from a nodeset.
 *
 * @param nset  the nodeset to delete in
 * @param key   a pointer to the element to be deleted
 *
 * @return
 *    the pointer to the removed element
 *
 * @remark
 *    The current implementation did not allow to remove non-existing elements.
 *    @@@ so, does it do now?
 *    Further, it is allowed to remove elements during an iteration
 *    including the current one.
 */
static INLINE ir_node *nodeset_remove(nodeset *nset, ir_node *key)
{
	return (ir_node *) pset_remove(nset, key, nodeset_hash(key));
}

/**
 * Returns the first node of a nodeset.
 *
 * @param nset  the nodeset to iterate
 *
 * @return a node or NULL if the set is empty
 */
static INLINE ir_node *nodeset_first(nodeset *nset)
{
	return (ir_node *) pset_first(nset);
}

/**
 * Returns the next node of a nodeset.
 *
 * @param nset  the nodeset to iterate
 *
 * @return a node or NULL if the iteration is finished
 */
static INLINE ir_node *nodeset_next(nodeset *nset)
{
	return (ir_node *) pset_next(nset);
}

/**
 * Breaks the iteration of a set. Must be called before
 * the next nodeset_first() call if the iteration was NOT
 * finished.
 *
 * @param nset  the nodeset
 */
static INLINE void nodeset_break(nodeset *nset)
{
	pset_break(nset);
}

/**
 * Iterate over a node set.
 *
 * @param nset  the nodeset
 * @param irn   the iterator node
 */
#define foreach_nodeset(nset, irn)	for (irn = nodeset_first(nset); irn; irn = nodeset_next(nset))

#endif /* _BENODESETS_H */
