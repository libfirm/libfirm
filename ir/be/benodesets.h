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
unsigned nodeset_hash(ir_node *n);

/**
 * Creates a new nodeset.
 *
 * @param slots   Initial number of collision chains.  I.e., #slots
 *                different keys can be hashed without collisions.
 *
 * @returns
 *    created nodeset
 */
#define new_nodeset(slots) new_pset(pset_default_ptr_cmp, (slots))

/**
 * Deletes a nodeset.
 *
 * @param nset   the nodeset
 *
 * @note
 *    This does NOT delete the elements of this node set, just it's pointers!
 */
#define del_nodeset(nset) del_pset(nset)

/**
 * Returns the number of nodes in a nodeset.
 *
 * @param nset   the nodeset
 */
#define nodeset_count(nset) pset_count(nset)

/**
 * Searches a node in a node set.
 *
 * @param nset  the pset to search in
 * @param key   the node to search
 *
 * @return
 *    the pointer of the found node in the nodeset or NULL if it was not found
 */
#define nodeset_find(nset, key)  (ir_node *)pset_find((nset), (key), nodeset_hash(key))

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
#define nodeset_insert(nset, key) (ir_node *)pset_insert((nset), (key), nodeset_hash(key))

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
#define nodeset_remove(nset, key)  (ir_node *)pset_remove((nset), (key), nodeset_hash(key))

/**
 * Returns the first node of a nodeset.
 *
 * @param nset  the nodeset to iterate
 *
 * @return a node or NULL if the set is empty
 */
#define nodeset_first(nset)  (ir_node *)pset_first(nset)

/**
 * Returns the next node of a nodeset.
 *
 * @param nset  the nodeset to iterate
 *
 * @return a node or NULL if the iteration is finished
 */
#define nodeset_next(nset)  (ir_node *)pset_next(nset)

/**
 * Breaks the iteration of a set. Must be called before
 * the next nodeset_first() call if the iteration was NOT
 * finished.
 *
 * @param nset  the nodeset
 */
#define nodeset_break(nset)  pset_break(nset)

/**
 * Iterate over a node set.
 *
 * @param nset  the nodeset
 * @param irn   the iterator node
 */
#define foreach_nodeset(nset, irn)	for (irn = nodeset_first(nset); irn; irn = nodeset_next(nset))

#endif /* _BENODESETS_H */
