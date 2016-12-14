#ifndef _FIRM_CLONING_VECTOR_T_H_
#define _FIRM_CLONING_VECTOR_T_H_

#include "irnode_t.h"
#include "bitset.h"

typedef ir_node **cloning_vector_t;

#define cv_foreach(cv, i, val)                                                 \
	ARR_FOREACH (cv, i, ir_node *, val)                                        \
		if (!val) {                                                            \
		} else

/**
 * A Cloning Vector (CV) is a mapping from parameter index to argument node.
 * But it only contains constant arguments that are passed to important
 * parameters of the called function.
 *
 * It could also be viewed as the set of (Index, Value) for the constant
 * important arguments to a function call.
 */

/**
 * Creates a new CV for the given call, ignoring all parameters whose indices
 * are unset in callee_vips. Memory is allocated using the given obstack.
 *
 * @param  call        The call node for which to create a CV
 * @param  callee_vips A bitset that determines which argument indices to check
 * @param  obst        The obstack used to obtain all necessary memory
 * @return             The new CV
 */
cloning_vector_t cv_new(const ir_node *call, const bitset_t *callee_vips,
                        struct obstack *obst);

/**
 * Returns the number of (truthy) entries in this CV.
 *
 * @param  cv The instance to operate on.
 * @return    the number of (truthy) entries in this CV
 */
size_t cv_get_size(const cloning_vector_t cv);

/**
 * Get the value for given paramter index or NULL if not present in this CV.
 *
 * @param  cv  The instance to operate on.
 * @param  pos The parameter index for which to get the value
 * @return     The value at given index or NULL if not present in this CV
 */
ir_node *cv_get(const cloning_vector_t cv, size_t pos);

bool cv_equal(const cloning_vector_t a, const cloning_vector_t b);

unsigned cv_hash(const cloning_vector_t cv);

/**
 * This gives the new index for the argument at index `idx`, if the called
 * function was specialized and all arguments present in this CV were dropped.
 *
 * @param  cv  The instance to operate on.
 * @param  idx The index of the argument for which to get the new index
 * @return     The new index for the argument formerly at `idx`
 */
size_t cv_get_new_idx(const cloning_vector_t cv, size_t idx);

#endif
