/**
 * Inverse operations
 * @author Adam Szalkowski
 * @date 2006-05-08
 */

#ifndef BE_INVERSE_H_
#define BE_INVERSE_H_

typedef struct be_inverse_t_ {
  int        n;
  int        costs;

  /** nodes for this inverse operation. shall be in
   *  schedule order. last element is the target value
   */
  ir_node  **nodes;
} be_inverse_t;

/**
 * Returns an inverse operation which yields the i-th argument
 * of the given node as result.
 *
 * @param irn       The original operation
 * @param i         Index of the argument we want the inverse oparation to yield
 * @param inverse   struct to be filled with the resulting inverse op
 * @param obstack   The obstack to use for allocation of the returned nodes array
 */
be_inverse_t *
be_get_inverse(ir_node * irn, int i, be_inverse_t * inverse, struct obstack * obstack);

#endif
