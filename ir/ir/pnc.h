
#ifndef _FIRM_PNC_H
#define _FIRM_PNC_H

/**
 * Projection numbers of compare: use for Proj nodes!
 * @remark there are numbers with normalized names below!
 */
typedef enum {
  False = 0,    /**< false */
  Eq,           /**< equal */
  Lt,           /**< less */
  Le,           /**< less or equal */
  Gt,           /**< greater */
  Ge,           /**< greater or equal */
  Lg,           /**< less or greater */
  Leg = 7,      /**< less, equal or greater = ordered */
  Uo,           /**< unordered */
  Ue,           /**< unordered or equal */
  Ul,           /**< unordered or less */
  Ule,          /**< unordered, less or equal */
  Ug,           /**< unordered or greater */
  Uge,          /**< unordered, greater or equal */
  Ne,           /**< unordered, less or greater = not equal */
  True = 15     /**< true */
  /* not_mask = Leg*/   /* bits to flip to negate comparison * @@ hack for jni interface */
} pnc_number;   /* pnc: Projection Number Cmp */
#define not_mask Leg


#endif
