/* Copyright (C) 2002 by Universitaet Karlsruhe
* All rights reserved.
*/

/**
 * @file irdom_t.h
 *
 * dDominator information private datastructures.
 *
 * @author Goetz Lindenmaier
 *
 */

/* $Id$ */

# ifndef _IRDOM_T_H_
# define _IRDOM_T_H_

#include "irdom.h"

/** For dominator information */
typedef struct dom_info {
  struct ir_node *idom;	/**< immediate CFG dominator */
  int pre_num;		      /**< pre-order graph-walk number */
  int dom_depth;	      /**< depth in dominator-tree */
} dom_info;

#endif /* _IRDOM_T_H_ */
