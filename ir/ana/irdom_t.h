/*
 * Project:     libFIRM
 * File name:   ir/ana/irdom_t.h
 * Purpose:     Construct and access dominator tree -- private datastructures.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     2.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


/**
 * @file irdom_t.h
 *
 * Dominator information private datastructures.
 *
 * @author Goetz Lindenmaier
 *
 */


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
