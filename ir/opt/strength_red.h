/**
 *
 * @file strength_red.h
 *
 * Project:     libFIRM
 * File name:   ir/opt/strenth_red.h
 * Purpose:     Strength reduction.
 * Author:      Beyhan Veliev
 * Modified by:
 * Created:     22.8.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 *
 *
 *
 */


# ifndef _STRENGTH_RED_H_
# define _STRENGTH_RED_H_

# include "irgraph.h"

/** Performs strength reduction for the passed graph. */
void reduce_strength(ir_graph *irg);



#endif /* _STRENGTH_RED_H_ */
