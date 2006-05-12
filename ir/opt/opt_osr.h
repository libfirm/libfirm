/**
 * Project:     libFIRM
 * File name:   ir/opt/opt_osr.h
 * Purpose:     Operator Strength Reduction,
 *              Keith D. Cooper, L. Taylor Simpson, Christopher A. Vick
 * Author:      Michael Beck
 * Modified by:
 * Created:     12.5.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _OPT_OSR_H_
#define _OPT_OSR_H_

#include "firm_types.h"

/**
 * Do the Operator Scalar Replacement optimization.
 *
 * @param irg  the graph which should be optimized
 */
void opt_osr(ir_graph *irg);

#endif /* _OPT_OSR_H_ */
