/*
 * Project:     libFIRM
 * File name:   ir/opt/return.h
 * Purpose:     normalize returns
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file return.h
 *
 * Normalize returns.
 *
 * @author Michael Beck
 */
#ifndef _RETURN_H_
#define _RETURN_H_

#include "irgraph.h"

/**
 * Normalize the Returns of a graph by creating a new End block
 * with One Return(Phi).
 * This is the prefered input for the if-conversion.
 *
 * In pseudocode, it means:
 *
 * if (a)
 *   return b;
 * else
 *   return c;
 *
 * is transformed into
 *
 * if (a)
 *   res = b;
 * else
 *   res = c;
 * return res;
 */
void normalize_one_return(ir_graph *irg);

/**
 * Normalize the Returns of a graph by moving
 * the Returns upwards as much as possible.
 * This might be prefered for code generation.
 *
 * In pseudocode, it means:
 *
 * if (a)
 *   res = b;
 * else
 *   res = c;
 * return res;
 *
 * is transformed into
 *
 * if (a)
 *   return b;
 * else
 *   return c;
 */
void normalize_n_returns(ir_graph *irg);

#endif /* _RETURN_H_ */
