/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief   Normalize returns.
 * @author  Michael Beck
 * @version $Id$
 */
#ifndef FIRM_OPT_RETURN_H
#define FIRM_OPT_RETURN_H

#include "firm_types.h"

/**
 * Normalize the Returns of a graph by creating a new End block
 * with One Return(Phi).
 * This is the preferred input for the if-conversion.
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
 * This might be preferred for code generation.
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

#endif /* FIRM_OPT_RETURN_H */
