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
 * @brief   Tail-recursion call optimization.
 * @date    08.06.2004
 * @author  Michael Beck
 * @version $Id$
 */
#ifndef FIRM_OPT_TAILREC_H
#define FIRM_OPT_TAILREC_H

#include "firm_types.h"

/**
 * Optimizes simple tail-recursion calls by
 * converting them into loops. Depends on the flag opt_tail_recursion.
 *
 * Does not work for Calls that use the exception stuff.
 *
 * @param irg   the graph to be optimized
 *
 * @return non-zero if the optimization could be applied, 0 else
 */
int opt_tail_rec_irg(ir_graph *irg);

/*
 * Optimize tail-recursion calls for all IR-Graphs.
 * Depends on the flag opt_tail_recursion.
 */
void opt_tail_recursion(void);

# endif /* FIRM_OPT_TAILREC_H */
