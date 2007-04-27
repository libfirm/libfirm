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

/*
 * Project:     libFIRM
 * File name:   ir/opt/tailrec.h
 * Purpose:     Tail-recursion call optimization.
 * Author:      Michael Beck
 * Created:     08.06.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 */

/**
 * @file tailrec.h
 *
 * Tail-recursion call optimization.
 *
 * @author Michael Beck
 */

# ifndef _TAILREC_H_
# define _TAILREC_H_

# include "irgraph.h"

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
 */
void opt_tail_recursion(void);

# endif /* _TAILREC_H_ */
