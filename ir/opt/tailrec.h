/*
 * Project:     libFIRM
 * File name:   ir/opt/tailrec.h
 * Purpose:     Tail-recursion call optimization.
 * Author:      Michael Beck
 * Created:     08.06.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
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
