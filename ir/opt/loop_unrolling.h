/*
 * Project:     libFIRM
 * File name:   ir/opt/loop_unrolling.h
 * Purpose:     Loop unrolling.
 * Author:      Beyhan Veliev
 * Modified by:
 * Created:     16.11.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file loop_unrolling.h
 *
 * Loop unrolling.
 *
 * @author Beyhan Veliev
 */
#ifndef _LOOP_UNROLLING_H_
#define _LOOP_UNROLLING_H_

#include "irgraph.h"

/**
 * Do Loop unrolling in the given graph.
 */
void optimize_loop_unrolling(ir_graph *irg);

#endif  /* _LOOP_UNROLLING_H_ */
