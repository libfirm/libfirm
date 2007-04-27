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
 * File name:   ir/opt/escape_ana.h
 * Purpose:     escape analysis and optimization
 * Author:      Michael Beck
 * Modified by:
 * Created:     03.11.2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2005 Universität Karlsruhe
 */
#ifndef _IR_OPT_ESCAPE_ANA_H_
#define _IR_OPT_ESCAPE_ANA_H_

#include "firm_types.h"

/**
 * Do simple and fast escape analysis for one graph.
 *
 * @param irg  the graph
 */
void escape_enalysis_irg(ir_graph *irg);

/**
 * Do simple and fast escape analysis for all graphs.
 *
 * This optimization implements a simple and fast but inexact
 * escape analysis. Some addresses might be marked as 'escaped' even
 * if they are not.
 * The advantage is a low memory footprint and fast speed.
 *
 * @param run_scalar_replace  if this flag in non-zero, scalar
 *                            replacement optimization is run on graphs with removed
 *                            allocation
 *
 * This optimization removes allocation which are not used (rare) and replace
 * allocation that can be proved dead at the end of the graph which stack variables.
 *
 * The creation of stack variable allows scalar replacement to be run only
 * on those graphs that have been changed.
 *
 * This is most effective on Java where no other stack variables exists.
 */
void escape_analysis(int run_scalar_replace);

#endif /* _IR_OPT_ESCAPE_ANA_H_ */
