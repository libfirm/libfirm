/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief      Decompost control flow graph into acylic, hierarchic intervals.
 * @author     Goetz Lindenmaier
 * @date       5.11.2004
 * @version    $Id$
 * @summary
 *  Decompost control flow graph into acylic, hierarchic intervals.
 *
 *  The analysis is based on the control flow looptree.  An intervall
 *  are basically all nodes in a single ir_loop entry, i.e., basic
 *  blocks and inner loop nodes.  The analysis computes a new set of
 *  edges that link all nodes of a loop to an acyclic graph.
 *
 *  The interval analysis counts the number of exception control flow
 *  operations leaving a block.  This depends on stuff computed in
 *  execution_freqencies.
 */
#ifndef FIRM_ANA_INTERVAL_ANALYSIS_H
#define FIRM_ANA_INTERVAL_ANALYSIS_H

#include "firm_types.h"

/** The ins of regions:  regions are loops or blocks.
 *
 *  @todo: we should make a type for the regions, or reuse loop_element.
 */
int   get_region_n_ins(void *region);
void *get_region_in   (void *region, int pos);
void  add_region_in   (void *region, void *in);


/** The number of out edges of a region.
 *
 *  This number is useful for evaluation of execution frequencies.
 */
int get_region_n_outs(void *region);

/** The number of exception out edges of a region.
 *
 *  This number is useful for evaluation of execution frequencies.
 */
int get_region_n_exc_outs(void *region);

/** The control flow operation corresponding to the loop-region in at
 *  position pos.
 */
void *get_loop_cfop(void *region, int pos);


/** The algorithm to construct the interval graph.
 *
 *  Constructs the cf loop tree and leaves a valid version of it.
 *
 *  @todo: @@@ add flag that states correctness of interval analysis information
 *  to irg.
 **/
void construct_intervals(ir_graph *irg);


/** frees interval information of all graphs. */
void free_intervals(void);


/** dump a graph with the intervals. File name suffix "-interval". */
void dump_interval_graph(ir_graph *irg, const char *suffix);


#define DDMR(X) if(is_ir_node(X)) DDMN(X); else DDML(X)

#endif
