/*
 * Project:     libFIRM
 * File name:   ir/ana/interval_analysis.h
 * Purpose:     Decompost control flow graph into acylic, hierarchic intervals.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     5.11.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
* @file interval_analysis.h
*
*  Decompost control flow graph into acylic, hierarchic intervals.
*
*  @author Goetz Lindenmaier
*
*  The analysis is based on the control flow looptree.  An intervall are basically
*  all nodes in a single ir_loop entry, i.e., basic blocks and inner loop nodes.
*  The analysis computes a new set of edges that link all nodes of a loop to an
*  acyclic graph.
*
*
*
*/

#ifndef _INTERVAL_ANALYSIS_H_
#define _INTERVAL_ANALYSIS_H_


#include "irloop.h"
#include "irnode.h"

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
int   get_region_n_outs(void *region);
int get_region_n_exc_outs(void *region);

/** The control flow operation corresponding to the loop-region in at
 *  position pos.
 */
void *get_loop_cfop(void *region, int pos);




/** The algorithm to construct the interval graph.
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


#endif /* _INTERVAL_ANALYSIS_H_ */
