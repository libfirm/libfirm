#ifndef _EXT_GRS_MATCH_H_
#define _EXT_GRS_MATCH_H_

/*
 * Project:     libFIRM/extension module/graph rewriting system
 * File name:   ext/grs/match.h
 * Purpose:     provides an interface for subgraph matching
 * Author:      Veit Batz
 * Modified by: Andreas Schoesser
 * Created:		6. August 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


/** Do the matching \b compliant, i.e. the matching allows
 *  parallel independent rule application. */
#define ext_grs_COMPLIANT 1
/** Do the matching \b not compliant, i.e. the
 *  matching does \b not regard parallel independent rule application. */
#define ext_grs_REGARDLESS 0
/** Find \b all occurences of the pattern graph in a given host graph,
 *  or in case of compliant matching, as much as possible occurences,
 *  which can be replaced parallel independent.
 *  @note It is \b not guarantied that the maximal possible number
 *  		of matches is found. */
#define ext_grs_ALL -1


#include "matchplan.h"



/** graph matching result */
typedef struct _ext_grs_match_t ext_grs_match_t;

/** wraps initial bindings for pivoted graph matching */
typedef struct _ext_grs_binding_t ext_grs_binding_t;







/** find at most n matches
 *
 *  @param irg			the host graph to be matched in
 *  @param plan			the matching plan parametrizing the search
 *  @param n			the maximum number of matches (-1 means all, if compliant
 * 						matching is activated not all, but as many as feasible
 * 						\b compliant matches are found)
 *  @param compliant	match compliant if nonzero, i.e. find only matches
 * 						that can be rewritten concurrently
 */
ext_grs_match_t *ext_grs_match(
	ir_graph *irg, ext_grs_match_plan_t *plan, int n, int compliant);

/** find at most n matches
 *
 *  @param irg			the host graph to be matched in
 *  @param plan			the matching plan parametrizing the search
 *  @param n			the maximum number of matches (-1 means all, if compliant
 * 						matching is activated not all, but as many as feasible
 * 						\b compliant matches are found)
 *  @param compliant	match compliant if non zero, i.e. find only matches
 * 						that can be rewritten concurrently
 *  @param binding		initial binding of pattern nodes/edges to host graph
 * 						nodes/edges, in the binding struct the appropriate
 * 						pattern and host graph nodes/edges have the same index
 *
 * @returns	a matching result
 */
ext_grs_match_t *ext_grs_match_pivot(ir_graph *irg,
	ext_grs_match_plan_t *plan, int n, int compliant, ext_grs_binding_t *binding);

/** perform rewriting step
 *
 *  @param match	a matching result
 *  @param n		The number of matches to be replaced, if n=0 no
 * 					replacement is performed, if n=-1 \b all matches
 * 					will be replaced. If n=-1 or n>1 the matching result
 * 					has to be compliant.
 *  @param which	An array containing the indices of the n matches to
 * 					be replaced. If \b all matches shall be replaced this
 * 					parameter will be ignored.
 *  @note The match object is freed afterwards. */
void ext_grs_finish_match(ext_grs_match_t *match, int n, int *which);
void ext_grs_free_match(ext_grs_match_t *match);

/** get the number of matches in the given matching result
 *  @param match	a matching result
 *  @returns the number of matches
 */
int ext_grs_get_n_matches(ext_grs_match_t *match);

/** get the ir graph of a given matching result
 *  @param match	a matching result
 *  @returns the ir graph the matching result belongs to
 */
ir_graph *ext_grs_get_match_irg(ext_grs_match_t *match);

/** get the node map of a match in a given matching result
 *
 *  @param 	match	a matching result
 *  @param which	the number of the match in the matching result
 *
 *  @returns the node map of the match
 */
ir_node **ext_grs_get_match_node_map(ext_grs_match_t *match, int which);

/** get the node map of a match in a given matching result
 *
 *  @param 	match	a matching result
 *  @param which	the id of the match in the matching result
 *
 *  @returns the edge map of the match
 */
const ir_edge_t **ext_grs_get_match_edge_map(ext_grs_match_t *match, int which);

ir_node **ext_grs_get_replace_node_map(ext_grs_match_t *match, int which);

/** dump the given ir graph the matching result belongs to where all matches
 *  of the given matching result are represented by a virtual "Match" node and
 *  some edges pointing from the Match node to the regarding nodes. All nodes
 *  and edges which belong to am match are drawed in red color.

 *  @param match	a matching result
 *  @param
 *
 *  @returns the number of matches
 */
void ext_grs_dump_match(ext_grs_match_t *match, const char *suffix);

int ext_grs_get_match_max_node_aiid(ext_grs_match_t *match);

int ext_grs_get_match_max_edge_aiid(ext_grs_match_t *match);



#endif /*_EXT_GRS_MATCH_H_*/
