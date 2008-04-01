#ifndef _EXT_GRS_MATCH_T_H_
#define _EXT_GRS_MATCH_T_H_

/*
 * Project:     libFIRM/extension module/graph rewriting system
 * File name:   ext/grs/match_t.h
 * Purpose:     provides the libfirm internal version of the interface
 *              for subgraph matching
 * Author:      Veit Batz
 * Created:		7. Junly 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include "common_t.h"
#include "matchplan_t.h"

#include "match.h"


/** graph matching result */
struct _ext_grs_match_t {
	/** the action this match belongs to */
	ext_grs_action_t *action;
	/** the host graph matched in */
	ir_graph *irg;
	/** flag: match valid */
	int compliant;
	/** the number of matches found */
	int n_matches;
	/** the number of nodes in each match */
	int n_nodes;
	/** the number of edges in each match */
	int n_edges;
	/** maximum node id in this match */
	int max_node_id;
	/** maximum edge id in this match */
	int max_edge_id;
	/** array of arrays, each mapping node/edge ids to (ir_node*),
	 *  the length is max_node_id + 1 */
	ir_node ***nodes;
	/** array of arrays, each mapping edge ids to (ir_node*),
	 *  the length is max_edge_id + 1 */
	const ir_edge_t ***edges;
	/** array of arrays, each mapping replace_node/edge ids to (ir_node*) */
	ir_node ***repl_nodes;
	/** internal obstack of the match object */
	struct obstack obst;
};

/** wraps initial bindings for pivoted graph matching */
struct _ext_grs_binding_t {
	/** number of bound host graph fragments */
	int size;
	/** number of involved pattern nodes */
	int n_nodes;
	/** number of involved pattern edges */
	int n_edges;
	/** the bound pattern nodes */
	ext_grs_node_t *pat_nodes;
	/** the bound pattern edges */
	ext_grs_edge_t *pat_edges;
	/** the ir nodes bound to the pattern nodes given by their index (node map) */
	ir_node ***nodes;
	/** the ir edges bound to the pattern edges given by their index (edge map) */
	ir_edge_t ***edges;
};


void _ext_grs_match_init(void);



#endif /*_EXT_GRS_MATCH_T_H_*/
