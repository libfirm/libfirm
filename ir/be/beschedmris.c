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
 * @brief       Implements a list scheduler for the MRIS algorithm.
 * @author      Sebastian Hack
 * @date        04.04.2006
 * @version     $Id$
 *
 * Implements a list scheduler for the MRIS algorithm in:
 * Govindarajan, Yang, Amaral, Zhang, Gao
 * Minimum Register Instruction Sequencing to Reduce Register Spills
 * in out-of-order issue superscalar architectures
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits.h>

#include "obst.h"
#include "debug.h"

#include "irgraph_t.h"
#include "irnode_t.h"
#include "iredges_t.h"
#include "ircons_t.h"
#include "irphase_t.h"
#include "irdump_t.h"
#include "irgwalk.h"
#include "irtools.h"
#include "irbitset.h"
#include "height.h"

#include "benode_t.h"
#include "besched_t.h"
#include "beschedmris.h"
#include "benodesets.h"
#include "beirg.h"

struct _mris_env_t {
	ir_phase          ph;
	heights_t         *heights;
	const arch_env_t  *aenv;
	ir_graph          *irg;
	ir_node           *bl;
	int               visited;
	struct list_head  lineage_head;
	struct obstack    obst;
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
};

typedef struct _mris_irn_t {
	int visited;
	ir_node *lineage_start;
	ir_node *lineage_next;
	ir_node *lineage_end;
	struct list_head lineage_list;
} mris_irn_t;

#define to_appear(env, irn) (to_appear_in_schedule(irn) && get_nodes_block(irn) == env->bl)

#define get_mris_irn(env, irn)   ((mris_irn_t *) phase_get_or_set_irn_data(&env->ph, irn))
#define foreach_lineage(env, pos, tmp) list_for_each_entry_safe(mris_irn_t, pos, tmp, &(env)->lineage_head, lineage_list)

static void *mris_irn_data_init(ir_phase *ph, ir_node *irn, void *data)
{
	mris_irn_t *mi = data ? data : phase_alloc(ph, sizeof(mi[0]));
	(void) irn;
	memset(mi, 0, sizeof(mi[0]));
	INIT_LIST_HEAD(&mi->lineage_list);
	return mi;
}

#if 0
static int compute_height(mris_env_t *env, ir_node *irn, unsigned long visited)
{
	mris_irn_t *mi = get_mris_irn(env, irn);

	if(get_irn_visited(irn) >= visited) {
		DBG((env->dbg, LEVEL_3, "\theight of %+F = %d\n", irn, mi->height));
		return mi->height;
	}

	else {
		const ir_edge_t *edge;

		set_irn_visited(irn, visited);
		mi->height  = 0;

		foreach_out_edge(irn, edge) {
			ir_node *dep = get_edge_src_irn(edge);

			if(!is_Block(dep) && get_nodes_block(dep) == env->bl) {
				int dep_height = compute_height(env, dep, visited);
				mi->height     = MAX(mi->height, dep_height);
			}
		}

		mi->height++;
		DBG((env->dbg, LEVEL_3, "\tsetting height of %+F = %d\n", irn, mi->height));
	}

	return mi->height;
}

static void compute_heights(mris_env_t *env)
{
	const ir_edge_t *edge;
	unsigned long visited;

	visited = get_irg_visited(env->irg) + 1;
	set_irg_visited(env->irg, visited);

	foreach_out_edge(env->bl, edge) {
		ir_node *dep = get_edge_src_irn(edge);
		if(to_appear(env, dep))
			compute_height(env, dep, visited);
	}
}
#endif

#define valid_node(env, dep) (to_appear(env, dep) && !be_is_Keep(dep))

static void grow_all_descendands(mris_env_t *env, ir_node *irn, bitset_t *visited)
{
	const ir_edge_t *edge;

	// assert(get_irn_mode(irn) != mode_T);

	foreach_out_edge(irn, edge) {
		ir_node *desc = get_edge_src_irn(edge);
		if(valid_node(env, desc) && !bitset_contains_irn(visited, desc)) {
			obstack_ptr_grow(&env->obst, desc);
			bitset_add_irn(visited, desc);
		}
	}

	foreach_out_edge_kind(irn, edge, EDGE_KIND_DEP) {
		ir_node *desc = get_edge_src_irn(edge);
		if(valid_node(env, desc) && !bitset_contains_irn(visited, desc)) {
			obstack_ptr_grow(&env->obst, desc);
			bitset_add_irn(visited, desc);
		}
	}
}

static ir_node **all_descendants(mris_env_t *env, ir_node *irn)
{
	bitset_t *visited = bitset_irg_malloc(env->irg);

	grow_all_descendands(env, irn, visited);

#if 0
	if(get_irn_mode(irn) == mode_T) {
		const ir_edge_t *edge;
		foreach_out_edge(irn, edge) {
			ir_node *desc = get_edge_src_irn(edge);
			assert(is_Proj(desc) && get_irn_mode(desc) != mode_T);
			grow_all_descendands(env, desc, visited);
		}
	}

	else
		grow_all_descendands(env, irn, visited);
#endif
	bitset_free(visited);
	obstack_ptr_grow(&env->obst, NULL);
	return obstack_finish(&env->obst);
}

static ir_node *put_lowest_in_front(mris_env_t *env, ir_node **in)
{
	int lowest_index       = 0;
	unsigned lowest_height = INT_MAX;
	int i;

	for(i = 0; in[i]; ++i) {
		unsigned height = get_irn_height(env->heights, in[i]);
		if(height < lowest_height) {
			lowest_height = height;
			lowest_index  = i;
		}
	}

	if(i > 0) {
		ir_node *tmp     = in[0];
		in[0]            = in[lowest_index];
		in[lowest_index] = tmp;
	}

	return in[0];
}

#if 0
static void reaches_walker(mris_env_t *env, ir_node *irn, ir_node *tgt, int *found, unsigned long visited)
{
	if(get_irn_visited(irn) < visited && get_nodes_block(irn) == env->bl) {

		set_irn_visited(irn, visited);

		if(irn == tgt)
			*found = 1;
		else {
			int i, n;

			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);
				if(!*found)
					reaches_walker(env, op, tgt, found, visited);
			}
		}
	}
}

static int reaches(mris_env_t *env, ir_node *src, ir_node *tgt)
{
	int found = 0;
	unsigned long visited = get_irg_visited(env->irg) + 1;

	set_irg_visited(env->irg, visited);
	reaches_walker(env, src, tgt, &found, visited);
	return found;
}
#endif

static INLINE ir_node *skip_Projs(ir_node *irn)
{
	return is_Proj(irn) ? skip_Projs(get_Proj_pred(irn)) : irn;
}

#if 0
static void replace_tuple_by_repr_proj(mris_env_t *env, ir_node **in)
{
	int i;

	for(i = 0; in[i]; ++i) {
		if(get_irn_mode(in[i]) == mode_T) {
			const ir_edge_t *edge;
			ir_node *proj  = NULL;
			ir_node *first = NULL;

			foreach_out_edge(in[i], edge) {
				ir_node *desc = get_edge_src_irn(edge);

				first = first ? first : desc;
				if(get_irn_mode(desc) == mode_M) {
					proj = desc;
					break;
				}
			}

			proj = proj ? proj : first;
			assert(proj);
			in[i] = proj;
		}
	}
}
#endif

static void lineage_formation(mris_env_t *env)
{
	DEBUG_ONLY(firm_dbg_module_t *dbg = env->dbg);
	nodeset *nodes         = new_nodeset(128);

	const ir_edge_t *edge;

	foreach_out_edge(env->bl, edge) {
		ir_node *irn = get_edge_src_irn(edge);
		if(to_appear(env, irn))
			nodeset_insert(nodes, irn);
	}

	while(nodeset_count(nodes) > 0) {
		mris_irn_t *mi;
		ir_node *irn;
		ir_node *highest_node = NULL;
		ir_node *lowest_desc  = NULL;
		ir_node *start;
		mris_irn_t *start_mi;

		ir_node **in;
		int recompute_height  = 0;
		unsigned  curr_height = 0;

		/* search the highest node which is not yet in a lineage. */
		for(irn = nodeset_first(nodes); irn; irn = nodeset_next(nodes)) {
			unsigned height = get_irn_height(env->heights, irn);
			if(height > curr_height) {
				highest_node = irn;
				curr_height  = height;
			}
		}

		assert(highest_node);
		DBG((dbg, LEVEL_2, "highest node is %+F height %d\n", highest_node, get_irn_height(env->heights, highest_node)));

		start = highest_node;
		mi = start_mi = get_mris_irn(env, highest_node);

		/* start a lineage beginning with highest_node. */
		mi->lineage_start = highest_node;
		mi->lineage_next  = NULL;
		mi->lineage_end   = NULL;
		list_add(&mi->lineage_list, &env->lineage_head);
		nodeset_remove(nodes, highest_node);

		/*
			put all descendants in an array.
			we also move the lowest descendant in front, so that the other nodes
			are easily accessible as an array, too.
		*/
		in          = all_descendants(env, highest_node);
		lowest_desc = put_lowest_in_front(env, in);

		/* as long as the current highest node has still descendants */
		while(lowest_desc) {
			mris_irn_t *lowest_mi  = get_mris_irn(env, lowest_desc);
			mris_irn_t *highest_mi = get_mris_irn(env, highest_node);
			int highest_is_tuple   = get_irn_mode(highest_node) == mode_T;

			int n_desc;

			DBG((dbg, LEVEL_2, "\tlowest descendant %+F height %d\n", lowest_desc, get_irn_height(env->heights, lowest_desc)));

			/* count the number of all descendants which are not the lowest descendant */
			for(n_desc = 0; in[n_desc]; ++n_desc);

			/*
			we insert a CopyKeep node to express the artificial dependencies from the lowest
			descendant to all other descendants.
			*/
			if(n_desc > 1 && !be_is_Keep(lowest_desc)) {
				ir_node *op;
				int i, n;

				for(i = 0, n = get_irn_ins_or_deps(lowest_desc); i < n; ++i) {
					ir_node *cmp;

					op  = get_irn_in_or_dep(lowest_desc, i);
					cmp = highest_is_tuple ? skip_Projs(op) : op;

					// if(cmp == highest_node)
					if(op == highest_node)
						break;
				}

				assert(i < n && "could not find operand");

				//replace_tuple_by_repr_proj(env, &in[1]);
				if(!is_Proj(lowest_desc))
					add_irn_dep(lowest_desc, in[1]);
			}
			obstack_free(&env->obst, in);

			/* if the current lowest node is not yet in a lineage, add it to the current one. */
			if(!lowest_mi->lineage_start) {
				/* mark the current lowest node as the last one in the lineage. */
				highest_mi->lineage_next = lowest_desc;
				start_mi->lineage_end    = lowest_desc;

				lowest_mi->lineage_start = start;
				nodeset_remove(nodes, lowest_desc);
			}

			/* else we cannot extend this lineage, so break. */
			else
				break;

			highest_node = lowest_desc;
			highest_mi   = lowest_mi;

			/* recompute the descendants array and the new lowest descendant. */
			in          = all_descendants(env, highest_node);
			lowest_desc = put_lowest_in_front(env, in);
		}

		/* recompute the heights if desired. */
		if(recompute_height)
			heights_recompute(env->heights);
	}
}

static int fuse_two_lineages(mris_env_t *env, mris_irn_t *u, mris_irn_t *v)
{
	mris_irn_t *mi;
	ir_node *irn, *last;
	ir_node *u_end   = u->lineage_end;
	ir_node *v_start = v->lineage_start;
	ir_node *start   = skip_Projs(v_start);

	if(be_is_Keep(start))
		return 0;

	/* set lineage end of nodes in u to end of v. */
	irn = last = u->lineage_start;
	mi         = get_mris_irn(env, irn);
	while(irn && irn != u_end) {
		mi = get_mris_irn(env, irn);
		mi->lineage_end = v->lineage_end;
		last = irn;
		irn = mi->lineage_next;
	}

	/* insert a CopyKeep to make lineage v dependent on u. */
	if(get_irn_ins_or_deps(start) == 0)
		return 0;

	if(get_irn_mode(last) == mode_T) {
		const ir_edge_t *edge;
		foreach_out_edge(last, edge) {
			last = get_edge_src_irn(edge);
			break;
		}
	}

	/* irn now points to the last node in lineage u; mi has the info for the node _before_ the terminator of the lineage. */
	mi->lineage_next       = v_start;

	/* add a dependency from the first node in v's lineage to the last in u's */
	add_irn_dep(u_end, v_start);

	/* set lineage start of nodes in v to start of u. */
	irn = v->lineage_start;
	while(irn && irn != v->lineage_end) {
		mris_irn_t *mi = get_mris_irn(env, irn);
		mi->lineage_start = u->lineage_start;
		irn = mi->lineage_next;
	}

	heights_recompute(env->heights);

	mi = get_mris_irn(env, v_start);
	list_del(&mi->lineage_list);

	return 1;
}

static void fuse_lineages(mris_env_t *env)
{
	mris_irn_t *u, *v, *tmp1, *tmp2;

again:
	foreach_lineage(env, u, tmp1) {
		foreach_lineage(env, v, tmp2) {
			if(u != v && u->lineage_start && v->lineage_start && u->lineage_end && v->lineage_end
				&& get_nodes_block(u->lineage_start) == get_nodes_block(v->lineage_start)) {
				int uv = heights_reachable_in_block(env->heights, u->lineage_start, v->lineage_end);
				int vu = heights_reachable_in_block(env->heights, v->lineage_start, u->lineage_end);

				if(uv && !vu) {
					if(fuse_two_lineages(env, u, v))
						goto again;
				}
			}
		}
	}
}

static mris_env_t *dump_env = NULL;

static void block_walker(ir_node *bl, void *data)
{
	mris_env_t *env = data;
	env->bl = bl;
	lineage_formation(env);
	fuse_lineages(env);
}

static int mris_edge_hook(FILE *F, ir_node *irn)
{
	mris_irn_t *mi = get_mris_irn(dump_env, irn);

	if(mi->lineage_next) {
		fprintf(F, "edge:{sourcename:\"");
		PRINT_NODEID(mi->lineage_next);
		fprintf(F, "\" targetname:\"");
		PRINT_NODEID(irn);
		fprintf(F, "\" color:lilac}\n");
	}
	return 1;
}

void dump_ir_block_graph_mris(mris_env_t *env, const char *suffix) {
	DUMP_NODE_EDGE_FUNC old = get_dump_node_edge_hook();

	dump_consts_local(0);
	set_dump_node_edge_hook(mris_edge_hook);
	dump_env = env;
	dump_ir_block_graph(env->irg, suffix);
	set_dump_node_edge_hook(old);
}

mris_env_t *be_sched_mris_preprocess(const be_irg_t *birg)
{
	mris_env_t *env = xmalloc(sizeof(env[0]));
	ir_graph   *irg = be_get_birg_irg(birg);

	phase_init(&env->ph, "mris", irg, 2 * PHASE_DEFAULT_GROWTH, mris_irn_data_init, NULL);
	env->aenv     = be_get_birg_arch_env(birg);
	env->irg      = irg;
	env->visited  = 0;
	env->heights  = heights_new(irg);
	INIT_LIST_HEAD(&env->lineage_head);
	FIRM_DBG_REGISTER(env->dbg, "firm.be.sched.mris");
	obstack_init(&env->obst);
	irg_walk_graph(irg, firm_clear_link, NULL, NULL);
	irg_block_walk_graph(irg, block_walker, NULL, env);
	obstack_free(&env->obst, NULL);
	// dump_ir_block_graph_mris(env, "-mris");
	return env;
}

void be_sched_mris_free(mris_env_t *env)
{
	phase_free(&env->ph);
	heights_free(env->heights);
	free(env);
}
