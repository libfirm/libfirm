/**
 * Implements a list scheduler for the MRIS algorithm in:
 * Govindarajan, Yang, Amaral, Zhang, Gao
 * Minimum Register Instruction Sequencing to Reduce Register Spills
 * in out-of-order issue superscalar architectures
 * @author Sebastian Hack
 * @date   04.04.2006
 */

#include <limits.h>

#include "misc.h"
#include "obstack.h"
#include "debug.h"

#include "irgraph_t.h"
#include "irnode_t.h"
#include "iredges_t.h"
#include "ircons_t.h"
#include "irgwalk.h"
#include "irtools.h"

#include "benode_t.h"
#include "besched_t.h"
#include "beschedmris.h"

struct _mris_env_t {
	firm_dbg_module_t *dbg;
	const arch_env_t  *aenv;
	ir_graph          *irg;
	ir_node           *bl;
	nodeset           *inserted;
	int               visited;
	struct list_head  lineage_head;
	struct obstack    obst;
};

typedef struct _mris_irn_t {
	int visited;
	int height;
	ir_node *lineage_start;
	ir_node *lineage_next;
	ir_node *lineage_end;
	struct list_head lineage_list;
} mris_irn_t;

#define to_appear(env, irn) (to_appear_in_schedule(irn) && get_nodes_block(irn) == env->bl)

#define get_irn_height(env, irn) (get_mris_irn(env, irn)->height)
#define foreach_lineage(env, pos, tmp) list_for_each_entry_safe(mris_irn_t, pos, tmp, &(env)->lineage_head, lineage_list)

static mris_irn_t *get_mris_irn(mris_env_t *env, ir_node *irn)
{
	mris_irn_t *mi = get_irn_link(irn);

	if(!mi) {
		mi = obstack_alloc(&env->obst, sizeof(mi[0]));
		memset(mi, 0, sizeof(mi[0]));
		set_irn_link(irn, mi);
		INIT_LIST_HEAD(&mi->lineage_list);
	}

	return mi;
}

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

#define valid_node(env, dep) (to_appear(env, dep) && !nodeset_find(env->inserted, dep) && !be_is_Keep(dep))

static void grow_all_descendands(mris_env_t *env, ir_node *irn, unsigned long visited)
{
	const ir_edge_t *edge;

	assert(get_irn_mode(irn) != mode_T);

	foreach_out_edge(irn, edge) {
		ir_node *desc = get_edge_src_irn(edge);
		if(valid_node(env, desc) && get_irn_visited(desc) < visited) {
			obstack_ptr_grow(&env->obst, desc);
			set_irn_visited(desc, visited);
		}
	}
}

static ir_node **all_descendants(mris_env_t *env, ir_node *irn)
{
	unsigned long visited;
	const ir_edge_t *edge;

	visited = get_irg_visited(env->irg) + 1;
	set_irg_visited(env->irg, visited);

	if(get_irn_mode(irn) == mode_T) {
		foreach_out_edge(irn, edge) {
			ir_node *desc = get_edge_src_irn(edge);
			assert(is_Proj(desc) && get_irn_mode(desc) != mode_T);
			grow_all_descendands(env, desc, visited);
		}
	}

	else
		grow_all_descendands(env, irn, visited);

	obstack_ptr_grow(&env->obst, NULL);
	return obstack_finish(&env->obst);
}

static ir_node *put_lowest_in_front(mris_env_t *env, ir_node **in)
{
	int lowest_index  = 0;
	int lowest_height = INT_MAX;
	int i;

	for(i = 0; in[i]; ++i) {
		mris_irn_t *mi = get_mris_irn(env, in[i]);
		if(mi->height < lowest_height) {
			lowest_height = mi->height;
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

static INLINE ir_node *skip_Projs(ir_node *irn)
{
	return is_Proj(irn) ? skip_Projs(get_Proj_pred(irn)) : irn;
}

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

static void lineage_formation(mris_env_t *env)
{
	firm_dbg_module_t *dbg = env->dbg;
	nodeset *nodes         = new_nodeset(128);

	const ir_edge_t *edge;

	foreach_out_edge(env->bl, edge) {
		ir_node *irn = get_edge_src_irn(edge);
		if(to_appear(env, irn))
			nodeset_insert(nodes, irn);
	}

	compute_heights(env);

	while(nodeset_count(nodes) > 0) {
		mris_irn_t *mi;
		ir_node *irn;
		ir_node *highest_node = NULL;
		ir_node *lowest_desc  = NULL;

		ir_node **in;
		int recompute_height  = 0;
		int curr_height       = 0;

		/* search the highest node which is not yet in a lineage. */
		for(irn = nodeset_first(nodes); irn; irn = nodeset_next(nodes)) {
			mris_irn_t *inf = get_mris_irn(env, irn);
			if(inf->height > curr_height) {
				highest_node = irn;
				curr_height  = inf->height;
			}
		}

		assert(highest_node);
		DBG((dbg, LEVEL_2, "highest node is %+F height %d\n", highest_node, get_irn_height(env, highest_node)));

		/* start a lineage beginning with highest_node. */
		mi = get_mris_irn(env, highest_node);
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
			mris_irn_t *start_mi   = get_mris_irn(env, highest_mi->lineage_start);
			int highest_is_tuple   = get_irn_mode(highest_node) == mode_T;

			int n_desc;

			DBG((dbg, LEVEL_2, "\tlowest descendant %+F height %d\n", lowest_desc, mi->height));

			/* count the number of all descendants which are not the lowest descendant */
			for(n_desc = 0; in[n_desc + 1]; ++n_desc);

			/*
			we insert a CopyKeep node to express the artificial dependencies from the lowest
			descendant to all other descendants.
			*/
			if(n_desc > 1 && !be_is_Keep(lowest_desc)) {
				const arch_register_class_t *cls;
				ir_node *copy_keep, *op;
				int i, n;

				for(i = 0, n = get_irn_arity(lowest_desc); i < n; ++i) {
					ir_node *cmp;

					op  = get_irn_n(lowest_desc, i);
					cmp = highest_is_tuple ? skip_Projs(op) : op;

					if(cmp == highest_node)
						break;
				}

				assert(i < n && "could not find operand");

				cls = arch_get_irn_reg_class(env->aenv, op, BE_OUT_POS(0));
				replace_tuple_by_repr_proj(env, &in[1]);
				copy_keep = be_new_CopyKeep(cls, env->irg, env->bl, op, n_desc, &in[1], get_irn_mode(op));
				set_irn_n(lowest_desc, i, copy_keep);
				nodeset_insert(env->inserted, copy_keep);
			}
			obstack_free(&env->obst, in);

			/* mark the current lowest node as the last one in the lineage. */
			highest_mi->lineage_next = lowest_desc;
			start_mi->lineage_end    = lowest_desc;

			/* if the current lowest node is not yet in a lineage, add it to the current one. */
			if(!lowest_mi->lineage_start) {
				lowest_mi->lineage_start = highest_mi->lineage_start;
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
			compute_heights(env);
	}
}

static int fuse_two_lineages(mris_env_t *env, mris_irn_t *u, mris_irn_t *v)
{
	mris_irn_t *mi;
	mris_irn_t *copy_mi;
	ir_node *irn, *last, *copy;
	ir_node *u_end   = u->lineage_end;
	ir_node *v_start = v->lineage_start;
	ir_node *start   = skip_Projs(v_start);

	if(be_is_Keep(start))
		return 0;

	/* set lineage end of nodes in u to end of v. */
	irn = last = u->lineage_start;
	mi         = get_mris_irn(env, irn);
	while(irn != u_end) {
		mi = get_mris_irn(env, irn);
		mi->lineage_end = v->lineage_end;
		last = irn;
		irn = mi->lineage_next;
	}

	/* insert a CopyKeep to make lineage v dependent on u. */
	{
		const arch_register_class_t *cls;
		ir_node *op    = NULL;
		int i, n;

		if(get_irn_arity(start) == 0)
			return 0;

		op = get_irn_n(start, 0);

		cls  = arch_get_irn_reg_class(env->aenv, op, BE_OUT_POS(0));
		if(get_irn_mode(last) == mode_T) {
			const ir_edge_t *edge;
			foreach_out_edge(last, edge) {
				last = get_edge_src_irn(edge);
				break;
			}
		}
		copy = be_new_CopyKeep_single(cls, env->irg, env->bl, op, last, get_irn_mode(op));
		set_irn_n(start, 0, copy);
		copy_mi = get_mris_irn(env, copy);
		nodeset_insert(env->inserted, copy);
	}

	/* irn now points to the last node in lineage u; mi has the info for the node _before_ the terminator of the lineage. */
	mi->lineage_next       = copy;
	copy_mi->lineage_start = u->lineage_start;
	copy_mi->lineage_end   = v->lineage_end;
	copy_mi->lineage_next  = v_start;

	/* set lineage start of nodes in v to start of u. */
	irn = v->lineage_start;
	while(irn != v->lineage_end) {
		mris_irn_t *mi = get_mris_irn(env, irn);
		mi->lineage_start = u->lineage_start;
		irn = mi->lineage_next;
	}

	mi = get_mris_irn(env, v_start);
	list_del(&mi->lineage_list);

	return 1;
}

static void fuse_lineages(mris_env_t *env)
{
	int fused = 1;
	mris_irn_t *u, *v, *tmp1, *tmp2;

again:
	foreach_lineage(env, u, tmp1) {
		foreach_lineage(env, v, tmp2) {
			if(u == v)
				continue;

			if(!reaches(env, u->lineage_start, v->lineage_end) && reaches(env, v->lineage_start, u->lineage_end)) {
				if(fuse_two_lineages(env, u, v))
					goto again;
			}
		}
	}
}

static void block_walker(ir_node *bl, void *data)
{
	mris_env_t *env = data;
	env->bl = bl;
	lineage_formation(env);
	fuse_lineages(env);
}


mris_env_t *be_sched_mris_preprocess(const be_irg_t *birg)
{
	mris_env_t *env = xmalloc(sizeof(env[0]));

	env->aenv     = birg->main_env->arch_env;
	env->irg      = birg->irg;
	env->visited  = 0;
	env->inserted = new_nodeset(128);
	INIT_LIST_HEAD(&env->lineage_head);
	FIRM_DBG_REGISTER(env->dbg, "firm.be.sched.mris");
	obstack_init(&env->obst);
	irg_walk_graph(env->irg, firm_clear_link, NULL, NULL);
	irg_block_walk_graph(birg->irg, block_walker, NULL, env);
	obstack_free(&env->obst, NULL);
	return env;
}

static void cleanup_inserted(mris_env_t *env)
{
	ir_node *irn;

	for(irn = nodeset_first(env->inserted); irn; irn = nodeset_next(env->inserted)) {
		int i, n;
		ir_node *tgt;

		assert(be_is_CopyKeep(irn));
		tgt = get_irn_n(irn, be_pos_CopyKeep_op);

		/* reroute the edges, remove from schedule and make it invisible. */
		edges_reroute(irn, tgt, env->irg);
		sched_remove(irn);
		for(i = -1, n = get_irn_arity(irn); i < n; ++i)
			set_irn_n(irn, i, new_r_Bad(env->irg));
	}
}

void be_sched_mris_free(mris_env_t *env)
{
	cleanup_inserted(env);
	del_nodeset(env->inserted);
	free(env);
}
