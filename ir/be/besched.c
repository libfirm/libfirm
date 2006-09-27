/* $Id$ */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "impl.h"
#include "irprintf.h"
#include "irgwalk.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "iredges_t.h"
#include "ircons.h"
#include "irextbb.h"
#include "debug.h"

#include "bearch.h"
#include "besched_t.h"
#include "beutil.h"
#include "belistsched.h"

FIRM_IMPL1(sched_get_time_step, int, const ir_node *)
FIRM_IMPL1(sched_has_next, int, const ir_node *)
FIRM_IMPL1(sched_has_prev, int, const ir_node *)
FIRM_IMPL1(sched_next, ir_node *, const ir_node *)
FIRM_IMPL1(sched_prev, ir_node *, const ir_node *)
FIRM_IMPL1(sched_first, ir_node *, const ir_node *)
FIRM_IMPL1(sched_last, ir_node *, const ir_node *)
FIRM_IMPL2(sched_add_after, ir_node *, ir_node *, ir_node *)
FIRM_IMPL2(sched_add_before, ir_node *, ir_node *, ir_node *)
FIRM_IMPL2(sched_comes_after, int, const ir_node *, const ir_node *)
FIRM_IMPL1_VOID(sched_remove, ir_node *)

size_t sched_irn_data_offset = 0;

static void block_sched_dumper(ir_node *block, void *env)
{
	FILE *f = env;
	const ir_node *curr;

	ir_fprintf(f, "%+F:\n", block);
	sched_foreach(block, curr) {
    sched_info_t *info = get_irn_sched_info(curr);
		ir_fprintf(f, "\t%6d: %+F\n", info->time_step, curr);
	}
}

void be_sched_dump(FILE *f, ir_graph *irg)
{
	irg_block_walk_graph(irg, block_sched_dumper, NULL, f);
}

/* Init the scheduling stuff. */
void be_sched_init(void)
{
	sched_irn_data_offset = register_additional_node_data(sizeof(sched_info_t));
}

void sched_renumber(const ir_node *block)
{
	ir_node *irn;
	sched_info_t *inf;
	sched_timestep_t step = SCHED_INITIAL_GRANULARITY;

	sched_foreach(block, irn) {
		inf = get_irn_sched_info(irn);
		inf->time_step = step;
		step += SCHED_INITIAL_GRANULARITY;
	}
}

/* Verify a schedule. */
int sched_verify(const ir_node *block)
{
	int res = 1;
	const ir_node *irn;
	int i, n;
	int *save_time_step;
	const ir_node **save_nodes;
	const ir_edge_t *edge;
	pset *scheduled_nodes = pset_new_ptr_default();
	FIRM_DBG_REGISTER(firm_dbg_module_t *dbg_sched, "firm.be.sched");

	/* Count the number of nodes in the schedule. */
	n = 0;
	sched_foreach(block, irn)
		n++;

	if(n <= 0)
		return 1;

	save_time_step = xmalloc(n * sizeof(save_time_step[0]));
	save_nodes = xmalloc(n * sizeof(save_nodes[0]));

	i = 0;
	sched_foreach(block, irn) {
		sched_info_t *info = get_irn_sched_info(irn);
		save_time_step[i] = info->time_step;
		save_nodes[i] = (ir_node *)irn;
		info->time_step = i;
		pset_insert_ptr(scheduled_nodes, irn);

		i += 1;
	}

	/*
	 * Check if each relevant operand of a node is scheduled before
	 * the node itself.
	 */
	sched_foreach(block, irn) {
		int i, n;
		int step = sched_get_time_step(irn);

		for(i = 0, n = get_irn_arity(irn); i < n; i++) {
			ir_node *op = get_irn_n(irn, i);

			if(to_appear_in_schedule(op)
				&& !is_Phi(irn)
				&& get_nodes_block(op) == block
				&& sched_get_time_step(op) > step) {

				DBG((dbg_sched, LEVEL_DEFAULT,
					"%+F: %+F is operand of %+F but scheduled after\n", block, op, irn));
				res = 0;
			}
		}
	}

	/* Check, if the time steps are correct */
	for(i = 1; i < n; ++i) {
		if(save_time_step[i] - save_time_step[i - 1] <= 0) {
			DBG((dbg_sched, LEVEL_DEFAULT,
				"%+F from %+F(%d) -> %+F(%d) step shrinks from %d -> %d\n",
				block, save_nodes[i - 1], i - 1, save_nodes[i], i,
				save_time_step[i - 1], save_time_step[i]));
			res = 0;
		}
	}

	/* Restore the old time steps */
	i = 0;
	sched_foreach(block, irn) {
		sched_info_t *info = get_irn_sched_info(irn);
		info->time_step = save_time_step[i++];
	}

	/* Check for all nodes in the block if they are scheduled. */
	foreach_out_edge(block, edge) {
		ir_node *irn = get_edge_src_irn(edge);
		if(to_appear_in_schedule(irn) && !pset_find_ptr(scheduled_nodes, irn)) {
			DBG((dbg_sched, LEVEL_DEFAULT,
				"%+F: %+F is in block but not scheduled\n", block, irn));
			res = 0;
		}
	}

	del_pset(scheduled_nodes);
	free(save_time_step);
	free((void *) save_nodes);
	return res;
}

/**
 * Block-Walker: verify the current block and update the status
 */
static void sched_verify_walker(ir_node *block, void *data)
{
	int *res = data;
	*res &= sched_verify(block);
}

/* Verify the schedules in all blocks of the irg. */
int sched_verify_irg(ir_graph *irg)
{
	int res = 1;
	irg_block_walk_graph(irg, sched_verify_walker, NULL, &res);

	return res;
}

int sched_skip_cf_predicator(const ir_node *irn, void *data) {
	arch_env_t *ae = data;
	return arch_irn_class_is(ae, irn, branch);
}

int sched_skip_phi_predicator(const ir_node *irn, void *data) {
	return is_Phi(irn);
}

/* Skip nodes in a schedule. */
ir_node *sched_skip(ir_node *from, int forward, sched_predicator_t *predicator, void *data)
{
	const ir_node *bl = get_block(from);
	ir_node *curr;

	if (is_Block(from))
		from = forward ? sched_next(from) : sched_prev(from);

	for(curr = from; curr != bl && predicator(curr, data); curr = forward ? sched_next(curr) : sched_prev(curr));

	return curr;
}

/** A simple forward single linked list. */
typedef struct {
	ir_node *start;   /**< start of the list */
	ir_node *end;     /**< last block in the list */
	unsigned n_blks;  /**< number of blocks in the list */
} anchor;

static void add_block(anchor *list, ir_node *block) {
	if(list->start == NULL) {
		list->start = block;
		list->end = block;
	} else {
		set_irn_link(list->end, block);
		list->end = block;
	}

	list->n_blks++;
}

static void create_block_list(ir_node *leader_block, anchor *list) {
	int i;
	ir_node *block = NULL;
	const ir_edge_t *edge;

	ir_extblk *extbb = get_Block_extbb(leader_block);
	if(extbb_visited(extbb))
		return;
	mark_extbb_visited(extbb);

	for(i = 0; i < get_extbb_n_blocks(extbb); ++i) {
		block = get_extbb_block(extbb, i);
		add_block(list, block);
	}

	assert(block != NULL);

	// pick successor extbbs
	foreach_block_succ(block, edge) {
		ir_node *succ = get_edge_src_irn(edge);

		create_block_list(succ, list);
	}

	for(i = 0; i < get_extbb_n_blocks(extbb) - 1; ++i) {
		block = get_extbb_block(extbb, i);
		foreach_block_succ(block, edge) {
			ir_node *succ = get_edge_src_irn(edge);

			create_block_list(succ, list);
		}
	}
}

void compute_extbb_execfreqs(ir_graph *irg, ir_exec_freq *execfreqs);

/*
 * Calculates a block schedule. The schedule is stored as a linked
 * list starting at the start_block of the irg.
 */
ir_node **sched_create_block_schedule(ir_graph *irg, ir_exec_freq *execfreqs)
{
	anchor list;
	ir_node **blk_list, *b, *n;
	unsigned i;

	/* schedule extended basic blocks */
	compute_extbb_execfreqs(irg, execfreqs);
	//compute_extbb(irg);

	list.start  = NULL;
	list.end    = NULL;
	list.n_blks = 0;
	inc_irg_block_visited(irg);
	create_block_list(get_irg_start_block(irg), &list);

	/** create an array, so we can go forward and backward */
	blk_list = NEW_ARR_D(ir_node *, irg->obst,list.n_blks);

	for (i = 0, b = list.start; b; b = n, ++i) {
		n = get_irn_link(b);
		blk_list[i] = b;
	}

	return blk_list;
}

typedef struct remove_dead_nodes_env_t_ {
	ir_graph *irg;
	bitset_t *reachable;
} remove_dead_nodes_env_t;

static void mark_dead_nodes_walker(ir_node *node, void *data)
{
	remove_dead_nodes_env_t *env = (remove_dead_nodes_env_t*) data;
	bitset_set(env->reachable, get_irn_idx(node));
}

static void remove_dead_nodes_walker(ir_node *block, void *data)
{
	remove_dead_nodes_env_t *env = (remove_dead_nodes_env_t*) data;
	ir_node *node, *next;

	for(node = sched_first(block); !sched_is_end(node); node = next) {
		int i, arity;

		// get next node now, as after calling sched_remove it will be invalid
		next = sched_next(node);

		if(bitset_is_set(env->reachable, get_irn_idx(node)))
			continue;

		arity = get_irn_arity(node);
		for(i = 0; i < arity; ++i)
			set_irn_n(node, i, new_r_Bad(env->irg));

		sched_remove(node);
	}
}

void be_remove_dead_nodes_from_schedule(ir_graph *irg)
{
	remove_dead_nodes_env_t env;
	env.irg = irg;
	env.reachable = bitset_alloca(get_irg_last_idx(irg));

	// mark all reachable nodes
	irg_walk_graph(irg, mark_dead_nodes_walker, NULL, &env);

	// walk schedule and remove non-marked nodes
	irg_block_walk_graph(irg, remove_dead_nodes_walker, NULL, &env);
}
