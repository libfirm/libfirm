#include "bespill.h"
#include "bespm.h"
#include "callgraph.h"
#include "cgana.h"
#include "execfreq.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irloop_t.h"
#include "irnode.h"
#include "irnode_t.h"
#include "irouts.h"
#include "irprog_t.h"
#include "pdeq.h"
#include "pmap.h"
#include "pset_new.h"
#include "util.h"
#include "xmalloc.h"

typedef struct timestamp timestamp;

struct timestamp {
	timestamp *last;
	timestamp *caller_block;
	ir_node *block;
	int finished_callees;
	//TODO: Convert to enum for better readability
	short finished_preds; //-1: UNKOWN, 0 = FALSE, 1 = TRUE, 2 = non-finished LOOP, 3 = finished loop, 4 = cond-join
	double irg_exec_freq;
};

typedef struct alloc_result {
	int free_space;
	pset_new_t *spm_set;
	pset_new_t *spm_set_reg_size;
	pset_new_t *copy_in;
	pset_new_t *copy_out;
	//Sets only temporarily needed:
	pset_new_t *swapout_set;
	pset_new_t *retain_set;
	pset_new_t *bring_in_set;
} alloc_result;

typedef enum node_data_type {
	CALLEE, MEM_ACCESS,
} node_data_type;

typedef struct spm_var_info {
	void *identifier;
	int size;
	bool modified;
} spm_var_info;

struct node_data {
	node_data_type data_type;
	list_head list;
	void *identifier; //TODO: change?
	int size; //in bytes
	bool modified;
	int access_cnt;
	double freq_per_byte;
};

typedef struct block_data {
	list_head *node_lists;
	int callee_cnt;
	double max_exec_freq;
	alloc_result **allocation_results;
	pset_new_t *compensation_callees; //callees which have to be surrounded by compensation code
	pset_new_t *dead_set; //Vars last accessed in this block //TODO: Array?
} block_data;

typedef struct drpg_walk_env {
	deq_t workqueue;
	pmap *block_data_map;
	timestamp *cur_branch;
} dprg_walk_env;

struct {
	int size;
	int latency_diff; //ram_lat - spm_lat
	float throughput_ram;
	float throughput_spm;
} spm_properties;

static int gp_reg_size;

static pmap *spm_var_infos;


node_data *(*retrieve_spm_node_data)(ir_node *);
/*void calc_irg_execfreq(ir_graph *irg, void *env) {
	ir_estimate_execfreq
}*/

static node_data *spm_get_node_data_by_type(node_data_type type, void *id, int size, bool modified)
{
	node_data *data = XMALLOC(node_data);
	data->data_type = type;
	data->identifier = id;
	data->size = size;
	data->modified = modified;
	data->access_cnt = 0;
	return data;
}

static node_data *spm_get_var_node_data(node_data_type type, void *id, int size, bool modified)
{
	if (!pmap_contains(spm_var_infos, id)) {
		spm_var_info *info = XMALLOC(spm_var_info);
		info->identifier = id;
		info->size = size;
		info->modified = modified;
		pmap_insert(spm_var_infos, id, info);
	}
	return spm_get_node_data_by_type(type, id, size, modified);
}

node_data *spm_get_mem_read_node_data(void *id, int size)
{
	return spm_get_node_data_by_type(MEM_ACCESS, id, size, false);
}

node_data *spm_get_mem_write_node_data(void *id, int size)
{
	return spm_get_node_data_by_type(MEM_ACCESS, id, size, true);
}

node_data *spm_get_callee_node_data(ir_entity *ent)
{
	return spm_get_node_data_by_type(CALLEE, ent, 0, false);
}

void spm_calculate_dprg_info()
{
	ir_entity **free_methods;
	cgana(&free_methods);


	compute_callgraph();
	find_callgraph_recursions(); //Possibly necessary as well

	//callgraph_walk(calc_irg_execfreq, NULL, &env);
	free(free_methods);

	//free_callgraph();

}

static void spm_collect_node_data(ir_node *node, void *env)
{
	pmap *block_data_map = env;
	node_data *n_data = retrieve_spm_node_data(node);
	if (n_data) {
		ir_node *block = get_nodes_block(node);
		block_data *b_data = pmap_get(block_data, block_data_map, block);
		if (!b_data) {
			b_data = XMALLOC(block_data);
			b_data->callee_cnt = 0;
			b_data->node_lists = NEW_ARR_F(list_head, 1);
			b_data->allocation_results = NULL;
			INIT_LIST_HEAD(b_data->node_lists);
		}
		if (n_data->data_type == CALLEE) {
			b_data->callee_cnt++;
			list_add(&n_data->list, b_data->node_lists);
			return;
		}
		//sort non callee node data by size (between two callee nodes)
		list_for_each_entry(node_data, n_data_iter, b_data->node_lists, list) {
			if (n_data_iter->data_type == CALLEE) {
				list_add_tail(&n_data->list, &n_data_iter->list);
				return;
			}
			if (n_data_iter->size < n_data->size) { //does node always gets insterted here?
				list_add_tail(&n_data->list, &n_data_iter->list);
				return;
			}
			if (n_data_iter->identifier == n_data->identifier) {
				n_data_iter->access_cnt++;
				n_data_iter->modified |= n_data->modified;
				free(n_data);
				return;
			}
		}
		if (list_empty(b_data->node_lists)) {
			list_add(&n_data->list, b_data->node_lists);
		}

	}

}

typedef struct callgraph_walk_env {
	double irg_freq;
	pmap *block_data_map;
	pmap *call_blocks;
} callgraph_walk_env;

static void spm_create_call_list(ir_node *block, void *env)
{
	callgraph_walk_env *c_env = env;
	block_data *b_data = pmap_get(block_data, c_env->block_data_map, block);
	list_head *node_list = &b_data->node_lists[0];

	double freq = c_env->irg_freq * get_block_execfreq(block);
	list_for_each_entry(node_data, n_data, node_list, list) {
		ir_graph *irg = get_entity_irg((ir_entity *) n_data->identifier);
		ir_node *start_block = get_irg_start_block(irg);
		block_data *callee_data = pmap_get(block_data, c_env->block_data_map, start_block);
		if (freq > callee_data->max_exec_freq) {
			callee_data->max_exec_freq = freq;
			pmap_insert(c_env->call_blocks, irg, block);
		}
	}

}

static void spm_collect_block_data(ir_graph *irg, void *env)
{
	irg_block_walk_graph(irg, spm_create_call_list, NULL, env);
}

/* Next two functions copied from callgraph.c and edited */
static void do_walk(ir_graph *irg, callgraph_walk_func *pre,
                    callgraph_walk_func *post, void *env)
{
	callgraph_walk_env *callg_env = env;
	if (pre != NULL)
		pre(irg, env);

	for (size_t i = 0, n_callees = get_irg_n_callees(irg); i < n_callees; i++) {
		if (is_irg_callee_backedge(irg, i)) {
			//TODO: Mark recursion + check backedge info gets calculated
		}
		ir_graph *m = get_irg_callee(irg, i);
		if (pmap_contains(callg_env->call_blocks, m)) {
			callgraph_walk_env c_env = {
				.irg_freq = pmap_get(block_data, callg_env->call_blocks, m)->max_exec_freq,
				.block_data_map = callg_env->block_data_map,
				.call_blocks = pmap_create(),
			};
			do_walk(m, pre, post, &c_env);
			pmap_destroy(c_env.call_blocks);
		}
	}

	if (post != NULL)
		post(irg, env);
}

static void spm_callgraph_walk(callgraph_walk_func *pre, callgraph_walk_func *post, void *env)
{
	/* roots are methods which have no callers in the current program */
	foreach_irp_irg(i, irg) {
		if (get_irg_n_callers(irg) == 0) {
			callgraph_walk_env c_env = {
				.irg_freq = 1.0,
				.block_data_map = env,
				.call_blocks = pmap_create(),
			};
			do_walk(irg, pre, post, &c_env);
			pmap_destroy(c_env.call_blocks);
		}
	}

	/* in case of unreachable call loops we haven't visited some irgs yet */
	foreach_irp_irg(i, irg) {
		callgraph_walk_env c_env = {
			.irg_freq = 1.0,
			.block_data_map = env,
			.call_blocks = pmap_create(),
		};
		do_walk(irg, pre, post, &c_env);
		pmap_destroy(c_env.call_blocks);
	}
}

static void spm_calc_blocks_access_freq(pmap *block_data_map)
{
	foreach_pmap(block_data_map, cur_entry) {
		block_data *blk_data = cur_entry->value;
		//TODO: alloc_results init doesn't fit in here?
		blk_data->allocation_results = NEW_ARR_FZ(alloc_result *, blk_data->callee_cnt + 1);
		//idx 0: call nodes, idx 1 to x are mem_access in between
		ARR_RESIZE(list_head, blk_data->node_lists, blk_data->callee_cnt + 2);
		//As arr_resize can change addr of node_lists, pointers have to be adjusted accordingly
		list_head *callee_list = blk_data->node_lists;
		callee_list->prev->next = callee_list;
		callee_list->next->prev = callee_list;
		for (int i = 1; i < blk_data->callee_cnt + 2; i++) {
			INIT_LIST_HEAD(&blk_data->node_lists[i]);
		}
		int cur_callee_cnt = 0;
		list_for_each_entry_safe(node_data, n_data, tmp, callee_list, list) {
			if (n_data->data_type == CALLEE) {
				cur_callee_cnt++;
				continue;
			}
			list_head *node_list = &blk_data->node_lists[cur_callee_cnt + 1];
			n_data->freq_per_byte = (float) n_data->access_cnt / n_data->size;
			if (list_empty(node_list)) {
				list_move(&n_data->list, node_list);
			} else {
				list_for_each_entry(node_data, n_data_iter, node_list, list) {
					if (n_data_iter->freq_per_byte < n_data->freq_per_byte) {
						list_move_tail(&n_data->list, &n_data_iter->list);
						break;
					}
					//adding el to tail of list, if end of list is reached
					if (n_data_iter->list.next == node_list)
						list_move_tail(&n_data->list, node_list);
				}
			}

		}
	}
}

static void pset_insert_set(pset_new_t *a, pset_new_t *b)
{
	pset_new_iterator_t iter;
	node_data *el;
	foreach_pset_new(b, node_data *, el, iter) {
		pset_new_insert(a, el);
	}
}

static int get_set_size_in_bytes(pset_new_t *node_data_set)
{
	pset_new_iterator_t iter;
	node_data *el;
	int size = 0;
	foreach_pset_new(node_data_set, node_data *, el, iter) {
		size += el->size;
	}
	return size;
}

static float get_spm_benefit(dprg_walk_env *env, node_data *n_data, node_data *swapout_candidate)
{
	timestamp *branch = env->cur_branch;
	float block_exec_freq = branch->irg_exec_freq * get_block_execfreq(branch->block);
	float latency_gain = block_exec_freq * n_data->access_cnt * spm_properties.latency_diff;

	float migration_overhead = spm_properties.throughput_spm * n_data->size; //TODO: find approximation
	//Access_cnt of swapout candidate in this block
	int swapout_acc_cnt = 0;
	if (swapout_candidate) {
		if (swapout_candidate->modified)
			migration_overhead += spm_properties.throughput_ram * swapout_candidate->size;

		block_data *blk_data = pmap_get(block_data, env->block_data_map, branch->block);
		list_head *node_list = &blk_data->node_lists[branch->finished_callees + 1];
		list_for_each_entry(node_data, n_data_iter, node_list, list) {
			if (n_data_iter->identifier == swapout_candidate->identifier) {
				swapout_acc_cnt = n_data_iter->access_cnt;
				break;
			}
		}
	}
	float latency_loss = swapout_acc_cnt * block_exec_freq * spm_properties.latency_diff;
	return latency_gain - latency_loss - migration_overhead;
}

static node_data *get_node_data_by_spm_var(dprg_walk_env *env, spm_var_info *var_info)
{
	block_data *blk_data = pmap_get(block_data, env->block_data_map, env->cur_branch->block);
	list_head *node_list = &blk_data->node_lists[env->cur_branch->finished_callees + 1];

	list_for_each_entry(node_data, n_data, node_list, list) {
		if (n_data->identifier == var_info->identifier)
			return n_data;
	}
	return NULL; //Should really never happen
}

static int spm_var_cmp(const void *p1, const void *p2)
{
	return QSORT_CMP((*(spm_var_info * const *) p1)->size, (*(spm_var_info * const *) p2)->size);
}

#define no_candidate(alloc_res, candidate) (pset_new_contains(alloc_res->swapout_set, candidate) \
		|| pset_new_contains(alloc_res->retain_set, candidate) \
		|| pset_new_contains(alloc_res->bring_in_set, candidate))
/* Returns size in bytes of swapout_res*/
static int find_swapout_set(dprg_walk_env *env, alloc_result *alloc_res, node_data *swapin, pset_new_t *swapout_res)
{
	int required_size = swapin->size - alloc_res->free_space;
	int swapout_size = 0;

	pset_new_iterator_t iter;
	spm_var_info *candidate;
	foreach_pset_new(alloc_res->spm_set_reg_size, spm_var_info *, candidate, iter) {
		//Build prio queue?
		if (no_candidate(alloc_res, candidate))
			continue;
		node_data *cand_node_data = get_node_data_by_spm_var(env, candidate);
		if (get_spm_benefit(env, swapin, cand_node_data) > 0) {
			pset_new_insert(swapout_res, candidate);
			required_size -= candidate->size;
			swapout_size += candidate->size;
		}
		if (required_size <= 0)
			break;
	}
	if (required_size > 0) {
		//Sort elements smaller than required size.
		//if no such elements exist, just choose smallest one
		spm_var_info **spm_sorted = NEW_ARR_F(spm_var_info *, 0);
		//cache sorted array (if so, them all spm_set els) ?
		spm_var_info *bigger_than_req = NULL; //also make array for prio list? or accept suboptimal?
		foreach_pset_new(alloc_res->spm_set, spm_var_info *, candidate, iter) {
			if (required_size <= candidate->size) {
				ARR_APP1(spm_var_info *, spm_sorted, candidate);
			} else {
				if (!bigger_than_req)
					bigger_than_req = candidate;
				else
					bigger_than_req = bigger_than_req->size > candidate->size ? candidate : bigger_than_req;
			}
		}
		int spm_sorted_len = ARR_LEN(spm_sorted);
		if (spm_sorted_len > 0) {
			QSORT_ARR(spm_sorted, spm_var_cmp);
			for (int i = 0; i < spm_sorted_len; i++) {
				candidate = spm_sorted[i];
				if (no_candidate(alloc_res, candidate))
					continue;
				node_data *cand_node_data = get_node_data_by_spm_var(env, candidate);
				if (get_spm_benefit(env, swapin, cand_node_data) > 0) {
					pset_new_insert(swapout_res, candidate);
					required_size -= candidate->size;
					swapout_size += candidate->size;
				}
				if (required_size <= 0)
					break;
			}
		} else {
			candidate = bigger_than_req;
			//TODO: avoid code duplication?
			node_data *cand_node_data = get_node_data_by_spm_var(env, candidate);
			if (!no_candidate(alloc_res, candidate) && get_spm_benefit(env, swapin, cand_node_data) > 0) {
				pset_new_insert(swapout_res, candidate);
				required_size -= candidate->size;
				swapout_size += candidate->size;
			}
		}
	}
	return swapout_size;
}

static void spm_calc_alloc_block(dprg_walk_env *env)
{
	timestamp *branch = env->cur_branch;
	ir_node *block = branch->block;
	ir_node *prev_block = branch->last->block;
	block_data *blk_data = pmap_get(block_data, env->block_data_map, block);

	alloc_result *result = XMALLOC(alloc_result);
	result->spm_set = XMALLOC(pset_new_t);
	result->copy_in = XMALLOC(pset_new_t);
	result->copy_out = XMALLOC(pset_new_t);
	result->swapout_set = XMALLOC(pset_new_t);
	result->retain_set = XMALLOC(pset_new_t);
	result->bring_in_set = XMALLOC(pset_new_t);
	pset_new_init(result->spm_set);
	pset_new_init(result->copy_in);
	pset_new_init(result->copy_out);
	pset_new_init(result->swapout_set);
	pset_new_init(result->retain_set);
	pset_new_init(result->bring_in_set);

	block_data *prev_blk_data = pmap_get(block_data, env->block_data_map, prev_block);
	alloc_result *pred_result = prev_blk_data->allocation_results[branch->last->finished_callees];

	//fill result with pred_result values
	pset_insert_set(result->spm_set, pred_result->spm_set);
	pset_insert_set(result->spm_set_reg_size, pred_result->spm_set_reg_size);


	//Handle deadset at beginning of block
	//TODO: how to handle end of call? (do deadset handling also at end of function)
	//
	//TODO: spm_var_info pointer in node_data to avoid hashmap
	pset_new_iterator_t iter;
	node_data *el;
	if (branch->finished_callees == 0) {
		foreach_pset_new(prev_blk_data->dead_set, node_data *, el, iter) {
			spm_var_info *el_info = pmap_get(spm_var_info, spm_var_infos, el);
			int el_size = el->size;
			if (el_size > gp_reg_size) {
				if (pset_new_contains(result->spm_set, el_info)) {
					pset_new_remove(result->spm_set, el_info);
					result->free_space += el_size;
				}
			} else {
				if (pset_new_contains(result->spm_set_reg_size, el_info)) {
					pset_new_remove(result->spm_set_reg_size, el_info);
					result->free_space += el_size;
				}
			}
		}
	}

	list_head *node_list = &blk_data->node_lists[branch->finished_callees + 1];

	list_for_each_entry(node_data, n_data, node_list, list) {
		spm_var_info *el_info = pmap_get(spm_var_info, spm_var_infos, n_data);
		if (pset_new_contains(result->spm_set, el_info) || pset_new_contains(result->spm_set_reg_size, el_info)) {
			if (!pset_new_contains(result->swapout_set, el_info))
				pset_new_insert(result->retain_set, el_info);
		} else {
			if (n_data->size <= result->free_space) {
				if (get_spm_benefit(env, n_data, NULL) > 0.0f) {
					pset_new_insert(result->bring_in_set, el_info);
				}
			} else {
				pset_new_t swapout_for_var;
				pset_new_init(&swapout_for_var);
				int swapout_size = find_swapout_set(env, result, n_data, &swapout_for_var);
				if (swapout_size) {
					pset_insert_set(result->swapout_set, &swapout_for_var);
					pset_new_insert(result->bring_in_set, el_info);
					result->free_space += swapout_size - n_data->size;
				}
			}
		}
	}

	//use swapout + bringin set to build enw spm_sets
	spm_var_info *var_info;
	foreach_pset_new(result->bring_in_set, spm_var_info *, var_info, iter) {
		if (var_info->size > gp_reg_size)
			pset_new_insert(result->spm_set, var_info);
		else
			pset_new_insert(result->spm_set_reg_size, var_info);
	}
	foreach_pset_new(result->swapout_set, spm_var_info *, var_info, iter) {
		if (var_info->size > gp_reg_size)
			pset_new_remove(result->spm_set, var_info);
		else
			pset_new_remove(result->spm_set_reg_size, var_info);
	}
	blk_data->allocation_results[branch->finished_callees] = result;
}

static ir_entity *get_next_call_from_block(dprg_walk_env *env)
{
	timestamp *branch = env->cur_branch;
	ir_node *block = branch->block;
	block_data *blk_data = pmap_get(block_data, env->block_data_map, block);
	int callee_cnt = blk_data->callee_cnt;
	if (!callee_cnt || callee_cnt == branch->finished_callees)
		return NULL;

	int i = 0;
	list_for_each_entry(node_data, n_data, blk_data->node_lists, list) {
		if (i == branch->finished_callees)
			return (ir_entity *) n_data->identifier;
		i++;
	}
	return NULL;
}

static void join_allocations(alloc_result *base_alloc, alloc_result *adj_alloc)
{
	pset_new_iterator_t iter;
	node_data *el;
	foreach_pset_new(base_alloc->spm_set, node_data *, el, iter) {
		if (!pset_new_contains(adj_alloc->spm_set, el)) {
			pset_new_insert(adj_alloc->spm_set, el);
			pset_new_insert(adj_alloc->copy_in, el);
		}
	}
	foreach_pset_new(base_alloc->spm_set_reg_size, node_data *, el, iter) {
		if (!pset_new_contains(adj_alloc->spm_set_reg_size, el)) {
			pset_new_insert(adj_alloc->spm_set_reg_size, el);
			pset_new_insert(adj_alloc->copy_in, el);
		}
	}
	foreach_pset_new(adj_alloc->spm_set, node_data *, el, iter) {
		if (!pset_new_contains(base_alloc->spm_set, el)) {
			pset_new_remove(adj_alloc->spm_set, el);
			pset_new_insert(adj_alloc->copy_out, el);
		}
	}
	foreach_pset_new(adj_alloc->spm_set_reg_size, node_data *, el, iter) {
		if (!pset_new_contains(base_alloc->spm_set_reg_size, el)) {
			pset_new_remove(adj_alloc->spm_set_reg_size, el);
			pset_new_insert(adj_alloc->copy_out, el);
		}
	}
}

static void spm_cond_join_blocks(dprg_walk_env *env)
{
	timestamp *branch = env->cur_branch;
	ir_node *block = branch->block;
	//We select block of last timestamp as base block TODO: Better choice?
	//Idea: select allocation which enhances perf of block the most (iterate var_accesses of block)
	block_data *last_block_data = pmap_get(block_data, env->block_data_map, branch->last->block);
	alloc_result *last_block_res = last_block_data->allocation_results[last_block_data->callee_cnt];
	for (int i = 0; i < get_Block_n_cfgpreds(block); i++) {
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		if (branch->last->block == pred_block)
			continue;

		block_data *pred_block_data = pmap_get(block_data, env->block_data_map, pred_block);
		alloc_result *pred_block_res = pred_block_data->allocation_results[pred_block_data->callee_cnt];
		join_allocations(last_block_res, pred_block_res);
	}
}

static void ensure_pred_blocks_visited(dprg_walk_env *env)
{
	timestamp *branch = env->cur_branch;
	ir_node *block = branch->block;
	for (int i = 0; i < get_Block_n_cfgpreds(block); i++) {
		bool loop_detected = false;
		//Loop detection here:
		if (is_backedge(block, i))
			loop_detected = true;

		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		block_data *pred_block_data = pmap_get(block_data, env->block_data_map, pred_block);
		alloc_result *pred_block_res = pred_block_data->allocation_results[pred_block_data->callee_cnt];
		if (pred_block_res) {
			if (loop_detected)
				branch->finished_preds = 3;
		} else {
			if (loop_detected) {
				branch->finished_preds = 2;
			} else {
				branch->finished_preds = 0;
				return;
			}
		}
	}
	if (branch->finished_preds == -1)
		branch->finished_preds = get_Block_n_cfgpreds(block) > 1 ? 4 : 1;
}

static void spm_mem_alloc_block(dprg_walk_env *env)
{
	timestamp *branch = env->cur_branch;
	ir_node *block = branch->block;

	//Skip block if not all predecessors have been visited (exception: backedge loop)
	if (branch->finished_preds == -1)
		ensure_pred_blocks_visited(env);
	if (branch->finished_preds == 0)
		return;
	if (branch->finished_preds == 3) {
		//do merging here. what to do with callees of loopheader?
	} else if (branch->finished_preds == 4) {
		spm_cond_join_blocks(env);
	}

	//At end of irg next block is caller block again
	//TODO: how to handle multiple returns? compensation code possibly needed for least freq ret blocks
	//has to be inserted before ret instructiion!
	if (get_irg_end_block(get_irn_irg(block)) == block) {
		timestamp *caller = branch->caller_block;
		caller->last = branch;
		caller->finished_callees++;
		deq_push_pointer_right(&env->workqueue, caller);
		return;
	}
	float block_exec_freq = branch->irg_exec_freq * get_block_execfreq(block);
	//calc allocation
	spm_calc_alloc_block(env);

	//handle next call or successor blocks when end of block is reached
	ir_entity *callee = get_next_call_from_block(env);
	if (callee) {
		ir_graph *irg = get_entity_irg(callee);
		ir_node *start_block = get_irg_start_block(irg);
		//Skip irg, if already visited with higher block_freq
		block_data *start_block_data = pmap_get(block_data, env->block_data_map, start_block);
		if (start_block_data->max_exec_freq == block_exec_freq) { //TODO: float compare okay? (does freq has to be float?)
			timestamp *new_branch = XMALLOC(timestamp);
			new_branch->caller_block = branch;
			new_branch->block = start_block;
			new_branch->irg_exec_freq = block_exec_freq;
			deq_push_pointer_right(&env->workqueue, new_branch);
		} else {
			//Add compensation code
			block_data *blk_data = pmap_get(block_data, env->block_data_map, block);
			if (blk_data->compensation_callees == NULL) {
				blk_data->compensation_callees = XMALLOC(pset_new_t);
				pset_new_init(blk_data->compensation_callees);
			}
			pset_new_insert(blk_data->compensation_callees, callee);
			//Add current block again, just as we would after handling the callee
			branch->finished_callees++;
			deq_push_pointer_right(&env->workqueue, branch);
		}
		return;
	}

	if (branch->finished_preds == 2) {
		//TODO: only push loop block onto queue
	} else if (branch->finished_preds == 3) {
		//TODO: only push non-loop block onto queue
	} else {
		int n_outs = get_Block_n_cfg_outs(block);
		for (int i = 0; i < n_outs; i++) {
			ir_node *succ_block = get_Block_cfg_out(block, i);
			timestamp *out_branch = XMALLOC(timestamp);
			out_branch->block = succ_block;
			out_branch->last = branch;
			out_branch->caller_block = branch->caller_block;
			out_branch->irg_exec_freq = branch->irg_exec_freq;
			deq_push_pointer_right(&env->workqueue, out_branch);
		}
	}
}

void spm_find_memory_allocation(node_data * (*retrieve_spm_node_data)(ir_node *))
{
	retrieve_spm_node_data = retrieve_spm_node_data;
	pmap *block_data_map  = pmap_create();
	spm_var_infos = pmap_create();

	foreach_irp_irg(i, irg) {
		ir_estimate_execfreq(irg);
		assure_irg_outs(irg);
		irg_walk_graph(irg, spm_collect_node_data, NULL, block_data_map);
	}
	spm_calc_blocks_access_freq(block_data_map);
	spm_callgraph_walk(spm_collect_block_data, NULL, block_data_map);
	//find main method (only one entry point possible?)
	ir_graph *main_irg = get_irp_main_irg();
	//init metadata list here (actual allocation probably as a map)
	timestamp main_info = {
		.last = NULL,
		.block = get_irg_start_block(main_irg),
		.irg_exec_freq = 1.0f,
	};

	dprg_walk_env walk_env;
	deq_init(&walk_env.workqueue);
	walk_env.cur_branch = &main_info;
	walk_env.block_data_map = block_data_map;
	spm_mem_alloc_block(&walk_env);

	timestamp *cur_branch;
	while ((cur_branch = deq_pop_pointer_left(timestamp, &walk_env.workqueue)) != NULL) {
		walk_env.cur_branch = cur_branch;
		spm_mem_alloc_block(&walk_env);
	}
	//walk cfg number blocks for each block entry calc and for each block exit calc
	//multiple exits -> same number -> merge alloc
	//found call in block: handle callees first
}

/*static void print_node(ir_node *node, void *env) {
	printf("\t%s\n", gdb_node_helper(node));
}*/

void spm_test_call()
{
	/*printf("TESTCALL OUTPUT:\n");
	foreach_irp_irg(i, irg) {
		printf("%s\n", get_entity_ld_name(get_irg_entity(irg)));
		irg_walk_blkwise_graph(irg, NULL, print_node, NULL);
	}*/
}
