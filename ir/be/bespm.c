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
#include "xmalloc.h"

typedef struct timestamp timestamp;

struct timestamp {
	timestamp *last;
	timestamp *caller_block;
	ir_node *block;
	int finished_callees;
	short finished_preds; //-1: UNKOWN, 0 = FALSE, 1 = TRUE, 2 = LOOP?
	float irg_exec_freq;
};

typedef struct alloc_result {
	int join_cnt;//procedure join cnt
	int free_space;
	pset_new_t *spm_set; //List sorted by freq/byte?
	//list not feasible due to many duplications
	pset_new_t *copy_in;
	pset_new_t *copy_out;
} alloc_result;

typedef enum node_data_type {
	CALLEE, MEM_READ, MEM_WRITE,
} node_data_type;

struct node_data {
	node_data_type data_type;
	list_head list;
	void *identifier; //TODO: change?
	int size; //in bytes
	int access_cnt;
	float freq_per_byte;
};

typedef struct block_data {
	list_head *node_lists;
	int callee_cnt;
	pset_new_t dead_set; //Vars last accessed in this block //TODO: Array?
} block_data;

typedef struct drpg_walk_env {
	deq_t workqueue;
	pmap *block_data_map;
	pmap *res_alloc_map;
	timestamp *cur_branch;
} dprg_walk_env;

struct {
	int size;
	int latency_diff; //ram_lat - spm_lat
} spm_properties;


node_data *(*retrieve_spm_node_data)(ir_node *);
/*void calc_irg_execfreq(ir_graph *irg, void *env) {
	ir_estimate_execfreq
}*/

static node_data *spm_get_node_data_by_type(node_data_type type, void *id, int size)
{
	node_data *data = XMALLOC(node_data);
	data->data_type = type;
	data->identifier = id;
	data->size = size;
	return data;
}

node_data *spm_get_mem_read_node_data(void *id, int size)
{
	return spm_get_node_data_by_type(MEM_READ, id, size);
}

node_data *spm_get_mem_write_node_data(void *id, int size)
{
	return spm_get_node_data_by_type(MEM_WRITE, id, size);
}

node_data *spm_get_callee_node_data(ir_entity *ent)
{
	return spm_get_node_data_by_type(CALLEE, ent, 0);
}

void spm_calculate_dprg_info()
{
	ir_entity **free_methods;
	cgana(&free_methods);


	//compute_callgraph();
	//find_callgraph_recursions(); //Possibly necessary as well

	//callgraph_walk(calc_irg_execfreq, NULL, &env);
	free(free_methods);

	//free_callgraph();

}

static void spm_collect_block_data(ir_node *node, void *env)
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
				free(n_data);
				return;
			}
		}
		if (list_empty(b_data->node_lists)) {
			list_add(&n_data->list, b_data->node_lists);
		}

	}

}

static void spm_calc_blocks_access_freq(pmap *block_data_map)
{
	foreach_pmap(block_data_map, cur_entry) {
		block_data *blk_data = cur_entry->value;
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

static void find_swapout_set(dprg_walk_env *env, alloc_result *alloc_res, pset_new_t *swapout_res)
{
}

static float get_spm_benefit(dprg_walk_env *env, node_data *n_data, node_data *swapout_candidate)
{
	timestamp *branch = env->cur_branch;
	float block_exec_freq = branch->irg_exec_freq * get_block_execfreq(branch->block);
	float latency_gain = block_exec_freq * n_data->access_cnt * spm_properties.latency_diff;

	//Access_cnt of swapout candidate in this block
	int swapout_acc_cnt = 0;
	if (swapout_candidate) {
		block_data *blk_data = pmap_get(block_data, env->block_data_map, branch->block);
		list_head *node_list = &blk_data->node_lists[branch->finished_callees + 1];
		list_for_each_entry(node_data, n_data_iter, node_list, list) {
			if (n_data_iter->identifier == swapout_candidate->identifier) {
				swapout_acc_cnt = n_data_iter->access_cnt;
			}
		}
	}
	float latency_loss = swapout_acc_cnt * block_exec_freq * spm_properties.latency_diff;
	float migration_overhead = 0; //TODO: find approximation
	return latency_gain - latency_loss - migration_overhead;
}

static void spm_calc_alloc_block(dprg_walk_env *env)
{
	timestamp *branch = env->cur_branch;
	ir_node *block = branch->block;
	block_data *blk_data = pmap_get(block_data, env->block_data_map, block);
	alloc_result *existing_block_res = pmap_get(alloc_result, env->res_alloc_map, block);

	pset_new_t *swapout_set = XMALLOC(pset_new_t);
	pset_new_init(swapout_set);
	pset_new_t *retain_set = XMALLOC(pset_new_t);
	pset_new_init(retain_set);
	pset_new_t *bring_in_set = XMALLOC(pset_new_t);
	pset_new_init(bring_in_set);

	alloc_result *result = XMALLOC(alloc_result);
	result->spm_set = XMALLOC(pset_new_t);
	result->copy_in = XMALLOC(pset_new_t);
	result->copy_out = XMALLOC(pset_new_t);
	pset_new_init(result->spm_set);
	pset_new_init(result->copy_in);
	pset_new_init(result->copy_out);

	alloc_result *pred_result = pmap_get(alloc_result, env->res_alloc_map, branch->last->block);

	//TODO: fill result with pred_result values
	pset_insert_set(result->spm_set, pred_result->spm_set);

	//Handle deadset

	list_head *node_list = &blk_data->node_lists[branch->finished_callees + 1];

	list_for_each_entry(node_data, n_data, node_list, list) {
		if (pset_new_contains(result->spm_set, n_data)) {
			if (!pset_new_contains(swapout_set, n_data))
				pset_new_insert(retain_set, n_data);
		} else {
			if (n_data->size <= result->free_space) {
				if (get_spm_benefit(env, n_data, NULL) > 0.0f) {
					pset_new_insert(bring_in_set, n_data);
				}
			} else {
				pset_new_t swapout_for_var;
				pset_new_init(&swapout_for_var);
				find_swapout_set(env, result, &swapout_for_var);
				if (pset_new_size(&swapout_for_var)) {
					pset_insert_set(swapout_set, &swapout_for_var);
					pset_new_insert(bring_in_set, n_data);
					result->free_space += get_set_size_in_bytes(&swapout_for_var) - n_data->size;
				}
			}
		}
	}


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

static void ensure_pred_blocks_visited(dprg_walk_env *env)
{
	timestamp *branch = env->cur_branch;
	ir_node *block = branch->block;
	//Skip block if not all predecessors have been visited
	alloc_result *block_res = pmap_get(alloc_result, env->res_alloc_map, block);
	int cur_block_join_cnt = block_res ? block_res->join_cnt : -1;
	for (int i = 0; i < get_Block_n_cfgpreds(block); i++) {
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		alloc_result *pred_block_res = pmap_get(alloc_result, env->res_alloc_map, pred_block);
		int pred_block_join_cnt = pred_block_res ? pred_block_res->join_cnt : -1;
		if (pred_block_join_cnt <= cur_block_join_cnt) {
			branch->finished_preds = 0;
			return;
		}

		//Loop detection here:
		if (is_backedge(block, i)) {
			branch->finished_preds = 2;
			return;
			//TODO: Loop handling
		}
	}
	branch->finished_preds = 1;
}

static void spm_mem_alloc_block(dprg_walk_env *env)
{
	timestamp *branch = env->cur_branch;
	ir_node *block = branch->block;
	//Skip block if not all predecessors have been visited
	if (branch->finished_preds == -1)
		ensure_pred_blocks_visited(env);
	if (branch->finished_preds == 0)
		return;
	//TODO: Loop handling
	//At end of irg next block is caller block again
	if (get_irg_end_block(get_irn_irg(block)) == block) {
		timestamp *caller = branch->caller_block;
		caller->finished_callees++;
		deq_push_pointer_right(&env->workqueue, caller);
		return;
	}

	float block_exec_freq = branch->irg_exec_freq * get_block_execfreq(block);
	//calc allocation
	//TODO: block_exit calc for block here as well
	spm_calc_alloc_block(env);

	//handle next call or successor blocks when end of block is reached
	ir_entity *callee = get_next_call_from_block(env);
	if (callee) {
		ir_graph *irg = get_entity_irg(callee);
		timestamp *new_branch = XMALLOC(timestamp);
		new_branch->caller_block = branch;
		new_branch->block = get_irg_start_block(irg);
		new_branch->irg_exec_freq = block_exec_freq;
		deq_push_pointer_right(&env->workqueue, new_branch);
		return;
	}

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

void spm_find_memory_allocation(node_data * (*retrieve_spm_node_data)(ir_node *))
{
	retrieve_spm_node_data = retrieve_spm_node_data;
	pmap *block_data_map  = pmap_create();

	foreach_irp_irg(i, irg) {
		ir_estimate_execfreq(irg);
		assure_irg_outs(irg);
		//Maybe do this at irg processing time, so map contains only info for one irg
		//or find way of walking over nodes in one block only
		irg_walk_graph(irg, NULL, spm_collect_block_data, block_data_map);
		spm_calc_blocks_access_freq(block_data_map);
	}
	//find main method (only one entry point possible?)
	ir_graph *main_irg = get_irp_main_irg();
	//init metadata list here (actual allocation probably as a map)
	timestamp main_info = {
		.last = NULL,
		.block = get_irg_start_block(main_irg),
		.irg_exec_freq = 1.0f,
	};

	pmap *res_alloc_map = pmap_create();

	dprg_walk_env walk_env;
	deq_init(&walk_env.workqueue);
	walk_env.cur_branch = &main_info;
	walk_env.block_data_map = block_data_map;
	walk_env.res_alloc_map = res_alloc_map;
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
