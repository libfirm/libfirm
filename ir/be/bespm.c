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

typedef struct alloc_result alloc_result;

typedef enum pred_states {
	UNKNOWN_STATUS, PRED_NOT_DONE, PREDS_DONE, UNFINISHED_LOOP, FINISHED_LOOP, COND_JOIN,
} pred_states;

struct timestamp {
	ir_node *last_block;
	alloc_result *last_alloc;
	timestamp *caller_timestamp;
	ir_node *block;
	int finished_callees;
	pred_states finished_preds;
	double irg_exec_freq;
};

typedef struct spm_var_info {
	void *identifier;
	int size;
	bool modified; //Modified info here not suitable. better set of modified vars in alloc_res
} spm_var_info;

typedef struct spm_content {
	list_head list;
	int addr;
	int next_use; //How many blocks away basically
	int gap_size;
	spm_var_info *content;
} spm_content;

typedef struct spm_transfer {
	//TODO:fixed addresses work only for mov within spm
	int from;
	int to;
} spm_transfer;

struct alloc_result {
	int free_space;
	pset_new_t *spm_set;
	pset_new_t *modified_set; //spm content which has to be written back on eviction
	pmap *copy_in;
	pmap *swapout_set;
	//At end of block compensation code may be added
	pset_new_t *compensation_transfers;
	//Set only temporarily needed:
	pset_new_t *retain_set;
	list_head spm_content_head;
};

typedef enum node_data_type {
	CALLEE, MEM_ACCESS,
} node_data_type;

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
	data->access_cnt = 1;
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
	return spm_get_var_node_data(MEM_ACCESS, id, size, false);
}

node_data *spm_get_mem_write_node_data(void *id, int size)
{
	return spm_get_var_node_data(MEM_ACCESS, id, size, true);
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
	//find_callgraph_recursions(); //Possibly necessary as well

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
			b_data->node_lists = NEW_ARR_F(list_head, 1);
			INIT_LIST_HEAD(b_data->node_lists);
			b_data->callee_cnt = 0;
			b_data->dead_set = NULL;
			b_data->max_exec_freq = 0.0;
			b_data->allocation_results = NULL;
			b_data->compensation_callees = NULL;
			pmap_insert(block_data_map, block, b_data);
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
	double freq = c_env->irg_freq * get_block_execfreq(block);
	block_data *b_data = pmap_get(block_data, c_env->block_data_map, block);
	if (!b_data) {
		b_data = XMALLOC(block_data);
		b_data->node_lists = NEW_ARR_F(list_head, 2);
		INIT_LIST_HEAD(b_data->node_lists);
		INIT_LIST_HEAD(&b_data->node_lists[1]);
		b_data->callee_cnt = 0;
		b_data->max_exec_freq = freq;
		b_data->allocation_results = NEW_ARR_FZ(alloc_result *, 1);
		b_data->compensation_callees = NULL;
		b_data->dead_set = NULL;
		pmap_insert(c_env->block_data_map, block, b_data);
	} else {
		if (freq > b_data->max_exec_freq)
			b_data->max_exec_freq = freq;
	}

	list_head *node_list = &b_data->node_lists[0];

	list_for_each_entry(node_data, n_data, node_list, list) {
		ir_graph *irg = get_entity_irg((ir_entity *) n_data->identifier);
		if (!irg)
			continue;
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

static double get_spm_benefit(dprg_walk_env *env, node_data *n_data, node_data *swapout_candidate)
{
	timestamp *branch = env->cur_branch;
	block_data *blk_data = pmap_get(block_data, env->block_data_map, branch->block);
	double block_exec_freq = blk_data->max_exec_freq;
	double latency_gain = block_exec_freq * n_data->access_cnt * spm_properties.latency_diff;

	double migration_overhead = spm_properties.throughput_spm * n_data->size; //TODO: find approximation
	//Access_cnt of swapout candidate in this block
	int swapout_acc_cnt = 0;
	if (swapout_candidate) {
		if (swapout_candidate->modified)
			migration_overhead += spm_properties.throughput_ram * swapout_candidate->size;

		list_head *node_list = &blk_data->node_lists[branch->finished_callees + 1];
		list_for_each_entry(node_data, n_data_iter, node_list, list) {
			if (n_data_iter->identifier == swapout_candidate->identifier) {
				swapout_acc_cnt = n_data_iter->access_cnt;
				break;
			}
		}
	}
	double latency_loss = swapout_acc_cnt * block_exec_freq * spm_properties.latency_diff;
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

static spm_transfer *create_spm_transfer_in(spm_content *content)
{
	spm_transfer *transfer = XMALLOC(spm_transfer);
	transfer->from = 0; //TODO
	transfer->to = content->addr;
	return transfer;
}

static spm_transfer *create_spm_transfer_out(spm_content *content)
{
	spm_transfer *transfer = XMALLOC(spm_transfer);
	transfer->from = content->addr;
	transfer->to = 0; //TODO
	return transfer;
}

static spm_transfer *create_spm_transfer_mov(spm_content *from, spm_content *to)
{
	spm_transfer *transfer = XMALLOC(spm_transfer);
	transfer->from = from->addr;
	transfer->to = to->addr;
	return transfer;
}

static spm_content *spm_content_insert(spm_var_info *var_info, int gap, spm_content *head)
{
	spm_content *new_spm_content = XMALLOC(spm_content);
	list_add(&new_spm_content->list, &head->list);
	new_spm_content->addr = head->addr + head->content->size;
	new_spm_content->content = var_info;
	new_spm_content->gap_size = gap;
	head->gap_size = 0;
	return new_spm_content;
}

#define no_candidate(alloc_res, candidate) (pset_new_contains(alloc_res->retain_set, candidate) \
		|| pmap_contains(alloc_res->copy_in, candidate))

//How to use spm_content allocation:
//next use is stored for every var
//window of spwapin size is dragged over spm_list to get the element(s) furthest away from use
//
//or: (this alternative is implemented)
//get possible sites where gap later will be minimal and then calculate next use values on the fly x call sites deep (or 10 blocks or so)
//This will lead to eviction of possible more vars then necessary, if write to (var + gap) leaves small gap, whereas (var + var) leaves smaller/no gap
//
//new idea: calc benefit for each window start and include compaction/other movements into evaluation
static void spm_force_insert(dprg_walk_env *env, alloc_result *result, node_data *swapin)
{
	(void) env;

	spm_content **best_fit_gap_el = NEW_ARR_FZ(spm_content *, 1);
	int cur_min_gap_size = INT_MAX;
	//start at first real element (after sentinel)
	spm_content *prev = list_entry(&result->spm_content_head.next, spm_content, list);
	list_for_each_entry(spm_content, var, result->spm_content_head.next, list) {
		if (no_candidate(result, var))
			continue;
		int swapout_size = prev->gap_size + var->content->size + var->gap_size;
		spm_content *next = var;
		while (swapout_size < swapin->size) {
			next = list_entry(next->list.next, spm_content, list);
			if (no_candidate(result, next))
				break;
			swapout_size += next->content->size + next->gap_size;
		}
		int new_gap = swapout_size - swapin->size;
		if (new_gap < 0)
			continue;
		if (new_gap < cur_min_gap_size) {
			free(best_fit_gap_el);
			best_fit_gap_el = NEW_ARR_F(spm_content *, 1);
			best_fit_gap_el[0] = var;
			cur_min_gap_size = new_gap;
		} else if (new_gap == cur_min_gap_size) {
			ARR_APP1(spm_content *, best_fit_gap_el, var);
		}
		prev = var;
	}

	//TODO: if best_fit_gap_el size > 1 calculate next uses
	//TODO: find benefit has to be integrated as well here
	spm_content *best_swapout_candidate = best_fit_gap_el[0];
	if (!best_swapout_candidate) //TODO: maybe have to do more than that
		return;
	prev = list_entry(&best_swapout_candidate->list.prev, spm_content, list);
	int swapout_size = prev->gap_size + best_swapout_candidate->content->size + best_swapout_candidate->gap_size;
	spm_content *next = list_entry(&best_swapout_candidate->list.next, spm_content, list);
	list_del(&best_swapout_candidate->list);
	pmap_insert(result->swapout_set, best_swapout_candidate->content, create_spm_transfer_out(best_swapout_candidate));
	free(best_swapout_candidate);
	while (swapout_size < swapin->size) {
		swapout_size += next->content->size + next->gap_size;
		spm_content *tmp = next;
		next = list_entry(&next->list.next, spm_content, list);
		list_del(&tmp->list);
		pmap_insert(result->swapout_set, tmp->content, create_spm_transfer_out(tmp));
		free(tmp);
	}
	int new_gap = swapout_size - swapin->size;
	spm_var_info *swapin_info = pmap_get(spm_var_info, spm_var_infos, swapin);
	spm_content *new_content = spm_content_insert(swapin_info, new_gap, prev);
	pmap_insert(result->copy_in, swapin_info, create_spm_transfer_in(new_content));
	if (swapin->modified)
		pset_new_insert(result->modified_set, swapin_info);
	result->free_space += new_gap;
}

static bool spm_try_best_fit_insert(alloc_result *result, node_data *var)
{
	spm_var_info *el_info = pmap_get(spm_var_info, spm_var_infos, var);
	spm_content *best_fit_gap_el = NULL;
	int best_fit_gap = INT_MAX;
	list_for_each_entry(spm_content, var, &result->spm_content_head, list) {
		int new_gap = var->gap_size - el_info->size;
		if (new_gap >= 0 && new_gap < best_fit_gap) {
			best_fit_gap = new_gap;
			best_fit_gap_el = var;
			if (new_gap == 0)
				break;
		}
	}
	//If space found insert el
	if (best_fit_gap != INT_MAX) {
		spm_content *new = spm_content_insert(el_info, best_fit_gap, best_fit_gap_el);
		pmap_insert(result->copy_in, el_info, create_spm_transfer_in(new));
		result->free_space -= el_info->size;
		if (var->modified)
			pset_new_insert(result->modified_set, el_info);
		return true;
	}
	return false;
}

static alloc_result *create_alloc_result(void)
{
	alloc_result *result = XMALLOC(alloc_result);
	result->spm_set = XMALLOC(pset_new_t);
	result->modified_set = XMALLOC(pset_new_t);
	result->copy_in = pmap_create();
	result->swapout_set = pmap_create();
	result->free_space = spm_properties.size;
	pset_new_init(result->spm_set);
	pset_new_init(result->modified_set);
	pset_new_init(result->retain_set);
	result->compensation_transfers = NULL;
	INIT_LIST_HEAD(&result->spm_content_head);
	return result;
}

static alloc_result *spm_calc_alloc_block(dprg_walk_env *env)
{
	timestamp *branch = env->cur_branch;
	ir_node *block = branch->block;
	block_data *blk_data = pmap_get(block_data, env->block_data_map, block);

	alloc_result *result = create_alloc_result();
	result->retain_set = XMALLOC(pset_new_t);

	block_data *prev_blk_data = NULL;
	alloc_result *pred_result = branch->last_alloc;
	if (branch->last_block) {
		prev_blk_data = pmap_get(block_data, env->block_data_map, branch->last_block);
	}

	//fill result with pred_result values
	if (pred_result) {
		pset_insert_set(result->spm_set, pred_result->spm_set);
		pset_insert_set(result->modified_set, pred_result->modified_set);
		//build spm_content
		list_for_each_entry(spm_content, var, &pred_result->spm_content_head, list) {
			//TODO: Remove deadset content here as well
			spm_content *spm_content_el = XMALLOC(spm_content);
			spm_content_el->addr = var->addr;
			spm_content_el->gap_size = var->gap_size;
			spm_content_el->content = var->content;
			list_add_tail(&spm_content_el->list, &result->spm_content_head);
		}
		result->free_space = pred_result->free_space;
	} else {
		spm_content *sentinel = XMALLOC(spm_content);
		sentinel->addr = 0;
		sentinel->gap_size = spm_properties.size;
		sentinel->content = NULL;
	}


	//Handle deadset at beginning of block (TODO: deadset also per region?!)
	//TODO: how to handle end of call? (do deadset handling also at end of function)
	//
	//TODO: spm_var_info pointer in node_data to avoid hashmap
	pset_new_iterator_t iter;
	node_data *el;
	if (prev_blk_data && branch->finished_callees == 0 && prev_blk_data->dead_set) {
		foreach_pset_new(prev_blk_data->dead_set, node_data *, el, iter) {
			spm_var_info *el_info = pmap_get(spm_var_info, spm_var_infos, el);
			if (pset_new_contains(result->spm_set, el_info)) {
				pset_new_remove(result->spm_set, el_info);
				result->free_space += el->size;
			}
		}
	}

	list_head *node_list = &blk_data->node_lists[branch->finished_callees + 1];

	list_for_each_entry(node_data, n_data, node_list, list) {
		spm_var_info *el_info = pmap_get(spm_var_info, spm_var_infos, n_data);
		if (pset_new_contains(result->spm_set, el_info)) {
			if (!pmap_contains(result->swapout_set, el_info)) {
				pset_new_insert(result->retain_set, el_info);
				if (n_data->modified)
					pset_new_insert(result->modified_set, el_info);
			}
		} else {
			if (n_data->size <= result->free_space) {
				if (get_spm_benefit(env, n_data, NULL) > 0.0f)
					if (!spm_try_best_fit_insert(result, n_data))
						spm_force_insert(env, result, n_data);
			} else {
				spm_force_insert(env, result, n_data);
			}
		}
	}

	foreach_pmap(result->copy_in, entry) {
		pset_new_insert(result->spm_set, (void *) entry->key);
	}
	foreach_pmap(result->swapout_set, entry) {
		pset_new_insert(result->spm_set, (void *) entry->key);
	}

	//Retain set only temporarily needed
	pset_new_destroy(result->retain_set);
	free(result->retain_set);

	return result;
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

static spm_content *find_var_in_spm_content(list_head *spm_list, spm_var_info *var)
{
	list_for_each_entry(spm_content, var_in_list, spm_list, list) {
		if (var_in_list->content == var)
			return var_in_list;
	}
	return NULL;
}

static void join_allocations(alloc_result *base_alloc, alloc_result *adj_alloc)
{
	//TODO: Clean-up
	pset_new_t *transfers = XMALLOC(pset_new_t);
	pset_new_init(transfers);

	spm_transfer *transfer;
	list_head *next_list_el_adj = adj_alloc->spm_content_head.next;
	spm_content *next_el_adj_alloc = list_entry(next_list_el_adj, spm_content, list);
	list_for_each_entry(spm_content, var, &base_alloc->spm_content_head, list) {
		while (next_el_adj_alloc->addr < var->addr) {
			if (!pset_new_contains(base_alloc->spm_set, next_el_adj_alloc)
			        && pset_new_contains(adj_alloc->modified_set, next_el_adj_alloc)) {
				transfer = create_spm_transfer_out(next_el_adj_alloc);
				pset_new_insert(transfers, transfer);
			}
			if (next_list_el_adj->next != &adj_alloc->spm_content_head) {
				next_list_el_adj = next_list_el_adj->next;
				next_el_adj_alloc = list_entry(next_list_el_adj, spm_content, list);
			} else {
				break;
			}
		}

		if (next_el_adj_alloc->content == var->content
		        && next_el_adj_alloc->addr == var->addr) {
			if (next_list_el_adj->next != &adj_alloc->spm_content_head) {
				next_list_el_adj = next_list_el_adj->next;
				next_el_adj_alloc = list_entry(next_list_el_adj, spm_content, list);
			}
			continue;
		}

		if (pset_new_contains(adj_alloc->spm_set, var->content)) {
			spm_content *el_in_adj_alloc = find_var_in_spm_content(&adj_alloc->spm_content_head, var->content);
			transfer = create_spm_transfer_mov(el_in_adj_alloc, var);
		} else {
			transfer = create_spm_transfer_in(var);
		}
		pset_new_insert(transfers, transfer);
	}

	//There might be more elemenst in adj_alloc spm which have to be written back
	while (next_list_el_adj != &adj_alloc->spm_content_head) {
		next_el_adj_alloc = list_entry(next_list_el_adj, spm_content, list);
		if (!pset_new_contains(base_alloc->spm_set, next_el_adj_alloc)
		        && pset_new_contains(adj_alloc->modified_set, next_el_adj_alloc)) {
			transfer = create_spm_transfer_out(next_el_adj_alloc);
			pset_new_insert(transfers, transfer);
		}
		next_list_el_adj = next_list_el_adj->next;
	}

	adj_alloc->compensation_transfers = transfers;
}

static void join_pred_allocations(dprg_walk_env *env, ir_node *base_block)
{
	timestamp *branch = env->cur_branch;
	ir_node *block = branch->block;
	block_data *base_block_data = pmap_get(block_data, env->block_data_map, base_block);
	alloc_result *base_block_res = base_block_data->allocation_results[base_block_data->callee_cnt];
	//Create comp alloc for all blocks except base_block
	for (int i = 1; i < get_Block_n_cfgpreds(block); i++) {
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		if (pred_block == base_block)
			continue;

		block_data *pred_block_data = pmap_get(block_data, env->block_data_map, pred_block);
		alloc_result *pred_block_res = pred_block_data->allocation_results[pred_block_data->callee_cnt];
		join_allocations(base_block_res, pred_block_res);
	}
}

static ir_node *spm_join_return_blocks(dprg_walk_env *env)
{
	timestamp *branch = env->cur_branch;
	ir_node *block = branch->block;
	//We select block with highest exec freq
	ir_node *base_block = get_Block_cfgpred_block(block, 0);
	for (int i = 1; i < get_Block_n_cfgpreds(block); i++) {
		ir_node *pred_block = get_Block_cfgpred_block(block, i);
		if (get_block_execfreq(pred_block) > get_block_execfreq(base_block))
			base_block = pred_block;
	}
	join_pred_allocations(env, base_block);
	return base_block;
}

static void spm_join_cond_blocks(dprg_walk_env *env)
{
	timestamp *branch = env->cur_branch;
	//ir_node *block = branch->block;
	//We select block of last timestamp as base block TODO: Better choice?
	//Idea: select allocation which enhances perf of block the most (iterate var_accesses of block)
	join_pred_allocations(env, branch->last_block);
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
				branch->finished_preds = FINISHED_LOOP;
		} else {
			if (loop_detected) {
				branch->finished_preds = UNFINISHED_LOOP;
			} else {
				branch->finished_preds = PRED_NOT_DONE;
				return;
			}
		}
	}
	if (branch->finished_preds == UNKNOWN_STATUS)
		branch->finished_preds = get_Block_n_cfgpreds(block) > 1 ? COND_JOIN : PREDS_DONE;
}

static void spm_mem_alloc_block(dprg_walk_env *env)
{
	timestamp *branch = env->cur_branch;
	ir_node *block = branch->block;

	//Skip block if not all predecessors have been visited (exception: backedge loop)
	if (branch->finished_preds == UNKNOWN_STATUS)
		ensure_pred_blocks_visited(env);
	if (branch->finished_preds == PRED_NOT_DONE)
		return;
	if (branch->finished_preds == FINISHED_LOOP) {
		//do merging here. what to do with callees of loopheader?
	} else if (branch->finished_preds == COND_JOIN) {
		spm_join_cond_blocks(env);
	}

	//At end of irg next block is caller block again
	if (get_irg_end_block(get_irn_irg(block)) == block) {
		timestamp *caller = branch->caller_timestamp;
		if (!caller)
			return;
		ir_node *return_block = spm_join_return_blocks(env);
		block_data *return_block_data = pmap_get(block_data, env->block_data_map, return_block);
		alloc_result *return_block_res = return_block_data->allocation_results[return_block_data->callee_cnt];
		caller->last_block = return_block;
		caller->last_alloc = return_block_res;
		caller->finished_callees++;
		deq_push_pointer_right(&env->workqueue, caller);
		return;
	}
	block_data *blk_data = pmap_get(block_data, env->block_data_map, block);
	double block_exec_freq = blk_data->max_exec_freq;
	//calc allocation
	alloc_result *cur_alloc = spm_calc_alloc_block(env);
	blk_data->allocation_results[branch->finished_callees] = cur_alloc;

	//handle next call or successor blocks when end of block is reached
	ir_entity *callee = get_next_call_from_block(env);
	if (callee) {
		ir_graph *irg = get_entity_irg(callee);
		ir_node *start_block = get_irg_start_block(irg);
		//Skip irg, if already visited with higher block_freq
		block_data *start_block_data = pmap_get(block_data, env->block_data_map, start_block);
		if (start_block_data->max_exec_freq == block_exec_freq) { //TODO: float compare okay? (does freq has to be float?)
			timestamp *new_branch = XMALLOC(timestamp);
			timestamp *branch_copy = XMALLOC(timestamp);
			*branch_copy = *branch;
			new_branch->caller_timestamp = branch_copy;
			new_branch->finished_callees = 0;
			new_branch->last_block = block;
			new_branch->last_alloc = cur_alloc;
			new_branch->block = start_block;
			new_branch->irg_exec_freq = block_exec_freq;
			deq_push_pointer_right(&env->workqueue, new_branch);
		} else {
			//Add compensation code
			if (blk_data->compensation_callees == NULL) {
				blk_data->compensation_callees = XMALLOC(pset_new_t);
				pset_new_init(blk_data->compensation_callees);
			}
			pset_new_insert(blk_data->compensation_callees, callee);
			//Add current block again, just as we would after handling the callee
			branch->finished_callees++;
			timestamp *branch_copy = XMALLOC(timestamp);
			*branch_copy = *branch;
			deq_push_pointer_right(&env->workqueue, branch_copy);
		}
		return;
	}

	if (branch->finished_preds == UNFINISHED_LOOP) {
		//TODO: only push loop block onto queue
	} else if (branch->finished_preds == FINISHED_LOOP) {
		//TODO: only push non-loop block onto queue
	} else {
		int n_outs = get_Block_n_cfg_outs(block);
		for (int i = 0; i < n_outs; i++) {
			ir_node *succ_block = get_Block_cfg_out(block, i);
			timestamp *out_branch = XMALLOC(timestamp);
			out_branch->block = succ_block;
			out_branch->last_block = block;
			out_branch->last_alloc = cur_alloc;
			out_branch->caller_timestamp = branch->caller_timestamp;
			out_branch->irg_exec_freq = branch->irg_exec_freq;
			out_branch->finished_callees = 0;
			deq_push_pointer_right(&env->workqueue, out_branch);
		}
	}
}

static void debug_print_block_data(pmap *block_data_map, bool call_list_only)
{
	printf("--- BLOCK DATA ---\n");
	foreach_pmap(block_data_map, cur_entry) {
		const ir_node *blk = cur_entry->key;
		block_data *blk_data = cur_entry->value;
		printf("Block %s\n", gdb_node_helper(blk));
		printf("\tCallee count: %d\n", blk_data->callee_cnt);
		printf("\tMax exec freq: %f\n", blk_data->max_exec_freq);
		for (int i = 0; i < blk_data->callee_cnt + 2; i++) {
			if (i == 0) {
				printf("\tCallee List:\n");
				list_for_each_entry(node_data, n_data, &blk_data->node_lists[i], list) {
					printf("\t\t%s\n", get_entity_name((ir_entity *) n_data->identifier));
				}
				if (call_list_only)
					break;
			} else {
				printf("\tNode List %d:\n", i - 1);
				list_for_each_entry(node_data, n_data, &blk_data->node_lists[i], list) {
					printf("\t\tEntity:%s, ", get_entity_name((ir_entity *) n_data->identifier));
					printf("Size: %d, Acc_cnt: %d", n_data->size, n_data->access_cnt);
					if (n_data->modified)
						printf(", Modified");
					printf("\n");
				}
			}
		}
	}
}

static void free_alloc_result(alloc_result *alloc_res)
{
	pset_new_destroy(alloc_res->spm_set);
	free(alloc_res->spm_set);
	pset_new_destroy(alloc_res->modified_set);
	free(alloc_res->modified_set);
	//TODO: when to free spm_transfer? when transfer nodes are built
	pmap_destroy(alloc_res->copy_in);
	pmap_destroy(alloc_res->swapout_set);
	if (alloc_res->compensation_transfers) {
		pset_new_destroy(alloc_res->compensation_transfers);
		free(alloc_res->compensation_transfers);
	}


	list_for_each_entry_safe(spm_content, spm_var, tmp, &alloc_res->spm_content_head, list) {
		free(spm_var);
	}
}

static void free_block_data(block_data *b_data)
{
	for (int i = 0; i < b_data->callee_cnt + 2; i++) {
		list_for_each_entry_safe(node_data, n_data, tmp, &b_data->node_lists[i], list) {
			free(n_data);
		}
	}
	DEL_ARR_F(b_data->node_lists);

	//Check necessary as end block don't have allocation
	if (b_data->allocation_results[0]) {
		for (int i = 0; i < b_data->callee_cnt + 1; i++) {
			free_alloc_result(b_data->allocation_results[i]);
		}
	}
	DEL_ARR_F(b_data->allocation_results);

	if (b_data->compensation_callees) {
		pset_new_destroy(b_data->compensation_callees);
		free(b_data->compensation_callees);
	}
	if (b_data->dead_set) {
		pset_new_destroy(b_data->dead_set);
		free(b_data->dead_set);
	}
	free(b_data);
}

static void free_block_data_map(pmap *block_data_map)
{
	foreach_pmap(block_data_map, entry) {
		free_block_data(entry->value);
	}
	pmap_destroy(block_data_map);
}

static void free_spm_var_infos(void)
{
	foreach_pmap(spm_var_infos, entry) {
		free(entry->value);
	}
	pmap_destroy(spm_var_infos);
}

void spm_find_memory_allocation(node_data * (*func)(ir_node *))
{
	//TODO: Proper init
	spm_properties.size = 1028;
	spm_properties.latency_diff = 20;
	spm_properties.throughput_ram = 1;
	spm_properties.throughput_spm = 1;
	retrieve_spm_node_data = func;
	pmap *block_data_map  = pmap_create();
	spm_var_infos = pmap_create();

	foreach_irp_irg(i, irg) {
		ir_estimate_execfreq(irg);
		assure_irg_outs(irg);
		irg_walk_graph(irg, spm_collect_node_data, NULL, block_data_map);
	}
	debug_print_block_data(block_data_map, true);
	spm_calc_blocks_access_freq(block_data_map);
	debug_print_block_data(block_data_map, false);
	spm_callgraph_walk(spm_collect_block_data, NULL, block_data_map);
	debug_print_block_data(block_data_map, false);

	//find main method (only one entry point possible?)
	ir_graph *main_irg = get_irp_main_irg();
	//init metadata list here (actual allocation probably as a map)
	timestamp main_info = {
		.last_block = NULL,
		.last_alloc = NULL,
		.caller_timestamp = NULL,
		.block = get_irg_start_block(main_irg),
		.finished_callees = 0,
		.finished_preds = PREDS_DONE,
		.irg_exec_freq = 1.0f,
	};

	dprg_walk_env walk_env;
	deq_init(&walk_env.workqueue);
	walk_env.cur_branch = &main_info;
	walk_env.block_data_map = block_data_map;
	spm_mem_alloc_block(&walk_env);

	timestamp *cur_branch;
	while (!deq_empty(&walk_env.workqueue)) {
		cur_branch = deq_pop_pointer_left(timestamp, &walk_env.workqueue);
		walk_env.cur_branch = cur_branch;
		spm_mem_alloc_block(&walk_env);
		free(cur_branch);
	}

	deq_free(&walk_env.workqueue);
	free_block_data_map(block_data_map);
	free_spm_var_infos();
}

/*static void print_node(ir_node *node, void *env) {
	printf("\t%s\n", gdb_node_helper(node));
}*/
