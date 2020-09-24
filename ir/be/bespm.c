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

typedef struct dprg_branch dprg_branch;

struct dprg_branch {
	dprg_branch *last;
	ir_node *block;
	int timestamp_entry;
	int timestamp_exit;
	float glb_exec_freq;
	dprg_branch *outs; //branches to be processed before calc exit alloc
};

typedef struct alloc_result {
} alloc_result;

typedef struct drpg_walk_env {
	deq_t workqueue;
	pmap *call_node_map;
	dprg_branch *cur_branch;
} dprg_walk_env;


int (*spm_is_be_Call)(ir_node *node);
/*void calc_irg_execfreq(ir_graph *irg, void *env) {
	ir_estimate_execfreq
}*/

void spm_calculate_dprg_info()
{
	ir_entity **free_methods;
	cgana(&free_methods);

	foreach_irp_irg(i, irg) {
		ir_estimate_execfreq(irg);
	}

	//compute_callgraph();
	//find_callgraph_recursions(); //Possibly necessary as well

	//callgraph_walk(calc_irg_execfreq, NULL, &env);
	free(free_methods);

	//free_callgraph();

}

static void spm_collect_calls(ir_node *node, void *env)
{
	pmap *call_node_map = env;

	if (spm_is_be_Call(node)) {
		ir_node *block = get_nodes_block(node);
		pset_new_t *call_node_set = pmap_get(pset_new_t, call_node_map, block);
		if (!call_node_set) {
			call_node_set = XMALLOC(pset_new_t);
			pset_new_init(call_node_set);
			pmap_insert(call_node_map, block, call_node_set);
		}
		//TODO: insert irg
		pset_new_insert(call_node_set, node);
	}

}

static void init_next_branch_node(dprg_branch *prev, dprg_branch *next)
{
	next->last = prev;
	next->timestamp_entry = prev->timestamp_entry + 1;
	next->timestamp_exit = -1;
	//TODO
}

static void spm_mem_alloc_block(dprg_walk_env *env)
{
	dprg_branch *branch = env->cur_branch;
	ir_node *block = branch->block;
	//calc allocation

	//handle calls
	pset_new_t *call_node_set = pmap_get(pset_new_t, env->call_node_map, block);
	if (call_node_set) {
		pset_new_iterator_t iter;
		ir_graph *irg;
		foreach_pset_new(call_node_set, ir_graph *, irg, iter) {
			dprg_branch *new_branch = XMALLOC(dprg_branch);
			init_next_branch_node(branch, new_branch);
			new_branch->block = get_irg_start_block(irg);
			deq_push_pointer_right(&env->workqueue, new_branch);
		}
	}
	//Successor blocks are handled later on
	int n_outs = get_Block_n_cfg_outs(block);
	branch->outs = XMALLOCN(dprg_branch, n_outs);
	for (int i = 0; i < n_outs; i++) {
		dprg_branch *out_branch = &branch->outs[i];
		ir_node *succ_block = get_Block_cfg_out(block, i);
		out_branch->block = succ_block;
		out_branch->last = branch;
	}
}

void spm_find_memory_allocation()
{
	pmap *call_node_map  = pmap_create();

	foreach_irp_irg(i, irg) {
		assure_irg_outs(irg);
		//Maybe do this at irg processing time, so map contains only info for one irg
		//or find way of walking over nodes in one block only
		irg_walk_graph(irg, NULL, spm_collect_calls, call_node_map);
	}
	//find main method (only one entry point possible?)
	ir_graph *main_irg = get_irp_main_irg();
	//init metadata list here (actual allocation probably as a map)
	dprg_branch main_info = {
		.last = NULL,
		.block = get_irg_start_block(main_irg),
		.timestamp_entry = 0,
		.timestamp_exit = -1, //negative value, as it is just a tmp value
		.glb_exec_freq = 1.0f,
	};
	dprg_walk_env walk_env;
	deq_init(&walk_env.workqueue);
	walk_env.cur_branch = &main_info;
	walk_env.call_node_map = call_node_map;
	spm_mem_alloc_block(&walk_env);

	dprg_branch *cur_branch;
	while ((cur_branch = deq_pop_pointer_left(dprg_branch, &walk_env.workqueue)) != NULL) {
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
