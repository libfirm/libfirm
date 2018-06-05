/*
 * collection of algorithms to ensure convexity of a subgraph
 *
 */
#include "pdeq.h" 
#include "pset_new.h"
#include "irnode.h"
#include "irouts.h"
#include "adt/obstack.h"
#include "heights.h"
#include "irgwalk.h"
#include "irgraph.h"
#include "irtools.h"
#include "ces_convex_graph.h"

static ir_nodemap* node_colors;



/*forward declarations*/
static void ces_set_node_color(ir_node* node, int color);
static int do_convexity_check(ir_node* node, ir_nodeset_t* loads, ir_nodeset_t* stores, ces_node_stats_t* stats, ir_nodeset_t* convex_set);
inline int is_proj_args(ir_node* node);
inline int is_proj_stack(ir_node* node);
inline int is_same_BB(ir_node* node, ir_node* pred);
int is_Float(const ir_node* node);
int is_pred_proj(ir_node* node);
int ces_is_node_illegal(const ir_node* node);

typedef int(*test_fun)(const ir_node* node);

test_fun illegal_node_tests[] = {
	is_Bad,
	is_Call,
	is_Float,
	NULL
};

/* CODE */
int is_same_BB(ir_node* node, ir_node* pred) {
	
	if (is_Const(node) || is_Const(pred))
		return true; //FIRM 'feature': const nodes are associated with the START block

	ir_node* node_BB = is_Block(node) ? node : get_irn_n(node, -1);
	ir_node* pred_BB = is_Block(pred) ? pred : get_irn_n(pred, -1);
	int res = ( node_BB  == pred_BB );
	return res;
}

int is_pred_proj(ir_node* node) {
	if (!is_Proj(node))
		return false;

	return is_Proj(get_Proj_pred(node));
}

int is_proj_args(ir_node* node) {
	if( !(is_Proj(node) && is_pred_proj(node)) ) 
		return false;

	ir_opcode opcode =  get_irn_opcode(get_Proj_pred(get_Proj_pred(node))) ;
	return (opcode == iro_Start );
}

int is_Float(const ir_node* node) {
	return mode_is_float(get_irn_mode(node));
}

int is_proj_stack(ir_node* node) {
	if (!is_Proj(node))
		return false;
	int res = (get_irn_opcode(get_Proj_pred(node)) == iro_Start);
	return res;
}

static void ces_set_node_color(ir_node* node, int color) {
	ir_nodemap_insert(node_colors, node, (void *)(intptr_t)color);
}

static int ces_get_node_color(ir_node* node) {
	return (intptr_t)ir_nodemap_get(intptr_t,node_colors, node);
}

void ces_convex_simple(ir_graph* new_graph, ir_graph* old_graph, ir_node* start, ir_node* stop) {
	/*
		begin at stop
		enumerate all predecessors
		if pred is outside basic block stop
		copy node to new graph
		until closure with start
		user wait_q because it restricts the interface tu push-back & pop-front
	*/
  get_irg_end(old_graph);
	get_irn_arity(start);

	deq_t queue;
	deq_init(&queue);
	deq_push_pointer_right(&queue, stop);
	unsigned int pred_outside_bb = 0;

	do {
		ir_node* node = deq_pop_pointer_left(ir_node, &queue);
		for( int i=0; i < get_irn_arity(node); i++) {
			ir_node* pred = get_irn_n(node, i);
			if ( get_irn_idx( pred ) != get_irn_idx(node) ) {
				pred_outside_bb = 1;
				break; //pred is outside BB - convexity unmet
			} else {
				deq_push_pointer_right(&queue, pred);
			}
		}
		if (pred_outside_bb)
			break; //pred is outside BB - convexity unmet
		else {
			ir_node* node = irn_copy_into_irg(node, new_graph);

		}
	} while( !deq_empty(&queue) );
}

/* /\* */
/*  * spiral search is a clustering */
/*  * search which looks for nodes to cluster starting from the level of the */
/*  * seed and following a spiral path S with center the seed, every time */
/*  * the spiral intersects a level, the nodes of that level (some or all) are */
/*  * analyzed and the ones which respect a predefined property P are */
/*  * included in the cluster */
/*  *\/ */
/* /\* */
/* int node_is_not_float(ir_node* node) { */
/* 	return mode_is_float(node); */
/* } */

/* void ces_spiral_Search(ir_graph* new_graph, ir_graph* old_graph, ir_node* start, ir_node* stop) { */
/* //should work with node idx or store data in user attirbute */
/* //get_irn_idx() */
/* 	properties = { */
/* 		node_is_not_float, */
/* 		NULL */
/* 	}; */
/* 	int distance = 0; //distance from seed */
/* }	 */
/* *\/ */

int do_convexity_check(ir_node* node, ir_nodeset_t* loads, ir_nodeset_t* stores, ces_node_stats_t* stats, ir_nodeset_t* convex_set) {
	int is_convex=true;

	if (ces_get_node_color(node) ) //shortcut: is a color registered, then we do not check again
		return true;

	if (is_Load(node) && ir_nodeset_contains(loads, node)) {
		//colorize qLoads light green
		ces_set_node_color(node, GREEN);
		return true;
	}

	if (is_Store(node) && !ir_nodeset_contains(stores, node)) {
		ces_set_node_color(node,RED);
		return false;
	}

	if (is_Store(node) && is_same_BB(node, get_irn_n(node,n_Store_value)) ) {
		if (do_convexity_check(get_irn_n(node, n_Store_value),loads, stores, stats, convex_set)) {
			ces_set_node_color(node, GREEN);
			return true;
		} else {
			ces_set_node_color(node, RED);
			return true;
		}
	}

	is_convex = !ces_is_node_illegal(node);

	if (is_End(node)) {
		for(int i=0; i < get_End_n_keepalives(node); i++) {
			is_convex &= do_convexity_check(get_End_keepalive(node, i), loads, stores, stats, convex_set);
		}
		ces_set_node_color(node, DARK_GREEN);
		ir_nodeset_insert(convex_set, node);
	}

	int counter=0;
	while( counter < get_irn_arity(node) && is_convex ) {
		ir_node* pred=get_irn_n(node, counter);

		if ( !is_same_BB(node, pred) ) { 
			if (is_proj_args(pred) || is_proj_stack(pred))
				is_convex &= true;
			else
				is_convex &= false;
		} else
			is_convex &= do_convexity_check(pred, loads, stores, stats, convex_set);

		counter++;
	}

	if (!is_convex)
		(*stats)[CES_STAT_ILLEGAL]++;

  //colorize is_convex nodes dark green, bad nodes red
	if (is_convex) {
		ces_set_node_color(node, DARK_GREEN );
		ir_nodeset_insert(convex_set, node);
	} else
		ces_set_node_color(node, RED);
	return is_convex;
}

static void ir_nodeset_insert_all(ir_nodeset_t* dest, ir_nodeset_t* src){
	foreach_ir_nodeset(src, el, iter)
			ir_nodeset_insert(dest, el);
}
int ces_check_convexity(ir_graph* irg, struct stream_description memory_streams[]) {

	ces_node_stats_t node_stats;
	memset(&node_stats, 0, sizeof(node_stats));
	int result = true;
	node_colors = malloc(sizeof(ir_nodemap));
	ir_nodemap_init(node_colors, irg);

	assure_irg_outs(irg); //creates outEdges
	//collect node stats
	irg_walk_graph(irg,ces_node_stat_walker, NULL, node_stats);

	//colorize qLoads light green (borked), convex nodes dark green, bad nodes red

	ir_nodeset_t* loads = ir_nodeset_new(10);
	ir_nodeset_t* stores = ir_nodeset_new(10);
	for(unsigned int i=0; i < ARR_LEN(memory_streams); i++)
		foreach_plist(memory_streams[i].memops, el) {
			if (is_Load(el->data))
				ir_nodeset_insert(loads, el->data);
			else
				ir_nodeset_insert(stores, el->data);
		}

	ir_nodeset_t* temp_list = malloc(sizeof(ir_nodeset_t));
	ir_nodeset_t* convex_set = ir_nodeset_new(10);
	foreach_ir_nodeset(stores, el, iter) {
		ir_nodeset_init(temp_list);
		result |= do_convexity_check(el, loads, stores, &node_stats, temp_list);
		if (result)
			ir_nodeset_insert_all(convex_set,temp_list);
		ir_nodeset_destroy(temp_list);
	}
	ir_nodeset_del(temp_list);
	if (result)
		memory_streams[0].convex_set = convex_set;
	else
		memory_streams[0].convex_set = NULL;

	ces_set_color_map(node_colors);
	DBG_DO(ces_dbg, LEVEL_1, ces_dump_colored_graph(irg,"ces_convex_colored"));

	DB((ces_dbg, LEVEL_1, "convex set: %u:\n",ir_nodeset_size(convex_set) ));
	foreach_ir_nodeset(convex_set, el, iter){
		DB((ces_dbg, LEVEL_1,"node %+F\n", el));
	}
	DB((ces_dbg, LEVEL_1, "selected Qloads are convex:%s\n",( result? "yes" : "no") ));

	ces_print_node_stats(&node_stats);

	ir_nodemap_destroy(node_colors);
	free(node_colors);
	return result;
}

int ces_is_node_illegal(const ir_node* node) {
	for(test_fun* test = illegal_node_tests; *test; test++) {
		if ((*test)(node))
			return true;
	}
	return false;
}
