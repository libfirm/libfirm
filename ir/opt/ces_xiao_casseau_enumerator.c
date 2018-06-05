/* /\* */
/* An Efficient Algorithm for Custom Instruction Enumeration */
/* Chenglong XIAO */
/* University of Rennes I, Irisa, Inria, France */
/* chenglong.xiao@irisa.fr */
/* Emmanuel Casseau */
/* *\/ */

#include "debug.h"
#include "pdeq.h"
#include "pset_new.h"
#include "irnode.h"
#include "irouts.h"
#include "adt/obstack.h"

#include "irgwalk.h"
#include "irgraph.h"
#include "irdump.h"
#include "irnode_t.h"
#include "ces_si_tools.h"
#include "irnodemap.h"

#include <stdint.h>

/******* private methods **********/
void xiao_casseau_walker(ir_node* node, void* envPtr);
void ces_pattern_add(struct pattern* pattern, ir_node* node);
void ces_pattern_allNodes(struct pattern* pattern, ir_node* node);
void ces_pattern_in(struct pattern* pattern);
void 	ces_pattern_out(struct pattern* pattern, ir_node* node);
void ces_pattern_inInput(struct pattern* pattern, ir_node* nodeU) ;
void ces_pattern_perInput(struct pattern* pattern, ir_node* nodeU);
void ces_pattern_disc(struct pattern* pattern, ir_node* node);
void ces_pattern_pred(struct pattern* pattern, ir_node* nodeU);
void ces_pattern_preds(ir_node* nodeU, pset_new_t* preds);
void ces_pattern_ipred(ir_node* nodeU, pset_new_t* ipreds);
void ces_xiao_casseau_enumerator(ir_graph* irg);
void ces_xiao_casseau_result(ir_graph* irg, ir_nodemap* map);
void ces_topo_dumper(void *data, FILE *f, const ir_node *node);
void ces_all_nodes_walker(ir_node* node, void *env);
void ces_topo_walker(ir_node *irn, void *env);
pset_new_t* ces_enum_xiao_casseau(ir_graph* irg, ir_node* start, ir_nodemap* map);
void xiao_casseau_walker(ir_node* node, void* envPtr);
void recursive_pattern_generator(struct pset_new_t* pattern_space, struct pattern* pattern);
int checkConstraint(pset_new_t* set ,unsigned int constraint);
void ces_dump_pattern(struct pattern* pattern) ;
pset_new_t* node_filter(struct pattern* pattern, unsigned int out_max);
void ces_succ_intersect_in_heights(ir_node* succ, void* envPtr);
int ces_irn_is_pred(ir_node* nodeU, ir_node* nodeV);
int ces_succ_in_in(struct pattern* pattern, ir_node* nodeU) ;
void SuccIntersectIN(ir_node* node, void* env_ptr);
bool is_node_in_out_set(ir_node* new_node, pset_new_t* pattern);


/*********private declarations****/
ir_nodemap* ces_block_lookup;

/* /\* */
/*  * checks if node current is disconnected from env->start */
/*  * and if hight is greater than pattern height */
/*  * FNS = FNS ∪ { v|v €DISC((G,P), v > P.getOrder()}; */
/*  * Disconnected nodes of P: Disc(G,P) = {u| u ∈ V, ∀ v ∈ P ,  (u,v)!∈E v (v,u)!∈E} */
/*  *\/ */
/*
void ces_walker_disc(ir_node* current, void* envPtr) {
	struct pattern* env = (struct pattern*)envPtr;
	pset_new_iterator_t iter;
	ir_node* patternElement;

	unsigned int current_height = get_irn_height(env->heights, current);
  //skip if already part of pattern
	if ( !pset_new_contains(env->pattern, current) )
    //skip if order is not greater than patterns order
		if ( current_height > env->pattern_height) {
			//check reachable against all nodes in pattern
			foreach_pset_new(env->pattern, ir_node*, patternElement, iter) {
				if ( heights_reachable_in_block(env->heights, patternElement, current) != 0 )
					return;
			}
			pset_new_insert(env->disc, current);
			env->new_height = current_height > env->new_height ? current_height : env->new_height;
		}
}
*/
//are not all SUCC(node) in pattern? viz. is node part of the out_set of pattern
bool is_node_in_out_set(ir_node* new_node, pset_new_t* pattern) {

	int out_edges = get_irn_n_outs(new_node);
	for (int i = 0; i < out_edges; i++) {
		ir_node* out_edge = get_irn_out(new_node, i);
		if ( !pset_new_contains(pattern, out_edge) ) {
			return true;
		}
	}
	return false;
}

void ces_pattern_ipred(ir_node* nodeU, pset_new_t* ipreds) {

	for(int i=0; i < get_irn_arity(nodeU); i++){
		ir_node* ipred = get_irn_n(nodeU, i);
		if( !pset_new_contains(ipreds, ipred) ) {
			pset_new_insert(ipreds, ipred);
		}
	}
}


void ces_pattern_preds(ir_node* nodeU, pset_new_t* preds) {
	pset_new_t* ipreds = obstack_alloc(ces_obstack, sizeof(pset_new_t));

	pset_new_init(ipreds);

	for(int i=0; i < get_irn_arity(nodeU); i++){
		ir_node* ipred = get_irn_n(nodeU, i);

		if( !is_Block(ipred) ) {
			if( get_nodes_block(ipred) == get_nodes_block(nodeU) && !pset_new_contains(preds, ipred) ) {
				pset_new_insert(preds, ipred);
				ces_pattern_preds(ipred, preds);
			}
		}
	}
}

/*
 * updates predecessor set of pattern
	Pred(G,P')= {
    Pred(G, P) − {u}        if u ∈ Pred(G, P)
		Pred(G, P) ∪ Pred(G, u) if u ∉ Pred(G, P)
	}
 */
void ces_pattern_pred(struct pattern* pattern, ir_node* nodeU) {
	if( pset_new_contains(pattern->pred, nodeU) ) {
		pset_new_remove(pattern->pred, nodeU);
	} else {
		//Pred(G, P) ∪ Pred(G, u)
		pset_new_t* pred = obstack_alloc(ces_obstack, sizeof(pset_new_t));
		pset_new_init(pred);

		ces_pattern_preds(nodeU, pred);
		pset_new_union(pattern->pred, pred);

		pset_new_destroy(pred);
		obstack_free(ces_obstack, pred);
	}
}

/*
 * updates disconnected set of pattern
 * {v| v ∈ Disc(G,P), v>P.getOrder()} = {v| v ∈ G - Pred(P), v>P.getOrder()}
 * depends: allNodes, pred
 */
void ces_pattern_disc(struct pattern* pattern, ir_node* node) {
	ir_node* el;
	pset_new_iterator_t iter;
UNUSED_PARAM(node);
	pattern->disc = obstack_alloc(ces_obstack, sizeof(pset_new_t));
	pset_new_init(pattern->disc);
	// v ∈ G
	pset_new_clone(pattern->disc, pattern->allNodes);
  // - Pred(P)
	foreach_pset_new(pattern->pred, ir_node*, el, iter) {
		pset_new_remove(pattern->disc, el);
	}
	// v > P.getOrder()}
	foreach_pset_new(pattern->disc, ir_node*, el, iter) {
		if ( (intptr_t)ir_nodemap_get_fast(pattern->map, el) <= pattern->pattern_height)
			pset_new_remove_iterator(pattern->disc, &iter);
	}

}

//PerInput(G,P')={v|v ∈ InInput(G, P), v < u} ∪PerInput(G,P)
//calculate this before updateing inInput - dpends on old pattern
//revised
void ces_pattern_perInput(struct pattern* pattern, ir_node* nodeU) {
	pset_new_iterator_t iter;
	ir_node* nodeV;

	foreach_pset_new(pattern->inInput, ir_node*, nodeV, iter) {
		if( !is_Proj(nodeV) && ir_nodemap_get_fast(pattern->map,nodeV) < ir_nodemap_get_fast(pattern->map,nodeU)) {
			pset_new_insert(pattern->perInput, nodeV);
		}
	}
	/* //according to chap.4 external nodes are also added */
	/* for(int i=0; i < get_irn_arity(nodeU); i++) { */
	/* 	ir_node* ipred = get_irn_n(nodeU, i); */

	/* 	if( !is_Block(ipred) && get_irn_n(nodeU, -1) == get_irn_n(ipred, -1)) { */
	/* 		if (is_Proj(ipred)) //skip proj */
	/* 			pset_new_insert(pattern->inInput, get_irn_n(ipred, 0)); */
	/* 		else */
	/* 			pset_new_insert(pattern->perInput, ipred); */
	/* 	} */
	/* } */
}


/*
 * updates out internal input set of pattern
 * revised
 */
//InInput(G, P') = {v|v ∈ InInput(G, P), v > u} ∪ IPred(u)
void ces_pattern_inInput(struct pattern* pattern, ir_node* nodeU) {
	ir_node* el;
	pset_new_iterator_t iter;

	foreach_pset_new(pattern->inInput, ir_node*, el, iter) {
		if ( ir_nodemap_get_fast(pattern->map,el) < ir_nodemap_get_fast(pattern->map, nodeU)) {
			pset_new_remove_iterator(pattern->inInput, &iter);
		}
	}
	// 2.) add IPred(u)
	for (int i = 0; i < get_irn_arity(nodeU); i++) {
		ir_node* ipred = get_irn_n(nodeU, i);

		//assert(!pset_new_contains(pattern->inInput, ipred) );
		if ( pset_new_contains(pattern->inInput, ipred) )
			continue;

		if( !is_Block(ipred) && get_irn_n(nodeU, -1) == get_irn_n(ipred, -1) ) {		//can only be inInput if it is in the same block
			if (is_Proj(ipred))
				pset_new_insert(pattern->inInput, get_irn_n(ipred, 0));
			else
				pset_new_insert(pattern->inInput,ipred);
		}
	}
}

/*
 * updates out set of pattern
 */
void 	ces_pattern_out(struct pattern* pattern, ir_node* node) {
	assure_irg_outs(get_irn_irg(node));
	for(unsigned int i=0; i < get_irn_n_outs(node); i++) {
		ir_node* succ = get_irn_out(node, i);
		if( !pset_new_contains(pattern->pattern, succ) ){
			pset_new_insert(pattern->out, succ);
		}
	}
}

/*
Paper says:
	 IN(G,P) ={v| v∉ Vp && v ∈ V ∪ V+, u ∈ Vp, (v, u) ∈ E ∪ E+ }
I say:
	 equals ∀v ∈ V: ipreds ∪ ipred(v)
Paper says:
	 IN(G,P)=InInput(G,P) ∪ PerInut(G,P)
*/
void ces_pattern_in(struct pattern* pattern) {
	pset_new_init(pattern->in);
	assert(pset_new_size(pattern->in) == 0);

	pset_new_clone(pattern->in, pattern->inInput);
	pset_new_union(pattern->in, pattern->perInput);
}


/*
 * update list of all nodes in block - stored in pattern->allNodes
 * requires irg_walk(ces_aaal_nodes_walker)
 */
void ces_pattern_allNodes(struct pattern* pattern, ir_node* node) {

	ir_node* block = get_irn_n(node, -1);
	pset_new_t* block_set = NULL;
	block_set = ir_nodemap_get_fast(ces_block_lookup, block);
	assert(block_set != NULL && "why u no have nodeset for this block?!");
	pattern->allNodes = block_set;

}

/*
 * add node to pattern, update all fields of pattern
 */
void ces_pattern_add(struct pattern* pattern, ir_node* node) {
	if( is_Proj(node))
		return ces_pattern_add(pattern, get_irn_n(node,0) );

	if( is_Block(node))
		return; //block nodes are not part of patterns

	if (pset_new_contains(pattern->pattern, node) )
		return;

	pset_new_insert(pattern->pattern, node);

	if( pattern->irg == NULL) {
		pattern->irg = get_irn_irg(node);
	}
	assert((pattern->map != NULL) && "topo map is missing");

	unsigned int node_height = (intptr_t)ir_nodemap_get_fast(pattern->map, node);
	pattern->pattern_height = (pattern->pattern_height < node_height)? node_height : pattern->pattern_height;

	ces_pattern_allNodes(pattern, node);
	ces_pattern_pred(pattern, node);
	ces_pattern_perInput(pattern, node); //before inInput
	ces_pattern_inInput(pattern, node);
	ces_pattern_out(pattern, node);
	ces_pattern_disc(pattern, node);// depends allNodes, pred
	ces_pattern_in(pattern); //depends new perInput, inInput
}

void SuccIntersectIN(ir_node* node, void* env_ptr){
	struct env* env = (struct env*)env_ptr;
  // if(succ(G, n)∩IN(G,P)=={})
  //   FNS = FNS ∪ n;
	/* follow firm until block start
	 * if node is not in IN set already put it in fns*/
	if( !is_Block(node) && (env->block == get_nodes_block(node)) ) {
		if( !pset_new_contains(env->in,node) )
			pset_new_insert(env->fns, node);
	}// else
//		break; //break walker if possible
}

int ces_succ_in_in(struct pattern* pattern, ir_node* nodeU) {
	assure_irg_outs(get_irn_irg(nodeU));
	int result = true;

	for(unsigned int i=0; i < get_irn_n_outs(nodeU); i++) {
		ir_node* succ = get_irn_out(nodeU, i);
		if( pset_new_contains(pattern->in, succ) ){
			return false;
		} else {
			result &= ces_succ_in_in(pattern, succ);
			if( result == false )
				return false; //oververbose shortcut
		}
	}
	return result;
}

struct succ {
	struct pattern* pattern;
	pset_new_t* succ;
};

/*
 * return true if there is an edge u->
 */
int ces_irn_is_pred(ir_node* nodeU, ir_node* nodeV){
	for( int i=0; i < get_irn_arity(nodeV); i++) {
		ir_node* pred = get_irn_n(nodeV, i);
		if( pred == nodeU)
			return true;
	}
	return false;
}

void ces_succ_intersect_in_heights(ir_node* succ, void* envPtr){
	struct succ* succEnv = (struct succ*) envPtr;
	ir_node* nodeU = succEnv->pattern->start;
	struct pattern* pattern = succEnv->pattern;

	if( !is_Block(succ) && (get_nodes_block(nodeU) == get_nodes_block(succ)) ) {

		//step1: succ is real succ(nodeU)
		// heights is not symmetric, follows use-def (regular) edges so: from succ to nodeU
		if( (nodeU != succ) && ces_irn_is_pred(succ, nodeU) ) {
//			printf("succ(%li)=%li\n",get_irn_node_nr(nodeU),get_irn_node_nr(succ));
			//Step2: check if succ is !in(pattern)
			if( pset_new_contains(pattern->in, succ) )
				pset_new_insert(succEnv->succ, succ);//add to result -
		}
	}
}


//FNS NodeFilter(P)
pset_new_t* node_filter(struct pattern* pattern, unsigned int out_max) {
	pset_new_iterator_t iter;
	ir_node* element;

	pset_new_t* fns = obstack_alloc(ces_obstack, sizeof(pset_new_t));
	pset_new_init(fns);

	struct succ *env = obstack_alloc(ces_obstack, sizeof(struct succ));
	pset_new_t* succ = obstack_alloc(ces_obstack, sizeof(pset_new_t));

	/*foreach node n ∈ inInput(G,P)*/
	foreach_pset_new(pattern->inInput, ir_node*, element, iter) {
		pset_new_init(succ);
		assert(pset_new_size(succ) == 0);
		env->pattern = pattern;
		env->succ = succ;

		current_ir_graph=pattern->irg;
		ir_node* old_start = pattern->start;
		pattern->start = element;

		//printf("inInput:%li\n",get_irn_node_nr(element));
		irg_out_walk(element, NULL, ces_succ_intersect_in_heights, (void *)env);
		pattern->start = old_start;
		if( pset_new_size(env->succ) == 0 )
			pset_new_insert(fns, element);
	}

	//DEBUG
/*
	foreach_pset_new(fns, ir_node*, element, iter) {
		printf("fns:%li, ",get_irn_node_nr(element));
	}
	printf("\n");
*/
	//if( |out(G,P)| < OUTmax)
	if (pset_new_size(pattern->out) < out_max) {
		// FNS = FNS ∪ { v| €DISC((G,P), v>P.getOrder()};
		pset_new_union(fns, pattern->disc);
	}
	return fns;
}

void ces_dump_pattern(struct pattern* pattern) {
	static int xiao_casseau_counter = 42;
	pset_new_iterator_t iter;
	ir_node* element;

	//annotates the current pattern with a "unique" number and dumps the graph
	foreach_pset_new(pattern->pattern, ir_node* , element, iter) {
		element->annot = xiao_casseau_counter;
	}

	char filename[255];
	snprintf(filename, 255, "xiao_casseau_%i",xiao_casseau_counter);
	dump_ir_graph(pattern->irg, filename);

	DB((ces_dbg, LEVEL_3, "new pattern with %i nodes\n", (int)pset_new_size(pattern->pattern)));
	xiao_casseau_counter++;
}

//returns true if constraint is not violated
int checkConstraint(pset_new_t* set ,unsigned int constraint) {
	if (pset_new_size(set) > constraint)
		return false;
	else
		return true;
}

/*
∪∩
PerInput(G,P') ={v|v∈ InInput(G, P), v < u} ∪PerInput(G, P)
InInput(G,P') = {v|v ∈ InInput(G, P), v > u} ∪ IPred(u)
//inInput(G,P') = {v|v ∈ inInput(G, P), v > u} ∪ IPred(u)
IN(G,P') = PerInput(G,P') ∪ InInput(G, P')
*/

#define OUT_MAX 2
#define IN_MAX 2
#define PERIN_MAX 2
void recursive_pattern_generator(struct pset_new_t* pattern_space, struct pattern* pattern) {
	ir_node* node;

	pset_new_iterator_t iter;

	pset_new_t* filtered_nodes = node_filter(pattern,OUT_MAX);
/*
	printf("filtered nodes of pattern {");
	foreach_pset_new(pattern->pattern, ir_node*, el, iter){
		printf("%s, ",help_node_name(el));
	}
	printf("}:\n");
	foreach_pset_new(filtered_nodes, ir_node*, el, iter){
		printf("%s, ",help_node_name(el));
	}
	printf("\n");
*/
	foreach_pset_new( filtered_nodes, ir_node*, node, iter ) {
		struct pattern*	new_pattern = ces_pattern_copy( pattern);
		ces_pattern_add(new_pattern, node);

		if ( !checkConstraint(new_pattern->out, OUT_MAX) )
			return;
		if( !checkConstraint(new_pattern->perInput, PERIN_MAX) )
			return;
		if( checkConstraint(new_pattern->in, IN_MAX) ) {
			pset_new_insert(pattern_space, new_pattern);
			recursive_pattern_generator(pattern_space, new_pattern);
		} else
			recursive_pattern_generator(pattern_space, new_pattern);
	}

}

//walks over the one-node-pattern-set
void xiao_casseau_walker(ir_node* node, void* envPtr) {
	if( is_Block(node) )
		return; //not iterating blocks

	static int count =0;
	ir_nodemap* map = (ir_nodemap* )envPtr;
//	ir_heights_t* heights = heights_new(get_irn_irg(node));
	struct pset_new_t* pattern_space;

	pattern_space = obstack_alloc(ces_obstack, sizeof(pset_new_t));
	pset_new_init(pattern_space);

	ir_printf("walking pattern no.%i:%+F, topo_idx:%i\n",count, node, ir_nodemap_get_fast(map, node));
	struct pattern* pattern = obstack_alloc(ces_obstack, sizeof(struct pattern));
	ces_pattern_new(pattern, map);
	pattern->start = node;
	ces_pattern_add(pattern, node);

	pset_new_insert(pattern_space, pattern);
	ces_dump_pattern(pattern);
	recursive_pattern_generator(pattern_space, pattern );
	count++;
}


pset_new_t* ces_enum_xiao_casseau(ir_graph* irg, ir_node* start, ir_nodemap* map) {
	DB((ces_dbg, LEVEL_DEFAULT, "ci_enum_xiao_casseau\n"));
	UNUSED_PARAM(start);
	//generates the one-node-set by walking all the nodes in given BB
	irg_walk_graph(irg, NULL, xiao_casseau_walker, (void*) map);
	return NULL;
}



/*
 * stores topological index in nodemap
 */

void ces_topo_walker(ir_node *irn, void *env) {
	ir_nodemap* map = (ir_nodemap* )env;
	static intptr_t counter;

	if( irn == NULL && env == NULL ) {
		counter = 0;
	} else {
		ir_nodemap_insert_fast(map, irn, (void*)counter);
		counter++;
	}
}

/*
 * prepare datastrcuter with set of nodes per block
 * use with wlaker beforehands
 */
void ces_all_nodes_walker(ir_node* node, void *env) {
	ir_nodemap* ces_block_lookup = (ir_nodemap* )env;

	if( !is_Block(node) ) {
		ir_node* block = get_irn_n(node, -1);
		pset_new_t* block_set = ir_nodemap_get_fast(ces_block_lookup, block);
		if( block_set == NULL) {
			block_set = obstack_alloc(ces_obstack, sizeof(pset_new_t));
			pset_new_init(block_set);
			ir_nodemap_insert(ces_block_lookup, block, block_set);
		}
//		ir_printf("adding to block %+F node %+F\n",block, node);
		pset_new_insert(block_set, node);
	}

}

void ces_topo_dumper(void *data, FILE *f, const ir_node *node) {

	int topo = (intptr_t)ir_nodemap_get_fast(data, node);
	fprintf(f, "topo=%u\n", topo);
}



void ces_xiao_casseau_result(ir_graph* irg, ir_nodemap* map) {
	//dump graph containing topological sort
	void *hook = dump_add_node_info_callback(ces_topo_dumper, map);

	dump_ir_graph(irg, "ces_toposort");
	dump_remove_node_info_callback(hook);
}

/*
 *
 * main function. this manages the whole algorithm
 *
 */
void ces_xiao_casseau_enumerator(ir_graph* irg) {
	DB((ces_dbg, LEVEL_DEFAULT, "Xiao_Casseau\n"));

	UNUSED_PARAM(ces_dbg);
	ir_nodemap topo_map;
	ir_nodemap_init(&topo_map, irg);

	ces_block_lookup = XMALLOC(ir_nodemap);// obstack_alloc(ces_obstack, sizeof(ir_nodemap));
	ir_nodemap_init(ces_block_lookup, irg);

	assure_irg_outs(irg);
	ces_topo_walker(NULL, NULL);

//topo_id(start)=0, topo_id(end)=max
//irg_walk(get_irg_end(irg), NULL, ces_topo_walker, (void *)&topo_map);

//topo_id(start=max) topo_id(end)=0
	irg_out_walk(get_irg_start(irg), NULL, ces_topo_walker, (void *)&topo_map);
	irg_walk_blkwise_graph(irg, NULL, ces_all_nodes_walker, (void *)ces_block_lookup);


//start xiao casseau enumerator
	ces_enum_xiao_casseau(irg, get_irg_end(irg), &topo_map);
//start data evaluation
	ces_xiao_casseau_result(irg, &topo_map);

	ir_nodemap_destroy(ces_block_lookup);
	free(ces_block_lookup);

	ir_nodemap_destroy(&topo_map);
}
