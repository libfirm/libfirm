/*
 * (c) Martin Haaß 2013
 * CES Henkel, KIT
 *
 * Functions to identify a SI based on the
 * occurence of load/store operations
 */

/* own includes */
#include "ces_ldst_enumerator.h"
#include "ces_convex_graph.h"
#include "ces_xiao_casseau_enumerator.h"
#include "ces_si_tools.h"
#include "ces_extract_base_offset.h"
#include "ces_agu_emulator.h"
#include "ces_time_measure.h"
#include "ces_normalize.h"
#include "ces_merge_memop.h"

/* framework includes */
#include "array.h"
#include "statev.h"
#include "debug.h"
#include "iredges_t.h"
#include "irnode.h"
#include "irgwalk.h"
#include "irdump.h"
#include "irop.h"
#include "iroptimize.h"
#include "irverify.h"
#include "irprintf.h"
#include "adt/plist.h"
#include "tv.h"
#include "dbginfo.h"
#include "obstack.h"
#include "irouts.h"
#include "irdump_t.h"
#include "irnodemap.h"
#include "error.h"
#include "pmap.h"

#include "bearch.h"
#include "bemodule.h"

/* system includes */
#include <assert.h>
#include <math.h>

/* extern declarations */

/* forward declarations */
void ces_free_memlist(pmap* memlist_per_bb);

ir_node* ces_find_block_end(ir_node* start_node, ir_node* block);

src_loc_t ces_get_dbg(ir_node* node);
void ces_split_load_by_base(struct stream_description memory_streams[], plist_t* memops);
int ces_compare_base(void* arg1, void* arg2);
void ces_exchange(ir_node* old, ir_node* new);

void ces_print_pattern_results(ir_node* load, int stride, int count, int width, plist_t* plist_loads);
struct load_base* ces_alloc_load_base(ir_node* memop);

int ces_get_edge_id(ir_node* parent, ir_node* child);
int ces_get_out_id(ir_node* parent, ir_node* child);

int ces_cmp_const(int v1, int v2);
int ces_cmp_equ(ir_node* v1, int c1, ir_node* v2, int c2);
int ces_cmp_offset(struct load_base* base1, struct load_base* base2);
int ces_offset_gt(void* arg1, void* arg2);

struct stream_description* ces_analyse_one_bucket(struct stream_description* const memory_stream);

struct stream_description* ces_create_memory_streams(ir_node* basic_block, plist_t* memops, const char* type);
void ces_cb_enum_memops(ir_node* node, void* env);
struct stream_description* ces_analyse_bb(ir_node* basic_block, struct memLists* memList, ir_graph* irg);


/* private definitions */
const int MIN_SYNC_IN = 2;
const int AGU_NUM = 10;


struct obstack* ces_obstack = NULL; //we setup up our (private) own obstack here
ir_nodemap* ces_nodemark = NULL;
ir_nodemap* ces_load_base = NULL; //stores base struct for all load nodes

struct ces_time* timePreprocess;
/* code */

void ces_free_memlist(pmap* memlist_per_bb) {
	foreach_pmap(memlist_per_bb, entry) {
		struct memLists* memlist = (struct memLists*)entry->value;
		plist_free(memlist->loadList);
		plist_free(memlist->storeList);
		free(memlist);
	}
	pmap_destroy(memlist_per_bb);
}

void ces_exchange(ir_node* old, ir_node* new) {
	ir_graph* irg = get_irn_irg(old);
	assert(edges_activated(irg));
	add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION);
	ir_node* temp = new_Dummy(get_irn_mode(old));

	edges_reroute(old, temp);
	edges_reroute(new, old);
	edges_reroute(temp, new);
	//delete temp
	edges_node_deleted(temp);
	/* noone is allowed to reference this node anymore */
	set_irn_op(temp, op_Deleted);

	clear_irg_constraints(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION);
}

int ces_get_edge_id(ir_node* parent, ir_node* child) {
	for ( int i=0; i < get_irn_arity(parent); i++) {
		if (get_irn_n(parent, i) == child)
			return i;
	}
	return 0;
}

int ces_get_out_id(ir_node* parent, ir_node* child) {
	for (unsigned int i=0; i < get_irn_n_outs(child); i++) {
		if (get_irn_out(child, i) == parent)
			return i;
	}
	return 0;
}

int ces_cmp_const(int v1, int v2) {
	int res = -1;
	res = (v1 > v2) ? 1 : 0;
	res = (v1 < v2) ? -1 : res;
	return res;
}

int ces_cmp_equ(ir_node* v1, int c1, ir_node* v2, int c2) {
	if (v1 == NULL && v2 == NULL)
		return 0; //this means it is equal
	if (v1 != NULL && v2 == NULL)
		return 1;
	if (v1 == NULL && v2 != NULL)
		return -1;
	if (v1 != NULL && v2 != NULL) {
		int result = ces_cmp_const(c1, c2);
		return result;
	}
	assert(0 && "this does not happen");
	return -1; //
}

/**
 * compares two Load_nodes.
 * @param Load_nodes
 * @return 1(true) if node1 value greater than node2 value
 * @return 0(true) if node1 smaller node2 value
 */
int ces_cmp_offset(struct load_base* base1, struct load_base* base2) {
	int result = -1;

	if( base1->base != base2->base)
		assert( 0 && "bases do not match, no use comparing offset");

	result = ces_cmp_equ(base1->x, base1->c1_value, base2->x, base2->c1_value);
	if (result == 0)
		result = ces_cmp_equ(base1->y, base1->c2_value, base2->y, base2->c2_value);
	if (result == 0)
		result = ces_cmp_equ((ir_node*)1, base1->c3_value, (ir_node*)1, base2->c3_value);

	return result;
}

int ces_offset_gt(void* arg1, void *arg2) {
	ir_node* node1 = (ir_node*)arg1;
	ir_node* node2 = (ir_node*)arg2;
	struct load_base* base1 = ir_nodemap_get_fast(ces_load_base, node1);
	struct load_base* base2 = ir_nodemap_get_fast(ces_load_base, node2);
	int result = ces_cmp_offset(base1, base2);

	return (result == 1) ? 1 : 0; //cmp_offset is trinary
}

int ces_compare_base(void* arg1, void* arg2) {
	ir_node* node1 = (ir_node*)arg1;
	ir_node* node2 = (ir_node*)arg2;
	struct load_base* base1 = ir_nodemap_get_fast(ces_load_base, node1);
	struct load_base* base2 = ir_nodemap_get_fast(ces_load_base, node2);
	int result = -1;
	if( base1->base == NULL || base2->base == NULL){
		assert(0 && "base not found");
	}

	if (base1->base == base2->base)
		result = 0;
	if (base1->base < base2->base)
		result = -1;
	if (base1->base > base2->base)
		result = 1;

	return result;
}

/**
 * analysis one list of related MEMOP nodes
 * tries to identify pattern for the AGU of RISPP
 *
 * @param plist of memops
 * @return list of new qMemops. to be freed by caller
 */
struct stream_description* ces_analyse_one_bucket(struct stream_description* const memory_stream) {
	void ces_print_QLoad(ir_node* qload); //extern

	struct ces_time time;
	stat_ev_ctx_push_str("function","analyse_one_bucket");
	ces_time_start(&time);

	plist_t* qMemops = ces_merge_into_qload(memory_stream->memops);

	assert(plist_count(memory_stream->memops) == 0);
	DB((ces_dbg, LEVEL_3, "after qload_merging: %i loads left, %i qLoads created\n",plist_count(memory_stream->memops), plist_count(qMemops)));
	ces_dump_irg(plist_first(qMemops)->data,"after_qmerge");


	const struct agu_strategy* const strategy = ces_agu_strategies(qMemops, &(memory_stream->params), ces_load_base);
	assert(strategy);

	ces_time_stop(&time); stat_ev_ull("strategy",time.cpu);

	DB((ces_dbg, LEVEL_3, "List QLoads with associated consumers\n"));
	if (firm_dbg_get_mask(ces_dbg) <= LEVEL_3) {
		foreach_plist(qMemops, el) {
			ir_node* temp = (ir_node*)(plist_element_get_value(el));
			ces_print_QLoad(temp);
		}
	}
	ces_time_stop(&time);
	stat_ev_ull("analyse_one_bucket",time.cpu);
	stat_ev_ctx_pop("function");

	memory_stream->strategy = *strategy;
	memory_stream->memops = qMemops;
	return memory_stream;
}

/**
 * splits the list of load_nodes in a maximum of AGU_NUM lists
 * sorting the load_nodes by their mem predecessor
 * the last list contains all nodes that did not match any base address
 * @param array of AGU_NUM plists to store the nodes
 * @param plist containing the load_nodes to be sorted
 * @return void
 * @TODO ensure that a single load cannot occupy one of the AGU lists
 */
void ces_split_load_by_base(struct stream_description memory_streams[], plist_t* memops) {

	stat_ev_ctx_push_str("function","split_load_by_base");

	foreach_plist(memops, el)  {
		DBG((ces_dbg, LEVEL_4, "node %+F\n",el->data));
		ir_node* memop = el->data;
		struct load_base* load_base = ces_alloc_load_base(memop);

		ir_graph* addr_irg = ces_dup_subgraph(memop);

		ces_normalize_memop(addr_irg);

		ir_node* new_ptr = get_irn_link(get_memop_ptr(memop));
		assert(load_base != NULL);

		//resulz in load_base are addr_irg-nodes!!
		int result = ces_extract_base_offset(new_ptr, load_base);

		//only to check extract_base_offset
		//ces_print_load_base(memop, load_base);
		if(result < 1 ) DBG((ces_dbg, LEVEL_3,RED("%+F: error(%i) extracting base offset\n"), memop, result));

		//if content is pointer *ahem* it's address > 0x1000 *ahem* get the original node
		if ((intptr_t)load_base->base >1000) load_base->base = get_irn_link(load_base->base);
		if ((intptr_t)load_base->c1 >1000) load_base->c1 = get_irn_link(load_base->c1);
		if ((intptr_t)load_base->c2 >1000) load_base->c2 = get_irn_link(load_base->c2);
		if ((intptr_t)load_base->c3 >1000) load_base->c3 = get_irn_link(load_base->c3);
		if ( load_base->x ) load_base->x = get_irn_link(load_base->x);
		if ( load_base->y ) load_base->y = get_irn_link(load_base->y);

		if( load_base->base == NULL) { //keep one check if base !=0 for bucketising
			DBG((ces_dbg, LEVEL_3, PIRATE("aaaarrgh") "could not find base\n"));
			DBG_DO(ces_dbg, LEVEL_3, dump_ir_graph(addr_irg, "aarrgh_no_base_found"));
			continue;
		}

		//match load with existing lists, last list is reserved for non matching nodes
		int i;
		for( i=0; i < AGU_NUM-1; i++ ) {
			if( (plist_count(memory_streams[i].memops) == 0) || (ces_compare_base(memop, plist_first(memory_streams[i].memops)->data) == 0) ) {
				plist_insert_sorted(memory_streams[i].memops, memop, &ces_offset_gt);
				break;
			}
		}
		if( i == AGU_NUM-1 ) { // no matching list found
			DBG((ces_dbg, LEVEL_3, "node base does not match:%+F\n", memop));
			plist_insert_front(memory_streams[AGU_NUM-1].memops, memop);
		}
		free_ir_graph(addr_irg);
	}
	stat_ev_ctx_pop("function");
}


/**
 * returns src_loc object containing the nodes location within source if applicable
 */
src_loc_t ces_get_dbg(ir_node* node) {
	dbg_info *dbgi = get_irn_dbg_info(node);
	src_loc_t loc =ir_retrieve_dbg_info(dbgi);
	if (loc.file)
		DB((ces_dbg, LEVEL_4, "\t source loc %s %u %u\n", loc.file, loc.line, loc.column));
	return loc;
}





/*
 *
 * uses reverse edges to traverse the tree until the node with highest source line number
 * in block is reached - it is not possible to define the last node in a block, schedule does not
 * yet exist

 */
ir_node* ces_find_block_end(ir_node* start_node, ir_node* block) {
	//ir_printf("reverse edges work dynamic reverse edges nicht\n");
	ir_node* last = NULL;

	if( irn_visited(start_node) ) {
		return start_node;
	}
	mark_irn_visited(start_node);

	int outs = get_irn_n_outs(start_node);
	//ir_printf("start node \"%+F\" has %i out_edges\n",start_node, outs);

	for(int i=0; i < outs; i++) {
		ir_node* child = get_irn_out(start_node, i);
		//ir_printf("out %u: %+F\n",i, temp);
		if ( (is_Block(child) ? child : get_nodes_block(child)) == block) {
			ir_node* temp = ces_find_block_end(child, block);
			if( temp && last) //NULL_PTR guard
				if( !last || ces_get_dbg(last).line < ces_get_dbg(temp).line)
					last = temp;
		}
	}
	/*
//DEMO CODE: BLOCK OUT EDGES
ir_printf("block out edges:\n");
outs = get_Block_n_cfg_outs (block);
for (int i=0; i<outs; i++) {
ir_node* out = get_Block_cfg_out(block,i);
ir_printf("out %u: %+F\n",i, out);
}
	*/
	return last;
}

void ces_print_pattern_results(ir_node* load, int stride, int count, int width, plist_t* plist_loads) {
	assert( is_Load(load) || is_Store(load) );
	ir_mode* mode = is_Load(load) ? get_irn_mode(get_Load_ptr(load)) : get_irn_mode(get_Store_ptr(load));
	DBG((ces_dbg, LEVEL_3, "type is data: %u, float: %u, int: %u, num: %u, ref: %u, signed: %u\n", mode_is_data(mode), mode_is_float(mode), mode_is_int(mode), mode_is_num(mode), mode_is_reference(mode), mode_is_signed(mode)));
	DBG((ces_dbg, LEVEL_3, "type: %+F\n",mode));

	DBG((ces_dbg, LEVEL_3, "this pattern is: stride %u, count %u, data-width: %u\n",stride, count, width));

	ir_node* b_block = get_nodes_block(load);
	DBG((ces_dbg, LEVEL_3, "BB#%+F \n",b_block));
	ir_node* start_node = plist_element_get_value( plist_first(plist_loads) );
	src_loc_t start_loc = ces_get_dbg(start_node);
	ir_node* end_node = NULL;
	src_loc_t end_loc;
	ir_node* block = is_Block(start_node) ? start_node : get_nodes_block(start_node);
	ir_graph* irg= get_irn_irg( block  );

	assure_irg_outs(irg); //creates outEdges
	inc_irg_visited(irg);
	end_node = ces_find_block_end(start_node, block);
	if (end_node) {
		DBG((ces_dbg, LEVEL_3, "end node: \"%+F\"\n",end_node));
		end_loc = ces_get_dbg(end_node);
	}

	if (start_loc.file != NULL)
		DBG((ces_dbg, LEVEL_3, "start loc: %s:%u,%u.\n\n", start_loc.file, start_loc.line, start_loc.column));
	else
		DBG((ces_dbg, LEVEL_3, "start loc:   no source line available\n\n"));
	if (end_loc.file != NULL)
		DBG((ces_dbg, LEVEL_3, "end loc:   %s:%u,%u.\n\n", end_loc.file,  end_loc.line, end_loc.column));
	else
		DBG((ces_dbg, LEVEL_3, "end loc:   no source line available\n\n"));
}

static int get_len(struct stream_description* streams) {
	unsigned int i=0;
	do {
		if (plist_count(streams[i].memops) == 0)
			return i;
		i++;
	} while (i<ARR_LEN(streams));
	return i;
}

/**
 * accepts basic block and list of contained memops,
 * derives memory streams as array of [memops,AGU parameters]
 * until all given memops are accounted for with a stream
 *
 * @param plist of memops (either load or store)
 * @return array of identified streams
 */
struct stream_description* ces_create_memory_streams(ir_node* basic_block, plist_t* memops, const char* type) {
	UNUSED_PARAM(basic_block);
	if(plist_count(memops) <= MIN_SYNC_IN) {
		DB((ces_dbg, LEVEL_3, "only %u %s, skipping\n", type, plist_count(memops) ));
		return NEW_ARR_F(struct stream_description,0);
	}
	DB((ces_dbg, LEVEL_3, BOLD("analysing %u of type %s:")"\n", plist_count(memops), type));

	struct stream_description *memory_streams = NEW_ARR_FZ(struct stream_description,AGU_NUM); //last list is for unmatched base adresses
	for(int i=0; i<AGU_NUM; i++)
		memory_streams[i].memops= plist_new();

	ces_split_load_by_base(memory_streams, memops);
	ARR_SETLEN(struct stream_description, memory_streams, get_len(memory_streams));
	memops = NULL; //provoke exception. memops shall not be used from this point

	unsigned int i=0;
	for( struct stream_description* stream = &memory_streams[0]; i < ARR_LEN(memory_streams); stream = &memory_streams[++i] ) {
		if( i != (unsigned)AGU_NUM-1)
			DB((ces_dbg, LEVEL_4, "\n"BOLD("ir_nodes sorted for AGU list #%u - %u nodes")"\n",i, plist_count(stream->memops)));
		else
			DB((ces_dbg, LEVEL_4, "nodes with non-matching base addresses:\n"));

		if (plist_count(stream->memops) > 0) {
			DB((ces_dbg, LEVEL_3, BOLD("bucket %i, %i nodes\n"),i,plist_count(stream->memops)));
			ces_analyse_one_bucket(stream);
			DB((ces_dbg, LEVEL_4, ".\n"));
		}
	}
	return memory_streams;
}

void ces_cb_enum_memops(ir_node* node, void* env) {
	pmap* memlist_per_bb = (pmap*) env;

	if(is_Load(node) || is_Store(node)) {
		ir_node* block = get_irn_n(node, -1);
		struct memLists* memList = pmap_get(struct memLists, memlist_per_bb, block);

		if(!pmap_contains(memlist_per_bb, block)) {
			memList = malloc(sizeof(struct memLists));
			memList->loadList = plist_new();
			memList->storeList = plist_new();
			pmap_insert(memlist_per_bb, block, memList);
		}

		if (is_Load(node)) plist_insert_back(memList->loadList, node);
		else if (is_Store(node)) plist_insert_back(memList->storeList, node);
	}
}

/*
 * identifys memory streams in the given BB
 * @return stream_description
 */
struct stream_description* ces_analyse_bb(ir_node* basic_block, struct memLists* memList, ir_graph* irg) {

	struct stream_description* load_streams = ces_create_memory_streams(basic_block, (plist_t*)memList->loadList, "loads");
	struct stream_description* store_streams = ces_create_memory_streams(basic_block, (plist_t*)memList->storeList, "store");

	const unsigned n_load  = ARR_LEN(load_streams);
	const unsigned n_store = ARR_LEN(store_streams);


	struct stream_description* all_streams = NEW_ARR_F(struct stream_description, n_load + n_store);
	const size_t elt_size = sizeof(struct stream_description);
	memcpy(all_streams,                     load_streams,  n_load  * elt_size);
	memcpy(all_streams + n_load * elt_size, store_streams, n_store * elt_size);

	int res = ces_check_convexity(irg, all_streams);
	if (res)
		DBG((ces_dbg, LEVEL_1, "irg is " BOLD("convex")"\n" ));
	else
		DBG((ces_dbg, LEVEL_1, "irg " RED("NOT") "convex\n" ));
	return all_streams;
}

/*
 * duplicate graph and prepare data structure to identify memory streams
 */

ir_graph* prepare_si_irg_from(ir_graph* irg) {
	extern void add_irp_irg(ir_graph *irg);
	ir_graph* si_irg;
	si_irg = create_irg_copy(irg);
	set_irg_entity(si_irg,get_irg_entity(irg));
	add_irp_irg(si_irg);
	ces_load_base = XMALLOC(ir_nodemap);// obstack_alloc(ces_obstack, sizeof(ir_nodemap));
	ir_nodemap_init(ces_load_base, si_irg);
	return si_irg;
}

/*
 * searches memops in irg, identifies memory patterns and replaces memops by 128bit-memops
 * work is done on copy of irg, original is unmodified.
 * sets current_ir_graph to new graph!
 * Expects IRG without control flow e.g. if-then-else tranformed to mux
 */
struct stream_description* ces_identify_streams(ir_graph* si_irg) {
	DBG((ces_dbg, LEVEL_1, RED("identify streams in irg \"%s\"!")"\n", get_irg_dump_name(si_irg)));
	si_irg = prepare_si_irg_from(si_irg);
	current_ir_graph = si_irg;
//TODO:find out if still necessary
	//set_opt_optimize(0); //formerly used to disable deduplication
	//clear_irg_constraints(get_irn_irg(node), IR_GRAPH_CONSTRAINT_ARCH_DEP);
	//ir_node* optimized_node = op_Shl->ops.transform_node(node);
	// int result = op_Shl->ops.reassociate(&node);

//collect memops in lists per BB
	pmap* memlist_per_bb = pmap_create();
	//collect all memops
	irg_walk_graph(si_irg, ces_cb_enum_memops, NULL, (void*)memlist_per_bb);
//collect memops END

//ananlyse each BB
	struct stream_description* streams = NULL;
	foreach_pmap(memlist_per_bb, entry) {
		long block_id = get_irn_node_nr(entry->key);
		struct memLists* memList = (struct memLists*)entry->value;
		DB((ces_dbg, LEVEL_1, BOLD("\nenumerating BB%u %+F")"\n", block_id, (ir_node*)entry->key));
		assert(streams == NULL && "warning, about to overwrite reuslts");
		streams = ces_analyse_bb( (ir_node*)entry->key, memList, si_irg);

		int result = ces_check_convexity(get_irn_irg((ir_node*)entry->key), streams);
		assert(result || "not convex ;(");
	}
//ananlyse each BB END

	ces_free_memlist(memlist_per_bb);
	ir_nodemap_destroy(ces_load_base);
	free(ces_load_base);

	ces_set_color_map(ces_nodemark);
	DBG_DO(ces_dbg, LEVEL_3, ces_dump_colored_graph(si_irg, "ces_after_ldst"));

	return streams;
}
