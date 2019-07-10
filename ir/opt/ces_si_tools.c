#include "ces_si_tools.h"
#include "ces_agu_emulator.h"

#include "statev.h"
#include "irdump_t.h"
#include <stdint.h>
#include <error.h>
#include "irnodeset.h"
#include "sipart/factor.h"

/* private functions */
static ir_nodemap* color_dump_map = NULL;
int ces_color_dumper_cb(FILE *out, const ir_node *node, const ir_node *local);

void ces_set_color_map(ir_nodemap* map) {
	color_dump_map = map;
}

//void ces_dump_colored_graph(ir_graph* irg, const char * const title, int(*color_dumper)(FILE *out, ir_node *node, ir_node *local) ) {

void ces_dump_colored_graph(ir_graph* irg, const char * const title ) {
	set_dump_node_vcgattr_hook(ces_color_dumper_cb);
	dump_ir_graph(irg, title);
	set_dump_node_vcgattr_hook(NULL);
}

int ces_color_dumper_cb(FILE *out, const ir_node *node, const ir_node *local) {
	UNUSED_PARAM(local);

	int color = (intptr_t)ir_nodemap_get(intptr_t, color_dump_map, node);
	if (color != 0 ) {
		fprintf(out, "color:%i\n", color);
		return 1;
	}
	return 0;
}

void ces_nodemap_list(ir_nodemap* map) {
	int count = ARR_LEN(map->data);
	for (int i=0; i < count; i++)
		ir_printf("id:%i, data:%+F\n",i, map->data[i]);
}

/*
 * generic irg walker
 * walks in depth-first manner
 * param up: true=>follows use-def edges (towards block start) false=> follows def-use (out) edge (towards block end)
 */
void ces_walker(ir_graph* graph, ir_node* start, void (*irg_walk_func)(ir_node *, void *), void* env, int up) {
	UNUSED_PARAM(ces_dbg);
	DB((ces_dbg, LEVEL_DEFAULT, "ces_walker:borked - does not mark nodes, loops forever\n"));
	return;
	static ir_node* block_node = NULL;
	if (block_node == NULL){
		if (is_Block(start)){
			block_node = start;
		} else {
			block_node = get_nodes_block(start);
		}
	}
//		block_node = is_Block(start) ? start : get_nodes_block(start);

	int arity = up ? get_irn_arity(start) : (int)get_irn_n_outs(start);
	for (int i = 0; i < arity; ++i) {
		ir_node * next;
		next = up ? get_irn_n(start, i) : get_irn_out(start, i);
		if (get_nodes_block(next) != block_node)
			break; //we stop at block boundaries
		irg_walk_func(start, env);
		ces_walker(graph, start, irg_walk_func, env, up);
	}
	block_node = NULL;
}

ir_mode* get_memop_mode(const ir_node* node) {
	return is_Load(node) ? get_Load_mode(node) : get_irn_mode(get_Store_value(node));
}

ir_align get_memop_unaligned(const ir_node* node) {
	return is_Load(node)? get_Load_unaligned(node) : get_Store_unaligned(node);
}

ir_volatility get_memop_volatility(const ir_node* node) {
	return is_Load(node)? get_Load_volatility(node) : get_Store_volatility(node);
}


/*
 * deep copy of pset_new
 * expects initialized set
 */
void pset_new_clone(pset_new_t* new, pset_new_t* old) {
	pset_new_iterator_t iter;
	ir_node* element;
	
	foreach_pset_new(old, ir_node*, element, iter) {
		pset_new_insert(new, element);
	}
}

/*
 * deep copy pattern
 * returned pattern is to be freed by caller
 */
struct pattern* ces_pattern_copy(struct pattern* old) {
	struct pattern* new = obstack_alloc(ces_obstack, sizeof(struct pattern) );
	ces_pattern_new(new, old->map);
	new->start = old->start;
	new->irg = old->irg;

	new->pattern_height = old->pattern_height;
	new->allNodes = old->allNodes;
	if (old->pattern) 
		pset_new_clone(new->pattern, old->pattern);
	if (old->in) 
		pset_new_clone(new->in, old->in);
	if (old->out) 
		pset_new_clone(new->out, old->out);
	if (old->disc) 
		pset_new_clone(new->disc, old->disc);
	if (old->inInput) 
		pset_new_clone(new->inInput, old->inInput);
	if (old->perInput) 
		pset_new_clone(new->perInput, old->perInput);
	 
	return new;
}

/*
 * initializes pattern
 */
void ces_pattern_new(struct pattern* pattern, ir_nodemap* topo_map) {
	pattern->pattern = obstack_alloc(ces_obstack, sizeof(pset_new_t));
	pset_new_init(pattern->pattern);

	pattern->in = obstack_alloc(ces_obstack, sizeof(pset_new_t));
	pset_new_init(pattern->in);
	pattern->out = obstack_alloc(ces_obstack, sizeof(pset_new_t));
	pset_new_init(pattern->out);
	pattern->pred = obstack_alloc(ces_obstack, sizeof(pset_new_t));
	pset_new_init(pattern->pred);
	pattern->disc = obstack_alloc(ces_obstack, sizeof(pset_new_t));
	pset_new_init(pattern->disc);
	pattern->inInput = obstack_alloc(ces_obstack, sizeof(pset_new_t));
	pset_new_init(pattern->inInput);
	pattern->perInput = obstack_alloc(ces_obstack, sizeof(pset_new_t));
	pset_new_init(pattern->perInput);

	pattern->start=NULL;
	pattern->map = topo_map;
	pattern->allNodes = NULL;
	pattern->pattern_height = 0;
	pattern->new_height = 0;
	pattern->irg = NULL;
}

/* 
 * set union
 * set2 is merged into set1
 */
void pset_new_union(pset_new_t* set1, const pset_new_t* set2) {
	void* set1_ptr;
	pset_new_iterator_t set1_iter;
	void* set2_ptr;
	pset_new_iterator_t set2_iter;
	foreach_pset_new(set2, void*, set2_ptr, set2_iter) {
		foreach_pset_new(set1, void*, set1_ptr, set1_iter) {
			if ( set1_ptr == set2_ptr) 
				break; //
		}
		if (set1_ptr == NULL) //set2_ptr not yet in set1
			pset_new_insert(set1, set2_ptr);
	}
}


/* 
 * set intersection
 * returned set must be freed by caller 
 * intersection  set1 & set2 is returned
 */
pset_new_t* pset_new_intersect(pset_new_t* set1, pset_new_t* set2) {
	pset_new_t* intersect = obstack_alloc(ces_obstack, sizeof(pset_new_t));
	pset_new_init(intersect);
	
	void* set1_el;
	pset_new_iterator_t set1_iter;
	void* set2_el;
	pset_new_iterator_t set2_iter;
	
	
	foreach_pset_new(set2, void*, set2_el, set2_iter) {
		foreach_pset_new(set1, void*, set1_el, set1_iter) {
			if ( set1_el == set2_el) 
				pset_new_insert(intersect, set1_el);
		}
	}
	return intersect;
}

/*
 * returns height of pattern
 */
int ces_pattern_get_height(struct pattern* pattern) {
	pset_new_iterator_t iter;
	ir_node* node_ptr;

	int height = 0;
	ir_nodemap* map = pattern->map;

	foreach_pset_new(pattern->pattern, ir_node*, node_ptr, iter) {
		int new_height =  (intptr_t)ir_nodemap_get_fast( map, node_ptr);
		height = new_height > height ? new_height : height;
	}
	return height;
}


//ich wollte irgendwas zählen, nur was
void ces_node_stat_walker(ir_node *irn, void *data) {
	ces_node_stats_t *const stats = (ces_node_stats_t*)data;

	switch (get_irn_opcode(irn)) {
	case iro_Phi:
		if (get_irn_mode(irn) == mode_M)
			(*stats)[CES_STAT_MEM_PHIS]++;
		else
			(*stats)[CES_STAT_PHIS]++;
		break;
	case iro_Load:
		(*stats)[CES_STAT_MEMOPS]++;
		(*stats)[CES_STAT_STORES]++;
		break;
	case iro_Store:
		(*stats)[CES_STAT_MEMOPS]++;
		(*stats)[CES_STAT_LOADS]++;
		break;
	case iro_Block:
		(*stats)[CES_STAT_BLOCKS]++;
		break;
	case iro_Sync:
		(*stats)[CES_STAT_SYNC]++;
		break;
	case 	iro_Call:
		(*stats)[CES_STAT_CALL]++;
		break;
	case iro_Cond:
	case iro_Jmp:
	case iro_Cmp:
	case iro_IJmp:
	case iro_End:
	case iro_Return:
	case iro_Start:
		(*stats)[CES_STAT_CFG_NODES]++;
		break;
	default:
		(*stats)[CES_STAT_NORMALS]++;
		break;
	}
	(*stats)[CES_STAT_TOTAL]++;
}

static void ces_stats_emit_blockwise(ir_node* irn, void* data){
	ces_node_stats_t* stats = data;
	static ir_node* block = NULL;
	if (is_Block(irn)) {
		char number[256];
		UNUSED_PARAM(number);
		if (block) {
			stat_ev_ctx_push_fmt("block","%li",get_irn_node_nr(block));
			ces_emit_node_stats(stats,"");
			stat_ev_ctx_pop("block");
		}
		memset(stats, 0, sizeof(*stats));
		block =  irn;
	}
}


void ces_collect_node_stats(ces_node_stats_t *new_stats, ir_graph *irg){
	stat_ev_ctx_push_str("nodestat",get_irg_dump_name(irg));
	irg_walk_graph(irg,ces_node_stat_walker, ces_stats_emit_blockwise, new_stats);
	stat_ev_ctx_pop("nodestat");
}

static const char *get_ces_stat_name(enum ces_stat_tag_t tag) {
	switch (tag) {
	case CES_STAT_PHIS:     return "phis";
	case CES_STAT_MEM_PHIS: return "mem_phis";
	case CES_STAT_MEMOPS:   return "mem_ops";
	case CES_STAT_LOADS:    return "loads";
	case CES_STAT_STORES:   return "stores";
	case CES_STAT_TOTAL:    return "total #";
	case CES_STAT_BLOCKS:   return "blocks";
	case CES_STAT_SYNC:   	return "sync";
	case CES_STAT_CALL:   	return "call";
	case CES_STAT_NORMALS:  return "normals";
	case CES_STAT_ILLEGAL:  return "illegal";
	case CES_STAT_CFG_NODES: return "CFG";
	default:               panic("unknown stat tag found");
	}
	return NULL;
}

void ces_emit_node_stats(ces_node_stats_t *stats, const char *prefix){
	ces_stat_tag_t i;
	UNUSED_PARAM(prefix);

	for (i = CES_STAT_FIRST; i < CES_STAT_MAX; ++i) {
		stat_ev_int(get_ces_stat_name(i), (*stats)[i]);
	}
}

void ces_print_node_stats(ces_node_stats_t *stats) {
	ces_stat_tag_t i;

	for (i = CES_STAT_FIRST; i < CES_STAT_MAX; ++i) {
		DB((ces_dbg, LEVEL_3, "%s:%i\t", get_ces_stat_name(i), (*stats)[i]));
	}
	DB((ces_dbg, LEVEL_3, "\n"));
}


ir_node* ces_load_get_proj(ir_node* load) {
	assert(is_Load(load));
	for(unsigned i=0; i < get_irn_n_outs(load); i++) {
		ir_node* out = get_irn_out(load, i);
		if (is_Proj(out) && (get_Proj_num(out) ==  pn_Load_res) ) {
			return out;
		}
	}
	return NULL;
}

int is_ProjP(ir_node* node) {
	return ( is_Proj(node) && get_irn_mode(node) == get_modeP() )? 1 : 0;
}

void ces_plist_remove_and_advance(plist_t* list, plist_element_t** el) {
	plist_element_t* temp= plist_element_get_next(*el);
	plist_erase(list, *el);
	*el = temp;
}


void plist_insert_all(plist_t* into, plist_t* from) {
	foreach_plist(from, el){
		plist_insert_back(into, el->data);
	}
}

void ces_dump_irg(ir_node* memop, const char const *title) {
	char buf[256];
	ir_snprintf(buf, 256, "%s_%+F",title, memop);
	dump_ir_graph(get_irn_irg(memop), buf);
}

void ces_print_irg(ir_node* node, int indent) {
	ces_print_irg_max(node, indent, 5);
}

void ces_print_irg_max(ir_node* node, int indent, int limit) {
	if (limit == 0)
		return;

	char bla[30]; memset(bla, '\0', 30);
	memset(bla, ' ', indent);
	if (!indent)
		ir_printf("graph %+F\n",node);
	if (is_Load(node))
		ir_printf("%s%+F,%+F\n",bla, node, get_Load_mode(node));
	else
		ir_printf("%s%+F\n",bla, node);
	for(int i=0; i < get_irn_arity(node); i++)
		ces_print_irg_max(get_irn_n(node, i), indent+1, limit-1);
}
void stat_ev_ull_fmt(char const * fmt, ...) {
	va_list argp;
	va_start(argp, fmt);
	char key[256];
	vsnprintf(key, 256, fmt, argp);
	unsigned long value = va_arg (argp, unsigned long);
	stat_ev_ull(key, value);
	va_end(argp);
}

/* find edge from->to switch  with edge from2->to2*/
int ces_exchange_edge(ir_node* from1, ir_node* to1, ir_node* from2, ir_node* to2) {
	int index1 = ces_find_edge(from1, to1);
	int index2 = ces_find_edge(from2, to2);
	set_irn_n(from1, index1, to2);
	set_irn_n(from2, index2, to1);
	return 0;
}

/* finds edge bewtween from -> to*/
int ces_find_edge(ir_node* from, ir_node* to) {
	for (int i=0 ; i < get_irn_arity(from); i++ ) {
		ir_node* use = get_irn_n(from, i);
		if (use == to)
			return i;
	}
	return -1;
}

ir_node* get_memop_ptr(ir_node* memop) {
	return is_Load(memop) ? get_Load_ptr(memop) : get_Store_ptr(memop);
}

/*
 ir_node* ces_get_original(ir_nodemap* ces_copy_map, ir_node* copy) {
	if (ces_copy_map == NULL) {
		DBG((ces_dbg, LEVEL_3, RED("ces_copy_map is NULL \n")));
		return NULL;
	}

	ir_node* temp = ir_nodemap_get(ir_node, ces_copy_map, copy);
	return (temp) ? temp : copy; //if there is no original return the copy again
}
*/


static bool coreisaPredicate(ir_node *node, void *data) {
	ir_nodeset_t* convex_set = data;
	if (ir_nodeset_contains(convex_set, node) && !is_Const(node) )
		return true;
	//exclude nodes set in the start block

	if (is_Sync(node))
		return true;
	if (is_Proj(node) && get_irn_mode(node) == mode_M && !(get_irn_n(node,-1)==get_irg_start_block(get_irn_irg(node))) )
		return true;
	if (is_Store(node))
		return true;
	return false;
}

ir_graph* ces_cut_coreISA(ir_graph* si_irg, struct stream_description* streams);
ir_graph* ces_cut_coreISA(ir_graph* si_irg, struct stream_description* streams) {
	// prepare new IRG
	ir_nodeset_t* inputs = ir_nodeset_new(10);
	ir_nodeset_t* outputs = ir_nodeset_new(10);
	ir_nodeset_t* keepalive= ir_nodeset_new(10);

	for (unsigned int i=0; i<ARR_LEN(streams); i++) {
		DB((ces_dbg, LEVEL_1, "coreISA: memops:%u\n", plist_count(streams[i].memops)));
		foreach_plist(streams[i].memops, el) {
			ir_node* pred = get_irn_n(el->data, 0);
			if (get_irn_mode(pred)== get_modeT())
				continue;
			if (is_Load(el->data))
				ir_nodeset_insert(inputs , get_memop_ptr(el->data));
			if (is_Store(el->data)) {
							ir_nodeset_insert(streams[0].convex_set, el->data);
							//proj after store are transitive. add all those
							//ir_nodeset_insert(streams[0].convex_set, get_irn_out(el->data,0));
			}
		}
	}


	DB((ces_dbg, LEVEL_1, "convex set:\n"));
	foreach_ir_nodeset(streams[0].convex_set, el, iter) {
		DB((ces_dbg, LEVEL_1, "%+F\n", el));
	}
	DB((ces_dbg, LEVEL_1, "input set:\n"));
	foreach_ir_nodeset(inputs, el, iter) {
		DB((ces_dbg, LEVEL_1, "%+F\n", el));
	}
	DB((ces_dbg, LEVEL_1, "output set:\n"));
	foreach_ir_nodeset(outputs, el, iter) {
		DB((ces_dbg, LEVEL_1, "%+F\n", el));
	}

	// cut nodes from irg to coreIRG
	struct factor_env env = {
			.predicate = coreisaPredicate,
			.clientdata = streams[0].convex_set,
			.new_irg = NULL,
			.old_irg = si_irg,
			.inputs = inputs,
			.outputs = outputs,
			.keepalives = keepalive,
			.mem_in = NULL,
			.mem_out = NULL, //set this to the old return->proj
			.block = get_irn_n(ir_nodeset_first(streams[0].convex_set), -1),
	};
	ir_node* pred = get_irn_n(get_irg_end_block(env.old_irg),0);
	ir_node* last_node = get_Return_mem(pred);
	env.mem_out = last_node;
	ir_nodeset_insert(env.clientdata,last_node); //duplicate last mode_M node too
	ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_LINK);
	set_current_ir_graph(env.old_irg);
	factor_subset_core(&env, "_coreISA");

	ir_graph* coreIrg = env.new_irg;
	return coreIrg;
}

