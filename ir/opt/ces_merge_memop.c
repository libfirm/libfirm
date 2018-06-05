/* own includes */
#include "ces_ldst_enumerator.h"
#include "ces_convex_graph.h"
#include "ces_xiao_casseau_enumerator.h"
#include "ces_si_tools.h"
#include "ces_extract_base_offset.h"
#include "ces_agu_emulator.h"
#include "ces_time_measure.h"
#include "ces_normalize.h"

/* framework includes */
#include "statev.h"
#include "debug.h"
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
extern ir_nodemap* ces_load_base;

/* forward declarations */
void ces_qload_preds_clear(void);
struct load_base* ces_mop_new(ir_node* memop, struct load_base* old_base, ir_node* mem_pred, ir_node** memop_wide);
int ces_unset_pred(ir_node* node, ir_node* pred);
void ces_minimize_memdeps(ir_node* qload);
void ces_add_to_mop(ir_node* memopQ, ir_node* memop);
void ces_print_QLoad(ir_node* qload);
int ces_replace_memop_slice(ir_node* from, ir_node* to, ir_node* memop);
void ces_accu_finalize(plist_t* qload_list, ir_node* qload);
plist_t* ces_merge_into_qload(plist_t* plist_loads);
struct load_base* ces_alloc_load_base(ir_node* memop);
void ces_print_load_base(ir_node* node, struct load_base* base);

/* local declarations */


ir_node* qLoad_prePreds = NULL;
#define QLOAD_PREDS 20
static ir_node* qload_preds[QLOAD_PREDS];

void ces_qload_preds_clear() {
	memset(qload_preds, 0, sizeof(ir_node*)*QLOAD_PREDS);
}

struct load_base* ces_alloc_load_base(ir_node* memop) {
	struct load_base * base = obstack_alloc(ces_obstack, sizeof(struct load_base));
	memset(base, 0, sizeof(struct load_base));
	ir_nodemap_insert(ces_load_base, memop, base);
	base->mode = get_memop_mode(memop);
	base->size = get_mode_size_bits(base->mode);
	return base;
}

void ces_print_load_base(ir_node* node, struct load_base* base) {
	static const int len = 1024;
	char text[len];
	char formula[len];
	char* ptr = text;
	char* formula_ptr = formula;

	if( base->base == NULL)
		ptr += ir_snprintf(ptr, len - (ptr - text), "%+F base:unknown ",node);
	else
		ptr += ir_snprintf(ptr, len - (ptr - text), "%+F base:%+F ",node, base->base);
	formula_ptr += ir_snprintf(formula_ptr, len-(formula_ptr - formula), "%+F:\t(%+F) + ",node, base->base);

	if( (intptr_t)base->c1 > 1000)
		ptr += ir_snprintf(ptr, len - (ptr - text), "c1(%+F): %li, ",base->c1, base->c1_value);
	else
		ptr += ir_snprintf(ptr, len - (ptr - text), "c1(noNode): %li, ", base->c1_value);
	formula_ptr += snprintf(formula_ptr, len-(formula_ptr - formula), "%ix",base->c1_value);

	if( (intptr_t)base->x)
		ptr += ir_snprintf(ptr, len - (ptr - text), "x:%+F ",base->x);
	else
		ptr += ir_snprintf(ptr, len - (ptr - text), "x:0 ");

	if( base->c2)
		ptr += ir_snprintf(ptr, len - (ptr - text), "c2(%+F): %i, ", get_irn_link(base->c2), base->c2_value);
	else
		ptr += ir_snprintf(ptr, len - (ptr - text), "c2(const): %li, ", base->c2_value);
	formula_ptr += snprintf(formula_ptr, len-(formula_ptr - formula), " + %iy",base->c2_value);

	if( (intptr_t)base->y)
		ptr += ir_snprintf(ptr, len - (ptr - text), "y:%+F, ",base->y);
	else{
		ptr += ir_snprintf(ptr, len - (ptr - text), "y:0, ");

	if( base->c3)
		ptr += ir_snprintf(ptr, len - (ptr - text), "c3(%+F): %i, ",base->c3, base->c3_value);
	else
		ptr += ir_snprintf(ptr, len - (ptr - text), "c3(const): %li, ", base->c3_value);
	formula_ptr += snprintf(formula_ptr, len-(formula_ptr - formula), " + %i",base->c3_value);
	}
	*ptr='\n';
	*formula_ptr='\n';
//	DBG((ces_dbg, LEVEL_3, text));
	DB((ces_dbg, LEVEL_4, formula));
}

int ces_unset_pred(ir_node* node, ir_node* pred) {
	for (int i=0; i < get_irn_arity(node); i++) {
		if (get_irn_n(node, i) == pred) {
			remove_irn_n(node,i);
			return true;
		}
	}
	return false;
}

void ces_print_QLoad(ir_node* qload) {
	struct load_base* base = ir_nodemap_get_fast(ces_load_base, qload);
	assure_irg_outs(get_irn_irg(qload));

	for(unsigned int i=0; i < get_irn_n_outs(qload); i++) {
		if (get_Proj_proj(get_irn_out(qload, i)) == pn_Load_res) {
			DBG((ces_dbg, LEVEL_1, "qLoad %+F(%i), base:%+F, %i loads\n", qload,
					 get_mode_size_bits(get_memop_mode(qload)), base->base, get_irn_n_outs(get_irn_out(qload, i))));
			ces_print_load_base(qload, base);
			for(unsigned int j=0; j < get_irn_n_outs(get_irn_out(qload, i)); j++) {
				ir_node* slice = get_irn_out(get_irn_out(qload, i), j);
				DB((ces_dbg, LEVEL_1, "slice %+F (%i-%i)\n",slice, get_Slice_from(slice), get_Slice_to(slice)));
			}
		}
	}
}

/*
 * removes dplucate edges of the form:
 * sync -> proj -> Qload
 * *child *proj   *qload
 */
void ces_minimize_memdeps(ir_node* qload) {
	assure_irg_outs(get_irn_irg(qload));
	for (unsigned  i=0 ; i < get_irn_n_outs(qload); i++ ) {
		ir_node* proj = get_irn_out(qload, i);
		if (is_Proj(proj) && get_irn_mode(proj) == mode_M) {

			for (unsigned  j=0 ; j < get_irn_n_outs(proj); j++ ) {
				ir_node* child = get_irn_out(proj, j);

				ir_node** pre = qload_preds;
				while( (*pre != child) && (*pre != NULL) && (pre < &qload_preds[QLOAD_PREDS]) ) {
					pre++;
				}

				if (*pre == child) {
					ces_unset_pred(child, proj);
					ces_unset_pred(proj, qload);
				}
				if (*pre == NULL)
					*pre = child;
				break;
			}
		}
	}
}

/*
 * walks from def to use
 * checks all use->def edges
 * if def == from replaces with to
 * use->from to use->to || use->memop
 * */
int ces_replace_memop_slice(ir_node* from, ir_node* to, ir_node* memop) {
	int count=0;
	assure_irg_outs(get_irn_irg(from));
	for (unsigned  i=0 ; i < get_irn_n_outs(from); i++ ) {
		ir_node* use = get_irn_out(from, i);
		for (int  j=0 ; j < get_irn_arity(use); j++ ) {
			//replace with slice unless memory_edge, rewire this to load
			if ( is_Proj(use) && (get_Proj_proj(use) == pn_Load_M || get_Proj_proj(use) == pn_Store_M ) ) {
				assert(get_irn_n(use,j) == from); // if this hits analyse situation. prob. move cond. into if
				set_irn_n(use, j, memop);
			} else
			if ( is_Proj(use) && (get_Proj_proj(use) == pn_Load_res) ) {
				ces_replace_memop_slice(use,to,memop);
			} else {
				if(get_irn_n(use,j) == from)
					set_irn_n(use, j, to);
			}
			count++;
		}
	}
	return count;
}

void ces_accu_finalize(plist_t* qload_list, ir_node* qload) {
	plist_insert_back(qload_list, qload);
	ces_minimize_memdeps(qload);
}

static void ces_memop_fix_mode(ir_node* memop, struct load_base* accu) {
	unsigned bit_count = 0;
	foreach_irn_in(memop, idx, pred) {
		bit_count += get_mode_size_bits(get_irn_mode(pred));
	}
	accu->size = bit_count;
	accu->mode = new_int_mode("pack_mode", irma_twos_complement, bit_count, 0, 32);
	set_irn_mode(memop, accu->mode);
}

//creates new merged mem op - mop.
//needs to be extended for store
struct load_base* ces_mop_new(ir_node* memop, struct load_base* old_base, ir_node* mem_pred , ir_node** memop_wide) {
	dbg_info* dbgi = NULL; /* get_irn_dbg_info(get_irn_link(memop)); */
	ir_node* block = get_irn_n(memop, -1);
	current_ir_graph = get_irn_irg(memop);
	add_irg_constraints(get_irn_irg(memop), IR_GRAPH_CONSTRAINT_CONSTRUCTION);

	ir_cons_flags flags = get_memop_volatility(memop) | get_memop_unaligned(memop);

	switch (get_irn_opcode(memop)) {
	case iro_Load: {
		ir_node *pred_proj = (is_Proj(mem_pred)||is_Sync(mem_pred))? mem_pred : new_rd_Proj(dbgi, mem_pred, get_modeM(), pn_Load_M);
		*memop_wide = new_rd_Load(dbgi, block, pred_proj, get_memop_ptr(memop), mode_LLu, flags);
		ir_node* proj_res = new_rd_Proj(dbgi, *memop_wide, mode_LLu, pn_Load_res);
		set_irn_link(*memop_wide,proj_res);
		break;
	}
	case iro_Store: {
		ir_node *pred_proj = (is_Proj(mem_pred)||is_Sync(mem_pred))? mem_pred : new_rd_Proj(dbgi, mem_pred, get_modeM(), pn_Store_M);
		ir_node* data[]= {get_Store_value(memop)};
		ir_node* pack = new_rd_Pack(dbgi, block, 1, data, get_modeLLu() );
		*memop_wide = new_rd_Store(dbgi, block, pred_proj, get_memop_ptr(memop), pack, flags);
		//bug hier. ein add zu viel, evtl der skip teil in de replace
		ces_replace_memop_slice(get_Store_value(memop),memop, pack);
		ces_replace_memop_slice(memop,*memop_wide,*memop_wide);
		break;
	}
	default:
		panic("unexpected node type \n");
	}

	/* check QLoad M proj preds file local array with preds*/
	ces_qload_preds_clear();

	struct load_base* accu = ces_alloc_load_base(*memop_wide);
	memcpy(accu, old_base, sizeof(struct load_base));

	ir_nodemap_insert(ces_load_base, *memop_wide,  accu);
	//ces_memop_fix_mode(*memop_wide, accu);// triggers other bugs

	clear_irg_constraints(get_irn_irg(memop), IR_GRAPH_CONSTRAINT_CONSTRUCTION);
	return accu;
}

static void ces_add_to_mop_load(ir_node* memopQ, ir_node* memop) {
	struct load_base* current = ir_nodemap_get_fast(ces_load_base, memop);
	struct load_base* accu = ir_nodemap_get_fast(ces_load_base, memopQ);
	//firm/cparser/sparc uses byte-adressing
  //	size_t bit_per_word = get_mode_size_bits(get_modeP());
	const size_t bit_per_word = 8;

	dbg_info* dbgi = NULL; /* get_irn_dbg_info(get_irn_link(memop)); */
	//from & to are absolute bits within QWord
	long slice_from = current->c3_value*bit_per_word - accu->c3_value*bit_per_word;
	long slice_to = slice_from + current->size -1;
	ir_node* memopQ_res = get_irn_link(memopQ);
	ir_node* slice = new_rd_Slice(dbgi,memopQ_res, get_Load_mode(memop), slice_from , slice_to);

	ces_replace_memop_slice(memop, slice, memopQ);
	set_irn_mode(slice, get_memop_mode(memop));
}

static void ces_add_to_mop_store(ir_node* memopQ, ir_node* memop) {
	struct load_base* current = ir_nodemap_get_fast(ces_load_base, memop);
	struct load_base* accu = ir_nodemap_get_fast(ces_load_base, memopQ);
	//firm/cparser/sparc uses byte-adressing
  //	size_t bit_per_word = get_mode_size_bits(get_modeP());
	const size_t bit_per_word = 8;
	ir_node* pack = get_irn_n(memopQ,n_Store_value);

	long pack_pos_from = current->c3_value*bit_per_word - accu->c3_value*bit_per_word;
	long pack_pos_used = 0;
	for(int i=0; i< get_irn_arity(pack);i++)
		pack_pos_used += get_mode_size_bits(get_irn_mode(get_irn_n(pack,i)));
	long diff = pack_pos_from - pack_pos_used;

	if (diff > 0) {
		add_irg_constraints(current_ir_graph,IR_GRAPH_CONSTRAINT_CONSTRUCTION);
		ir_mode* mode = new_int_mode("pack_padding", irma_twos_complement, diff, 0, diff);
		ir_tarval* tarval = new_tarval_from_long(diff, mode);
		add_irn_n(pack, new_Const(tarval));
		clear_irg_constraints(current_ir_graph,IR_GRAPH_CONSTRAINT_CONSTRUCTION);
	}
	if( diff < 0) {
		//because the first el is added at pack-creation
		//we see the same node here again
		//assert(get_Store_value(memop) == get_irn_n(pack,0));
		//or we try to overwrite
  } else {
	add_irn_n(pack,get_Store_value(memop));
	ces_replace_memop_slice(memop, memopQ, memopQ);
  }
}

void ces_add_to_mop(ir_node* memopQ, ir_node* memop) {
	switch (get_irn_opcode(memopQ)) {
	case iro_Load:
		ces_add_to_mop_load(memopQ, memop);
		break;
	case iro_Store:
		ces_add_to_mop_store(memopQ, memop);
		break;
	default:
		panic("unexpected node tpye\n");
	}
	/* update irg flags */
	clear_irg_properties(get_irn_irg(memopQ), IR_GRAPH_PROPERTY_CONSISTENT_OUTS);
}

/*
 * consumes all memops given
 * either memop fits into a qmemop or a separate qmemop is created
 * @param plist_loads is modified. Is empty after merging
 * @returm plist of qMemops to be freed by caller
 */
plist_t* ces_merge_into_qload(plist_t* plist_loads) {
	const size_t LSU_WIDTH = 128;
	plist_t* qload_list = plist_new();
	ir_node* memop = NULL;
	struct load_base* merged_base = NULL;
	struct load_base* current_base;

	//firm/cparser/sparc uses byte-adressing
  //	size_t bit_per_word = get_mode_size_bits(get_modeP());
	size_t bit_per_word = 8;

	ir_node* memop_merged;
	ir_node* mem_pred = NULL;
	// TODO: kick non fitting loads from list. ATM those are transformed to separate QLoad
	plist_element_t* el = plist_first(plist_loads);
	while (el) {
		memop = plist_element_get_value(el);
		current_base = ir_nodemap_get_fast(ces_load_base, memop);

		assert( (is_Load(memop) || is_Store(memop)) && current_base->base != 0); //just to be sure - this should not happen
		assert(current_base->size < LSU_WIDTH && current_base->size > 0);     //c cannot express data types > 128; neither can the rest of the tools

//		assert(current->size == amount_data); //if this holds amount_data can be removed
		ces_print_load_base(memop, current_base);

		if (!merged_base) {
			if (!mem_pred)
			  mem_pred = get_memop_mem(memop);
			merged_base = ces_mop_new(memop, current_base, mem_pred, &memop_merged);
			mem_pred = memop_merged;
		}
		// bug: es werden zu viele bits in ein qSTORE!! gepackt.

		ir_printf("%+F\n", memop);
		size_t new_size = (current_base->c3_value*bit_per_word + current_base->size) - merged_base->c3_value*bit_per_word;
		if( !((merged_base->c1_value==current_base->c1_value) && (merged_base->c2_value==current_base->c2_value) && (new_size <= LSU_WIDTH)) ) {
			//memop_merged full - create new
			DB((ces_dbg, LEVEL_5, "new qload\n"));
			ces_accu_finalize(qload_list, memop_merged);

			merged_base = NULL;
		} else {
			DB((ces_dbg, LEVEL_5, "merge: offset:%u, size:%u, accu start:%u, size:%u\n",current_base->c3_value,get_mode_size_bits(get_memop_mode(memop)), merged_base->c3_value, new_size));

			ces_add_to_mop(memop_merged,  memop);
			merged_base->size = new_size;

			set_irn_op(memop,op_Deleted);
			ces_plist_remove_and_advance(plist_loads, &el); //inside else, so we can restart without advancing if qop was full
		}
		//stuff here should go into previous else
	}
	ces_accu_finalize(qload_list, memop_merged);

	assert( (plist_count(plist_loads) == 0) && "not all memops transformed. investigate!");
	return qload_list;
}
