/*
 * controls ces-si phase
 */
#include "ces_special_instruction.h"
#include "ces_si_tools.h"
#include "ces_convex_graph.h"
#include "ces_xiao_casseau_enumerator.h"
#include "ces_ldst_enumerator.h"
#include "ces_time_measure.h"
#include "sipart/sipart.h"

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
#include "iropt_t.h"
#include "irmode.h"
#include "irtools.h"

#include "bearch.h"
#include "bemodule.h"

DEBUG_ONLY(firm_dbg_module_t *ces_dbg = NULL;);

extern ir_nodemap* ces_nodemark;
void* slice_dump_orig_func;
ces_si_params_t ces_si_params = {
	.example = 42,
	.phases = 0,
};

ir_graph* ces_cut_coreISA(ir_graph* si_irg, struct stream_description* streams);
int ces_allow_ifconf(ir_node const *sel, ir_node const *mux_false, ir_node const *mux_true);
static int ces_is_mux_allowed(ir_node* mux);
static int has_si_attribute(ir_graph* irg);

int ces_allow_ifconf(ir_node const *sel, ir_node const *mux_false,ir_node const *mux_true) {
	UNUSED_PARAM(sel);
	UNUSED_PARAM(mux_false);
	UNUSED_PARAM(mux_true);
	return true;
}

__attribute__((unused))
static int ces_is_mux_allowed(ir_node* mux) {
//	return	sparc_is_mux_allowed(get_Mux_sel(mux), get_Mux_false(mux), get_Mux_true(mux) );
	return !ir_is_optimizable_mux(get_Mux_sel(mux), get_Mux_false(mux), get_Mux_true(mux));
}

static void node_dump_Slice(FILE *out, const ir_node *self, dump_reason_t reason) {
	ir_node *pred;
	size_t slice_from, slice_to;
	ir_mode* mode;

	switch(reason) {
	case dump_node_opcode_txt:
		fprintf(out, "%s[%li..%li]", get_irn_opname(self), get_Slice_from(self), get_Slice_to(self) );
		break;

	case dump_node_mode_txt:
		mode = get_irn_mode(self);

		if (mode != NULL && mode != mode_BB && mode != mode_ANY && mode != mode_BAD && mode != mode_T )
			fprintf(out, "%s", get_mode_name(mode));
		break;

	case dump_node_nodeattr_txt:
		//keep empty - nodenr inserted automagically
		break;

	case dump_node_info_txt:
		pred    = get_Slice_pred(self);
		slice_from = get_Slice_from(self);
		slice_to   = get_Slice_to(self);
		fprintf(out, "\t%s[%li..%li]\n", get_irn_opname(self), slice_from, slice_to);
		ir_fprintf(out, "Pred: %+F\n", pred);
		break;

	default:
		DBG((ces_dbg, LEVEL_3, "unknown dump_node_kind\n"));
	}
}


static void ces_setup_special_nodetypes(void) {
	set_op_dump(op_Slice, node_dump_Slice);
}

static int has_si_attribute(ir_graph* irg) {
	return (mtp_special_instruction & get_entity_additional_properties(get_irg_entity(irg))) ? 1 : 0;
}


#define CES_DBG_PRINT_CURR_IRG(x) \
DBG((ces_dbg, LEVEL_1, x)); \
	if (get_irn_arity(get_irg_end(current_ir_graph)) >0) \
		ces_print_irg_max(get_irg_end(current_ir_graph),0,-1); \
	else \
		ces_print_irg_max(get_irn_n(get_irg_end_block(current_ir_graph),0),0,-1);

/*
 * manages ces-si phase
 *
 */
void ces_special_instruction(ir_graph *irg) {
/*
	if (!has_si_attribute(irg))
		return;
*/
	DBG((ces_dbg,LEVEL_1, "warning: paper mode engaged\n"));

	DBG((ces_dbg,LEVEL_3, "found si attribute in irg:%s\n",get_irg_dump_name(irg)));


	DBG((ces_dbg, LEVEL_3, "ces_special_instruction start (%s)\n", get_irg_dump_name(irg) ));

	ces_obstack = XMALLOCZ(struct obstack);
	obstack_init(ces_obstack);

	ces_nodemark = XMALLOC(ir_nodemap);// obstack_alloc(ces_obstack, sizeof(ir_nodemap));
	ir_nodemap_init(ces_nodemark, irg);

	ces_setup_special_nodetypes();

	//this eventually goes somewhere else after si-interface is stable
	ces_node_stats_t stats;

	char filename[256];
	snprintf(filename, 256,"ces-node-stats-%s",get_irg_dump_name(irg));
	stat_ev_begin(filename, NULL);
	stat_ev_ctx_push_str("ces_si",get_irg_dump_name(irg));
	ces_collect_node_stats(&stats, irg);

	DBG_DO(ces_dbg, LEVEL_3, dump_ir_graph(irg,"before_ces_si"));
	DBG_DO(ces_dbg, LEVEL_3, ir_export("before_ces_si.irg"));

	DBG((ces_dbg, LEVEL_3, "calling if-conversion\n")); // unconditionally transforms if to mux-nodes
	opt_if_conv_cb(irg, &ces_allow_ifconf);
	DBG_DO(ces_dbg, LEVEL_3, dump_ir_graph(irg, "ces_after_ifconv"));
	DBG_DO(ces_dbg, LEVEL_3, ir_export("ces_after_ifconv.irg"));

	assert(irg == current_ir_graph);
	ir_graph* old_irg = irg;
	//theory: delete old_irg at function end or reset current_ir_graph
	struct stream_description* streams;
/*
	CES_DBG_PRINT_CURR_IRG("si_irg before ldst:\n");
	//identify memory streaming ci
	if (ces_si_params.phases & CES_PHASE_LDST){
		streams = ces_identify_streams(current_ir_graph);
		CES_DBG_PRINT_CURR_IRG("si_irg after ldst:\n");

		//TODO: restructure into 3 steps
		//1.)identify CI, convexity 2.)extract coreISA 3.)merge LD/ST
		ir_graph* coreISA_irg = ces_cut_coreISA(current_ir_graph, streams);
	}

	//enumerate feasible pattern
	if (ces_si_params.phases & CES_PHASE_XIAO) {
		xiao_casseau_enumerator_bitsets(irg, pattern_dump_cb, NULL);
	}
*/
	//call si-partitioning
	if (ces_si_params.phases & CES_PHASE_SIPART) {
		sipart_opt(current_ir_graph, streams);
	}

	//see if we left the irg in a sane state
	irg_verify(irg);

	ir_nodemap_destroy(ces_nodemark);
	free(ces_nodemark);

	DBG_DO(ces_dbg, LEVEL_3, dump_ir_graph(irg,"ces_after_si"));

	stat_ev_end();

	// call firm2vhdl on si graph
	//check irdo for howto
	//delete si graph

	obstack_finish(ces_obstack);
	free(ces_obstack);
}

/*
 * if no debug mask is given it defaults to LEVEL_3 (4)
 * more output is available with LEVEL_4(8) and LEVEL_5(16)
 */
void firm_init_ces_special_instruction(){
  FIRM_DBG_REGISTER(ces_dbg, "firm.opt.si");
	if (firm_dbg_get_mask(ces_dbg) == 0) {
		DB((ces_dbg, LEVEL_DEFAULT, "For module firm.opt.si no debug mask given, setting to L3(4)\n"));
		firm_dbg_set_mask(ces_dbg, SET_LEVEL_3);
	}
	lc_opt_entry_t *opt_grp   = lc_opt_get_grp(firm_opt_get_root(), "opt");
	lc_opt_entry_t *si_grp = lc_opt_get_grp(opt_grp, "si");
	lc_opt_add_table(si_grp, si_options);
	sipart_register_opt();
}

void firm_finish_ces_special_instruction() {

}
