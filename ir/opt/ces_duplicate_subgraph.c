#include "ces_si_tools.h"
#include "firm.h"
#include "irtools.h"

/*
 * creates new irg.
 * does not make irg global known, reuses type from current_ir_Graph
 */
static ir_graph* new_tmp_irg(void){
	ident *factor_id = new_id_fmt("%s_ldstana", get_entity_ident(get_irg_entity(current_ir_graph)));
	ir_type* orig_type = get_entity_type(get_irg_entity(current_ir_graph));

	ir_type* tmp_type = new_type_method(get_method_n_params(orig_type), 1,
	                                    false, cc_cdecl_set, mtp_no_property);
	set_method_res_type(tmp_type,0,get_type_for_mode(get_modeIu()));
	for(unsigned i=0; i<get_method_n_params(orig_type); i++)
		set_method_param_type(tmp_type,i, get_method_param_type(orig_type, i));

	ir_entity *tmp_ent= new_entity(get_glob_type(), factor_id,  tmp_type );
	ir_graph* tmp_irg = new_ir_graph(tmp_ent, get_irg_n_locs(current_ir_graph));
	return tmp_irg;
}

static void duplicate_walker(ir_node* node, void* env) {
	ir_graph* tmp_irg = (ir_graph*)env;
	ir_graph* old_irg = current_ir_graph;
	ir_node* copy;

	switch( get_irn_opcode(node) ) {
	case iro_Block:
		if( node != get_irg_end_block(old_irg))
			if( node != get_irg_start_block(old_irg)) {
				set_irn_link(node, get_r_cur_block(tmp_irg));
				break;
			}
		//no break here, fallthrough: copy regular BBs
	default:
		copy = irn_copy_into_irg(node, tmp_irg);

		current_ir_graph = tmp_irg;
		if (!is_Block(node) ) {
			set_nodes_block(copy,get_cur_block());
		}
		set_irn_link(copy, node);
		set_irn_link(node, copy);

		current_ir_graph = old_irg;
		break;
	}
}

static void rewire_walker(ir_node *irn, void *env)
{
	irn_rewire_inputs(irn);
	//because we copy init_exec etc. to a new block this block is not designated startBlock yet
	if(is_Start(irn))
		set_irg_start_block( (ir_graph*)env, get_nodes_block(get_irn_link(irn)) );
}

/*copy everything upwards from node into a new IRG, shares the entity with the original */
ir_graph* ces_dup_subgraph(ir_node* node){
	ir_graph* save_current_irg = current_ir_graph;

	ir_graph* tmp_irg = new_tmp_irg();
	irg_walk(node, duplicate_walker, rewire_walker, tmp_irg);

	//connect the  end block to a return to the memop
	set_current_ir_graph(tmp_irg);
	ir_node* dummy[] = {new_Const_long(get_modeIu(),0)};
	ir_node* proj = new_Proj(get_irn_link(node),get_modeM(),pn_Load_M);
	ir_node* ret = new_Return(proj, 1, dummy);
	add_immBlock_pred(get_irg_end_block(tmp_irg),ret);

	//for testing. can be removed
	add_End_keepalive(get_irg_end(tmp_irg),ret);

	irg_finalize_cons(tmp_irg);
	//irg_assert_verify(tmp_irg);
	DBG_DO(ces_dbg, LEVEL_5, ces_dump_irg(get_irn_link(node), "sub_ir"));

	current_ir_graph = save_current_irg;
	return tmp_irg;
}
