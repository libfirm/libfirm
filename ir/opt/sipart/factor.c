
#include <assert.h>
#include <firm.h>
#include <stdint.h>

#include "irnode_t.h"
#include "sitools.h"
#include "factor.h"
#include <debug.h>


DEBUG_ONLY(firm_dbg_module_t *ces_dbg;)
#define DB1(...) 	DB((ces_dbg,LEVEL_1,__VA_ARGS__))
#define DB2(...) 	DB((ces_dbg,LEVEL_2,__VA_ARGS__))
#define DB3(...) 	DB((ces_dbg,LEVEL_3,__VA_ARGS__))

#define panic(x) do { puts(x); exit(-1);  } while(0)

/* Factoring out BBs from IRGs */

struct biggest_block_env {
  ir_node *current;
  unsigned count;
  unsigned topcount;
  ir_node *biggest;
};

/* Find the block containing the most data mode nodes in an IRG */
irg_walk_func choose_biggest_block;
static void find_biggest_block_walker(ir_node *node, void *data) {
	struct biggest_block_env *env = data;

	if (is_Block(node))
		return;
	if (! mode_is_data(get_irn_mode(node)))
		return;

	{
		ir_node *bb = get_nodes_block(node);

		if (bb != env->current) {
			env->current = bb;
			env->count = 0;
		}
		env->count ++;
		if (env->count > env->topcount) {
			env->topcount = env->count;
			env->biggest = bb;
		}
	}
}

ir_node *find_biggest_block(ir_graph *irg) {

  struct biggest_block_env env = {
    .current = 0,
    .count = 0,
    .topcount = 0,
    .biggest = 0,
  };

  irg_walk_blkwise_graph(irg, find_biggest_block_walker, NULL, &env);

  return env.biggest;
}



/* find inputs/outputs of the BB */
static void locate_inputs_outputs(ir_node *node, void *data) {
	struct factor_env *env = data;
/* 	if (is_Block(node)) return; */

	bool node_result = env->predicate(node, env->clientdata);
	for (int i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);
		if (is_Block(pred)) continue;
		if (node_result == env->predicate(pred, env->clientdata)) continue;
		if (is_Const(pred)) continue;
		if (is_Address(pred)) continue;
		if (is_Offset(pred)) continue;
		if (is_Align(pred)) continue;
		if (mode_M == get_irn_mode(pred)) {
			if (node_result)
				env->mem_in = pred;
			else
				env->mem_out = pred;
			continue;
		}
		if (!mode_is_data(get_irn_mode(pred))) continue;
		ir_nodeset_insert(node_result ? env->inputs : env->outputs, pred);
	}
}

void dupe_nodes_walker(ir_node *node, void *data) {
	struct factor_env *env = data;
	ir_graph *new_irg = env->new_irg;
	assert(get_irn_irg(node) != new_irg);
	if (is_Block(node)) return;
	if (is_End(node)) return;

	if (!is_Const(node)
		&& !env->predicate(node, env->clientdata))
		return;

	current_ir_graph = new_irg;

	{
		ir_node *const node_block = get_nodes_block(node);
		if (get_irg_start_block(env->old_irg) != node_block) {
			if (!env->block)
				env->block = node_block;
			else {
				assert((node_block == env->block) && "set restricted to single Block");
			}
		}
	}

	ir_node *irn_copy_into_irg(const ir_node *node, ir_graph *irg);
	ir_node *dupe = irn_copy_into_irg(node, new_irg);
	set_nodes_block(dupe, get_cur_block());
	copy_node_attr(new_irg, node, dupe);

	set_irn_link(node, dupe);
	for (int i = get_irn_arity(node) - 1; i >= 0; --i) {
		ir_node *pred = get_irn_n(node, i);
		assert(pred);
		ir_mode *pred_mode = get_irn_mode(pred);
		if (is_Block(pred)) continue;
		if (ir_nodeset_contains(env->inputs, pred)) {
			if (mode_is_data(get_irn_mode(pred))|| mode_is_reference(get_irn_mode(pred))) {
				ir_node *proj=NULL;
				if ( (get_irn_link(pred) != NULL) && get_irn_link(pred) > 0x1000 ) {
					//link field contains ir_node
					proj = (ir_node*)get_irn_link(pred);
				} else {
					//link field contains position in proj
					int pos = (uintptr_t)get_irn_link(pred);
					ir_node *args = new_Proj(new_Start(), mode_T, pn_Start_T_args);
					proj = new_Proj(args, pred_mode, pos);
				}
				set_irn_n(dupe, i, proj);
			} else if (pred_mode == mode_M) {
			    ir_node *proj = new_Proj(new_Start(), mode_M, pn_Start_M);
			    set_irn_n(dupe, i, proj);
			  } else {
			  panic("cannot turn edge into an argument");
			}

		} else {
			//heuristic to see if it is  a pointer
		  if (get_irn_link(pred) && get_irn_link(pred)>0x1000 && is_ir_node(get_irn_link(pred))) {
		    /* A copy of the predecessor exists. */
		    set_irn_n(dupe, i, get_irn_link(pred));
		  } else {
		    /* Pred is neither part of the IRG nor an explicit argument. */
		    if (pred_mode == mode_M) {
		      ir_node *proj = new_Proj(new_Start(), mode_M, pn_Start_M);
		      set_irn_n(dupe, i, proj);
		    }

		    if (is_irn_start_block_placed(dupe)) {
				ir_node *proj = irn_copy_into_irg(pred, new_irg);
				copy_node_attr(new_irg, pred, proj);
				set_nodes_block(proj, get_nodes_block(new_Start()));
				set_irn_n(dupe, i, proj);
		    }
		  }
		}
	}

	if (is_irn_start_block_placed(dupe)) {
	  set_nodes_block(dupe, get_irg_start_block(new_irg));
	}

	current_ir_graph = env->old_irg;
}

static void wire_outputs_walker(ir_node *node, void *data) {
	struct factor_env *env = data;

	if (is_Block(node))
		return;

	{
		for (int i = get_irn_arity(node) - 1; i >= 0; --i) {
			ir_node *pred = get_irn_n(node, i);
			ir_mode *pred_mode = get_irn_mode(pred);
			int j = 0;
			foreach_ir_nodeset(env->outputs, output, iter) {
				if (pred == output) {
					if (mode_is_data(get_irn_mode(output))) {
						ir_node *result = new_r_Proj(env->call, mode_T, pn_Call_T_result);
						ir_node *proj = new_r_Proj(result, pred_mode, j);
						set_irn_n(node, i, proj);
					} else if (get_irn_mode(output) == mode_M) {
						ir_node *proj = new_r_Proj(env->call, mode_T, pn_Call_M);
						set_irn_n(node, i, proj);
					}
				}
				j++;
			}
		}
	}
}


ir_graph *factor_subset(ir_graph *irg, subset_predicate *predicate, void *predicate_data) {

	FIRM_DBG_REGISTER(ces_dbg, "firm.si.factor");

	edges_deactivate(irg);
	ir_reserve_resources(current_ir_graph, IR_RESOURCE_IRN_LINK);
	/*   edges_deactivate(get_irn_irg(source_block)); */
	struct factor_env env = {
			.predicate = predicate,
			.clientdata = predicate_data,
			.inputs = ir_nodeset_new(10),
			.outputs = ir_nodeset_new(10),
			.keepalives = ir_nodeset_new(10),
			.old_irg = irg,
			.mem_in = 0,
			.mem_out = 0,
			.block = 0,
	};

	irg_walk_graph(irg, locate_inputs_outputs, 0, &env);


	factor_subset_core(&env, "_factor");

	return env.new_irg;
}


void factor_subset_core(struct factor_env* env, const char* name) {

	/* Prevent keep-alive edges from becoming real outputs. */
	ir_node *end = get_irg_end(env->old_irg);
	for(int n = 0; n < get_End_n_keepalives(end); n++) {
		ir_node *keepalive = get_End_keepalive(end, n);
		ir_nodeset_remove(env->outputs, keepalive);
		ir_nodeset_insert(env->keepalives, keepalive);
	}


	foreach_ir_nodeset(env->inputs, input, iter) {
		DB1("input: %+F \n", input);
	}
	foreach_ir_nodeset(env->outputs, output, iter) {
		DB1("output: %+F \n", output);
	}
	foreach_ir_nodeset(env->keepalives, keepalive, iter) {
		DB2("keepalive: %+F \n", keepalive);
	}

	/* create the new method */

	ir_type *mtype;
	{
		/* create entity */
		mtype = new_type_method(
				ir_nodeset_size(env->inputs),
				ir_nodeset_size(env->outputs));
		{
			int i = 0;
			foreach_ir_nodeset(env->inputs, input, iter) {
				set_irn_link(input, (void *)(uintptr_t)i);
				set_method_param_type(mtype, i++, new_type_primitive(get_irn_mode(input)));
			}
		}
		{
			int i = 0;
			foreach_ir_nodeset(env->outputs, output, iter) {
				set_irn_link(output, (void *)(uintptr_t)i);
				set_method_res_type(mtype, i++, new_type_primitive(get_irn_mode(output)));
			}
		}
		ident *factor_id = id_mangle3("",
				get_entity_ident(get_irg_entity(env->old_irg)),
				name);
		ir_entity *ment = new_entity(get_glob_type(), factor_id, mtype);

		env->new_irg = new_ir_graph(ment, ir_nodeset_size(env->inputs));
		set_current_ir_graph(env->new_irg);

		irg_walk_blkwise_graph(env->old_irg, 0, dupe_nodes_walker, env);

		{
			size_t sz = ir_nodeset_size(env->outputs);
			ir_node **res = ALLOCAN(ir_node*, sz+2);

			{
				int i=0;
				foreach_ir_nodeset(env->outputs, output, iter) {
					res[i++] = get_irn_link(output);
				}
			}
			ir_node   *return_node = new_Return(env->mem_out ? get_irn_link(env->mem_out) : get_store(), sz, res);

			ir_node *end_block   = get_irg_end_block(current_ir_graph);
			add_immBlock_pred(end_block, return_node);

			mature_immBlock(get_cur_block());
			set_cur_block(NULL);

		}
		irg_finalize_cons(env->old_irg);
		irg_assert_verify(env->old_irg);

		/* Replace BB contents in original graph with a Call. */
		set_current_ir_graph(env->old_irg);
		add_irg_constraints(env->old_irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION);
		set_current_ir_graph(env->old_irg);
		assert(env->block && "need block to place call in");
		set_cur_block(env->block);
		size_t sz = ir_nodeset_size(env->inputs);
		ir_node **args = ALLOCAN(ir_node*, sz+2);

		{
			int i = 0;
			foreach_ir_nodeset(env->inputs, input, iter) {
				args[i++] = input;
			}
		}

		env->call = new_Call(env->mem_in ? env->mem_in : get_irg_no_mem(env->old_irg),
				new_r_Address(env->old_irg, ment),
				sz,
				args,
				mtype);
		irg_walk_graph(env->old_irg, wire_outputs_walker, 0, env);
		set_entity_link(ment, env->call);
	}
	set_current_ir_graph(env->old_irg);

	{
		ir_node *end = get_irg_end(env->old_irg);
		foreach_ir_nodeset(env->keepalives, keepalive, iter) {
			remove_End_keepalive(end, keepalive);
		}
	}

	irg_finalize_cons(env->new_irg);
	irg_assert_verify(env->new_irg);
	irg_finalize_cons(env->old_irg);
	irp_finalize_cons(); /* huh, why isn't the prior line sufficient? */
	ir_nodeset_destroy(env->inputs);
	ir_nodeset_destroy(env->outputs);
	ir_free_resources(current_ir_graph, IR_RESOURCE_IRN_LINK);
}

static subset_predicate factor_bb_p;
static bool factor_bb_p(ir_node *node, void *data)
{
	ir_node *block = data;
	if (is_Block(node)) return false;
	return get_nodes_block(node) == block;
}

ir_graph *factor_bb(ir_graph *irg, ir_node *source_block)
{
	assert(is_Block(source_block));
	irg = factor_subset(irg, factor_bb_p, (void *)source_block);
	ir_entity *entity = get_irg_entity(irg);

	set_entity_ident(entity,
					 id_mangle3("",
								get_entity_ident(get_irg_entity(irg)),
								"_biggest_block"));
	return irg;
}

static subset_predicate factor_nodeset_p;
static bool factor_nodeset_p(ir_node *node, void *data)
{
	ir_nodeset_t *set = data;
	return ir_nodeset_contains(set, node);
}

ir_graph *factor_nodeset(ir_graph *irg, ir_nodeset_t *set)
{
	return factor_subset(irg, factor_nodeset_p, (void *)set);
}

