#include "loop_pagecache.h"
#include "irloop_t.h"
#include "debug.h"
#include "dbginfo.h"
#include "nodes.h"
#include "entity_t.h"
#include <xmalloc.h>
#include "firm_types.h"
#include "irmode.h"
#include "irgopt.h"
#include "iredges_t.h"
#include "irdump.h"

DEBUG_ONLY(static firm_dbg_module_t *dbg = NULL;)

static bool is_in_loop(ir_loop *loop, ir_node *node);
static ir_node *find_phi(ir_node *node, ir_loop *outer);
static ir_node *find_confirm(ir_node *node, ir_loop *loop);
static bool move_to_block_recursive(ir_node *node, ir_node *target, ir_loop *loop);
static void loop_pagecache_dfs(list_head *list, ir_node *node, ir_loop *loop);
static ir_node *clone_to_block_with_replace(ir_node *node, ir_node *target, loop_var_t *vars, bool init_limit, ir_loop *loop);
static void loop_pagecache_outer(ir_loop *loop);

static int block_has_phi_loop(ir_node *node, ir_node** phi_out) {
	if (!is_Block(node)) {
		return false;
	}
	for (ir_node *phi = get_Block_phis(node); phi != NULL; phi = get_Phi_next(phi)) {
		if (get_Phi_loop(phi)) {
			*phi_out = phi;
			return true;
		}
	}
	return false;
}

static ir_node * make_call(ir_node *block, ir_node *mem, ir_node **in, int in_nr, ir_entity *func, ir_type *func_type) {

	ir_node *addr = new_r_Address(get_irn_irg(block), func);
	ir_node *call = new_r_Call(block, mem, addr, in_nr, in, func_type);
	ir_node *proj = new_r_Proj(call, mode_M, pn_Call_M);
	return proj;
}

/**
 * Creates a new entity representing the equivalent of
 * static <element_mode> <name>[<length>];
 */
static ir_entity *new_array_entity(ident *const name, ir_mode *const element_mode, unsigned const length, ir_linkage const linkage)
{
	ir_type *const element_type = get_type_for_mode(element_mode);
	ir_type *const array_type   = new_type_array(element_type, length);
	ident   *const id           = new_id_from_str(name);
	ir_type *const owner        = get_glob_type();
	return new_global_entity(owner, id, array_type, ir_visibility_private, linkage);
}

/**
 * Creates a new entity representing the equivalent of
 * static const char name[strlen(string)+1] = string
 */
static ir_entity *new_static_string_entity(char const *const string)
{
	static int str_counter = 0;

	char name[100];
	snprintf(name, 100, "__funcname_%d", str_counter++);
	name[99] = '\0';

	/* Create the type for a fixed-length string */
	ir_mode   *const mode   = mode_Bs;
	size_t     const length = strlen(string) + 1;
	ir_entity *const result = new_array_entity(name, mode, length, IR_LINKAGE_CONSTANT);

	/* There seems to be no simpler way to do this. Or at least, cparser
	 * does exactly the same thing... */
	ir_initializer_t *const contents = create_initializer_compound(length);
	for (size_t i = 0; i < length; i++) {
		ir_tarval        *const c    = new_tarval_from_long(string[i], mode);
		ir_initializer_t *const init = create_initializer_tarval(c);
		set_initializer_compound_value(contents, i, init);
	}
	set_entity_initializer(result, contents);

	return result;
}

static ir_node *make_prefetch(ir_node *block, ir_node *mem, ir_node *init, ir_node *limit, ir_node *size, ir_node *iter) {
	DB((dbg, LEVEL_2, "Inserting prefetch in %+F\n", get_irn_irg(mem)));

	static ir_type *prefetch_type;
	if (prefetch_type == NULL) {
		prefetch_type = new_type_method(5, 1, 0, 0, mtp_no_property);
		set_type_dbg_info(prefetch_type, NULL);
		set_method_param_type(prefetch_type, 0, get_type_for_mode(mode_P));
		set_method_param_type(prefetch_type, 1, get_type_for_mode(mode_P));
		set_method_param_type(prefetch_type, 2, get_type_for_mode(get_modeIu()));
		set_method_param_type(prefetch_type, 3, get_type_for_mode(get_modeLs()));
		set_method_param_type(prefetch_type, 4, get_type_for_mode(mode_P));
		set_method_res_type(prefetch_type, 0, get_type_for_mode(get_modeIs()));
	}
	static ir_entity *prefetch;
	if (prefetch == NULL) {
		prefetch = new_global_entity(get_segment_type(IR_SEGMENT_GLOBAL), get_id_str("vsm_prefetch"), prefetch_type, ir_visibility_external, IR_LINKAGE_DEFAULT);
	}

	ir_entity *enclosing_function = get_irg_entity(get_irn_irg(mem));

	ir_entity *func_name = new_static_string_entity(get_entity_name(enclosing_function));

	ir_node **in = XMALLOCN(ir_node*, 5);
	in[0] = init;
	in[1] = limit;
	in[2] = size;
	in[3] = iter;
	in[4] = new_r_Address(get_irn_irg(mem), func_name);

	return make_call(block, mem, in, 5, prefetch, prefetch_type);
}

static ir_node *make_free(ir_node *block, ir_node *mem, ir_node *req) {
	static ir_type *free_type;
	if (free_type == NULL) {
		free_type = new_type_method(1, 0, 0, 0, mtp_no_property);
		set_type_dbg_info(free_type, NULL);
        set_method_param_type(free_type, 0, get_type_for_mode(get_modeIs()));
	}
	static ir_entity *free;
	if (free == NULL) {
		free = new_global_entity(get_segment_type(IR_SEGMENT_GLOBAL), get_id_str("vsm_free"), free_type, ir_visibility_external, IR_LINKAGE_DEFAULT);
	}
	ir_node **in = XMALLOCN(ir_node*, 1);
	in[0] = req;
	return make_call(block, mem, in, 1, free, free_type);
}

static ir_node *make_iterations(ir_node *block, loop_var_t *var, ir_node *mem, ir_loop *outer, loop_var_t *vars) {
	ir_node *one = new_r_Const(get_irn_irg(block), new_tarval_from_long(1, mode_Ls));
	inc_irg_visited(get_irn_irg(block));
	ir_node *limit = clone_to_block_with_replace(var->limit, block, vars, true, outer);
	inc_irg_visited(get_irn_irg(block));
	ir_node *init = clone_to_block_with_replace(var->init, block, vars, false, outer);
	if (limit == NULL || init == NULL) {
		return NULL;
	}
	ir_node *sub = new_r_Sub(block, limit, init);
	ir_node *step = new_r_Conv(block, var->step, mode_Ls);
	if (get_Const_long(step) == 0) step = one;
	ir_node *div = new_r_Div(block, mem, new_r_Conv(block, sub, mode_Ls), step, 0);
	return div;
}

static void loop_find_header(ir_loop *loop, ir_node **phi_loop, ir_node **header) {

	// Find block with phi[loop]
	loop_element element;
	for (size_t i = 0; i < get_loop_n_elements(loop); i++) {
		element = get_loop_element(loop, i);
		if (*element.kind == k_ir_node) {
			*header = element.node;
			if (block_has_phi_loop(*header, phi_loop)) {
				break;
			}
		}
	}
}

static void analyze_dummy(ir_node *node, size_t *num_ext_pred, size_t *num_int_pred, int *ext_pred, int *int_pred) {
	*num_ext_pred = 0;
	*num_int_pred = 0;
	ir_loop* loop = get_irn_loop(get_block(node));
	ir_node *pred;
	for (int in_pos = 0; in_pos < get_irn_arity(node); in_pos++) {
		pred = get_irn_n(node, in_pos);
		if (is_in_loop(loop, pred)) {
			int_pred[1 + (*num_int_pred)++] = in_pos;
		} else {
			ext_pred[(*num_ext_pred)++] = in_pos;
		}
	}
}

static bool is_in_loop(ir_loop *loop, ir_node *node) {
	ir_node *t = NULL;
	if (is_Block(node)) t = node;
	if (!is_Block(node)) t = get_block(node);
	ir_loop *outer = get_irn_loop(t);
	if (outer == NULL) return false;
	ir_loop *l;
	do {
		l = outer;
		if (loop == l) return true;
		outer = get_loop_outer_loop(l);
	} while(l != outer);
	return false;
}

static void loop_insert_dummy(ir_node *header, ir_node **dummy) {
	size_t num_int_pred, num_ext_pred;
	int int_pred[get_irn_arity(header)], ext_pred[get_irn_arity(header)];
	analyze_dummy(header, &num_ext_pred, &num_int_pred, ext_pred, int_pred);
	ir_node *ext_pred_irn[num_ext_pred];
	for (size_t i = 0; i < num_ext_pred; i++) {
		ext_pred_irn[i] = get_irn_n(header, ext_pred[i]);
	}
	ir_node *int_pred_irn[num_int_pred + 1];
	for (size_t i = 1; i < num_int_pred + 1; i++) {
		int_pred_irn[i] = get_irn_n(header, int_pred[i]);
	}
	ir_node *dummy_block = new_r_Block(get_irn_irg(header), num_ext_pred, ext_pred_irn);
	set_irn_loop(dummy_block, get_loop_outer_loop(get_irn_loop(header)));
	ir_node *dummy_jmp = new_r_Jmp(dummy_block);
	int_pred_irn[0] = dummy_jmp;
	set_irn_in(header, (int)num_int_pred + 1, int_pred_irn);

	foreach_out_edge_safe(header, edge) {
		ir_node *node = get_edge_src_irn(edge);
		if (!is_Phi(node)) continue;
		ir_node *ext_pred_phi[num_ext_pred];
		for (size_t i = 0; i < num_ext_pred; i++) {
			ext_pred_phi[i] = get_irn_n(node, ext_pred[i]);
		}
		ir_node *int_pred_phi[num_int_pred + 1];
		for (size_t i = 1; i < num_int_pred + 1; i++) {
			int_pred_phi[i] = get_irn_n(node, int_pred[i]);
		}
		ir_node *dummy_phi = new_r_Phi(dummy_block, num_ext_pred, ext_pred_phi, get_irn_mode(node));
		int_pred_phi[0] = dummy_phi;
		set_irn_in(node, (int)num_int_pred + 1, int_pred_phi);
	}

	*dummy = dummy_block;
	ir_free_resources(get_irn_irg(header), IR_RESOURCE_IRN_VISITED);
	local_optimize_graph(get_irn_irg(header));
	edges_deactivate(get_irn_irg(header));
	edges_activate(get_irn_irg(header));
	ir_reserve_resources(get_irn_irg(header), IR_RESOURCE_IRN_LINK);
	collect_phiprojs_and_start_block_nodes(get_irn_irg(header));
	ir_reserve_resources(get_irn_irg(header), IR_RESOURCE_IRN_VISITED);
	ir_free_resources(get_irn_irg(header), IR_RESOURCE_IRN_LINK);
}


static void build_dijkstra(ir_node *start, dijkstra_node_t *last, ir_node *target, dijkstra_graph_t *graph, bool init, ir_tarval *accum) {
	if (init) last = dijkstra_add_node(graph, start);
	if (!init && irn_visited_else_mark(start)) return;
	if (!is_in_loop(get_irn_loop(get_block(target)), start)) return;
	if (!init && is_Phi(start)) {
		dijkstra_node_t *phi;
		phi = dijkstra_add_node(graph, start);
		dijkstra_add_edge(graph, last, phi, accum);
		accum = new_tarval_from_long(0, graph->mode);
		last = phi;
		if (start == target) return;

	} else if (is_Add(start)) {
		ir_node *right = get_Add_right(start);
		if (is_Const(right)) {
			return build_dijkstra(get_Add_left(start), last, target, graph, false, tarval_add(accum, get_Const_tarval(right)));
		}
	} else if (is_Sub(start)) {
		ir_node *right = get_Sub_right(start);
		if (is_Const(right)) {
			return build_dijkstra(get_Sub_left(start), last, target, graph, false, tarval_sub(accum, get_Const_tarval(right)));
		}
	} else if (is_Conv(start)) {
        return build_dijkstra(get_Conv_op(start), last, target, graph, false, tarval_convert_to(accum, get_irn_mode(get_Conv_op(start))));
	}
	foreach_irn_in(start, idx, pred) {
		build_dijkstra(pred, last, target, graph, false, accum);
	}
}

static void find_step(loop_var_t *loop_var) {
	dijkstra_graph_t *graph = new_dijkstra(get_irn_mode(loop_var->phi));
	inc_irg_visited(get_irn_irg(loop_var->phi));
	// build_dijkstra(loop_var->phi, NULL, loop_var->phi, graph, true, new_tarval_from_long(0, graph->mode));
	loop_var->step = new_r_Const(get_irn_irg(loop_var->phi), new_tarval_from_long(1, mode_Ls));
	free_dijkstra(graph);
}

static void mem_ops_unique(list_head *list) {
    //mem_op_t *a, *b, *m, *n;
    list_for_each_entry_safe(mem_op_t, a, m, list, list) {
        if (a->duplicate) continue;
        list_for_each_entry_safe(mem_op_t, b, n, list, list) {
            if (a == b) continue;
            if (a->address == b->address && a->mode == b->mode) {
                b->duplicate = true;
                a->count++;
            }
        }
    }
}

static bool find_variable(ir_node *phi, ir_node *header, ir_node *phi_loop, ir_loop *loop, loop_var_t *loop_var) {
	if (is_Deleted_(phi)) return false;
    if (get_Phi_loop(phi)) return false;

    loop_var->phi = phi;
    loop_var->loop = loop;
    loop_var->init = get_irn_n(loop_var->phi, 0); // Dummy is always at pos 0
    //if (find_phi(loop_var->init, outer) != NULL) {
    //    printf("Init has Phi\n");
    //    return false;
    //}
    inc_irg_visited(get_irn_irg(header));
    loop_var->confirm = find_confirm(loop_var->phi, loop);
    if (loop_var->confirm == NULL) {
	    DB((dbg, LEVEL_2, "No confirm\n"));
        return false;
    }
    loop_var->limit = get_Confirm_bound(loop_var->confirm);
    //if (find_phi(loop_var->limit, outer) != NULL) {
    //    printf("Limit has Phi\n");
    //    return false;
    //}
    find_step(loop_var);
    loop_var->phi_loop = phi_loop;
    return true;
}

static int loop_pagecache_variables(ir_loop *loop, ir_loop *outer, loop_var_t **vars) {
	ir_node *header, *phi_loop, *dummy;
	loop_find_header(loop, &phi_loop, &header);
	loop_insert_dummy(header, &dummy);

	int num_variables = 0;
	loop_var_t loop_var;
	ir_node *phi;
	for (phi = get_Block_phis(header); phi != NULL; phi = get_Phi_next(phi)) {
		if (find_variable(phi, header, phi_loop, loop, &loop_var)) {
			num_variables++;
			ARR_APP1(loop_var_t, *vars, loop_var);

			DB((dbg, LEVEL_2, "Found loop variable from "));
			if (is_Const(loop_var.init)) {
				DB((dbg, LEVEL_2, "%ld", get_Const_long(loop_var.init)));
			} else {
				DB((dbg, LEVEL_2, "(%s %ld)", get_irn_opname(loop_var.init), get_irn_node_nr(loop_var.init)));
			}
			DB((dbg, LEVEL_2, " to "));
			if (is_Const(loop_var.limit)) {
				DB((dbg, LEVEL_2, "%ld", get_Const_long(loop_var.limit)));
			} else {
				DB((dbg, LEVEL_2, "(%s %ld)", get_irn_opname(loop_var.limit), get_irn_node_nr(loop_var.limit)));
			}
			DB((dbg, LEVEL_2, " step %ld", get_Const_long(loop_var.step)));
			DB((dbg, LEVEL_2, "\n"));
		}
	}
	loop_element element;
	for (size_t j = 0; j < get_loop_n_elements(loop); j++) {
		element = get_loop_element(loop, j);
		if (*element.kind == k_ir_loop) {
			if (num_variables) {
				loop_pagecache_variables(element.son, outer, vars);
			} else {
				loop_pagecache_outer(element.son);
			}
		}
	}
	return num_variables;
}


static void variable_tree(ir_loop *loop, ir_node *block, ir_node *factor, ir_node** mem, ir_loop*outer, loop_var_t *vars) {
	loop_var_t* var = NULL;
	for (size_t i = 0; i < ARR_LEN(vars); i++) {
		if ((vars + i)->loop == loop) {
			var = (vars + i);
			break;
		}
	}

	if (var != NULL) {
		ir_node *div = make_iterations(block, var, *mem, outer, vars);

		if (div != NULL) {
			*mem = new_r_Proj(div, mode_M, pn_Div_M);
			ir_node *one = new_r_Const(get_irn_irg(block), new_tarval_from_long(1, mode_Ls));
			ir_node *result = new_r_Add(block, one, new_r_Proj(div, mode_Ls, pn_Div_res));
			if (factor == NULL) {
				factor = result;
			} else {
				factor = new_r_Mul(block, factor, result);
			}
			DB((dbg, LEVEL_2, "Setting iteration count of %p to %+F\n", var, factor));
			var->iterations = factor;
		} else {
			DB((dbg, LEVEL_2, "Could not find iteration count for %p\n", var));
			var->iterations = NULL; // new_r_Const(get_irn_irg(block), new_tarval_from_long(0, mode_Ls));
		}
	}

	for (size_t i = 0; i < get_loop_n_elements(loop); i++) {
		loop_element el = get_loop_element(loop, i);
		if (*el.kind == k_ir_loop) {
			variable_tree(el.son, block, factor, mem, outer, vars);
		}
	}
}

static void loop_pagecache_memops(ir_loop *loop, loop_var_t *vars) {
	ir_node *header, *phi_loop, *dummy;
	loop_find_header(loop, &phi_loop, &header);
	dummy = get_block(get_irn_n(header, 0));

	list_head mem_ops;
	INIT_LIST_HEAD(&mem_ops);
	loop_pagecache_dfs(&mem_ops, phi_loop, loop);

	mem_ops_unique(&mem_ops);

	list_head *i;
	list_for_each(i, &mem_ops) {
		mem_op_t *op = (mem_op_t*)i;
		if (op->duplicate) continue;
		ir_node *node = op->irn;
		ir_node *address = op->address;
		src_loc_t loc = ir_retrieve_dbg_info(get_irn_dbg_info(node));
		DB((dbg, LEVEL_2, "%s:%d:%d %s %ld ", loc.file ? loc.file : "<no debug>", loc.line, loc.column, get_irn_opname(node), get_irn_node_nr(node)));

		int num_outs = 0;
		foreach_out_edge_safe(phi_loop, edge) {
			ir_node *src = get_edge_src_irn(edge);
			if (is_in_loop(loop, src)) continue;
			if (is_End(src)) continue;
			num_outs++;
		}
		if (num_outs == 0) {
			DB((dbg, LEVEL_2, "No out edge\n"));
			continue;
		}
		inc_irg_visited(get_irn_irg(node));
		bool has_phi = find_phi(address, loop);
		ir_node *size = new_r_Const(get_irn_irg(dummy), new_tarval_from_long(get_mode_size_bytes(op->mode), get_modeIu()));
		ir_node *boundInit = NULL, *boundLimit = NULL;
		if (has_phi) {
			DB((dbg, LEVEL_2, "- HAS PHI\n"));
			inc_irg_visited(get_irn_irg(node));
			boundInit = clone_to_block_with_replace(address, dummy, vars, false, loop);
			if (boundInit == NULL) {
				DB((dbg, LEVEL_2, "Could not create init bound \n"));
				continue;
			}
			inc_irg_visited(get_irn_irg(node));
			boundLimit = clone_to_block_with_replace(address, dummy, vars, true, loop);
			if (boundLimit == NULL) {
				DB((dbg, LEVEL_2, "Could not create limit bound \n"));
				continue;
			}
		} else {
			DB((dbg, LEVEL_2, "- NO PHI\n"));
			boundInit = address;
			boundLimit = address;
		}
		ir_loop *mem_op_loop = get_irn_loop(get_block(node));
		ir_node *iteration = NULL;
		for (size_t j = 0; j < ARR_LEN(vars); j++) {
			if (mem_op_loop == (vars + j)->loop && (vars + j)->iterations != NULL) {
				iteration = (vars+j)->iterations;
				DB((dbg, LEVEL_2, "Found iteration count of %p: %+F\n", (vars+j), iteration));
				break;
			}
		}
		if (iteration == NULL) {
			iteration = new_r_Const(get_irn_irg(node), new_tarval_from_long(0, mode_Ls));
			DB((dbg, LEVEL_2, "No iteration count found, using %+F\n", iteration));
		}

		ir_node *prefetch = make_prefetch(dummy, get_irn_n(phi_loop, 0), boundInit, boundLimit, size, iteration);

		ir_node *res = new_r_Proj(get_Proj_pred(prefetch), mode_T, pn_Call_T_result);
		ir_node *req = new_r_Proj(res, mode_Is, 0);
		set_Phi_pred(phi_loop, 0, prefetch);

		foreach_out_edge_safe(phi_loop, edge) {
			ir_node *src = get_edge_src_irn(edge);
			if (is_in_loop(loop, src)) continue;
			if (is_End(src)) continue;
			ir_node *block = get_block(src);
			if (is_Phi(src)) {
				block = get_block(get_Block_cfgpred(block, get_edge_src_pos(edge)));
			}
			set_irn_n(src, get_edge_src_pos(edge), src);
			ir_node *free = make_free(block, phi_loop, req);
			set_irn_n(src, get_edge_src_pos(edge), free);
		}
	}
}

static void loop_pagecache_outer(ir_loop *loop) {
	loop_var_t *vars = NEW_ARR_F(loop_var_t, 0);
	if (loop_pagecache_variables(loop, loop, &vars)) {
		ir_node *header, *phi_loop, *dummy;
		loop_find_header(loop, &phi_loop, &header);
		dummy = get_block(get_irn_n(header, 0));
		ir_node *mem = get_irn_n(phi_loop, 0);
		variable_tree(loop, dummy, NULL, &mem, loop, vars);
		set_Phi_pred(phi_loop, 0, mem);
		loop_pagecache_memops(loop, vars);
	}
	DEL_ARR_F(vars);
}

static ir_node *find_phi(ir_node *node, ir_loop *outer) {
	if (irn_visited_else_mark(node)) return NULL;
	if (!is_in_loop(outer, node)) return NULL;
	if (is_Phi(node)) return node;
	foreach_irn_in(node, i, pred) {
		ir_node *res = find_phi(pred, outer);
		if (res != NULL) return res;
	}
	return NULL;
}

static ir_node *find_confirm(ir_node *phi, ir_loop *loop) {
	foreach_out_edge(phi, edge) {
	    ir_node *src = get_edge_src_irn(edge);
	    if (!is_Confirm(src)) continue;
	    if (get_Confirm_relation(src) != ir_relation_less && get_Confirm_relation(src) != ir_relation_less_equal) continue;
	    if (get_Confirm_value(src) != phi) continue;

	    DB((dbg, LEVEL_2, "%+F ...", src));

	    ir_node *confirm_block = get_nodes_block(src);
	    if (!is_in_loop(loop, confirm_block)) {
		    DB((dbg, LEVEL_2, " not in the loop\n"));
		    continue;
	    }
	    ir_node *phi_block = get_nodes_block(phi);
	    if (confirm_block != phi_block && get_nodes_block(get_Block_cfgpred(confirm_block, 0)) != phi_block) {
		    DB((dbg, LEVEL_2, " wrong block %+F (phi block %+F)\n", confirm_block, phi_block));
		    continue;
	    }

	    DB((dbg, LEVEL_2, " OK!\n"));
	    return src;
	}
	return NULL;
}

static bool move_to_block_recursive(ir_node *node, ir_node *target, ir_loop *loop) {
	if (is_Phi(node)) return false;
	if (irn_visited_else_mark(node)) return true;
	if (is_in_loop(loop, node)) {
		set_nodes_block(node, target);
	}
	foreach_irn_in(node, i, pred) {
		if (!move_to_block_recursive(pred, target, loop)) return false;
	}
	return true;
}

static ir_node *clone_to_block_with_replace(ir_node *node, ir_node *target, loop_var_t *vars, bool init_limit, ir_loop *loop) {
	for (size_t i = 0; i < ARR_LEN(vars); i++) {
	    loop_var_t v = vars[i];
	    if (v.phi == node) {
	        ir_node *replace = init_limit ? v.limit : v.init;
			node = replace;
			break;
	    }
	}

	ir_node **in = XMALLOCN(ir_node*, get_irn_arity(node));
	if (is_in_loop(loop, node)) {
		if (is_Confirm(node)) {
			return clone_to_block_with_replace(get_Confirm_value(node), target, vars, init_limit, loop);
		}
		if (irn_visited_else_mark(node)) {
			DB((dbg, LEVEL_2, "Recursion in address, Node Nr %ld already visited\n", get_irn_node_nr(node)));
			return NULL;
		}
		foreach_irn_in(node, i, pred) {
			bool il = init_limit;
			if (is_Sub(node) && i == n_Sub_right) il = !il;
			in[i] = clone_to_block_with_replace(pred, target, vars, il, loop);
			if (in[i] == NULL) return NULL;
		}

		ir_node *new = new_ir_node(
			get_irn_dbg_info(node),
			get_irn_irg(node),
			target,
			get_irn_op(node),
			get_irn_mode(node),
			get_irn_arity(node),
			in
		);
		copy_node_attr(get_irn_irg(node), node, new);
		return new;
	}
	return node;
}

static void loop_pagecache_dfs(list_head *list, ir_node *node, ir_loop *loop) {
	if (irn_visited_else_mark(node)) {
		return;
	}
	if (is_Phi(node)) {
		for(int i = 0; i < get_Phi_n_preds(node); i++) {
			ir_node *pred = get_Phi_pred(node, i);
			// Is in same loop?
			if (is_in_loop(loop, pred)) {
				loop_pagecache_dfs(list, pred, loop);
			}
		}
	} else if (is_Load(node)) {
		mem_op_t *m = XMALLOC(mem_op_t);
		m->mode = get_type_mode(get_Load_type(node));
		m->irn = node;
		m->address = get_Load_ptr(node);
		m->count = 1;
		m->duplicate = false;
		list_add(&m->list, list);
		loop_pagecache_dfs(list, get_Load_mem(node), loop);
	} else if (is_Store(node)) {
		mem_op_t *m = XMALLOC(mem_op_t);
		m->mode = get_type_mode(get_Store_type(node));
		m->irn = node;
		m->address = get_Store_ptr(node);
        m->count = 1;
        m->duplicate = false;
		list_add(&m->list, list);
		loop_pagecache_dfs(list, get_Store_mem(node), loop);
	} else if (is_Proj(node)) {
		loop_pagecache_dfs(list, get_Proj_pred(node), loop);
	} else if (is_Call(node)) {
		loop_pagecache_dfs(list, get_irn_n(node, 0), loop);
	}
}

void do_loop_pagecache(ir_graph *const irg) {
	FIRM_DBG_REGISTER(dbg, "firm.opt.pagecache");

	DB((dbg, LEVEL_1, "do_loop_pagecache on %+F\n", irg));
	assure_irg_properties(irg, IR_GRAPH_PROPERTY_CONSISTENT_LOOPINFO | IR_GRAPH_PROPERTY_CONSISTENT_OUT_EDGES);
	ir_reserve_resources_(irg, IR_RESOURCE_IRN_LINK | IR_RESOURCE_PHI_LIST);
	collect_phiprojs_and_start_block_nodes(irg);
	ir_free_resources(irg, IR_RESOURCE_IRN_LINK);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	add_irg_constraints(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION);
	inc_irg_visited(irg);

	ir_loop *loop = get_irg_loop(irg);
	if (loop == NULL) {
		return;
	}
	// Find first loop child
	loop_element element;
	for (size_t i = 0; i < get_loop_n_elements(loop); i++) {
		element = get_loop_element(loop, i);
		if (*element.kind == k_ir_loop) {
			loop_pagecache_outer(element.son);
		}
	}

	ir_free_resources(irg, IR_RESOURCE_PHI_LIST | IR_RESOURCE_IRN_VISITED);
	clear_irg_constraints(irg, IR_GRAPH_CONSTRAINT_CONSTRUCTION);
	confirm_irg_properties(irg, IR_GRAPH_PROPERTIES_NONE);
}
