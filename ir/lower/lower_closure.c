/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Replaces Closure nodes
 * @author  Daniel Krueger
 */
#include <assert.h>

#include "lowering.h"
#include "array.h"
#include "irnode_t.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irgmod.h"
#include "ircons.h"
#include "util.h"


typedef struct closure_type {
	ir_type *closure_struct;
	ir_entity *closure_proc;
	ir_entity *closure_env;
} closure_type;

typedef struct walk_env {
	ir_node **closures;
	ir_node **calls;
} walk_env_t;

static void find_closure_nodes(ir_node *node, void *ctx)
{
	walk_env_t *env = (walk_env_t*)ctx;

	if (is_Closure(node)) {
		ARR_APP1(ir_node*, env->closures, node);
	} else if (is_CallClosure(node)) {
		ARR_APP1(ir_node*, env->calls, node);
	}
}

static void lower_callclosure_node(ir_node *call, closure_type *t)
{
	ir_node *block = get_nodes_block(call);

	ir_node *closure          = get_CallClosure_ptr(call);
	ir_node *closure_proc_res = new_r_Load(block, get_CallClosure_mem(call), new_r_Member(block, closure, t->closure_proc), mode_P, get_entity_type(t->closure_proc), cons_none);
	ir_node *closure_proc_mem = new_r_Proj(closure_proc_res, mode_M, pn_Load_M);
	ir_node *closure_proc_ptr = new_r_Proj(closure_proc_res, mode_P, pn_Load_res);
	ir_node *closure_env_res  = new_r_Load(block, closure_proc_mem, new_r_Member(block, closure, t->closure_env), mode_P, get_entity_type(t->closure_env), cons_none);
	ir_node *closure_env_mem  = new_r_Proj(closure_env_res, mode_M, pn_Load_M);
	ir_node *closure_env_ptr  = new_r_Proj(closure_env_res, mode_P, pn_Load_res);

	size_t n_params         = get_CallClosure_n_params(call) + 1;
	ir_node* args[n_params];
	args[0]                 = closure_env_ptr;
	for (size_t i = 1; i < n_params; ++i) {
		args[i] = get_CallClosure_param(call, i-1);
	}

	ir_node *real_call           = new_r_Call(block, closure_env_mem, closure_proc_ptr, n_params, args, get_CallClosure_type(call));
	ir_node *real_call_M         = new_r_Proj(real_call, mode_M, pn_Call_M);
	ir_node *real_call_T_result  = new_r_Proj(real_call, mode_T, pn_Call_T_result);

	ir_node *call_rets[pn_CallClosure_max + 1];
	call_rets[pn_CallClosure_M]         = real_call_M;
	call_rets[pn_CallClosure_T_result]  = real_call_T_result;

	ir_node *call_result = new_r_Tuple(block, pn_CallClosure_max + 1, call_rets);

	exchange(call, call_result);
}

static void lower_closure_node(ir_node *closure, ir_entity *malloc_ent, closure_type *t)
{
	ir_node  *block = get_nodes_block(closure);
	ir_graph *graph = get_irn_irg(closure);

	ir_node *malloc_args[] = { new_r_Size(graph, mode_Iu, t->closure_struct) };
	ir_node *malloc_res    = new_r_Call(block, get_Closure_mem(closure), new_r_Address(graph, malloc_ent), 1, malloc_args, get_entity_type(malloc_ent));
	ir_node *malloc_store  = new_r_Proj(malloc_res, mode_M, pn_Call_M);
	ir_node *malloc_ret    = new_r_Proj(malloc_res, mode_T, pn_Call_T_result);
	ir_node *closure_ptr   = new_r_Proj(malloc_ret, mode_P, 0);

	ir_node *store_closure_proc = new_r_Store(block, malloc_store, new_r_Member(block, closure_ptr, t->closure_proc), get_Closure_proc(closure), get_entity_type(t->closure_proc), cons_none);
	ir_node *closure_proc_store = new_r_Proj(store_closure_proc, mode_M, pn_Store_M);

	ir_node *store_closure_env = new_r_Store(block, closure_proc_store, new_r_Member(block, closure_ptr, t->closure_env), get_Closure_env(closure), get_entity_type(t->closure_proc), cons_none);
	ir_node *closure_env_store = new_r_Proj(store_closure_env, mode_M, pn_Store_M);


	ir_node *closure_rets[pn_Closure_max + 1];
	closure_rets[pn_Closure_M]   = closure_env_store;
	closure_rets[pn_Closure_res] = closure_ptr;

	ir_node *closure_result = new_r_Tuple(block, pn_Closure_max + 1, closure_rets);

	exchange(closure, closure_result);
}

ir_entity* create_std_malloc_entity(char* name)
{
	ir_type *malloc_type = new_type_method(1, 1, false, cc_cdecl_set, mtp_property_malloc);
	set_method_param_type(malloc_type, 0, get_type_for_mode(mode_Iu));
	set_method_res_type(malloc_type, 0, get_type_for_mode(mode_P));

	ident     *malloc_id  = new_id_from_str(name);
	ir_entity *malloc_ent = new_entity(get_glob_type(), malloc_id, malloc_type);

	set_entity_ld_ident(malloc_ent, malloc_id);
	set_entity_additional_properties(malloc_ent, mtp_property_malloc);

	return malloc_ent;
}

static void init_closure_type(closure_type *t)
{
	t->closure_struct = new_type_struct(id_unique("closure_type"));
	t->closure_proc = new_entity(t->closure_struct, id_unique("closure_proc"), get_type_for_mode(mode_P));
	t->closure_env  = new_entity(t->closure_struct, id_unique("closure_env"), get_type_for_mode(mode_P));
	default_layout_compound_type(t->closure_struct);
}


static void lower_irg_closure(ir_graph *irg, ir_entity* malloc_ent, closure_type *t)
{
	/* Scan the graph for closure nodes to lower. */
	walk_env_t env;
	env.closures = NEW_ARR_F(ir_node*, 0);
	env.calls    = NEW_ARR_F(ir_node*, 0);
	irg_walk_graph(irg, find_closure_nodes, 0, &env);

	size_t n_closures = ARR_LEN(env.closures);
	for (size_t i = 0; i < n_closures; ++i) {
		lower_closure_node(env.closures[i], malloc_ent, t);
	}
	DEL_ARR_F(env.closures);

	size_t n_calls = ARR_LEN(env.calls);
	for (size_t i = 0; i < n_calls; ++i) {
		lower_callclosure_node(env.calls[i], t);
	}
	DEL_ARR_F(env.calls);
}


void lower_closure(ir_entity* malloc_ent)
{
	closure_type t;
	init_closure_type(&t);

	for (size_t i = 0; i < get_irp_n_irgs(); i++) {
                ir_graph *irg = get_irp_irg(i);
		lower_irg_closure(irg, malloc_ent, &t);
        }
}
