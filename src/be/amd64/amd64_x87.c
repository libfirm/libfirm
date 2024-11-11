#include "../ia32/x86_x87.h"
#include "amd64_bearch_t.h"
#include "amd64_new_nodes.h"
#include "gen_amd64_regalloc_if.h"
#include "panic.h"

static void sim_amd64_fst(x87_state *const state, ir_node *const node)
{
	amd64_addr_attr_t const *const attr = get_amd64_addr_attr_const(node);
	unsigned const bits = x86_bytes_from_size(attr->base.size) * 8;
	x86_sim_x87_store(state, node, 0, bits);
}

static void sim_amd64_fstp(x87_state *const state, ir_node *const node)
{
	x86_sim_x87_store_pop(state, node, 0);
}

static void sim_amd64_fld(x87_state *const state, ir_node *const node)
{
	ir_node *const value = get_Proj_for_pn(node, pn_amd64_fld_res);
	x86_sim_x87_load(state, node, value);
}

static void sim_amd64_fild(x87_state *const state, ir_node *const node)
{
	ir_node *const value = get_Proj_for_pn(node, pn_amd64_fild_res);
	x86_sim_x87_load(state, node, value);
}

static void sim_amd64_fisttp(x87_state *const state, ir_node *const node)
{
	x86_sim_x87_store_pop(state, node, 0);
}

static void sim_amd64_call(x87_state *const state, ir_node *const node)
{
	/** push fp results onto x87 stack */
	amd64_call_addr_attr_t const *const attr
		= get_amd64_call_addr_attr(node);
	for (unsigned o = 0, n = attr->n_reg_results; o < n; ++o) {
		unsigned const pn = pn_amd64_call_first_result + o;
		arch_register_t const *const reg = arch_get_irn_register_out(node, pn);
		if (reg->cls == &amd64_reg_classes[CLASS_amd64_x87]) {
			ir_node *const value = get_Proj_for_pn(node, pn);
			x86_x87_push(state, value);
		}
	}
}

static void sim_amd64_fadd(x87_state *const state, ir_node *const node)
{
	arch_register_t const *const out = arch_get_irn_register_out(node, 0);
	x86_sim_x87_binop(state, node, n_amd64_fadd_left, n_amd64_fadd_right, out);
}

static void sim_amd64_fsub(x87_state *const state, ir_node *const node)
{
	arch_register_t const *const out = arch_get_irn_register_out(node, 0);
	x86_sim_x87_binop(state, node, n_amd64_fsub_left, n_amd64_fsub_right, out);
}

static void sim_amd64_fmul(x87_state *const state, ir_node *const node)
{
	arch_register_t const *const out = arch_get_irn_register_out(node, 0);
	x86_sim_x87_binop(state, node, n_amd64_fmul_left, n_amd64_fmul_right, out);
}

static void sim_amd64_fdiv(x87_state *const state, ir_node *const node)
{
	arch_register_t const *const out = arch_get_irn_register_out(node, 0);
	x86_sim_x87_binop(state, node, n_amd64_fdiv_left, n_amd64_fdiv_right, out);
}

static void sim_amd64_fucomi(x87_state *const state, ir_node *const node)
{
	ir_node *const op0 = get_irn_n(node, n_amd64_fucomi_left);
	ir_node *const op1 = get_irn_n(node, n_amd64_fucomi_right);
	unsigned const additional_pop = x86_sim_x87_fucom(state, node, op0, op1);
	if (additional_pop != X87_NOT_ON_STACK)
		x86_x87_create_fpop(state, node, additional_pop);
}

static void prepare_callbacks(void)
{
	x86_prepare_x87_callbacks();
	x86_register_x87_sim(op_amd64_call,   sim_amd64_call);
	x86_register_x87_sim(op_amd64_fadd,   sim_amd64_fadd);
	x86_register_x87_sim(op_amd64_fchs,   x86_sim_x87_unop);
	x86_register_x87_sim(op_amd64_fdiv,   sim_amd64_fdiv);
	x86_register_x87_sim(op_amd64_fild,   sim_amd64_fild);
	x86_register_x87_sim(op_amd64_fisttp, sim_amd64_fisttp);
	x86_register_x87_sim(op_amd64_fld,    sim_amd64_fld);
	x86_register_x87_sim(op_amd64_fld1,   x86_x87_push);
	x86_register_x87_sim(op_amd64_fldz,   x86_x87_push);
	x86_register_x87_sim(op_amd64_fmul,   sim_amd64_fmul);
	x86_register_x87_sim(op_amd64_fst,    sim_amd64_fst);
	x86_register_x87_sim(op_amd64_fstp,   sim_amd64_fstp);
	x86_register_x87_sim(op_amd64_fsub,   sim_amd64_fsub);
	x86_register_x87_sim(op_amd64_fucomi, sim_amd64_fucomi);
	x86_register_x87_sim(op_amd64_ret,    x86_sim_x87_ret);
}

void amd64_simulate_graph_x87(ir_graph *irg)
{
	prepare_callbacks();
	const x87_simulator_config_t config = {
		.regclass     = &amd64_reg_classes[CLASS_amd64_x87],
		.new_bd_fdup  = new_bd_amd64_fdup,
		.new_bd_fxch  = new_bd_amd64_fxch,
		.new_bd_fpop  = new_bd_amd64_fpop,
		.get_x87_attr = amd64_get_x87_attr,
	};
	x86_x87_simulate_graph(irg, &config);
}
