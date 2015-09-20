#include "panic.h"
#include "bearch_amd64_t.h"
#include "amd64_new_nodes.h"
#include "gen_amd64_regalloc_if.h"
#include "../ia32/x86_x87.h"

static unsigned get_bits_from_insn_mode(amd64_insn_mode_t const insn_mode)
{
	switch (insn_mode) {
	case INSN_MODE_8: return 8;
	case INSN_MODE_16: return 16;
	case INSN_MODE_32: return 32;
	case INSN_MODE_64: return 64;
	case INSN_MODE_128: return 128;
	case INSN_MODE_INVALID:
		break;
	}
	panic("invalid insn_mode");
}

static void sim_amd64_fst(x87_state *const state, ir_node *const node)
{
	amd64_addr_attr_t const *const attr = get_amd64_addr_attr_const(node);
	unsigned const bits = get_bits_from_insn_mode(attr->insn_mode);
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

static void prepare_callbacks(void)
{
	x86_prepare_x87_callbacks();
	x86_register_x87_sim(op_amd64_call, sim_amd64_call);
	x86_register_x87_sim(op_amd64_fld,  sim_amd64_fld);
	x86_register_x87_sim(op_amd64_fld1, x86_x87_push);
	x86_register_x87_sim(op_amd64_fldz, x86_x87_push);
	x86_register_x87_sim(op_amd64_fchs, x86_sim_x87_unop);
	x86_register_x87_sim(op_amd64_fst,  sim_amd64_fst);
	x86_register_x87_sim(op_amd64_fstp, sim_amd64_fstp);
	x86_register_x87_sim(op_amd64_ret,  x86_sim_x87_ret);
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
