#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#include "irgraph.h"
#include "irgwalk.h"
#include "irdump_t.h"
#include "ircons.h"
#include "iropt.h"
#include "irgopt.h"
#include "irprintf.h"

#include "beutil.h"
#include "besched_t.h"
#include "bearch.h"

struct dump_env {
  FILE *f;
  arch_env_t *env;
};

static void dump_allocated_block(ir_node *block, void *data)
{
	int i, n;
	const ir_node *irn;
  struct dump_env *dump_env = data;
	FILE *f = dump_env->f;
  arch_env_t *env = dump_env->env;

	ir_fprintf(f, "node:{title:\"b%N\"\nlabel:\"%n\n", block, block);
	sched_foreach(block, irn) {
		const char *prefix = "";

    const arch_register_t *reg = arch_get_irn_register(env, irn, 0);

		ir_fprintf(f, "\n");
    if(reg)
      ir_fprintf(f, "%s = ", arch_register_get_name(reg));
		ir_fprintf(f, "%n(", irn);

		if(block != get_irg_start_block(get_irn_irg(block))) {
			for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *op = get_irn_n(irn, i);
        if(arch_is_register_operand(dump_env->env, op, arch_pos_make_out(0))) {
					ir_fprintf(f, "%s%s", prefix,
              arch_register_get_name(arch_get_irn_register(env, op, 0)));
					prefix = ", ";
				}
			}
		}

		ir_fprintf(f, ")");
	}
	ir_fprintf(f, "\"}\n");

	if(get_irg_start_block(get_irn_irg(block)) != block) {
		for(i = 0, n = get_irn_arity(block); i < n; ++i) {
			ir_node *pred_bl = get_nodes_block(get_irn_n(block, i));
			ir_fprintf(f, "edge:{sourcename:\"b%N\" targetname:\"b%N\"}\n", pred_bl, block);
		}
	}
}

void dump_allocated_irg(arch_env_t *arch_env, ir_graph *irg, char *suffix)
{
	char buf[1024];
  struct dump_env env;

  env.env = arch_env;

	ir_snprintf(buf, sizeof(buf), "%F-alloc%s.vcg", irg, suffix);

	if((env.f = fopen(buf, "wt")) != NULL) {
		fprintf(env.f, "graph:{title:\"prg\"\n");
		irg_block_walk_graph(irg, dump_allocated_block, NULL, &env);
		fprintf(env.f, "}\n");
		fclose(env.f);
	}
}

static void localize_const_walker(ir_node *irn, void *data)
{
	if(!is_Block(irn)) {
		int i, n;

		for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *op = get_irn_n(irn, i);
			if(get_irn_opcode(op) == iro_Const) {
				ir_node *tgt_block, *cnst;

				/* Special treatment for phi nodes, because phi-usage is different */
				tgt_block = get_nodes_block(irn);
				if(is_Phi(irn))
					tgt_block = get_nodes_block(get_irn_n(tgt_block, i));

				/*
				 * We have to create the const node by ourselves, since the
				 * firmcons implementation always places it in the start block.
				 */
				cnst = new_ir_node(NULL, get_irn_irg(irn),
						tgt_block, op_Const, get_irn_mode(op), 0, NULL);
				cnst->attr.con.tv = get_Const_tarval(op);
				set_irn_n(irn, i, cnst);
			}
		}
	}
}

void localize_consts(ir_graph *irg)
{
	irg_walk_graph(irg, localize_const_walker, NULL, NULL);
	dead_node_elimination(irg);
}

static int sched_edge_hook(FILE *F, ir_node *irn)
{
    if(sched_is_scheduled(irn) && sched_has_prev(irn)) {
        ir_node *prev = sched_prev(irn);
        fprintf(F, "edge:{sourcename:\"");
        PRINT_NODEID(irn);
        fprintf(F, "\" targetname:\"");
        PRINT_NODEID(prev);
        fprintf(F, "\" color:magenta}\n");
    }
    return 1;
}

void dump_ir_block_graph_sched(ir_graph *irg, const char *suffix) {
    DUMP_NODE_EDGE_FUNC old = get_dump_node_edge_hook();

    set_dump_node_edge_hook(sched_edge_hook);
    dump_ir_block_graph(irg, suffix);
    set_dump_node_edge_hook(old);
}

static void clear_link(ir_node *irn, void *data)
{
  set_irn_link(irn, NULL);
}

static void collect_phis(ir_node *irn, void *data)
{
  if(is_Phi(irn)) {
    ir_node *bl = get_nodes_block(irn);
    set_irn_link(irn, get_irn_link(bl));
    set_irn_link(bl, irn);
  }
}

void be_clear_links(ir_graph *irg)
{
	irg_walk_graph(irg, clear_link, NULL, NULL);
}

void be_collect_phis(ir_graph *irg)
{
	irg_walk_graph(irg, collect_phis, NULL, NULL);
}
