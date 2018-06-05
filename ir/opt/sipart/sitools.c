#include <assert.h>
#include <firm.h>
#include <stdint.h>

#include "irnode_t.h"
#include "sitools.h"
#include <debug.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg;)
#define panic(x) do { puts(x); exit(-1);  } while(0)

void unpin_loadstore(ir_node *node, void *data) {
	(void) data;
	if (is_Load(node) || is_Store(node))
		set_irn_pinned(node, op_pin_state_floats);
}

irg_walk_func collect_deps_walker;
void collect_deps_walker(ir_node *node, void *data) {
	ir_node **list = data;
	if(is_Block(node)) return;
	if(mode_X == get_irn_mode(node)) return;
	set_irn_link(node, *list);
	*list = node;
}

bool expell_loop_increment(ir_node *block) {
	FIRM_DBG_REGISTER(dbg, "firm.si.tools");
	if (1 != get_Block_n_cfgpreds(block)) {
		DB2("cannot expunge block - more than 1 cfgpred\n");
		return 0;
	}
	ir_node *list = 0;
	ir_node *cfgpred = get_Block_cfgpred(block, 0);

	irg_walk(cfgpred, collect_deps_walker, 0, &list);

	{
		unsigned count = 0;
		for (ir_node* n = list; n; n = get_irn_link(n))
			if (block == get_nodes_block(n)) count++;
		if (count > 2) {
			DB2("larger computation, probably not a loop increment\n");
			return 0;
		}
	}

	ir_node *target_block = 0;

	for (; list; list = get_irn_link(list)) {
		if (get_nodes_block(list) == block) {
			/* CF depends on computation inside BB */
			if (!target_block) {
				add_irg_constraints(get_irn_irg(block), IR_GRAPH_CONSTRAINT_CONSTRUCTION);
				ir_node *in = cfgpred;
				target_block = new_Block(1, &in);
				set_Block_cfgpred(block, 0, new_r_Jmp(target_block));
			}
			DB2("expelling %+F\n", list);
			set_nodes_block(list, target_block);
		}
	}
	if (target_block)
		irg_finalize_cons(get_irn_irg(block));
	return 1;
}
