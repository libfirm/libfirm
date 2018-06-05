#include <assert.h>
#include <firm.h>
#include <stdint.h>

#include "aggregate.h"
#include "irnode_t.h"
#include "sitools.h"
#include "irop.h"
#include "irverify.h"
#include <debug.h>

DEBUG_ONLY(static firm_dbg_module_t *dbg;)

static void node_dump_Aggregate(FILE *out, const ir_node *self, dump_reason_t reason) {
  ir_mode* mode;

  switch(reason) {
  case dump_node_opcode_txt:
    fprintf(out, "Agg%s", get_id_str(get_Aggregate_op(self)->name));
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
    break;

  default:
    DBG((dbg, LEVEL_DEFAULT, "unknown dump_node_kind\n"));
  }
}

static irg_walk_func aggregate_node;
static void aggregate_node(ir_node *node, void *data) {
	ir_node *victim = data;

	if (!victim) {
		if (!is_binop(node)) return;
		if (!get_op_flags(get_irn_op((node))) & irop_flag_commutative) return;

		ir_node **in = NEW_ARR_F(ir_node *, 0);

		set_irn_link(node, in);

		foreach_irn_in(node, i, pred) {
			aggregate_node(pred, node);
		}

		in = (ir_node **)get_irn_link(node);

		if (ARR_LEN(in)<3) return;

		add_irg_constraints(get_irn_irg(node), IR_GRAPH_CONSTRAINT_CONSTRUCTION);
		ir_node *agg = new_Aggregate(ARR_LEN(in), in, get_irn_mode(node), get_irn_op(node));
		exchange(node, agg);
		clear_irg_constraints(get_irn_irg(node), IR_GRAPH_CONSTRAINT_CONSTRUCTION);

	} else {

		foreach_irn_in(node, i, pred) {
			if (get_irn_op(victim) == get_irn_op(pred)) {
				aggregate_node(pred, victim);
			} else {
				ir_node **in = (ir_node **)get_irn_link(victim);
				ARR_APP1(ir_node *, in, pred);
				set_irn_link(victim, in);
			}
		}
	}
}

/* Find aggregating constructs and turn them into Aggregate nodes. */
void aggregate_transform(ir_graph *irg)
{
  FIRM_DBG_REGISTER(dbg, "firm.si.aggregate");

  set_op_dump(op_Aggregate, node_dump_Aggregate);
  irg_walk_graph(irg, aggregate_node, 0, 0);
}

