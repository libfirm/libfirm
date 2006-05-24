/**
 * Interblock liveness analysis.
 * @author Sebastian Hack
 * @date 6.12.2004
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "impl.h"
#include "iredges_t.h"
#include "irgwalk.h"
#include "irprintf_t.h"
#include "irbitset.h"

#include "beutil.h"
#include "belive_t.h"
#include "besched_t.h"

#define DBG_MODULE "firm.be.liveness"

FIRM_IMPL2(is_live_in, int, const ir_node *, const ir_node *)
FIRM_IMPL2(is_live_out, int, const ir_node *, const ir_node *)
FIRM_IMPL2(is_live_end, int, const ir_node *, const ir_node *)

/** The offset of the liveness information in a firm node. */
size_t live_irg_data_offset = 0;

void be_liveness_init(void)
{
  live_irg_data_offset = register_additional_graph_data(sizeof(irn_live_t));
}

/**
 * Mark a node as live-in in a block.
 */
static INLINE void mark_live_in(ir_node *block, const ir_node *irn)
{
  _get_or_set_live(block, irn, live_state_in);
}

/**
 * Mark a node as live-out in a block.
 */
static INLINE void mark_live_out(ir_node *block, const ir_node *irn)
{
  _get_or_set_live(block, irn, live_state_out | live_state_end);
}

/**
 * Mark a node as live-end in a block.
 */
static INLINE void mark_live_end(ir_node *block, const ir_node *irn)
{
  _get_or_set_live(block, irn, live_state_end);
}

/**
 * Mark a node (value) live out at a certain block. Do this also
 * transitively, i.e. if the block is not the block of the value's
 * definition, all predecessors are also marked live.
 * @param def The node (value).
 * @param block The block to mark the value live out of.
 * @param visited A set were all visited blocks are recorded.
 * @param is_true_out Is the node real out there or only live at the end
 * of the block.
 */
static void live_end_at_block(ir_node *def, ir_node *block, bitset_t *visited, int is_true_out)
{
	mark_live_end(block, def);
	if(is_true_out)
		mark_live_out(block, def);

	if(!bitset_contains_irn(visited, block)) {
		bitset_add_irn(visited, block);

		/*
		* If this block is not the definition block, we have to go up
		* further.
		*/
		if(get_nodes_block(def) != block) {
			int i, n;

			mark_live_in(block, def);

			for(i = 0, n = get_Block_n_cfgpreds(block); i < n; ++i)
				live_end_at_block(def, get_Block_cfgpred_block(block, i), visited, 1);
		}

	}
}

/**
 * Liveness analysis for a value.
 * This functions is meant to be called by a firm walker, to compute the
 * set of all blocks a value is live in.
 * @param irn The node (value).
 * @param env Ignored.
 */
static void liveness_for_node(ir_node *irn, void *data)
{
	bitset_t *visited = data;
	const ir_edge_t *edge;
	ir_node *def_block;

	/* Don't compute liveness information for non-data nodes. */
	if(!is_data_node(irn))
		return;

	bitset_clear_all(visited);
	def_block = get_nodes_block(irn);

	/* Go over all uses of the value */
	foreach_out_edge(irn, edge) {
		ir_node *use = edge->src;
		ir_node *use_block;

		/*
		* If the usage is no data node, skip this use, since it does not
		* affect the liveness of the node.
		*/
		if(!is_data_node(use))
			continue;

		/* Get the block where the usage is in. */
		use_block = get_nodes_block(use);

		/*
		* If the use is a phi function, determine the corresponding block
		* through which the value reaches the phi function and mark the
		* value as live out of that block.
		*/
		if(is_Phi(use)) {
			ir_node *pred_block = get_Block_cfgpred_block(use_block, edge->pos);
			live_end_at_block(irn, pred_block, visited, 0);
		}

		/*
		* Else, the value is live in at this block. Mark it and call live
		* out on the predecessors.
		*/
		else if(def_block != use_block) {
			int i, n;

			mark_live_in(use_block, irn);

			for(i = 0, n = get_Block_n_cfgpreds(use_block); i < n; ++i) {
				ir_node *pred_block = get_Block_cfgpred_block(use_block, i);
				live_end_at_block(irn, pred_block, visited, 1);
			}
		}
	}
}

/**
 * Compare two live entries.
 */
static int cmp_irn_live(const void *a, const void *b, size_t size)
{
  const irn_live_t *p = a;
  const irn_live_t *q = b;

  return !(p->block == q->block && p->irn == q->irn);
}

static int (*old_dump_block_func)(ir_node *self, FILE *F, dump_reason_t reason) = NULL;

static int dump_block_func(ir_node *self, FILE *F, dump_reason_t reason)
{
	switch(reason) {
	case dump_node_opcode_txt:
		fprintf(F, get_irn_opname(self));
		break;
	case dump_node_mode_txt:
		fprintf(F, get_irn_modename(self));
		break;
	case dump_node_nodeattr_txt:
		break;
	case dump_node_info_txt:
		if(!get_irg_live_info(get_irn_irg(self))->live)
			return 0;
#if 0
		fprintf(F, "liveness information:\n");
		{
			irn_live_t *li;
			live_foreach(self, li) {
				ir_fprintf(F, "%+F", li->irn);
				if(live_is_in(li))
					fprintf(F, " in");
				if(live_is_end(li))
					fprintf(F, " end");
				if(live_is_out(li))
					fprintf(F, " out");

				fprintf(F, "\n");
			}
		}
#endif
	}

	return 0;
}

/* Compute the inter block liveness for a graph. */
void be_liveness(ir_graph *irg)
{
	bitset_t *visited = bitset_irg_malloc(irg);

	irg_live_info_t *live_info = get_irg_live_info(irg);
	if(live_info->live)
		del_set(live_info->live);

	live_info->live = new_set(cmp_irn_live, 8192);
	irg_walk_graph(irg, liveness_for_node, NULL, visited);

	old_dump_block_func     = op_Block->ops.dump_node;
	op_Block->ops.dump_node = dump_block_func;

	bitset_free(visited);
}

/**
 * Pre-walker: dump liveness data to a file
 */
static void dump_liveness_walker(ir_node *bl, void *data)
{
	FILE *f = data;
	const irn_live_t *li;
	char buf[64];

	ir_fprintf(f, "%+F\n", bl);
	live_foreach(bl, li) {
		strcpy(buf, "");

		if(live_is_in(li))
			strcat(buf, "in ");

		if(live_is_end(li))
			strcat(buf, "end ");

		if(live_is_out(li))
			strcat(buf, "out ");

		ir_fprintf(f, "\t%+20F %s\n", li->irn, buf);
	}
}

/* Dump the liveness information for a graph. */
void be_liveness_dump(ir_graph *irg, FILE *f)
{
	irg_block_walk_graph(irg, dump_liveness_walker, NULL, f);
}

/* Dump the liveness information for a graph. */
void be_liveness_dumpto(ir_graph *irg, const char *cls_name)
{
	FILE *f;
	char buf[128];
	ir_snprintf(buf, sizeof(buf), "%F_%s-live.txt", irg, cls_name);
	if((f = fopen(buf, "wt")) != NULL) {
		be_liveness_dump(irg, f);
		fclose(f);
	}
}

/**
 * Walker: checks the every predecessors of a node dominate
 * the note.
 */
static void dom_check(ir_node *irn, void *data)
{
	if(!is_Block(irn) && irn != get_irg_end(get_irn_irg(irn))) {
		int i, n;
		ir_node *bl = get_nodes_block(irn);

		for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
			ir_node *op     = get_irn_n(irn, i);
			ir_node *def_bl = get_nodes_block(op);
			ir_node *use_bl = bl;

			if(is_Phi(irn))
				use_bl = get_Block_cfgpred_block(bl, i);

			if(!block_dominates(def_bl, use_bl)) {
				ir_fprintf(stderr, "%+F in %+F must dominate %+F for user %+F\n", op, def_bl, use_bl, irn);
				assert(0);
			}
		}
	}
}

/* Check, if the SSA dominance property is fulfilled. */
void be_check_dominance(ir_graph *irg)
{
	irg_walk_graph(irg, dom_check, NULL, NULL);
}

pset *be_liveness_transfer(const arch_env_t *arch_env, const arch_register_class_t *cls, ir_node *irn, pset *live)
{
	int i, n;
	ir_node *x;
	FIRM_DBG_REGISTER(firm_dbg_module_t *dbg, DBG_MODULE);

	DEBUG_ONLY(
		DBG((dbg, LEVEL_1, "%+F\n", irn));
		for(x = pset_first(live); x; x = pset_next(live))
			DBG((dbg, LEVEL_1, "\tlive: %+F\n", x));
	)

	/* You should better break out of your loop when hitting the first phi function. */
	assert(!is_Phi(irn) && "liveness_transfer produces invalid results for phi nodes");

	if(arch_irn_consider_in_reg_alloc(arch_env, cls, irn)) {
		ir_node *del = pset_remove_ptr(live, irn);
		assert(irn == del);
	}

	for(i = 0, n = get_irn_arity(irn); i < n; ++i) {
		ir_node *op = get_irn_n(irn, i);

		if(arch_irn_consider_in_reg_alloc(arch_env, cls, op))
			pset_insert_ptr(live, op);
	}

	return live;
}

pset *be_liveness_end_of_block(const arch_env_t *arch_env, const arch_register_class_t *cls, const ir_node *bl, pset *live)
{
	irn_live_t *li;

	live_foreach(bl, li) {
		ir_node *irn = (ir_node *) li->irn;
		if(live_is_end(li) && arch_irn_consider_in_reg_alloc(arch_env, cls, irn))
			pset_insert_ptr(live, irn);
	}

	return live;
}

pset *be_liveness_nodes_live_at(const arch_env_t *arch_env, const arch_register_class_t *cls, const ir_node *pos, pset *live)
{
	const ir_node *bl = is_Block(pos) ? pos : get_nodes_block(pos);
	ir_node *irn;

	be_liveness_end_of_block(arch_env, cls, bl, live);
	sched_foreach_reverse(bl, irn) {
		/*
		 * If we encounter the node we want to insert the Perm after,
		 * exit immediately, so that this node is still live
		 */
		if(irn == pos)
			return live;

		be_liveness_transfer(arch_env, cls, irn, live);
	}

	return live;
}
