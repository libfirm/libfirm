/**
 * @file   beuse.c
 * @date   27.06.2005
 * @author Sebastian Hack, Matthias Braun
 *
 * Methods to compute when a value will be used again.
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <limits.h>
#include <stdlib.h>

#include "config.h"
#include "obst.h"
#include "pmap.h"
#include "debug.h"

#include "irgwalk.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irgraph_t.h"
#include "iredges_t.h"
#include "irdom_t.h"

#include "be_t.h"
#include "beutil.h"
#include "belive_t.h"
#include "benode_t.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "bearch.h"
#include "beuses_t.h"
#include "benodesets.h"

#define SCAN_INTERBLOCK_USES

typedef struct _be_use_t {
	const ir_node *block;
	const ir_node *node;
	unsigned next_use;
} be_use_t;

struct _be_uses_t {
  	set *uses;
	ir_graph *irg;
	const ir_exec_freq *execfreqs;
	const be_lv_t *lv;
	DEBUG_ONLY(firm_dbg_module_t *dbg;)
};

static int cmp_use(const void *a, const void *b, size_t n)
{
	const be_use_t *p = a;
	const be_use_t *q = b;
	return !(p->block == q->block && p->node == q->node);
}

static const be_use_t *get_or_set_use_block(be_uses_t *uses,
                                            const ir_node *block,
                                            const ir_node *def)
{
	unsigned hash = HASH_COMBINE(nodeset_hash(block), nodeset_hash(def));
	be_use_t temp;
	be_use_t* result;

	temp.block = block;
	temp.node = def;
	result = set_find(uses->uses, &temp, sizeof(temp), hash);

	if(result == NULL) {
		// insert templ first as we might end in a loop in the get_next_use
		// call otherwise
		temp.next_use = USES_INFINITY;
		result = set_insert(uses->uses, &temp, sizeof(temp), hash);

		result->next_use = be_get_next_use(uses, sched_first(block), 0, def, 0);
	}

	return result;
}

unsigned be_get_next_use(be_uses_t *uses, const ir_node *from,
                         unsigned from_step, const ir_node *def,
                         int skip_from_uses)
{
	unsigned step = from_step;
	ir_node *block = get_nodes_block(from);
	const ir_node *node;
	const ir_edge_t *edge;

	if(skip_from_uses) {
		step++;
		from = sched_next(from);
	}

	sched_foreach_from(from, node) {
		int i, arity;

		arity = get_irn_arity(node);
		for (i = 0; i < arity; ++i) {
			const ir_node *operand = get_irn_n(node, i);

			if (operand == def) {
				DBG((uses->dbg, LEVEL_3, "found use of %+F at %+F\n", operand, node));
				return step;
			}
		}

		step++;
	}

	if(be_is_live_end(uses->lv, block, def))
		return step;

#ifdef SCAN_INTERBLOCK_USES
	{
	double best_execfreq = -1;
	unsigned next_use = USES_INFINITY;

	foreach_block_succ(block, edge) {
		const be_use_t *use;
		const ir_node *succ_block = get_edge_src_irn(edge);
		double execfreq = get_block_execfreq(uses->execfreqs, succ_block);

		//execfreq_sum += execfreq;

		if(execfreq > best_execfreq) {
			best_execfreq = execfreq;

			if(!be_is_live_in(uses->lv, succ_block, def)) {
				next_use = USES_INFINITY;
				continue;
			}

			use = get_or_set_use_block(uses, succ_block, def);
			//if(USES_IS_INFINITE(use->next_use))
			//	continue;

			next_use = use->next_use;
		}

		//next_use += use->next_use / execfreq;
	}

	/*if(next_use == 0)
		return USES_INFINITY;*/

	//next_use /= execfreq_sum;

	return ((unsigned) next_use) + step;
	}
#else
	return USES_INFINITY;
#endif
}

be_uses_t *be_begin_uses(ir_graph *irg, const ir_exec_freq *execfreqs, const be_lv_t *lv)
{
	be_uses_t *uses = xmalloc(sizeof(uses[0]));

	edges_assure(irg);

	uses->uses = new_set(cmp_use, 512);
	uses->irg = irg;
	uses->execfreqs = execfreqs;
	uses->lv = lv;
	FIRM_DBG_REGISTER(uses->dbg, "firm.be.uses");

	return uses;
}

void be_end_uses(be_uses_t *uses)
{
	del_set(uses->uses);
	free(uses);
}
