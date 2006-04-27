/**
 * @file   beuse.c
 * @date   27.06.2005
 * @author Sebastian Hack
 *
 * Methods to compute when a value will be used again.
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

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

#define DBG_LEVEL SET_LEVEL_0

typedef struct _be_use_t {
	const ir_node *bl;
	const ir_node *irn;
	unsigned next_use;
	int is_set;
} be_use_t;

struct _be_uses_t {
  set *uses;
  ir_graph *irg;
  const arch_env_t *arch_env;
  DEBUG_ONLY(firm_dbg_module_t *dbg;)
};

static INLINE unsigned sadd(unsigned a, unsigned b)
{
  return a + b;
}

static INLINE unsigned sdiv(unsigned a, unsigned b)
{
  return a / b;
}

static int cmp_use(const void *a, const void *b, size_t n)
{
  const be_use_t *p = a;
  const be_use_t *q = b;
  return !(p->bl == q->bl && p->irn == q->irn);
}

static INLINE be_use_t *get_or_set_use(be_uses_t *uses,
    const ir_node *bl, const ir_node *irn, unsigned next_use)
{
  unsigned hash = HASH_COMBINE(HASH_PTR(bl), HASH_PTR(irn));
  be_use_t templ;

  templ.bl = bl;
  templ.irn = irn;
  templ.next_use = next_use;
  templ.is_set = 0;
  return set_insert(uses->uses, &templ, sizeof(templ), hash);
}

unsigned be_get_next_use(be_uses_t *uses, const ir_node *from,
    unsigned from_step, const ir_node *def, int skip_from_uses);

static unsigned get_next_use_bl(be_uses_t *uses, const ir_node *bl,
    const ir_node *def)
{
  be_use_t *u;

  u = get_or_set_use(uses, bl, def, 0);
  if (! u->is_set) {
	u->is_set = 1;
	u->next_use = USES_INFINITY;
	u->next_use = be_get_next_use(uses, sched_first(bl), 0, def, 0);
  }
  return u->next_use;
}

static unsigned get_next_use(be_uses_t *uses, const ir_node *from, unsigned from_step, const ir_node *def, int skip_from_uses, unsigned long visited_nr)
{
	unsigned next_use = USES_INFINITY;
	unsigned step     = from_step;
	unsigned n        = 0;
	ir_node *bl       = get_nodes_block(from);
	const ir_node *irn;
	const ir_edge_t *succ_edge;

	set_irn_visited(bl, visited_nr);

	sched_foreach_from(from, irn) {
		int i, n;

		if (! skip_from_uses) {
			for (i = 0, n = get_irn_arity(irn); i < n; ++i) {
				ir_node *operand = get_irn_n(irn, i);

				if (operand == def) {
					DBG((uses->dbg, LEVEL_3, "found use of %+F at %+F\n", operand, irn));
					return step;
				}
			}
		}

		skip_from_uses = 0;
		step++;
	}

	/* FIXME: quick and dirty hack to prevent ignore nodes (like stack pointer) from being spilled */
	return is_live_end(bl, def) ? step : USES_INFINITY;

	next_use = USES_INFINITY;
	foreach_block_succ(bl, succ_edge) {
		const ir_node *succ_bl = succ_edge->src;
		if(get_irn_visited(succ_bl) < visited_nr && (is_live_in(succ_bl, def) || (get_irn_arity(succ_bl) > 1 && is_live_end(bl, def)))) {
			unsigned next = get_next_use_bl(uses, succ_bl, def);

			DBG((uses->dbg, LEVEL_2, "\t\tnext use in succ %+F: %d\n", succ_bl, next));
			next_use = MIN(next_use, next);
			n++;
		}
	}

	return next_use + step;
}

unsigned be_get_next_use(be_uses_t *uses, const ir_node *from, unsigned from_step, const ir_node *def, int skip_from_uses)
{
	unsigned long visited_nr = get_irg_visited(uses->irg) + 1;

	set_irg_visited(uses->irg, visited_nr);
	return get_next_use(uses, from, from_step, def, skip_from_uses, visited_nr);
}


be_uses_t *be_begin_uses(ir_graph *irg, const arch_env_t *arch_env, const arch_register_class_t *cls)
{
  be_uses_t *uses = xmalloc(sizeof(uses[0]));

  edges_assure(irg);

  uses->arch_env = arch_env;
  uses->uses     = new_set(cmp_use, 512);
  uses->irg      = irg;
  FIRM_DBG_REGISTER(uses->dbg, "firm.be.uses");

  return uses;
}

void be_end_uses(be_uses_t *uses)
{
  del_set(uses->uses);
  free(uses);
}

int loc_compare(const void *a, const void *b)
{
  const loc_t *p = a;
  const loc_t *q = b;
  return p->time - q->time;
}
