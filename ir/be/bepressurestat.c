/** vim: set sw=4 ts=4:
 * @file   bepressurestat.c
 * @date   2006-04-06
 * @author Adam M. Szalkowski
 *
 * Register Pressure Statistics
 *
 * Copyright (C) 2006 Universitaet Karlsruhe
 * Released under the GPL
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <math.h>

#include "hashptr.h"
#include "debug.h"
#include "obst.h"
#include "set.h"
#include "list.h"
#include "pmap.h"

#include "irprintf.h"
#include "irgwalk.h"
#include "irdump_t.h"
#include "irnode_t.h"
#include "ircons_t.h"
#include "irloop_t.h"
#include "phiclass.h"
#include "iredges.h"
#include "execfreq.h"

#include <libcore/lc_bitset.h>

#include "be_t.h"
#include "belive_t.h"
#include "besched_t.h"
#include "beirgmod.h"
#include "bearch.h"
#include "benode_t.h"
#include "beutil.h"
#include "bespillremat.h"
#include "bespill.h"

#include "bechordal_t.h"

#define MAXPRESSURE 128

typedef struct _regpressure_ana_t {
	const arch_register_class_t  *cls;
	const be_chordal_env_t       *chordal_env;
	unsigned int                 *stat;
	DEBUG_ONLY(firm_dbg_module_t * dbg);
} regpressure_ana_t;

static INLINE int
has_reg_class(const regpressure_ana_t * ra, const ir_node * irn)
{
	return chordal_has_class(ra->chordal_env, irn);
}

static INLINE int
regpressure(pset * live)
{
	int pressure = pset_count(live);

	return (pressure>MAXPRESSURE)?MAXPRESSURE:pressure;
}

static void
regpressureanawalker(ir_node * bb, void * data)
{
  regpressure_ana_t  *ra = data;
  pset               *live = pset_new_ptr_default();
  const ir_node      *irn;
  unsigned int       *stat = ra->stat;
  int                 i;

  be_lv_foreach(ra->chordal_env->lv, bb, be_lv_state_end, i) {
    ir_node *value = be_lv_get_irn(ra->chordal_env->lv, bb, i);
    if (has_reg_class(ra, value)) {
      pset_insert_ptr(live, value);
    }
  }
  stat[regpressure(live)]++;

  sched_foreach_reverse(bb, irn) {

    if(is_Phi(irn)) break;

    if(has_reg_class(ra, irn)) {
      pset_remove_ptr(live, irn);
    }

    for(i=get_irn_arity(irn)-1; i>=0; --i) {
      ir_node  *arg = get_irn_n(irn, i);

      if(has_reg_class(ra, arg)) {
		  pset_insert_ptr(live, arg);
      }
    }

    if(!is_Proj(irn)) stat[regpressure(live)]++;
  }
}

void
be_analyze_regpressure(const be_chordal_env_t * chordal_env, const char * suffix)
{
  regpressure_ana_t   ra;
  unsigned int        stat[MAXPRESSURE+1];
  unsigned int        i;
  char                fname[256];
  FILE               *f;

  ir_snprintf(fname, sizeof(fname), "%F_%s%s_pressure.stat", chordal_env->irg, chordal_env->cls->name, suffix);
  f = fopen(fname, "w");
  assert(f);

  FIRM_DBG_REGISTER(ra.dbg, "firm.be.regpressureana");

  ra.chordal_env = chordal_env;
  ra.cls = chordal_env->cls;
  ra.stat = stat;

  memset(stat, 0, sizeof(stat));

  irg_block_walk_graph(chordal_env->irg, regpressureanawalker, NULL, &ra);

  for(i=0; i<=MAXPRESSURE; ++i) {
    fprintf(f,"%d\n",stat[i]);
  }

  fclose(f);
}
