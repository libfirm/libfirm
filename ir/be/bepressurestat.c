/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

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
#include "bearch_t.h"
#include "benode_t.h"
#include "beutil.h"
#include "bespillremat.h"
#include "bespill.h"

#include "bechordal_t.h"

#define MAXPRESSURE 128

typedef struct _regpressure_ana_t {
	arch_env_t                   *arch_env;
	const arch_register_class_t  *cls;
	const be_lv_t                *lv;
	unsigned int                 *stat;
	DEBUG_ONLY(firm_dbg_module_t * dbg);
} regpressure_ana_t;

static INLINE int
has_reg_class(const regpressure_ana_t * ra, const ir_node * irn)
{
	return arch_irn_consider_in_reg_alloc(ra->arch_env, ra->cls, irn);
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
  regpressure_ana_t  *ra   = data;
  pset               *live = pset_new_ptr_default();
  const ir_node      *irn;
  unsigned int       *stat = ra->stat;
  int                i;
  const be_lv_t      *lv   = ra->lv;

  be_lv_foreach(lv, bb, be_lv_state_end, i) {
    ir_node *value = be_lv_get_irn(lv, bb, i);
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
be_analyze_regpressure(be_irg_t *birg, const arch_register_class_t *cls,
                       const char * suffix)
{
  regpressure_ana_t   ra;
  unsigned int        stat[MAXPRESSURE+1];
  unsigned int        i;
  char                fname[256];
  FILE               *f;
  ir_graph           *irg = be_get_birg_irg(birg);

  ir_snprintf(fname, sizeof(fname), "%F_%s%s_pressure.stat", irg, cls->name, suffix);
  f = fopen(fname, "w");
  assert(f);

  be_assure_liveness(birg);

  FIRM_DBG_REGISTER(ra.dbg, "firm.be.regpressureana");

  ra.arch_env = birg->main_env->arch_env;
  ra.lv = be_get_birg_liveness(birg);
  ra.cls = cls;
  ra.stat = stat;

  memset(stat, 0, sizeof(stat));

  irg_block_walk_graph(irg, regpressureanawalker, NULL, &ra);

  for(i=0; i<=MAXPRESSURE; ++i) {
    fprintf(f,"%d\n",stat[i]);
  }

  fclose(f);
}
