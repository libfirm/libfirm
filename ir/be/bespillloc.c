/**
 * spill location scheduling
 * @author Sven Polk
 * @date 1.4.2006
 */

#include "obst.h"
#include "set.h"
#include "pset.h"
#include "irprintf_t.h"
#include "irgraph.h"
#include "irnode.h"
#include "irmode.h"
#include "irgwalk.h"
#include "iredges_t.h"
#include "ircons_t.h"
#include "irprintf.h"


#include <stdio.h>
#include <stdlib.h>
#include "bitset.h"

#include "irprog.h"
#include "irgopt.h"
#include "irdump.h"
#include "phiclass.h"
#include "irdom_t.h"
#include "iredges_t.h"
#include "irloop_t.h"
#include "irtools.h"
#include "return.h"

#include "bearch.h"
#include "firm/bearch_firm.h"
#include "ia32/bearch_ia32.h"
#include "arm/bearch_arm.h"
#include "ppc32/bearch_ppc32.h"
#include "mips/bearch_mips.h"

#include "be_t.h"
#include "benumb_t.h"
#include "beutil.h"
#include "benode_t.h"
#include "beirgmod.h"
#include "besched_t.h"
#include "belistsched.h"
#include "belive_t.h"
#include "bespillilp.h"
#include "bespillbelady.h"
#include "bera.h"
#include "beraextern.h"
#include "bechordal_t.h"
#include "beifg.h"
#include "beifg_impl.h"
#include "becopyopt.h"
#include "becopystat.h"
#include "bessadestr.h"
#include "beabi.h"
#include "belower.h"
#include "bestat.h"

#include "typewalk.h"


typedef struct _spilloc_env_t{

	struct obstack ob;
	ir_graph *irg;

	pset *adjlst;
	spill_env_t *senv;

} spilloc_env_t;


typedef struct Edge {
	int nodenumber;
	entity *ent;
	struct Edge *next;
} edge;

typedef struct {int count; edge *link;} vertex;
typedef struct {int wt_vif; int wt_cif;} wheights;


void get_them_all()
{
	// create new elements in graph representing data structure

}

void be_spill_loc(const be_chordal_env_t *chordal_env)
{

	spilloc_env_t spi;

	obstack_init(&spi.ob);

	// create initial in graph representing data structure

	walk_types_entities(get_irg_frame_type(chordal_env->irg), &get_them_all , &chordal_env);

	get_irg_entity(chordal_env->irg);
	get_irg_frame_type(chordal_env->irg);

	obstack_free(&spi.ob, NULL);
}
