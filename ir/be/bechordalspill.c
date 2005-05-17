/**
 * Spilling for chordal interference graphs.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "obst.h"
#include "list.h"
#include "bitset.h"
#include "iterator.h"

#include "irmode_t.h"
#include "irgraph_t.h"
#include "irprintf_t.h"
#include "irgwalk.h"
#include "irdump.h"
#include "irdom.h"
#include "debug.h"
#include "xmalloc.h"

#include "beutil.h"
#include "besched.h"
#include "benumb_t.h"
#include "besched_t.h"
#include "belive_t.h"
#include "bechordal_t.h"

void be_ra_chordal_spill(be_chordal_env_t *env)
{
}
