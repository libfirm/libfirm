#ifndef _EXT_GRS_COMMON_T_H_
#define _EXT_GRS_COMMON_T_H_



#include <libcore/lc_pset.h>
#include <libcore/lc_list.h>

#ifdef HAVE_CONFIG_H
	#include "config.h"
#endif

#include "irgraph_t.h"
#include "irop_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "iredges_t.h"




typedef enum {
	ext_grs_in = 0,
	ext_grs_out = 1
} ext_grs_direction_t;







#endif /* _EXT_GRS_COMMON_T_H_ */
