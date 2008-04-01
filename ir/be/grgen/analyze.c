/*
 * Project:     libFIRM/extension module/graph rewriting system
 * File name:   ext/grs/analyze.c
 * Purpose:     provides interfaces for incremental and from scratch
 * 				analysis collection information for a search planer
 * Author:      Veit Batz
 * Created:		22. June 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


 /**
  * @file ext/grs/analyze.c
  * @brief	provides an interface for incremental and from scratch
  *			analyses collecting information for a search planer
  * */

#include "base_t.h"
#include "analyze_t.h"


/** perform a from scratch analysis of a given ir graph */
void ext_grs_analyze(ext_grs_analyzer_t *alz, ir_graph *irg)
{
	ext_grs_irg_private_t *pr_g = _ext_grs_get_irg_private(irg);
	if (pr_g->matching_enabled) alz->analyze(alz, irg);
}

void ext_grs_free_ana_result(ext_grs_analyzer_t *alz, ir_graph *irg)
{
	alz->free_ana_result(alz, irg);
}


/** enable incremantal analysis for the given ir graph */
void ext_grs_enable_incr_ana(ext_grs_analyzer_t *alz, ir_graph *irg)
{
	ext_grs_irg_private_t *pr_g = _ext_grs_get_irg_private(irg);
	if (pr_g->matching_enabled) alz->enable_incr_ana(alz, irg);
}

/** disable incremantal analysis for the given ir graph */
void ext_grs_disable_incr_ana(ext_grs_analyzer_t *alz, ir_graph *irg)
{
	alz->disable_incr_ana(alz, irg);
}

/** dump the current analysis result for a given ir graph */
void ext_grs_dump_ana_result(ext_grs_analyzer_t *alz, ir_graph *irg)
{
	alz->dump_ana_result(alz, irg);
}





/** from scratch analysis of all ir graphs building a global data set */
void ext_grs_analyze_global(ext_grs_analyzer_t *alz)
{
	alz->analyze_global(alz);
}

/** enable global incremantal analysis building a global data set */
void ext_grs_enable_global_incr_ana(ext_grs_analyzer_t *alz)
{
	alz->enable_global_incr_ana(alz);
}

/** disable global incremantal analysis building a global data set */
void ext_grs_disable_global_incr_ana(ext_grs_analyzer_t *alz)
{
	alz->disable_global_incr_ana(alz);
}

/** dump the current global analysis result */
void ext_grs_dump_global_ana_result(ext_grs_analyzer_t *alz)
{
	alz->dump_global_ana_result(alz);
}
