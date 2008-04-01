/*
 * Project:     libFIRM/extension module/graph rewriting system
 * File name:   ext/grs/analyze.h
 * Purpose:     provides an interface for incremental and from scratch
 * 				analysis for the collection of information needed by
 * 				apropriate search planers
 * Author:      Veit Batz
 * Created:		22. June 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


/**
 * @file ext/grs/analyze.h
 * @brief	provides an interface for incremental and from scratch
 *			analysis for the collection of information needed by
 *			apropriate search planers
 * */

#ifndef _EXT_GRS_ANALYZE_T_H_
#define _EXT_GRS_ANALYZE_T_H_


#include "common_t.h"
#include "analyze.h"




/** interface of an ir graph analyzer */
struct _ext_grs_analyzer_t
{
	/** a tag identifying an analyzer, to be checked by search planers */
	const char *tag;

	/** perform a from scratch analysis of a given ir graph */
	void (*analyze) (struct _ext_grs_analyzer_t *, ir_graph *);
	/** free the result of the last analysis done on the given
	 *  ir graph by this analyzer */
	void (*free_ana_result) (ext_grs_analyzer_t *, ir_graph *);
	/** enable incremantal analysis for the given ir graph */
	void (*enable_incr_ana) (struct _ext_grs_analyzer_t *, ir_graph *);
	/** disable incremantal analysis for the given ir graph */
	void (*disable_incr_ana) (struct _ext_grs_analyzer_t *, ir_graph *);
	/** dump the current analysis result */
	void (*dump_ana_result) (struct _ext_grs_analyzer_t *, ir_graph *);

	/** from scratch analysis of all ir graphs building a global data set */
	void (*analyze_global) (struct _ext_grs_analyzer_t *);
	/** enable global incremantal analysis building a global data set */
	void (*enable_global_incr_ana) (struct _ext_grs_analyzer_t *);
	/** disable global incremantal analysis building a global data set */
	void (*disable_global_incr_ana) (struct _ext_grs_analyzer_t *);
	/** dump the current global analysis result */
	void (*dump_global_ana_result) (struct _ext_grs_analyzer_t *);

	/** this analyzers private data storage area */
	void *data;
};





#endif /*_EXT_GRS_ANALYZE_T_H_*/
