#ifndef _EXT_GRS_ANALYZE_H_
#define _EXT_GRS_ANALYZE_H_

#include "common.h"

typedef struct _ext_grs_analyzer_t ext_grs_analyzer_t;



/** initialize an analyzer */
void ext_grs_init_analyzer(ext_grs_analyzer_t *alz);

/** perform a from scratch analysis of a given ir graph */
void ext_grs_analyze(ext_grs_analyzer_t *alz, ir_graph *irg);

/** free the result of the last analysis done on the given
 *  ir graph by the given analyzer */
void ext_grs_free_ana_result(ext_grs_analyzer_t *alz, ir_graph *irg);



/** enable incremental analysis for the given ir graph */
void ext_grs_enable_incr_ana(ext_grs_analyzer_t *alz, ir_graph *irg);

/** disable incremental analysis for the given ir graph */
void ext_grs_disable_incr_ana(ext_grs_analyzer_t *alz, ir_graph *irg);

/** dump the current analysis result for a given ir graph */
void ext_grs_dump_ana_result(ext_grs_analyzer_t *alz, ir_graph *irg);




/** from scratch analysis of all ir graphs building a global data set */
void ext_grs_analyze_global(ext_grs_analyzer_t *alz);

/** enable global incremental analysis building a global data set */
void ext_grs_enable_global_incr_ana(ext_grs_analyzer_t *alz);

/** disable global incremental analysis building a global data set */
void ext_grs_disable_global_incr_ana(ext_grs_analyzer_t *alz);

/** dump the current global analysis result */
void ext_grs_dump_global_ana_result(ext_grs_analyzer_t *alz);




#endif /*_EXT_GRS_ANALYZE_H_*/
