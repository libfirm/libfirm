#ifndef _EXT_GRS_BASE_H_
#define _EXT_GRS_BASE_H_



#include "common.h"


/**
 * Activates the /ext/grs module.
 * Must be called once \b before init_firm()
 * if the ext/grs module of Firm should be used.
 *
 * @remark Call this function \b before init_firm()
 */
void ext_grs_activate(void);

/** Finalize the /ext/grs module. */
void ext_grs_finalize(void);

/**
 * get an ir_op by its name
 */
ir_op *ext_grs_lookup_op(char *op_name);

/**
 * get an ir_mode by its name
 * */
ir_mode *ext_grs_lookup_mode(char *name);

/**
 * announce that firm op o1 inherits from firm op o2
 */
void ext_grs_appoint_heir(ir_op *o1, ir_op *o2);

/**
 * Setup internal inheritance related data strucutres.
 * @note	must be called after one or more invocations of
 * 			ext_grs_appoint_heir()
 */
void ext_grs_inheritance_mature(void);

/**
 *  Enables subgraph matching for a given ir graph.\
 */

void ext_grs_enable_irg(ir_graph *irg);
/**
 *  Disables subgraph matching for a given irg, several internal
 *  data structures will be freed (needed if there is not enough
 *  memory to keep the data for all present ir graphs simultaneously.
 */
void ext_grs_disable_irg(ir_graph *irg);


#endif /* _EXT_GRS_BASE_H_ */
