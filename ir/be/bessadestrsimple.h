#include "pset.h"

/**
 * Insert copys to destruct SSA form and
 * define variables (numbers) for all SSA-values.
 * All values in a phi class get assigned the same variable name.
 * The link field maps values to the var-name
 *
 * @return A set mapping SSA values to variables
 */
set *be_ssa_destr_simple(ir_graph *irg, const arch_env_t *aenv);

void free_ssa_destr_simple(set *vars);

/**
 * This struct maps a variable (nr) to the values belonging to this variable
 */
typedef struct _be_var_info_t {
	int var_nr;		/* the key */
	pset *values;	/* the ssa-values belonging to this variable */
} be_var_info_t;

/**
 * The link field of an irn points to the var_info struct
 * representing the corresponding variable.
 */
#define be_get_var_info(irn)				((be_var_info_t *)get_irn_link(irn))

#define SET_REMOVED -1

pset *be_get_var_values(set *vals, int var_nr);
be_var_info_t *be_var_add_value(set *vars, int var_nr, ir_node *irn);
be_var_info_t *be_var_find_or_insert(set *vars, int var_nr);
be_var_info_t *be_var_find(set *vars, int var_nr);
