/**
 * Base routines for register allocation.
 * @author Sebastian Hack
 * @date 22.11.2004
 */

#include "pset.h"
#include "impl.h"

#include "irnode.h"
#include "irmode.h"
#include "besched.h"

#include "bera_t.h"

FIRM_IMPL1(is_allocatable_irn, int, const ir_node *)

size_t ra_irn_data_offset = 0;
size_t ra_irg_data_offset = 0;

void be_ra_init(void)
{
	ra_irn_data_offset = register_additional_node_data(sizeof(ra_info_t));
	ra_irg_data_offset = register_additional_graph_data(sizeof(void *));
}
