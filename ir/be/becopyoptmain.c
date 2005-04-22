/**
 * Main file for the optimization reducing the copies needed for:
 * - phi coalescing
 * - register-constrained nodes
 *
 * @author Daniel Grund
 * @date 11.04.2005
 */

#include "becopyopt.h"
#include "becopystat.h"
#include "becopyoptmain.h"

#define DO_HEUR
#define DO_ILP

#define DEBUG_LVL SET_LEVEL_1
static firm_dbg_module_t *dbg = NULL;

void be_copy_opt_init(void) {
	dbg = firm_dbg_register("ir.be.copyopt");
	firm_dbg_set_mask(dbg, DEBUG_LVL);
}

void be_copy_opt(ir_graph* irg, const arch_isa_if_t *isa, const arch_register_class_t *cls) {
	copy_opt_t *co;
	int lb, copies;

	DBG((dbg, LEVEL_1, "\nIRG: %s\n\n", get_entity_name(get_irg_entity(irg))));
	co = new_copy_opt(irg, isa, cls);
	co_check_allocation(co);

#ifdef DO_STAT
	copies = co_get_copy_count(co);
	curr_vals[I_COPIES_INIT] += copies;
	DBG((dbg, 1, "Init copies: %3d\n", copies));
#endif

#ifdef DO_HEUR
	co_heur_opt(co);
	co_check_allocation(co);
#ifdef DO_STAT
	copies = co_get_copy_count(co);
	curr_vals[I_COPIES_HEUR] += copies;
	DBG((dbg, 1, "Heur copies: %3d\n", copies));
#endif
#endif

#ifdef DO_ILP
	lb = co_get_lower_bound(co);
	copies = co_get_copy_count(co);
//TODO remove checks and enable lb
	assert(copies>=lb && "At least one computation of these two is boooogy");
//	if (copies > lb) {
		co_ilp_opt(co);
		co_check_allocation(co);
//	}
	copies = co_get_copy_count(co);
	assert(copies>=lb && "At least one computation of these two is boooogy");

#ifdef DO_STAT
	copies = co_get_copy_count(co);
	curr_vals[I_COPIES_HEUR] += copies;
	DBG((dbg, 1, "Opt  copies: %3d\n", copies));
#endif
#endif

	free_copy_opt(co);
}
