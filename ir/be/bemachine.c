/**
 * Abstract machine interface.
 * @author Christian Wuerdig
 * @date   01.12.2006
 * @cvs-id $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "bemachine.h"

/* We define a generic dummy unit */
be_execution_unit_t be_machine_execution_units_DUMMY[1];

be_execution_unit_type_t be_machine_execution_unit_types[] = {
	{ 1, 1, "DUMMY", be_machine_execution_units_DUMMY },
};

/**
 * Initialize generic dummy unit.
 */
void be_machine_init_dummy_unit(void) {
	be_machine_execution_units_DUMMY[0].name = "GENERIC_DUMMY_UNIT";
	be_machine_execution_units_DUMMY[0].tp   = &be_machine_execution_unit_types[0];
}

/**
 * Returns the generic dummy unit.
 */
be_execution_unit_t *be_machine_get_dummy_unit(void) {
	return &be_machine_execution_units_DUMMY[0];
}

/**
 * Check if given unit is the generic dummy unit.
 */
int be_machine_is_dummy_unit(const be_execution_unit_t *unit) {
	return unit == &be_machine_execution_units_DUMMY[0];
}

/**
 * Check if given unit is the generic dummy unit type.
 */
int be_machine_is_dummy_unit_type(const be_execution_unit_type_t *tp) {
	return tp == &be_machine_execution_unit_types[0];
}
