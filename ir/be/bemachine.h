#ifndef _BE_MACHINE_H_
#define _BE_MACHINE_H_

/**
 * Abstract machine interface.
 * @author Christian Wuerdig
 * @date   23.10.2006
 * @cvs-id $Id$
 */

typedef struct _be_execution_unit_type_t be_execution_unit_type_t;
typedef struct _be_execution_unit_t      be_execution_unit_t;
typedef struct _be_machine_t             be_machine_t;

struct _be_execution_unit_t {
	be_execution_unit_type_t *tp;
	const char               *name;
};

struct _be_execution_unit_type_t {
	unsigned            n_units;
	unsigned            ports_per_unit;
	const char          *name;
	be_execution_unit_t *units;
};

struct _be_machine_t {
	unsigned                 bundle_size;
	unsigned                 bundels_per_cycle;
	unsigned                 n_unit_types;
	be_execution_unit_type_t *unit_types;
};

/**
 * Get the number of available unit types in the given machine.
 */
#define be_machine_get_n_unit_types(machine) ((machine)->n_unit_types)

/**
 * Get the unit type number @p i from the given machine.
 */
#define be_machine_get_unit_type(machine, i) ((machine)->unit_types[(i)])

/**
 * Get the name of the given unit type.
 */
#define be_machine_get_unit_type_name(tp) ((tp)->name)

/**
 * Get the number of available execution units from the given unit type.
 */
#define be_machine_get_n_execunits(tp) ((tp)->n_units)

/**
 * Get the execution unit number @p i from the given unit type.
 */
#define be_machine_get_execunit(tp, i) ((tp)->units[(i)])

/**
 * Get the name of the given execution unit.
 */
#define be_machine_get_execunit_name(unit) ((unit)->name)

/**
 * Get the unit type of the given execution unit.
 */
#define be_machine_get_execunit_type(unit) ((unit)->tp)

#endif /* _BE_MACHINE_H_ */
