/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       Abstract machine interface.
 * @author      Christian Wuerdig
 * @date        01.12.2006
 * @version     $Id$
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
