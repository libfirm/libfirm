/*
 * Author:      Matthias Braun
 * Date:		11.12.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef BEMODULE_T_H_
#define BEMODULE_T_H_

#include "bemodule.h"

struct be_module_list_entry_t {
	const char *name;
	void *data;
	struct be_module_list_entry_t *next;
};

#endif
