/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Backend module interface.
 * @author      Matthias Braun
 * @date        11.12.2006
 */
#ifndef FIRM_BE_BEMODULE_T_H
#define FIRM_BE_BEMODULE_T_H

#include "bemodule.h"

/**
 * A module list entry.
 */
struct be_module_list_entry_t {
	const char *name;                    /**< The name of the entry. */
	void *data;                          /**< Some data associated with this entry. */
	struct be_module_list_entry_t *next; /**< Points to the next entry. */
};

#endif /* FIRM_BE_BEMODULE_T_H */
