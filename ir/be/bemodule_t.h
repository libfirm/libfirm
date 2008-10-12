/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief       Backend module interface.
 * @author      Matthias Braun
 * @date        11.12.2006
 * @version     $Id$
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
