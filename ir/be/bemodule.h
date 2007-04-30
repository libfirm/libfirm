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
 * @brief       Backend module interface.
 * @author      Matthias Braun
 * @date        11.12.2006
 * @version     $Id$
 */
#ifndef FIRM_BE_BEMODULE_H
#define FIRM_BE_BEMODULE_H

/**
 * Mark a function as module constructor.
 * Currently you have to add modules manually in the list in bemodule.c.
 * However future extensions might allow them to be automatically discovered
 * when they are marked with BE_REGISTER_MODULE_CONSTRUCTOR
 */
#define BE_REGISTER_MODULE_CONSTRUCTOR(func)

/**
 * Mark a function as module destructor.
 */
#define BE_REGISTER_MODULE_DESTRUCTOR(func)

/**
 * Call all module constructors
 */
void be_init_modules(void);

/**
 * Call all module destructors
 */
void be_quit_modules(void);

//---------------------------------------------------------------------------

#include <libcore/lc_opts.h>

typedef struct be_module_list_entry_t be_module_list_entry_t;

void be_add_module_to_list(be_module_list_entry_t **list_head, const char *name,
                           void *module);

void be_add_module_list_opt(lc_opt_entry_t *grp, const char *name,
                            const char *description,
                            be_module_list_entry_t * const * first,
                            void **var);

#endif /* FIRM_BE_BEMODULE_H */
