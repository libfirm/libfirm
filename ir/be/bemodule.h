/*
 * Author:      Matthias Braun
 * Date:		11.12.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef BEMODULE_H_
#define BEMODULE_H_

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

#endif
