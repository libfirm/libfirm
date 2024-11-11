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
#ifndef FIRM_BE_BEMODULE_H
#define FIRM_BE_BEMODULE_H

/**
 * Mark a function as module constructor.
 * Currently you have to add modules manually in the list in bemodule.c.
 * However future extensions might allow them to be automatically discovered
 * when they are marked with BE_REGISTER_MODULE_CONSTRUCTOR
 *
 * Add this before your constructor as it will declare the function
 */
#define BE_REGISTER_MODULE_CONSTRUCTOR(func)         void func(void);

/**
 * Mark a function as module destructor.
 *
 * Add this before your constructor as it will declare the function
 */
#define BE_REGISTER_MODULE_DESTRUCTOR(func)          void func(void);

/**
 * Call all module constructors
 */
void be_init_modules(void);

/**
 * Call all module destructors
 */
void be_quit_modules(void);

//---------------------------------------------------------------------------

#include "lc_opts.h"

typedef struct be_module_list_entry_t be_module_list_entry_t;

void be_add_module_to_list(be_module_list_entry_t **list_head, const char *name,
                           void *module);

void be_add_module_list_opt(lc_opt_entry_t *grp, const char *name,
                            const char *description,
                            be_module_list_entry_t * const * first,
                            void **var);

#endif
