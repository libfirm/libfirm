/*
 * Author:      Matthias Braun
 * Date:		11.12.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef BEMODULES_H_
#define BEMODULES_H_

typedef void (*be_module_constructor_func)(void);

/**
 * Use this macro to register a module constructor. You should put this macro in
 * a .c file of your module. Compiler magic will make sure that your constructor
 * function gets called after the main backend structures are initialized.
 *
 * The module constructor is a convenient place to register commandline options,
 * or add the module to extension lists.
 */
#define BE_REGISTER_MODULE_CONSTRUCTOR(func) \
	static void __attribute__((constructor)) _be_constructor(void) \
	{                                                              \
		be_module_add_constructor(func);                           \
	}

// TODO msvc version

/**
 * Warning: internal function, use BE_REGISTER_MODULE_CONSTRUCTOR instead.
 *
 * This function registers the constructor function of a backend module
 */
void be_module_add_constructor(be_module_constructor_func func);

/**
 * Call all registered constructor functions
 */
void be_module_call_constructors(void);

#endif
