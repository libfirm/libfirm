/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Debug facility.
 * @author  Michael Beck, Sebastian Hack
 * @date    15.12.2004
 */
#ifndef FIRM_COMMON_DEBUG_H
#define FIRM_COMMON_DEBUG_H

/* WITH DEBUG OUTPUT */
#ifdef DEBUG_libfirm

#include <stdio.h>

enum firm_dbg_level_t {
	LEVEL_DEFAULT = 0, /**< Prints always. Use with DBG(). */
	LEVEL_1 = 1,
	LEVEL_2 = 2,
	LEVEL_3 = 4,
	LEVEL_4 = 8,
	LEVEL_5 = 16,

	SET_LEVEL_0 = 0,   /**< use with firm_dbg_set_mask(). */
	SET_LEVEL_1 = 1,
	SET_LEVEL_2 = 3,
	SET_LEVEL_3 = 7,
	SET_LEVEL_4 = 15,
	SET_LEVEL_5 = 31,
	SET_LEVEL_ALL = SET_LEVEL_5
};

typedef struct firm_dbg_module_t firm_dbg_module_t;

/* Internal function to the debug module. */
void *_firm_dbg_make_msg(const firm_dbg_module_t *mod, unsigned mask, const char *fmt, ...);

/* Internal function to the debug module. */
void _firm_dbg_print_msg(const char *filename, int line, const char *func, void *data);

/* Internal function to the debug module. */
void _firm_dbg_print(const firm_dbg_module_t *mod, unsigned mask, const char *fmt, ...);

/**
 * Register a module to the firm debug facility.
 * If the module has already been registered, no new module is allocated
 * but the handle is returned. By default, all messages go to @c stderr
 * and the debug mask is set to 0, i.e. the module is muted.
 * @param name The name of the module to register.
 * @return The module handle.
 */
firm_dbg_module_t *firm_dbg_register(const char *name);

/**
 * Set the mask of a module.
 * @param module The module.
 * @param mask The new mask for the module.
 */
void firm_dbg_set_mask(firm_dbg_module_t *module, unsigned mask);

/**
 * Get the mask of a module.
 * @param module The module handle.
 * @return The mask currently used by the module.
 */
unsigned firm_dbg_get_mask(const firm_dbg_module_t *module);

/**
 * Set the output file of a module.
 * @param module The module handle.
 * @param file The new file to use by this handle.
 */
void firm_dbg_set_file(firm_dbg_module_t *module, FILE *file);

/**
 * Issue a debug message.
 * @param args The arguments.
 *
 * The arguments is a list surrounded by parentheses. The items
 * of the list are:
 * - The module handle as returned by firm_dbg_register().
 * - The debug mask that you want associate with this message.
 * - A format string for the message to pass to ir_printf().
 * - Further optional arguments are passed to ir_printf().
 *
 * The mask is anded against the module's mask. If both have some bits
 * in common, the message is issued. If the given mask is 0, the message
 * is always dumped regardless of the module's mask. You can also use
 * the mask in a level based manner, see firm_dbg_level_t.
 *
 * Here is an example:
 * @code
 * DBG((my_mod, MASK_ERR, "ir node %n is not green", node))
 * ...
 * DBG((my_mod, LEVEL_DEFAULT, "entity %e has type %t", ent, type))
 * @endcode
 */
#define DBG(args) _firm_dbg_print_msg(__FILE__, __LINE__, __func__, _firm_dbg_make_msg args)
#define DB(args)  _firm_dbg_print args

/** create a debug handle in debug mode */
#define FIRM_DBG_REGISTER(handle, name) handle = firm_dbg_register(name)
#define DEBUG_ONLY(code)   code
#define RELEASE_ONLY(code)

#else /* ndef DEBUG_libfirm */

/* DEBUG OUTPUT IS COMPLETELY DISABLED */

#define DBG(x)   (void)0
#define DB(x)    (void)0

/** create a debug handle in release mode */
#define FIRM_DBG_REGISTER(handle, name)  (void)0
#define DEBUG_ONLY(code)
#define RELEASE_ONLY(code) code

#define firm_dbg_set_mask(module, mask)  (void)0
#define firm_dbg_get_mask(module)        (void)0
#define firm_dbg_set_file(module, file)  (void)0

#endif /* DEBUG_libfirm */

#endif
