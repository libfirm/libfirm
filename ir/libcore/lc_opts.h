/*
 * This file is part of libFirm.
 * Copyright (C) 2012 IPD Goos, Universit"at Karlsruhe, Germany
 */

/*
   Option management library.
   This module can read (typed) options from a config file or
   parse a command line. The options are managed in a tree structure.
*/

#ifndef _LC_OPTS_H
#define _LC_OPTS_H

#include <stdio.h>
#include <stdbool.h>

/**
 * The type of an option.
 */
typedef enum {
	lc_opt_type_invalid,
	lc_opt_type_enum,
	lc_opt_type_bit,
	lc_opt_type_boolean,
	lc_opt_type_string,
	lc_opt_type_int,
	lc_opt_type_double
} lc_opt_type_t;

typedef struct lc_opt_entry_t lc_opt_entry_t;

typedef bool (lc_opt_callback_t)(void *data, size_t length, char const *value);

typedef int (lc_opt_dump_t)(char *buf, size_t n, void *data);

typedef int (lc_opt_dump_vals_t)(char *buf, size_t n, void *data);

typedef struct {
	const char *name;               /**< The name of the option. */
	const char *desc;               /**< A description for the option. */
	lc_opt_type_t type;             /**< The type of the option (see enum). */
	void *value;                    /**< A pointer to the area, where the value
	                                     of the option shall be put to. May be
	                                     NULL. */

	size_t len;                     /**< The amount of bytes available at the
	                                     location value points to. */
	lc_opt_callback_t *cb;          /**< A callback that is called, when the
	                                     option is set. This may never be NULL. */

	lc_opt_dump_t *dump;            /**< A function which is able to format the
	                                     options value into a string. May be
	                                     NULL. */

	lc_opt_dump_vals_t *dump_vals;  /**< A function which is able to format the possible values
	                                     for this option into a string. May be NULL. */


} lc_opt_table_entry_t;

#define _LC_OPT_ENT(name, desc, type, val_type, value, len, cb, dump, dump_vals) \
	{ name, desc, type, 1 ? (value) : (val_type*)0 /* Produces a warning, if var has wrong type. */, len, cb, dump, dump_vals }

#define LC_OPT_ENT_INT(name, desc, addr) \
	_LC_OPT_ENT(name, desc, lc_opt_type_int, int, addr, 0, lc_opt_int_cb, lc_opt_int_dump, NULL)

#define LC_OPT_ENT_DBL(name, desc, addr) \
	_LC_OPT_ENT(name, desc, lc_opt_type_double, double, addr, 0, lc_opt_double_cb, lc_opt_double_dump, NULL)

#define LC_OPT_ENT_BIT(name, desc, addr, mask) \
	_LC_OPT_ENT(name, desc, lc_opt_type_bit, unsigned, addr, mask, lc_opt_bit_cb, lc_opt_bit_dump, NULL)

#define LC_OPT_ENT_BOOL(name, desc, addr) \
	_LC_OPT_ENT(name, desc, lc_opt_type_boolean, bool, addr, 0, lc_opt_bool_cb, lc_opt_bool_dump, lc_opt_bool_dump_vals)

typedef char lc_opt_str_t[];
#define LC_OPT_ENT_STR(name, desc, buf) \
	_LC_OPT_ENT(name, desc, lc_opt_type_string, lc_opt_str_t, buf, sizeof(*buf), lc_opt_string_cb, lc_opt_string_dump, NULL)

#define LC_OPT_LAST \
	_LC_OPT_ENT(NULL, NULL, lc_opt_type_invalid, void, NULL, 0, NULL, NULL, NULL)

/**
 * Get the root option group.
 * @return The root option group.
 */
lc_opt_entry_t *lc_opt_root_grp(void);

/**
 * Get an option group.
 * If the group is not already present, it is created.
 * @param parent   The parent group to look in.
 * @param name     The name of the group to lookup
 * @return         The already present or created group.
 */
lc_opt_entry_t *lc_opt_get_grp(lc_opt_entry_t *parent, const char *name);

/**
 * Add an option to a group.
 * @param grp       The group to add the option to.
 * @param name      The name of the option (must be unique inside the group).
 * @param desc      A description of the option.
 * @param type      The data type of the option (see lc_opt_type_*)
 * @param value     A pointer to the memory, where the value shall be stored.
 *                  (May be NULL).
 * @param length    Amount of bytes available at the memory location
 *                  indicated by @p value.
 * @param cb        A callback function to be called, as the option's value
 *                  is set (may be NULL).
 * @param err       Error information to be set (may be NULL).
 * @return          The handle for the option.
 */
lc_opt_entry_t *lc_opt_add_opt(lc_opt_entry_t *grp,
                               const char *name,
                               const char *desc,
                               lc_opt_type_t type,
                               void *value, size_t length,
                               lc_opt_callback_t *cb,
                               lc_opt_dump_t *dump,
                               lc_opt_dump_vals_t *dump_vals);

lc_opt_callback_t lc_opt_bit_cb;
lc_opt_callback_t lc_opt_bool_cb;
lc_opt_callback_t lc_opt_double_cb;
lc_opt_callback_t lc_opt_int_cb;
lc_opt_callback_t lc_opt_string_cb;

lc_opt_dump_t lc_opt_bit_dump;
lc_opt_dump_t lc_opt_bool_dump;
lc_opt_dump_t lc_opt_double_dump;
lc_opt_dump_t lc_opt_int_dump;
lc_opt_dump_t lc_opt_string_dump;

lc_opt_dump_vals_t lc_opt_bool_dump_vals;


/**
 * Find a group inside another group.
 * @param grp   The group to search inside.
 * @param name  The name of the group you are looking for.
 * @return      The group or NULL, if no such group can be found.
 */
lc_opt_entry_t *lc_opt_find_grp(const lc_opt_entry_t *grp, const char *name);

/**
 * Find an option inside another group.
 * @param grp   The group to search inside.
 * @param name  The name of the option you are looking for.
 * @return      The group or NULL, if no such option can be found.
 */
lc_opt_entry_t *lc_opt_find_opt(const lc_opt_entry_t *grp, const char *name);

/**
 * Resolve a group.
 * @param root   The group to start resolving from.
 * @param names  A string array containing the path to the group.
 * @param n      Number of entries in @p names to consider.
 * @return       The group or NULL, if none is found.
 */
lc_opt_entry_t *lc_opt_resolve_grp(const lc_opt_entry_t *root,
                                   const char * const *names, int n);

/**
 * Resolve an option.
 * @param root   The group to start resolving from.
 * @param names  A string array containing the path to the option.
 * @param n      Number of entries in @p names to consider.
 * @return       The option or NULL, if none is found.
 */
lc_opt_entry_t *lc_opt_resolve_opt(const lc_opt_entry_t *root,
                                   const char * const *names, int n);

/**
 * Get the name of the type of an option.
 * @param ent The option.
 * @return The name of the type of the option.
 */
const char *lc_opt_get_type_name(const lc_opt_entry_t *ent);

/**
 * Print the help screen for the given entity to the given file.
 * Use separator instead of '.' and ignore entities above ent,
 * i.e. if ent is root.be and has option isa.mach, prints
 * isa<separator>mach instead of root.be.isa.mach
 */
void lc_opt_print_help_for_entry(lc_opt_entry_t *ent, char separator, FILE *f);

bool lc_opt_add_table(lc_opt_entry_t *grp, const lc_opt_table_entry_t *table);

/**
 * Set options from a single (command line) argument.
 * @param root          The root group we start resolving from.
 * @param arg           The command line argument itself.
 * @return              1, if the argument was set, 0 if not.
 */
int lc_opt_from_single_arg(const lc_opt_entry_t *grp, const char *arg);

#endif
