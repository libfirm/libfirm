/*
  libcore: library for basic data structures and algorithms.
  Copyright (C) 2005  IPD Goos, Universit"at Karlsruhe, Germany

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/*
   Option management library.
   This module can read (typed) options from a config file or
   parse a command line. The options are managed in a tree structure.
*/

#ifndef _LC_OPTS_H
#define _LC_OPTS_H

#include <stdio.h>

#include <libcore/lc_printf.h>

/**
 * The type of an option.
 */
typedef enum {
	lc_opt_type_enum,
	lc_opt_type_bit,
	lc_opt_type_negbit,
	lc_opt_type_boolean,
	lc_opt_type_negboolean,
	lc_opt_type_string,
	lc_opt_type_int,
	lc_opt_type_double
} lc_opt_type_t;

/**
 * Error codes.
 */
typedef enum {
	lc_opt_err_none = 0,
	lc_opt_err_no_callback,
	lc_opt_err_illegal_option_type,
	lc_opt_err_illegal_format,
	lc_opt_err_grp_not_found,
	lc_opt_err_opt_not_found,
	lc_opt_err_grp_expected,
	lc_opt_err_opt_already_there,
	lc_opt_err_file_not_found,
	lc_opt_err_unknown_value
} lc_opt_err_t;

typedef struct {
	int error;
	const char *msg;
	const char *arg;
} lc_opt_err_info_t;

#define lc_opt_is_error(err) ((err)->error != lc_opt_err_none)

typedef struct _lc_opt_entry_t lc_opt_entry_t;

typedef int (lc_opt_callback_t)(const char *name, lc_opt_type_t type, void *data, size_t length, ...);

typedef int (lc_opt_dump_t)(char *buf, size_t n, const char *name, lc_opt_type_t type, void *data, size_t length);

typedef int (lc_opt_dump_vals_t)(char *buf, size_t n, const char *name, lc_opt_type_t type, void *data, size_t length);

typedef int (lc_opt_error_handler_t)(const char *prefix, const lc_opt_err_info_t *err);

typedef struct {
	const char *name;				/**< The name of the option. */
	const char *desc;				/**< A description for the option. */
	lc_opt_type_t type;				/**< The type of the option (see enum). */
	void *value;					/**< A pointer to the area, where the value
								 		of the option shall be put to. May be NULL. */

	size_t len;						/**< The amount of bytes available at the
								 		location value points to. */
	lc_opt_callback_t *cb;			/**< A callback that is called, when the option is set.
							     		This may never be NULL. */

	lc_opt_dump_t *dump;			/**< A function which is able to format the options value
							     		into a string. May be NULL. */

	lc_opt_dump_vals_t *dump_vals;	/**< A function which is able to format the possible values
									  for this option into a string. May be NULL. */


} lc_opt_table_entry_t;

#define _LC_OPT_ENT(name, desc, type, value, len, cb, dump, dump_vals) \
	{ name, desc, type, value, len, cb, dump, dump_vals }

#define LC_OPT_ENT_INT(name, desc, addr) \
	_LC_OPT_ENT(name, desc, lc_opt_type_int, addr, 0, lc_opt_std_cb, lc_opt_std_dump, NULL)

#define LC_OPT_ENT_DBL(name, desc, addr) \
	_LC_OPT_ENT(name, desc, lc_opt_type_double, addr, 0, lc_opt_std_cb, lc_opt_std_dump, NULL)

#define LC_OPT_ENT_BIT(name, desc, addr, mask) \
	_LC_OPT_ENT(name, desc, lc_opt_type_bit, addr, mask, lc_opt_std_cb, lc_opt_std_dump, NULL)

#define LC_OPT_ENT_NEGBIT(name, desc, addr, mask) \
	_LC_OPT_ENT(name, desc, lc_opt_type_negbit, addr, mask, lc_opt_std_cb, lc_opt_std_dump, NULL)

#define LC_OPT_ENT_BOOL(name, desc, addr) \
	_LC_OPT_ENT(name, desc, lc_opt_type_boolean, addr, 0, lc_opt_std_cb, lc_opt_std_dump, lc_opt_bool_dump_vals)

#define LC_OPT_ENT_NEGBOOL(name, desc, addr) \
	_LC_OPT_ENT(name, desc, lc_opt_type_negboolean, addr, 0, lc_opt_std_cb, lc_opt_std_dump, lc_opt_bool_dump_vals)

#define LC_OPT_ENT_STR(name, desc, buf, len) \
	_LC_OPT_ENT(name, desc, lc_opt_type_string, buf, len, lc_opt_std_cb, lc_opt_std_dump, NULL)

#define LC_OPT_ENT_CB(name, desc, type, data, len, cb, dump, dump_vals) \
	_LC_OPT_ENT(name, desc, type, data, len, cb, dump, dump_vals)

#define LC_OPT_LAST \
	_LC_OPT_ENT(NULL, NULL, 0, NULL, 0, NULL, NULL, NULL)

/**
 * Get the root option group.
 * @return The root option group.
 */
lc_opt_entry_t *lc_opt_root_grp(void);

/**
 * Check, if a group is the root group
 * @param ent   The entry to check for.
 * @return      1, if the entry is the root group, 0 otherwise.
 */
int lc_opt_grp_is_root(const lc_opt_entry_t *ent);

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
 * 									(May be NULL).
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
							   lc_opt_dump_vals_t *dump_vals,
							   lc_opt_err_info_t *err);

int lc_opt_std_cb(const char *name, lc_opt_type_t type, void *data, size_t length, ...);

int lc_opt_std_dump(char *buf, size_t n, const char *name, lc_opt_type_t type, void *data, size_t length);

int lc_opt_bool_dump_vals(char *buf, size_t n, const char *name, lc_opt_type_t type, void *data, size_t length);

#define lc_opt_add_opt_int(grp, name, desc, value, err) \
	lc_opt_add_opt(grp, name, desc, lc_opt_type_int, value, 0, lc_opt_std_cb, lc_opt_std_dump, NULL, err)

#define lc_opt_add_opt_double(grp, name, desc, value, err) \
	lc_opt_add_opt(grp, name, desc, lc_opt_type_double, value, 0, lc_opt_std_cb, lc_opt_std_dump, NULL, err)

#define lc_opt_add_opt_string(grp, name, desc, buf, len, err) \
	lc_opt_add_opt(grp, name, desc, lc_opt_type_string, buf, len, lc_opt_std_cb, lc_opt_std_dump, NULL, err)

#define lc_opt_add_opt_bit(grp, name, desc, value, mask, err) \
	lc_opt_add_opt(grp, name, desc, lc_opt_type_bit, value, mask, lc_opt_std_cb, lc_opt_std_dump, NULL, err)


/**
 * Find a group inside another group.
 * @param grp   The group to search inside.
 * @param name  The name of the group you are looking for.
 * @param err   Error info (may be NULL).
 * @return      The group or NULL, if no such group can be found.
 */
lc_opt_entry_t *lc_opt_find_grp(const lc_opt_entry_t *grp, const char *name, lc_opt_err_info_t *err);

/**
 * Find an option inside another group.
 * @param grp   The group to search inside.
 * @param name  The name of the option you are looking for.
 * @param err   Error info (may be NULL).
 * @return      The group or NULL, if no such option can be found.
 */
lc_opt_entry_t *lc_opt_find_opt(const lc_opt_entry_t *grp, const char *name, lc_opt_err_info_t *err);

/**
 * Resolve a group.
 * @param root   The group to start resolving from.
 * @param names  A string array containing the path to the group.
 * @param n      Number of entries in @p names to consider.
 * @param err    Error information (may be NULL).
 * @return       The group or NULL, if none is found.
 */
lc_opt_entry_t *lc_opt_resolve_grp(const lc_opt_entry_t *root,
		const char * const *names, int n, lc_opt_err_info_t *err);

/**
 * Resolve an option.
 * @param root   The group to start resolving from.
 * @param names  A string array containing the path to the option.
 * @param n      Number of entries in @p names to consider.
 * @param err    Error information (may be NULL).
 * @return       The option or NULL, if none is found.
 */
lc_opt_entry_t *lc_opt_resolve_opt(const lc_opt_entry_t *root,
		const char * const *names, int n, lc_opt_err_info_t *err);

/**
 * Set the value of an option.
 * @param opt    The option to set.
 * @param value  The value of the option in a string representation.
 * @param err    Error information (may be NULL).
 * @return       0, if an error occurred, 1 else.
 */
int lc_opt_occurs(lc_opt_entry_t *opt, const char *value, lc_opt_err_info_t *err);

/**
 * Convert the option to a string representation.
 * @param buf  The string buffer to put the string representation to.
 * @param len  The length of @p buf.
 * @param ent  The option to process.
 * @return     @p buf.
 */
char *lc_opt_value_to_string(char *buf, size_t len, const lc_opt_entry_t *ent);

/**
 * Get the name of the type of an option.
 * @param ent The option.
 * @return The name of the type of the option.
 */
const char *lc_opt_get_type_name(const lc_opt_entry_t *ent);

/**
 * Print the help screen for the given entity to the given file.
 */
void lc_opt_print_help(lc_opt_entry_t *ent, FILE *f);

/**
 * Print the help screen for the given entity to the given file.
 * Use separator instead of '.' and ignore entities above ent,
 * i.e. if ent is root.be and has option isa.mach, prints
 * isa<separator>mach instead of root.be.isa.mach
 */
void lc_opt_print_help_for_entry(lc_opt_entry_t *ent, char separator, FILE *f);

void lc_opt_print_tree(lc_opt_entry_t *ent, FILE *f);

int lc_opt_add_table(lc_opt_entry_t *grp, const lc_opt_table_entry_t *table);

void lc_opt_from_file(const char *filenmame, FILE *f, lc_opt_error_handler_t *handler);

/**
 * The same as lc_opt_from_single_arg() only for an array of arguments.
 */
int lc_opt_from_argv(const lc_opt_entry_t *root,
					 const char *opt_prefix,
					 int argc, const char *argv[],
					 lc_opt_error_handler_t *handler);

/**
 * Set options from a single (command line) argument.
 * @param root			The root group we start resolving from.
 * @param opt_prefix	The option prefix which shall be stripped of (mostly --).
 * @param arg			The command line argument itself.
 * @param handler       An error handler.
 * @return              1, if the argument was set, 0 if not.
 */
int lc_opt_from_single_arg(const lc_opt_entry_t *grp,
						   const char *opt_prefix,
						   const char *arg,
						   lc_opt_error_handler_t *handler);

/**
 * Get printf environment for the option module.
 * Currently implemented options are:
 * %{opt:value} (%V)		Value of an option.
 * %{opt:type}  (%T)		Type of an option.
 * %{opt:name}  (%O)		Name of an option.
 * %{opt:desc}  (%D)		Description of an option.
 * @return The option printf environment.
 */
const lc_arg_env_t *lc_opt_get_arg_env(void);

/**
 * This function tries to open a ini file in the user's homedir
 * (On win32 this is \Documents and Settings\Application Data)
 * which is called .<ini_name>rc (on win32 <ini_name>.ini)
 *
 * and an ini file in the current directory which is called <ini_name>.ini on
 * both systems.
 */
void lc_opt_default_configs(const char *init_name);

#endif
