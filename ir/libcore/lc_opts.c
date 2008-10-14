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
#include "config.h"

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#if defined(__FreeBSD__)
#include <stdlib.h>
#elif defined(_WIN32)
#include <malloc.h>
#else
#include <alloca.h>
#endif

/* Includes to determine user's home directory */
#ifdef _WIN32
#include <shlobj.h>
#else
#include <sys/types.h>
#include <unistd.h>
#include <pwd.h>
#endif

/* maximum length of a path. */
#ifndef MAX_PATH
#define MAX_PATH 2048
#endif


#include "lc_common_t.h"
#include "lc_opts_t.h"
#include "lc_opts_enum.h"
#include "lc_parser_t.h"
#include "hashptr.h"
#include "lc_printf.h"

#define ERR_STRING "In argument \"%s\": "

#define OPT_DELIM '-'

#define HELP_TEMPL		"%-15s %-10s %-45s"
#define HELP_TEMPL_VALS	HELP_TEMPL " [%s] (%s)"

static struct obstack obst;

static void set_name(lc_opt_entry_t *ent, const char *name)
{
	ent->name = name;
	ent->hash = HASH_STR(name, strlen(name));
}

#define entry_matches(ent,hash_val,str) \
	((ent)->hash == hash_val && strcmp((ent)->name, (str)) == 0)

#define entries_equal(e1,e2) entry_matches(e1, (e2)->hash, (e2)->name)

static lc_opt_err_info_t *set_error(lc_opt_err_info_t *err, int error, const char *arg)
{
	if(err) {
		err->error = error;
		err->msg = "";
		err->arg = arg;
	}

	return err;
}

int lc_opt_raise_error(const lc_opt_err_info_t *err, lc_opt_error_handler_t *handler,
		const char *fmt, ...)
{
	va_list args;
	int res = 0;

	va_start(args, fmt);
	if(err && lc_opt_is_error(err)) {
		res = 1;
		if(handler) {
			char buf[256];
			vsnprintf(buf, sizeof(buf), fmt, args);
			handler(buf, err);
		}
	}
	va_end(args);

	return res;
}

static lc_opt_entry_t *init_entry(lc_opt_entry_t *ent, lc_opt_entry_t *parent,
		const char *name, const char *desc)
{
	const char *copied_name;
	const char *copied_desc;

	obstack_grow0(&obst, name, strlen(name));
	copied_name = obstack_finish(&obst);
	obstack_grow0(&obst, desc, strlen(desc));
	copied_desc = obstack_finish(&obst);

	memset(ent, 0, sizeof(*ent));
	set_name(ent, copied_name);
	ent->desc = copied_desc;
	ent->parent = parent;
	return ent;
}

static lc_opt_entry_t *init_grp(lc_opt_entry_t *ent, lc_opt_err_info_t *err)
{
	ent->is_grp = 1;
	INIT_LIST_HEAD(&ent->v.grp.grps);
	INIT_LIST_HEAD(&ent->v.grp.opts);

	set_error(err, lc_opt_err_none, "");
	if(ent->parent) {
		if(ent->parent->is_grp)
			list_add_tail(&ent->list, &lc_get_grp_special(ent->parent)->grps);
		else
			set_error(err, lc_opt_err_grp_expected, ent->parent->name);
	}

	return ent;
}

static lc_opt_entry_t *init_opt(lc_opt_entry_t *ent,
								lc_opt_type_t type,
								void *val, size_t length,
								lc_opt_callback_t *cb,
								lc_opt_dump_t *dump,
								lc_opt_dump_vals_t *dump_vals,
								lc_opt_err_info_t *err)
{
	lc_opt_special_t *s = lc_get_opt_special(ent);

	ent->is_grp = 0;
	set_error(err, lc_opt_err_none, "");
	list_add_tail(&ent->list, &lc_get_grp_special(ent->parent)->opts);

	s->type		 = type;
	s->value	 = val;
	s->cb		 = cb;
	s->dump      = dump;
	s->dump_vals = dump_vals;
	s->length	 = length;

	return ent;
}


lc_opt_entry_t *lc_opt_root_grp(void)
{
	static lc_opt_entry_t root_group;
	static int inited = 0;

	if(!inited) {
		obstack_init(&obst);
		inited = 1;

		init_entry(&root_group, NULL, "root", "The root node");
		init_grp(&root_group, NULL);
	}

	return &root_group;
}

int lc_opt_grp_is_root(const lc_opt_entry_t *ent)
{
	return ent->parent == NULL;
}

static const char *get_type_name(lc_opt_type_t type)
{
	const char *res;

#define XXX(t) case lc_opt_type_ ## t: res = #t; break
	switch(type) {
		XXX(enum);
		XXX(bit);
		XXX(int);
		XXX(double);
		XXX(boolean);
		XXX(string);
		case lc_opt_type_negbit:     res = "bit";     break;
		case lc_opt_type_negboolean: res = "boolean"; break;
		default:
		res = "<none>";
	}
#undef XXX

	return res;
}

const char *lc_opt_get_type_name(const lc_opt_entry_t *ent)
{
	return get_type_name(lc_get_opt_special(ent)->type);
}


lc_opt_entry_t *lc_opt_find_grp(const lc_opt_entry_t *grp, const char *name, lc_opt_err_info_t *err);
lc_opt_entry_t *lc_opt_find_opt(const lc_opt_entry_t *grp, const char *name, lc_opt_err_info_t *err);

lc_opt_entry_t *lc_opt_get_grp(lc_opt_entry_t *parent, const char *name)
{
	lc_opt_entry_t *ent = lc_opt_find_grp(parent, name, NULL);

	if(!ent) {
		ent = obstack_alloc(&obst, sizeof(*ent));
		init_entry(ent, parent, name, "");
		init_grp(ent, NULL);
	}

	return ent;
}

lc_opt_entry_t *lc_opt_add_opt(lc_opt_entry_t *parent,
							   const char *name, const char *desc,
							   lc_opt_type_t type, void *value, size_t length,
							   lc_opt_callback_t *cb, lc_opt_dump_t *dump,
							   lc_opt_dump_vals_t *dump_vals,
							   lc_opt_err_info_t *err)
{
	lc_opt_entry_t *res = NULL;

	if(parent->is_grp) {
		lc_opt_entry_t *ent = lc_opt_find_opt(parent, name, NULL);

		if(!ent) {
			res = obstack_alloc(&obst, sizeof(*ent));
			init_entry(res, parent, name, desc);
			init_opt(res, type, value, length, cb, dump, dump_vals, err);
		} else
			set_error(err, lc_opt_err_opt_already_there, name);
	} else
		set_error(err, lc_opt_err_grp_expected, name);

	return res;
}


static lc_opt_entry_t *lc_opt_find_ent(const struct list_head *head, const char *name,
		int error_to_use, lc_opt_err_info_t *err)
{
	lc_opt_entry_t *ent, *found = NULL;
	int error = error_to_use;
	unsigned hash = HASH_STR(name, strlen(name));

	if(!list_empty(head)) {
		list_for_each_entry(lc_opt_entry_t, ent, head, list) {
			if(entry_matches(ent, hash, name)) {
				error = lc_opt_err_none;
				found = ent;
				break;
			}
		}
	}

	set_error(err, error, name);
	return found;
}

lc_opt_entry_t *lc_opt_find_grp(const lc_opt_entry_t *grp, const char *name, lc_opt_err_info_t *err)
{
	return grp ? lc_opt_find_ent(&lc_get_grp_special(grp)->grps,
			name, lc_opt_err_grp_not_found, err) : NULL;
}

lc_opt_entry_t *lc_opt_find_opt(const lc_opt_entry_t *grp, const char *name, lc_opt_err_info_t *err)
{
	return grp ? lc_opt_find_ent(&lc_get_grp_special(grp)->opts,
			name, lc_opt_err_opt_not_found, err) : NULL;
}

static const lc_opt_entry_t *resolve_up_to_last(const lc_opt_entry_t *root,
		const char * const *names, int pos, int n, lc_opt_err_info_t *err)
{
	lc_opt_entry_t *ent;

	if(pos == n)
		return root;

	ent = lc_opt_find_grp(root, names[pos], err);
	return ent ? resolve_up_to_last(ent, names, pos + 1, n, err) : NULL;
}

static const char *path_delim = "/.";

static lc_opt_entry_t *resolve_up_to_last_str_rec(lc_opt_entry_t *from,
														const char *path,
														const char **last_name)
{

	lc_opt_entry_t *res = from;
	size_t end          = strcspn(path, path_delim);

	if(path[end] != '\0') {
		/* skip all delimiters */
		size_t next = strspn(path + end, path_delim);

		/* copy the part of the path into a buffer */
		char *buf = malloc((end+1) * sizeof(buf[0]));
		strncpy(buf, path, end);
		buf[end] = '\0';

		/* resolve the group and free */
		from = lc_opt_get_grp(from, buf);
		free(buf);

		res = resolve_up_to_last_str_rec(from, path + end + next, last_name);
	}

	else if(last_name != NULL) {
		*last_name = path;
	}

	return res;
}

static lc_opt_entry_t *resolve_up_to_last_str(lc_opt_entry_t *root, const char *path, const char **last_name)
{
	size_t next = strspn(path, path_delim);

	/* if l != 0 we saw deliminators, so we resolve from the root */
	if(next > 0)
		root = lc_opt_root_grp();

	return resolve_up_to_last_str_rec(root, path + next, last_name);
}

lc_opt_entry_t *lc_opt_resolve_grp(const lc_opt_entry_t *root,
		const char * const *names, int n, lc_opt_err_info_t *err)
{
	const lc_opt_entry_t *grp = resolve_up_to_last(root, names, 0, n - 1, err);
	return lc_opt_find_grp(grp, names[n - 1], err);
}

lc_opt_entry_t *lc_opt_resolve_opt(const lc_opt_entry_t *root,
		const char * const *names, int n, lc_opt_err_info_t *err)
{
	const lc_opt_entry_t *grp = resolve_up_to_last(root, names, 0, n - 1, err);
	return lc_opt_find_opt(grp, names[n - 1], err);
}

static char *strtolower(char *buf, size_t n, const char *str)
{
	unsigned i;
	for(i = 0; i < n; ++i)
		buf[i] = tolower(str[i]);
	return buf;
}

int lc_opt_std_cb(UNUSED(const char *name), lc_opt_type_t type, void *data, size_t length, ...)
{
	va_list args;
	int res = 0;
	int integer;

	va_start(args, length);

	if(data) {
		res = 1;
		switch(type) {
		case lc_opt_type_bit:
			integer = va_arg(args, int);
			if(integer)
				*((int *) data) |= length;
			else
				*((int *) data) &= ~length;
			break;

		case lc_opt_type_negbit:
			integer = va_arg(args, int);
			if(integer)
				*((int *) data) &= ~length;
			else
				*((int *) data) |= length;
			break;

		case lc_opt_type_boolean:
			*((int *) data) = va_arg(args, int);
			break;

		case lc_opt_type_negboolean:
			*((int *) data) = !va_arg(args, int);
			break;

		case lc_opt_type_string:
			strncpy(data, va_arg(args, const char *), length);
			break;

		case lc_opt_type_int:
			*((int *) data) = va_arg(args, int);
			break;

		case lc_opt_type_double:
			*((double *) data) = va_arg(args, double);
			break;
		default:
			res = 0;
		}
	}

	va_end(args);
	return res;
}

int lc_opt_std_dump(char *buf, size_t n, UNUSED(const char *name), lc_opt_type_t type, void *data, UNUSED(size_t length))
{
	int res;

	if(data) {
		switch(type) {
		case lc_opt_type_bit:
		case lc_opt_type_negbit:
			res = snprintf(buf, n, "%x", *((int *) data));
			break;
		case lc_opt_type_boolean:
		case lc_opt_type_negboolean:
			res = snprintf(buf, n, "%s", *((int *) data) ? "true" : "false");
			break;
		case lc_opt_type_string:
			strncpy(buf, data, n);
			res = n;
			break;
		case lc_opt_type_int:
			res = snprintf(buf, n, "%d", *((int *) data));
			break;
		case lc_opt_type_double:
			res = snprintf(buf, n, "%g", *((double *) data));
			break;
		default:
			strncpy(buf, "", n);
			res = 0;
		}
	}

	else {
		strncpy(buf, "", n);
		res = 0;
	}

	return res;
}

int lc_opt_bool_dump_vals(char *buf, size_t n, UNUSED(const char *name), UNUSED(lc_opt_type_t type), UNUSED(void *data), UNUSED(size_t length))
{
	strncpy(buf, "true, false", n);
	return n;
}

int lc_opt_occurs(lc_opt_entry_t *opt, const char *value, lc_opt_err_info_t *err)
{
	static const struct {
		const char *str;
		int val;
	} bool_strings[] = {
		{ "yes", 1 },
		{ "true", 1 },
		{ "on", 1 },
		{ "1", 1 },
		{ "no", 0 },
		{ "false", 0 },
		{ "off", 0 },
		{ "0", 0 },
	};

	unsigned i;
	int error = lc_opt_err_illegal_format;
	lc_opt_special_t *s = lc_get_opt_special(opt);
	char buf[16];
	union {
		int integer;
		double dbl;
	} val_storage, *val = &val_storage;

	if(!opt) {
		set_error(err, lc_opt_err_opt_not_found, "");
		return 0;
	}

	if(!s->cb) {
		set_error(err, lc_opt_err_no_callback, "");
		return 0;
	}

	s->is_set = 1;

	switch(s->type) {
		case lc_opt_type_int:
			if(sscanf(value, "%i", (int *) val)) {
				error = lc_opt_err_unknown_value;
				if (s->cb(opt->name, s->type, s->value, s->length, val->integer))
					error = lc_opt_err_none;
			}
			break;

		case lc_opt_type_double:
			if(sscanf(value, "%lf", (double *) val)) {
				error = lc_opt_err_unknown_value;
				if (s->cb(opt->name, s->type, s->value, s->length, val->dbl))
					error = lc_opt_err_none;
			}
			break;

		case lc_opt_type_boolean:
		case lc_opt_type_negboolean:
		case lc_opt_type_bit:
		case lc_opt_type_negbit:
				strtolower(buf, sizeof(buf), value);
				for(i = 0; i < LC_ARRSIZE(bool_strings); ++i) {
					if(strcmp(buf, bool_strings[i].str) == 0) {
						val->integer = bool_strings[i].val;
						error = lc_opt_err_none;
						break;
					}
				}

				if (error == lc_opt_err_none) {
					error = lc_opt_err_unknown_value;
					if (s->cb(opt->name, s->type, s->value, s->length, val->integer))
						error = lc_opt_err_none;
				}

			break;

		case lc_opt_type_string:
		case lc_opt_type_enum:
			error = lc_opt_err_unknown_value;
			if (s->cb(opt->name, s->type, s->value, s->length, value))
				error = lc_opt_err_none;
			break;
	}

	set_error(err, error, value);
	return error == lc_opt_err_none;
}

char *lc_opt_value_to_string(char *buf, size_t len, const lc_opt_entry_t *ent)
{
	const lc_opt_special_t *s = lc_get_opt_special(ent);
	if(s->dump)
		s->dump(buf, len, ent->name, s->type, s->value, s->length);
	else
		strncpy(buf, "<n/a>", len);

	return buf;
}

char *lc_opt_values_to_string(char *buf, size_t len, const lc_opt_entry_t *ent)
{
	const lc_opt_special_t *s = lc_get_opt_special(ent);
	if(s->dump_vals)
		s->dump_vals(buf, len, ent->name, s->type, s->value, s->length);

	return buf;
}

static lc_opt_entry_t *resolve_up_to_last_str(lc_opt_entry_t *root, const char *path, const char **last_name);

int lc_opt_add_table(lc_opt_entry_t *root, const lc_opt_table_entry_t *table)
{
	int i, res = 0;
	lc_opt_err_info_t err;

	for(i = 0; table[i].name != NULL; ++i) {
		const char *name;
		const lc_opt_table_entry_t *tab = &table[i];
		lc_opt_entry_t *grp = resolve_up_to_last_str(root, tab->name, &name);

		lc_opt_add_opt(grp, name, tab->desc, tab->type, tab->value, tab->len, tab->cb, tab->dump, tab->dump_vals, &err);
		if(err.error != lc_opt_err_none)
			res = 1;
	}

	return res;
}

static void lc_opt_print_grp_path_rec(char *buf, size_t len, const lc_opt_entry_t *ent, char separator, lc_opt_entry_t *stop_ent)
{
	if (ent == stop_ent)
		return;
	if (!lc_opt_grp_is_root(ent)) {
		size_t l;
		lc_opt_print_grp_path_rec(buf, len, ent->parent, separator, stop_ent);
		l = strlen(buf);
		if (l > 0 && l < len-1) {
			buf[l]     = separator;
			buf[l + 1] = '\0';
		}
	}

	strncat(buf, ent->name, len);
}

static char *lc_opt_print_grp_path(char *buf, size_t len, const lc_opt_entry_t *ent, char separator, lc_opt_entry_t *stop_ent)
{
	if (len > 0)
		buf[0] = '\0';
	lc_opt_print_grp_path_rec(buf, len, ent, separator, stop_ent);
	return buf;
}

/**
 * dump the option tree.
 * @param ent        starting entity
 * @param separator  separator char
 * @param stop_ent   stop at this entity when dumping the name
 * @param f          output file
 */
static void lc_opt_print_help_rec(lc_opt_entry_t *ent, char separator, lc_opt_entry_t *stop_ent, FILE *f)
{
	lc_grp_special_t *s = lc_get_grp_special(ent);
	char grp_name[512];
	char value[256];
	char values[512];
	lc_opt_entry_t *e;

	if(!list_empty(&s->opts)) {
		lc_opt_print_grp_path(grp_name, sizeof(grp_name), ent, separator, stop_ent);
		fputc('\n', f);
		if (grp_name[0])
			fprintf(f, "%s:\n", grp_name);

		list_for_each_entry(lc_opt_entry_t, e, &s->opts, list) {
			value[0]  = '\0';
			values[0] = '\0';
			lc_opt_value_to_string(value, sizeof(value), e);
			lc_opt_values_to_string(values, sizeof(values), e);
			fprintf(f, HELP_TEMPL_VALS "\n", e->name, lc_opt_get_type_name(e), e->desc, value, values);
		}
	}

	list_for_each_entry(lc_opt_entry_t, e, &s->grps, list) {
		lc_opt_print_help_rec(e, separator, stop_ent, f);
	}

}

void lc_opt_print_help(lc_opt_entry_t *ent, FILE *f)
{
	fprintf(f, HELP_TEMPL_VALS "\n", "option", "type", "description", "default", "possible options");
	lc_opt_print_help_rec(ent, '.', NULL, f);
}

void lc_opt_print_help_for_entry(lc_opt_entry_t *ent, char separator, FILE *f)
{
	fprintf(f, HELP_TEMPL_VALS "\n", "option", "type", "description", "default", "possible options");
	lc_opt_print_help_rec(ent, separator, ent, f);
}


static void indent(FILE *f, int n)
{
	int i;
	for(i = 0; i < n; ++i)
		fputc(' ', f);
}

static void lc_opt_print_tree_lc_opt_indent(lc_opt_entry_t *ent, FILE *f, int level)
{
	char buf[256];
	lc_opt_special_t *s = lc_get_opt_special(ent);

	indent(f, level);
	fprintf(f, "%c%s(\"%s\"):%s = %s\n", s->is_set ? '+' : '-', ent->name,
			ent->desc, lc_opt_get_type_name(ent), lc_opt_value_to_string(buf, sizeof(buf), ent));
}

static void lc_opt_print_tree_grp_indent(lc_opt_entry_t *ent, FILE *f, int level)
{
	lc_grp_special_t *s;

	if(ent->is_grp) {
		lc_opt_entry_t *e;

		s = lc_get_grp_special(ent);
		indent(f, level);
		fprintf(f, "/%s\n", ent->name);

		list_for_each_entry(lc_opt_entry_t, e, &s->grps, list) {
			lc_opt_print_tree_grp_indent(e, f, level + 2);
		}

		list_for_each_entry(lc_opt_entry_t, e, &s->opts, list) {
			lc_opt_print_tree_lc_opt_indent(e, f, level + 2);
		}
	}
}

void lc_opt_print_tree(lc_opt_entry_t *ent, FILE *f)
{
	lc_opt_print_tree_grp_indent(ent, f, 0);
}


void lc_opt_from_file(const char *filename, FILE *f, lc_opt_error_handler_t *handler)
{
	PMANGLE(in) = f;
	lc_opt_init_parser(filename, handler);
	PMANGLE(parse)();
}

int lc_opt_from_single_arg(const lc_opt_entry_t *root,
						   const char *opt_prefix,
						   const char *arg, lc_opt_error_handler_t *handler)
{
	const lc_opt_entry_t *grp = root;
	int n                     = strlen(arg);
	int n_prefix              = opt_prefix ? strlen(opt_prefix) : 0;
	int error                 = 0;
	int ret                   = 0;

	lc_opt_err_info_t err;
	char *end, *buf, *eqsign;

	if(n >= n_prefix && strncmp(opt_prefix, arg, n_prefix) == 0) {
		arg = arg + n_prefix;

		/*
		 * check, if the next character is a @.
		 * This means, that we want to read options
		 * from a file.
		 */
		if(arg[0] == '@') {
			size_t n		= strcspn(&arg[1], " \t\n");
			char *fname		= alloca(n + 1);
			FILE *f;

			strncpy(fname, &arg[1], n);
			if((f = fopen(fname, "rt")) != NULL) {
				lc_opt_from_file(fname, f, handler);
				fclose(f);
				set_error(&err, lc_opt_err_none, NULL);
			}

			else
				set_error(&err, lc_opt_err_file_not_found, arg);

			return !lc_opt_raise_error(&err, handler, "Unable to open file: %s", fname);
		}

		/* find the next delimiter (the -) and extract the string up to
		 * there. */
		end    = strchr(arg, OPT_DELIM);
		eqsign = strchr(arg, '=');
		if (eqsign && eqsign < end)
			end = NULL;
		while(end != NULL) {
			/*
			 * Copy the part of the option into the buffer and add the
			 * finalizing zero.
			 */
			buf = obstack_copy0(&obst, arg, end - arg);

			/* Resolve the group inside the group */
			grp = lc_opt_find_grp(grp, buf, &err);
			error = lc_opt_raise_error(&err, handler, ERR_STRING, arg);
			if(error)
				break;

			/* Find the next option part delimiter. */
			arg = end + 1;
			end    = strchr(arg, OPT_DELIM);
			eqsign = strchr(arg, '=');
			if (eqsign && eqsign < end)
				end = NULL;
			obstack_free(&obst, buf);
		}

		if(!error) {
			lc_opt_entry_t *opt;

			/*
			 * Now, we are at the last option part:
			 * --grp1-grp2-...-grpn-opt=value
			 * Check, for the = and evaluate the option string. If the = is
			 * missing, we should have a boolean option, but that is checked
			 * later.
			 */
			end = strchr(arg, '=');
			buf = obstack_copy0(&obst, arg, end ? end - arg : (int) strlen(arg));
			opt = lc_opt_find_opt(grp, buf, &err);
			error = lc_opt_raise_error(&err, handler, ERR_STRING, arg);

			if(!error) {
				/*
				 * Now evaluate the parameter of the option (the part after
				 * the =) if it was given.
				 */
				arg = end ? end + 1 : "true";

				/* Set the value of the option. */
				lc_opt_occurs(opt, arg, &err);
				ret = !lc_opt_raise_error(&err, handler, ERR_STRING, arg);
			}
		}
	}

	return ret;
}

int lc_opt_from_argv(const lc_opt_entry_t *root,
					 const char *opt_prefix,
					 int argc, const char *argv[],
					 lc_opt_error_handler_t *handler)
{
	int i;
	int options_set = 0;

	for(i = 0; i < argc; ++i) {
		options_set |= lc_opt_from_single_arg(root, opt_prefix, argv[i], handler);
	}

	return options_set;
}

static int opt_arg_type(UNUSED(const lc_arg_occ_t *occ))
{
	return lc_arg_type_ptr;
}

static int opt_arg_emit(lc_appendable_t *app, const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	char buf[256];

	lc_opt_entry_t *opt = arg->v_ptr;
	const char *s		= buf;
	size_t res			= 0;

	switch(occ->conversion) {
	case 'V':
		lc_opt_value_to_string(buf, sizeof(buf), opt);
		break;
	case 'T':
		s = lc_opt_get_type_name(opt);
		break;
	case 'D':
		s = opt->desc;
		break;
	case 'O':
		s = opt->name;
		break;
	default:
		s = NULL;
	}

	if(s)
		res = lc_appendable_snadd(app, s, strlen(s));

	return res;
}

static const lc_arg_handler_t lc_opt_arg_handler = {
	opt_arg_type,
	opt_arg_emit
};


/* lc_printf facility for options */

const lc_arg_env_t *lc_opt_get_arg_env(void)
{
	static lc_arg_env_t *env = NULL;

	if(!env) {
		env = lc_arg_new_env();

		lc_arg_register(env, "opt:value", 'V', &lc_opt_arg_handler);
		lc_arg_register(env, "opt:type",  'T', &lc_opt_arg_handler);
		lc_arg_register(env, "opt:desc",  'D', &lc_opt_arg_handler);
		lc_arg_register(env, "opt:name",  'O', &lc_opt_arg_handler);
	}

	return env;
}

static int lc_opts_default_error_handler(const char *prefix, const lc_opt_err_info_t *err)
{
	fprintf(stderr, "%s: %s; %s\n", prefix, err->msg, err->arg);
	return 0;
}

void lc_opts_init(const char *ini_name, lc_opt_entry_t *root, const char *arg_prefix, int argc, const char **argv)
{
	FILE *f;
	char path[MAX_PATH];
	char local_ini_file[MAX_PATH];
	char home_dir_ini_file[MAX_PATH];

	/* <cmnt>.ini */
	strncpy(local_ini_file, ini_name, sizeof(local_ini_file));
	strncat(local_ini_file, ".ini", sizeof(local_ini_file));
	local_ini_file[sizeof(local_ini_file) - 1] = '\0';
	path[0] = '\0';

#ifdef _WIN32
#if _MSC_VER > 1200
	/* ARG: need newer SDK to compile this */
	SHGetFolderPath(NULL, CSIDL_LOCAL_APPDATA, NULL, 0, path);
	strncat(path, "\\", sizeof(path));
#endif
	strncpy(home_dir_ini_file, local_ini_file, sizeof(home_dir_ini_file));
	home_dir_ini_file[sizeof(home_dir_ini_file) - 1] = '\0';
#else
	{
		struct passwd *entry = getpwuid(getuid());
		if (entry != NULL) {
			strcpy(path, entry->pw_dir);
			strncat(path, "/", sizeof(path));
			/* .<cmnt>rc */
			snprintf(home_dir_ini_file, sizeof(home_dir_ini_file), ".%src", ini_name);
			home_dir_ini_file[sizeof(home_dir_ini_file) - 1] = '\0';
		} else {
			/* FIXME: some error occurred */
			home_dir_ini_file[0] = '\0';
		}
	}
#endif

	strncat(path, home_dir_ini_file, sizeof(path));
	path[sizeof(path) - 1] = '\0';

	/* Process ini file in user's home. */
	f = fopen(path, "rt");
	if (f) {
		fprintf(stderr, "Warning: Automatically reading options from '%s'\n", path);
		lc_opt_from_file(path, f, lc_opts_default_error_handler);
		fclose(f);
	}

	/* Process ini file in current directory. */
	f = fopen(local_ini_file, "rt");
	if (f) {
		fprintf(stderr, "Warning: Automatically reading options from '%s'\n", local_ini_file);
		lc_opt_from_file(local_ini_file, f, lc_opts_default_error_handler);
		fclose(f);
	}

	/* process arguments from the command line */
	lc_opt_from_argv(root, arg_prefix, argc, argv, lc_opts_default_error_handler);
}
