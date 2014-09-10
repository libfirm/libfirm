/*
 * This file is part of libFirm.
 * Copyright (C) 2012 IPD Goos, Universit"at Karlsruhe, Germany
 */
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#include "lc_opts_t.h"
#include "lc_opts_enum.h"
#include "hashptr.h"
#include "lc_printf.h"
#include "util.h"
#include "xmalloc.h"
#include "obst.h"

#define OPT_DELIM '-'

#define HELP_TEMPL         "%-15s %-10s %-45s"
#define HELP_TEMPL_VALS    HELP_TEMPL " [%s] (%s)"

static struct obstack obst;

static void set_name(lc_opt_entry_t *ent, const char *name)
{
	ent->name = name;
	ent->hash = hash_str(name);
}

#define entry_matches(ent,hash_val,str) \
	((ent)->hash == hash_val && strcmp((ent)->name, (str)) == 0)

#define entries_equal(e1,e2) entry_matches(e1, (e2)->hash, (e2)->name)

static lc_opt_entry_t *init_entry(lc_opt_entry_t *ent, lc_opt_entry_t *parent,
                                  const char *name, const char *desc)
{
	obstack_grow0(&obst, name, strlen(name));
	const char *copied_name = (char*)obstack_finish(&obst);
	obstack_grow0(&obst, desc, strlen(desc));
	const char *copied_desc = (char*)obstack_finish(&obst);

	memset(ent, 0, sizeof(*ent));
	set_name(ent, copied_name);
	ent->desc = copied_desc;
	ent->parent = parent;
	return ent;
}

static lc_opt_entry_t *init_grp(lc_opt_entry_t *ent)
{
	ent->is_grp = true;
	INIT_LIST_HEAD(&ent->v.grp.grps);
	INIT_LIST_HEAD(&ent->v.grp.opts);

	if (ent->parent && ent->parent->is_grp)
		list_add_tail(&ent->list, &lc_get_grp_special(ent->parent)->grps);

	return ent;
}

static lc_opt_entry_t *init_opt(lc_opt_entry_t *ent,
								lc_opt_type_t type,
								void *val, size_t length,
								lc_opt_callback_t *cb,
								lc_opt_dump_t *dump,
								lc_opt_dump_vals_t *dump_vals)
{
	lc_opt_special_t *s = lc_get_opt_special(ent);

	ent->is_grp = false;
	list_add_tail(&ent->list, &lc_get_grp_special(ent->parent)->opts);

	s->type      = type;
	s->value     = val;
	s->cb        = cb;
	s->dump      = dump;
	s->dump_vals = dump_vals;
	s->length    = length;
	return ent;
}


lc_opt_entry_t *lc_opt_root_grp(void)
{
	static lc_opt_entry_t root_group;
	static bool inited = 0;

	if (!inited) {
		obstack_init(&obst);
		inited = true;

		init_entry(&root_group, NULL, "root", "The root node");
		init_grp(&root_group);
	}

	return &root_group;
}

int lc_opt_grp_is_root(const lc_opt_entry_t *ent)
{
	return ent->parent == NULL;
}

static const char *get_type_name(lc_opt_type_t type)
{
	switch (type) {
	case lc_opt_type_enum:    return "enum";
	case lc_opt_type_bit:     return "bit";
	case lc_opt_type_int:     return "int";
	case lc_opt_type_double:  return "double";
	case lc_opt_type_boolean: return "boolean";
	case lc_opt_type_string:  return "string";
	case lc_opt_type_invalid: break;
	}
	return "<none>";
}

const char *lc_opt_get_type_name(const lc_opt_entry_t *ent)
{
	return get_type_name(lc_get_opt_special(ent)->type);
}

lc_opt_entry_t *lc_opt_get_grp(lc_opt_entry_t *parent, const char *name)
{
	lc_opt_entry_t *ent = lc_opt_find_grp(parent, name);

	if (!ent) {
		ent = OALLOC(&obst, lc_opt_entry_t);
		init_entry(ent, parent, name, "");
		init_grp(ent);
	}

	return ent;
}

lc_opt_entry_t *lc_opt_add_opt(lc_opt_entry_t *parent,
							   const char *name, const char *desc,
							   lc_opt_type_t type, void *value, size_t length,
							   lc_opt_callback_t *cb, lc_opt_dump_t *dump,
							   lc_opt_dump_vals_t *dump_vals)
{
	if (!parent->is_grp)
		return NULL;
	lc_opt_entry_t *ent = lc_opt_find_opt(parent, name);
	if (ent != NULL)
		return NULL;

	lc_opt_entry_t *res = OALLOC(&obst, lc_opt_entry_t);
	init_entry(res, parent, name, desc);
	init_opt(res, type, value, length, cb, dump, dump_vals);
	return res;
}


static lc_opt_entry_t *lc_opt_find_ent(const struct list_head *head,
                                       const char *name)
{
	unsigned hash = hash_str(name);
	list_for_each_entry(lc_opt_entry_t, ent, head, list) {
		if (entry_matches(ent, hash, name))
			return ent;
	}
	return NULL;
}

lc_opt_entry_t *lc_opt_find_grp(const lc_opt_entry_t *grp, const char *name)
{
	return grp ? lc_opt_find_ent(&lc_get_grp_special(grp)->grps, name) : NULL;
}

lc_opt_entry_t *lc_opt_find_opt(const lc_opt_entry_t *grp, const char *name)
{
	return grp ? lc_opt_find_ent(&lc_get_grp_special(grp)->opts, name) : NULL;
}

static const lc_opt_entry_t *resolve_up_to_last(const lc_opt_entry_t *root,
                                                const char *const *names,
                                                int pos, int n)
{
	if (pos == n)
		return root;

	lc_opt_entry_t *ent = lc_opt_find_grp(root, names[pos]);
	return ent != NULL ? resolve_up_to_last(ent, names, pos + 1, n) : NULL;
}

static const char *path_delim = "/.";

static lc_opt_entry_t *resolve_up_to_last_str_rec(lc_opt_entry_t *from,
                                                  const char *path,
                                                  const char **last_name)
{

	lc_opt_entry_t *res = from;
	size_t end          = strcspn(path, path_delim);

	if (path[end] != '\0') {
		/* skip all delimiters */
		size_t next = strspn(path + end, path_delim);

		/* copy the part of the path into a buffer */
		char *buf = (char*)malloc((end+1) * sizeof(buf[0]));
		strncpy(buf, path, end);
		buf[end] = '\0';

		/* resolve the group and free */
		from = lc_opt_get_grp(from, buf);
		free(buf);

		res = resolve_up_to_last_str_rec(from, path + end + next, last_name);
	} else if (last_name != NULL) {
		*last_name = path;
	}

	return res;
}

static lc_opt_entry_t *resolve_up_to_last_str(lc_opt_entry_t *root, const char *path, const char **last_name)
{
	size_t next = strspn(path, path_delim);

	/* if l != 0 we saw deliminators, so we resolve from the root */
	if (next > 0)
		root = lc_opt_root_grp();

	return resolve_up_to_last_str_rec(root, path + next, last_name);
}

lc_opt_entry_t *lc_opt_resolve_grp(const lc_opt_entry_t *root,
                                   const char * const *names, int n)
{
	const lc_opt_entry_t *grp = resolve_up_to_last(root, names, 0, n - 1);
	return lc_opt_find_grp(grp, names[n - 1]);
}

lc_opt_entry_t *lc_opt_resolve_opt(const lc_opt_entry_t *root,
                                   const char * const *names, int n)
{
	const lc_opt_entry_t *grp = resolve_up_to_last(root, names, 0, n - 1);
	return lc_opt_find_opt(grp, names[n - 1]);
}

static char *strtolower(char *buf, size_t n, const char *str)
{
	for (unsigned i = 0; i < n; ++i)
		buf[i] = tolower((unsigned char)str[i]);
	return buf;
}

bool lc_opt_std_cb(const char *name, lc_opt_type_t type, void *data,
                   size_t length, ...)
{
	(void)name;

	va_list args;
	va_start(args, length);

	bool res = false;
	if (data) {
		res = true;
		switch (type) {
		case lc_opt_type_bit: {
			bool val = va_arg(args, int);
			if (val)
				*(unsigned*)data |= length;
			else
				*(unsigned*)data &= ~length;
			break;
		}

		case lc_opt_type_boolean:
			*((bool*) data) = va_arg(args, int);
			break;

		case lc_opt_type_string:
			strncpy((char*)data, va_arg(args, const char *), length);
			break;

		case lc_opt_type_int:
			*((int *) data) = va_arg(args, int);
			break;

		case lc_opt_type_double:
			*((double *) data) = va_arg(args, double);
			break;
		default:
			res = false;
		}
	}

	va_end(args);
	return res;
}

int lc_opt_std_dump(char *buf, size_t n, const char *name, lc_opt_type_t type, void *data, size_t length)
{
	(void)name;
	(void)length;

	int res;
	if (data) {
		switch (type) {
		case lc_opt_type_bit:
			res = snprintf(buf, n, "%x", *((bool*) data));
			break;
		case lc_opt_type_boolean:
			res = snprintf(buf, n, "%s", *((bool*) data) ? "true" : "false");
			break;
		case lc_opt_type_string:
			strncpy(buf, (const char*)data, n);
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
	} else {
		strncpy(buf, "", n);
		res = 0;
	}

	return res;
}

int lc_opt_bool_dump_vals(char *buf, size_t n, const char *name, lc_opt_type_t type, void *data, size_t length)
{
	(void)name;
	(void)type;
	(void)data;
	(void)length;
	strncpy(buf, "true, false", n);
	return n;
}

bool lc_opt_occurs(lc_opt_entry_t *opt, const char *value)
{
	static const struct {
		const char *str;
		int val;
	} bool_strings[] = {
		{ "true",  1 },
		{ "yes",   1 },
		{ "on",    1 },
		{ "1",     1 },
		{ "false", 0 },
		{ "no",    0 },
		{ "off",   0 },
		{ "0",     0 },
	};

	if (opt == NULL)
		return false;

	lc_opt_special_t *s = lc_get_opt_special(opt);
	if (!s->cb)
		return false;

	s->is_set = true;

	bool fine = true;
	switch (s->type) {
	case lc_opt_type_int: {
		int val;
		if (sscanf(value, "%i", &val))
			fine = s->cb(opt->name, s->type, s->value, s->length, val);
		break;
	}

	case lc_opt_type_double: {
		double val;
		if (sscanf(value, "%lf", &val))
			fine = s->cb(opt->name, s->type, s->value, s->length, val);
		break;
	}

	case lc_opt_type_boolean:
	case lc_opt_type_bit: {
		char buf[16];
		bool val = false;
		strtolower(buf, sizeof(buf), value);
		for (unsigned i = 0; i < ARRAY_SIZE(bool_strings); ++i) {
			if (strcmp(buf, bool_strings[i].str) == 0) {
				val = bool_strings[i].val;
				fine = true;
				break;
			}
		}

		if (fine)
			fine = s->cb(opt->name, s->type, s->value, s->length, val);

		break;
	}

	case lc_opt_type_string:
	case lc_opt_type_enum:
		fine = s->cb(opt->name, s->type, s->value, s->length, value);
		break;
	case lc_opt_type_invalid:
		abort();
	}

	return fine;
}

char *lc_opt_value_to_string(char *buf, size_t len, const lc_opt_entry_t *ent)
{
	const lc_opt_special_t *s = lc_get_opt_special(ent);
	if (s->dump)
		s->dump(buf, len, ent->name, s->type, s->value, s->length);
	else
		strncpy(buf, "<n/a>", len);
	return buf;
}

static char *lc_opt_values_to_string(char *buf, size_t len,
                                     const lc_opt_entry_t *ent)
{
	const lc_opt_special_t *s = lc_get_opt_special(ent);
	if (s->dump_vals)
		s->dump_vals(buf, len, ent->name, s->type, s->value, s->length);
	return buf;
}

bool lc_opt_add_table(lc_opt_entry_t *root, const lc_opt_table_entry_t *table)
{
	bool res = false;
	for (int i = 0; table[i].name != NULL; ++i) {
		const char *name;
		const lc_opt_table_entry_t *tab = &table[i];
		lc_opt_entry_t *grp   = resolve_up_to_last_str(root, tab->name, &name);
		lc_opt_entry_t *entry = lc_opt_add_opt(grp, name, tab->desc, tab->type, tab->value, tab->len, tab->cb, tab->dump, tab->dump_vals);
		if (entry == NULL)
			res = true;
	}

	return res;
}

static void lc_opt_print_grp_path_rec(char *buf, size_t len, const lc_opt_entry_t *ent, char separator, lc_opt_entry_t *stop_ent)
{
	if (ent == stop_ent)
		return;
	if (!lc_opt_grp_is_root(ent)) {
		lc_opt_print_grp_path_rec(buf, len, ent->parent, separator, stop_ent);
		size_t l = strlen(buf);
		if (l > 0 && l < len-1) {
			buf[l]     = separator;
			buf[l + 1] = '\0';
		}
	}

	strncat(buf, ent->name, len-1);
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

	if (!list_empty(&s->opts)) {
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
	for (int i = 0; i < n; ++i)
		fputc(' ', f);
}

static void lc_opt_print_tree_lc_opt_indent(lc_opt_entry_t *ent, FILE *f, int level)
{
	char buf[256];
	lc_opt_special_t *s = lc_get_opt_special(ent);

	indent(f, level);
	fprintf(f, "%c%s(\"%s\"):%s = %s\n", s->is_set ? '+' : '-', ent->name,
	        ent->desc, lc_opt_get_type_name(ent),
	        lc_opt_value_to_string(buf, sizeof(buf), ent));
}

static void lc_opt_print_tree_grp_indent(lc_opt_entry_t *ent, FILE *f, int level)
{
	if (!ent->is_grp)
		return;

	lc_grp_special_t *s = lc_get_grp_special(ent);
	indent(f, level);
	fprintf(f, "/%s\n", ent->name);

	list_for_each_entry(lc_opt_entry_t, e, &s->grps, list) {
		lc_opt_print_tree_grp_indent(e, f, level + 2);
	}

	list_for_each_entry(lc_opt_entry_t, e, &s->opts, list) {
		lc_opt_print_tree_lc_opt_indent(e, f, level + 2);
	}
}

void lc_opt_print_tree(lc_opt_entry_t *ent, FILE *f)
{
	lc_opt_print_tree_grp_indent(ent, f, 0);
}

int lc_opt_from_single_arg(const lc_opt_entry_t *root, const char *arg)
{
	const lc_opt_entry_t *grp = root;

	/* find the next delimiter (the -) and extract the string up to
	 * there. */
	const char *end    = strchr(arg, OPT_DELIM);
	const char *eqsign = strchr(arg, '=');
	if (eqsign && eqsign < end)
		end = NULL;
	while (end != NULL) {
		/* Copy the part of the option into the buffer and add the
		 * finalizing zero. */
		char *buf = (char*)obstack_copy0(&obst, arg, end - arg);

		/* Resolve the group inside the group */
		lc_opt_entry_t *new_grp = lc_opt_find_grp(grp, buf);
		if (new_grp == NULL)
			break;
		grp = new_grp;

		/* Find the next option part delimiter. */
		arg    = end + 1;
		end    = strchr(arg, OPT_DELIM);
		eqsign = strchr(arg, '=');
		if (eqsign && eqsign < end)
			end = NULL;
		obstack_free(&obst, buf);
	}

	/*
	 * Now, we are at the last option part:
	 * --grp1-grp2-...-grpn-opt=value
	 * Check, for the = and evaluate the option string. If the = is
	 * missing, we should have a boolean option, but that is checked
	 * later.
	 */
	end = strchr(arg, '=');
	char *buf = (char*)obstack_copy0(&obst, arg, end ? end - arg : (int) strlen(arg));
	lc_opt_entry_t *opt = lc_opt_find_opt(grp, buf);
	if (opt == NULL)
		return 0;

	/*
	 * Now evaluate the parameter of the option (the part after
	 * the =) if it was given.
	 */
	arg = end ? end + 1 : "true";

	/* Set the value of the option. */
	return lc_opt_occurs(opt, arg);
}
