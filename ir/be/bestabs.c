/**
 * Stabs support.
 *
 * @author Michael Beck
 * @date   11.9.2006
 * @cvsid  $Id$
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#include "entity.h"
#include "xmalloc.h"
#include "be_dbgout.h"

/* stabs types */
enum stabs_types {
	NIL     = 0,
	N_GSYM  = 32,   /**< global symbol */
	N_FUN   = 36,   /**< procedure */
	N_STSYM = 38,   /**< Initialized static variable */
	N_LCSYM = 40,   /**< uninitialized static var */
	N_RSYM  = 64,   /**< global register variable */
	N_SLINE = 68,
	N_SO    = 100,  /**< name and path of the source file */
	N_LSYM  = 128,  /**< local symbol */
	N_PSYM  = 160,  /**< parameters to a function */
	N_LBRAC = 192,  /**< left brace */
	N_RBRAC = 224,  /**< right brace */
};

/**
 * The stabs handle.
 */
typedef struct stabs_handle {
	dbg_handle base;   /**< the base class */
	FILE       *f;     /**< the file write to */
	entity     *cur_ent;
} stabs_handle;

/**
 * Returns the stabs type number of a Firm type.
 */
static unsigned get_type_number(ir_type *tp) {
	return 0;
}  /* get_type_number */

/**
 * begin a new file
 */
static void stabs_begin(dbg_handle *handle, const char *filename) {
	stabs_handle *h = (stabs_handle *)handle;
	fprintf(h->f, ".stabs \"%s\",%d,0,0,.Ltext0\n", filename, N_SO);
}  /* stabs_begin */

/**
 * prints a line number
 */
static void stabs_line(dbg_handle *handle, unsigned lineno, const char *address) {
	stabs_handle *h = (stabs_handle *)handle;
	fprintf(h->f, ".stabn %d, 0, %u, %s-%s\n", N_SLINE, lineno, address, get_entity_ld_name(h->cur_ent));
}  /* stabs_line */

/**
 * prints the stabs for a function
 */
static void stabs_method(dbg_handle *handle, entity *ent) {
	stabs_handle *h = (stabs_handle *)handle;
	ir_type *tp;
	unsigned type_num;

	h->cur_ent = ent;

	tp = get_entity_type(ent);
	if (get_method_n_ress(tp) > 0)
		tp = get_method_res_type(tp, 0);
	else
		tp = NULL;
	type_num = get_type_number(tp);
	fprintf(h->f, ".stabs \"%s:%c%u\",%u,0,0,%s\n",
		get_entity_name(ent),
		get_entity_visibility(ent) == visibility_external_visible ? 'F' : 'f',
		type_num,
		N_FUN,
		get_entity_ld_name(ent));
}  /* stabs_method */

/* close the stabs handler */
static void stabs_close(dbg_handle *handle) {
	stabs_handle *h = (stabs_handle *)handle;
	free(h);
}  /* stabs_close */

/** The stabs operations. */
static const debug_ops stabs_ops = {
	stabs_close,
	stabs_begin,
	stabs_method,
	stabs_line,
};

/* Opens a stabs handler */
dbg_handle *be_stabs_open(FILE *out) {
	stabs_handle *h = xmalloc(sizeof(*h));

	h->base.ops = &stabs_ops;
	h->f = out;
	return &h->base;
}  /* stabs_open */

/** close a debug handler. */
void be_dbg_close(dbg_handle *h) {
	if (h->ops->close)
		h->ops->close(h);
}  /* be_dbg_close */

/**
 * begin a new file
 */
void be_dbg_begin(dbg_handle *h, const char *filename) {
	if (h->ops->begin)
		h->ops->begin(h, filename);
}  /* be_dbg_begin */

/** debug for a function */
void be_dbg_method(dbg_handle *h, entity *ent) {
	if (h->ops->method)
		h->ops->method(h, ent);
}  /* be_dbg_method */

/** debug for line number */
void be_dbg_line(dbg_handle *h, unsigned lineno, const char *address) {
	if (h->ops->line)
		h->ops->line(h, lineno, address);
}  /* be_dbg_line */
