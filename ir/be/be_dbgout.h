#ifndef __BE_DBGOUT_H__
#define __BE_DBGOUT_H__
/**
 * Debug output support.
 *
 * @author Michael Beck
 * @date   11.9.2006
 * @cvsid  $Id$
 */
#include "obst.h"

typedef struct dbg_handle dbg_handle;

/**
 * Debug operations.
 */
typedef struct debug_ops {
	/** close the stabs handler. */
	void (*close)(dbg_handle *handle);

	/** start a new source object (compilation unit) */
	void (*so)(dbg_handle *handle, const char *filename);

	/** Main Program */
	void (*main_program)(dbg_handle *handle);

	/** dumps the stabs for a function */
	void (*method)(dbg_handle *handle, entity *ent);

	/** dumps a line number */
	void (*line)(dbg_handle *handle, unsigned lineno, const char *address);

	/** dump types */
	void (*types)(dbg_handle *handle);

	/** dump a global */
	void (*global)(dbg_handle *h, struct obstack *obst, entity *ent);

} debug_ops;

/** The base class of all debug implementations. */
struct dbg_handle {
	const debug_ops *ops;
};

/** close a debug handler. */
void be_dbg_close(dbg_handle *handle);

/** start a new source object (compilation unit) */
void be_dbg_so(dbg_handle *handle, const char *filename);

/** Main program */
void be_dbg_main_program(dbg_handle *handle);

/** debug for a function */
void be_dbg_method(dbg_handle *handle, entity *ent);

/** debug for line number */
void be_dbg_line(dbg_handle *handle, unsigned lineno, const char *address);

/** dump types */
void be_dbg_types(dbg_handle *handle);

/** dump a global */
void be_dbg_global(dbg_handle *handle, struct obstack *obst, entity *ent);

/** Opens a stabs handler. */
dbg_handle *be_stabs_open(FILE *out);

#endif /* __BE_DBGOUT_H__ */
