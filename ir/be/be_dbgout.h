#ifndef __BE_DBGOUT_H__
#define __BE_DBGOUT_H__
/**
 * Debug output support.
 *
 * @author Michael Beck
 * @date   11.9.2006
 * @cvsid  $Id$
 */
typedef struct dbg_handle dbg_handle;

/**
 * Debug operations.
 */
typedef struct debug_ops {
	/** close the stabs handler. */
	void (*close)(dbg_handle *h);

	/** begin a new file */
	void (*begin)(dbg_handle *handle, const char *filename);

	/** prints the stabs for a function */
	void (*method)(dbg_handle *h, entity *ent);

	/** prints a line number */
	void (*line)(dbg_handle *h, unsigned lineno, const char *address);
} debug_ops;

/** The base class of all debug implementations. */
struct dbg_handle {
	const debug_ops *ops;
};

/** close a debug handler. */
void be_dbg_close(dbg_handle *h);

/** begin a new file */
void be_dbg_begin(dbg_handle *handle, const char *filename);

/** debug for a function */
void be_dbg_method(dbg_handle *h, entity *ent);

/** debug for line number */
void be_dbg_line(dbg_handle *h, unsigned lineno, const char *address);

/** Opens a stabs handler. */
dbg_handle *be_stabs_open(FILE *out);

#endif /* __BE_DBGOUT_H__ */
