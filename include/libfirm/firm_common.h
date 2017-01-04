/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    common firm declarations
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier
 */
#ifndef FIRM_COMMON_FIRM_COMMON_H
#define FIRM_COMMON_FIRM_COMMON_H

#include "firm_types.h"
#include "begin.h"

/**
 * @defgroup initalization  Library Initialization
 * The functions in this section deal with initialization and deinitalization
 * of the libFirm library.
 * @{
 */

/**
 * Initializes the firm library; prepares code generation for the host machine.
 */
FIRM_API void ir_init(void);

/**
 * Initializes the firm library, prepare code generation for the machine
 * specified by @p target_triple. The specification should be in the form
 * 'cpu-vendor-os' or 'cpu-vendor-kernel' @see machine_triple.
 *
 * Returns 1 if successfull, 0 if @p target_triple is malformed or not
 * supported.
 */
FIRM_API int ir_init_target(const char *target_triple);

/**
 * Initialize the firm library, prepare code generation for the machine
 * specified by the parsed target triple @p machine.
 * @see ir_init_target()
 *
 * Returns 1 if successfull, 0 if the target is malformed or not supported.
 */
FIRM_API int ir_init_target_triple(ir_machine_triple_t const *machine);

/**
 * Initializes the firm library does not prepare for any code generation.
 */
FIRM_API void ir_init_no_target(void);

/**
 * Frees all memory occupied by the firm library.
 */
FIRM_API void ir_finish(void);

/** returns the libFirm major version number */
FIRM_API unsigned ir_get_version_major(void);
/** returns libFirm minor version number */
FIRM_API unsigned ir_get_version_minor(void);
/** returns libFirm micro version number */
FIRM_API unsigned ir_get_version_micro(void);
/** returns string describing libFirm revision */
FIRM_API const char *ir_get_version_revision(void);
/** returns string describing libFirm build */
FIRM_API const char *ir_get_version_build(void);

/**
 * A list of firm kinds.
 * Most important data structures in firm contain a firm_kind field at the
 * beginning so given void* pointer you can usually still guess the kind
 * of thing the pointer points to.
 * This is used in debug helper functions and printers.
 */
typedef enum firm_kind {
	k_BAD = 0,                /**< An invalid firm node. */
	k_entity,                 /**< An entity. */
	k_type,                   /**< A type. */
	k_ir_graph,               /**< An IR graph. */
	k_ir_node,                /**< An IR node. */
	k_ir_mode,                /**< An IR mode. */
	k_tarval,                 /**< A tarval. */
	k_ir_loop,                /**< A loop. */
	k_ir_max                  /**< maximum value -- illegal for firm nodes. */
} firm_kind;

/**
 * Returns the kind of a thing.
 *
 * @param firm_thing  pointer representing a firm object
 */
FIRM_API firm_kind get_kind(const void *firm_thing);

/** @} */

#include "end.h"

#endif
