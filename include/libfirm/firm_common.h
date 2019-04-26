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
 * Initializes the firm library and prepares code generation for the host.
 *
 * This is a convenince function that does:
 *
 *     ir_init_library();
 *     ir_target_set_triple(ir_get_host_machine_triple());
 *     ir_target_init();
 */
FIRM_API void ir_init(void);

/**
 * Initializes the firm library but does not prepare for any code generation.
 * This is typically followed by target initialization (\see target,
 * ir_target_set(), ir_target_set_triple(), ir_target_init()) to get mode_P
 * defined, a number of transformations also depend on target information being
 * available.
 */
FIRM_API void ir_init_library(void);

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
