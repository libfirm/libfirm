/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Entry point to the representation of a whole program.
 * @author  Goetz Lindenmaier
 * @date    2000
 * @brief
 *  Intermediate Representation (IR) of a program.
 *
 */
#ifndef FIRM_IR_IRPROG_H
#define FIRM_IR_IRPROG_H

#include <stddef.h>
#include "firm_types.h"
#include "irgraph.h"

#include "begin.h"

/**
 * @defgroup ir_prog Program
 *
 *  ir_prog keeps information about a program:
 *   - A reference point to the method to be executed on program start.
 *   - A list of all procedures.
 *   - A list of all types.
 *   - A global type that contains all global variables and procedures that do
 *     not belong to a class.  This type represents the data segment of the
 *     program.  It is not the base class of
 *     all classes in a class hierarchy (as, e.g., "object" in java).
 *   - A degenerated graph that contains constant expressions.
 *   - the output file name
 *
 * @{
 */

/**
 * Segment
 *
 * A progrom has a number of special segments at the toplevel which modify
 * the behaviour of the entities in them.
 */
typedef enum ir_segment_t {
	IR_SEGMENT_FIRST,
	/** "normal" global data */
	IR_SEGMENT_GLOBAL = IR_SEGMENT_FIRST,
	/** thread local storage segment */
	IR_SEGMENT_THREAD_LOCAL,
	/**
	 * the constructors segment. Contains pointers to functions which are
	 * executed on module initialization (program start or when a library is
	 * dynamically loaded)
	 */
	IR_SEGMENT_CONSTRUCTORS,
	/** like constructors, but functions are executed on module exit */
	IR_SEGMENT_DESTRUCTORS,
	/** java class registry */
	IR_SEGMENT_JCR,

	IR_SEGMENT_LAST = IR_SEGMENT_JCR
} ir_segment_t;
ENUM_COUNTABLE(ir_segment_t)

/**
 * A variable pointing to the current irp (program or module).
 * This variable should be considered constant. Moreover, one should use get_irp()
 * to get access the the irp.
 *
 * @note Think of the irp as the "handle" of a program.
 */
FIRM_API ir_prog *irp;

/**
 * Resources usable by algorithms modifying the program
 */
typedef enum irp_resources_t {
	IRP_RESOURCE_NONE         = 0,      /**< no resource */
	/** irg link field @see set_irg_link(), get_irg_link() */
	IRP_RESOURCE_IRG_LINK     = 1 << 0,
	/** entity link field @see set_entity_link(), get_entity_link() */
	IRP_RESOURCE_ENTITY_LINK  = 1 << 1,
	/** type visited field @see type_visited(), mark_type_visited(),
	 *  inc_master_type_visited() */
	IRP_RESOURCE_TYPE_VISITED = 1 << 2,
	/** type link field @see set_type_link(), get_type_link() */
	IRP_RESOURCE_TYPE_LINK    = 1 << 3,
} irp_resources_t;
ENUM_BITSET(irp_resources_t)

/**
 * Reserve resources available for a whole program.
 *
 * This is a debug tool: All code should properly allocate the resources it uses
 * so if two interlocked algorithms use the same resources that bug will get
 * detected.
 */
FIRM_API void irp_reserve_resources(ir_prog *irp, irp_resources_t resources);
/** Frees resources available for a whole program. */
FIRM_API void irp_free_resources(ir_prog *irp, irp_resources_t resources);
/** Returns currently reserved whole program resources. */
FIRM_API irp_resources_t irp_resources_reserved(const ir_prog *irp);

/**
 * Returns the current irp from where everything in the current module
 * can be accessed.
 *
 * @see irp
 */
FIRM_API ir_prog *get_irp(void);

/** Sets current irp */
FIRM_API void set_irp(ir_prog *irp);

/**
 * Creates a new ir_prog (a module or compilation unit).
 * Note: This does not set irp to the newly created ir_prog
 *
 * @param name  the name of this irp (module)
 */
FIRM_API ir_prog *new_ir_prog(const char *name);

/** Frees all memory used by irp.  Types in type list and irgs in irg
 *  list must be freed by hand before. */
FIRM_API void free_ir_prog(void);

/** Sets the file name / executable name or the like. Initially the
    ident 'no_name_set'. */
FIRM_API void set_irp_prog_name(ident *name);

/** Returns true if the user ever set a program name */
FIRM_API int irp_prog_name_is_set(void);

/** Returns the name of the current irp. */
FIRM_API ident *get_irp_ident(void);

/** Returns the name of the current irp. */
FIRM_API const char *get_irp_name(void);

/** Returns the main routine of the compiled program. */
FIRM_API ir_graph *get_irp_main_irg(void);

/** Sets the main routine of the compiled program. */
FIRM_API void set_irp_main_irg(ir_graph *main_irg);

/** returns the biggest not used irg index number */
FIRM_API size_t get_irp_last_idx(void);

/** Returns the number of ir graphs in the irp. */
FIRM_API size_t get_irp_n_irgs(void);

/** Returns the ir graph at position pos in the irp. */
FIRM_API ir_graph *get_irp_irg(size_t pos);

/** Sets the ir graph at position pos. */
FIRM_API void set_irp_irg(size_t pos, ir_graph *irg);

/**
 * Returns the type containing the entities for a segment.
 *
 * @param segment  the segment
 */
FIRM_API ir_type *get_segment_type(ir_segment_t segment);

/**
 * @brief Changes a segment segment type for the program.
 * (use with care)
 */
FIRM_API void set_segment_type(ir_segment_t segment, ir_type *new_type);

/**
 * Returns the "global" type of the irp.
 * Upon creation this is an empty class type.
 * This is a convenience function for get_segment_type(IR_SEGMENT_GLOBAL)
 */
FIRM_API ir_type *get_glob_type(void);

/**
 * Returns the "thread local storage" type of the irp.
 * Upon creation this is an empty struct type.
 * This is a convenience function for get_segment_type(IR_SEGMENT_THREAD_LOCAL)
 */
FIRM_API ir_type *get_tls_type(void);

/**
 * Returns global entity with name \p name.
 * A global entity is an entity in one of the segment types.
 * \see get_glob_type(), \see get_segment_type()
 */
FIRM_API ir_entity *ir_get_global(ident *name);

/**
 * Returns the number of all types in the irp.
 * @deprecated
 */
FIRM_API size_t get_irp_n_types(void);

/**
 * Returns the type at position pos in the irp.
 * @deprecated
 */
FIRM_API ir_type *get_irp_type(size_t pos);

/**
 * Overwrites the type at position pos with another type.
 * @deprecated
 */
FIRM_API void set_irp_type(size_t pos, ir_type *typ);

/**  Returns the graph for global constants of the current irp.
 *
 *   Returns an irgraph that only contains constant expressions for
 *   constant entities.  Do not use any access function for this
 *   graph, do not generate code for this graph.  This graph contains
 *   only one block.  The constant expressions may not contain control
 *   flow.
 *   Walking the graph starting from any node will not reach the block
 *   or any controlflow.
 *   See also copy_const_code() in entity.h.
 */
FIRM_API ir_graph *get_const_code_irg(void);

/** Returns callee info state for the whole program.
 * @see get_irg_callee_info_state() */
FIRM_API irg_callee_info_state get_irp_callee_info_state(void);
/** Sets callee info state for the whole program.
 * @see set_irg_callee_info_state() */
FIRM_API void set_irp_callee_info_state(irg_callee_info_state s);

/** Returns a new, unique label number. */
FIRM_API ir_label_t get_irp_next_label_nr(void);

/** Add a new global asm include. */
FIRM_API void add_irp_asm(ident *asm_string);

/** Returns the number of global asm includes. */
FIRM_API size_t get_irp_n_asms(void);

/** Returns the global asm include at position pos. */
FIRM_API ident *get_irp_asm(size_t pos);

/** @} */

#include "end.h"

#endif
