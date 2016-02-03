/** Common infrastructure for generating and emitting exception tables in backends.
 *
 * This module implements a map between throwing nodes (e.g. Call) and their
 * respective X_except blocks ("landing pads"). The map may be used by
 * exception handling (e.g., unwinding using libunwind) to obtain the catch
 * block that should be jumped to for a specific node that has thrown a runtime
 * exception.
 *
 * The mapping is constructed by walking a function's schedule and assigning a
 * unique assembly label to each throwing node ("be_exc_init").
 *
 * The backend is responsible for emission of these labels *after* the assembly
 * instruction of the throwing node ("be_exc_emit_irn_label"). Example:
 *
 *     main:
 *             ...
 *             call some_throwing_function
 *     .LE123: ...
 *
 * At the beginning of each function that contains a throwing node, a reference
 * to the function's exception table must be made ("be_exc_emit_function_prolog").
 *
 * At the end of each function that contains a throwing node, that function's
 * exception table should be emitted ("be_exc_emit_table").  The exception
 * table layout is as follows:
 *
 *     .quad <number of entries that follow>
 *     .quad <node 1 label> <node 1 X_except block label>
 *     [...]
 *     .quad <node N label> <node N X_except block label>
 *
 * Entries are in order of the schedule given to "be_exc_init", i.e. if an
 * instruction is placed "before" another in the assembler code, its exception
 * table entry is also placed before the other instruction's.  In other words,
 * exception table entries are ascending in the ("instruction pointer") address
 * of the "node" column. This allows for binary search when looking for the
 * exception table entry for a certain instruction pointer.
 *
 * After exception table emission, this module's internal state must be cleaned
 * up using "be_exc_finish".
 */
#include "firm_types.h"

#define FIRM_PERSONALITY_NAME "firm_personality"

/**
 * Initialize beexc and assign exception labels to any nodes in the schedule that
 * have "needs_exc_label" set.
 * @param irg_entity  The ir_entity of the function graph. This is used for
 *                    generating labels.
 * @param schedule    The block schedule.
 */
void be_exc_init(ir_entity const *irg_entity, ir_node *const *const schedule);

/**
 * Clean up beexc.
 */
void be_exc_finish(void);

/**
 * Emit exception handling setup function prolog.
 */
void be_exc_emit_function_prolog(void);

/**
 * Emit an exception label ("LE123:") for the given node.
 */
void be_exc_emit_irn_label(const ir_node*);

/**
 * Emit the exception table for the graph.
 * This calls "emit_entry" for every exception table entry.
 */
void be_exc_emit_table(void);
