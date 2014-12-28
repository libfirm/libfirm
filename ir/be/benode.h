/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Backend node support for generic backend nodes.
 * @author      Sebastian Hack
 * @date        17.05.2005
 *
 * Backend node support for generic backend nodes.
 * This file provides Perm, and Copy nodes.
 */
#ifndef FIRM_BE_BENODE_T_H
#define FIRM_BE_BENODE_T_H

#include <limits.h>

#include "firm_types.h"
#include "irnode_t.h"
#include "bearch.h"

typedef enum be_opcode {
	beo_AnyVal,
	beo_first = beo_AnyVal,
	beo_Copy,
	beo_CopyKeep,
	beo_IncSP,
	beo_Keep,
	beo_MemPerm,
	beo_Perm,
	beo_last  = beo_Perm
} be_opcode;

extern ir_op *op_be_AnyVal;
extern ir_op *op_be_Copy;
extern ir_op *op_be_CopyKeep;
extern ir_op *op_be_IncSP;
extern ir_op *op_be_Keep;
extern ir_op *op_be_MemPerm;
extern ir_op *op_be_Perm;

/**
 * Determines if irn is a be_node.
 */
bool is_be_node(const ir_node *irn);

be_opcode get_be_irn_opcode(const ir_node *node);

/**
 * Create all BE specific opcodes.
 */
void be_init_op(void);

void be_finish_op(void);

/**
 * Position numbers for the be_Copy inputs.
 */
enum {
	n_be_Copy_op = 0
};

/**
 * Make a new Copy node.
 */
ir_node *be_new_Copy(ir_node *block, ir_node *in);
/** Returns the Copy Argument. */
ir_node *be_get_Copy_op(const ir_node *cpy);

/**
 * Make a new Perm node.
 */
ir_node *be_new_Perm(arch_register_class_t const *cls, ir_node *block, int n,
                     ir_node *const *in);

/**
 * Reduce a Perm.
 * Basically, we provide a map to remap the Perm's arguments. If an entry in the
 * map is -1, the argument gets deleted.
 * This function takes care, that the register data and the input array reflects
 * the changes described by the map.
 * This is needed by the Perm optimization/movement in belower.c, see
 * push_through_perm().
 * @param perm     The perm node.
 * @param new_size The new number of arguments (must be smaller or equal to the
 *                 current one).
 * @param map      A map assigning each operand a new index (or -1 to indicate
 *                 deletion).
 */
void be_Perm_reduce(ir_node *perm, int new_size, int *map);

/**
 * Create a new MemPerm node.
 * A MemPerm node exchanges the values of memory locations. (Typically entities
 * used as spillslots). MemPerm nodes perform this operation without modifying
 * any register values.
 */
ir_node *be_new_MemPerm(ir_node *block, int n, ir_node *const *in);

ir_node *be_new_Keep(ir_node *block, int arity, ir_node *const *in);

void be_Keep_add_node(ir_node *keep, const arch_register_class_t *cls,
                      ir_node *node);

/**
 * Make a stack pointer increase/decrease node.
 * @param sp     The stack pointer register.
 * @param block  The block to insert the node into.
 * @param old_sp The node defining the former stack pointer.
 * @param offset amount the stack should expand (positive offset) or shrink
 *               (negative offset). Note that the offset is independent of the
 *               natural stack direction of the architecture but just specifies
 *               abstract expanding/shrinking of the stack area.
 * @param align  force stack alignment to this power of 2. (i.e. specifying 4
 *               results in a 2**4 = 16 bytes stack alignment)
 * @return       A new stack pointer increment/decrement node.
 * @note         This node sets a register constraint to the @p sp register on
 *               its output.
 */
ir_node *be_new_IncSP(const arch_register_t *sp, ir_node *block,
                      ir_node *old_sp, int offset, unsigned align);

/** Returns the previous node that computes the stack pointer. */
ir_node *be_get_IncSP_pred(ir_node *incsp);

/** Sets the previous node that computes the stack pointer. */
void be_set_IncSP_pred(ir_node *incsp, ir_node *pred);

/**
 * Sets a new offset to a IncSP node.
 * A positive offset means expanding the stack, a negative offset shrinking
 * an offset is == BE_STACK_FRAME_SIZE will be replaced by the real size of the
 * stackframe in the fix_stack_offsets phase.
 */
void be_set_IncSP_offset(ir_node *irn, int offset);

/** Gets the offset from a IncSP node. */
int be_get_IncSP_offset(const ir_node *irn);
/** Return requested stack alignment (as a logarithm of two, i.e. 4 means
 * the stack alignment will be 2**4=16 bytes) */
unsigned be_get_IncSP_align(const ir_node *irn);

enum {
	n_be_CopyKeep_op,
	n_be_CopyKeep_max = n_be_CopyKeep_op
};
ir_node *be_new_CopyKeep(ir_node *block, ir_node *src, int n, ir_node *const *in_keep);

ir_node *be_new_CopyKeep_single(ir_node *block, ir_node *src, ir_node *keep);

ir_node *be_get_CopyKeep_op(const ir_node *cpy);

void be_set_CopyKeep_op(ir_node *cpy, ir_node *op);

void be_set_MemPerm_in_entity(const ir_node *irn, unsigned n, ir_entity* ent);
ir_entity *be_get_MemPerm_in_entity(const ir_node *irn, unsigned n);

void be_set_MemPerm_out_entity(const ir_node *irn, unsigned n, ir_entity* ent);
ir_entity *be_get_MemPerm_out_entity(const ir_node *irn, unsigned n);

void be_set_MemPerm_offset(ir_node *irn, int offset);
int be_get_MemPerm_offset(const ir_node *irn);

unsigned be_get_MemPerm_entity_arity(const ir_node *irn);

/**
 * Create a AnyVal node. Use of this node should be avoided!
 * The node is used as input at places where we need an input register assigned
 * but don't care about its contents. This is for example necessary to fixup
 * nodes which are not register pressure faithfull.
 */
ir_node *be_new_AnyVal(ir_node *block, const arch_register_class_t *cls);

/**
 * Impose a register constraint on a backend node.
 * @param irn The node.
 * @param pos The position of the argument.
 * @param reg The register which is admissible for that node, argument/result
 *            and position.
 */
void be_set_constr_single_reg_out(ir_node *irn, int pos,
		const arch_register_t *reg, arch_register_req_type_t additional_flags);

const arch_register_req_t *be_create_reg_req(struct obstack *obst,
		const arch_register_t *reg, arch_register_req_type_t additional_types);

/**
 * Set the register requirements for a phi node.
 */
void be_set_phi_reg_req(ir_node *phi, const arch_register_req_t *req);

void be_dump_phi_reg_reqs(FILE *out, const ir_node *node, dump_reason_t reason);

/**
 * Creates a new phi with associated backend informations
 */
ir_node *be_new_Phi(ir_node *block, int n_ins, ir_node **ins, ir_mode *mode,
                    const arch_register_req_t *req);

/**
 * Search for output of start node with a specific register
 */
ir_node *be_get_initial_reg_value(ir_graph *irg, const arch_register_t *reg);

static inline bool be_is_Copy    (const ir_node *irn) { return get_irn_op(irn) == op_be_Copy     ; }
static inline bool be_is_CopyKeep(const ir_node *irn) { return get_irn_op(irn) == op_be_CopyKeep ; }
static inline bool be_is_Perm    (const ir_node *irn) { return get_irn_op(irn) == op_be_Perm     ; }
static inline bool be_is_MemPerm (const ir_node *irn) { return get_irn_op(irn) == op_be_MemPerm  ; }
static inline bool be_is_Keep    (const ir_node *irn) { return get_irn_op(irn) == op_be_Keep     ; }
static inline bool be_is_IncSP   (const ir_node *irn) { return get_irn_op(irn) == op_be_IncSP    ; }

#endif
