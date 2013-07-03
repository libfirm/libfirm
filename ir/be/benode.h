/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       Backend node support for generic backend nodes.
 * @author      Sebastian Hack
 * @date        17.05.2005
 *
 * Backend node support for generic backend nodes.
 * This file provides Perm, Copy, Spill and Reload nodes.
 */
#ifndef FIRM_BE_BENODE_T_H
#define FIRM_BE_BENODE_T_H

#include <limits.h>

#include "firm_types.h"
#include "irnode_t.h"
#include "bearch.h"

/**
 * The benode op's.  Must be available to register emitter function.
 */
extern ir_op *op_be_Spill;
extern ir_op *op_be_Reload;
extern ir_op *op_be_Perm;
extern ir_op *op_be_MemPerm;
extern ir_op *op_be_Copy;
extern ir_op *op_be_Keep;
extern ir_op *op_be_CopyKeep;
extern ir_op *op_be_Call;
extern ir_op *op_be_Return;
extern ir_op *op_be_IncSP;
extern ir_op *op_be_AddSP;
extern ir_op *op_be_SubSP;
extern ir_op *op_be_Start;
extern ir_op *op_be_FrameAddr;

/**
 * Determines if irn is a be_node.
 */
int is_be_node(const ir_node *irn);

/**
 * Create all BE specific opcodes.
 */
void be_init_op(void);

void be_finish_op(void);

/**
 * Position numbers for the be_Spill inputs.
 */
enum {
	n_be_Spill_frame = 0,
	n_be_Spill_val   = 1
};

/**
 * Make a new Spill node.
 */
ir_node *be_new_Spill(const arch_register_class_t *cls,
                      const arch_register_class_t *cls_frame, ir_node *block,
                      ir_node *frame, ir_node *to_spill);

/**
 * Position numbers for the be_Reload inputs.
 */
enum {
	n_be_Reload_frame = 0,
	n_be_Reload_mem   = 1
};

/**
 * Make a new Reload node.
 */
ir_node *be_new_Reload(const arch_register_class_t *cls,
                       const arch_register_class_t *cls_frame, ir_node *block,
                       ir_node *frame, ir_node *mem, ir_mode *mode);

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
ir_node *be_new_Perm(const arch_register_class_t *cls, ir_node *block,
                     int n, ir_node *in[]);

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
ir_node *be_new_MemPerm(ir_node *block, int n, ir_node *in[]);
ir_node *be_new_Keep(ir_node *block, int arity, ir_node *in[]);

void be_Keep_add_node(ir_node *keep, const arch_register_class_t *cls,
                      ir_node *node);

/**
 * Position numbers for the be_FrameAddr inputs
 */
enum {
	n_be_FrameAddr_ptr = 0
};

/** Create a new FrameAddr node. */
ir_node *be_new_FrameAddr(const arch_register_class_t *cls_frame,
                          ir_node *block, ir_node *frame, ir_entity *ent);

/** Return the frame input of a FrameAddr node. */
ir_node *be_get_FrameAddr_frame(const ir_node *node);

ir_entity *be_get_FrameAddr_entity(const ir_node *node);

/**
 * Position numbers for the be_AddSP inputs
 */
enum {
	n_be_AddSP_old_sp = 0,
	n_be_AddSP_size   = 1,
	n_be_AddSP_last   = 2
};

enum {
	pn_be_AddSP_sp   = 0,
	pn_be_AddSP_res  = 1,
	pn_be_AddSP_M    = 2,
	pn_be_AddSP_last = 3
};

/**
 * Make a new AddSP node.
 * An AddSP node expresses an increase of the stack pointer in the direction
 * the stack grows. In contrast to IncSP, the amount of bytes the stack pointer
 * is grown, is not given by a constant but an ordinary Firm node.
 * @param sp     The stack pointer register.
 * @param block  The block.
 * @param old_sp The node representing the old stack pointer value.
 * @param size   The node expressing the size by which the stack pointer shall
 *               be grown.
 * @return       A new AddSP node.
 */
ir_node *be_new_AddSP(const arch_register_t *sp, ir_node *block,
                      ir_node *old_sp, ir_node *size);

/**
 * Position numbers for the be_SubSP inputs
 */
enum {
	n_be_SubSP_old_sp = 0,
	n_be_SubSP_size   = 1,
	n_be_SubSP_last   = 2
};

enum {
	pn_be_SubSP_sp   = 0,
	pn_be_SubSP_M    = 1,
	pn_be_SubSP_last = 2
};

/**
 * Make a new SubSP node.
 * A SubSP node expresses a decrease of the stack pointer in the direction the
 * stack grows. In contrast to IncSP, the amount of bytes the stack pointer is
 * grown, is not given by a constant but an ordinary Firm node.
 * @param sp     The stack pointer register.
 * @param block  The block.
 * @param old_sp The node representing the old stack pointer value.
 * @param size   The node expressing the size by which the stack pointer shall
 *               be grown.
 * @return       A new DecSP node.
 */
ir_node *be_new_SubSP(const arch_register_t *sp, ir_node *block,
                      ir_node *old_sp, ir_node *size);

/**
 * Make a stack pointer increase/decrease node.
 * @param sp     The stack pointer register.
 * @param block  The block to insert the node into.
 * @param old_sp The node defining the former stack pointer.
 * @param offset amount the stack should expand (positive offset) or shrink
 *               (negative offset). Note that the offset is independent of the
 *               natural stack direction of the architecture but just specifies
 *               abstract expanding/shrinking of the stack area.
 * @param align  force stack alignment to this power of 2. (ie. specifying 3
 *               results in a 2^3 = 8byte stack alignment)
 * @return       A new stack pointer increment/decrement node.
 * @note         This node sets a register constraint to the @p sp register on
 *               its output.
 */
ir_node *be_new_IncSP(const arch_register_t *sp, ir_node *block,
                      ir_node *old_sp, int offset, int align);

/** Returns the previous node that computes the stack pointer. */
ir_node *be_get_IncSP_pred(ir_node *incsp);

/** Sets the previous node that computes the stack pointer. */
void     be_set_IncSP_pred(ir_node *incsp, ir_node *pred);

/**
 * Sets a new offset to a IncSP node.
 * A positive offset means expanding the stack, a negative offset shrinking
 * an offset is == BE_STACK_FRAME_SIZE will be replaced by the real size of the
 * stackframe in the fix_stack_offsets phase.
 */
void     be_set_IncSP_offset(ir_node *irn, int offset);

/** Gets the offset from a IncSP node. */
int be_get_IncSP_offset(const ir_node *irn);
int be_get_IncSP_align(const ir_node *irn);

/** Gets the call entity or NULL if this is no static call. */
ir_entity  *be_Call_get_entity(const ir_node *call);
/** Sets the call entity. */
void     be_Call_set_entity(ir_node *call, ir_entity *ent);
/** Gets the call type. */
ir_type *be_Call_get_type(ir_node *call);
/** Sets the call type. */
void     be_Call_set_type(ir_node *call, ir_type *call_tp);

void     be_Call_set_pop(ir_node *call, unsigned pop);

unsigned be_Call_get_pop(const ir_node *call);

/**
 * Position numbers for the be_Call inputs.
 */
enum {
	n_be_Call_mem       = 0,  /**< memory input of a be_Call node */
	n_be_Call_sp        = 1,  /**< stack pointer input of a be_Call node */
	n_be_Call_ptr       = 2,  /**< call pointer input of a be_Call node */
	n_be_Call_first_arg = 3   /**< first argument input of a be_Call node */
};

/**
 * Projection numbers for result of be_Call node: use for Proj nodes!
 */
typedef enum {
	pn_be_Call_M         = pn_Call_M, /**< The memory result of a be_Call. */
	pn_be_Call_X_regular = pn_Call_X_regular,
	pn_be_Call_X_except  = pn_Call_X_except,
	pn_be_Call_sp        = pn_Call_max+1,
	pn_be_Call_first_res     /**< The first result proj number of a be_Call. */
} pn_be_Call;

/**
 * Construct a new be_Call.
 *
 * @param dbg      debug info
 * @param irg      the graph where the call is placed
 * @param block    the block where the call is placed
 * @param mem      the memory input of the call
 * @param sp       the stack pointer input of the call
 * @param ptr      the address of the called function, if immediate call set
 *                 to sp
 * @param n_outs   the number of outcoming values from this call
 * @param n        the number of (register) inputs of this call
 * @param in       the (register) inputs of this call
 * @param call_tp  the call type of this call
 */
ir_node *be_new_Call(dbg_info *dbg, ir_graph *irg, ir_node *block, ir_node *mem,
                     const arch_register_req_t *sp_req, ir_node *sp,
                     const arch_register_req_t *ptr_req, ir_node *ptr,
                     int n_outs, int n, ir_node *in[], ir_type *call_tp);

/**
 * Position numbers for the be_Return inputs.
 */
enum {
	n_be_Return_mem  = 0,     /**< memory input of a be_Return node */
	n_be_Return_sp   = 1,     /**< stack pointer input of a be_Return node */
	n_be_Return_val  = 2,     /**< first "real" return value if any */
};

/**
 * Construct a new be_Return.
 *
 * @param dbg    debug info
 * @param irg    the graph where the new node will be placed
 * @param block  the block where the new node will be placed
 * @param n_res  number of "real" results
 * @param pop    pop number of bytes on return
 * @param n      number of inputs
 * @param in     input array
 */
ir_node *be_new_Return(dbg_info *dbg, ir_graph *irg, ir_node *block, int n_res,
                       unsigned pop, int n, ir_node *in[]);

/** Returns the number of real returns values */
int be_Return_get_n_rets(const ir_node *ret);

/**
 * Return the number of bytes that should be popped from stack when executing
 * the Return.
 *
 * @param ret  the be_Return node
 */
unsigned be_Return_get_pop(const ir_node *ret);

/**
 * Return non-zero, if number of popped bytes must be always emitted.
 *
 * @param ret  the be_Return node
 */
int be_Return_get_emit_pop(const ir_node *ret);

/**
 * Set the emit_pop flag.
 *
 * @param ret  the be_Return node
 */
void be_Return_set_emit_pop(ir_node *ret, int emit_pop);

ir_node *be_new_Start(dbg_info *dbgi, ir_node *block, int n_out);

enum {
	n_be_CopyKeep_op = 0
};
ir_node *be_new_CopyKeep(ir_node *block, ir_node *src,
                         int n, ir_node *in_keep[]);

ir_node *be_new_CopyKeep_single(ir_node *block, ir_node *src, ir_node *keep);

ir_node *be_get_CopyKeep_op(const ir_node *cpy);

void be_set_CopyKeep_op(ir_node *cpy, ir_node *op);

/**
 * Returns the frame entity of a be node.
 * Try to avoid this function and better call arch_get_frame_entity!
 *
 * @return the frame entity used by the be node
 */
ir_entity *be_get_frame_entity(const ir_node *irn);

void be_node_set_frame_entity(ir_node *node, ir_entity *entity);

/**
 * Returns the frame offset of this node.
 */
int be_get_frame_offset(const ir_node *irn);

ir_node* be_get_Reload_mem(const ir_node *irn);
ir_node *be_get_Reload_frame(const ir_node *irn);
ir_node* be_get_Spill_val(const ir_node *irn);
ir_node *be_get_Spill_frame(const ir_node *irn);

void be_set_MemPerm_in_entity(const ir_node *irn, int n, ir_entity* ent);
ir_entity *be_get_MemPerm_in_entity(const ir_node *irn, int n);

void be_set_MemPerm_out_entity(const ir_node *irn, int n, ir_entity* ent);
ir_entity *be_get_MemPerm_out_entity(const ir_node *irn, int n);

void be_set_MemPerm_offset(ir_node *irn, int offset);
int be_get_MemPerm_offset(const ir_node *irn);

int be_get_MemPerm_entity_arity(const ir_node *irn);

/**
 * Impose a register constraint on a backend node.
 * @param irn The node.
 * @param pos The position of the argument.
 * @param reg The register which is admissible for that node, argument/result
 *            and position.
 */
void be_set_constr_single_reg_in(ir_node *irn, int pos,
		const arch_register_t *reg, arch_register_req_type_t additional_flags);
void be_set_constr_single_reg_out(ir_node *irn, int pos,
		const arch_register_t *reg, arch_register_req_type_t additional_flags);

const arch_register_req_t *be_create_reg_req(struct obstack *obst,
		const arch_register_t *reg, arch_register_req_type_t additional_types);

/**
 * Impose register constraints on a backend node.
 * The register subsets given by the limited function in @p req are copied to
 * the backend node. This requires that the constraint type of the @p req is
 * arch_register_req_type_limited.
 * @param irn The backend node.
 * @param pos The position (@see be_set_constr_single_reg()).
 * @param req The register requirements which shall be transferred.
 */
void be_set_constr_in(ir_node *irn, int pos, const arch_register_req_t *req);
void be_set_constr_out(ir_node *irn, int pos, const arch_register_req_t *req);

/**
 * Set the register class of a node.
 * @param irn The node itself.
 * @param pos The position (0..n) for arguments
 * @param flags The register class to set for that node and position.
 */
void be_node_set_reg_class_in(ir_node *irn, int pos,
                              const arch_register_class_t *cls);
void be_node_set_reg_class_out(ir_node *irn, int pos,
                               const arch_register_class_t *cls);

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

/**
 * Search for input of a return node with a specific register and return
 * its number.
 */
int be_find_return_reg_input(ir_node *ret, const arch_register_t *reg);

static inline int be_is_Spill    (const ir_node *irn) { return get_irn_opcode(irn) == beo_Spill    ; }
static inline int be_is_Reload   (const ir_node *irn) { return get_irn_opcode(irn) == beo_Reload   ; }
static inline int be_is_Copy     (const ir_node *irn) { return get_irn_opcode(irn) == beo_Copy     ; }
static inline int be_is_CopyKeep (const ir_node *irn) { return get_irn_opcode(irn) == beo_CopyKeep ; }
static inline int be_is_Perm     (const ir_node *irn) { return get_irn_opcode(irn) == beo_Perm     ; }
static inline int be_is_MemPerm  (const ir_node *irn) { return get_irn_opcode(irn) == beo_MemPerm  ; }
static inline int be_is_Keep     (const ir_node *irn) { return get_irn_opcode(irn) == beo_Keep     ; }
static inline int be_is_Call     (const ir_node *irn) { return get_irn_opcode(irn) == beo_Call     ; }
static inline int be_is_Return   (const ir_node *irn) { return get_irn_opcode(irn) == beo_Return   ; }
static inline int be_is_IncSP    (const ir_node *irn) { return get_irn_opcode(irn) == beo_IncSP    ; }
static inline int be_is_AddSP    (const ir_node *irn) { return get_irn_opcode(irn) == beo_AddSP    ; }
static inline int be_is_SubSP    (const ir_node *irn) { return get_irn_opcode(irn) == beo_SubSP    ; }
static inline int be_is_Start    (const ir_node *irn) { return get_irn_opcode(irn) == beo_Start    ; }
static inline int be_is_FrameAddr(const ir_node *irn) { return get_irn_opcode(irn) == beo_FrameAddr; }

#endif
