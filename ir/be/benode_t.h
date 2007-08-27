/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @version     $Id$
 *
 * Backend node support for generic backend nodes.
 * This file provides Perm, Copy, Spill and Reload nodes.
 */
#ifndef FIRM_BE_BENODE_T_H
#define FIRM_BE_BENODE_T_H

#include <limits.h>

#include "firm_types.h"
#include "irnode.h"
#include "bearch.h"

#define BE_OUT_POS(p) (-((p) + 1))

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
extern ir_op *op_be_SetSP;
extern ir_op *op_be_RegParams;
extern ir_op *op_be_FrameAddr;
extern ir_op *op_be_Barrier;

typedef enum {
	beo_NoBeOp = -1,
	beo_Spill,
	beo_Reload,
	beo_Perm,
	beo_MemPerm,
	beo_Copy,
	beo_Keep,
	beo_CopyKeep,
	beo_Call,
	beo_Return,
	beo_AddSP,
	beo_SubSP,
	beo_IncSP,
	beo_SetSP,
	beo_RegParams,
	beo_FrameAddr,
	beo_Barrier,
	beo_Last
} be_opcode_t;

/** Not used yet. */
typedef enum {
	be_frame_flag_spill = 1,
	be_frame_flag_local = 2,
	be_frame_flag_arg   = 4
} be_frame_flag_t;

/**
 * A "symbolic constant" for the size of the stack frame to use with IncSP nodes.
 * It gets back-patched to the real size as soon it is known.
 */
#define BE_STACK_FRAME_SIZE_EXPAND INT_MAX
#define BE_STACK_FRAME_SIZE_SHRINK INT_MIN

/**
 * Determines if irn is a be_node.
 */
int is_be_node(const ir_node *irn);

/**
 * Create all BE specific opcodes.
 */
void be_node_init(void);

/**
 * Position numbers for the be_Spill inputs.
 */
enum {
	be_pos_Spill_frame = 0,
	be_pos_Spill_val   = 1
};

/**
 * Make a new Spill node.
 */
ir_node *be_new_Spill(const arch_register_class_t *cls, const arch_register_class_t *cls_frame,
	ir_graph *irg, ir_node *bl, ir_node *frame, ir_node *to_spill);

/**
 * Position numbers for the be_Reload inputs.
 */
enum {
	be_pos_Reload_frame = 0,
	be_pos_Reload_mem   = 1
};

/**
 * Make a new Reload node.
 */
ir_node *be_new_Reload(const arch_register_class_t *cls, const arch_register_class_t *cls_frame,
	ir_graph *irg, ir_node *bl, ir_node *frame, ir_node *mem, ir_mode *mode);

/**
 * Position numbers for the be_Copy inputs.
 */
enum {
	be_pos_Copy_op = 0
};

/**
 * Make a new Copy node.
 */
ir_node *be_new_Copy(const arch_register_class_t *cls, ir_graph *irg, ir_node *block, ir_node *in);
/** Returns the Copy Argument. */
ir_node *be_get_Copy_op(const ir_node *cpy);
/** Sets the Copy Argument. */
void be_set_Copy_op(ir_node *cpy, ir_node *op);

/**
 * Make a new Perm node.
 */
ir_node *be_new_Perm(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, int arity, ir_node *in[]);

/**
 * Reduce a Perm.
 * Basically, we provide a map to remap the Perm's arguments. If an entry in the
 * map is -1, the argument gets deleted.
 * This function takes care, that the register data and the input array reflects
 * the changes described by the map.
 * This is needed by the Perm optimization/movement in belower.c, see push_through_perm().
 * @param perm     The perm node.
 * @param new_size The new number of arguments (must be smaller or equal to the current one).
 * @param map      A map assigning each operand a new index (or -1 to indicate deletion).
 */
void be_Perm_reduce(ir_node *perm, int new_size, int *map);

/**
 * Create a new MemPerm node.
 */
ir_node *be_new_MemPerm(const arch_env_t *arch_env, ir_graph *irg, ir_node *bl, int n, ir_node *in[]);
ir_node *be_new_Keep(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, int arity, ir_node *in[]);

void be_Keep_add_node(ir_node *keep, const arch_register_class_t *cls, ir_node *node);

/**
 * Position numbers for the be_FrameAddr inputs
 */
enum {
	be_pos_FrameAddr_ptr = 0
};

/** Create a new FrameAddr node. */
ir_node *be_new_FrameAddr(const arch_register_class_t *cls_frame, ir_graph *irg, ir_node *bl, ir_node *frame, ir_entity *ent);

/** Return the frame input of a FrameAddr node. */
ir_node *be_get_FrameAddr_frame(ir_node *node);

/**
 * Position numbers for the be_AddSP inputs
 */
enum {
	be_pos_AddSP_old_sp = 0,
	be_pos_AddSP_size   = 1,
	be_pos_AddSP_last   = 2
};

enum {
	pn_be_AddSP_sp   = 0,
	pn_be_AddSP_res  = 1,
	pn_be_AddSP_M    = 2,
	pn_be_AddSP_last = 3
};

/**
 * Make a new AddSP node.
 * An AddSP node expresses an increase of the stack pointer in the direction the stack
 * grows. In contrast to IncSP, the amount of bytes the stack pointer is grown, is not
 * given by a constant but an ordinary Firm node.
 * @param sp     The stack pointer register.
 * @param irg    The graph.
 * @param bl     The block.
 * @param old_sp The node representing the old stack pointer value.
 * @param sz     The node expressing the size by which the stack pointer shall be grown.
 * @return       A new AddSP node.
 */
ir_node *be_new_AddSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, ir_node *sz);

/**
 * Position numbers for the be_SubSP inputs
 */
enum {
	be_pos_SubSP_old_sp = 0,
	be_pos_SubSP_size   = 1,
	be_pos_SubSP_last   = 2
};

enum {
	pn_be_SubSP_sp   = 0,
	pn_be_SubSP_M    = 1,
	pn_be_SubSP_last = 2
};

/**
 * Make a new SubSP node.
 * A SubSP node expresses a decrease of the stack pointer in the direction the stack
 * grows. In contrast to IncSP, the amount of bytes the stack pointer is grown, is not
 * given by a constant but an ordinary Firm node.
 * @param sp     The stack pointer register.
 * @param irg    The graph.
 * @param bl     The block.
 * @param old_sp The node representing the old stack pointer value.
 * @param sz     The node expressing the size by which the stack pointer shall be grown.
 * @return       A new DecSP node.
 */
ir_node *be_new_SubSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, ir_node *sz);

ir_node *be_new_SetSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, ir_node *operand, ir_node *mem);

/**
 * Make a stack pointer increase/decrease node.
 * @param sp     The stack pointer register.
 * @param irg    The graph to insert the node to.
 * @param bl     The block to insert the node into.
 * @param old_sp The node defining the former stack pointer.
 * @param amount The mount of bytes the stack shall be expanded/shrinked (see set_IncSP_offset)
 * @param dir    The direction in which the stack pointer shall be modified:
 *               Along the stack's growing direction or against.
 * @return       A new stack pointer increment/decrement node.
 * @note         This node sets a register constraint to the @p sp register on its output.
 */
ir_node *be_new_IncSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, int offset);

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

/** Gets the call entity or NULL if this is no static call. */
ir_entity  *be_Call_get_entity(const ir_node *call);
/** Sets the call entity. */
void     be_Call_set_entity(ir_node *call, ir_entity *ent);
/** Gets the call type. */
ir_type *be_Call_get_type(ir_node *call);
/** Sets the call type. */
void     be_Call_set_type(ir_node *call, ir_type *call_tp);

/**
 * Position numbers for the be_Call inputs.
 */
enum {
	be_pos_Call_mem       = 0,  /**< memory input of a be_Call node */
	be_pos_Call_sp        = 1,  /**< stack pointer input of a be_Call node */
	be_pos_Call_ptr       = 2,  /**< call pointer input of a be_Call node */
	be_pos_Call_first_arg = 3   /**< first argument input of a be_Call node */
};

/**
 * Projection numbers for result of be_Call node: use for Proj nodes!
 */
typedef enum {
	pn_be_Call_M_regular = pn_Call_M_regular,  /**< The memory result of a be_Call. */
	pn_be_Call_first_res = pn_Call_max         /**< The first result proj number of a be_Call. */
} pn_be_Call;

/**
 * Construct a new be_Call.
 *
 * @param dbg      debug info
 * @param irg      the graph where the call is placed
 * @param bl       the block where the call is placed
 * @param mem      the memory input of the call
 * @param sp       the stack pointer input of the call
 * @param ptr      the address of the called function, if immediate call set to sp
 * @param n_outs   the number of outcoming values from this call
 * @param n        the number of (register) inputs of this call
 * @param in       the (register) inputs of this call
 * @param call_tp  the call type of this call
 */
ir_node *be_new_Call(dbg_info *dbg, ir_graph *irg, ir_node *bl, ir_node *mem, ir_node *sp, ir_node *ptr,
                     int n_outs, int n, ir_node *in[], ir_type *call_tp);

/**
 * Position numbers for the be_Return inputs.
 */
enum {
	be_pos_Return_mem  = 0,     /**< memory input of a be_Return node */
	be_pos_Return_sp   = 1,     /**< stack pointer input of a be_Return node */
	be_pos_Return_val  = 2,     /**< first "real" return value if any */
};

/**
 * Construct a new be_Return.
 * @param irg    the graph where the new node will be placed
 * @param bl     the block where the new node will be placed
 * @param n_res  number of "real" results
 * @param n      number of inputs
 * @param in     input array
 */
ir_node *be_new_Return(dbg_info *dbg, ir_graph *irg, ir_node *bl, int n_res, int n, ir_node *in[]);

/** Returns the number of real returns values */
int be_Return_get_n_rets(ir_node *ret);

/** appends a node to the return node, returns the position of the node */
int be_Return_append_node(ir_node *ret, ir_node *node);

ir_node *be_new_RegParams(ir_graph *irg, ir_node *bl, int n_out);

ir_node *be_new_Barrier(ir_graph *irg, ir_node *bl, int n, ir_node *in[]);

/**
 * Appends a node to a barrier, returns the result proj of the node
 */
ir_node *be_Barrier_append_node(ir_node *barrier, ir_node *node);

/**
 * Appends a register out requirement to a RegParams node
 *
 * @returns the proj node for the new register
 */
ir_node *be_RegParams_append_out_reg(ir_node *regparams,
                                     const arch_env_t *arch_env,
                                     const arch_register_t *reg);

/**
 * Make a spill node.
 *
 * @param arch_env  The architecture environment.
 * @param irn       The node to be spilled.
 * @param spill_ctx The context in which the spill is introduced (This is mostly == irn up to the case of Phis).
 * @return          The new spill node.
 */
ir_node *be_spill(const arch_env_t *arch_env, ir_node *irn);

/**
 * Make a reload and insert it into the schedule.
 *
 * @param arch_env The architecture environment.
 * @param cls      The register class of the reloaded value.
 * @param insert   The node in the schedule in front of which the reload is inserted.
 * @param mode     The mode of the original (spilled) value.
 * @param spill    The spill node corresponding to this reload.
 * @return         A freshly made reload.
 */
ir_node *be_reload(const arch_env_t *arch_env, const arch_register_class_t *cls, ir_node *insert, ir_mode *mode, ir_node *spill);

enum {
	be_pos_CopyKeep_op = 0
};
ir_node *be_new_CopyKeep(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, ir_node *src, int n, ir_node *in_keep[], ir_mode *mode);
ir_node *be_new_CopyKeep_single(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, ir_node *src, ir_node *keep, ir_mode *mode);
ir_node *be_get_CopyKeep_op(const ir_node *cpy);
void be_set_CopyKeep_op(ir_node *cpy, ir_node *op);

/**
 * Get the backend opcode of a backend node.
 * @param irn The node.
 * @return The backend opcode.
 */
be_opcode_t be_get_irn_opcode(const ir_node *irn);

int be_is_Spill(const ir_node *irn);
int be_is_Reload(const ir_node *irn);
int be_is_Copy(const ir_node *irn);
int be_is_Perm(const ir_node *irn);
int be_is_MemPerm(const ir_node *irn);
int be_is_Keep(const ir_node *irn);
int be_is_CopyKeep(const ir_node *irn);
int be_is_Call(const ir_node *irn);
int be_is_Return(const ir_node *irn);
int be_is_IncSP(const ir_node *irn);
int be_is_SetSP(const ir_node *irn);
int be_is_AddSP(const ir_node *irn);
int be_is_SubSP(const ir_node *irn);
int be_is_RegParams(const ir_node *irn);
int be_is_FrameAddr(const ir_node *irn);
int be_is_Barrier(const ir_node *irn);

/**
 * Try to avoid this function and better call arch_get_frame_entity!
 *
 * Returns the frame entity used by the be node
 */
ir_entity *be_get_frame_entity(const ir_node *irn);

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

int be_get_MemPerm_entity_arity(const ir_node *irn);

/**
 * Impose a register constraint on a backend node.
 * @param irn The node.
 * @param pos The position of the argument/result. Results range from -1..-m and arguments form 0..n
 * @param reg The register which is admissible for that node, argument/result and position.
 */
void be_set_constr_single_reg(ir_node *irn, int pos, const arch_register_t *reg);

/**
 * Impose register constraints on a backend node.
 * The register subsets given by the limited function in @p req are copied to the backend node.
 * This requires that the constraint type of the @p req is arch_register_req_type_limited.
 * @param irn The backend node.
 * @param pos The position (@see be_set_constr_single_reg()).
 * @param req The register requirements which shall be transferred.
 */
void be_set_constr_limited(ir_node *irn, int pos, const arch_register_req_t *req);

/**
 * Set the flags of a node.
 * @param irn The node itself.
 * @param pos The position (0..n) for arguments, (-1..-m) for results.
 * @param flags The flags to set for that node and position.
 */
void be_node_set_flags(ir_node *irn, int pos, arch_irn_flags_t flags);

/**
 * Set the register class of a node.
 * @param irn The node itself.
 * @param pos The position (0..n) for arguments, (-1..-m) for results.
 * @param flags The register class to set for that node and position.
 */
void be_node_set_reg_class(ir_node *irn, int pos, const arch_register_class_t *cls);

/**
 * Set the register requirement type of a node.
 * @param irn The node itself.
 * @param pos The position (0..n) for arguments, (-1..-m) for results.
 * @param flags The register requirement type to set for that node and position.
 */
void be_node_set_req_type(ir_node *irn, int pos, arch_register_req_type_t type);

/**
 * Make a new phi handler.
 * @param env The architecture environment.
 * @return A new phi handler.
 */
arch_irn_handler_t *be_phi_handler_new(const arch_env_t *arch_env);

/**
 * Free a phi handler.
 * @param handler The handler to free.
 */
void be_phi_handler_free(arch_irn_handler_t *handler);

/**
 * Reset the register data in the phi handler.
 * This should be called on each new graph and deletes the register information of the current graph.
 */
void be_phi_handler_reset(arch_irn_handler_t *handler);

/**
 * Set the register requirements for a phi node.
 */
void be_set_phi_reg_req(const arch_env_t *arch_env, ir_node *phi,
                        const arch_register_req_t *req);

/*
 * Set flags for a phi node
 */
void be_set_phi_flags(const arch_env_t *arch_env, ir_node *phi,
                      arch_irn_flags_t flags);

/**
 * irn handler for common be nodes.
 */
extern const arch_irn_handler_t be_node_irn_handler;

#endif /* FIRM_BE_BENODE_T_H */
