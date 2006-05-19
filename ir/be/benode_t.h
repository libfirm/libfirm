/**
 * @file   benode_t.h
 * @date   17.05.2005
 * @author Sebastian Hack
 *
 * Backend node support.
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _BENODE_T_H
#define _BENODE_T_H

#include "firm_config.h"

#include "irmode.h"
#include "irnode.h"
#include "entity_t.h"

#include "be_t.h"
#include "bearch.h"

#define BE_OUT_POS(p) (-((p) + 1))

/**
 * The benode op's.  Must be available to register emitter function.
 */
extern ir_op *op_be_Spill;
extern ir_op *op_be_Reload;
extern ir_op *op_be_Perm;
extern ir_op *op_be_Copy;
extern ir_op *op_be_Keep;
extern ir_op *op_be_CopyKeep;
extern ir_op *op_be_Call;
extern ir_op *op_be_Return;
extern ir_op *op_be_IncSP;
extern ir_op *op_be_AddSP;
extern ir_op *op_be_SetSP;
extern ir_op *op_be_RegParams;
extern ir_op *op_be_StackParam;
extern ir_op *op_be_FrameAddr;
extern ir_op *op_be_FrameLoad;
extern ir_op *op_be_FrameStore;
extern ir_op *op_be_Barrier;

typedef enum {
	beo_NoBeOp = 0,
	beo_Spill,
	beo_Reload,
	beo_Perm,
	beo_Copy,
	beo_Keep,
	beo_CopyKeep,
	beo_Call,
	beo_Return,
	beo_AddSP,
	beo_IncSP,
	beo_SetSP,
	beo_RegParams,
	beo_StackParam,
	beo_FrameLoad,
	beo_FrameStore,
	beo_FrameAddr,
	beo_Barrier,
	beo_Last
} be_opcode_t;

/** Expresses the direction of the stack pointer increment of IncSP nodes. */
typedef enum {
	be_stack_dir_expand = 0,
	be_stack_dir_shrink = 1
} be_stack_dir_t;

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
#define BE_STACK_FRAME_SIZE ((unsigned) -1)

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
ir_node *be_new_Spill(const arch_register_class_t *cls, const arch_register_class_t *cls_frame, ir_graph *irg, ir_node *bl, ir_node *frame, ir_node *node_to_spill, ir_node *ctx);

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
ir_node *be_new_Reload(const arch_register_class_t *cls, const arch_register_class_t *cls_frame, ir_graph *irg, ir_node *bl, ir_node *frame, ir_node *spill_node, ir_mode *mode);

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
ir_node *be_new_Keep(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, int arity, ir_node *in[]);

ir_node *be_new_FrameLoad(const arch_register_class_t *cls_frame, const arch_register_class_t *cls_data,
						  ir_graph *irg, ir_node *bl, ir_node *mem, ir_node *frame, entity *ent);
ir_node *be_new_FrameStore(const arch_register_class_t *cls_frame, const arch_register_class_t *cls_data,
						   ir_graph *irg, ir_node *bl, ir_node *mem, ir_node *frame, ir_node *data, entity *ent);
ir_node *be_new_FrameAddr(const arch_register_class_t *cls_frame, ir_graph *irg, ir_node *bl, ir_node *frame, entity *ent);

/**
 * Position numbers for the be_AddSP inputs
 */
enum {
	be_pos_AddSP_old_sp = 0,
	be_pos_AddSP_size   = 1,
	be_pos_AddSP_last   = 2
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

ir_node *be_new_SetSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, ir_node *operand, ir_node *mem);

/**
 * Make a stack pointer increase/decrease node.
 * @param sp     The stack pointer register.
 * @param irg    The graph to insert the node to.
 * @param bl     The block to insert the node into.
 * @param old_sp The node defining the former stack pointer.
 * @param amount The mount of bytes the stack pointer shall be increased/decreased.
 * @param dir    The direction in which the stack pointer shall be modified:
 *               Along the stack's growing direction or against.
 * @return       A new stack pointer increment/decrement node.
 * @note         This node sets a register constraint to the @p sp register on its output.
 */
ir_node *be_new_IncSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, ir_node *mem, unsigned amount, be_stack_dir_t dir);

/** Returns the previous node that computes the stack pointer. */
ir_node *be_get_IncSP_pred(ir_node *incsp);

/** Sets the previous node that computes the stack pointer. */
void     be_set_IncSP_pred(ir_node *incsp, ir_node *pred);

/** Returns the memory input of the IncSP. */
ir_node *be_get_IncSP_mem(ir_node *irn);

/** Sets a new offset to a IncSP node. */
void     be_set_IncSP_offset(ir_node *irn, unsigned offset);

/** Gets the offset from a IncSP node. */
unsigned be_get_IncSP_offset(const ir_node *irn);

/** Sets a new direction to a IncSP node. */
void           be_set_IncSP_direction(ir_node *irn, be_stack_dir_t dir);

/** Gets the direction from a IncSP node. */
be_stack_dir_t be_get_IncSP_direction(const ir_node *irn);

/** Gets the call entity or NULL if this is no static call. */
entity  *be_Call_get_entity(const ir_node *call);
/** Sets the call entity. */
void     be_Call_set_entity(ir_node *call, entity *ent);
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
 * Construct a new be_Call
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

/**
 * Construct a new Stack Parameter node.
 */
ir_node *be_new_StackParam(const arch_register_class_t *cls, const arch_register_class_t *cls_frame, ir_graph *irg, ir_node *bl, ir_mode *mode, ir_node *frame_pointer, entity *ent);
ir_node *be_new_RegParams(ir_graph *irg, ir_node *bl, int n_out);

ir_node *be_new_Barrier(ir_graph *irg, ir_node *bl, int n, ir_node *in[]);

/**
 * Make a spill node and insert it into the schedule.
 *
 * @param arch_env  The architecture environment.
 * @param irn       The node to be spilled.
 * @param spill_ctx The context in which the spill is introduced (This is mostly == irn up to the case of Phis).
 * @return          The new spill node.
 */
ir_node *be_spill(const arch_env_t *arch_env, ir_node *irn, ir_node *spill_ctx);

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
int be_is_Keep(const ir_node *irn);
int be_is_CopyKeep(const ir_node *irn);
int be_is_Call(const ir_node *irn);
int be_is_Return(const ir_node *irn);
int be_is_IncSP(const ir_node *irn);
int be_is_SetSP(const ir_node *irn);
int be_is_AddSP(const ir_node *irn);
int be_is_RegParams(const ir_node *irn);
int be_is_StackParam(const ir_node *irn);
int be_is_FrameAddr(const ir_node *irn);
int be_is_FrameLoad(const ir_node *irn);
int be_is_FrameStore(const ir_node *irn);
int be_is_Barrier(const ir_node *irn);

/**
 * Get the entity on the stack frame the given node uses.
 * @param irn The node.
 * @return The entity on the stack frame used by the node or NULL,
 *         if the node does not access the stack frame or is no back-end node.
 *
 */
entity *be_get_frame_entity(const ir_node *irn);

void   be_set_Spill_entity(ir_node *irn, entity *ent);
entity *be_get_spill_entity(const ir_node *irn);

ir_node *be_get_Spill_context(const ir_node *irn);

/**
 * Set the entities of a Reload to the ones of the Spill it is pointing to.
 * @param irg The graph.
 */
void be_copy_entities_to_reloads(ir_graph *irg);

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
 * irn handler for common be nodes.
 */
extern const arch_irn_handler_t be_node_irn_handler;

#endif /* _BENODE_T_H */
