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
#include "pmap.h"

#include "irmode.h"
#include "irnode.h"
#include "entity_t.h"

#include "be_t.h"
#include "bearch.h"

typedef enum {
	beo_NoBeOp = 0,
	beo_Spill,
	beo_Reload,
	beo_Perm,
	beo_Copy,
	beo_Keep,
	beo_NoReg,
	beo_Call,
	beo_AddSP,
	beo_IncSP,
	beo_RegParams,
	beo_StackParam,
	beo_Last
} be_opcode_t;

typedef enum {
	be_stack_dir_along = 0,
	be_stack_dir_against = 1
} be_stack_dir_t;

void be_node_init(void);

const arch_irn_handler_t be_node_irn_handler;

ir_node *be_new_Spill(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, ir_node *node_to_spill, ir_node *ctx);
ir_node *be_new_Reload(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, ir_mode *mode, ir_node *spill_node);
ir_node *be_new_Copy(const arch_register_class_t *cls, ir_graph *irg, ir_node *block, ir_node *in);
ir_node *be_new_Perm(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, int arity, ir_node *in[]);
ir_node *be_new_Keep(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, int arity, ir_node *in[]);

ir_node *be_new_AddSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, ir_node *operand);

/**
 * Make a stack pointer increase/decrease node.
 * @param sp     The stack poitner register.
 * @param irg    The graph to insert the node to.
 * @param bl     The block to insert the node into.
 * @param old_sp The node defining the former stack pointer.
 * @param amount The mount of bytes the stack pointer shall be increased/decreased.
 * @param dir    The direction in which the stack pointer shall be modified:
 *               Along the stack's growing direction or against.
 * @return       A new stack pointer increment/decrement node.
 * @note         This node sets a register constraint to the @p sp register on its output.
 */
ir_node *be_new_IncSP(const arch_register_t *sp, ir_graph *irg, ir_node *bl, ir_node *old_sp, unsigned amount, be_stack_dir_t dir);

void     be_set_IncSP_offset(ir_node *irn, unsigned offset);
unsigned be_get_IncSP_offset(ir_node *irn);

void           be_set_IncSP_direction(ir_node *irn, be_stack_dir_t dir);
be_stack_dir_t be_get_IncSP_direction(ir_node *irn);

ir_node *be_new_Call(ir_graph *irg, ir_node *bl, ir_node *mem, ir_node *sp, ir_node *ptr, int n_outs, int n, ir_node *in[]);
ir_node *be_new_StackParam(const arch_register_class_t *cls, ir_graph *irg, ir_node *bl, ir_mode *mode, ir_node *frame_pointer, unsigned offset);
ir_node *be_new_RegParams(ir_graph *irg, int n_out);
ir_node *be_new_NoReg(const arch_register_t *reg, ir_graph *irg, ir_node *bl);

ir_node *be_spill(const arch_env_t *arch_env, ir_node *irn,ir_node *spill_ctx);
ir_node *be_reload(const arch_env_t *arch_env, const arch_register_class_t *cls, ir_node *irn, int pos, ir_mode *mode, ir_node *spill);

int be_is_Spill(const ir_node *irn);
int be_is_Reload(const ir_node *irn);
int be_is_Copy(const ir_node *irn);
int be_is_Perm(const ir_node *irn);
int be_is_Keep(const ir_node *irn);
int be_is_Call(const ir_node *irn);
int be_is_AddSP(const ir_node *irn);
int be_is_RegParams(const ir_node *irn);
int be_is_StackParam(const ir_node *irn);
int be_is_NoReg(const ir_node *irn);


void   be_set_Spill_entity(ir_node *irn, entity *ent);
entity *be_get_spill_entity(ir_node *irn);

ir_node *be_get_Spill_context(const ir_node *irn);

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
 * Insert a Perm node after a specific node in the schedule.
 * The Perm permutes over all values live at the given node.
 * This means that all liveness intervals are cut apart at this
 * location in the program.
 */
ir_node *insert_Perm_after(const arch_env_t *env,
						   const arch_register_class_t *cls,
						   dom_front_info_t *dom_front,
						   ir_node *pos);


#endif /* _BENODE_T_H */
