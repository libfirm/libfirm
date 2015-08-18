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
#ifndef FIRM_BE_BENODE_H
#define FIRM_BE_BENODE_H

#include "be_types.h"
#include "firm_types.h"
#include "irnode_t.h"

typedef enum be_opcode {
	beo_Asm,
	beo_first = beo_Asm,
	beo_Copy,
	beo_CopyKeep,
	beo_IncSP,
	beo_Keep,
	beo_MemPerm,
	beo_Perm,
	beo_Relocation,
	beo_Start,
	beo_last  = beo_Start
} be_opcode;

typedef struct be_node_attr_t {
	except_attr exc;
} be_node_attr_t;

typedef struct be_asm_attr_t {
	be_node_attr_t base;
	ident         *text;
	void          *operands;
} be_asm_attr_t;

extern ir_op *op_be_Asm;
extern ir_op *op_be_Copy;
extern ir_op *op_be_CopyKeep;
extern ir_op *op_be_IncSP;
extern ir_op *op_be_Keep;
extern ir_op *op_be_MemPerm;
extern ir_op *op_be_Perm;
extern ir_op *op_be_Relocation;
extern ir_op *op_be_Start;

/**
 * Determines if irn is a be_node.
 */
bool is_be_node(const ir_node *irn);

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
 * Create a new MemPerm node.
 * A MemPerm node exchanges the values of memory locations. (Typically entities
 * used as spillslots). MemPerm nodes perform this operation without modifying
 * any register values.
 */
ir_node *be_new_MemPerm(ir_node *block, int n, ir_node *const *in);

ir_node *be_new_Keep(ir_node *block, int arity, ir_node *const *in);
ir_node *be_new_Keep_one(ir_node *kept);

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

ir_node *be_get_CopyKeep_op(const ir_node *cpy);

void be_set_MemPerm_in_entity(const ir_node *irn, unsigned n, ir_entity* ent);
ir_entity *be_get_MemPerm_in_entity(const ir_node *irn, unsigned n);

void be_set_MemPerm_out_entity(const ir_node *irn, unsigned n, ir_entity* ent);
ir_entity *be_get_MemPerm_out_entity(const ir_node *irn, unsigned n);

void be_set_MemPerm_offset(ir_node *irn, int offset);
int be_get_MemPerm_offset(const ir_node *irn);

unsigned be_get_MemPerm_entity_arity(const ir_node *irn);

arch_register_req_t const **be_allocate_in_reqs(ir_graph *irg, unsigned n);

const arch_register_req_t *be_create_reg_req(struct obstack *obst,
                                             const arch_register_t *reg,
                                             bool ignore);

/**
 * Set the register requirements for a phi node.
 */
void be_set_phi_reg_req(ir_node *phi, const arch_register_req_t *req);

/**
 * Creates a new phi with associated backend informations
 */
ir_node *be_new_Phi(ir_node *block, int n_ins, ir_node **ins, ir_mode *mode,
                    const arch_register_req_t *req);

/**
 * Create a new Phi with backend info and without inputs.
 * Inputs are added later with @see be_complete_Phi().
 */
ir_node *be_new_Phi0(ir_node *block, ir_mode *mode, arch_register_req_t const *req);

/**
 * Add inputs to a inputless Phi created by @see be_new_Phi0().
 */
ir_node *be_complete_Phi(ir_node *phi, unsigned n_ins, ir_node **ins);

ir_node *be_new_Asm(dbg_info *dbgi, ir_node *block, int n_ins, ir_node **ins, int n_outs, ident *text, void *operands);

/**
 * Create a new Relocation node. The node returns the reference to an entity
 * with a specific linker relocation kind. The relocation kind is backend
 * specific. This node is meant to be used in preparation phases for position
 * independent code.
 */
ir_node *be_new_Relocation(ir_graph *irg, unsigned kind, ir_entity *entity);

ir_entity *be_get_Relocation_entity(ir_node const* node);

unsigned be_get_Relocation_kind(ir_node const* node);

typedef enum be_start_out {
	BE_START_NO,
	BE_START_REG,
	BE_START_IGNORE,
} be_start_out;

ir_node *be_new_Start(ir_graph *irg, be_start_out const *outs);

ir_node *be_get_Start_mem(ir_graph *irg);

/**
 * Get Proj of start node with a specific register.
 */
ir_node *be_get_Start_proj(ir_graph *irg, arch_register_t const *reg);

/**
 * Create a new Proj node.  Its mode is determined from the out requirement
 * @p pos of @p pred.
 */
ir_node *be_new_Proj(ir_node *pred, unsigned pos);

/**
 * Gets the Proj with number pn from irn.
 * Creates the Proj, if it does not exist, yet.
 */
ir_node *be_get_or_make_Proj_for_pn(ir_node *irn, unsigned pn);

static inline bool be_is_Asm       (const ir_node *irn) { return get_irn_op(irn) == op_be_Asm       ; }
static inline bool be_is_Copy      (const ir_node *irn) { return get_irn_op(irn) == op_be_Copy      ; }
static inline bool be_is_CopyKeep  (const ir_node *irn) { return get_irn_op(irn) == op_be_CopyKeep  ; }
static inline bool be_is_Perm      (const ir_node *irn) { return get_irn_op(irn) == op_be_Perm      ; }
static inline bool be_is_MemPerm   (const ir_node *irn) { return get_irn_op(irn) == op_be_MemPerm   ; }
static inline bool be_is_Keep      (const ir_node *irn) { return get_irn_op(irn) == op_be_Keep      ; }
static inline bool be_is_IncSP     (const ir_node *irn) { return get_irn_op(irn) == op_be_IncSP     ; }
static inline bool be_is_Relocation(const ir_node *irn) { return get_irn_op(irn) == op_be_Relocation; }
static inline bool be_is_Start     (const ir_node *irn) { return get_irn_op(irn) == op_be_Start     ; }

static inline be_asm_attr_t const *get_be_asm_attr_const(ir_node const *const asmn)
{
	assert(be_is_Asm(asmn));
	return (be_asm_attr_t const*)get_irn_generic_attr_const(asmn);
}

/**
 * Copies the backend specific attributes from old node to new node.
 */
void be_copy_attr(ir_graph *irg, ir_node const *old_node, ir_node *new_node);

#endif
