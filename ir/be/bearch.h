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
 * @brief       Processor architecture specification.
 * @author      Sebastian Hack
 * @version     $Id$
 */
#ifndef FIRM_BE_BEARCH_H
#define FIRM_BE_BEARCH_H

#include "firm_types.h"
#include "bitset.h"
#include "be.h"
#include "obst.h"

typedef struct arch_register_class_t     arch_register_class_t;
typedef struct arch_register_req_t       arch_register_req_t;
typedef struct arch_register_t           arch_register_t;
typedef struct arch_flag_t               arch_flag_t;
typedef struct arch_inverse_t            arch_inverse_t;
typedef struct arch_isa_if_t             arch_isa_if_t;
typedef struct arch_env_t                arch_env_t;
typedef struct arch_code_generator_t     arch_code_generator_t;
typedef struct arch_code_generator_if_t  arch_code_generator_if_t;

typedef enum arch_register_class_flags_t {
	arch_register_class_flag_none      = 0,
	arch_register_class_flag_manual_ra = 1,  /**< don't do automatic register allocation for this class */
	arch_register_class_flag_state     = 2
} arch_register_class_flags_t;

typedef enum arch_register_type_t {
	arch_register_type_none         = 0,
	arch_register_type_caller_save  = 1,  /**< The register must be saved by the caller
	                                           upon a function call. It thus can be overwritten
	                                           in the called function. */
	arch_register_type_callee_save  = 2,  /**< The register must be saved by the caller
	                                           upon a function call. It thus can be overwritten
	                                           in the called function. */
	arch_register_type_ignore       = 4,  /**< Do not consider this register when allocating. */
	arch_register_type_joker        = 8,  /**< The emitter can choose an arbitrary register */
	arch_register_type_virtual      = 16, /**< This is just a virtual register.Virtual registers have
	                                           nearly no constraints, it is a allowed to have multiple
	                                           definition for the same register at a point) */
	arch_register_type_state        = 32, /**< The register represents a state that should be handled by
	                                           bestate code */
} arch_register_type_t;

/**
 * Different types of register allocation requirements.
 */
typedef enum arch_register_req_type_t {
	arch_register_req_type_none              = 0,  /**< No register requirement. */
	arch_register_req_type_normal            = 1,  /**< All registers in the class are allowed. */
	arch_register_req_type_limited           = 2,  /**< Only a real subset of the class is allowed. */
	arch_register_req_type_should_be_same    = 4,  /**< The register should be equal to another one at the node. */
	arch_register_req_type_must_be_different = 8,  /**< The register must be unequal from some other at the node. */
} arch_register_req_type_t;

extern const arch_register_req_t *arch_no_register_req;

/**
 * Format a register requirements information into a string.
 * @param buf The string where to put it to.
 * @param len The size of @p buf.
 * @param req The requirements structure to format.
 * @return    A pointer to buf.
 */
extern char *arch_register_req_format(char *buf, size_t len, const arch_register_req_t *req, const ir_node *node);

/**
 * Certain node classes which are relevant for the register allocator.
 */
typedef enum arch_irn_class_t {
	arch_irn_class_spill      = 1 << 0,
	arch_irn_class_reload     = 1 << 1,
	arch_irn_class_remat      = 1 << 2,
	arch_irn_class_copy       = 1 << 3,
	arch_irn_class_perm       = 1 << 4,
	arch_irn_class_branch     = 1 << 5
} arch_irn_class_t;

/**
 * Some flags describing a node in more detail.
 */
typedef enum arch_irn_flags_t {
	arch_irn_flags_none             = 0,       /**< Node flags. */
	arch_irn_flags_dont_spill       = 1U << 0, /**< This must not be spilled. */
	arch_irn_flags_rematerializable = 1U << 1, /**< This can be replicated instead of spilled/reloaded. */
	arch_irn_flags_ignore           = 1U << 2, /**< Ignore node during register allocation. */
	arch_irn_flags_modify_sp        = 1U << 3, /**< I modify the stack pointer. */
	arch_irn_flags_modify_flags     = 1U << 4  /**< I modify flags. */
} arch_irn_flags_t;

void arch_set_frame_offset(ir_node *irn, int bias);

ir_entity *arch_get_frame_entity(const ir_node *irn);
void       arch_set_frame_entity(ir_node *irn, ir_entity *ent);
int        arch_get_sp_bias(ir_node *irn);

int             arch_get_op_estimated_cost(const ir_node *irn);
arch_inverse_t *arch_get_inverse(const ir_node *irn, int i, arch_inverse_t *inverse, struct obstack *obstack);
int             arch_possible_memory_operand(const ir_node *irn, unsigned int i);
void            arch_perform_memory_operand(ir_node *irn, ir_node *spill, unsigned int i);

/**
 * Get the register requirements for a node.
 * @param irn The node.
 * @param pos The position of the operand you're interested in.
 * @return    A pointer to the register requirements.  If NULL is returned, the
 *            operand was no register operand.
 */
const arch_register_req_t *arch_get_register_req(const ir_node *irn, int pos);

/**
 * Get the number of allocatable registers concerning
 * a register class for an operand of a node.
 * @param irn The node.
 * @param pos The position of the node's operand.
 * @param bs  The bitset all allocatable registers shall be put into.
 *            Note, that you can also pass NULL here. If you don't,
 *            make sure, the bitset is as large as the register class
 *            has registers.
 * @return    The amount of registers allocatable for that operand.
 */
int arch_get_allocatable_regs(const ir_node *irn, int pos, bitset_t *bs);

/**
 * Put all registers which shall not be ignored by the register
 * allocator in a bit set.
 * @param cls The register class to consider.
 * @param bs  The bit set to put the registers to.
 */
extern void arch_put_non_ignore_regs(const arch_register_class_t *cls, bitset_t *bs);

/**
 * Check, if a register is assignable to an operand of a node.
 * @param irn The node.
 * @param pos The position of the operand.
 * @param reg The register.
 * @return    1, if the register might be allocated to the operand 0 if not.
 */
int arch_reg_is_allocatable(const ir_node *irn, int pos, const arch_register_t *reg);

/**
 * Get the register class of an operand of a node.
 * @param irn The node.
 * @param pos The position of the operand, -1 for the output.
 * @return    The register class of the operand or NULL, if
 *            operand is a non-register operand.
 */
const arch_register_class_t *arch_get_irn_reg_class(const ir_node *irn, int pos);

/**
 * Get the register allocated at a certain output operand of a node.
 * @param irn The node.
 * @return    The register allocated for this operand
 */
const arch_register_t *arch_get_irn_register(const ir_node *irn);

/**
 * Set the register for a certain output operand.
 * @param irn The node.
 * @param reg The register.
 */
void arch_set_irn_register(ir_node *irn, const arch_register_t *reg);

/**
 * Classify a node.
 * @param irn The node.
 * @return A classification of the node.
 */
arch_irn_class_t arch_irn_classify(const ir_node *irn);

#define arch_irn_class_is(irn, irn_class) ((arch_irn_classify(irn) & arch_irn_class_ ## irn_class) != 0)

/**
 * Get the flags of a node.
 * @param irn The node.
 * @return The flags.
 */
arch_irn_flags_t arch_irn_get_flags(const ir_node *irn);

#define arch_irn_is(irn, flag) ((arch_irn_get_flags(irn) & arch_irn_flags_ ## flag) != 0)

#define arch_irn_has_reg_class(irn, pos, cls) \
	((cls) == arch_get_irn_reg_class(irn, pos))

#define arch_irn_consider_in_reg_alloc(cls, irn) \
	(arch_irn_has_reg_class(irn, -1, cls) && !arch_irn_is(irn, ignore))

/**
 * Get the operations of an irn.
 * @param self The handler from which the method is invoked.
 * @param irn Some node.
 * @return Operations for that irn.
 */
typedef const void *(arch_get_irn_ops_t)(const ir_node *irn);

/**
 * Initialize the architecture environment struct.
 * @param isa           The isa which shall be put into the environment.
 * @param file_handle   The file handle
 * @return The environment.
 */
extern arch_env_t *arch_env_init(const arch_isa_if_t *isa,
                                 FILE *file_handle, be_main_env_t *main_env);

/**
 * Register an instruction set architecture
 */
void be_register_isa_if(const char *name, const arch_isa_if_t *isa);

#endif /* FIRM_BE_BEARCH_H */
