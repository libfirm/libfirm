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
 * @brief       Processor architecture specification.
 * @author      Sebastian Hack
 * @version     $Id$
 */
#ifndef FIRM_BE_BEARCH_H
#define FIRM_BE_BEARCH_H

#include "firm_types.h"
#include "bitset.h"
#include "be.h"

typedef struct arch_register_class_t     arch_register_class_t;
typedef struct arch_register_req_t       arch_register_req_t;
typedef struct arch_register_t           arch_register_t;
typedef struct arch_flag_t               arch_flag_t;
typedef struct arch_inverse_t            arch_inverse_t;
typedef struct arch_isa_if_t             arch_isa_if_t;
typedef struct arch_isa_t                arch_isa_t;
typedef struct arch_env_t                arch_env_t;
typedef struct arch_irn_ops_if_t         arch_irn_ops_if_t;
typedef struct arch_irn_ops_t            arch_irn_ops_t;
typedef struct arch_irn_handler_t        arch_irn_handler_t;
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
 * Put all registers in a class into a bitset.
 * @param cls The class.
 * @param bs The bitset. May be NULL.
 * @return The number of registers in the class.
 */
extern int arch_register_class_put(const arch_register_class_t *cls, bitset_t *bs);

typedef enum arch_operand_type_t {
  	arch_operand_type_invalid,
	arch_operand_type_memory,
	arch_operand_type_register,
	arch_operand_type_immediate,
	arch_operand_type_symconst,
	arch_operand_type_last
} arch_operand_type_t;

/**
 * Different types of register allocation requirements.
 */
typedef enum arch_register_req_type_t {
	arch_register_req_type_none                         = 0,  /**< No register requirement. */
	arch_register_req_type_normal                       = 1,  /**< All registers in the class are allowed. */
	arch_register_req_type_limited                      = 2,  /**< Only a real subset of the class is allowed. */
	arch_register_req_type_should_be_same               = 4,  /**< The register should be equal another one at the node. */
	arch_register_req_type_should_be_different          = 8,  /**< The register must be unequal to some other at the node. */
	arch_register_req_type_should_be_different_from_all = 16, /**< The register must be different from all in's at the node */
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
	arch_irn_class_normal     = 1 << 0,
	arch_irn_class_spill      = 1 << 1,
	arch_irn_class_reload     = 1 << 2,
	arch_irn_class_copy       = 1 << 3,
	arch_irn_class_perm       = 1 << 4,
	arch_irn_class_branch     = 1 << 5,
	arch_irn_class_call       = 1 << 6,
	arch_irn_class_load       = 1 << 7,
	arch_irn_class_store      = 1 << 8,
	arch_irn_class_stackparam = 1 << 9,
} arch_irn_class_t;

/**
 * Some flags describing a node in more detail.
 */
typedef enum arch_irn_flags_t {
	arch_irn_flags_none             = 0, /**< Node flags. */
	arch_irn_flags_dont_spill       = 1, /**< This must not be spilled. */
	arch_irn_flags_rematerializable = 2, /**< This can be replicated instead of spilled/reloaded. */
	arch_irn_flags_ignore           = 4, /**< Ignore node during register allocation. */
	arch_irn_flags_modify_sp        = 8, /**< I modify the stack pointer. */
	arch_irn_flags_modify_flags     = 16, /**< I modify flags. */
	arch_irn_flags_last             = arch_irn_flags_modify_flags
} arch_irn_flags_t;

/**
 * Get the string representation of a flag.
 * This functions does not handle or'ed bitmasks of flags.
 * @param flag The flag.
 * @return The flag as a string.
 */
extern const char *arch_irn_flag_str(arch_irn_flags_t flag);

extern const arch_irn_ops_t *arch_get_irn_ops(const arch_env_t *env,
                                              const ir_node *irn);

extern void arch_set_frame_offset(const arch_env_t *env, ir_node *irn, int bias);

extern ir_entity *arch_get_frame_entity(const arch_env_t *env, const ir_node *irn);
extern void arch_set_frame_entity(const arch_env_t *env, ir_node *irn, ir_entity *ent);
extern int arch_get_sp_bias(const arch_env_t *env, ir_node *irn);

extern int arch_get_op_estimated_cost(const arch_env_t *env, const ir_node *irn);
extern arch_inverse_t *arch_get_inverse(const arch_env_t *env, const ir_node *irn, int i, arch_inverse_t *inverse, struct obstack *obstack);
extern int arch_possible_memory_operand(const arch_env_t *env, const ir_node *irn, unsigned int i);
extern void arch_perform_memory_operand(const arch_env_t *env, ir_node *irn, ir_node *spill, unsigned int i);

/**
 * Get the register requirements for a node.
 * @param env The architecture environment.
 * @param req A pointer to a requirements structure, where the data can
 *            be put into.
 * @param irn The node.
 * @param pos The position of the operand you're interested in.
 * @return    A pointer to the register requirements which may <b>not</b>
 *            neccessarily be equal to @p req. If NULL is returned, the
 *            operand was no register operand.
 */
extern const arch_register_req_t *
arch_get_register_req(const arch_env_t *env, const ir_node *irn, int pos);

/**
 * Check if an operand is a register operand.
 * @param env The environment.
 * @param irn The node.
 * @param pos The position of the operand.
 * @return 1, if the operand is significant for register allocation, 0
 * if not.
 */
extern int arch_is_register_operand(const arch_env_t *env,
    const ir_node *irn, int pos);

/**
 * Get the number of allocatable registers concerning
 * a register class for an operand of a node.
 * @param env The environment.
 * @param irn The node.
 * @param pos The postition of the node's operand.
 * @param bs  The bitset all allocatable registers shall be put into.
 *            Note, that you can also pass NULL here. If you don't,
 *            make sure, the bitset is as large as the register class
 *            has registers.
 * @return    The amount of registers allocatable for that operand.
 */
extern int arch_get_allocatable_regs(const arch_env_t *env, const ir_node *irn, int pos, bitset_t *bs);

/**
 * Put all registers which shall not be ignored by the register
 * allocator in a bit set.
 * @param env The arch env.
 * @param cls The register class to consider.
 * @param bs  The bit set to put the registers to.
 */
extern void arch_put_non_ignore_regs(const arch_env_t *env, const arch_register_class_t *cls, bitset_t *bs);

/**
 * Check, if a register is assignable to an operand of a node.
 * @param env The architecture environment.
 * @param irn The node.
 * @param pos The position of the operand.
 * @param reg The register.
 * @return    1, if the register might be allocated to the operand 0 if not.
 */
extern int arch_reg_is_allocatable(const arch_env_t *env,
    const ir_node *irn, int pos, const arch_register_t *reg);

/**
 * Get the register class of an operand of a node.
 * @param env The architecture environment.
 * @param irn The node.
 * @param pos The position of the operand, -1 for the output.
 * @return    The register class of the operand or NULL, if
 *            operand is a non-register operand.
 */
extern const arch_register_class_t *
arch_get_irn_reg_class(const arch_env_t *env, const ir_node *irn, int pos);

/**
 * Get the register allocated at a certain output operand of a node.
 * @param env The arch environment.
 * @param irn The node.
 * @return    The register allocated for this operand
 */
extern const arch_register_t *
arch_get_irn_register(const arch_env_t *env, const ir_node *irn);

/**
 * Set the register for a certain output operand.
 * @param env The architecture environment.
 * @param irn The node.
 * @param idx The index of the output operand.
 * @param reg The register.
 */
extern void arch_set_irn_register(const arch_env_t *env, ir_node *irn,
		const arch_register_t *reg);

/**
 * Classify a node.
 * @param env The architecture environment.
 * @param irn The node.
 * @return A classification of the node.
 */
extern arch_irn_class_t arch_irn_classify(const arch_env_t *env, const ir_node *irn);

#define arch_irn_class_is(env, irn, irn_class) ((arch_irn_classify(env, irn) & arch_irn_class_ ## irn_class) != 0)

/**
 * Get the flags of a node.
 * @param env The architecture environment.
 * @param irn The node.
 * @return The flags.
 */
extern arch_irn_flags_t arch_irn_get_flags(const arch_env_t *env, const ir_node *irn);

#define arch_irn_is(env, irn, flag) ((arch_irn_get_flags(env, irn) & arch_irn_flags_ ## flag) != 0)

#define arch_irn_has_reg_class(env, irn, pos, cls) \
	((cls) == arch_get_irn_reg_class(env, irn, pos))

#define arch_irn_consider_in_reg_alloc(env, cls, irn) \
	(arch_irn_has_reg_class(env, irn, -1, cls) && !arch_irn_is(env, irn, ignore))

/**
 * Initialize the architecture environment struct.
 * @param isa           The isa which shall be put into the environment.
 * @param file_handle   The file handle
 * @return The environment.
 */
extern arch_env_t *arch_env_init(arch_env_t *env, const arch_isa_if_t *isa,
                                 FILE *file_handle, be_main_env_t *main_env);

/**
 * Add a node handler to the environment.
 * @param env The environment.
 * @param handler A node handler.
 * @return The environment itself.
 */
extern arch_env_t *arch_env_push_irn_handler(arch_env_t *env, const arch_irn_handler_t *handler);

/**
 * Remove a node handler from the handler stack.
 * @param env The architecture environment.
 * @return The popped handler.
 */
extern const arch_irn_handler_t *arch_env_pop_irn_handler(arch_env_t *env);

/**
 * Register an instruction set architecture
 */
void be_register_isa_if(const char *name, const arch_isa_if_t *isa);

#endif /* FIRM_BE_BEARCH_H */
