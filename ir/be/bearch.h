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

#include <stdbool.h>

#include "firm_types.h"
#include "bitset.h"
#include "obst.h"
#include "raw_bitset.h"
#include "irop_t.h"

#include "be_types.h"
#include "beinfo.h"
#include "be.h"
#include "beirg.h"

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
	arch_register_req_type_none              = 0, /**< No register requirement. */
	arch_register_req_type_normal            = 1U << 0, /**< All registers in the class are allowed. */
	arch_register_req_type_limited           = 1U << 1, /**< Only a real subset of the class is allowed. */
	arch_register_req_type_should_be_same    = 1U << 2, /**< The register should be equal to another one at the node. */
	arch_register_req_type_must_be_different = 1U << 3, /**< The register must be unequal from some other at the node. */
	arch_register_req_type_ignore            = 1U << 4, /**< ignore while allocating registers */
	arch_register_req_type_produces_sp       = 1U << 5, /**< the output produces a new value for the stack pointer */
} arch_register_req_type_t;

extern const arch_register_req_t *arch_no_register_req;

/**
 * Print information about a register requirement in human readable form
 * @param F   output stream/file
 * @param req The requirements structure to format.
 */
void arch_dump_register_req(FILE *F, const arch_register_req_t *req,
                            const ir_node *node);

/**
 * Node classification. Mainly used for statistics.
 */
typedef enum arch_irn_class_t {
	arch_irn_class_spill  = 1 << 0,
	arch_irn_class_reload = 1 << 1,
	arch_irn_class_remat  = 1 << 2,
	arch_irn_class_copy   = 1 << 3,
	arch_irn_class_perm   = 1 << 4
} arch_irn_class_t;

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
 * @note Deprecated API! Preferably use
 *       arch_get_in_register_req and
 *       arch_get_out_register_req.
 *
 * @param irn The node.
 * @param pos The position of the operand you're interested in.
 * @return    A pointer to the register requirements.  If NULL is returned, the
 *            operand was no register operand.
 */
const arch_register_req_t *arch_get_register_req(const ir_node *irn, int pos);
const arch_register_req_t *arch_get_register_req_out(const ir_node *irn);

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

#define arch_reg_out_is_allocatable(irn, reg) arch_reg_is_allocatable(irn, -1, reg)

/**
 * Get the register class of an operand of a node.
 * @param irn The node.
 * @param pos The position of the operand, -1 for the output.
 * @return    The register class of the operand or NULL, if
 *            operand is a non-register operand.
 */
const arch_register_class_t *arch_get_irn_reg_class(const ir_node *irn, int pos);

#define arch_get_irn_reg_class_out(irn) arch_get_irn_reg_class(irn, -1)

/**
 * Get the register allocated at a certain output operand of a node.
 * @param irn The node.
 * @return    The register allocated for this operand
 */
const arch_register_t *arch_get_irn_register(const ir_node *irn);
const arch_register_t *arch_irn_get_register(const ir_node *irn, int pos);

/**
 * Set the register for a certain output operand.
 * @param irn The node.
 * @param reg The register.
 */
void arch_set_irn_register(ir_node *irn, const arch_register_t *reg);
void arch_irn_set_register(ir_node *irn, int pos, const arch_register_t *reg);

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

void arch_irn_set_flags(ir_node *node, arch_irn_flags_t flags);
void arch_irn_add_flags(ir_node *node, arch_irn_flags_t flags);

#define arch_irn_is(irn, flag) ((arch_irn_get_flags(irn) & arch_irn_flags_ ## flag) != 0)

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

/**
 * A register.
 */
struct arch_register_t {
	const char                  *name;        /**< The name of the register. */
	const arch_register_class_t *reg_class;   /**< The class the register belongs to. */
	unsigned                    index;        /**< The index of the register in the class. */
	arch_register_type_t        type;         /**< The type of the register. */
};

static inline const arch_register_class_t *
_arch_register_get_class(const arch_register_t *reg)
{
	return reg->reg_class;
}

static inline
unsigned _arch_register_get_index(const arch_register_t *reg)
{
	return reg->index;
}

static inline
const char *_arch_register_get_name(const arch_register_t *reg)
{
	return reg->name;
}

#define arch_register_get_class(reg)      _arch_register_get_class(reg)
#define arch_register_get_index(reg)      _arch_register_get_index(reg)
#define arch_register_get_name(reg)       _arch_register_get_name(reg)

/**
 * Convenience macro to check for register type.
 * @param req   A pointer to register.
 * @param kind  The kind of type to check for (see arch_register_type_t).
 * @return      1, If register is of given kind, 0 if not.
 */
#define arch_register_type_is(reg, kind) \
  (((reg)->type & arch_register_type_ ## kind) != 0)

/**
 * A class of registers.
 * Like general purpose or floating point.
 */
struct arch_register_class_t {
	unsigned                     index;  /**< index of this register class */
	const char                  *name;   /**< The name of the register class.*/
	unsigned                     n_regs; /**< Number of registers in this
	                                          class. */
	ir_mode                     *mode;   /**< The mode of the register class.*/
	const arch_register_t       *regs;   /**< The array of registers. */
	arch_register_class_flags_t  flags;  /**< register class flags. */
};

/** return the number of registers in this register class */
#define arch_register_class_n_regs(cls) ((cls)->n_regs)

/** return the largest mode of this register class */
#define arch_register_class_mode(cls) ((cls)->mode)

/** return the name of this register class */
#define arch_register_class_name(cls) ((cls)->name)

/** return the index of this register class */
#define arch_register_class_index(cls)  ((cls)->index)

/** return the register class flags */
#define arch_register_class_flags(cls) ((cls)->flags)

static inline const arch_register_t *
_arch_register_for_index(const arch_register_class_t *cls, unsigned idx)
{
	assert(idx < cls->n_regs);
	return &cls->regs[idx];
}

#define arch_register_for_index(cls, idx)   _arch_register_for_index(cls, idx)

/**
 * Convenience macro to check for set constraints.
 * @param req   A pointer to register requirements.
 * @param kind  The kind of constraint to check for (see arch_register_req_type_t).
 * @return      1, If the kind of constraint is present, 0 if not.
 */
#define arch_register_req_is(req, kind) \
	(((req)->type & (arch_register_req_type_ ## kind)) != 0)

/**
 * Expresses requirements to register allocation for an operand.
 */
struct arch_register_req_t {
	arch_register_req_type_t     type;  /**< The type of the constraint. */
	const arch_register_class_t *cls;   /**< The register class this constraint belongs to. */

	const unsigned *limited;            /**< allowed register bitset */

	unsigned other_same;                /**< Bitmask of ins which should use the
	                                         same register (should_be_same). */
	unsigned other_different;           /**< Bitmask of ins which shall use a
	                                         different register
	                                         (must_be_different) */
};

static inline int reg_reqs_equal(const arch_register_req_t *req1,
                                 const arch_register_req_t *req2)
{
	if (req1 == req2)
		return 1;

	if (req1->type != req2->type
			|| req1->cls != req2->cls
			|| req1->other_same != req2->other_same
			|| req1->other_different != req2->other_different)
		return 0;

	if (req1->limited != NULL) {
		size_t n_regs;

		if (req2->limited == NULL)
			return 0;

		n_regs = arch_register_class_n_regs(req1->cls);
		if (!rbitset_equal(req1->limited, req2->limited, n_regs))
			return 0;
	}

	return 1;
}

/**
 * An inverse operation returned by the backend
 */
struct arch_inverse_t {
	int      n;       /**< count of nodes returned in nodes array */
	int      costs;   /**< costs of this remat */

	/**< nodes for this inverse operation. shall be in
	 *  schedule order. last element is the target value
	 */
	ir_node  **nodes;
};

struct arch_irn_ops_t {

	/**
  	 * Get the register requirements for a given operand.
	 * @param irn The node.
	 * @param pos The operand's position
	 * @return    The register requirements for the selected operand.
	 *            The pointer returned is never NULL.
	 */
	const arch_register_req_t *(*get_irn_reg_req_in)(const ir_node *irn, int pos);

	/**
  	 * Get the register requirements for values produced by a node
	 * @param irn The node.
	 * @param pos The operand's position (0 for most nodes,
	 *                                    0..n for mode_T nodes)
	 * @return    The register requirements for the selected operand.
	 *            The pointer returned is never NULL.
	 */
	const arch_register_req_t *(*get_irn_reg_req_out)(const ir_node *irn, int pos);

	/**
	 * Classify the node.
	 * @param irn The node.
	 * @return A classification.
	 */
	arch_irn_class_t (*classify)(const ir_node *irn);

	/**
	 * Get the entity on the stack frame this node depends on.
	 * @param irn  The node in question.
	 * @return The entity on the stack frame or NULL, if the node does not have a
	 *         stack frame entity.
	 */
	ir_entity *(*get_frame_entity)(const ir_node *irn);

	/**
	 * Set the entity on the stack frame this node depends on.
	 * @param irn  The node in question.
	 * @param ent  The entity to set
	 */
	void (*set_frame_entity)(ir_node *irn, ir_entity *ent);

	/**
	 * Set the offset of a node carrying an entity on the stack frame.
	 * @param irn  The node.
	 * @param offset The offset of the node's stack frame entity.
	 */
	void (*set_frame_offset)(ir_node *irn, int offset);

	/**
	 * Returns the delta of the stackpointer for nodes that increment or
	 * decrement the stackpointer with a constant value. (push, pop
	 * nodes on most architectures).
	 * A positive value stands for an expanding stack area, a negative value for
	 * a shrinking one.
	 *
	 * @param irn       The node
	 * @return          0 if the stackpointer is not modified with a constant
	 *                  value, otherwise the increment/decrement value
	 */
	int (*get_sp_bias)(const ir_node *irn);

	/**
	 * Returns an inverse operation which yields the i-th argument
	 * of the given node as result.
	 *
	 * @param irn       The original operation
	 * @param i         Index of the argument we want the inverse operation to yield
	 * @param inverse   struct to be filled with the resulting inverse op
	 * @param obstack   The obstack to use for allocation of the returned nodes array
	 * @return          The inverse operation or NULL if operation invertible
	 */
	arch_inverse_t *(*get_inverse)(const ir_node *irn, int i, arch_inverse_t *inverse, struct obstack *obstack);

	/**
	 * Get the estimated cycle count for @p irn.
	 *
	 * @param irn  The node.
	 *
	 * @return     The estimated cycle count for this operation
	 */
	int (*get_op_estimated_cost)(const ir_node *irn);

	/**
	 * Asks the backend whether operand @p i of @p irn can be loaded form memory internally
	 *
	 * @param irn  The node.
	 * @param i    Index of the argument we would like to know whether @p irn can load it form memory internally
	 *
	 * @return     nonzero if argument can be loaded or zero otherwise
	 */
	int (*possible_memory_operand)(const ir_node *irn, unsigned int i);

	/**
	 * Ask the backend to assimilate @p reload of operand @p i into @p irn.
	 *
	 * @param irn    The node.
	 * @param spill  The spill.
	 * @param i      The position of the reload.
	 */
	void (*perform_memory_operand)(ir_node *irn, ir_node *spill, unsigned int i);
};

/**
 * The code generator interface.
 */
struct arch_code_generator_if_t {
	/**
	 * Initialize the code generator.
	 * @param birg A backend IRG session.
	 * @return     A newly created code generator.
	 */
	void *(*init)(be_irg_t *birg);

	/**
	 * return node used as base in pic code addresses
	 */
	ir_node* (*get_pic_base)(void *self);

	/**
	 * Called before abi introduce.
	 */
	void (*before_abi)(void *self);

	/**
	 * Called, when the graph is being normalized.
	 */
	void (*prepare_graph)(void *self);

	/**
	 * Backend may provide an own spiller.
	 * This spiller needs to spill all register classes.
	 */
	void (*spill)(void *self, be_irg_t *birg);

	/**
	 * Called before register allocation.
	 */
	void (*before_ra)(void *self);

	/**
	 * Called after register allocation.
	 */
	void (*after_ra)(void *self);

	/**
	 * Called directly before done is called. This should be the last place
	 * where the irg is modified.
	 */
	void (*finish)(void *self);

	/**
	 * Called after everything happened. This call should emit the final
	 * assembly code but avoid changing the irg.
	 * The code generator must also be de-allocated here.
	 */
	void (*done)(void *self);
};

/**
 * helper macro: call function func from the code generator
 * if it's implemented.
 */
#define _arch_cg_call(cg, func) \
do { \
	if((cg)->impl->func) \
		(cg)->impl->func(cg); \
} while(0)

#define _arch_cg_call_env(cg, env, func) \
do { \
	if((cg)->impl->func) \
		(cg)->impl->func(cg, env); \
} while(0)

#define arch_code_generator_before_abi(cg)      _arch_cg_call(cg, before_abi)
#define arch_code_generator_prepare_graph(cg)   _arch_cg_call(cg, prepare_graph)
#define arch_code_generator_before_ra(cg)       _arch_cg_call(cg, before_ra)
#define arch_code_generator_after_ra(cg)        _arch_cg_call(cg, after_ra)
#define arch_code_generator_finish(cg)          _arch_cg_call(cg, finish)
#define arch_code_generator_done(cg)            _arch_cg_call(cg, done)
#define arch_code_generator_spill(cg, birg)     _arch_cg_call_env(cg, birg, spill)
#define arch_code_generator_has_spiller(cg)     ((cg)->impl->spill != NULL)
#define arch_code_generator_get_pic_base(cg)    \
	((cg)->impl->get_pic_base != NULL ? (cg)->impl->get_pic_base(cg) : NULL)

/**
 * Code generator base class.
 */
struct arch_code_generator_t {
	const arch_code_generator_if_t *impl;
};

/**
 * Architecture interface.
 */
struct arch_isa_if_t {
	/**
	 * Initialize the isa interface.
	 * @param file_handle  the file handle to write the output to
	 * @return a new isa instance
	 */
	arch_env_t *(*init)(FILE *file_handle);

	/**
	 * Free the isa instance.
	 */
	void (*done)(void *self);

	/**
	 * Called directly after initialization. Backend should handle all
	 * intrinsics here.
	 */
	void (*handle_intrinsics)(void);

	/**
	 * Get the the number of register classes in the isa.
	 * @return The number of register classes.
	 */
	unsigned (*get_n_reg_class)(const void *self);

	/**
	 * Get the i-th register class.
	 * @param i The number of the register class.
	 * @return The register class.
	 */
	const arch_register_class_t *(*get_reg_class)(const void *self, unsigned i);

	/**
	 * Get the register class which shall be used to store a value of a given mode.
	 * @param self The this pointer.
	 * @param mode The mode in question.
	 * @return A register class which can hold values of the given mode.
	 */
	const arch_register_class_t *(*get_reg_class_for_mode)(const void *self, const ir_mode *mode);

	/**
	 * Get the ABI restrictions for procedure calls.
	 * @param self        The this pointer.
	 * @param call_type   The call type of the method (procedure) in question.
	 * @param p           The array of parameter locations to be filled.
	 */
	void (*get_call_abi)(const void *self, ir_type *call_type, be_abi_call_t *abi);

	/**
	 * Get the code generator interface.
	 * @param self The this pointer.
	 * @return     Some code generator interface.
	 */
	const arch_code_generator_if_t *(*get_code_generator_if)(void *self);

	/**
	 * Get the list scheduler to use. There is already a selector given, the
	 * backend is free to modify and/or ignore it.
	 *
	 * @param self     The isa object.
	 * @param selector The selector given by options.
	 * @return         The list scheduler selector.
	 */
	const list_sched_selector_t *(*get_list_sched_selector)(const void *self, list_sched_selector_t *selector);

	/**
	 * Get the ILP scheduler to use.
	 * @param self  The isa object.
	 * @return      The ILP scheduler selector
	 */
	const ilp_sched_selector_t *(*get_ilp_sched_selector)(const void *self);

	/**
	 * Get the necessary alignment for storing a register of given class.
	 * @param self  The isa object.
	 * @param cls   The register class.
	 * @return      The alignment in bytes.
	 */
	int (*get_reg_class_alignment)(const void *self, const arch_register_class_t *cls);

	/**
	 * A "static" function, returns the frontend settings
	 * needed for this backend.
	 */
	const backend_params *(*get_params)(void);

	/**
	 * Returns an 2-dim array of execution units, @p irn can be executed on.
	 * The first dimension is the type, the second the allowed units of this
	 * type.
	 * Each dimension is a NULL terminated list.
	 * @param self  The isa object.
	 * @param irn   The node.
	 * @return An array of allowed execution units.
	 *         exec_unit = {
	 *                       { unit1_of_tp1, ..., unitX1_of_tp1, NULL },
	 *                       ...,
	 *                       { unit1_of_tpY, ..., unitXn_of_tpY, NULL },
	 *                       NULL
	 *                     };
	 */
	const be_execution_unit_t ***(*get_allowed_execution_units)(const void *self, const ir_node *irn);

	/**
	 * Return the abstract machine for this isa.
	 * @param self  The isa object.
	 */
	const be_machine_t *(*get_machine)(const void *self);

	/**
	 * Return an ordered list of irgs where code should be generated for.
	 * If NULL is returned, all irg will be taken into account and they will be
	 * generated in an arbitrary order.
	 * @param self   The isa object.
	 * @param irgs   A flexible array ARR_F of length 0 where the backend can append the desired irgs.
	 * @return A flexible array ARR_F containing all desired irgs in the desired order.
	 */
	ir_graph **(*get_backend_irg_list)(const void *self, ir_graph ***irgs);

	/**
	 * mark node as rematerialized
	 */
	void (*mark_remat)(const void *self, ir_node *node);

	/**
	 * parse an assembler constraint part and set flags according to its nature
	 * advances the *c pointer to point to the last parsed character (so if you
	 * parse a single character don't advance c)
	 */
	asm_constraint_flags_t (*parse_asm_constraint)(const void *self, const char **c);

	/**
	 * returns true if the string is a valid clobbered (register) in this
	 * backend
	 */
	int (*is_valid_clobber)(const void *self, const char *clobber);
};

#define arch_env_done(env)                             ((env)->impl->done(env))
#define arch_env_handle_intrinsics(env)                \
	do { if((env)->impl->handle_intrinsics != NULL) (env)->impl->handle_intrinsics(); } while(0)
#define arch_env_get_n_reg_class(env)                  ((env)->impl->get_n_reg_class(env))
#define arch_env_get_reg_class(env,i)                  ((env)->impl->get_reg_class(env, i))
#define arch_env_get_reg_class_for_mode(env,mode)      ((env)->impl->get_reg_class_for_mode((env), (mode)))
#define arch_env_get_call_abi(env,tp,abi)              ((env)->impl->get_call_abi((env), (tp), (abi)))
#define arch_env_get_code_generator_if(env)            ((env)->impl->get_code_generator_if((env)))
#define arch_env_get_list_sched_selector(env,selector) ((env)->impl->get_list_sched_selector((env), (selector)))
#define arch_env_get_ilp_sched_selector(env)           ((env)->impl->get_ilp_sched_selector(env))
#define arch_env_get_reg_class_alignment(env,cls)      ((env)->impl->get_reg_class_alignment((env), (cls)))
#define arch_env_get_params(env)                       ((env)->impl->get_params())
#define arch_env_get_allowed_execution_units(env,irn)  ((env)->impl->get_allowed_execution_units((env), (irn)))
#define arch_env_get_machine(env)                      ((env)->impl->get_machine(env))
#define arch_env_get_backend_irg_list(env,irgs)        ((env)->impl->get_backend_irg_list((env), (irgs)))
#define arch_env_parse_asm_constraint(env,c)           ((env)->impl->parse_asm_constraint((env), (c))
#define arch_env_is_valid_clobber(env,clobber)         ((env)->impl->is_valid_clobber((env), (clobber))
#define arch_env_mark_remat(env,node) \
	do { if ((env)->impl->mark_remat != NULL) (env)->impl->mark_remat((env), (node)); } while(0)

/**
 * ISA base class.
 */
struct arch_env_t {
	const arch_isa_if_t   *impl;
	const arch_register_t *sp;               /** The stack pointer register. */
	const arch_register_t *bp;               /** The base pointer register. */
	const arch_register_class_t *link_class; /** The static link pointer register class. */
	int                    stack_dir;        /** -1 for decreasing, 1 for increasing. */
	int                    stack_alignment;  /** power of 2 stack alignment */
	const be_main_env_t   *main_env;         /** the be main environment */
	int                    spill_cost;       /** cost for a be_Spill node */
	int                    reload_cost;      /** cost for a be_Reload node */
};

static inline unsigned arch_irn_get_n_outs(const ir_node *node)
{
	backend_info_t *info = be_get_info(node);
	return ARR_LEN(info->out_infos);
}

static inline bool arch_irn_consider_in_reg_alloc(
		const arch_register_class_t *cls, const ir_node *node)
{
	const arch_register_req_t *req = arch_get_register_req_out(node);
	return
		req->cls == cls &&
		!(req->type & arch_register_req_type_ignore);
}

static inline bool arch_irn_is_ignore(const ir_node *irn)
{
	const arch_register_req_t *req = arch_get_register_req_out(irn);
	return !!(req->type & arch_register_req_type_ignore);
}

static inline const arch_irn_ops_t *get_irn_ops_simple(const ir_node *node)
{
	const ir_op          *ops    = get_irn_op(node);
	const arch_irn_ops_t *be_ops = get_op_ops(ops)->be_ops;
	assert(!is_Proj(node));
	return be_ops;
}

/**
 * Get register constraints for an operand at position @p
 */
static inline const arch_register_req_t *arch_get_in_register_req(
		const ir_node *node, int pos)
{
	const arch_irn_ops_t *ops = get_irn_ops_simple(node);
	return ops->get_irn_reg_req_in(node, pos);
}

/**
 * Get register constraint for a produced result (the @p pos result)
 */
static inline const arch_register_req_t *arch_get_out_register_req(
		const ir_node *node, int pos)
{
	const arch_irn_ops_t *ops = get_irn_ops_simple(node);
	return ops->get_irn_reg_req_out(node, pos);
}

#endif /* FIRM_BE_BEARCH_H */
