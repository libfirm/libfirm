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
 * @brief       Processor architecture specification - internal data structures.
 * @author      Sebastian Hack
 * @version     $Id$
 */
#ifndef FIRM_BE_BEARCH_T_H
#define FIRM_BE_BEARCH_T_H

#include "bearch.h"

#include "belistsched.h"
#include "beilpsched.h"
#include "bemachine.h"
#include "beirg.h"
#include "beabi.h"

/**
 * A register.
 */
struct arch_register_t {
	const char                  *name;        /**< The name of the register. */
	const arch_register_class_t *reg_class;   /**< The class the register belongs to. */
	int                         index;        /**< The index of the register in the class. */
	arch_register_type_t        type;         /**< The type of the register. */
	void                        *data;        /**< Custom data. */
};

static INLINE const arch_register_class_t *
_arch_register_get_class(const arch_register_t *reg)
{
	return reg->reg_class;
}

static INLINE
int _arch_register_get_index(const arch_register_t *reg)
{
	return reg->index;
}

static INLINE
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
	const char                  *name;   /**< The name of the register class.*/
	int                          n_regs; /**< Number of registers in this
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

/** return the register class flags */
#define arch_register_class_flags(cls) ((cls)->flags)

static INLINE const arch_register_t *
_arch_register_for_index(const arch_register_class_t *cls, int idx)
{
	assert(0 <= idx && idx < cls->n_regs);
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
	arch_register_req_type_t type;      /**< The type of the constraint. */
	const arch_register_class_t *cls;   /**< The register class this constraint belongs to. */

	const unsigned *limited;            /**< allowed register bitset */

	int other_same[2];                /**< The in numbers which shall have the
	                                       same res (should_be_same).  More than
	                                       two are unnecessary because there is
	                                       no machine with more than two
	                                       commutative inputs to one operation */
	int other_different;                /**< The other node from which this
										     one's register must be different
											 (case must_be_different). */
};

struct arch_flag_t {
	const char *name;
	unsigned    index;
};

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

struct arch_irn_ops_if_t {

	/**
  	 * Get the register requirements for a given operand.
	 * @param self The self pointer.
	 * @param irn The node.
	 * @param pos The operand's position
	 *        (-1 for the result of the node, 0..n for the input operands).
	 * @return    The register requirements for the selected operand.
	 *            The pointer returned is never NULL.
	 */
	const arch_register_req_t *(*get_irn_reg_req)(const void *self,
	                                              const ir_node *irn, int pos);

	/**
	 * Set the register for an output operand.
	 * @param irn The node.
	 * @param reg The register allocated to that operand.
	 * @note      If the operand is not a register operand,
	 *            the call is ignored.
	 */
	void (*set_irn_reg)(const void *self, ir_node *irn, const arch_register_t *reg);

	/**
	 * Get the register allocated for an output operand.
	 * @param irn The node.
	 * @return    The register allocated at that operand. NULL, if
	 *            the operand was no register operand or
	 *            @c arch_register_invalid, if no register has yet been
	 *            allocated for this node.
	 */
	const arch_register_t *(*get_irn_reg)(const void *self, const ir_node *irn);

	/**
	 * Classify the node.
	 * @param irn The node.
	 * @return A classification.
	 */
	arch_irn_class_t (*classify)(const void *self, const ir_node *irn);

	/**
	 * Get the flags of a node.
	 * @param self The irn ops themselves.
	 * @param irn The node.
	 * @return A set of flags.
	 */
	arch_irn_flags_t (*get_flags)(const void *self, const ir_node *irn);

	/**
	 * Get the entity on the stack frame this node depends on.
	 * @param self The this pointer.
	 * @param irn  The node in question.
	 * @return The entity on the stack frame or NULL, if the node does not have a
	 *         stack frame entity.
	 */
	ir_entity *(*get_frame_entity)(const void *self, const ir_node *irn);

	/**
	 * Set the entity on the stack frame this node depends on.
	 * @param self The this pointer.
	 * @param irn  The node in question.
	 * @param ent  The entity to set
	 */
	void (*set_frame_entity)(const void *self, ir_node *irn, ir_entity *ent);

	/**
	 * Set the offset of a node carrying an entity on the stack frame.
	 * @param self The this pointer.
	 * @param irn  The node.
	 * @param offset The offset of the node's stack frame entity.
	 */
	void (*set_frame_offset)(const void *self, ir_node *irn, int offset);

	/**
	 * Returns the delta of the stackpointer for nodes that increment or
	 * decrement the stackpointer with a constant value. (push, pop
	 * nodes on most architectures).
	 * A positive value stands for an expanding stack area, a negative value for
	 * a shrinking one.
	 *
	 * @param self      The this pointer
	 * @param irn       The node
	 * @return          0 if the stackpointer is not modified with a constant
	 *                  value, otherwise the increment/decrement value
	 */
	int (*get_sp_bias)(const void *self, const ir_node *irn);

	/**
	 * Returns an inverse operation which yields the i-th argument
	 * of the given node as result.
	 *
	 * @param self      The this pointer.
	 * @param irn       The original operation
	 * @param i         Index of the argument we want the inverse operation to yield
	 * @param inverse   struct to be filled with the resulting inverse op
	 * @param obstack   The obstack to use for allocation of the returned nodes array
	 * @return          The inverse operation or NULL if operation invertible
	 */
	arch_inverse_t *(*get_inverse)(const void *self, const ir_node *irn, int i, arch_inverse_t *inverse, struct obstack *obstack);

	/**
	 * Get the estimated cycle count for @p irn.
	 *
	 * @param self The this pointer.
	 * @param irn  The node.
	 *
	 * @return     The estimated cycle count for this operation
	 */
	int (*get_op_estimated_cost)(const void *self, const ir_node *irn);

	/**
	 * Asks the backend whether operand @p i of @p irn can be loaded form memory internally
	 *
	 * @param self The this pointer.
	 * @param irn  The node.
	 * @param i    Index of the argument we would like to know whether @p irn can load it form memory internally
	 *
	 * @return     nonzero if argument can be loaded or zero otherwise
	 */
	int (*possible_memory_operand)(const void *self, const ir_node *irn, unsigned int i);

	/**
	 * Ask the backend to assimilate @p reload of operand @p i into @p irn.
	 *
	 * @param self   The this pointer.
	 * @param irn    The node.
	 * @param spill  The spill.
	 * @param i      The position of the reload.
	 */
	void (*perform_memory_operand)(const void *self, ir_node *irn, ir_node *spill, unsigned int i);
};

/**
 * irn_ops base class.
 */
struct arch_irn_ops_t {
	const arch_irn_ops_if_t *impl;
};

/**
 * Somebody who can be asked about IR nodes.
 */
struct arch_irn_handler_t {

  /**
    * Get the operations of an irn.
    * @param self The handler from which the method is invoked.
    * @param irn Some node.
    * @return Operations for that irn.
    */
  const void *(*get_irn_ops)(const arch_irn_handler_t *handler,
      const ir_node *irn);
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
	 * Called before scheduling.
	 */
	void (*before_sched)(void *self);

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
#define arch_code_generator_before_sched(cg)    _arch_cg_call(cg, before_sched)
#define arch_code_generator_before_ra(cg)       _arch_cg_call(cg, before_ra)
#define arch_code_generator_after_ra(cg)        _arch_cg_call(cg, after_ra)
#define arch_code_generator_finish(cg)          _arch_cg_call(cg, finish)
#define arch_code_generator_done(cg)            _arch_cg_call(cg, done)
#define arch_code_generator_spill(cg, birg)     _arch_cg_call_env(cg, birg, spill)
#define arch_code_generator_has_spiller(cg)     ((cg)->impl->spill != NULL)

/**
 * Code generator base class.
 */
struct arch_code_generator_t {
	const arch_code_generator_if_t *impl;
};

/**
 * ISA base class.
 */
struct arch_isa_t {
	const arch_isa_if_t   *impl;
	const arch_register_t *sp;        /** The stack pointer register. */
	const arch_register_t *bp;        /** The base pointer register. */
	const int              stack_dir; /** -1 for decreasing, 1 for increasing. */
	const be_main_env_t   *main_env;  /** the be main environment */
	const int              spill_cost;  /** cost for a be_Spill node */
	const int              reload_cost; /** cost for a be_Reload node */
};

#define arch_isa_stack_dir(isa)  ((isa)->stack_dir)
#define arch_isa_sp(isa)         ((isa)->sp)
#define arch_isa_bp(isa)         ((isa)->bp)

/**
 * Architecture interface.
 */
struct arch_isa_if_t {
	/**
	 * Initialize the isa interface.
	 * @param file_handle  the file handle to write the output to
	 * @param main_env     the be main environment
	 * @return a new isa instance
	 */
	void *(*init)(FILE *file_handle);

	/**
	 * Free the isa instance.
	 */
	void (*done)(void *self);

	/**
	 * Get the the number of register classes in the isa.
	 * @return The number of register classes.
	 */
	int (*get_n_reg_class)(const void *self);

	/**
	 * Get the i-th register class.
	 * @param i The number of the register class.
	 * @return The register class.
	 */
	const arch_register_class_t *(*get_reg_class)(const void *self, int i);

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
	 * @param method_type The type of the method (procedure) in question.
	 * @param p           The array of parameter locations to be filled.
	 */
	void (*get_call_abi)(const void *self, ir_type *method_type, be_abi_call_t *abi);

	/**
	 * The irn handler for this architecture.
	 * The irn handler is registered by the Firm back end
	 * when the architecture is initialized.
	 * (May be NULL).
	 */
	const arch_irn_handler_t *(*get_irn_handler)(const void *self);

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
};

#define arch_isa_get_n_reg_class(isa)                  ((isa)->impl->get_n_reg_class(isa))
#define arch_isa_get_reg_class(isa,i)                  ((isa)->impl->get_reg_class(isa, i))
#define arch_isa_get_irn_handler(isa)                  ((isa)->impl->get_irn_handler(isa))
#define arch_isa_get_call_abi(isa,tp,abi)              ((isa)->impl->get_call_abi((isa), (tp), (abi)))
#define arch_isa_get_reg_class_for_mode(isa,mode)      ((isa)->impl->get_reg_class_for_mode((isa), (mode)))
#define arch_isa_make_code_generator(isa,irg)          ((isa)->impl->make_code_generator((isa), (irg)))
#define arch_isa_get_reg_class_alignment(isa, cls)     ((isa)->impl->get_reg_class_alignment((isa), (cls)))
#define arch_isa_get_allowed_execution_units(isa, irn) ((isa)->impl->get_allowed_execution_units((isa), (irn)))
#define arch_isa_get_machine(isa)                      ((isa)->impl->get_machine((isa)))
#define arch_isa_get_backend_irg_list(isa, irgs)       ((isa)->impl->get_backend_irg_list((isa), (irgs)))

#define ARCH_MAX_HANDLERS         8

/**
 * Environment for the architecture infrastructure.
 * Keep this everywhere you're going.
 */
struct arch_env_t {
	arch_isa_t *isa;                                /**< The isa about which everything is. */

	arch_irn_handler_t const *handlers[ARCH_MAX_HANDLERS]; /**< The handlers are organized as
                                                           a stack. */

	int handlers_tos;                                   /**< The stack pointer of the handler
                                                        stack. */
};

/**
 * Get the isa of an arch environment.
 * @param env The environment.
 * @return The isa with which the env was initialized with.
 */
#define arch_env_get_isa(env)   ((env)->isa)

#endif /* FIRM_BE_BEARCH_T_H */
