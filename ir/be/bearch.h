#ifndef _FIRM_BEARCH_H
#define _FIRM_BEARCH_H

#include "firm_config.h"

#include "firm_types.h"

#include "bitset.h"

#include "belistsched.h"
#include "beilpsched.h"
#include "beabi_t.h"
#include "bearch_t.h"
#include "be_t.h"
#include "bemachine.h"
#include "beirg.h"

typedef enum _arch_register_type_t {
  	arch_register_type_none         = 0,
	arch_register_type_caller_save  = 1,  /**< The register must be saved by the caller
                                             upon a function call. It thus can be overwritten
                                             in the called function. */
	arch_register_type_callee_save  = 2,  /**< The register must be saved by the caller
                                             upon a function call. It thus can be overwritten
                                             in the called function. */
	arch_register_type_ignore       = 4,  /**< Do not consider this register when allocating. */
	arch_register_type_joker        = 8,  /**< The emitter can choose an arbitrary register */
	arch_register_type_virtual      = 16, /**< This is just a virtual register  */
	arch_register_type_state        = 32,
} arch_register_type_t;

/**
 * Convenience macro to check for register type.
 * @param req   A pointer to register.
 * @param kind  The kind of type to check for (see arch_register_type_t).
 * @return      1, If register is of given kind, 0 if not.
 */
#define arch_register_type_is(reg, kind) \
  (((reg)->type & arch_register_type_ ## kind) != 0)

/**
 * A register.
 */
struct _arch_register_t {
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

static INLINE int _arch_register_get_index(const arch_register_t *reg)
{
	return reg->index;
}

static INLINE const char *_arch_register_get_name(const arch_register_t *reg)
{
	return reg->name;
}

#define arch_register_get_class(reg)      _arch_register_get_class(reg)
#define arch_register_get_index(reg)      _arch_register_get_index(reg)
#define arch_register_get_name(reg)       _arch_register_get_name(reg)

/**
 * A class of registers.
 * Like general purpose or floating point.
 */
struct _arch_register_class_t {
	const char *name;               /**< The name of the register class. */
	int n_regs;                     /**< Number of registers in this class. */
	ir_mode *mode;                  /**< The mode of the register class. */
	const arch_register_t *regs;    /**< The array of registers. */
};

/** return the number of registers in this register class */
#define arch_register_class_n_regs(cls) ((cls)->n_regs)

/** return the largest mode of this register class */
#define arch_register_class_mode(cls) ((cls)->mode)

/** return the name of this register class */
#define arch_register_class_name(cls) ((cls)->name)

/**
 * Put all registers in a class into a bitset.
 * @param cls The class.
 * @param bs The bitset. May be NULL.
 * @return The number of registers in the class.
 */
extern int arch_register_class_put(const arch_register_class_t *cls, bitset_t *bs);

static INLINE const arch_register_t *
_arch_register_for_index(const arch_register_class_t *cls, int idx)
{
	assert(0 <= idx && idx < cls->n_regs);
	return &cls->regs[idx];
}

#define arch_register_for_index(cls, idx) \
  _arch_register_for_index(cls, idx)

typedef enum _arch_operand_type_t {
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
typedef enum _arch_register_req_type_t {
	arch_register_req_type_none                         = 0,  /**< No register requirement. */
	arch_register_req_type_normal                       = 1,  /**< All registers in the class are allowed. */
	arch_register_req_type_limited                      = 2,  /**< Only a real subset of the class is allowed. */
	arch_register_req_type_should_be_same               = 4,  /**< The register should be equal another one at the node. */
	arch_register_req_type_should_be_different          = 8,  /**< The register must be unequal to some other at the node. */
	arch_register_req_type_should_be_different_from_all = 16, /**< The register must be different from all in's at the node */
} arch_register_req_type_t;

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
typedef struct _arch_register_req_t {
	arch_register_req_type_t type;      /**< The type of the constraint. */
	const arch_register_class_t *cls;   /**< The register class this constraint belongs to. */

	const unsigned *limited;            /**< allowed register bitset */

	int other_same;                     /**< The in number which shall have
										     the same res (should_be_same)*/
	int other_different;                /**< The other node from which this
										     one's register must be different
											 (case must_be_different). */
} arch_register_req_t;

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
typedef enum _arch_irn_class_t {
	arch_irn_class_normal     = 1 << 0,
	arch_irn_class_spill      = 1 << 1,
	arch_irn_class_reload     = 1 << 2,
	arch_irn_class_copy       = 1 << 3,
	arch_irn_class_perm       = 1 << 4,
	arch_irn_class_branch     = 1 << 5,
	arch_irn_class_call       = 1 << 6,
	arch_irn_class_const      = 1 << 7,
	arch_irn_class_load       = 1 << 8,
	arch_irn_class_store      = 1 << 9,
	arch_irn_class_stackparam = 1 << 10,
} arch_irn_class_t;

/**
 * An inverse operation returned by the backend
 */
typedef struct _arch_inverse_t {
	int      n;       /**< count of nodes returned in nodes array */
	int      costs;   /**< costs of this remat */

	/**< nodes for this inverse operation. shall be in
	 *  schedule order. last element is the target value
	 */
	ir_node  **nodes;
} arch_inverse_t;

/**
 * Some flags describing a node in more detail.
 */
typedef enum _arch_irn_flags_t {
	arch_irn_flags_none             = 0, /**< Node flags. */
	arch_irn_flags_dont_spill       = 1, /**< This must not be spilled. */
	arch_irn_flags_rematerializable = 2, /**< This can be replicated instead of spilled/reloaded. */
	arch_irn_flags_ignore           = 4, /**< Ignore node during register allocation. */
	arch_irn_flags_modify_sp        = 8, /**< I modify the stack pointer. */
	arch_irn_flags_last             = arch_irn_flags_modify_sp
} arch_irn_flags_t;

/**
 * Get the string representation of a flag.
 * This functions does not handle or'ed bitmasks of flags.
 * @param flag The flag.
 * @return The flag as a string.
 */
extern const char *arch_irn_flag_str(arch_irn_flags_t flag);

struct _arch_irn_ops_if_t {

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
struct _arch_irn_ops_t {
	const arch_irn_ops_if_t *impl;
};

extern const arch_irn_ops_t *arch_get_irn_ops(const arch_env_t *env, const ir_node *irn);

extern void arch_set_frame_offset(const arch_env_t *env, ir_node *irn, int bias);

extern ir_entity *arch_get_frame_entity(const arch_env_t *env, ir_node *irn);
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
 * Somebody who can be asked about IR nodes.
 */
struct _arch_irn_handler_t {

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
struct _arch_code_generator_if_t {
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
struct _arch_code_generator_t {
	const arch_code_generator_if_t *impl;
};

/**
 * ISA base class.
 */
struct _arch_isa_t {
	const arch_isa_if_t   *impl;
	const arch_register_t *sp;        /** The stack pointer register. */
	const arch_register_t *bp;        /** The base pointer register. */
	const int             stack_dir;  /** -1 for decreasing, 1 for increasing. */
	const be_main_env_t   *main_env;  /** the be main environment */
};

#define arch_isa_stack_dir(isa)  ((isa)->stack_dir)
#define arch_isa_sp(isa)         ((isa)->sp)
#define arch_isa_bp(isa)         ((isa)->bp)

/**
 * Architecture interface.
 */
struct _arch_isa_if_t {
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
	 * The first dimension is the type, the second the allowed units of this type.
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
	 * @param irgs   A flexible array ARR_F of length 0 where the backend cann append the desired irgs.
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
struct _arch_env_t {
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

/**
 * Initialize the architecture environment struct.
 * @param isa           The isa which shall be put into the environment.
 * @param file_handle   The file handle
 * @return The environment.
 */
extern arch_env_t *arch_env_init(arch_env_t *env, const arch_isa_if_t *isa, FILE *file_handle, be_main_env_t *main_env);

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

#endif /* _FIRM_BEARCH_H */
