#ifndef _FIRM_BEARCH_H
#define _FIRM_BEARCH_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#ifdef WITH_LIBCORE
#include <libcore/lc_opts.h>
#endif

#include "irnode.h"
#include "irmode.h"

#include "bitset.h"
#include "hashptr.h"
#include "fourcc.h"
#include "set.h"
#include "list.h"
#include "ident.h"

#include "belistsched.h"

typedef struct _arch_register_class_t     arch_register_class_t;
typedef struct _arch_register_t           arch_register_t;
typedef struct _arch_isa_if_t             arch_isa_if_t;
typedef struct _arch_isa_t                arch_isa_t;
typedef struct _arch_env_t                arch_env_t;
typedef struct _arch_irn_ops_t            arch_irn_ops_t;
typedef struct _arch_irn_handler_t        arch_irn_handler_t;
typedef struct _arch_code_generator_t     arch_code_generator_t;
typedef struct _arch_code_generator_if_t  arch_code_generator_if_t;

struct _be_node_factory_t;

typedef enum _arch_register_type_t {
  arch_register_type_none = 0,
  arch_register_type_write_invariant,
  arch_register_type_caller_saved,    /**< The register must be saved by the caller
                                           upon a function call. It thus can be overwritten
                                           in the called function. */
  arch_register_type_callee_saved,    /**< The register must be saved by the called function,
                                           it thus survives a function call. */
  arch_register_type_ignore           /**< Do not consider this register when allocating. */
} arch_register_type_t;

/**
 * A register.
 */
struct _arch_register_t {
  const char *name;                         /**< The name of the register. */
  const arch_register_class_t *reg_class;   /**< The class the register belongs to. */
  int index;                                /**< The index of the register in the class. */
  arch_register_type_t type;                /**< The type of the register. */
  void *data;                               /**< Custom data. */
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

#define arch_register_get_class(reg)      _arch_register_get_class(reg)
#define arch_register_get_index(reg)      _arch_register_get_index(reg)
#define arch_register_get_name(reg)       ((reg)->name)

/**
 * A class of registers.
 * Like general purpose or floating point.
 */
struct _arch_register_class_t {
  const char *name;               /**< The name of the register class. */
  int n_regs;                     /**< Number of registers in this class. */
  const arch_register_t *regs;    /**< The array of registers. */
};

#define arch_register_class_n_regs(cls) ((cls)->n_regs)

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

/**
 * Get the register set for a register class.
 * @param cls The register class.
 * @return The set containing all registers in the class.
 */
#define arch_get_register_set_for_class(cls) ((cls)->set)

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
  arch_register_req_type_none = 0,        /**< No register requirement. */

  arch_register_req_type_normal = 1,      /**< All registers in the class
                                               are allowed. */

  arch_register_req_type_limited = 2,     /**< Only a real subset of
                                               the class is allowed. */

  arch_register_req_type_should_be_same = 4,       /**< The register should be equal
                                                        another one at the node. */

  arch_register_req_type_should_be_different = 8,  /**< The register must be unequal
                                                        to some other at the node. */

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
	arch_register_req_type_t type;          /**< The type of the constraint. */
	const arch_register_class_t *cls;       /**< The register class this constraint belongs to. */

	int (*limited)(const ir_node *irn, int pos, bitset_t *bs);
                                          /**< In case of the 'limited'
                                            constraint, this function
                                            must put all allowable
                                            registers in the bitset and
                                            return the number of registers
                                            in the bitset. */

	ir_node *other;						  /**< In case of "should be equal"
										    or should be different, this gives
											the node to whose register this
											one's should be the same/different. */
} arch_register_req_t;

/**
 * Certain node classes which are relevant for the register allocator.
 */
typedef enum _arch_irn_class_t {
  arch_irn_class_normal,
  arch_irn_class_spill,
  arch_irn_class_reload,
  arch_irn_class_copy,
  arch_irn_class_perm,
  arch_irn_class_branch
} arch_irn_class_t;

/**
 * Some flags describing a node in more detail.
 */
typedef enum _arch_irn_flags_t {
  arch_irn_flags_spillable = 1,
  arch_irn_flags_rematerializable = 2
} arch_irn_flags_t;

struct _arch_irn_ops_t {

  /**
   * Get the register requirements for a given operand.
   * @param self The self pointer.
   * @param irn The node.
   * @param pos The operand's position
	 * 						(-1 for the result of the node, 0..n for the input
	 * 						operands).
   * @return    The register requirements for the selected operand.
   *            The pointer returned is never NULL.
   */
  const arch_register_req_t *(*get_irn_reg_req)(const arch_irn_ops_t *self,
      arch_register_req_t *req, const ir_node *irn, int pos);

  /**
   * Set the register for an output operand.
   * @param irn The node.
   * @param reg The register allocated to that operand.
   * @note      If the operand is not a register operand,
   *            the call is ignored.
   */
  void (*set_irn_reg)(const arch_irn_ops_t *self, ir_node *irn, const arch_register_t *reg);

  /**
   * Get the register allocated for an output operand.
   * @param irn The node.
   * @return    The register allocated at that operand. NULL, if
   *            the operand was no register operand or
   *            @c arch_register_invalid, if no register has yet been
   *            allocated for this node.
   */
  const arch_register_t *(*get_irn_reg)(const arch_irn_ops_t *self, const ir_node *irn);

  /**
   * Classify the node.
   * @param irn The node.
   * @return A classification.
   */
  arch_irn_class_t (*classify)(const arch_irn_ops_t *self, const ir_node *irn);

  /**
   * Get the flags of a node.
   * @param self The irn ops themselves.
   * @param irn The node.
   * @return A set of flags.
   */
  arch_irn_flags_t (*get_flags)(const arch_irn_ops_t *self, const ir_node *irn);

};

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
arch_get_register_req(const arch_env_t *env, arch_register_req_t *req,
    const ir_node *irn, int pos);

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
 * @param cls The register class.
 * @param bs  The bitset all allocatable registers shall be put into.
 *            Note, that you can also pass NULL here. If you don't,
 *            make sure, the bitset is as large as the register class
 *            has registers.
 * @return    The amount of registers allocatable for that operand.
 */
extern int arch_get_allocatable_regs(const arch_env_t *env, const ir_node *irn,
    int pos, const arch_register_class_t *cls, bitset_t *bs);

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
 * @param pos The position of the operand.
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

/**
 * Get the flags of a node.
 * @param env The architecture environment.
 * @param irn The node.
 * @return The flags.
 */
extern arch_irn_flags_t arch_irn_get_flags(const arch_env_t *env, const ir_node *irn);

#define arch_irn_has_reg_class(env, irn, pos, cls) \
  ((cls) == arch_get_irn_reg_class(env, irn, pos))

/**
 * Somebody who can be asked about nodes.
 */
struct _arch_irn_handler_t {

  /**
    * Get the operations of an irn.
    * @param self The handler from which the method is invoked.
    * @param irn Some node.
    * @return Operations for that irn.
    */
  const arch_irn_ops_t *(*get_irn_ops)(const arch_irn_handler_t *handler,
      const ir_node *irn);

};

/**
 * The code generator.
 */
struct _arch_code_generator_if_t {


	/**
	 * Initialize the code generator.
	 * @param file The file to dump to.
	 * @param irg  The function to generate code for.
	 * @param env  The architecture environment.
	 * @return     A newly created code generator.
	 */
	void *(*init)(FILE *file, ir_graph *irg, const arch_env_t *env);

	/**
	 * Called, when the graph is being normalized.
	 */
	void (*prepare_graph)(void *self);

	/**
	 * Called before scheduling.
	 */
	void (*before_sched)(void *self);

	/**
	 * Called before register allocation.
	 */
	void (*before_ra)(void *self);

	/**
	 * Called after everything happened.
	 * The code generator must also be de-allocated here.
	 */
	void (*done)(void *self);

};

#define _arch_cg_call(cg, func) \
do { \
	if((cg)->impl->func) \
		(cg)->impl->func(cg); \
} while(0)

#define arch_code_generator_prepare_graph(cg)   _arch_cg_call(cg, prepare_graph)
#define arch_code_generator_before_sched(cg)    _arch_cg_call(cg, before_sched)
#define arch_code_generator_before_ra(cg)       _arch_cg_call(cg, before_ra)
#define arch_code_generator_done(cg)            _arch_cg_call(cg, done)

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
	const arch_isa_if_t *impl;
};

/**
 * Architecture interface.
 */
struct _arch_isa_if_t {

#ifdef WITH_LIBCORE
  void (*register_options)(lc_opt_entry_t *grp);
#endif

  /**
   * Initialize the isa interface.
   */
  void *(*init)(void);

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
  const arch_code_generator_if_t *(*get_code_generator)(void *self);

  /**
   * Get the list scheduler to use.
   * @param self  The isa object.
   * @return      The list scheduler selector.
   */
  const list_sched_selector_t *(*get_list_sched_selector)(const void *self);
};

#define arch_isa_get_n_reg_class(isa)           ((isa)->impl->get_n_reg_class(isa))
#define arch_isa_get_reg_class(isa,i)           ((isa)->impl->get_reg_class(isa, i))
#define arch_isa_get_irn_handler(isa)           ((isa)->impl->get_irn_handler(isa))
#define arch_isa_make_code_generator(isa,irg)   ((isa)->impl->make_code_generator(isa, irg))

#define ARCH_MAX_HANDLERS         8

/**
 * Environment for the architecture infrastructure.
 * Keep this everywhere you're going.
 */
struct _arch_env_t {
  const struct _be_node_factory_t *node_factory;  /**< The node factory for be nodes. */
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
 * @param isa The isa which shall be put into the environment.
 * @return The environment.
 */
extern arch_env_t *arch_env_init(arch_env_t *env, const arch_isa_if_t *isa);

/**
 * Add a node handler to the environment.
 * @param env The environment.
 * @param handler A node handler.
 * @return The environment itself.
 */
extern arch_env_t *arch_env_add_irn_handler(arch_env_t *env,
    const arch_irn_handler_t *handler);

#endif /* _FIRM_BEARCH_H */
