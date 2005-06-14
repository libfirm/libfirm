
#ifndef _FIRM_BEARCH_H
#define _FIRM_BEARCH_H

#include "firm_config.h"

#include "irnode.h"
#include "irmode.h"

#include "hashptr.h"
#include "fourcc.h"
#include "set.h"
#include "list.h"
#include "ident.h"

struct _bitset_t;

typedef struct _arch_register_class_t   arch_register_class_t;
typedef struct _arch_register_t         arch_register_t;
typedef struct _arch_enum_t             arch_enum_t;
typedef struct _arch_enum_member_t      arch_enum_member_t;
typedef struct _arch_isa_if_t           arch_isa_if_t;
typedef struct _arch_env_t              arch_env_t;
typedef struct _arch_irn_ops_t          arch_irn_ops_t;
typedef struct _arch_irn_handler_t      arch_irn_handler_t;

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
	int index;																/**< The index of the register in the class. */
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
  const char *name;               /**< The name of the register. */
	int n_regs;								      /**< Number of registers in this class. */
	const arch_register_t *regs;    /**< The array of registers. */
};

#define arch_register_class_n_regs(cls) ((cls)->n_regs)

/**
 * Put all registers in a class into a bitset.
 * @param cls The class.
 * @param bs The bitset. May be NULL.
 * @return The number of registers in the class.
 */
extern int arch_register_class_put(const arch_register_class_t *cls,
    struct _bitset_t *bs);

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

/**
 * An immediate.
 */
struct _arch_immediate_t {
  const char *name;         /**< The name of the immediate. */
	ir_mode *mode;						/**< The mode of the immediate. */
};

/**
 * The member of an enum.
 */
struct _arch_enum_member_t {
	arch_enum_t *enm;					/**< The enum, this member belongs to. */
};

/**
 * An enumeration operand type.
 *
 * Enumeration operand types can be used to describe the variants
 * of an instruction, like giving the cases for a compare (gt, lt,
 * eq, ...) some other special attributes of an instruction.
 */
struct _arch_enum_t {
	int n_members;										/**< The number of members in this enum. */
	arch_enum_member_t *members[1];		/**< The array of members. */
};

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
  arch_register_req_type_none = 0,        /** No register requirement. */

  arch_register_req_type_normal = 1,      /** All registers in the class
                                            are allowed. */

  arch_register_req_type_limited = 2,     /** Only a real subset of
                                            the class is allowed. */

  arch_register_req_type_equal = 4,       /** The register must equal
                                            another one at the node. */

  arch_register_req_type_unequal = 8,     /** The register must be unequal
                                            to some other at the node. */

  arch_register_req_type_pair = 16        /** The register is part of a
                                            register pair. */
} arch_register_req_type_t;

#define arch_register_req_is_constr(x) \
  ((x)->type & (arch_register_req_type_pair + arch_register_req_type_limited - 1) != 0)

/**
 * Expresses requirements to register allocation for an operand.
 */
typedef struct _arch_register_req_t {
  arch_register_req_type_t type;          /** The type of the constraint. */
  const arch_register_class_t *cls;       /** The register class this
                                            constraint belongs to. */
  union {
    int (*limited)(const ir_node *irn, int pos, struct _bitset_t *bs);
                                          /** In case of the 'limited'
                                            constraint, this function
                                            must put all allowable
                                            registers in the bitset and
                                            return the number of registers
                                            in the bitset. */

    int pos;                             /** In case of the equal constraint,
                                            this gives the position of the
                                            operand to which the register of
                                            this should be equal to. Same for
                                            unequal. */
  } data;
} arch_register_req_t;

/**
 * Certain node classes which are relevent for the register allocator.
 */
typedef enum _arch_irn_class_t {
  arch_irn_class_normal,
  arch_irn_class_spill,
  arch_irn_class_reload,
  arch_irn_class_copy,
  arch_irn_class_perm,
  arch_irn_class_branch
} arch_irn_class_t;

/*
 * Some words about positions and indices:
 *
 * Firm has the policy "One node per value", that's why there are
 * Proj nodes. This view has its advantages, but in a backend
 * setting where we talk about instructions (which can also have
 * multiple results and not a single Tuple value) this is sometimes
 * hard.
 *
 * Each node representing an instruction must provide information
 * about the kind of its operands (where operands mean both input
 * and output operands). Such an operand is addressed with a position
 * which is infact a tuple {in, out} x N. The fact that a position
 * is an input/output operand is encoded in the sign, so input operands
 * go from 0..n-1 and output operands from -1..-m if the
 * instruction has n input and m output operands.
 */

#define _BEARCH_TRANSFORM_INDEX(cmp, index) ((index) cmp 0 ? -((index) + 1) : (index))

/**
 * Make an in position from an index.
 * @param index The index.
 * @return The position representing the index as an in operand.
 */
#define arch_pos_make_in(index)   _BEARCH_TRANSFORM_INDEX(<, index)

/**
 * Make an out position from an index.
 * @param index The index.
 * @return The position representing the index as an out operand.
 */
#define arch_pos_make_out(index)  _BEARCH_TRANSFORM_INDEX(>=, index)

/**
 * Check, if a position denotes an input operand.
 * @param pos The position.
 * @return 1, if the position denotes an input operand 0 if not.
 */
#define arch_pos_is_in(pos)       ((pos) >= 0)

/**
 * Check, if a position denotes an output operand.
 * @param pos The position.
 * @return 1, if the position denotes an output operand 0 if not.
 */
#define arch_pos_is_out(pos)      (!arch_pos_is_in(pos))

/**
 * Get the index of a position.
 * @param pos The position.
 * @return The index of the position.
 */
#define arch_pos_get_index(pos)   _BEARCH_TRANSFORM_INDEX(<, pos)

struct _arch_irn_ops_t {

  /**
   * Get the register requirements for a given operand.
   * @param self The self pointer.
   * @param irn The node.
   * @param pos The operand's position.
   * @return    The register requirements for the selected operand.
   *            The pointer returned is never NULL.
   */
  const arch_register_req_t *(*get_irn_reg_req)(const arch_irn_ops_t *self,
      arch_register_req_t *req,
      const ir_node *irn, int pos);

  /**
   * Get the number of operands of a node.
   * @param irn     The node.
   * @param in_out  Denotes wither input (a number >= 0) or
   *                output (a number < 0).
   * @return        The number of operands for either in, or output.
   */
  int (*get_n_operands)(const arch_irn_ops_t *self, const ir_node *irn, int in_out);

  /**
   * Set the register for an output operand.
   * @param irn The node.
   * @param pos The position of the output operand.
   * @param reg The register allocated to that operand.
   * @note      If the operand is not a register operand,
   *            the call is ignored.
   */
  void (*set_irn_reg)(const arch_irn_ops_t *self, ir_node *irn,
      int idx, const arch_register_t *reg);

  /**
   * Get the register allocated for an output operand.
   * @param irn The node.
   * @param pos The index of the output operand.
   * @return    The register allocated at that operand. NULL, if
   *            the operand was no register operand or
   *            @c arch_register_invalid, if no register has yet been
   *            allocated for this node.
   */
  const arch_register_t *(*get_irn_reg)(const arch_irn_ops_t *self,
      const ir_node *irn, int idx);

  /**
   * Classify the node.
   * @param irn The node.
   * @return A classification.
   */
  arch_irn_class_t (*classify)(const arch_irn_ops_t *self, const ir_node *irn);

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
    int pos, const arch_register_class_t *cls, struct _bitset_t *bs);

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
 * @param idx The position of the operand.
 * @return    The register class of the operand or NULL, if
 *            operand is a non-register operand.
 */
extern const arch_register_class_t *
arch_get_irn_reg_class(const arch_env_t *env, const ir_node *irn, int pos);

/**
 * Get the register allocated at a certain output operand of a node.
 * @param env The arch nvironment.
 * @param irn The node.
 * @param idx The index of the output operand.
 * @return    The register allocated for this operand
 */
extern const arch_register_t *
arch_get_irn_register(const arch_env_t *env, const ir_node *irn, int idx);

/**
 * Set the register for a certain output operand.
 * @param env The architecture environment.
 * @param irn The node.
 * @param idx The index of the output operand.
 * @param reg The register.
 */
extern void arch_set_irn_register(const arch_env_t *env,
    ir_node *irn, int idx, const arch_register_t *reg);

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
 * Architecture interface.
 */
struct _arch_isa_if_t {

  /**
   * Initialize the isa interface.
   */
  void (*init)(void);

  /**
   * Get the the number of register classes in the isa.
   * @return The number of register classes.
   */
  int (*get_n_reg_class)(void);

  /**
   * Get the i-th register class.
   * @param i The number of the register class.
   * @return The register class.
   */
  const arch_register_class_t *(*get_reg_class)(int i);

};

#define ARCH_MAX_HANDLERS         8

/**
 * Environment for the architecture infrastructure.
 * Keep this everywhere you're going.
 */
struct _arch_env_t {
  const struct _be_node_factory_t *node_factory;  /**< The node factory for be nodes. */
  const arch_isa_if_t *isa;               /**< The isa about which everything is. */

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
