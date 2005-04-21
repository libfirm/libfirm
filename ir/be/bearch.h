
#ifndef _FIRM_BEARCH_H
#define _FIRM_BEARCH_H

#include "firm_config.h"

#include "irop_t.h"
#include "irnode_t.h"
#include "irmode_t.h"

#include "hashptr.h"
#include "fourcc.h"
#include "set.h"
#include "list.h"
#include "ident.h"

#include "bearch.h"

#define ARCH_OBJ(name,x) typedef struct _arch_ ## name ## _t arch_ ## name ## _t;
#include "bearch_obj.def"
#undef ARCH_OBJ

struct _bitset_t;

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

/**
 * A (sub-) set of registers.
 */
struct _arch_register_set_t {
	const struct _arch_register_class_t *reg_class;   /**< The register class for this set. */
	int *regs;                                        /**< An array containing 0/1 at place i
																											whether the register with index i is
																											in the set or not. */
};

static INLINE int _arch_register_in_set(const arch_register_set_t *set,
    const arch_register_t *reg)
{
	if(reg->reg_class != set->reg_class)
		return 0;

	return set->regs[reg->index];
}


/**
 * A class of registers.
 * Like general purpose or floating point.
 */
struct _arch_register_class_t {
  const char *name;         /**< The name of the register. */
	arch_register_set_t *set; /**< A register set containing all registers
															in this class. */
	int n_regs;								/**< Number of registers in this class. */
	const arch_register_t *regs; /**< The array of registers. */
};

#define arch_register_class_n_regs(cls) ((cls)->n_regs)

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
#define ARCH_OPERAND_TYPE(name,size_in_irn) arch_operand_type_ ## name,
#include "bearch_operand_types.def"
#undef ARCH_OPERAND_TYPE
	arch_operand_type_last
} arch_operand_type_t;



/**
 * The data for the different flavours of operand types.
 */
typedef union _arch_operand_data_t {
	const arch_register_set_t *set;       /**< The set of valid registers is directly
                                          given. Note, that if an insn has no constraints,
                                          the set comprises all registers in the
                                          register class. */

	const arch_immediate_t *imm;          /**< If the operand is an immediate
                                          operand, this describes the kind of
                                          immediate. */

	const arch_enum_t *enm;               /**< Some enumeration value. */

	int same_as_pos;                      /**< 'Same as' position for equals. */
} arch_operand_data_t;

/**
 * An operand to an instruction.
 */
struct _arch_operand_t {
	arch_operand_type_t type;									/**< The type of the operand. */
	arch_operand_data_t data;									/**< The payload. */
};

/**
 * An instruction format.
 */
struct _arch_insn_format_t {
	int n_in;                       /**< Number of in operands. */
	int n_out;                      /**< Number of out operands. */

	arch_operand_t * const *in_operands;    /**< In operands. */
	arch_operand_t * const *out_operands;   /**< Out operands. */
};

/**
 * An instruction.
 */
struct _arch_insn_t {
	const arch_insn_format_t *format;			/**< The format of the instruction. */
	ir_op *op;														/**< The firm opcode for this insn. */
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

  /**
   * Check, if a register is suitable to carry the node's value.
   * @param irn The node.
   * @param reg The register to check for.
   * @return 1, if the register can be allocated for that node, 0 if
   * not.
   */
  int (*is_reg_allocatable)(const ir_node *irn, const arch_register_t *reg);

  /**
   * Put all registers of a given class which are allocatable to a
   * certain node into a bitset.
   * The bitset contains the indices of the registers concerning
   * the register class @p cls.
   * @param irn The node.
   * @param cls The register class.
   * @param bs The bitset.
   * @return The number of registers which were put into the bitset.
   */
  int (*get_allocatable_regs)(const ir_node *irn,
      const arch_register_class_t *cls, struct _bitset_t *bs);

  /**
   * Get the register class, the value of a node belongs to.
   * @param irn The node.
   * @return The register class of the registers which can hold the
   * value of irn. If the node does not return a value, or possesses no
   * suitable register class, NULL is returned.
   */
  const arch_register_class_t *(*get_irn_reg_class)(const ir_node *irn);

  /**
   * Get an op for a name.
   * @note This method may not be implemented.
   * @param name The name of the op.
   * @return The op with that name.
   */
  ir_op *(*get_op_by_name)(const char *name);
};

/**
 * Check, if the value of a node can be stored in a
 * specific register class.
 * @param isa The isa.
 * @param irn The node.
 * @param cls The register class.
 * @return 1, if the value can be stored in the register class, 0 if
 * not.
 */
#define arch_isa_irn_has_reg_class(isa, irn, cls) \
  ((isa)->get_irn_reg_class(irn) == (cls))

#endif /* _FIRM_BEARCH_H */
