
#ifndef _FIRM_BEARCH_T_H
#define _FIRM_BEARCH_T_H

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

#define ARCH_IRN_FOURCC		FOURCC('A', 'R', 'C', 'H')

/**
 * Flags for registers.
 */
enum {

	/**
	 * The register is invariant concerning writes.
	 * Examples are the 0 registers in RISC architectures.
	 */
	REG_WRITE_INVARIAT 				= 1

} arch_register_flags_t;

typedef enum {
#define ARCH_OBJ(x,list) 	arch_kind_##x,
#include "bearch_obj.def"
#undef ARCH_OBJ
	arch_kind_last
} arch_kind_t;

/**
 * A header which each of the arch structs should posess.
 */
typedef struct {
	arch_kind_t kind;
	arch_isa_t *isa;
	const char *name;
	struct list_head list;
	unsigned is_new : 1;
} arch_header_t;


/**
 * Get the architecture an arch object belongs to.
 * @param obj The object.
 * @return The architecture it belongs to.
 */
static INLINE arch_isa_t *arch_obj_get_isa(const void *obj)
{
	return ((const arch_header_t *) obj)->isa;
}

typedef enum _arch_register_flag_t {
	arch_register_flag_none,
	arch_register_flag_caller_saved,						/**< The register must be saved by the caller
																								upon a function call. It thus can be overwritten
																								in the called function. */
	arch_register_flag_callee_saved,						/**< The register must be saved by the called function,
																								it thus survives a function call. */
	arch_register_flag_ignore										/**< Do not consider this register when allocating. */
} arch_register_flag_t;

/**
 * A register.
 */
struct _arch_register_t {
	arch_header_t header;
	int index;																/**< The index of the register in the class. */
	const arch_register_class_t *reg_class;		/**< The class the register belongs to. */
	arch_register_flag_t flags;								/**< Flags describing several properties of
																							the register. */
};

/**
 * A (sub-) set of registers.
 */
struct _arch_register_set_t {
	arch_header_t header;
	const struct _arch_register_class_t *reg_class;		/**< The register class for this set. */
	unsigned comprises_full_class : 1;								/**< True, if all registers of the class
																											are contained in this set. */
	int regs[1];																			/**< An array containing 0/1 at place i
																											whether the register with index i is
																											in the set or not. */
};

static INLINE int _arch_register_in_set(const arch_register_set_t *set, const arch_register_t *reg)
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
	arch_header_t header;
	struct list_head list;		/**< list head to list up in the list of all
															register classes in an isa. */
	arch_register_set_t *set; /**< A register set containing all registers
															in this class. */
	int n_regs;								/**< Number of registers in this class. */
	arch_register_t *regs[1]; /**< The array of registers. */
};

static INLINE const arch_register_t *_arch_register_for_index(const arch_register_class_t *cls, int idx)
{
	assert(0 <= idx && idx <= cls->n_regs);
	return cls->regs[idx];
}

/**
 * Get the register set for a register class.
 * @param cls The register class.
 * @return The set containing all registers in the class.
 */
static INLINE arch_register_set_t *_arch_get_register_set_for_class(const arch_register_class_t *cls)
{
	return cls->set;
}

/**
 * An immediate.
 */
struct _arch_immediate_t {
	arch_header_t header;
	ir_mode *mode;						/**< The mode of the immediate. */
};

/**
 * The member of an enum.
 */
struct _arch_enum_member_t {
	arch_header_t header;			/**< The omnipresent header. */
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
	arch_header_t header;
	int n_members;										/**< The number of members in this enum. */
	arch_enum_member_t *members[1];		/**< The array of members. */
};

typedef enum _arch_operand_type_t {
	arch_operand_type_ir = 0,
	arch_operand_type_variadic,
	arch_operand_type_symconst,
	arch_operand_type_register_set,
	arch_operand_type_immediate
} arch_operand_type_t;



/**
 * The data for the different flavours of operand types.
 */
typedef union _arch_operand_data_t {
	arch_register_callback_t *callback;				/**< The set of valid registers is determined
																							by a callback function. */

	const arch_register_set_t *set;						/**< The set of valid registers is directly
																							given. Note, that if an insn has no constraints,
																							the set comprises all registers in the
																							register class. */

	const arch_immediate_t *imm;							/**< If the operand is an immediate
																							operand, this describes the kind of
																							immediate. */

	const arch_enum_t *enm;										/**< Some enumeration value. */
} arch_operand_data_t;

/**
 * An operand to an instruction.
 */
struct _arch_operand_t {
	int offset_in_irn_data;
	arch_operand_type_t type;									/**< The type of the operand. */
	arch_operand_data_t data;									/**< The payload. */
};

/**
 * An instruction format.
 */
struct _arch_insn_format_t {
	arch_header_t header;
	int n_in;																	/**< Number of in operands. */
	int n_out;																/**< Number of out operands. */
	int irn_data_size;

	arch_operand_t operands[1];	/**< Array with operands. */
};

#define arch_get_in_operand(fmt, index) 		(&((fmt)->operands[(fmt)->n_out + (index)]))
#define arch_get_out_operand(fmt, index) 		(&((fmt)->operands[index]))

/**
 * An instruction.
 */
struct _arch_insn_t {
	arch_header_t header;
	const arch_insn_format_t *format;			/**< The format of the instruction. */
	ir_op *op;														/**< The firm opcode for this insn. */
};

/**
 * This truct is placed into each ir_node which is made from an arch
 * insn (If the node is made via arch_new node).
 */
typedef struct _arch_irn_data_t {
	unsigned magic;												/**< A magic number to tell if node is an
																					arch node. */
	const arch_insn_t *insn;							/**< The insn this nodes instantiates. */
} arch_irn_data_t;

#define _arch_get_irn_data(irn) ((const arch_irn_data_t *) &((irn)->attr))

/**
 * Check, if an ir node is made by the arch module.
 * @param irn An ir node.
 * @return 1 if the node was made via arch_new_node() or 0 otherwise.
 */
static INLINE int _arch_is_irn(const ir_node *irn)
{
	return _arch_get_irn_data(irn)->magic == ARCH_IRN_FOURCC;
}

static INLINE const arch_insn_t *_arch_irn_get_insn(const ir_node *irn)
{
	if(!_arch_is_irn(irn))
		return NULL;

	return _arch_get_irn_data(irn)->insn;
}


/**
 * An instruction set architecture.
 */
struct _arch_isa_t {
	arch_header_t header;
	struct list_head heads[arch_kind_last];		/**< List heads to list objects created in the
																							context of this isa. Note: some of the list heads
																							remain unused. */
};

struct _arch_implementation_t {
	const arch_isa_t *isa;
	const char *name;
};


#endif /* _FIRM_BEARCH_T_H */
