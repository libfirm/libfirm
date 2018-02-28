/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Processor architecture specification.
 * @author      Sebastian Hack
 */
#ifndef FIRM_BE_BEARCH_H
#define FIRM_BE_BEARCH_H

#include <stdbool.h>

#include "firm_types.h"
#include "jit.h"
#include "raw_bitset.h"

#include "be_types.h"
#include "beinfo.h"
#include "be.h"

extern arch_register_class_t arch_exec_cls;

extern arch_register_req_t const arch_exec_requirement;
#define arch_exec_req (&arch_exec_requirement)

extern arch_register_req_t const arch_memory_requirement;
#define arch_memory_req (&arch_memory_requirement)

extern arch_register_req_t const arch_no_requirement;
#define arch_no_register_req (&arch_no_requirement)

static inline reg_out_info_t *get_out_info_n(ir_node const *const node, unsigned const pos)
{
	backend_info_t const *const info = be_get_info(node);
	assert(pos < ARR_LEN(info->out_infos));
	return &info->out_infos[pos];
}

/**
 * Get the register allocated for a value.
 */
const arch_register_t *arch_get_irn_register(const ir_node *irn);

/**
 * Assign register to a value
 */
void arch_set_irn_register(ir_node *irn, const arch_register_t *reg);

/**
 * Set the register for a certain output operand.
 */
void arch_set_irn_register_out(ir_node *irn, unsigned pos, const arch_register_t *r);

const arch_register_t *arch_get_irn_register_out(const ir_node *irn, unsigned pos);
const arch_register_t *arch_get_irn_register_in(const ir_node *irn, int pos);

/**
 * Get register constraints for an operand at position @p
 */
static inline const arch_register_req_t *arch_get_irn_register_req_in(
		const ir_node *node, int pos)
{
	const backend_info_t *info = be_get_info(node);
	return info->in_reqs[pos];
}

/**
 * Get register constraint for a produced result (the @p pos result)
 */
static inline const arch_register_req_t *arch_get_irn_register_req_out(
		const ir_node *node, unsigned pos)
{
	reg_out_info_t const *const out = get_out_info_n(node, pos);
	return out->req;
}

static inline void arch_set_irn_register_req_out(ir_node *node, unsigned pos,
		const arch_register_req_t *req)
{
	reg_out_info_t *const out = get_out_info_n(node, pos);
	out->req = req;
}

static inline void arch_set_irn_register_reqs_in(ir_node *node,
		const arch_register_req_t **reqs)
{
	backend_info_t *info = be_get_info(node);
	info->in_reqs = reqs;
}

static inline const arch_register_req_t **arch_get_irn_register_reqs_in(
		const ir_node *node)
{
	backend_info_t *info = be_get_info(node);
	return info->in_reqs;
}

static inline reg_out_info_t *get_out_info(const ir_node *node)
{
	assert(get_irn_mode(node) != mode_T);
	size_t pos = 0;
	if (is_Proj(node)) {
		pos  = get_Proj_num(node);
		node = get_Proj_pred(node);
	}

	return get_out_info_n(node, pos);
}

static inline const arch_register_req_t *arch_get_irn_register_req(const ir_node *node)
{
	reg_out_info_t *out = get_out_info(node);
	return out->req;
}



/**
 * Get the flags of a node.
 * @param irn The node.
 * @return The flags.
 */
static inline arch_irn_flags_t arch_get_irn_flags(const ir_node *node)
{
	backend_info_t const *const info = be_get_info(node);
	return info->flags;
}

static inline void arch_add_irn_flags(ir_node *const node, arch_irn_flags_t const flags)
{
	backend_info_t *const info = be_get_info(node);
	info->flags |= flags;
}

void arch_set_additional_pressure(ir_node *node, arch_register_class_t const *,
                                  be_add_pressure_t pressure);

be_add_pressure_t arch_get_additional_pressure(ir_node const *node, arch_register_class_t const *cls);

/**
 * Returns true if the given node should not be scheduled (has
 * arch_irn_flag_not_scheduled flag set)
 */
static inline bool arch_is_irn_not_scheduled(const ir_node *node)
{
	return is_Proj(node)
	    || (arch_get_irn_flags(node) & arch_irn_flag_not_scheduled);
}

#define arch_irn_is(irn, flag) ((arch_get_irn_flags(irn) & arch_irn_flag_ ## flag) != 0)

static inline unsigned arch_get_irn_n_outs(const ir_node *node)
{
	backend_info_t *const info = be_get_info(node);
	return (unsigned)ARR_LEN(info->out_infos);
}

#define be_foreach_out(node, i) \
	for (unsigned i = 0, i##__n = arch_get_irn_n_outs(node); i != i##__n; ++i)

/**
 * A register.
 */
struct arch_register_t {
	const char                  *name;         /**< The name of the register. */
	arch_register_class_t const *cls;          /**< The class of the register */
	/** register constraint allowing just this register */
	const arch_register_req_t   *single_req;
	unsigned short               index;        /**< The index of the register in
	                                                the class. */
	/** The global index this register in the architecture. */
	unsigned short               global_index;
	/** register number in dwarf debugging format */
	unsigned short               dwarf_number;
	/** register number in instruction encoding */
	unsigned short               encoding;
	/** This is just a virtual register. Virtual registers fulfill any register
	 * constraints as long as the register class matches. It is allowed to
	 * have multiple definitions for the same virtual register at a point */
	bool                         is_virtual : 1;
};

/**
 * A class of registers.
 * Like general purpose or floating point.
 */
struct arch_register_class_t {
	const char                *name;   /**< The name of the register class.*/
	ir_mode                   *mode;   /**< The mode of the register class.*/
	const arch_register_t     *regs;   /**< The array of registers. */
	const arch_register_req_t *class_req;
	unsigned                   index;  /**< index of this register class */
	unsigned                   n_regs; /**< Number of registers in this
	                                        class. */
	/** don't do register allocation for this class */
	bool                       manual_ra : 1;
	/** Still allow clobbered registers as input. */
	bool                       allow_clobber_input : 1;
	bool                       double_registers_allowed : 1;
};

static inline const arch_register_t *arch_register_for_index(
		const arch_register_class_t *cls, unsigned idx)
{
	assert(idx < cls->n_regs);
	return &cls->regs[idx];
}

/**
 * Expresses requirements to register allocation for an operand.
 */
struct arch_register_req_t {
	/** The register class this constraint belongs to. */
	const arch_register_class_t *cls;
	/** allowed register bitset (in case of wide-values this is only about the
	 * first register). NULL if all registers are allowed. */
	const unsigned              *limited;
	/** Bitmask of ins which should use the same register. */
	unsigned                     should_be_same;
	/** Bitmask of ins which shall use a different register (must_be_different) */
	unsigned                     must_be_different;
	/** Specifies how many sequential registers are required */
	unsigned char                width;
	/** ignore this input/output while allocating registers */
	bool                         ignore : 1;
	/** The instructions modifies the value in the register in an unknown way,
	 * the value has to be copied if it is needed afterwards. */
	bool                         kills_value : 1;
};

static inline bool reg_reqs_equal(const arch_register_req_t *req1,
                                  const arch_register_req_t *req2)
{
	if (req1 == req2)
		return true;

	if (req1->cls               != req2->cls               ||
	    req1->should_be_same    != req2->should_be_same    ||
	    req1->must_be_different != req2->must_be_different ||
	    req1->width             != req2->width             ||
	    req1->ignore            != req2->ignore            ||
	    req1->kills_value       != req2->kills_value       ||
	    (req1->limited != NULL) != (req2->limited != NULL))
		return false;

	if (req1->limited != NULL) {
		if (!rbitsets_equal(req1->limited, req2->limited, req1->cls->n_regs))
			return false;
	}

	return true;
}

static inline bool reg_req_has_constraint(const arch_register_req_t *req)
{
	return req->limited || req->must_be_different != 0 || req->ignore || req->width != 1;
}

/**
 * Architecture interface.
 */
struct arch_isa_if_t {
	char const *name;
	uint8_t     pointer_size;           /**< Pointer size in bytes */
	uint16_t    modulo_shift;           /**< Target modulo shift value */
	bool        big_endian;             /**< Target is big endian */
	uint8_t     po2_biggest_alignment;  /**< power of 2 of biggest alignment
	                                         necessary/recommended for any data
	                                         type on the target. */
	bool        pic_supported;

	unsigned                     n_registers;        /**< number of registers */
	arch_register_t       const *registers;          /**< register array */
	unsigned                     n_register_classes; /**< number of register classes */
	arch_register_class_t const *register_classes;   /**< register classes */

	/**
	 * Initializes the isa interface. This is necessary before calling any
	 * other functions from this interface. Also initializes the target
	 * information in ir_target.
	 */
	void (*init)(void);

	/**
	 * Fress resources allocated by this isa interface.
	 */
	void (*finish)(void);

	/**
	 * Generate code for the current firm program.
	 */
	void (*generate_code)(FILE *output, const char *cup_name);

	ir_jit_function_t* (*jit_compile)(ir_jit_segment_t *segment, ir_graph *irg);

	void (*emit_function)(char *buffer, ir_jit_function_t *function);

	/**
	 * lowers current program for target. See the documentation for
	 * be_lower_for_target() for details.
	 */
	void (*lower_for_target)(void);

	/**
	 * Additional register names in addition to the regular register names.
	 */
	be_register_name_t const *additional_reg_names;

	/**
	 * Architecture-specific name prefix for registers.
	 */
	char register_prefix;

	/**
	 * Called directly after initialization. Backend should handle all
	 * intrinsics here.
	 */
	void (*handle_intrinsics)(ir_graph *irg);

	/**
	 * Get a cost estimation for node @p irn. The cost should be similar to the
	 * number of cycles necessary to execute the instruction.
	 */
	unsigned (*get_op_estimated_cost)(const ir_node *irn);
};

static inline bool arch_irn_is_ignore(const ir_node *irn)
{
	const arch_register_req_t *req = arch_get_irn_register_req(irn);
	return req->ignore;
}

/**
 * Get the required register width (number of single registers) of a node
 */
static inline unsigned char arch_get_irn_register_req_width(const ir_node *node)
{
	reg_out_info_t *out = get_out_info(node);
	return out->req->width;
}

static inline bool arch_irn_consider_in_reg_alloc(
		const arch_register_class_t *cls, const ir_node *node)
{
	const arch_register_req_t *req = arch_get_irn_register_req(node);
	return req->cls == cls && !req->ignore;
}

arch_register_t const *arch_find_register(char const *name);

#define be_foreach_value(node, value, code) \
	do { \
		if (get_irn_mode(node) == mode_T) { \
			foreach_out_edge(node, node##__edge) { \
				ir_node *const value = get_edge_src_irn(node##__edge); \
				if (!is_Proj(value)) \
					continue; \
				code \
			} \
		} else { \
			ir_node *const value = node; \
			code \
		} \
	} while (0)

#define be_foreach_definition_(node, ccls, value, req, code) \
	be_foreach_value(node, value, \
		arch_register_req_t const *const req = arch_get_irn_register_req(value); \
		if (req->cls != ccls) \
			continue; \
		code \
	)

/**
 * Iterate over all values defined by an instruction.
 * Only looks at values in a certain register class where the requirements
 * are not marked as ignore.
 * Executes @p code for each definition.
 */
#define be_foreach_definition(node, ccls, value, req, code) \
	be_foreach_definition_(node, ccls, value, req, \
		if (req->ignore) \
			continue; \
		code \
	)

#define be_foreach_use(node, ccls, in_req, value, value_req, code)           \
	do {                                                                     \
	for (int i_ = 0, n_ = get_irn_arity(node); i_ < n_; ++i_) {              \
		const arch_register_req_t *in_req = arch_get_irn_register_req_in(node, i_); \
		if (in_req->cls != ccls)                                             \
			continue;                                                        \
		ir_node                   *value     = get_irn_n(node, i_);              \
		const arch_register_req_t *value_req = arch_get_irn_register_req(value); \
		if (value_req->ignore)                                               \
			continue;                                                        \
		code                                                                 \
	}                                                                        \
	} while (0)

bool arch_reg_is_allocatable(const arch_register_req_t *req,
                             const arch_register_t *reg);

void arch_copy_irn_out_info(ir_node *dst, unsigned dst_pos, ir_node const *src);

arch_register_req_t *be_create_cls_req(ir_graph *irg, arch_register_class_t const *cls, unsigned char width);

arch_register_req_t const *be_create_reg_req(ir_graph *irg, arch_register_t const *reg, bool ignore);

static inline void arch_set_irn_register_idx(ir_node *const irn, unsigned const idx)
{
	arch_register_class_t const *const cls = arch_get_irn_register_req(irn)->cls;
	arch_register_t       const *const reg = arch_register_for_index(cls, idx);
	arch_set_irn_register(irn, reg);
}

int be_get_input_pos_for_req(ir_node const *irn, arch_register_req_t const *req);

#endif
