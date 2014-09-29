/*
 * This file is part of libFirm.
 * Copyright (C) 2014 Karlsruhe Institute of Technology
 */

/**
 * @file
 * @brief  Some machine dependent optimizations.
 * @author Sebastian Hack
 */
#ifndef FIRM_IR_IRARCH_T_H
#define FIRM_IR_IRARCH_T_H

#include "irarch.h"

/**
 * The Multiplication replacement can consist of the following instructions.
 */
typedef enum insn_kind {
	LEA,   /**< the LEA instruction */
	SHIFT, /**< the SHIFT instruction */
	SUB,   /**< the SUB instruction */
	ADD,   /**< the ADD instruction */
	ZERO,  /**< creates a ZERO constant */
	MUL,   /**< the original MUL instruction */
	ROOT   /**< the ROOT value that is multiplied */
} insn_kind;

/**
 * A Callback for evaluating the costs of an instruction.
 *
 * @param kind   the instruction
 * @param mode   the mode of the instruction
 * @param tv     for MUL instruction, the multiplication constant
 *
 * @return the costs of this instruction
 */
typedef int (*evaluate_costs_func)(insn_kind kind, const ir_mode *mode, ir_tarval *tv);

/**
 * A parameter structure that drives the machine dependent Firm
 * optimizations.
 */
struct ir_settings_arch_dep_t {
	/* Mul optimization */
	unsigned also_use_subs : 1;    /**< Use also Subs when resolving Muls to shifts */
	unsigned maximum_shifts;       /**< The maximum number of shifts that shall be inserted for a mul. */
	unsigned highest_shift_amount; /**< The highest shift amount you want to
	                                    tolerate. Muls which would require a higher
	                                    shift constant are left. */
	evaluate_costs_func evaluate;  /**< Evaluate the costs of a generated instruction. */

	/* Div/Mod optimization */
	unsigned allow_mulhs   : 1;    /**< Use the Mulhs operation for division by constant */
	unsigned allow_mulhu   : 1;    /**< Use the Mulhu operation for division by constant */
	unsigned max_bits_for_mulh;    /**< Maximum number of bits the Mulh operation can take.
	                                    Modes with higher amount of bits will use Mulh */
};

/**
 * A factory function, that provides architecture parameters for
 * machine dependent optimizations.
 */
typedef const ir_settings_arch_dep_t *(*arch_dep_params_factory_t)(void);

/**
 * Replaces Muls with Lea/Shifts/Add/Subs if these
 * have smaller costs than the original multiplication.
 *
 * @param irn       The Firm node to inspect.
 * @return          A replacement expression for irn.
 */
ir_node *arch_dep_replace_mul_with_shifts(ir_node *irn);

/**
 * Replaces Divs with Shifts and Add/Subs and Mulh.
 * This function is driven by the 3 parameters:
 * - allow_mulhu
 * - allow_mulhs
 * - max_bits_for_mulh
 *
 * If irn is a Div with a Const, the constant is inspected if it meets the
 * requirements of the variables stated above. If a Shl/Add/Sub/Mulh
 * sequence can be generated that meets these requirements, this expression
 * is returned. In each other case irn is returned unmodified.
 *
 * @param irn       The Firm node to inspect.
 * @return          A replacement expression for irn.
 */
ir_node *arch_dep_replace_div_by_const(ir_node *irn);

/**
 * Replaces Mods with Shifts and Add/Subs and Mulh.
 * This function is driven by the 3 parameters:
 * - allow_mulhu
 * - allow_mulhs
 * - max_bits_for_mulh
 *
 * If irn is a Mod with a Const, the constant is inspected if it meets the
 * requirements of the variables stated above. If a Shl/Add/Sub/Mulh
 * sequence can be generated that meets these requirements, this expression
 * is returned. In each other case irn is returned unmodified.
 *
 * @param irn       The Firm node to inspect.
 * @return          A replacement expression for irn.
 */
ir_node *arch_dep_replace_mod_by_const(ir_node *irn);

#endif
