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
 * @brief       Register param constraints and some other register handling tools.
 * @author      Christian Wuerdig
 * @version     $Id$
 */
#ifndef FIRM_BE_IA32_IA32_MAP_REGS_H
#define FIRM_BE_IA32_IA32_MAP_REGS_H

#include "irnode.h"
#include "irmode.h"
#include "set.h"

#include "../bearch_t.h"

/**
 * Convenience macro to check if register <code>out</code>
 * and register <code>in</code> are equal.
 */
#define REGS_ARE_EQUAL(out, in) \
	((arch_register_get_class(out) == arch_register_get_class(in)) && \
	(arch_register_get_index(out) == arch_register_get_index(in)))

/**
 * Set compare function
 */
int  ia32_cmp_irn_reg_assoc(const void *a, const void *b, size_t len);

/**
 * Assigns a register to a firm node.
 */
void ia32_set_firm_reg(ir_node *irn, const arch_register_t *reg, set *reg_set);

/**
 * Gets the register assigned to a firm node.
 */
const arch_register_t *ia32_get_firm_reg(const ir_node *irn, set *reg_set);

/**
 * Enters for each general purpose register the corresponding 16bit
 * name into a pmap.
 */
void ia32_build_16bit_reg_map(pmap *reg_map);

/**
 * Enters for each general purpose register the corresponding 8bit
 * name into a pmap.
 */
void ia32_build_8bit_reg_map(pmap *reg_map);

/**
 * Returns the corresponding mapped name for a register.
 */
const char *ia32_get_mapped_reg_name(pmap *reg_map, const arch_register_t *reg);

/**
 * Check all parameters and determine the maximum number of parameters
 * to pass in gp regs resp. in fp regs.
 *
 * @param n       The number of parameters
 * @param modes   The list of the parameter modes
 * @param n_int   Holds the number of int parameters to be passed in regs after the call
 * @param n_float Holds the number of float parameters to be passed in regs after the call
 * @return        The number of the last parameter to be passed in register
 */
int ia32_get_n_regparam_class(int n, ir_mode **modes, int *n_int, int *n_float);

/**
 * Returns the register for parameter nr.
 *
 * @param n     The number of parameters
 * @param modes The list of the parameter modes
 * @param nr    The number of the parameter to return the requirements for
 * @param cc    The calling convention
 * @return      The register
 */
const arch_register_t *ia32_get_RegParam_reg(int n, ir_mode **modes, long nr, unsigned cc);

#endif /* FIRM_BE_IA32_IA32_MAP_REGS_H */
