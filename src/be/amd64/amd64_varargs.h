/*
 * This file is part of libFirm.
 * Copyright (C) 2015 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Implements vararg handling for AMD64
 * @author      Andreas Fried
 */
#ifndef FIRM_BE_AMD64_AMD64_VARARGS_H
#define FIRM_BE_AMD64_AMD64_VARARGS_H

#include "firm_types.h"

#include "bearch.h"
#include "../ia32/x86_cconv.h"

/**
 * Sets the entity that points to the first variadic argument passed
 * on the stack.
 *
 * @param param A parameter entity pointing to the first variadic
 *              argument on the stack
 */
void amd64_set_va_stack_args_param(ir_entity *param);

/**
 * This function must be called before any other functions in this
 * module, as they all require the va_list type to be known.
 *
 * @return the type used vor va_list objects.
 */
ir_type *amd64_build_va_list_type(void);

/**
 * Adds start infos for the registers which might be used to pass
 * variadic arguments to @p outs.
 *
 * @param outs   Register mask for the Start node
 * @param cconv  The calling convention in force for the current function
 */
void amd64_collect_variadic_params(be_start_out *outs, x86_cconv_t *cconv);

/**
 * Adds a frame member in which to store the registers containing the
 * variadic arguments.
 *
 * @param irg The current irg
 * @param cconv The calling convention in force for the current function
 */
void amd64_insert_reg_save_area(ir_graph *irg, x86_cconv_t *cconv);

/**
 * Lowers an ir_bk_va_arg Builtin node.
 *
 * @param node The va_arg_node to lower
 */
void amd64_lower_va_arg(ir_node *node);

/**
 * Initializes the va_list structure at @p ap.
 *
 * @param dbgi The current dbg_info
 * @param block The block where to put the generated nodes
 * @param cconv The calling convention in force for the current function
 * @param mem The memory value afterw which to put the generated nodes
 * @param ap The node representing the va_list object
 * @param frame The current frame base
 *
 * @return The memory value of the last generated node.
 */
ir_node *amd64_initialize_va_list(dbg_info *dbgi, ir_node *block, x86_cconv_t *cconv,
                                  ir_node *mem, ir_node *ap, ir_node *frame);

/**
 * Insert prologue code to save the registers containing the variadic arguments.
 *
 * @param irg   The current irg
 * @param cconv The calling convention in force for the current function
 * @param frame The current frame base
 */
void amd64_save_vararg_registers(ir_graph *irg, x86_cconv_t const *cconv, ir_node *fp);


#endif
