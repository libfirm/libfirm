/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   Stabs support.
 * @author  Michael Beck
 * @date    11.9.2006
 * @version $Id: bestabs.c 17143 2008-01-02 20:56:33Z beck $
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "be_dbgout_t.h"
#include "bemodule.h"
#include "irtools.h"

static dbg_handle *handle = NULL;

void be_dbg_close(void) {
	if (handle->ops->close)
		handle->ops->close(handle);
}

void be_dbg_so(const char *filename) {
	if (handle->ops->so)
		handle->ops->so(handle, filename);
}

void be_dbg_main_program(void) {
	if (handle->ops->main_program)
		handle->ops->main_program(handle);
}

void be_dbg_method_begin(ir_entity *ent, const be_stack_layout_t *layout) {
	if (handle->ops->method_begin)
		handle->ops->method_begin(handle, ent, layout);
}

void be_dbg_method_end(void) {
	if (handle->ops->method_end)
		handle->ops->method_end(handle);
}

void be_dbg_types(void) {
	if (handle->ops->types)
		handle->ops->types(handle);
}

void be_dbg_variable(struct obstack *obst, ir_entity *ent) {
	if (handle->ops->variable)
		handle->ops->variable(handle, obst, ent);
}

void be_dbg_set_dbg_info(dbg_info *dbgi) {
	if (handle->ops->set_dbg_info)
		handle->ops->set_dbg_info(handle, dbgi);
}

static be_module_list_entry_t       *dbgout_modules         = NULL;
static be_create_dbgout_module_func  selected_dbgout_module = NULL;

void be_dbg_open(void) {
	handle = selected_dbgout_module();
}

void be_register_dbgout_module(const char *name,
                               be_create_dbgout_module_func func);

void be_register_dbgout_module(const char *name,
                               be_create_dbgout_module_func func)
{
	if (selected_dbgout_module == NULL)
		selected_dbgout_module = func;
	be_add_module_to_list(&dbgout_modules, name, func);
}

static dbg_handle *create_null_dbgout_module(void)
{
	static const debug_ops null_ops;
	static dbg_handle null_handle = { &null_ops };
	return &null_handle;
}

void be_init_dbgout(void) {
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");

	be_add_module_list_opt(be_grp, "debuginfo", "debug info format",
	                       &dbgout_modules, (void**) &selected_dbgout_module);
	be_register_dbgout_module("null", create_null_dbgout_module);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_dbgout);
