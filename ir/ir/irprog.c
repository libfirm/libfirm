/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @brief    Entry point to the representation of a whole program.
 * @author   Goetz Lindenmaier, Michael Beck
 * @date     2000
 */
#include "config.h"

#include <string.h>

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irpass_t.h"
#include "array.h"
#include "error.h"
#include "obst.h"
#include "irop_t.h"
#include "irmemory.h"
#include "ircons.h"

/** The initial name of the irp program. */
#define INITAL_PROG_NAME "no_name_set"

ir_prog *irp;
ir_prog *get_irp(void) { return irp; }
void set_irp(ir_prog *new_irp)
{
	irp = new_irp;
}

/**
 *  Create a new incomplete ir_prog.
 */
static ir_prog *new_incomplete_ir_prog(void)
{
	ir_prog *res = XMALLOCZ(ir_prog);

	res->kind           = k_ir_prog;
	res->graphs         = NEW_ARR_F(ir_graph *, 0);
	res->types          = NEW_ARR_F(ir_type *, 0);
	res->modes          = NEW_ARR_F(ir_mode *, 0);
	res->opcodes        = NEW_ARR_F(ir_op *, 0);
	res->global_asms    = NEW_ARR_F(ident *, 0);
	res->last_label_nr  = 1;  /* 0 is reserved as non-label */
	res->max_irg_idx    = 0;
	res->max_node_nr    = 0;
#ifndef NDEBUG
	res->reserved_resources = IR_RESOURCE_NONE;
#endif

	return res;
}

/**
 * Completes an incomplete irprog.
 *
 * @param irp          the (yet incomplete) irp
 * @param module_name  the (module) name for this irp
 */
static void complete_ir_prog(ir_prog *irp, const char *module_name)
{
#define IDENT(x)  new_id_from_chars(x, sizeof(x) - 1)

	irp->name = new_id_from_str(module_name);
	irp->segment_types[IR_SEGMENT_GLOBAL]
		= new_type_class(IDENT("GlobalType"));
	irp->segment_types[IR_SEGMENT_THREAD_LOCAL]
		= new_type_struct(IDENT("ThreadLocal"));
	irp->segment_types[IR_SEGMENT_CONSTRUCTORS]
		= new_type_class(IDENT("Constructors"));
	irp->segment_types[IR_SEGMENT_DESTRUCTORS]
		= new_type_class(IDENT("Destructors"));

	/* Set these flags for debugging. */
	irp->segment_types[IR_SEGMENT_GLOBAL]->flags       |= tf_segment|tf_global_type;
	irp->segment_types[IR_SEGMENT_THREAD_LOCAL]->flags |= tf_segment|tf_tls_type;
	irp->segment_types[IR_SEGMENT_CONSTRUCTORS]->flags |= tf_segment|tf_constructors;
	irp->segment_types[IR_SEGMENT_DESTRUCTORS]->flags  |= tf_segment|tf_destructors;

	/* The global type is a class, but we cannot derive from it, so set
	   the final property to assist optimizations that checks for it. */
	set_class_final(irp->segment_types[IR_SEGMENT_GLOBAL], 1);

	irp->const_code_irg             = new_const_code_irg();
	irp->phase_state                = phase_building;
	irp->class_cast_state           = ir_class_casts_transitive;
	irp->globals_entity_usage_state = ir_entity_usage_not_computed;
#undef IDENT
}

void init_irprog_1(void)
{
	irp = new_incomplete_ir_prog();
}

void init_irprog_2(void)
{
	complete_ir_prog(irp, INITAL_PROG_NAME);
	ir_init_type(irp);
	ir_init_entity(irp);
}

ir_prog *new_ir_prog(const char *name)
{
	ir_prog *irp = new_incomplete_ir_prog();
	complete_ir_prog(irp, name);
	return irp;
}

void free_ir_prog(void)
{
	size_t i;
	/* must iterate backwards here */
	for (i = get_irp_n_irgs(); i > 0;)
		free_ir_graph(get_irp_irg(--i));

	free_type_entities(get_glob_type());
	/* must iterate backwards here */
	for (i = get_irp_n_types(); i > 0;)
		free_type_entities(get_irp_type(--i));

	for (i = get_irp_n_types(); i > 0;)
		free_type(get_irp_type(--i));

	free_ir_graph(irp->const_code_irg);
	DEL_ARR_F(irp->graphs);
	DEL_ARR_F(irp->types);
	DEL_ARR_F(irp->modes);

	finish_op();
	DEL_ARR_F(irp->opcodes);
	DEL_ARR_F(irp->global_asms);

	irp->name           = NULL;
	irp->const_code_irg = NULL;
	irp->kind           = k_BAD;
}

ir_graph *get_irp_main_irg(void)
{
	assert(irp);
	return irp->main_irg;
}

void set_irp_main_irg(ir_graph *main_irg)
{
	assert(irp);
	irp->main_irg = main_irg;
}

ir_type *(get_segment_type)(ir_segment_t segment)
{
	return get_segment_type_(segment);
}

void set_segment_type(ir_segment_t segment, ir_type *new_type)
{
	assert(segment <= IR_SEGMENT_LAST);
	irp->segment_types[segment] = new_type;
}

ir_type *(get_glob_type)(void)
{
	return get_glob_type_();
}

ir_type *(get_tls_type)(void)
{
	return get_tls_type_();
}

void add_irp_irg(ir_graph *irg)
{
	assert(irg != NULL);
	assert(irp && irp->graphs);
	ARR_APP1(ir_graph *, irp->graphs, irg);
}

void remove_irp_irg_from_list(ir_graph *irg)
{
	size_t i, l;

	assert(irg);
	l = ARR_LEN(irp->graphs);
	for (i = 0; i < l; ++i) {
		if (irp->graphs[i] == irg) {
			for (; i < l - 1; ++i) {
				irp->graphs[i] = irp->graphs[i+1];
			}
			ARR_SETLEN(ir_graph*, irp->graphs, l - 1);
			break;
		}
	}
}

void remove_irp_irg(ir_graph *irg)
{
	free_ir_graph(irg);
	remove_irp_irg_from_list(irg);
}

size_t (get_irp_n_irgs)(void)
{
	return get_irp_n_irgs_();
}

ir_graph *(get_irp_irg)(size_t pos)
{
	return get_irp_irg_(pos);
}

size_t get_irp_last_idx(void)
{
	return irp->max_irg_idx;
}

void set_irp_irg(size_t pos, ir_graph *irg)
{
	assert(irp && irg);
	assert(pos < ARR_LEN(irp->graphs));
	irp->graphs[pos] = irg;
}

void add_irp_type(ir_type *typ)
{
	assert(typ != NULL);
	assert(irp);
	ARR_APP1(ir_type *, irp->types, typ);
}

void remove_irp_type(ir_type *typ)
{
	size_t i, l;
	assert(typ);

	l = ARR_LEN(irp->types);
	for (i = 0; i < l; ++i) {
		if (irp->types[i] == typ) {
			for (; i < l - 1; ++i) {
				irp->types[i] = irp->types[i+1];
			}
			ARR_SETLEN(ir_type *, irp->types, l - 1);
			break;
		}
	}
}

size_t (get_irp_n_types) (void)
{
	return get_irp_n_types_();
}

ir_type *(get_irp_type) (size_t pos)
{
	return get_irp_type_(pos);
}

void set_irp_type(size_t pos, ir_type *typ)
{
	assert(irp && typ);
	assert(pos < ARR_LEN((irp)->types));
	irp->types[pos] = typ;
}

size_t (get_irp_n_modes)(void)
{
	return get_irp_n_modes_();
}

ir_mode *(get_irp_mode)(size_t pos)
{
	return get_irp_mode_(pos);
}

void add_irp_mode(ir_mode *mode)
{
	assert(mode != NULL);
	assert(irp);
	ARR_APP1(ir_mode *, irp->modes, mode);
}

void add_irp_opcode(ir_op *opcode)
{
	size_t len;
	size_t code;
	assert(opcode != NULL);
	assert(irp);
	len  = ARR_LEN(irp->opcodes);
	code = opcode->code;
	if (code >= len) {
		ARR_RESIZE(ir_op*, irp->opcodes, code+1);
		memset(&irp->opcodes[len], 0, (code-len+1) * sizeof(irp->opcodes[0]));
	}

	assert(irp->opcodes[code] == NULL && "opcode registered twice");
	irp->opcodes[code] = opcode;
}

void remove_irp_opcode(ir_op *opcode)
{
	assert(opcode->code < ARR_LEN(irp->opcodes));
	irp->opcodes[opcode->code] = NULL;
}

size_t (get_irp_n_opcodes)(void)
{
	return get_irp_n_opcodes_();
}

ir_op *(get_irp_opcode)(size_t pos)
{
	return get_irp_opcode_(pos);
}

void clear_irp_opcodes_generic_func(void)
{
	size_t i, n;

	for (i = 0, n = get_irp_n_opcodes(); i < n; ++i) {
		ir_op *op = get_irp_opcode(i);
		op->ops.generic = (op_func)NULL;
	}
}

void set_irp_prog_name(ident *name)
{
	irp->name = name;
}
int irp_prog_name_is_set(void)
{
	return irp->name != new_id_from_str(INITAL_PROG_NAME);
}
ident *get_irp_ident(void)
{
	return irp->name;
}
const char  *get_irp_name(void)
{
	return get_id_str(irp->name);
}


ir_graph *(get_const_code_irg)(void)
{
	return get_const_code_irg_();
}

irg_phase_state get_irp_phase_state(void)
{
	return irp->phase_state;
}

void set_irp_phase_state(irg_phase_state s)
{
	irp->phase_state = s;
}

typedef struct pass_t {
	ir_prog_pass_t  pass;
	irg_phase_state state;
} pass_t;

/**
 * Wrapper for setting the state of a whole ir_prog.
 */
static int set_irp_phase_state_wrapper(ir_prog *irp, void *context)
{
	pass_t         *pass  = (pass_t *)context;
	irg_phase_state state = pass->state;
	size_t          i, n;

	(void)irp;

	/* set the phase of all graphs */
	for (i = 0, n = get_irp_n_irgs(); i < n; ++i)
		set_irg_phase_state(get_irp_irg(i), state);

	/* set the irp phase */
	set_irp_phase_state(state);

	return 0;
}

ir_prog_pass_t *set_irp_phase_state_pass(const char *name, irg_phase_state state)
{
	struct pass_t *pass = XMALLOCZ(struct pass_t);

	def_prog_pass_constructor(
		&pass->pass, name ? name : "set_irp_phase", set_irp_phase_state_wrapper);
	pass->state = state;

	/* no dump/verify */
	pass->pass.verify_irprog = ir_prog_no_verify;
	pass->pass.dump_irprog   = ir_prog_no_dump;

	return &pass->pass;
}

void set_irp_ip_outedges(ir_node ** ip_outedges)
{
	irp->ip_outedges = ip_outedges;
}

ir_node** get_irp_ip_outedges(void)
{
	return irp->ip_outedges;
}

irg_callee_info_state get_irp_callee_info_state(void)
{
	return irp->callee_info_state;
}

void set_irp_callee_info_state(irg_callee_info_state s)
{
	irp->callee_info_state = s;
}

ir_label_t (get_irp_next_label_nr)(void)
{
	return get_irp_next_label_nr_();
}

void add_irp_asm(ident *asm_string)
{
	ARR_APP1(ident *, irp->global_asms, asm_string);
}

size_t get_irp_n_asms(void)
{
	return ARR_LEN(irp->global_asms);
}

ident *get_irp_asm(size_t pos)
{
	assert(pos < get_irp_n_asms());
	return irp->global_asms[pos];
}

int (get_irp_optimization_dumps)(void)
{
	return get_irp_optimization_dumps_();
}

void (enable_irp_optimization_dumps)(void)
{
	enable_irp_optimization_dumps_();
}

#ifndef NDEBUG
void irp_reserve_resources(ir_prog *irp, irp_resources_t resources)
{
	assert((irp->reserved_resources & resources) == 0);
	irp->reserved_resources |= resources;
}

void irp_free_resources(ir_prog *irp, irp_resources_t resources)
{
	assert((irp->reserved_resources & resources) == resources);
	irp->reserved_resources &= ~resources;
}

irp_resources_t irp_resources_reserved(const ir_prog *irp)
{
	return irp->reserved_resources;
}
#endif
