/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Entry point to the representation of a whole program.
 * @author   Goetz Lindenmaier, Michael Beck
 * @date     2000
 */
#include "irprog_t.h"

#include "array.h"
#include "ident_t.h"
#include "ircons.h"
#include "irgraph_t.h"
#include "irmemory.h"
#include "irop_t.h"
#include "obst.h"

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

	res->graphs         = NEW_ARR_F(ir_graph *, 0);
	res->types          = NEW_ARR_F(ir_type *, 0);
	res->global_asms    = NEW_ARR_F(ident *, 0);
	res->last_label_nr  = 1;  /* 0 is reserved as non-label */
	res->max_irg_idx    = 0;
	res->max_node_nr    = 0;
#ifndef NDEBUG
	res->reserved_resources = IRP_RESOURCE_NONE;
#endif
	res->globals        = pmap_create();

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
	ir_init_type(irp);
	ir_init_entity(irp);

	irp->name = new_id_from_str(module_name);
	irp->segment_types[IR_SEGMENT_GLOBAL]       = new_type_segment(NEW_IDENT("GlobalType"),          tf_none);
	irp->segment_types[IR_SEGMENT_THREAD_LOCAL] = new_type_segment(NEW_IDENT("ThreadLocal"),         tf_none);
	irp->segment_types[IR_SEGMENT_CONSTRUCTORS] = new_type_segment(NEW_IDENT("Constructors"),        tf_info);
	irp->segment_types[IR_SEGMENT_DESTRUCTORS]  = new_type_segment(NEW_IDENT("Destructors"),         tf_info);
	irp->segment_types[IR_SEGMENT_JCR]          = new_type_segment(NEW_IDENT("Java Class Registry"), tf_info);

	irp->const_code_irg             = new_const_code_irg();
	irp->globals_entity_usage_state = ir_entity_usage_not_computed;
}

void init_irprog_1(void)
{
	irp = new_incomplete_ir_prog();
}

void init_irprog_2(void)
{
	complete_ir_prog(irp, INITAL_PROG_NAME);
}

ir_prog *new_ir_prog(const char *name)
{
	ir_prog *irp = new_incomplete_ir_prog();
	complete_ir_prog(irp, name);
	return irp;
}

void free_ir_prog(void)
{
	if (irp == NULL)
		return;

	/* must iterate backwards here */
	foreach_irp_irg_r(i, irg) {
		free_ir_graph(irg);
	}

	/* free entities first to avoid entity types being destroyed before
	 * the entities using them */
	for (size_t i = get_irp_n_types(); i > 0;)
		free_type_entities(get_irp_type(--i));

	for (size_t i = get_irp_n_types(); i > 0;)
		free_type(get_irp_type(--i));

	free_ir_graph(irp->const_code_irg);

	ir_finish_type(irp);

	DEL_ARR_F(irp->graphs);
	DEL_ARR_F(irp->types);

	DEL_ARR_F(irp->global_asms);

	pmap_destroy(irp->globals);

	irp->name           = NULL;
	irp->const_code_irg = NULL;
	free(irp);
	irp = NULL;
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

ir_entity *ir_get_global(ident *name)
{
	return pmap_get(ir_entity, irp->globals, name);
}

void add_irp_irg(ir_graph *irg)
{
	assert(irg != NULL);
	assert(irp && irp->graphs);
	ARR_APP1(ir_graph *, irp->graphs, irg);
}

void remove_irp_irg(ir_graph *irg)
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

void (irp_reserve_resources)(ir_prog *irp, irp_resources_t resources)
{
	irp_reserve_resources_(irp, resources);
}

void (irp_free_resources)(ir_prog *irp, irp_resources_t resources)
{
	irp_free_resources_(irp, resources);
}

irp_resources_t (irp_resources_reserved)(const ir_prog *irp)
{
	return irp_resources_reserved_(irp);
}
