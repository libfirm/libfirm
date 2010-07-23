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
 * @brief   This file implements the creation of the achitecture specific firm
 *          opcodes and the coresponding node constructors for the sparc
 *          assembler irg.
 * @version $Id$
 */
#include "config.h"

#include <stdlib.h>

#include "irprog_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "ircons_t.h"
#include "iropt_t.h"
#include "irop.h"
#include "irvrfy_t.h"
#include "irprintf.h"
#include "xmalloc.h"

#include "../bearch.h"

#include "sparc_nodes_attr.h"
#include "sparc_new_nodes.h"
#include "gen_sparc_regalloc_if.h"

static bool has_symconst_attr(const ir_node *node)
{
	return is_sparc_SymConst(node) || is_sparc_FrameAddr(node);
}

static bool has_load_store_attr(const ir_node *node)
{
	return is_sparc_Ld(node) || is_sparc_St(node);
}

static bool has_jmp_cond_attr(const ir_node *node)
{
	return is_sparc_BXX(node);
}

static bool has_jmp_switch_attr(const ir_node *node)
{
	return is_sparc_SwitchJmp(node);
}

static bool has_save_attr(const ir_node *node)
{
	return is_sparc_Save(node);
}

/**
 * Dumper interface for dumping sparc nodes in vcg.
 * @param F        the output file
 * @param n        the node to dump
 * @param reason   indicates which kind of information should be dumped
 */
static void sparc_dump_node(FILE *F, ir_node *n, dump_reason_t reason)
{
	switch (reason) {
	case dump_node_opcode_txt:
		fprintf(F, "%s", get_irn_opname(n));
		break;

	case dump_node_mode_txt:
		break;

	case dump_node_info_txt:
		arch_dump_reqs_and_registers(F, n);
		if (has_save_attr(n)) {
			const sparc_save_attr_t *attr = get_sparc_save_attr_const(n);
			fprintf(F, "initial stacksize: %d\n", attr->initial_stacksize);
		}
		if (has_symconst_attr(n)) {
			const sparc_symconst_attr_t *attr = get_sparc_symconst_attr_const(n);
			ir_fprintf(F, "entity: %+F\n", attr->entity);
			fprintf(F, "fp_offset: %d\n", attr->fp_offset);
		}
		if (has_load_store_attr(n)) {
			const sparc_load_store_attr_t *attr = get_sparc_load_store_attr_const(n);
			ir_fprintf(F, "load store mode: %+F\n", attr->load_store_mode);
			ir_fprintf(F, "entity: (sign %d) %+F\n", attr->entity_sign,
			           attr->entity);
			fprintf(F, "offset: %ld\n", attr->offset);
			fprintf(F, "is frame entity: %s\n",
			        attr->is_frame_entity ? "true" : "false");
		}
		if (has_jmp_cond_attr(n)) {
			const sparc_jmp_cond_attr_t *attr
				= get_sparc_jmp_cond_attr_const(n);
			fprintf(F, "pnc: %d (%s)\n", attr->proj_num,
			        get_pnc_string(attr->proj_num));
			fprintf(F, "unsigned: %s\n", attr->is_unsigned ? "true" : "false");
		}
		if (has_jmp_switch_attr(n)) {
			const sparc_jmp_switch_attr_t *attr
				= get_sparc_jmp_switch_attr_const(n);
			fprintf(F, "n projs: %d\n", attr->n_projs);
			fprintf(F, "default proj: %ld\n", attr->default_proj_num);
		}
		break;

	case dump_node_nodeattr_txt:
		break;
	}
}

static void sparc_set_attr_imm(ir_node *res, int immediate_value)
{
	sparc_attr_t *attr = get_irn_generic_attr(res);
	attr->immediate_value = immediate_value;
}

static void init_sparc_jmp_cond_attr(ir_node *node, int proj_num,
                                     bool is_unsigned)
{
	sparc_jmp_cond_attr_t *attr = get_sparc_jmp_cond_attr(node);
	attr->proj_num    = proj_num;
	attr->is_unsigned = is_unsigned;
}

void set_sparc_jmp_switch_n_projs(ir_node *node, int n_projs)
{
	sparc_jmp_switch_attr_t *attr = get_sparc_jmp_switch_attr(node);
	attr->n_projs = n_projs;
}

void set_sparc_jmp_switch_default_proj_num(ir_node *node, long def_proj_num)
{
	sparc_jmp_switch_attr_t *attr = get_sparc_jmp_switch_attr(node);
	attr->default_proj_num = def_proj_num;
}



int get_sparc_jmp_switch_n_projs(const ir_node *node)
{
	const sparc_jmp_switch_attr_t *attr = get_sparc_jmp_switch_attr_const(node);
	return attr->n_projs;
}

long get_sparc_jmp_switch_default_proj_num(const ir_node *node)
{
	const sparc_jmp_switch_attr_t *attr = get_sparc_jmp_switch_attr_const(node);
	return attr->default_proj_num;
}

sparc_attr_t *get_sparc_attr(ir_node *node)
{
	assert(is_sparc_irn(node));
	return (sparc_attr_t*) get_irn_generic_attr(node);
}

const sparc_attr_t *get_sparc_attr_const(const ir_node *node)
{
	assert(is_sparc_irn(node));
	return (const sparc_attr_t*) get_irn_generic_attr_const(node);
}

sparc_load_store_attr_t *get_sparc_load_store_attr(ir_node *node)
{
	assert(has_load_store_attr(node));
	return (sparc_load_store_attr_t*) get_irn_generic_attr_const(node);
}

const sparc_load_store_attr_t *get_sparc_load_store_attr_const(const ir_node *node)
{
	assert(has_load_store_attr(node));
	return (const sparc_load_store_attr_t*) get_irn_generic_attr_const(node);
}

sparc_symconst_attr_t *get_sparc_symconst_attr(ir_node *node)
{
	assert(has_symconst_attr(node));
	return (sparc_symconst_attr_t*) get_irn_generic_attr_const(node);
}

const sparc_symconst_attr_t *get_sparc_symconst_attr_const(const ir_node *node)
{
	assert(has_symconst_attr(node));
	return (const sparc_symconst_attr_t*) get_irn_generic_attr_const(node);
}

sparc_jmp_cond_attr_t *get_sparc_jmp_cond_attr(ir_node *node)
{
	assert(has_jmp_cond_attr(node));
	return (sparc_jmp_cond_attr_t*) get_irn_generic_attr_const(node);
}

const sparc_jmp_cond_attr_t *get_sparc_jmp_cond_attr_const(const ir_node *node)
{
	assert(has_jmp_cond_attr(node));
	return (const sparc_jmp_cond_attr_t*) get_irn_generic_attr_const(node);
}

sparc_jmp_switch_attr_t *get_sparc_jmp_switch_attr(ir_node *node)
{
	assert(has_jmp_switch_attr(node));
	return (sparc_jmp_switch_attr_t*) get_irn_generic_attr_const(node);
}

const sparc_jmp_switch_attr_t *get_sparc_jmp_switch_attr_const(const ir_node *node)
{
	assert(has_jmp_switch_attr(node));
	return (const sparc_jmp_switch_attr_t*) get_irn_generic_attr_const(node);
}

sparc_save_attr_t *get_sparc_save_attr(ir_node *node)
{
	assert(has_save_attr(node));
	return (sparc_save_attr_t*) get_irn_generic_attr_const(node);
}

const sparc_save_attr_t *get_sparc_save_attr_const(const ir_node *node)
{
	assert(has_save_attr(node));
	return (const sparc_save_attr_t*) get_irn_generic_attr_const(node);
}

/**
 * Returns the argument register requirements of a sparc node.
 */
const arch_register_req_t **get_sparc_in_req_all(const ir_node *node)
{
	const sparc_attr_t *attr = get_sparc_attr_const(node);
	return attr->in_req;
}

void set_sparc_in_req_all(ir_node *node, const arch_register_req_t **reqs)
{
	sparc_attr_t *attr = get_sparc_attr(node);
	attr->in_req = reqs;
}

/**
 * Returns the argument register requirement at position pos of an sparc node.
 */
const arch_register_req_t *get_sparc_in_req(const ir_node *node, int pos)
{
	const sparc_attr_t *attr = get_sparc_attr_const(node);
	return attr->in_req[pos];
}

/**
 * Sets the IN register requirements at position pos.
 */
void set_sparc_req_in(ir_node *node, const arch_register_req_t *req, int pos)
{
	sparc_attr_t *attr  = get_sparc_attr(node);
	attr->in_req[pos] = req;
}

/**
 * Initializes the nodes attributes.
 */
static void init_sparc_attributes(ir_node *node, arch_irn_flags_t flags,
                                  const arch_register_req_t **in_reqs,
                                  const be_execution_unit_t ***execution_units,
                                  int n_res)
{
	ir_graph        *irg  = get_irn_irg(node);
	struct obstack  *obst = get_irg_obstack(irg);
	sparc_attr_t *attr = get_sparc_attr(node);
	backend_info_t  *info;
	(void) execution_units;

	arch_irn_set_flags(node, flags);
	attr->in_req  = in_reqs;
	attr->is_load_store = false;

	info            = be_get_info(node);
	info->out_infos = NEW_ARR_D(reg_out_info_t, obst, n_res);
	memset(info->out_infos, 0, n_res * sizeof(info->out_infos[0]));
}

/* CUSTOM ATTRIBUTE INIT FUNCTIONS */
static void init_sparc_load_store_attributes(ir_node *res, ir_mode *ls_mode,
											ir_entity *entity,
											int entity_sign, long offset,
											bool is_frame_entity)
{
	sparc_load_store_attr_t *attr = get_sparc_load_store_attr(res);
	attr->load_store_mode    = ls_mode;
	attr->entity             = entity;
	attr->entity_sign        = entity_sign;
	attr->is_frame_entity    = is_frame_entity;
	attr->offset             = offset;
	attr->base.is_load_store = true;
}

static void init_sparc_symconst_attributes(ir_node *res, ir_entity *entity)
{
	sparc_symconst_attr_t *attr = get_sparc_symconst_attr(res);
	attr->entity    = entity;
	attr->fp_offset = 0;
}

static void init_sparc_save_attr(ir_node *res, int initial_stacksize)
{
	sparc_save_attr_t *attr = get_sparc_save_attr(res);
	attr->initial_stacksize = initial_stacksize;
}

/**
 * copies sparc attributes of  node
 */
static void sparc_copy_attr(ir_graph *irg, const ir_node *old_node,
                            ir_node *new_node)
{
	struct obstack     *obst    = get_irg_obstack(irg);
	const sparc_attr_t *attr_old = get_sparc_attr_const(old_node);
	sparc_attr_t       *attr_new = get_sparc_attr(new_node);
	backend_info_t     *old_info = be_get_info(old_node);
	backend_info_t     *new_info = be_get_info(new_node);

	/* copy the attributes */
	memcpy(attr_new, attr_old, get_op_attr_size(get_irn_op(old_node)));
	/* copy out flags */
	new_info->out_infos =
		DUP_ARR_D(reg_out_info_t, obst, old_info->out_infos);
}

/**
 * compare some node's attributes
 */
static int cmp_attr_sparc(ir_node *a, ir_node *b)
{
	const sparc_attr_t *attr_a = get_sparc_attr_const(a);
	const sparc_attr_t *attr_b = get_sparc_attr_const(b);

	return attr_a->immediate_value != attr_b->immediate_value
			|| attr_a->is_load_store != attr_b->is_load_store;
}


/* CUSTOM ATTRIBUTE CMP FUNCTIONS */
static int cmp_attr_sparc_load_store(ir_node *a, ir_node *b)
{
	const sparc_load_store_attr_t *attr_a = get_sparc_load_store_attr_const(a);
	const sparc_load_store_attr_t *attr_b = get_sparc_load_store_attr_const(b);

	if (cmp_attr_sparc(a, b))
			return 1;

	return attr_a->entity != attr_b->entity
			|| attr_a->entity_sign != attr_b->entity_sign
			|| attr_a->is_frame_entity != attr_b->is_frame_entity
			|| attr_a->load_store_mode != attr_b->load_store_mode
			|| attr_a->offset != attr_b->offset;
}

static int cmp_attr_sparc_symconst(ir_node *a, ir_node *b)
{
	const sparc_symconst_attr_t *attr_a = get_sparc_symconst_attr_const(a);
	const sparc_symconst_attr_t *attr_b = get_sparc_symconst_attr_const(b);

	if (cmp_attr_sparc(a, b))
			return 1;

	return attr_a->entity != attr_b->entity
			|| attr_a->fp_offset != attr_b->fp_offset;
}

static int cmp_attr_sparc_jmp_cond(ir_node *a, ir_node *b)
{
	const sparc_jmp_cond_attr_t *attr_a = get_sparc_jmp_cond_attr_const(a);
	const sparc_jmp_cond_attr_t *attr_b = get_sparc_jmp_cond_attr_const(b);

	if (cmp_attr_sparc(a, b))
			return 1;

	return attr_a->proj_num != attr_b->proj_num
		|| attr_a->is_unsigned != attr_b->is_unsigned;
}

static int cmp_attr_sparc_jmp_switch(ir_node *a, ir_node *b)
{
	const sparc_jmp_switch_attr_t *attr_a = get_sparc_jmp_switch_attr_const(a);
	const sparc_jmp_switch_attr_t *attr_b = get_sparc_jmp_switch_attr_const(b);

	if (cmp_attr_sparc(a, b))
			return 1;

	return attr_a->default_proj_num != attr_b->default_proj_num
			|| attr_a->n_projs != attr_b->n_projs;
}

static int cmp_attr_sparc_save(ir_node *a, ir_node *b)
{
	const sparc_save_attr_t *attr_a = get_sparc_save_attr_const(a);
	const sparc_save_attr_t *attr_b = get_sparc_save_attr_const(b);

	if (cmp_attr_sparc(a, b))
			return 1;

	return attr_a->initial_stacksize != attr_b->initial_stacksize;
}

/* Include the generated constructor functions */
#include "gen_sparc_new_nodes.c.inl"
