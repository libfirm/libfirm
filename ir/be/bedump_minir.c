/*
 * Copyright (C) 2010 University of Karlsruhe.  All right reserved.
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
 * @brief       Dump MinIR format for register coalescing
 * @author      Matthias Braun
 * @version     $Id$
 */
#include "config.h"

#include <stdbool.h>

#include "irgwalk.h"
#include "iredges_t.h"
#include "bearch.h"
#include "besched.h"
#include "bedump_minir.h"

typedef enum yaml_state_t {
	STATE_BLOCK_MAPPING,
	STATE_BLOCK_SEQUENCE,
	STATE_LIST,
	STATE_MAPPING,
	STATE_EXPECT_VALUE
} yaml_state_t;

static const arch_env_t *arch_env;
static FILE             *out;
static int               indent;
static bool              had_list_item;
static bool              had_dict_item;
static bool              no_newline;
static unsigned          state_pos;
static yaml_state_t      state_stack[1024];
static yaml_state_t      state;

static void init_yaml(FILE *new_out)
{
	out       = new_out;
	indent    = 0;
	state_pos = 0;
	state     = STATE_BLOCK_MAPPING;
}

static void exit_yaml(void)
{
	assert(indent == 0);
	assert(state_pos == 0);
	fputs("\n", out);
}

static void push_state(yaml_state_t new_state)
{
	assert(state_pos < sizeof(state_stack)/sizeof(yaml_state_t));
	state_stack[state_pos++] = state;
	state = new_state;
}

static void pop_state(void)
{
	assert(state_pos > 0);
	state = state_stack[--state_pos];
}

static void newline(void)
{
	int i;

	if (no_newline) {
		no_newline = false;
		return;
	}

	fputc('\n', out);
	for (i = 0; i < indent; ++i) {
		fputs("  ", out);
	}
}

static void list_item(void);
static void mapping_item(const char *name);

static void start(const char *name)
{
	if (name != NULL) {
		mapping_item(name);
	} else if (state == STATE_LIST || state == STATE_BLOCK_SEQUENCE) {
		list_item();
	}

	assert(state == STATE_EXPECT_VALUE);
	pop_state();
}

static void begin_block_sequence(const char *name)
{
	start(name);
	push_state(STATE_BLOCK_SEQUENCE);
}

static void end_block_sequence(const char *name)
{
	(void) name;
	assert(state == STATE_BLOCK_SEQUENCE);
	pop_state();
}

static void begin_block_mapping(const char *name)
{
	start(name);
	++indent;
	push_state(STATE_BLOCK_MAPPING);
	if (name == NULL) {
		no_newline = true;
	}
}

static void end_block_mapping(const char *name)
{
	(void) name;
	--indent;
	assert(state == STATE_BLOCK_MAPPING);
	pop_state();
}

static void mapping_item(const char *name)
{
	if (state == STATE_BLOCK_MAPPING) {
		newline();
	} else {
		assert(state == STATE_MAPPING);
		if (had_dict_item)
			fputs(", ", out);
		had_dict_item = true;
	}
	fprintf(out, "%s: ", name);
	push_state(STATE_EXPECT_VALUE);
}

static void list_item(void)
{
	if (state == STATE_BLOCK_SEQUENCE) {
		newline();
		fputs("- ", out);
	} else {
		assert(state == STATE_LIST);
		if (had_list_item)
			fputs(", ", out);
		had_list_item = true;
	}
	push_state(STATE_EXPECT_VALUE);
}

static void value(const char *val)
{
	if (state == STATE_BLOCK_SEQUENCE
			|| state == STATE_LIST) {
		list_item();
	}

	assert(state == STATE_EXPECT_VALUE);
	pop_state();
	fputs(val, out);
}

static void key_value(const char *name, const char *val)
{
	mapping_item(name);
	value(val);
}

static void begin_list(const char *name)
{
	start(name);
	fputs("[", out);
	had_list_item = false;
	push_state(STATE_LIST);
}

static void end_list(const char *name)
{
	fputs("]", out);
	(void) name;
	assert(state == STATE_LIST);
	pop_state();
}

static void begin_mapping(const char *name)
{
	start(name);
	fputs("{", out);
	had_dict_item = false;
	push_state(STATE_MAPPING);
}

static void end_mapping(const char *name)
{
	fputs("}", out);
	(void) name;
	assert(state == STATE_MAPPING);
	pop_state();
}

static void print_regclasses(void)
{
	int n_classes = arch_env_get_n_reg_class(arch_env);
	int c;

	begin_block_sequence("regclasses");
	for (c = 0; c < n_classes; ++c) {
		int                          n_regs;
		int                          r;
		const arch_register_class_t *cls = arch_env_get_reg_class(arch_env, c);
		if (arch_register_class_flags(cls) & arch_register_class_flag_manual_ra)
			continue;

		n_regs = arch_register_class_n_regs(cls);

		begin_block_mapping(NULL);

		key_value("name", cls->name);
		begin_list("registers");
		for (r = 0; r < n_regs; ++r) {
			const arch_register_t *reg = arch_register_for_index(cls, r);
			value(reg->name);
		}
		end_list("registers");

		begin_block_mapping("flags");
		for (r = 0; r < n_regs; ++r) {
			const arch_register_t *reg  = arch_register_for_index(cls, r);
			unsigned               type = reg->type;
			if (type & arch_register_type_ignore) {
				begin_list(reg->name);
				value("reserved");
				value("nossa"); /* do we need this? */
				end_list(reg->name);
			}
		}
		end_block_mapping("flags");

		end_block_mapping(NULL);
	}
	end_block_sequence("regclasses");
}

static void print_value_name(ir_node *node)
{
	char name[128];
	const arch_register_req_t *req = arch_get_register_req_out(node);
	snprintf(name, sizeof(name), "V%ld.%s", get_irn_node_nr(node),
	         req->cls->name);

	value(name);
}

static void print_node(ir_node *node)
{
	ir_op *op = get_irn_op(node);
	int    arity;
	int    i;

	begin_mapping(NULL);

	mapping_item("op");
	value(get_op_name(op));

	mapping_item("defs");
	begin_list(NULL);
	if (get_irn_mode(node) == mode_T) {
		const ir_edge_t *edge;
		foreach_out_edge(node, edge) {
			ir_node *proj = get_edge_src_irn(edge);
			const arch_register_req_t *req = arch_get_register_req_out(proj);

			if (req->cls == NULL || (req->type & arch_register_req_type_ignore))
				continue;

			list_item();
			print_value_name(proj);
		}
	} else {
		const arch_register_req_t *req = arch_get_register_req_out(node);
		if (req->cls != NULL && !(req->type & arch_register_req_type_ignore)) {
			list_item();
			print_value_name(node);
		}
	}
	end_list(NULL);

	mapping_item("uses");
	begin_list(NULL);
	arity = get_irn_arity(node);
	for (i = 0; i < arity; ++i) {
		const arch_register_req_t *req = arch_get_register_req(node, i);
		ir_node                   *op  = get_irn_n(node, i);

		if (req->cls == NULL || (req->type & arch_register_req_type_ignore))
			continue;

		list_item();
		print_value_name(op);
	}
	end_list(NULL);

	end_mapping(NULL);
}

static void dump_block(ir_node *block, void *data)
{
	ir_graph *irg = get_irn_irg(block);
	ir_node  *node;
	(void) data;

	begin_block_mapping(NULL);

	if (block == get_irg_start_block(irg)) {
		key_value("label", "start");
	} else if (block == get_irg_end_block(irg)) {
		key_value("label", "end");
	} else {
		char name[128];
		snprintf(name, sizeof(name), "BB%ld", get_irn_node_nr(block));
		key_value("label", name);
	}

	begin_block_sequence("ops");

	sched_foreach(block, node) {
		print_node(node);
	}

	end_block_sequence("ops");

	end_block_mapping(NULL);
}

static void print_function(ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);

	begin_block_mapping(NULL);

	key_value("label", get_entity_name(entity));
	begin_list("entries"); value("start"); end_list("entries");
	begin_list("exit");    value("end");   end_list("exit");

	begin_block_sequence("bbs");
	irg_block_walk_graph(irg, dump_block, NULL, NULL);
	end_block_sequence("bbs");

	end_block_mapping(NULL);
}

void be_export_minir(FILE *out, ir_graph *irg)
{
	arch_env = be_get_irg_arch_env(irg);
	init_yaml(out);

	print_regclasses();

	begin_block_sequence("functions");
	print_function(irg);
	end_block_sequence("functions");

	exit_yaml();
}
