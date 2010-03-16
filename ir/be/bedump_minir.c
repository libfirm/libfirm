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
#include "bearch.h"

static const arch_env_t *arch_env;
static FILE             *out;
static int               indent;
static int               item_indent_add;
static bool              had_list_item;
static bool              no_newline;

static void newline(void)
{
	int i;

	if (no_newline) {
		no_newline = false;
		return;
	}

	fputc('\n', out);
	for (i = 0; i < indent + item_indent_add; ++i) {
		fputs("  ", out);
	}
}

static void begin_key(const char *name)
{
	newline();
	fprintf(out, "%s: ", name);
}

static void end_key(const char *name)
{
	/* TODO verify */
	(void) name;
}

static void begin_block_sequence(const char *name)
{
	begin_key(name);
	indent++;
}

static void end_block_sequence(const char *name)
{
	end_key(name);
	indent--;
	item_indent_add = 0;
}

static void block_sequence_item(void)
{
	item_indent_add = 0;
	newline();
	fputs("- ", out);
	item_indent_add = 1;
	no_newline = true;
}

static void begin_mapping(const char *name)
{
	begin_key(name);
	indent++;
}

static void end_maping(const char *name)
{
	end_key(name);
	indent--;
}

static void value(const char *val)
{
	fputs(val, out);
}

static void key_value(const char *name, const char *val)
{
	begin_key(name);
	value(val);
	end_key(name);
}

static void begin_list(const char *name)
{
	begin_key(name);
	fputs("[", out);
	had_list_item = false;
}

static void end_list(const char *name)
{
	/* TODO: verify that we started a list */
	fputs("]", out);
	end_key(name);
}

static void list_item(void)
{
	if (had_list_item)
		fputs(", ", out);
	had_list_item = true;
}

static void list_item_value(const char *val)
{
	list_item();
	value(val);
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

		block_sequence_item();

		key_value("name", cls->name);
		begin_list("registers");
		for (r = 0; r < n_regs; ++r) {
			const arch_register_t *reg = arch_register_for_index(cls, r);
			list_item_value(reg->name);
		}
		end_list("registers");

		begin_mapping("flags");
			for (r = 0; r < n_regs; ++r) {
				const arch_register_t *reg  = arch_register_for_index(cls, r);
				unsigned               type = reg->type;
				if (type & arch_register_type_ignore) {
					begin_list(reg->name);
					list_item_value("reserved");
					list_item_value("nossa"); /* do we need this? */
					end_list(reg->name);
				}
			}
		end_maping("flags");
	}
	end_block_sequence("regclasses");
}

static void dump_block(ir_node *block, void *data)
{
	ir_graph *irg = get_irn_irg(block);
	(void) data;

	block_sequence_item();
	if (block == get_irg_start_block(irg)) {
		key_value("label", "start");
	} else if (block == get_irg_end_block(irg)) {
		key_value("label", "end");
	} else {
		char name[128];
		snprintf(name, sizeof(name), "bb%ld", get_irn_node_nr(block));
		key_value("label", name);
	}

	begin_block_sequence("ops");
	end_block_sequence("ops");
}

static void print_function(ir_graph *irg)
{
	ir_entity *entity = get_irg_entity(irg);

	block_sequence_item();

	key_value("label", get_entity_name(entity));
	begin_list("entries"); list_item_value("start"); end_list("entries");
	begin_list("exit"); list_item_value("end"); end_list("entries");

	begin_block_sequence("bbs");
	irg_block_walk_graph(irg, dump_block, NULL, NULL);
	end_block_sequence("bbs");
}

void be_export_minir(const arch_env_t *new_arch_env, FILE *new_out, ir_graph *irg)
{
	arch_env = new_arch_env;
	out      = new_out;

	no_newline = true;

	print_regclasses();

	begin_block_sequence("functions");
	print_function(irg);
	end_block_sequence("functions");

	assert(indent == 0);
	fputs("\n", out);

}
