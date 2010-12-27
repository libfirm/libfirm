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
 * @brief    Support for libcore IR object output.
 * @author   Sebastian Hack
 * @version  $Id$
 */
#include "config.h"

#include "irargs_t.h"

#include <ctype.h>

#include "bitset.h"
#include "lc_printf.h"
#include "firm_common.h"
#include "irnode_t.h"
#include "entity_t.h"
#include "irloop_t.h"
#include "tv_t.h"
#include "dbginfo_t.h"

/**
 * identify a firm object type
 */
static int firm_get_arg_type(const lc_arg_occ_t *occ)
{
	(void) occ;
	/* Firm objects are always pointer */
	return lc_arg_type_ptr;
}

static int firm_get_arg_type_int(const lc_arg_occ_t *occ)
{
	(void) occ;
	return lc_arg_type_int;
}


static int bitset_get_arg_type(const lc_arg_occ_t *occ)
{
	(void) occ;
	return lc_arg_type_ptr;
}

static int bitset_emit(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	int res = 2;
	bitset_t *b = (bitset_t*)arg->v_ptr;
	size_t  p;
	char buf[32];
	const char *prefix = "";

	lc_arg_append(app, occ, "[", 1);
	bitset_foreach(b, p) {
		int n;

		n = snprintf(buf, sizeof(buf), "%s%d", prefix, (int) p);
		lc_arg_append(app, occ, buf, n);
		prefix = ", ";
		res += n;
	}
	lc_arg_append(app, occ, "]", 1);

	return res;
}

/**
 * emit an opaque Firm dbg_info object
 */
static int firm_emit_dbg(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	char buf[1024];
	ir_node *irn = (ir_node*)arg->v_ptr;
	dbg_info *dbg = get_irn_dbg_info(irn);

	ir_dbg_info_snprint(buf, sizeof(buf), dbg);
	return lc_arg_append(app, occ, buf, strlen(buf));
}

/**
 * Beware: do not set the entity ld_name
 */
static const char *get_entity_ld_name_ex(ir_entity *ent)
{
	if (ent->ld_name)
		return get_entity_ld_name(ent);
	return get_entity_name(ent);
}

/**
 * emit a Firm object
 */
static int firm_emit(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
#define A(s)    occ->flag_hash ? s " ": ""

	void *X = (void*)arg->v_ptr;
	firm_kind *obj = (firm_kind*)X;
	int i, n;
	ir_node *block;
	char add[64];
	char buf[256];
	char tv_buf[256];
	ir_entity *ent;

	buf[0] = '\0';
	add[0] = '\0';

	if (X == NULL) {
		return lc_arg_append(app, occ, "(null)", 6);
	}

	switch (*obj) {
	case k_BAD:
		snprintf(buf, sizeof(buf), "BAD");
		snprintf(add, sizeof(add), "[%p]", X);
		break;
	case k_entity: {
		ir_entity *entity = (ir_entity*)X;
		snprintf(buf, sizeof(buf), "%s%s", A("ent"),
			isupper(occ->conversion) ? get_entity_ld_name_ex(entity): get_entity_name(entity));
		snprintf(add, sizeof(add), "[%ld]", get_entity_nr(entity));
		break;
	}
	case k_type: {
		ir_type *type = (ir_type*)X;
		char type_name[256];
		ir_print_type(type_name, sizeof(type_name), type);
		snprintf(buf, sizeof(buf), "%s%s:%s", A("type"),
		         get_type_tpop_name(type), type_name);
		snprintf(add, sizeof(add), "[%ld]", get_type_nr(type));
		break;
	}
	case k_ir_graph: {
		ir_graph *irg = (ir_graph*)X;
		if (irg == get_const_code_irg())
			snprintf(buf, sizeof(buf), "%s<ConstCodeIrg>", A("irg"));
		else
			snprintf(buf, sizeof(buf), "%s%s", A("irg"), get_entity_name(get_irg_entity(irg)));
		snprintf(add, sizeof(add), "[%ld]", get_irg_graph_nr(irg));
		break;
	}
	case k_ir_node: {
		ir_node *node = (ir_node*)X;
		switch (occ->conversion) {
		case 'B':
			block = !is_Block(node) ? get_nodes_block(node) : node;
			snprintf(buf, sizeof(buf), "%s%s %s", A("irn"),
			         get_irn_opname(block), get_mode_name(get_irn_mode(block)));
			snprintf(add, sizeof(add), "[%ld]", get_irn_node_nr(block));
			break;
		case 'N':
			snprintf(buf, sizeof(buf), "%ld", get_irn_node_nr(node));
			break;
		default:
			if (is_Const(node)) {
				ir_tarval *tv = get_Const_tarval(node);
				if (tv)
					tarval_snprintf(tv_buf, sizeof(tv_buf), tv);
				else
					strncpy(tv_buf, "(NULL)", sizeof(tv_buf));
				snprintf(buf, sizeof(buf), "%s%s %s<%s>", A("irn"), get_irn_opname(node),
					get_mode_name(get_irn_mode(node)), tv_buf);
			} else if (is_SymConst_addr_ent(node)) {
				snprintf(buf, sizeof(buf), "%s%s %s[%s]", A("irn"), get_irn_opname(node),
				get_mode_name(get_irn_mode(node)), get_entity_name(get_SymConst_entity(node)));
			} else if (is_Sel(node)) {
				snprintf(buf, sizeof(buf), "%s%s %s[%s]", A("irn"), get_irn_opname(node),
				get_mode_name(get_irn_mode(node)), get_entity_name(get_Sel_entity(node)));
			} else {
				snprintf(buf, sizeof(buf), "%s%s %s", A("irn"), get_irn_opname(node),
				get_mode_name(get_irn_mode(node)));
			}
			snprintf(add, sizeof(add), "[%ld:%u]", get_irn_node_nr(node), get_irn_idx(node));
		}
		break;
	}
	case k_ir_mode: {
		ir_mode *mode = (ir_mode*)X;
		snprintf(buf, sizeof(buf), "%s%s", A("mode"), get_mode_name(mode));
		break;
	}
	case k_tarval: {
		ir_tarval *tarval = (ir_tarval*)X;
		tarval_snprintf(tv_buf, sizeof(tv_buf), tarval);
		snprintf(buf, sizeof(buf), "%s%s", A("tv"), tv_buf);
		break;
	}
	case k_ir_loop: {
		ir_loop *loop = (ir_loop*)X;
		snprintf(buf, sizeof(buf), "loop[%d:%d]", get_loop_loop_nr(loop), get_loop_depth(loop));
		break;
	}
	case k_ir_op: {
		ir_op *op = (ir_op*)X;
		snprintf(buf, sizeof(buf), "%s%s", A("op"), get_op_name(op));
		break;
	}
	case k_ir_compound_graph_path: {
		compound_graph_path *path = (compound_graph_path*)X;
		n = get_compound_graph_path_length(path);

		for (i = 0; i < n; ++i) {
			ent = get_compound_graph_path_node(path, i);

			strncat(buf, ".", sizeof(buf)-1);
			strncat(buf, get_entity_name(ent), sizeof(buf)-1);
			if (is_Array_type(get_entity_owner(ent))) {
				snprintf(add, sizeof(add), "[%d]",
					get_compound_graph_path_array_index(path, i));
				strncat(buf, add, sizeof(buf)-1);
			}
		}
		add[0] = '\0';
		break;
	}
	case k_ir_extblk: {
		ir_extblk *extblk = (ir_extblk*)X;
		snprintf(buf, sizeof(buf), "ExtBlock");
		snprintf(add, sizeof(add), "[%ld]", get_irn_node_nr(get_extbb_leader(extblk)));
		break;
	}

	default:
		snprintf(buf, sizeof(buf), "UNKWN");
		snprintf(add, sizeof(add), "[%p]", X);
	}

	if (occ->flag_plus)
		strncat(buf, add, sizeof(buf)-1);

	return lc_arg_append(app, occ, buf, strlen(buf));
#undef A
}

/**
 * emit an ident
 */
static int firm_emit_ident(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	ident *id = (ident *)arg->v_ptr;
	const char *p = id ? get_id_str(id) : "(null)";

	return lc_arg_append(app, occ, p, strlen(p));
}

/**
 * Emit indent.
 */
static int firm_emit_indent(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	int i;
	int width  = occ->width > 0 ? occ->width : 1;
	int amount = arg->v_int * width;

	for (i = 0; i < amount; ++i)
		lc_appendable_chadd(app, (i % width) == 0 ? '|' : ' ');

	return amount;
}

/**
 * Emit pnc.
 */
static int firm_emit_pnc(lc_appendable_t *app,
    const lc_arg_occ_t *occ, const lc_arg_value_t *arg)
{
	int value = arg->v_int;
	const char *p = get_pnc_string(value);

	return lc_arg_append(app, occ, p, strlen(p));
}

lc_arg_env_t *firm_get_arg_env(void)
{

	static lc_arg_env_t *env = NULL;

	static lc_arg_handler_t firm_handler   = { firm_get_arg_type, firm_emit };
	static lc_arg_handler_t ident_handler  = { firm_get_arg_type, firm_emit_ident };
	static lc_arg_handler_t indent_handler = { firm_get_arg_type_int, firm_emit_indent };
	static lc_arg_handler_t pnc_handler    = { firm_get_arg_type_int, firm_emit_pnc };
	static lc_arg_handler_t bitset_handler = { bitset_get_arg_type, bitset_emit };
	static lc_arg_handler_t debug_handler  = { firm_get_arg_type, firm_emit_dbg };

	static struct {
		const char *name;
		char letter;
	} args[] = {
		{"firm:type",      't'},
		{"firm:entity",    'e'},
		{"firm:entity_ld", 'E'},
		{"firm:tarval",    'T'},
		{"firm:irn",       'n'},
		{"firm:op",        'O'},
		{"firm:irn_nr",    'N'},
		{"firm:mode",      'm'},
		{"firm:block",     'B'},
		{"firm:cg_path",   'P'},
	};

	size_t i;

	if (env == NULL) {
		env = lc_arg_new_env();
		lc_arg_add_std(env);

		lc_arg_register(env, "firm", 'F', &firm_handler);
		for (i = 0; i < sizeof(args)/sizeof(args[0]); ++i)
			lc_arg_register(env, args[i].name, args[i].letter, &firm_handler);

		lc_arg_register(env, "firm:ident",    'I', &ident_handler);
		lc_arg_register(env, "firm:indent",   'D', &indent_handler);
		lc_arg_register(env, "firm:dbg_info", 'G', &debug_handler);
		lc_arg_register(env, "firm:bitset",   'B', &bitset_handler);
		lc_arg_register(env, "firm:pnc",      '=', &pnc_handler);
	}

	return env;
}
