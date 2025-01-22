/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Support for libcore IR object output.
 * @author   Sebastian Hack
 */
#include "irargs_t.h"

#include "bitset.h"
#include "dbginfo_t.h"
#include "entity_t.h"
#include "firm_common.h"
#include "irloop_t.h"
#include "irnode_t.h"
#include "irprintf.h"
#include "lc_printf.h"
#include "tv_t.h"
#include "util.h"
#include <ctype.h>

/**
 * identify a firm object type
 */
static int firm_get_arg_type(const lc_arg_occ_t *occ)
{
	(void)occ;
	/* Firm objects are always pointer */
	return lc_arg_type_ptr;
}

static int firm_get_arg_type_int(const lc_arg_occ_t *occ)
{
	(void)occ;
	return lc_arg_type_int;
}


static int bitset_get_arg_type(const lc_arg_occ_t *occ)
{
	(void)occ;
	return lc_arg_type_ptr;
}

static int bitset_emit(lc_appendable_t *app, const lc_arg_occ_t *occ,
                       const lc_arg_value_t *arg)
{
	int res = 0;
	lc_arg_append(app, occ, "[", 1);
	++res;
	const char *prefix = "";
	bitset_t *b = (bitset_t*)arg->v_ptr;
	bitset_foreach(b, p) {
		char buf[32];
		int  n = snprintf(buf, sizeof(buf), "%s%d", prefix, (int) p);
		lc_arg_append(app, occ, buf, n);
		prefix = ", ";
		res += n;
	}
	lc_arg_append(app, occ, "]", 1);
	++res;

	return res;
}

/**
 * emit an opaque Firm dbg_info object
 */
static int firm_emit_dbg(lc_appendable_t *app, const lc_arg_occ_t *occ,
                         const lc_arg_value_t *arg)
{
	ir_node  *irn = (ir_node*)arg->v_ptr;
	dbg_info *dbg = get_irn_dbg_info(irn);

	char buf[1024];
	ir_dbg_info_snprint(buf, sizeof(buf), dbg);
	size_t size = strlen(buf);
	lc_arg_append(app, occ, buf, size);
	return size;
}

static void print_entity_name(char *buffer, size_t buffer_size,
                              char const *const add,
                              ir_entity const *const entity)
{
	switch (entity->kind) {
	case IR_ENTITY_PARAMETER:
		snprintf(buffer, buffer_size, "%sparameter.%lu", add,
		         (unsigned long)get_entity_parameter_number(entity));
		return;
	case IR_ENTITY_LABEL:
		snprintf(buffer, buffer_size, "%slabel", add);
		return;
	case IR_ENTITY_SPILLSLOT:
		snprintf(buffer, buffer_size, "%sspillslot", add);
		return;
	default:
		snprintf(buffer, buffer_size, "%s%s", add, get_entity_ld_name(entity));
		return;
	}
}

static const char *get_entity_name_or_null_str(const ir_entity *const ent)
{
	return ent != NULL ? get_entity_name(ent) : "(null)";
}

/**
 * emit a Firm object
 */
static int firm_emit(lc_appendable_t *app, const lc_arg_occ_t *occ,
                     const lc_arg_value_t *arg)
{
	void *X = (void*)arg->v_ptr;
	if (X == NULL)
		return lc_arg_append(app, occ, "(null)", 6);

	char buf[256];
	buf[0] = '\0';
	char add[64];
	add[0] = '\0';

#define A(s)    occ->flag_hash ? s " ": ""
	firm_kind *obj = (firm_kind*)X;
	switch (*obj) {
	case k_BAD:
		snprintf(buf, sizeof(buf), "BAD");
		snprintf(add, sizeof(add), "[%p]", X);
		break;
	case k_entity: {
		ir_entity const *const entity = (ir_entity const*)X;
		print_entity_name(buf, sizeof(buf), A("ent"), entity);
		snprintf(add, sizeof(add), "[%ld]", get_entity_nr(entity));
		break;
	}
	case k_type: {
		ir_type *type = (ir_type*)X;
		char type_name[256];
		ir_print_type(type_name, sizeof(type_name), type);
		tp_opcode opcode = get_type_opcode(type);
		snprintf(buf, sizeof(buf), "%s%s:%s", A("type"),
		         get_type_opcode_name(opcode), type_name);
		snprintf(add, sizeof(add), "[%ld]", get_type_nr(type));
		break;
	}
	case k_ir_graph: {
		ir_graph *irg = (ir_graph*)X;
		if (irg == get_const_code_irg())
			snprintf(buf, sizeof(buf), "%s<ConstCodeIrg>", A("irg"));
		else
			ir_snprintf(buf, sizeof(buf), "%s%F", A("irg"), get_irg_entity(irg));
		snprintf(add, sizeof(add), "[%ld]", get_irg_graph_nr(irg));
		break;
	}
	case k_ir_node: {
		ir_node *node = (ir_node*)X;
		switch (occ->conversion) {
		case 'B': {
			ir_node *const block = get_block(node);
			snprintf(buf, sizeof(buf), "%s%s %s", A("irn"),
			         get_irn_opname(block), get_mode_name(get_irn_mode(block)));
			snprintf(add, sizeof(add), "[%ld]", get_irn_node_nr(block));
			break;
		}
		case 'N':
			snprintf(buf, sizeof(buf), "%ld", get_irn_node_nr(node));
			break;
		default:
			if (is_Const(node)) {
				char tv_buf[256];
				ir_tarval *tv = get_Const_tarval(node);
				if (tv)
					tarval_snprintf(tv_buf, sizeof(tv_buf), tv);
				else
					strncpy(tv_buf, "(NULL)", sizeof(tv_buf));
				snprintf(buf, sizeof(buf), "%s%s %s<%s>", A("irn"), get_irn_opname(node),
				         get_mode_name(get_irn_mode(node)), tv_buf);
			} else if (is_Address(node)) {
				const char *entity_name = get_entity_name_or_null_str(get_Address_entity(node));
				snprintf(buf, sizeof(buf), "%s%s %s[%s]", A("irn"), get_irn_opname(node),
				         get_mode_name(get_irn_mode(node)), entity_name);
			} else if (is_Member(node)) {
				const char *entity_name = get_entity_name_or_null_str(get_Member_entity(node));
				snprintf(buf, sizeof(buf), "%s%s %s[%s]", A("irn"), get_irn_opname(node),
				         get_mode_name(get_irn_mode(node)), entity_name);
			} else if (is_Cmp(node)) {
				ir_relation relation = get_Cmp_relation(node);
				snprintf(buf, sizeof(buf), "%s%s %s", A("irn"), get_irn_opname(node), get_relation_string(relation));
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
		char tv_buf[256];
		tarval_snprintf(tv_buf, sizeof(tv_buf), tarval);
		snprintf(buf, sizeof(buf), "%s%s", A("tv"), tv_buf);
		break;
	}
	case k_ir_loop: {
		ir_loop *loop = (ir_loop*)X;
		snprintf(buf, sizeof(buf), "loop[%ld:%u]", get_loop_loop_nr(loop), get_loop_depth(loop));
		break;
	}

	default:
		snprintf(buf, sizeof(buf), "UNKWN");
		snprintf(add, sizeof(add), "[%p]", X);
	}

	if (occ->flag_plus)
		strncat(buf, add, sizeof(buf)-strlen(buf)-1);

	size_t size = strlen(buf);
	lc_arg_append(app, occ, buf, size);
	return size;
#undef A
}

/**
 * emit an ident
 */
static int firm_emit_ident(lc_appendable_t *app, const lc_arg_occ_t *occ,
                           const lc_arg_value_t *arg)
{
	ident      *id   = (ident *)arg->v_ptr;
	const char *p    = id != NULL ? get_id_str(id) : "(null)";
	size_t      size = strlen(p);
	lc_arg_append(app, occ, p, size);
	return size;
}

/**
 * Emit indent.
 */
static int firm_emit_indent(lc_appendable_t *app, const lc_arg_occ_t *occ,
                            const lc_arg_value_t *arg)
{
	int width  = MAX(1, occ->width);
	int amount = arg->v_int * width;
	for (int i = 0; i < amount; ++i)
		lc_appendable_chadd(app, (i % width) == 0 ? '|' : ' ');
	return amount;
}

/**
 * Emit pnc.
 */
static int firm_emit_pnc(lc_appendable_t *app, const lc_arg_occ_t *occ,
                         const lc_arg_value_t *arg)
{
	ir_relation value = (ir_relation)arg->v_int;
	const char *p = get_relation_string(value);

	size_t size = strlen(p);
	lc_arg_append(app, occ, p, size);
	return size;
}

lc_arg_env_t *firm_get_arg_env(void)
{
	static lc_arg_handler_t const firm_handler   = { firm_get_arg_type, firm_emit };
	static lc_arg_handler_t const ident_handler  = { firm_get_arg_type, firm_emit_ident };
	static lc_arg_handler_t const indent_handler = { firm_get_arg_type_int, firm_emit_indent };
	static lc_arg_handler_t const pnc_handler    = { firm_get_arg_type_int, firm_emit_pnc };
	static lc_arg_handler_t const bitset_handler = { bitset_get_arg_type, bitset_emit };
	static lc_arg_handler_t const debug_handler  = { firm_get_arg_type, firm_emit_dbg };

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
	};

	static lc_arg_env_t *env = NULL;
	if (env == NULL) {
		env = lc_arg_new_env();
		lc_arg_add_std(env);

		lc_arg_register(env, "firm", 'F', &firm_handler);
		for (size_t i = 0; i != ARRAY_SIZE(args); ++i)
			lc_arg_register(env, args[i].name, args[i].letter, &firm_handler);

		lc_arg_register(env, "firm:ident",    'I', &ident_handler);
		lc_arg_register(env, "firm:indent",   'D', &indent_handler);
		lc_arg_register(env, "firm:dbg_info", 'G', &debug_handler);
		lc_arg_register(env, "firm:bitset",   'B', &bitset_handler);
		lc_arg_register(env, "firm:pnc",      '=', &pnc_handler);
	}

	return env;
}
