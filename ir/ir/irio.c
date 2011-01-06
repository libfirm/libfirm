/*
 * Copyright (C) 1995-2009 University of Karlsruhe.  All right reserved.
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
 * @brief   Write textual representation of firm to file.
 * @author  Moritz Kroll
 * @version $Id$
 */
#include "config.h"

#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdarg.h>

#include "irio.h"

#include "irnode_t.h"
#include "irprog.h"
#include "irgraph_t.h"
#include "ircons.h"
#include "irgmod.h"
#include "irflag_t.h"
#include "irgwalk.h"
#include "tv.h"
#include "array.h"
#include "error.h"
#include "typerep.h"
#include "adt/set.h"
#include "adt/obst.h"

#define SYMERROR ((unsigned) ~0)

typedef struct io_env_t
{
	int c;                /**< currently read char */
	FILE *file;
	set *idset;           /**< id_entry set, which maps from file ids to new Firm elements */
	int ignoreblocks;
	const char *inputname;
	int line;
	ir_type **fixedtypes;
	struct obstack obst;
	ir_graph *irg;
} io_env_t;

typedef enum typetag_t
{
	tt_align,
	tt_allocation,
	tt_builtin,
	tt_cond_jmp_predicate,
	tt_initializer,
	tt_iro,
	tt_keyword,
	tt_mode_sort,
	tt_mode_arithmetic,
	tt_pin_state,
	tt_tpo,
	tt_type_state,
	tt_volatility,
	tt_linkage,
	tt_segment,
	tt_visibility
} typetag_t;

typedef enum keyword_t
{
	kw_constirg,
	kw_entity,
	kw_irg,
	kw_mode,
	kw_modes,
	kw_type,
	kw_typegraph,
	kw_program,
	kw_segment_type
} keyword_t;

typedef struct symbol_t
{
	const char *str;      /**< The name of this symbol. */
	typetag_t   typetag;  /**< The type tag of this symbol. */
	unsigned    code;     /**< The value of this symbol. */
} symbol_t;

typedef struct id_entry
{
	long id;
	void *elem;
} id_entry;

/** The symbol table, a set of symbol_t elements. */
static set *symtbl;

/**
 * Compare two symbol table entries.
 */
static int symbol_cmp(const void *elt, const void *key, size_t size)
{
	int res;
	const symbol_t *entry = (const symbol_t *) elt;
	const symbol_t *keyentry = (const symbol_t *) key;
	(void) size;
	res = entry->typetag - keyentry->typetag;
	if (res) return res;
	return strcmp(entry->str, keyentry->str);
}

static int id_cmp(const void *elt, const void *key, size_t size)
{
	const id_entry *entry = (const id_entry *) elt;
	const id_entry *keyentry = (const id_entry *) key;
	(void) size;
	return entry->id - keyentry->id;
}

static void __attribute__((format(printf, 2, 3)))
parse_error(io_env_t *env, const char *fmt, ...)
{
	va_list ap;
	int     line = env->line;

	/* workaround read_c "feature" that a '\n' triggers the line++
	 * instead of the character after the '\n' */
	if (env->c == '\n') {
		line--;
	}

	fprintf(stderr, "%s:%d: error ", env->inputname, line);

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
}

/** Initializes the symbol table. May be called more than once without problems. */
static void symtbl_init(void)
{
	symbol_t key;

	/* Only initialize once */
	if (symtbl != NULL)
		return;

	symtbl = new_set(symbol_cmp, 256);

#define INSERT(tt, s, cod)                                       \
	key.str = (s);                                               \
	key.typetag = (tt);                                          \
	key.code = (cod);                                            \
	set_insert(symtbl, &key, sizeof(key), firm_fnv_hash_str(s) + tt * 17)

#define INSERTENUM(tt, e) INSERT(tt, #e, e)
#define INSERTKEYWORD(k) INSERT(tt_keyword, #k, kw_##k)

	INSERT(tt_tpo, "array", tpo_array);
	INSERT(tt_tpo, "class", tpo_class);
	INSERT(tt_tpo, "method", tpo_method);
	INSERT(tt_tpo, "pointer", tpo_pointer);
	INSERT(tt_tpo, "primitive", tpo_primitive);
	INSERT(tt_tpo, "struct", tpo_struct);
	INSERT(tt_tpo, "union", tpo_union);
	INSERT(tt_tpo, "Unknown", tpo_unknown);

	INSERT(tt_mode_sort, "auxiliary", irms_auxiliary);
	INSERT(tt_mode_sort, "control_flow", irms_control_flow);
	INSERT(tt_mode_sort, "memory", irms_memory);
	INSERT(tt_mode_sort, "internal_boolean", irms_internal_boolean);
	INSERT(tt_mode_sort, "reference", irms_reference);
	INSERT(tt_mode_sort, "int_number", irms_int_number);
	INSERT(tt_mode_sort, "float_number", irms_float_number);

	INSERT(tt_segment, "global", IR_SEGMENT_GLOBAL);
	INSERT(tt_segment, "thread_local", IR_SEGMENT_THREAD_LOCAL);
	INSERT(tt_segment, "constructors", IR_SEGMENT_CONSTRUCTORS);
	INSERT(tt_segment, "destructors", IR_SEGMENT_DESTRUCTORS);

	INSERT(tt_linkage, "constant", IR_LINKAGE_CONSTANT);
	INSERT(tt_linkage, "weak", IR_LINKAGE_WEAK);
	INSERT(tt_linkage, "garbage_collect", IR_LINKAGE_GARBAGE_COLLECT);
	INSERT(tt_linkage, "merge", IR_LINKAGE_MERGE);
	INSERT(tt_linkage, "hidden_user", IR_LINKAGE_HIDDEN_USER);

	INSERT(tt_visibility, "local", ir_visibility_local);
	INSERT(tt_visibility, "external", ir_visibility_external);
	INSERT(tt_visibility, "default", ir_visibility_default);
	INSERT(tt_visibility, "private", ir_visibility_private);

	INSERTKEYWORD(constirg);
	INSERTKEYWORD(entity);
	INSERTKEYWORD(irg);
	INSERTKEYWORD(mode);
	INSERTKEYWORD(modes);
	INSERTKEYWORD(type);
	INSERTKEYWORD(typegraph);
	INSERTKEYWORD(program);
	INSERTKEYWORD(segment_type);

#include "gen_irio_lex.inl"

	INSERTENUM(tt_align, align_non_aligned);
	INSERTENUM(tt_align, align_is_aligned);

	INSERTENUM(tt_builtin, ir_bk_trap);
	INSERTENUM(tt_builtin, ir_bk_debugbreak);
	INSERTENUM(tt_builtin, ir_bk_return_address);
	INSERTENUM(tt_builtin, ir_bk_frame_address);
	INSERTENUM(tt_builtin, ir_bk_prefetch);
	INSERTENUM(tt_builtin, ir_bk_ffs);
	INSERTENUM(tt_builtin, ir_bk_clz);
	INSERTENUM(tt_builtin, ir_bk_ctz);
	INSERTENUM(tt_builtin, ir_bk_popcount);
	INSERTENUM(tt_builtin, ir_bk_parity);
	INSERTENUM(tt_builtin, ir_bk_bswap);
	INSERTENUM(tt_builtin, ir_bk_inport);
	INSERTENUM(tt_builtin, ir_bk_outport);
	INSERTENUM(tt_builtin, ir_bk_inner_trampoline);

	INSERTENUM(tt_cond_jmp_predicate, COND_JMP_PRED_NONE);
	INSERTENUM(tt_cond_jmp_predicate, COND_JMP_PRED_TRUE);
	INSERTENUM(tt_cond_jmp_predicate, COND_JMP_PRED_FALSE);

	INSERTENUM(tt_initializer, IR_INITIALIZER_CONST);
	INSERTENUM(tt_initializer, IR_INITIALIZER_TARVAL);
	INSERTENUM(tt_initializer, IR_INITIALIZER_NULL);
	INSERTENUM(tt_initializer, IR_INITIALIZER_COMPOUND);

	INSERTENUM(tt_mode_arithmetic, irma_uninitialized);
	INSERTENUM(tt_mode_arithmetic, irma_none);
	INSERTENUM(tt_mode_arithmetic, irma_twos_complement);
	INSERTENUM(tt_mode_arithmetic, irma_ones_complement);
	INSERTENUM(tt_mode_arithmetic, irma_int_BCD);
	INSERTENUM(tt_mode_arithmetic, irma_ieee754);
	INSERTENUM(tt_mode_arithmetic, irma_float_BCD);

	INSERTENUM(tt_pin_state, op_pin_state_floats);
	INSERTENUM(tt_pin_state, op_pin_state_pinned);
	INSERTENUM(tt_pin_state, op_pin_state_exc_pinned);
	INSERTENUM(tt_pin_state, op_pin_state_mem_pinned);

	INSERTENUM(tt_type_state, layout_undefined);
	INSERTENUM(tt_type_state, layout_fixed);

	INSERTENUM(tt_volatility, volatility_non_volatile);
	INSERTENUM(tt_volatility, volatility_is_volatile);

#undef INSERTKEYWORD
#undef INSERTENUM
#undef INSERT
}

static const char *get_segment_name(ir_segment_t segment)
{
	switch (segment) {
	case IR_SEGMENT_GLOBAL:       return "global";
	case IR_SEGMENT_THREAD_LOCAL: return "thread_local";
	case IR_SEGMENT_CONSTRUCTORS: return "constructors";
	case IR_SEGMENT_DESTRUCTORS:  return "destructors";
	}
	return "INVALID_SEGMENT";
}

static const char *get_visibility_name(ir_visibility visibility)
{
	switch (visibility) {
	case ir_visibility_local:    return "local";
	case ir_visibility_external: return "external";
	case ir_visibility_default:  return "default";
	case ir_visibility_private:  return "private";
	}
	return "INVALID_VISIBILITY";
}

/** Returns the according symbol value for the given string and tag, or SYMERROR if none was found. */
static unsigned symbol(const char *str, typetag_t typetag)
{
	symbol_t key, *entry;

	key.str = str;
	key.typetag = typetag;

	entry = (symbol_t*)set_find(symtbl, &key, sizeof(key), firm_fnv_hash_str(str) + typetag * 17);
	return entry ? entry->code : SYMERROR;
}

static void *get_id(io_env_t *env, long id)
{
	id_entry key, *entry;
	key.id = id;

	entry = (id_entry*)set_find(env->idset, &key, sizeof(key), (unsigned) id);
	return entry ? entry->elem : NULL;
}

static void set_id(io_env_t *env, long id, void *elem)
{
	id_entry key;
	key.id = id;
	key.elem = elem;
	set_insert(env->idset, &key, sizeof(key), (unsigned) id);
}

static void write_long(io_env_t *env, long value)
{
	fprintf(env->file, "%ld ", value);
}

static void write_int(io_env_t *env, int value)
{
	fprintf(env->file, "%d ", value);
}

static void write_unsigned(io_env_t *env, unsigned value)
{
	fprintf(env->file, "%u ", value);
}

static void write_entity_ref(io_env_t *env, ir_entity *entity)
{
	write_long(env, get_entity_nr(entity));
}

static void write_type_ref(io_env_t *env, ir_type *type)
{
	write_long(env, get_type_nr(type));
}

static void write_string(io_env_t *env, const char *string)
{
	const char *c;
	fputc('"', env->file);
	for (c = string; *c != '\0'; ++c) {
		switch (*c) {
		case '\n':
			fputc('\\', env->file);
			fputc('n', env->file);
			break;
		case '"':
		case '\\':
			fputc('\\', env->file);
			/* FALLTHROUGH */
		default:
			fputc(*c, env->file);
			break;
		}
	}
	fputc('"', env->file);
}

static void write_ident(io_env_t *env, ident *id)
{
	write_string(env, get_id_str(id));
	fputc(' ', env->file);
}

static void write_ident_null(io_env_t *env, ident *id)
{
	if (id == NULL) {
		fputs("NULL ", env->file);
	} else {
		write_ident(env, id);
	}
}

static void write_mode(io_env_t *env, ir_mode *mode)
{
	fputs(get_mode_name(mode), env->file);
	fputc(' ', env->file);
}

static void write_tarval(io_env_t *env, ir_tarval *tv)
{
	char buf[1024];
	write_mode(env, get_tarval_mode(tv));
	tarval_snprintf(buf, sizeof(buf), tv);
	fputs(buf, env->file);
	fputc(' ', env->file);
}

static void write_align(io_env_t *env, ir_node *irn)
{
	ir_align align;

	if (is_Load(irn))
		align = get_Load_align(irn);
	else if (is_Store(irn))
		align = get_Store_align(irn);
	else
		panic("Invalid optype for write_align");

	fputs(get_align_name(align), env->file);
	fputc(' ', env->file);
}

static void write_builtin_kind(io_env_t *env, ir_node *irn)
{
	fputs(get_builtin_kind_name(get_Builtin_kind(irn)), env->file);
	fputc(' ', env->file);
}

static void write_cond_jmp_predicate(io_env_t *env, ir_node *irn)
{
	fputs(get_cond_jmp_predicate_name(get_Cond_jmp_pred(irn)), env->file);
	fputc(' ', env->file);
}

static void write_list_begin(io_env_t *env)
{
	fputs("[", env->file);
}

static void write_list_end(io_env_t *env)
{
	fputs("] ", env->file);
}

static void write_initializer(io_env_t *env, ir_initializer_t *ini)
{
	FILE *f = env->file;
	ir_initializer_kind_t ini_kind = get_initializer_kind(ini);

	fputs(get_initializer_kind_name(ini_kind), f);
	fputc(' ', f);

	switch (ini_kind) {
	case IR_INITIALIZER_CONST:
		write_long(env, get_irn_node_nr(get_initializer_const_value(ini)));
		break;

	case IR_INITIALIZER_TARVAL:
		write_tarval(env, get_initializer_tarval_value(ini));
		break;

	case IR_INITIALIZER_NULL:
		break;

	case IR_INITIALIZER_COMPOUND: {
		unsigned i, n = get_initializer_compound_n_entries(ini);
		fprintf(f, "%u ", n);
		for (i = 0; i < n; i++)
			write_initializer(env, get_initializer_compound_value(ini, i));
		break;
	}

	default:
		panic("Unknown initializer kind");
	}
}

static void write_pin_state(io_env_t *env, ir_node *irn)
{
	fputs(get_op_pin_state_name(get_irn_pinned(irn)), env->file);
	fputc(' ', env->file);
}

static void write_volatility(io_env_t *env, ir_node *irn)
{
	ir_volatility vol;

	if (is_Load(irn))
		vol = get_Load_volatility(irn);
	else if (is_Store(irn))
		vol = get_Store_volatility(irn);
	else
		panic("Invalid optype for write_volatility");

	fputs(get_volatility_name(vol), env->file);
	fputc(' ', env->file);
}

static void export_type_common(io_env_t *env, ir_type *tp)
{
	fprintf(env->file, "\ttype %ld %s %u %u %s %u ",
	        get_type_nr(tp),
	        get_type_tpop_name(tp),
	        get_type_size_bytes(tp),
	        get_type_alignment_bytes(tp),
	        get_type_state_name(get_type_state(tp)),
	        tp->flags);
}

static void export_type_pre(io_env_t *env, ir_type *tp)
{
	FILE *f = env->file;

	/* skip types to be handled by post walker */
	switch (get_type_tpop_code(tp)) {
	case tpo_array:
	case tpo_method:
	case tpo_pointer:
		return;
	default:
		break;
	}

	export_type_common(env, tp);

	switch (get_type_tpop_code(tp)) {
	case tpo_uninitialized:
		panic("invalid type found");

	case tpo_class:
		write_ident_null(env, get_compound_ident(tp));
		break;

	case tpo_primitive:
		write_mode(env, get_type_mode(tp));
		break;

	case tpo_union:
	case tpo_struct:
	case tpo_enumeration:
		write_ident_null(env, get_compound_ident(tp));
		break;

	case tpo_array:
	case tpo_method:
	case tpo_pointer:
	case tpo_code:
	case tpo_none:
	case tpo_unknown:
		break;
	}
	fputc('\n', f);
}

static void export_type_post(io_env_t *env, ir_type *tp)
{
	FILE *f = env->file;
	int i;

	/* skip types already handled by pre walker */
	switch (get_type_tpop_code(tp)) {
	case tpo_class:
	case tpo_primitive:
	case tpo_struct:
	case tpo_union:
	case tpo_unknown:
	case tpo_uninitialized:
	case tpo_code:
	case tpo_none:
		return;
	case tpo_array:
	case tpo_method:
	case tpo_pointer:
	case tpo_enumeration:
		break;
	}

	export_type_common(env, tp);

	switch (get_type_tpop_code(tp)) {
	case tpo_array: {
		int n = get_array_n_dimensions(tp);
		fprintf(f, "%d %ld ", n, get_type_nr(get_array_element_type(tp)));
		for (i = 0; i < n; i++) {
			ir_node *lower = get_array_lower_bound(tp, i);
			ir_node *upper = get_array_upper_bound(tp, i);

			if (is_Const(lower))
				write_long(env, get_tarval_long(get_Const_tarval(lower)));
			else
				panic("Lower array bound is not constant");

			if (is_Const(upper))
				write_long(env, get_tarval_long(get_Const_tarval(upper)));
			else if (is_Unknown(upper))
				fputs("unknown ", f);
			else
				panic("Upper array bound is not constant");
		}
		break;
	}

	case tpo_method: {
		int nparams  = get_method_n_params(tp);
		int nresults = get_method_n_ress(tp);
		fprintf(f, "%u %u %d %d ", get_method_calling_convention(tp),
			get_method_additional_properties(tp), nparams, nresults);
		for (i = 0; i < nparams; i++)
			write_long(env, get_type_nr(get_method_param_type(tp, i)));
		for (i = 0; i < nresults; i++)
			write_long(env, get_type_nr(get_method_res_type(tp, i)));
		fprintf(f, "%d ", get_method_first_variadic_param_index(tp));
		break;
	}

	case tpo_pointer:
		write_mode(env, get_type_mode(tp));
		write_long(env, get_type_nr(get_pointer_points_to_type(tp)));
		break;

	case tpo_enumeration:
		fprintf(stderr, "Enumeration type not handled yet by exporter\n");
		break;

	default:
		printf("export_type: Unknown type code \"%s\".\n",
		       get_type_tpop_name(tp));
		break;
	}
	fputc('\n', f);
}

static void export_entity(io_env_t *env, ir_entity *ent)
{
	FILE          *file       = env->file;
	ir_type       *owner      = get_entity_owner(ent);
	ir_visibility  visibility = get_entity_visibility(ent);
	ir_linkage     linkage    = get_entity_linkage(ent);

	/* we don't dump array_element_ent entities. They're a strange concept
	 * and lead to cycles in type_graph.
	 */
	if (is_Array_type(owner))
		return;

	fprintf(env->file, "\tentity ");
	write_long(env, get_entity_nr(ent));
	write_ident_null(env, get_entity_ident(ent));
	if (!entity_has_ld_ident(ent)) {
		write_ident_null(env, NULL);
	} else {
		write_ident_null(env, get_entity_ld_ident(ent));
	}

	/* visibility + linkage */
	if (visibility != ir_visibility_default) {
		fprintf(file, "%s ", get_visibility_name(visibility));
	}
	if (linkage & IR_LINKAGE_CONSTANT)
		fputs("constant ", file);
	if (linkage & IR_LINKAGE_WEAK)
		fputs("weak ", file);
	if (linkage & IR_LINKAGE_GARBAGE_COLLECT)
		fputs("garbage_collect ", file);
	if (linkage & IR_LINKAGE_MERGE)
		fputs("merge ", file);
	if (linkage & IR_LINKAGE_HIDDEN_USER)
		fputs("hidden_user ", file);

	fprintf(file, "%ld %ld %d %u %d %s ",
			get_type_nr(get_entity_type(ent)),
			get_type_nr(owner),
			get_entity_offset(ent),
			(unsigned) get_entity_offset_bits_remainder(ent),
			is_entity_compiler_generated(ent),
			get_volatility_name(get_entity_volatility(ent)));

	if (ent->initializer != NULL) {
		fputs("initializer ", env->file);
		write_initializer(env, get_entity_initializer(ent));
	} else if (entity_has_compound_ent_values(ent)) {
		int i, n = get_compound_ent_n_values(ent);
		fputs("compoundgraph ", env->file);
		fprintf(env->file, "%d ", n);
		for (i = 0; i < n; i++) {
			ir_entity *member = get_compound_ent_value_member(ent, i);
			ir_node   *irn    = get_compound_ent_value(ent, i);
			fprintf(env->file, "%ld %ld ", get_entity_nr(member), get_irn_node_nr(irn));
		}
	} else {
		fputs("none", env->file);
	}

	fputc('\n', env->file);
}

static void export_type_or_ent_pre(type_or_ent tore, void *ctx)
{
	io_env_t *env = (io_env_t *) ctx;
	if (get_kind(tore.typ) == k_type)
		export_type_pre(env, tore.typ);
}

static void export_type_or_ent_post(type_or_ent tore, void *ctx)
{
	io_env_t *env = (io_env_t *) ctx;

	switch (get_kind(tore.ent)) {
	case k_entity:
		export_entity(env, tore.ent);
		break;

	case k_type:
		export_type_post(env, tore.typ);
		break;

	default:
		panic("export_type_or_ent_post: Unknown type or entity.");
		break;
	}
}

/**
 * Walker: exports every node.
 */
static void export_node(ir_node *irn, void *ctx)
{
	io_env_t *env = (io_env_t *) ctx;
	int i, n;
	unsigned opcode = get_irn_opcode(irn);

	if (env->ignoreblocks && opcode == iro_Block)
		return;

	fprintf(env->file, "\t%s ", get_irn_opname(irn));
	write_long(env, get_irn_node_nr(irn));

	write_list_begin(env);
	n = get_irn_arity(irn);
	if (!is_Block(irn)) {
		ir_node *block = get_nodes_block(irn);
		write_long(env, get_irn_node_nr(block));
	}

	for (i = 0; i < n; i++) {
		ir_node *pred = get_irn_n(irn, i);
		if (pred == NULL) {
			/* Anchor node may have NULL predecessors */
			assert(is_Anchor(irn));
			fputs("-1 ", env->file);
		} else {
			write_long(env, get_irn_node_nr(pred));
		}
	}
	write_list_end(env);

	fputs("{ ", env->file);

	switch (opcode) {
	case iro_Start:
	case iro_End:
	case iro_Block:
	case iro_Anchor:
		break;
	case iro_SymConst:
		/* TODO: only symconst_addr_ent implemented yet */
		assert(get_SymConst_kind(irn) == symconst_addr_ent);
		fprintf(env->file, "%ld ", get_entity_nr(get_SymConst_entity(irn)));
		break;
	case iro_Proj:
		write_mode(env, get_irn_mode(irn));
		fprintf(env->file, "%ld ", get_Proj_proj(irn));
		break;
#include "gen_irio_export.inl"
	default:
		panic("no export code for node %+F\n", irn);
	}
	fputs("}\n", env->file);
}

static const char *get_mode_sort_name(ir_mode_sort sort)
{
	switch (sort) {
	case irms_auxiliary:        return "auxiliary";
	case irms_control_flow:     return "control_flow";
	case irms_memory:           return "memory";
	case irms_internal_boolean: return "internal_boolean";
	case irms_reference:        return "reference";
	case irms_int_number:       return "int_number";
	case irms_float_number:     return "float_number";
	}
	panic("invalid mode sort found");
}

static void export_modes(io_env_t *env)
{
	int i, n_modes = get_irp_n_modes();

	fputs("modes {\n", env->file);

	for (i = 0; i < n_modes; i++) {
		ir_mode *mode = get_irp_mode(i);
		switch (get_mode_sort(mode)) {
		case irms_auxiliary:
		case irms_control_flow:
		case irms_memory:
		case irms_internal_boolean:
			/* skip "internal" modes, which may not be user defined */
			continue;
		default:
			break;
		}

		fprintf(env->file, "\tmode ");
		write_string(env, get_mode_name(mode));
		fprintf(env->file, "%s %u %d %s %u %u ",
		        get_mode_sort_name(get_mode_sort(mode)),
		        get_mode_size_bits(mode), get_mode_sign(mode),
		        get_mode_arithmetic_name(get_mode_arithmetic(mode)),
		        get_mode_modulo_shift(mode),
		        get_mode_n_vector_elems(mode));
		if (mode_is_reference(mode)) {
			write_mode(env, get_reference_mode_signed_eq(mode));
			write_mode(env, get_reference_mode_unsigned_eq(mode));
		}
		fputc('\n', env->file);
	}

	fputs("}\n", env->file);
}

static void export_program(io_env_t *env)
{
	FILE         *f = env->file;
	ir_segment_t  s;

	fputs("\nprogram {\n", f);
	if (irp_prog_name_is_set()) {
		fprintf(f, "\tname ");
		write_string(env, get_irp_name());
		fputc('\n', f);
	}

	for (s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s) {
		ir_type *segment_type = get_segment_type(s);
		fprintf(f, "\tsegment_type %s ", get_segment_name(s));
		if (segment_type == NULL) {
			fputs(" NULL\n", f);
		} else {
			write_long(env, get_type_nr(segment_type));
			fputc('\n', f);
		}
	}
	fputs("}\n", f);
}

void ir_export(const char *filename)
{
	FILE *file = fopen(filename, "wt");
	if (file == NULL) {
		perror(filename);
		return;
	}

	ir_export_file(file, filename);
	fclose(file);
}

/* Exports the whole irp to the given file in a textual form. */
void ir_export_file(FILE *file, const char *outputname)
{
	io_env_t env;
	int i, n_irgs = get_irp_n_irgs();

	(void) outputname;
	env.file = file;

	export_modes(&env);

	fputs("\ntypegraph {\n", env.file);
	type_walk_prog(export_type_or_ent_pre, export_type_or_ent_post, &env);
	fputs("}\n", env.file);

	for (i = 0; i < n_irgs; i++) {
		ir_graph *irg       = get_irp_irg(i);
		ir_type  *valuetype = get_irg_value_param_type(irg);

		fprintf(env.file, "\nirg %ld %ld %ld {\n",
		        get_entity_nr(get_irg_entity(irg)),
		        get_type_nr(get_irg_frame_type(irg)),
		        valuetype == NULL ? -1 : get_type_nr(valuetype));

		env.ignoreblocks = 0;
		irg_block_walk_graph(irg, NULL, export_node, &env);

		env.ignoreblocks = 1;
		irg_walk_anchors(irg, NULL, export_node, &env);

		fputs("}\n", env.file);
	}

	fprintf(env.file, "\nconstirg %ld {\n", get_irn_node_nr(get_const_code_irg()->current_block));

	walk_const_code(NULL, export_node, &env);
	fputs("}\n", env.file);

	export_program(&env);
}

/* Exports the given irg to the given file. */
void ir_export_irg(ir_graph *irg, FILE *file, const char *outputname)
{
	io_env_t env;

	(void) outputname;
	env.file = file;

	export_modes(&env);

	fputs("typegraph {\n", env.file);

	type_walk_irg(irg, export_type_or_ent_pre, export_type_or_ent_post, &env);

	fprintf(env.file, "}\n\nirg %ld {\n", get_entity_nr(get_irg_entity(irg)));

	env.ignoreblocks = 0;
	irg_block_walk_graph(irg, NULL, export_node, &env);

	env.ignoreblocks = 1;
	irg_walk_anchors(irg, NULL, export_node, &env);

	/* TODO: Only output needed constants */
	fprintf(env.file, "}\n\nconstirg %ld {\n", get_irn_node_nr(get_const_code_irg()->current_block));
	walk_const_code(NULL, export_node, &env);
	fputs("}\n", env.file);
}

static void read_c(io_env_t *env)
{
	int c = fgetc(env->file);
	env->c = c;
	if (c == '\n')
		env->line++;
}

/** Returns the first non-whitespace character or EOF. **/
static void skip_ws(io_env_t *env)
{
	while (true) {
		switch (env->c) {
		case ' ':
		case '\t':
		case '\n':
		case '\r':
			read_c(env);
			continue;

		default:
			return;
		}
	}
}

static void skip_to(io_env_t *env, char to_ch)
{
	while (env->c != to_ch && env->c != EOF) {
		read_c(env);
	}
}

static int expect_char(io_env_t *env, char ch)
{
	skip_ws(env);
	if (env->c != ch) {
		parse_error(env, "Unexpected char '%c', expected '%c'\n",
		            env->c, ch);
		return 0;
	}
	read_c(env);
	return 1;
}

#define EXPECT(c) if (expect_char(env, (c))) {} else return 0

static char *read_word(io_env_t *env)
{
	skip_ws(env);

	assert(obstack_object_size(&env->obst) == 0);
	while (true) {
		int c = env->c;
		switch (c) {
		case EOF:
		case ' ':
		case '\t':
		case '\n':
		case '\r':
			goto endofword;

		default:
			obstack_1grow(&env->obst, c);
			break;
		}
		read_c(env);
	}

endofword:
	obstack_1grow(&env->obst, '\0');
	return (char*)obstack_finish(&env->obst);
}

static char *read_string(io_env_t *env)
{
	skip_ws(env);
	if (env->c != '"') {
		parse_error(env, "Expected string, got '%c'\n", env->c);
		exit(1);
	}
	read_c(env);

	assert(obstack_object_size(&env->obst) == 0);
	while (env->c != '"') {
		if (env->c == EOF) {
			parse_error(env, "Unexpected EOF while parsing string\n");
			exit(1);
		}

		if (env->c == '\\') {
			read_c(env);
			switch (env->c) {
			case 'n':
				obstack_1grow(&env->obst, '\n');
				break;
			case '"':
			case '\\':
				obstack_1grow(&env->obst, env->c);
				break;
			default:
				parse_error(env, "Unknown escape sequence '\\%c'\n", env->c);
				exit(1);
				break;
			}
		} else {
			obstack_1grow(&env->obst, env->c);
		}
		read_c(env);
	}
	read_c(env);
	obstack_1grow(&env->obst, 0);

	return (char*)obstack_finish(&env->obst);
}

static ident *read_ident(io_env_t *env)
{
	char  *str = read_string(env);
	ident *res = new_id_from_str(str);
	obstack_free(&env->obst, str);
	return res;
}

/*
 * reads a "quoted string" or alternatively the token NULL
 */
static char *read_string_null(io_env_t *env)
{
	skip_ws(env);
	if (env->c == 'N') {
		char *str = read_word(env);
		if (strcmp(str, "NULL") == 0) {
			obstack_free(&env->obst, str);
			return NULL;
		}
	} else if (env->c == '"') {
		return read_string(env);
	}

	parse_error(env, "Expected \"string\" or NULL\n");
	exit(1);
}

static ident *read_ident_null(io_env_t *env)
{
	ident *res;
	char  *str = read_string_null(env);
	if (str == NULL)
		return NULL;

	res = new_id_from_str(str);
	obstack_free(&env->obst, str);
	return res;
}

static long read_long(io_env_t *env)
{
	long  result;
	char *str;

	skip_ws(env);
	if (!isdigit(env->c) && env->c != '-') {
		parse_error(env, "Expected number, got '%c'\n", env->c);
		exit(1);
	}

	assert(obstack_object_size(&env->obst) == 0);
	do {
		obstack_1grow(&env->obst, env->c);
		read_c(env);
	} while (isdigit(env->c));
	obstack_1grow(&env->obst, 0);

	str = (char*)obstack_finish(&env->obst);
	result = atol(str);
	obstack_free(&env->obst, str);

	return result;
}

static int read_int(io_env_t *env)
{
	return (int) read_long(env);
}

static unsigned read_unsigned(io_env_t *env)
{
	return (unsigned) read_long(env);
}

static void expect_list_begin(io_env_t *env)
{
	skip_ws(env);
	if (env->c != '[') {
		parse_error(env, "Expected list, got '%c'\n", env->c);
		exit(1);
	}
	read_c(env);
}

static bool list_has_next(io_env_t *env)
{
	if (feof(env->file)) {
		parse_error(env, "Unexpected EOF while reading list");
		exit(1);
	}
	skip_ws(env);
	if (env->c == ']') {
		read_c(env);
		return false;
	}

	return true;
}

static ir_node *get_node_or_null(io_env_t *env, long nodenr)
{
	ir_node *node = (ir_node *) get_id(env, nodenr);
	if (node && node->kind != k_ir_node) {
		panic("Irn ID %ld collides with something else in line %d\n",
		      nodenr, env->line);
	}
	return node;
}

static ir_node *get_node_or_dummy(io_env_t *env, long nodenr)
{
	ir_node *node = get_node_or_null(env, nodenr);
	if (node == NULL) {
		node = new_r_Dummy(env->irg, mode_X);
		set_id(env, nodenr, node);
	}
	return node;
}

static ir_type *get_type(io_env_t *env, long typenr)
{
	ir_type *type = (ir_type *) get_id(env, typenr);
	if (type == NULL)
		panic("unknown type: %ld in line %d\n", typenr, env->line);
	else if (type->kind != k_type)
		panic("type ID %ld collides with something else in line %d\n",
		      typenr, env->line);
	return type;
}

static ir_type *read_type(io_env_t *env)
{
	return get_type(env, read_long(env));
}

static ir_entity *get_entity(io_env_t *env, long entnr)
{
	ir_entity *entity = (ir_entity *) get_id(env, entnr);
	if (entity == NULL) {
		parse_error(env, "unknown entity: %ld\n", entnr);
		exit(1);
	} else if (entity->kind != k_entity) {
		panic("Entity ID %ld collides with something else in line %d\n",
		      entnr, env->line);
	}

	return entity;
}

static ir_entity *read_entity(io_env_t *env)
{
	return get_entity(env, read_long(env));
}

static ir_mode *read_mode(io_env_t *env)
{
	char *str = read_word(env);
	int i, n;

	n = get_irp_n_modes();
	for (i = 0; i < n; i++) {
		ir_mode *mode = get_irp_mode(i);
		if (strcmp(str, get_mode_name(mode)) == 0) {
			obstack_free(&env->obst, str);
			return mode;
		}
	}

	parse_error(env, "unknown mode \"%s\"\n", str);
	exit(1);
}

static const char *get_typetag_name(typetag_t typetag)
{
	switch (typetag) {
	case tt_align:              return "align";
	case tt_allocation:         return "allocation";
	case tt_builtin:            return "builtin kind";
	case tt_cond_jmp_predicate: return "cond_jmp_predicate";
	case tt_initializer:        return "initializer kind";
	case tt_iro:                return "opcode";
	case tt_keyword:            return "keyword";
	case tt_linkage:            return "linkage";
	case tt_mode_arithmetic:    return "mode_arithmetic";
	case tt_mode_sort:          return "mode_sort";
	case tt_pin_state:          return "pin state";
	case tt_segment:            return "segment";
	case tt_tpo:                return "type";
	case tt_type_state:         return "type state";
	case tt_volatility:         return "volatility";
	case tt_visibility:         return "visibility";
	}
	return "<UNKNOWN>";
}

/**
 * Read and decode an enum constant.
 */
static unsigned read_enum(io_env_t *env, typetag_t typetag)
{
	char     *str  = read_word(env);
	unsigned  code = symbol(str, typetag);

	if (code != SYMERROR) {
		obstack_free(&env->obst, str);
		return code;
	}

	parse_error(env, "invalid %s: \"%s\"\n", get_typetag_name(typetag), str);
	return 0;
}

#define read_align(env)              ((ir_align)              read_enum(env, tt_align))
#define read_allocation(env)         ((ir_allocation)         read_enum(env, tt_allocation))
#define read_builtin_kind(env)       ((ir_builtin_kind)       read_enum(env, tt_builtin))
#define read_cond_jmp_predicate(env) ((cond_jmp_predicate)    read_enum(env, tt_cond_jmp_predicate))
#define read_initializer_kind(env)   ((ir_initializer_kind_t) read_enum(env, tt_initializer))
#define read_mode_arithmetic(env)    ((ir_mode_arithmetic)    read_enum(env, tt_mode_arithmetic))
#define read_peculiarity(env)        ((ir_peculiarity)        read_enum(env, tt_peculiarity))
#define read_pin_state(env)          ((op_pin_state)          read_enum(env, tt_pin_state))
#define read_type_state(env)         ((ir_type_state)         read_enum(env, tt_type_state))
#define read_variability(env)        ((ir_variability)        read_enum(env, tt_variability))
#define read_volatility(env)         ((ir_volatility)         read_enum(env, tt_volatility))

static ir_cons_flags get_cons_flags(io_env_t *env)
{
	ir_cons_flags flags = cons_none;

	op_pin_state pinstate = read_pin_state(env);
	switch (pinstate) {
	case op_pin_state_floats: flags |= cons_floats; break;
	case op_pin_state_pinned: break;
	default:
		panic("Error in %d: Invalid pinstate: %s", env->line,
		      get_op_pin_state_name(pinstate));
	}

	if (read_volatility(env) == volatility_is_volatile)
		flags |= cons_volatile;
	if (read_align(env) == align_non_aligned)
		flags |= cons_unaligned;

	return flags;
}

static ir_tarval *read_tv(io_env_t *env)
{
	ir_mode   *tvmode = read_mode(env);
	char      *str    = read_word(env);
	ir_tarval *tv     = new_tarval_from_str(str, strlen(str), tvmode);
	obstack_free(&env->obst, str);

	return tv;
}

static ir_initializer_t *read_initializer(io_env_t *env)
{
	ir_initializer_kind_t ini_kind = read_initializer_kind(env);

	switch (ini_kind) {
	case IR_INITIALIZER_CONST: {
		ir_node *irn = get_node_or_dummy(env, read_long(env));
		return create_initializer_const(irn);
	}

	case IR_INITIALIZER_TARVAL:
		return create_initializer_tarval(read_tv(env));

	case IR_INITIALIZER_NULL:
		return get_initializer_null();

	case IR_INITIALIZER_COMPOUND: {
		unsigned i, n = (unsigned) read_long(env);
		ir_initializer_t *ini = create_initializer_compound(n);
		for (i = 0; i < n; i++) {
			ir_initializer_t *curini = read_initializer(env);
			set_initializer_compound_value(ini, i, curini);
		}
		return ini;
	}

	default:
		panic("Unknown initializer kind");
	}
}


/** Reads a type description and remembers it by its id. */
static void import_type(io_env_t *env)
{
	int            i;
	ir_type       *type;
	long           typenr = read_long(env);
	tp_opcode      tpop   = (tp_opcode) read_enum(env, tt_tpo);
	unsigned       size   = (unsigned) read_long(env);
	unsigned       align  = (unsigned) read_long(env);
	ir_type_state  state  = read_type_state(env);
	unsigned       flags  = (unsigned) read_long(env);

	switch (tpop) {
	case tpo_array: {
		int ndims = (int) read_long(env);
		long elemtypenr = read_long(env);
		ir_type *elemtype = get_type(env, elemtypenr);

		type = new_type_array(ndims, elemtype);
		for (i = 0; i < ndims; i++) {
			char *str = read_word(env);
			if (strcmp(str, "unknown") != 0) {
				long lowerbound = atol(str);
				set_array_lower_bound_int(type, i, lowerbound);
			}
			obstack_free(&env->obst, str);

			str = read_word(env);
			if (strcmp(str, "unknown") != 0) {
				long upperbound = atol(str);
				set_array_upper_bound_int(type, i, upperbound);
			}
			obstack_free(&env->obst, str);
		}
		set_type_size_bytes(type, size);
		break;
	}

	case tpo_class: {
		ident *id = read_ident_null(env);

		if (typenr == (long) IR_SEGMENT_GLOBAL)
			type = get_glob_type();
		else
			type = new_type_class(id);
		set_type_size_bytes(type, size);
		break;
	}

	case tpo_method: {
		unsigned                  callingconv = (unsigned) read_long(env);
		mtp_additional_properties addprops    = (mtp_additional_properties) read_long(env);
		int nparams          = (int)      read_long(env);
		int nresults         = (int)      read_long(env);
		int variaindex;

		type = new_type_method(nparams, nresults);

		for (i = 0; i < nparams; i++) {
			long     typenr = read_long(env);
			ir_type *paramtype = get_type(env, typenr);

			set_method_param_type(type, i, paramtype);
		}
		for (i = 0; i < nresults; i++) {
			long typenr = read_long(env);
			ir_type *restype = get_type(env, typenr);

			set_method_res_type(type, i, restype);
		}

		variaindex = (int) read_long(env);
		if (variaindex != -1) {
			set_method_variadicity(type, variadicity_variadic);
			if (variaindex != nparams)
				set_method_first_variadic_param_index(type, variaindex);
		}

		set_method_calling_convention(type, callingconv);
		set_method_additional_properties(type, addprops);
		break;
	}

	case tpo_pointer: {
		ir_mode *mode     = read_mode(env);
		ir_type *pointsto = get_type(env, read_long(env));
		type = new_type_pointer(pointsto);
		set_type_mode(type, mode);
		break;
	}

	case tpo_primitive: {
		ir_mode *mode = read_mode(env);
		type = new_type_primitive(mode);
		break;
	}

	case tpo_struct: {
		ident *id = read_ident_null(env);
		type = new_type_struct(id);
		set_type_size_bytes(type, size);
		break;
	}

	case tpo_union: {
		ident *id = read_ident_null(env);
		type = new_type_union(id);
		set_type_size_bytes(type, size);
		break;
	}

	case tpo_unknown:
		return;   /* ignore unknown type */

	default:
		parse_error(env, "unknown type kind: \"%d\"\n", tpop);
		skip_to(env, '\n');
		return;
	}

	set_type_alignment_bytes(type, align);
	type->flags = flags;

	if (state == layout_fixed)
		ARR_APP1(ir_type *, env->fixedtypes, type);

	set_id(env, typenr, type);
}

/** Reads an entity description and remembers it by its id. */
static void import_entity(io_env_t *env)
{
	long           entnr      = read_long(env);
	ident         *name       = read_ident(env);
	ident         *ld_name    = read_ident_null(env);
	ir_visibility  visibility = ir_visibility_default;
	ir_linkage     linkage    = IR_LINKAGE_DEFAULT;
	long           typenr;
	long           ownertypenr;
	const char    *str;
	ir_type       *type;
	ir_type       *ownertype;
	ir_entity     *entity;

	skip_ws(env);
	while (!isdigit(env->c)) {
		char     *str = read_word(env);
		unsigned  v;

		skip_ws(env);

		v = symbol(str, tt_visibility);
		if (v != SYMERROR) {
			visibility = (ir_visibility)v;
			continue;
		}
		v = symbol(str, tt_linkage);
		if (v != SYMERROR) {
			linkage |= (ir_linkage)v;
			continue;
		}
		printf("Parser error, expected visibility or linkage, got '%s'\n",
		       str);
		break;
	}

	typenr      = read_long(env);
	ownertypenr = read_long(env);

	type      = get_type(env, typenr);
	ownertype = !ownertypenr ? get_glob_type() : get_type(env, ownertypenr);
	entity    = new_entity(ownertype, name, type);

	if (ld_name != NULL)
		set_entity_ld_ident(entity, ld_name);
	set_entity_offset(entity, (int) read_long(env));
	set_entity_offset_bits_remainder(entity, (unsigned char) read_long(env));
	set_entity_compiler_generated(entity, (int) read_long(env));
	set_entity_volatility(entity, read_volatility(env));
	set_entity_visibility(entity, visibility);
	set_entity_linkage(entity, linkage);

	str = read_word(env);
	if (strcmp(str, "initializer") == 0) {
		set_entity_initializer(entity, read_initializer(env));
	} else if (strcmp(str, "compoundgraph") == 0) {
		int n = (int) read_long(env);
		int i;
		for (i = 0; i < n; i++) {
			ir_entity *member = get_entity(env, read_long(env));
			ir_node   *irn    = get_node_or_dummy(env, read_long(env));
			add_compound_ent_value(entity, irn, member);
		}
	} else if (strcmp(str, "none") == 0) {
		/* do nothing */
	} else {
		parse_error(env, "expected 'initializer', 'compoundgraph' or 'none' got '%s'\n", str);
		exit(1);
	}

	set_id(env, entnr, entity);
}

/** Parses the whole type graph. */
static int parse_typegraph(io_env_t *env)
{
	ir_graph *old_irg = env->irg;
	keyword_t kwkind;

	EXPECT('{');

	env->irg = get_const_code_irg();

	/* parse all types first */
	while (true) {
		skip_ws(env);
		if (env->c == '}') {
			read_c(env);
			break;
		}

		kwkind = (keyword_t) read_enum(env, tt_keyword);
		switch (kwkind) {
		case kw_type:
			import_type(env);
			break;

		case kw_entity:
			import_entity(env);
			break;

		default:
			parse_error(env, "type graph element not supported yet: %d\n", kwkind);
			skip_to(env, '\n');
			break;
		}
	}
	env->irg = old_irg;
	return 1;
}

static int read_node_header(io_env_t *env, long *nodenr, ir_node ***preds,
                            const char **nodename)
{
	int numpreds;

	*nodename = read_word(env);
	*nodenr   = read_long(env);

	ARR_RESIZE(ir_node*, *preds, 0);

	expect_list_begin(env);
	for (numpreds = 0; list_has_next(env); numpreds++) {
		long     val  = read_long(env);
		ir_node *pred = get_node_or_dummy(env, val);
		ARR_APP1(ir_node*, *preds, pred);
	}

	return numpreds;
}

/** Parses an IRG. */
static int parse_graph(io_env_t *env, ir_graph *irg)
{
	ir_node   **preds = NEW_ARR_F(ir_node*,0);
	int         i, numpreds, ret = 1;
	long        nodenr;
	const char *nodename;
	ir_node    *node, *newnode;

	env->irg = irg;

	EXPECT('{');

	while (true) {
		skip_ws(env);
		if (env->c == '}') {
			read_c(env);
			break;
		}

		numpreds = read_node_header(env, &nodenr, &preds, &nodename);

		node = get_node_or_null(env, nodenr);
		newnode = NULL;

		EXPECT('{');

		switch (symbol(nodename, tt_iro)) {
		case iro_End: {
			ir_node *newendblock = preds[0];
			newnode = get_irg_end(irg);
			exchange(get_nodes_block(newnode), newendblock);
			for (i = 1; i < numpreds; i++)
				add_irn_n(newnode, preds[i]);
			break;
		}

		case iro_Start: {
			ir_node *newstartblock = preds[0];
			newnode = get_irg_start(irg);
			exchange(get_nodes_block(newnode), newstartblock);
			break;
		}

		case iro_Block:
			newnode = new_r_Block(irg, numpreds, preds);
			break;

		case iro_Anchor:
			newnode = irg->anchor;
			for (i = 1; i < numpreds; i++)
				set_irn_n(newnode, i-1, preds[i]);
			set_nodes_block(newnode, preds[0]);
			break;

		case iro_SymConst: {
			long entnr = read_long(env);
			union symconst_symbol sym;
			sym.entity_p = get_entity(env, entnr);
			newnode = new_r_SymConst(irg, mode_P, sym, symconst_addr_ent);
			break;
		}

		case iro_Proj: {
			ir_mode *mode = read_mode(env);
			long     pn   = read_long(env);
			newnode = new_r_Proj(preds[1], mode, pn);
			/* explicitely set block, since preds[1] might be a dummy node
			 * which is always in the startblock */
			set_nodes_block(newnode, preds[0]);
			break;
		}

		#include "gen_irio_import.inl"

		default:
			goto notsupported;
		}

		EXPECT('}');

		if (!newnode) {
notsupported:
			parse_error(env, "node type not supported yet: %s\n", nodename);
			abort();
		}

		if (node)
			exchange(node, newnode);
		/* Always update hash entry to avoid more uses of id nodes */
		set_id(env, nodenr, newnode);
	}

	DEL_ARR_F(preds);

	return ret;
}

static int parse_modes(io_env_t *env)
{
	EXPECT('{');

	while (true) {
		keyword_t kwkind;

		skip_ws(env);
		if (env->c == '}') {
			read_c(env);
			break;
		}

		kwkind = (keyword_t) read_enum(env, tt_keyword);
		switch (kwkind) {
		case kw_mode: {
			const char *name = read_string(env);
			ir_mode_sort sort = (ir_mode_sort)read_enum(env, tt_mode_sort);
			int size = read_long(env);
			int sign = read_long(env);
			ir_mode_arithmetic arith = read_mode_arithmetic(env);
			unsigned modulo_shift = read_long(env);
			int vector_elems = read_long(env);
			ir_mode *mode;

			if (vector_elems != 1) {
				panic("no support for import of vector modes yes");
			}

			mode = new_ir_mode(name, sort, size, sign, arith, modulo_shift);
			if (mode_is_reference(mode)) {
				set_reference_mode_signed_eq(mode, read_mode(env));
				set_reference_mode_unsigned_eq(mode, read_mode(env));
			}
			break;
		}

		default:
			skip_to(env, '\n');
			break;
		}
	}
	return 1;
}

static int parse_program(io_env_t *env)
{
	EXPECT('{');

	while (true) {
		keyword_t kwkind;

		skip_ws(env);
		if (env->c == '}') {
			read_c(env);
			break;
		}

		kwkind = (keyword_t) read_enum(env, tt_keyword);
		switch (kwkind) {
		case kw_segment_type: {
			ir_segment_t  segment = (ir_segment_t) read_enum(env, tt_segment);
			ir_type      *type    = read_type(env);
			set_segment_type(segment, type);
			break;
		}
		default:
			parse_error(env, "unexpected keyword %d\n", kwkind);
			skip_to(env, '\n');
		}
	}
	return 1;
}

void ir_import(const char *filename)
{
	FILE *file = fopen(filename, "rt");
	if (file == NULL) {
		perror(filename);
		exit(1);
	}

	ir_import_file(file, filename);

	fclose(file);
}

void ir_import_file(FILE *input, const char *inputname)
{
	int oldoptimize = get_optimize();
	firm_verification_t oldver = get_node_verification_mode();
	io_env_t ioenv;
	io_env_t *env = &ioenv;
	int i, n;

	symtbl_init();

	memset(env, 0, sizeof(*env));
	obstack_init(&env->obst);
	env->idset      = new_set(id_cmp, 128);
	env->fixedtypes = NEW_ARR_F(ir_type *, 0);
	env->inputname  = inputname;
	env->file       = input;
	env->line       = 1;

	/* read first character */
	read_c(env);

	set_optimize(0);
	do_node_verification(FIRM_VERIFICATION_OFF);

	while (true) {
		keyword_t kw;

		skip_ws(env);
		if (env->c == EOF)
			break;

		kw = (keyword_t)read_enum(env, tt_keyword);
		switch (kw) {
		case kw_modes:
			if (!parse_modes(env)) goto end;
			break;

		case kw_typegraph:
			if (!parse_typegraph(env)) goto end;
			break;

		case kw_irg:
		{
			ir_entity *irgent = get_entity(env, read_long(env));
			long valuetypeid;
			ir_graph *irg = new_ir_graph(irgent, 0);
			set_irg_frame_type(irg, get_type(env, read_long(env)));
			valuetypeid = read_long(env);
			if (valuetypeid != -1)
				set_method_value_param_type(get_entity_type(irgent),
						get_type(env, valuetypeid));

			if (!parse_graph(env, irg)) goto end;
			break;
		}

		case kw_constirg: {
			ir_graph *constirg = get_const_code_irg();
			long bodyblockid = read_long(env);
			set_id(env, bodyblockid, constirg->current_block);
			if (!parse_graph(env, constirg)) goto end;
			break;
		}

		case kw_program:
			parse_program(env);
			break;

		default: {
			parse_error(env, "Unexpected keyword %d at toplevel\n", kw);
			exit(1);
		}
		}
	}

end:
	n = ARR_LEN(env->fixedtypes);
	for (i = 0; i < n; i++)
		set_type_state(env->fixedtypes[i], layout_fixed);

	DEL_ARR_F(env->fixedtypes);

	del_set(env->idset);

	irp_finalize_cons();

	do_node_verification(oldver);
	set_optimize(oldoptimize);

	obstack_free(&env->obst, NULL);
}
