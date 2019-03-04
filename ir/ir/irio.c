/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Write textual representation of firm to file.
 * @author  Moritz Kroll, Matthias Braun
 */
#include "irio_t.h"

#include "array.h"
#include "ircons_t.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irprintf.h"
#include "irprog_t.h"
#include "obst.h"
#include "panic.h"
#include "pmap.h"
#include "tv_t.h"
#include "util.h"
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>

#define SYMERROR ((unsigned) ~0)

typedef enum typetag_t {
	tt_align,
	tt_builtin_kind,
	tt_cond_jmp_predicate,
	tt_initializer,
	tt_keyword,
	tt_linkage,
	tt_loop,
	tt_mode_arithmetic,
	tt_pin_state,
	tt_segment,
	tt_throws,
	tt_tpo,
	tt_type_state,
	tt_visibility,
	tt_volatility,
} typetag_t;

typedef enum keyword_t {
	kw_asm,
	kw_compound_member,
	kw_constirg,
	kw_entity,
	kw_float_mode,
	kw_int_mode,
	kw_irg,
	kw_alias,
	kw_gotentry,
	kw_label,
	kw_method,
	kw_modes,
	kw_parameter,
	kw_program,
	kw_reference_mode,
	kw_segment_type,
	kw_type,
	kw_typegraph,
	kw_unknown,
} keyword_t;

typedef struct symbol_t {
	const char *str;      /**< The name of this symbol. */
	typetag_t   typetag;  /**< The type tag of this symbol. */
	unsigned    code;     /**< The value of this symbol. */
} symbol_t;

typedef struct id_entry {
	long id;
	void *elem;
} id_entry;

/** The symbol table, a set of symbol_t elements. */
static set *symtbl;

/**
 * The number of types prior to calling ir_import.
 *
 * These were added by e.g. ir_init() and we have
 * to go through these when we add a new type so
 * that we don't accidentally add duplicates, leading
 * to a bloated registry.
 */
static int n_initial_types;

/**
 * True, if we may still be looking at one of the initial types common
 * to every program.
 *
 * As soon as the types diverge (because exporter and importer have
 * initialized FIRM differently), we set this to false and stop
 * deduplicating.
 */
static bool maybe_initial_type;

/**
 * Compare two symbol table entries.
 */
static int symbol_cmp(const void *elt, const void *key, size_t size)
{
	(void)size;
	const symbol_t *entry = (const symbol_t *) elt;
	const symbol_t *keyentry = (const symbol_t *) key;
	int res = entry->typetag - keyentry->typetag;
	if (res != 0)
		return res;
	return strcmp(entry->str, keyentry->str);
}

static int id_cmp(const void *elt, const void *key, size_t size)
{
	(void)size;
	const id_entry *entry = (const id_entry *) elt;
	const id_entry *keyentry = (const id_entry *) key;
	return entry->id - keyentry->id;
}

static void FIRM_PRINTF(2, 3)
parse_error(read_env_t *env, const char *fmt, ...)
{
	/* workaround read_c "feature" that a '\n' triggers the line++
	 * instead of the character after the '\n' */
	unsigned line = env->line;
	if (env->c == '\n') {
		line--;
	}

	fprintf(stderr, "%s:%u: error ", env->inputname, line);
	env->read_errors = true;

	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
}

COMPILETIME_ASSERT(ir_bk_va_arg == ir_bk_last, complete_builtin_list)

/** Initializes the symbol table. May be called more than once without problems. */
static void symtbl_init(void)
{
	/* Only initialize once */
	if (symtbl != NULL)
		return;

	symtbl = new_set(symbol_cmp, 256);

	symbol_t key;
#define INSERT(tt, s, cod)                                       \
	key.str = (s);                                               \
	key.typetag = (tt);                                          \
	key.code = (cod);                                            \
	(void)set_insert(symbol_t, symtbl, &key, sizeof(key), hash_str(s) + tt * 17)

#define INSERTENUM(tt, e) INSERT(tt, #e, e)
#define INSERTKEYWORD(k) INSERT(tt_keyword, #k, kw_##k)

	INSERT(tt_tpo, "array", tpo_array);
	INSERT(tt_tpo, "class", tpo_class);
	INSERT(tt_tpo, "method", tpo_method);
	INSERT(tt_tpo, "pointer", tpo_pointer);
	INSERT(tt_tpo, "primitive", tpo_primitive);
	INSERT(tt_tpo, "segment", tpo_segment);
	INSERT(tt_tpo, "struct", tpo_struct);
	INSERT(tt_tpo, "union", tpo_union);
	INSERT(tt_tpo, "Unknown", tpo_unknown);

	INSERT(tt_segment, "global", IR_SEGMENT_GLOBAL);
	INSERT(tt_segment, "thread_local", IR_SEGMENT_THREAD_LOCAL);
	INSERT(tt_segment, "constructors", IR_SEGMENT_CONSTRUCTORS);
	INSERT(tt_segment, "destructors", IR_SEGMENT_DESTRUCTORS);
	INSERT(tt_segment, "jcr", IR_SEGMENT_JCR);

	INSERT(tt_linkage, "constant", IR_LINKAGE_CONSTANT);
	INSERT(tt_linkage, "weak", IR_LINKAGE_WEAK);
	INSERT(tt_linkage, "garbage_collect", IR_LINKAGE_GARBAGE_COLLECT);
	INSERT(tt_linkage, "merge", IR_LINKAGE_MERGE);
	INSERT(tt_linkage, "hidden_user", IR_LINKAGE_HIDDEN_USER);

	INSERT(tt_loop, "loop",   true);
	INSERT(tt_loop, "noloop", false);

	INSERT(tt_visibility, "external",           ir_visibility_external);
	INSERT(tt_visibility, "external_private",   ir_visibility_external_private);
	INSERT(tt_visibility, "external_protected", ir_visibility_external_protected);
	INSERT(tt_visibility, "local",              ir_visibility_local);
	INSERT(tt_visibility, "private",            ir_visibility_private);

	INSERT(tt_throws, "throw",   true);
	INSERT(tt_throws, "nothrow", false);

	INSERTKEYWORD(alias);
	INSERTKEYWORD(asm);
	INSERTKEYWORD(compound_member);
	INSERTKEYWORD(constirg);
	INSERTKEYWORD(entity);
	INSERTKEYWORD(float_mode);
	INSERTKEYWORD(gotentry);
	INSERTKEYWORD(int_mode);
	INSERTKEYWORD(irg);
	INSERTKEYWORD(label);
	INSERTKEYWORD(method);
	INSERTKEYWORD(modes);
	INSERTKEYWORD(parameter);
	INSERTKEYWORD(program);
	INSERTKEYWORD(reference_mode);
	INSERTKEYWORD(segment_type);
	INSERTKEYWORD(type);
	INSERTKEYWORD(typegraph);
	INSERTKEYWORD(unknown);

	INSERTENUM(tt_align, align_non_aligned);
	INSERTENUM(tt_align, align_is_aligned);

	INSERTENUM(tt_builtin_kind, ir_bk_trap);
	INSERTENUM(tt_builtin_kind, ir_bk_debugbreak);
	INSERTENUM(tt_builtin_kind, ir_bk_return_address);
	INSERTENUM(tt_builtin_kind, ir_bk_frame_address);
	INSERTENUM(tt_builtin_kind, ir_bk_prefetch);
	INSERTENUM(tt_builtin_kind, ir_bk_ffs);
	INSERTENUM(tt_builtin_kind, ir_bk_clz);
	INSERTENUM(tt_builtin_kind, ir_bk_ctz);
	INSERTENUM(tt_builtin_kind, ir_bk_popcount);
	INSERTENUM(tt_builtin_kind, ir_bk_parity);
	INSERTENUM(tt_builtin_kind, ir_bk_bswap);
	INSERTENUM(tt_builtin_kind, ir_bk_inport);
	INSERTENUM(tt_builtin_kind, ir_bk_outport);
	INSERTENUM(tt_builtin_kind, ir_bk_saturating_increment);
	INSERTENUM(tt_builtin_kind, ir_bk_compare_swap);
	INSERTENUM(tt_builtin_kind, ir_bk_may_alias);
	INSERTENUM(tt_builtin_kind, ir_bk_va_start);
	INSERTENUM(tt_builtin_kind, ir_bk_va_arg);

	INSERTENUM(tt_cond_jmp_predicate, COND_JMP_PRED_NONE);
	INSERTENUM(tt_cond_jmp_predicate, COND_JMP_PRED_TRUE);
	INSERTENUM(tt_cond_jmp_predicate, COND_JMP_PRED_FALSE);

	INSERTENUM(tt_initializer, IR_INITIALIZER_CONST);
	INSERTENUM(tt_initializer, IR_INITIALIZER_TARVAL);
	INSERTENUM(tt_initializer, IR_INITIALIZER_NULL);
	INSERTENUM(tt_initializer, IR_INITIALIZER_COMPOUND);

	INSERT(tt_mode_arithmetic, "none",               irma_none);
	INSERT(tt_mode_arithmetic, "twos_complement",    irma_twos_complement);
	INSERT(tt_mode_arithmetic, "ieee754",            irma_ieee754);
	INSERT(tt_mode_arithmetic, "x86_extended_float", irma_x86_extended_float);

	INSERTENUM(tt_pin_state, op_pin_state_floats);
	INSERTENUM(tt_pin_state, op_pin_state_pinned);
	INSERTENUM(tt_pin_state, op_pin_state_exc_pinned);

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
	case IR_SEGMENT_JCR:          return "jcr";
	}
	panic("invalid segment");
}

static const char *get_visibility_name(ir_visibility visibility)
{
	switch (visibility) {
	case ir_visibility_external:           return "external";
	case ir_visibility_external_private:   return "external_private";
	case ir_visibility_external_protected: return "external_protected";
	case ir_visibility_local:              return "local";
	case ir_visibility_private:            return "private";
	}
	panic("invalid visibility");
}

static const char *get_mode_arithmetic_name(ir_mode_arithmetic arithmetic)
{
	switch (arithmetic) {
	case irma_none:               return "none";
	case irma_twos_complement:    return "twos_complement";
	case irma_ieee754:            return "ieee754";
	case irma_x86_extended_float: return "x86_extended_float";
	}
	panic("invalid mode_arithmetic");
}

/** Returns the according symbol value for the given string and tag, or SYMERROR if none was found. */
static unsigned symbol(const char *str, typetag_t typetag)
{
	symbol_t key;
	key.str     = str;
	key.typetag = typetag;

	symbol_t *entry = set_find(symbol_t, symtbl, &key, sizeof(key),
	                           hash_str(str) + typetag * 17);
	return entry ? entry->code : SYMERROR;
}

void write_long(write_env_t *env, long value)
{
	fprintf(env->file, "%ld ", value);
}

void write_int(write_env_t *env, int value)
{
	fprintf(env->file, "%d ", value);
}

void write_unsigned(write_env_t *env, unsigned value)
{
	fprintf(env->file, "%u ", value);
}

void write_size_t(write_env_t *env, size_t value)
{
	ir_fprintf(env->file, "%zu ", value);
}

void write_symbol(write_env_t *env, const char *symbol)
{
	fputs(symbol, env->file);
	fputc(' ', env->file);
}

void write_entity_ref(write_env_t *env, ir_entity *entity)
{
	write_long(env, get_entity_nr(entity));
}

void write_type_ref(write_env_t *env, ir_type *type)
{
	switch (get_type_opcode(type)) {
	case tpo_unknown:
		write_symbol(env, "unknown");
		return;
	case tpo_code:
		write_symbol(env, "code");
		return;
	default:
		break;
	}
	write_long(env, get_type_nr(type));
}

void write_string(write_env_t *env, const char *string)
{
	fputc('"', env->file);
	for (const char *c = string; *c != '\0'; ++c) {
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
	fputc(' ', env->file);
}

void write_ident(write_env_t *env, ident *id)
{
	write_string(env, get_id_str(id));
}

void write_ident_null(write_env_t *env, ident *id)
{
	if (id == NULL) {
		fputs("NULL ", env->file);
	} else {
		write_ident(env, id);
	}
}

void write_mode_ref(write_env_t *env, ir_mode *mode)
{
	write_string(env, get_mode_name(mode));
}

void write_tarval_ref(write_env_t *env, ir_tarval *tv)
{
	ir_mode *mode = get_tarval_mode(tv);
	write_mode_ref(env, mode);
	char buf[128];
	const char *ascii = ir_tarval_to_ascii(buf, sizeof(buf), tv);
	fputs(ascii, env->file);
	fputc(' ', env->file);
}

void write_align(write_env_t *env, ir_align align)
{
	fputs(get_align_name(align), env->file);
	fputc(' ', env->file);
}

void write_builtin_kind(write_env_t *env, ir_builtin_kind kind)
{
	fputs(get_builtin_kind_name(kind), env->file);
	fputc(' ', env->file);
}

void write_cond_jmp_predicate(write_env_t *env, cond_jmp_predicate pred)
{
	fputs(get_cond_jmp_predicate_name(pred), env->file);
	fputc(' ', env->file);
}

void write_relation(write_env_t *env, ir_relation relation)
{
	write_long(env, (long)relation);
}

void write_throws(write_env_t *env, bool throws)
{
	write_symbol(env, throws ? "throw" : "nothrow");
}

void write_loop(write_env_t *env, bool loop)
{
	write_symbol(env, loop ? "loop" : "noloop");
}

static void write_list_begin(write_env_t *env)
{
	fputs("[", env->file);
}

static void write_list_end(write_env_t *env)
{
	fputs("] ", env->file);
}

static void write_scope_begin(write_env_t *env)
{
	fputs("{\n", env->file);
}

static void write_scope_end(write_env_t *env)
{
	fputs("}\n\n", env->file);
}

void write_node_ref(write_env_t *env, const ir_node *node)
{
	write_long(env, get_irn_node_nr(node));
}

void write_initializer(write_env_t *const env,
                       ir_initializer_t const *const ini)
{
	FILE *f = env->file;
	ir_initializer_kind_t ini_kind = get_initializer_kind(ini);

	fputs(get_initializer_kind_name(ini_kind), f);
	fputc(' ', f);

	switch (ini_kind) {
	case IR_INITIALIZER_CONST:
		write_node_ref(env, get_initializer_const_value(ini));
		return;

	case IR_INITIALIZER_TARVAL:
		write_tarval_ref(env, get_initializer_tarval_value(ini));
		return;

	case IR_INITIALIZER_NULL:
		return;

	case IR_INITIALIZER_COMPOUND: {
		size_t n = get_initializer_compound_n_entries(ini);
		write_size_t(env, n);
		for (size_t i = 0; i < n; ++i)
			write_initializer(env, get_initializer_compound_value(ini, i));
		return;
	}
	}
	panic("unknown initializer kind");
}

void write_pin_state(write_env_t *env, op_pin_state state)
{
	fputs(get_op_pin_state_name(state), env->file);
	fputc(' ', env->file);
}

void write_volatility(write_env_t *env, ir_volatility vol)
{
	fputs(get_volatility_name(vol), env->file);
	fputc(' ', env->file);
}

static void write_type_state(write_env_t *env, ir_type_state state)
{
	fputs(get_type_state_name(state), env->file);
	fputc(' ', env->file);
}

void write_visibility(write_env_t *env, ir_visibility visibility)
{
	fputs(get_visibility_name(visibility), env->file);
	fputc(' ', env->file);
}

static void write_mode_arithmetic(write_env_t *env, ir_mode_arithmetic arithmetic)
{
	fputs(get_mode_arithmetic_name(arithmetic), env->file);
	fputc(' ', env->file);
}

static void write_type_common(write_env_t *env, ir_type *tp)
{
	fputc('\t', env->file);
	write_symbol(env, "type");
	write_long(env, get_type_nr(tp));
	write_symbol(env, get_type_opcode_name(get_type_opcode(tp)));
	write_unsigned(env, get_type_size(tp));
	write_unsigned(env, get_type_alignment(tp));
	write_type_state(env, get_type_state(tp));
	write_unsigned(env, tp->flags);
}

static void write_type(write_env_t *env, ir_type *tp);

static bool is_internal_mode(ir_mode *mode)
{
	return !mode_is_int(mode) && !mode_is_reference(mode)
	    && !mode_is_float(mode);
}

static bool is_default_mode(ir_mode *mode)
{
	/* some modes which are always available in libfirm */
	return mode == mode_b || mode == mode_X || mode == mode_BB
	    || mode == mode_T || mode == mode_ANY || mode == mode_BAD;
}

static void write_type_primitive(write_env_t *env, ir_type *tp)
{
	/* skip types for internal modes */
	ir_mode *mode = get_type_mode(tp);
	if (is_internal_mode(mode) && !is_default_mode(mode))
		return;

	write_type_common(env, tp);
	write_mode_ref(env, mode);
	fputc('\n', env->file);
}

static void write_type_compound(write_env_t *env, ir_type *tp)
{
	if (is_Class_type(tp)) {
		if (get_class_n_subtypes(tp) > 0 || get_class_n_supertypes(tp) > 0) {
			/* sub/superclass export not implemented yet, it's unclear whether
			 * class types will stay in libfirm anyway */
			panic("can't export class types yet");
		}
	}
	write_type_common(env, tp);
	write_ident_null(env, get_compound_ident(tp));
	fputc('\n', env->file);

	for (size_t i = 0, n = get_compound_n_members(tp); i < n; ++i) {
		ir_entity *member = get_compound_member(tp, i);
		deq_push_pointer_right(&env->entity_queue, member);
	}
}

static void write_type_array(write_env_t *env, ir_type *tp)
{
	ir_type *element_type = get_array_element_type(tp);
	write_type(env, element_type);

	write_type_common(env, tp);
	write_type_ref(env, element_type);
	write_unsigned(env, get_array_size(tp));
	fputc('\n', env->file);
}

static void write_type_method(write_env_t *env, ir_type *tp)
{
	size_t nparams  = get_method_n_params(tp);
	for (size_t i = 0; i < nparams; i++)
		write_type(env, get_method_param_type(tp, i));
	size_t nresults = get_method_n_ress(tp);
	for (size_t i = 0; i < nresults; i++)
		write_type(env, get_method_res_type(tp, i));

	write_type_common(env, tp);
	write_unsigned(env, get_method_calling_convention(tp));
	write_unsigned(env, get_method_additional_properties(tp));
	write_size_t(env, nparams);
	write_size_t(env, nresults);
	write_unsigned(env, is_method_variadic(tp));
	for (size_t i = 0; i < nparams; i++)
		write_type_ref(env, get_method_param_type(tp, i));
	for (size_t i = 0; i < nresults; i++)
		write_type_ref(env, get_method_res_type(tp, i));
	fputc('\n', env->file);
}

static void write_type_pointer(write_env_t *env, ir_type *tp)
{
	ir_type *points_to = get_pointer_points_to_type(tp);

	write_type(env, points_to);

	write_type_common(env, tp);
	write_type_ref(env, points_to);
	fputc('\n', env->file);
}

static void write_type(write_env_t *env, ir_type *tp)
{
	if (type_visited(tp))
		return;
	mark_type_visited(tp);

	switch (get_type_opcode(tp)) {
	case tpo_unknown:
	case tpo_code:
	case tpo_uninitialized:
		/* no need to write special builtin types */
		return;

	case tpo_union:
	case tpo_struct:
	case tpo_class:
	case tpo_segment:
		write_type_compound(env, tp);
		return;

	case tpo_primitive: write_type_primitive(env, tp); return;
	case tpo_method:    write_type_method(env, tp);    return;
	case tpo_pointer:   write_type_pointer(env, tp);   return;
	case tpo_array:     write_type_array(env, tp);     return;
	}
	panic("can't write invalid type %+F", tp);
}

static void write_entity(write_env_t *env, ir_entity *ent)
{
	ir_type       *type       = get_entity_type(ent);
	ir_type       *owner      = get_entity_owner(ent);
	ir_visibility  visibility = get_entity_visibility(ent);
	ir_linkage     linkage    = get_entity_linkage(ent);

	if (entity_visited(ent))
		return;
	mark_entity_visited(ent);

	write_type(env, type);
	write_type(env, owner);
	if (is_alias_entity(ent)) {
		ir_entity *aliased = get_entity_alias(ent);
		write_entity(env, aliased);
	}

	fputc('\t', env->file);
	switch ((ir_entity_kind)ent->kind) {
	case IR_ENTITY_ALIAS:           write_symbol(env, "alias");           break;
	case IR_ENTITY_NORMAL:          write_symbol(env, "entity");          break;
	case IR_ENTITY_METHOD:          write_symbol(env, "method");          break;
	case IR_ENTITY_LABEL:           write_symbol(env, "label");           break;
	case IR_ENTITY_COMPOUND_MEMBER: write_symbol(env, "compound_member"); break;
	case IR_ENTITY_PARAMETER:       write_symbol(env, "parameter");       break;
	case IR_ENTITY_UNKNOWN:
		write_symbol(env, "unknown");
		write_long(env, get_entity_nr(ent));
		goto end_line;
	case IR_ENTITY_SPILLSLOT:
		panic("Unexpected entity %+F", ent); // Should only exist in backend
	}
	write_long(env, get_entity_nr(ent));

	if (ent->kind != IR_ENTITY_LABEL && ent->kind != IR_ENTITY_PARAMETER) {
		write_ident_null(env, get_entity_ident(ent));
		if (!entity_has_ld_ident(ent)) {
			write_ident_null(env, NULL);
		} else {
			write_ident_null(env, get_entity_ld_ident(ent));
		}
	}

	write_visibility(env, visibility);
	write_list_begin(env);
	if (linkage & IR_LINKAGE_CONSTANT)
		write_symbol(env, "constant");
	if (linkage & IR_LINKAGE_WEAK)
		write_symbol(env, "weak");
	if (linkage & IR_LINKAGE_GARBAGE_COLLECT)
		write_symbol(env, "garbage_collect");
	if (linkage & IR_LINKAGE_MERGE)
		write_symbol(env, "merge");
	if (linkage & IR_LINKAGE_HIDDEN_USER)
		write_symbol(env, "hidden_user");
	write_list_end(env);

	write_type_ref(env, type);
	if (ent->kind != IR_ENTITY_LABEL)
		write_type_ref(env, owner);
	write_volatility(env, get_entity_volatility(ent));

	switch ((ir_entity_kind)ent->kind) {
	case IR_ENTITY_ALIAS:
		write_entity_ref(env, get_entity_alias(ent));
		break;

	case IR_ENTITY_NORMAL: {
		ir_initializer_t const *const init = get_entity_initializer(ent);
		if (init) {
			write_symbol(env, "initializer");
			write_initializer(env, init);
		} else {
			write_symbol(env, "none");
		}
		break;
	}

	case IR_ENTITY_COMPOUND_MEMBER:
		write_long(env, get_entity_offset(ent));
		write_unsigned(env, get_entity_bitfield_offset(ent));
		write_unsigned(env, get_entity_bitfield_size(ent));
		break;
	case IR_ENTITY_PARAMETER: {
		size_t num = get_entity_parameter_number(ent);
		if (num == IR_VA_START_PARAMETER_NUMBER) {
			write_symbol(env, "va_start");
		} else {
			write_size_t(env, num);
		}
		write_long(env, get_entity_offset(ent));
		write_unsigned(env, get_entity_bitfield_offset(ent));
		write_unsigned(env, get_entity_bitfield_size(ent));
		break;
	}
	case IR_ENTITY_METHOD:
		write_long(env, (long)get_entity_additional_properties(ent));
		break;
	case IR_ENTITY_UNKNOWN:
	case IR_ENTITY_LABEL:
	case IR_ENTITY_SPILLSLOT:
		break;
	}

end_line:
	fputc('\n', env->file);
}

void write_switch_table_ref(write_env_t *env, const ir_switch_table *table)
{
	size_t n_entries = ir_switch_table_get_n_entries(table);
	write_size_t(env, n_entries);
	for (size_t i = 0; i < n_entries; ++i) {
		long       pn  = ir_switch_table_get_pn(table, i);
		ir_tarval *min = ir_switch_table_get_min(table, i);
		ir_tarval *max = ir_switch_table_get_max(table, i);
		write_long(env, pn);
		write_tarval_ref(env, min);
		write_tarval_ref(env, max);
	}
}

void write_pred_refs(write_env_t *env, const ir_node *node, int from)
{
	write_list_begin(env);
	int arity = get_irn_arity(node);
	assert(from <= arity);
	for (int i = from; i < arity; ++i) {
		ir_node *pred = get_irn_n(node, i);
		write_node_ref(env, pred);
	}
	write_list_end(env);
}

void write_node_nr(write_env_t *env, const ir_node *node)
{
	write_long(env, get_irn_node_nr(node));
}

static void write_ASM(write_env_t *env, const ir_node *node)
{
	write_symbol(env, "ASM");
	write_node_nr(env, node);
	write_node_nr(env, get_nodes_block(node));
	write_node_nr(env, get_ASM_mem(node));

	write_ident(env, get_ASM_text(node));
	write_list_begin(env);
	ir_asm_constraint *const constraints = get_ASM_constraints(node);
	for (int i = 0, n = get_ASM_n_constraints(node); i < n; ++i) {
		ir_asm_constraint const *const constraint = &constraints[i];
		write_int(env, constraint->in_pos);
		write_int(env, constraint->out_pos);
		write_ident(env, constraint->constraint);
		write_mode_ref(env, constraint->mode);
	}
	write_list_end(env);

	write_list_begin(env);
	ident **clobbers   = get_ASM_clobbers(node);
	size_t  n_clobbers = get_ASM_n_clobbers(node);
	for (size_t i = 0; i < n_clobbers; ++i) {
		ident *clobber = clobbers[i];
		write_ident(env, clobber);
	}
	write_list_end(env);

	write_pin_state(env, get_irn_pinned(node));
	write_throws(env, ir_throws_exception(node));
	write_pred_refs(env, node, n_ASM_max+1);
}

static void write_Phi(write_env_t *env, const ir_node *node)
{
	write_symbol(env, "Phi");
	write_node_nr(env, node);
	write_node_ref(env, get_nodes_block(node));
	write_mode_ref(env, get_irn_mode(node));
	write_loop(env, get_Phi_loop(node));
	write_pred_refs(env, node, 0);
}

static void write_Block(write_env_t *env, const ir_node *node)
{
	ir_entity *entity = get_Block_entity(node);

	if (entity != NULL) {
		write_symbol(env, "BlockL");
		write_node_nr(env, node);
		write_entity_ref(env, entity);
	} else {
		write_symbol(env, "Block");
		write_node_nr(env, node);
	}
	write_pred_refs(env, node, 0);
}

static void write_Anchor(write_env_t *env, const ir_node *node)
{
	write_symbol(env, "Anchor");
	write_node_nr(env, node);
	write_pred_refs(env, node, 0);
}

void register_node_writer(ir_op *op, write_node_func *func)
{
	set_generic_function_ptr(op, func);
}

static void writers_init(void)
{
	ir_clear_opcodes_generic_func();
	register_node_writer(op_Anchor, write_Anchor);
	register_node_writer(op_ASM,    write_ASM);
	register_node_writer(op_Block,  write_Block);
	register_node_writer(op_Phi,    write_Phi);
	register_generated_node_writers();
}

static void write_node(const ir_node *node, write_env_t *env)
{
	ir_op           *const op   = get_irn_op(node);
	write_node_func *const func = get_generic_function_ptr(write_node_func, op);

	fputc('\t', env->file);
	if (func == NULL)
		panic("no write_node_func for %+F", node);
	func(env, node);
	fputc('\n', env->file);
}

static void write_node_recursive(ir_node *node, write_env_t *env);

static void write_preds(ir_node *node, write_env_t *env)
{
	foreach_irn_in(node, i, pred) {
		write_node_recursive(pred, env);
	}
}

/**
 * Recursively write nodes.
 * The reader expects nodes in a way that except for block/phi/anchor nodes
 * all predecessors are already defined when we reach them. So usually we
 * recurse to all our predecessors except for block/phi/anchor nodes where
 * we put the predecessors into a queue for later processing.
 */
static void write_node_recursive(ir_node *node, write_env_t *env)
{
	if (irn_visited_else_mark(node))
		return;

	if (!is_Block(node)) {
		write_node_recursive(get_nodes_block(node), env);
	}
	/* write predecessors */
	if (!is_Phi(node) && !is_Block(node) && !is_Anchor(node)) {
		write_preds(node, env);
	} else {
		foreach_irn_in(node, i, pred) {
			deq_push_pointer_right(&env->write_queue, pred);
		}
	}
	write_node(node, env);
}

static void write_mode(write_env_t *env, ir_mode *mode)
{
	if (mode_is_int(mode)) {
		write_symbol(env, "int_mode");
		write_string(env, get_mode_name(mode));
		write_mode_arithmetic(env, get_mode_arithmetic(mode));
		write_unsigned(env, get_mode_size_bits(mode));
		write_int(env, mode_is_signed(mode));
		write_unsigned(env, get_mode_modulo_shift(mode));
	} else if (mode_is_reference(mode)) {
		write_symbol(env, "reference_mode");
		write_string(env, get_mode_name(mode));
		write_mode_arithmetic(env, get_mode_arithmetic(mode));
		write_unsigned(env, get_mode_size_bits(mode));
		write_unsigned(env, get_mode_modulo_shift(mode));

		write_mode_ref(env, get_reference_offset_mode(mode));
		write_int(env, (mode == mode_P ? 1 : 0));
	} else if (mode_is_float(mode)) {
		write_symbol(env, "float_mode");
		write_string(env, get_mode_name(mode));
		write_mode_arithmetic(env, get_mode_arithmetic(mode));
		write_unsigned(env, get_mode_exponent_size(mode));
		write_unsigned(env, get_mode_mantissa_size(mode));
		write_unsigned(env, get_mode_float_int_overflow(mode));
	} else {
		panic("cannot write internal modes");
	}
}

static void write_modes(write_env_t *env)
{
	write_symbol(env, "modes");
	fputs("{\n", env->file);

	for (size_t i = 0, n_modes = ir_get_n_modes(); i < n_modes; i++) {
		ir_mode *mode = ir_get_mode(i);
		if (is_internal_mode(mode))
			continue;
		fputc('\t', env->file);
		write_mode(env, mode);
		fputc('\n', env->file);
	}

	fputs("}\n\n", env->file);
}

static void write_program(write_env_t *env)
{
	write_symbol(env, "program");
	write_scope_begin(env);
	if (irp_prog_name_is_set()) {
		fputc('\t', env->file);
		write_symbol(env, "name");
		write_string(env, get_irp_name());
		fputc('\n', env->file);
	}

	for (ir_segment_t s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s) {
		ir_type *segment_type = get_segment_type(s);
		fputc('\t', env->file);
		write_symbol(env, "segment_type");
		write_symbol(env, get_segment_name(s));
		if (segment_type == NULL) {
			write_symbol(env, "NULL");
		} else {
			write_type_ref(env, segment_type);
		}
		fputc('\n', env->file);
	}

	for (size_t i = 0, n_asms = get_irp_n_asms(); i < n_asms; ++i) {
		ident *asm_text = get_irp_asm(i);
		fputc('\t', env->file);
		write_symbol(env, "asm");
		write_ident(env, asm_text);
		fputc('\n', env->file);
	}
	write_scope_end(env);
}

int ir_export(const char *filename)
{
	FILE *file = fopen(filename, "wt");
	int   res  = 0;
	if (file == NULL) {
		perror(filename);
		return 1;
	}

	ir_export_file(file);
	res = ferror(file);
	fclose(file);
	return res;
}

static void write_node_cb(ir_node *node, void *ctx)
{
	write_env_t *env = (write_env_t*)ctx;
	write_node(node, env);
}

static void write_typegraph(write_env_t *env)
{
	write_symbol(env, "typegraph");
	write_scope_begin(env);
	irp_reserve_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	inc_master_type_visited();
	for (size_t i = 0, n_types = get_irp_n_types(); i < n_types; ++i) {
		ir_type *type = get_irp_type(i);
		write_type(env, type);
	}

	while (!deq_empty(&env->entity_queue)) {
		ir_entity *entity = deq_pop_pointer_left(ir_entity, &env->entity_queue);
		write_entity(env, entity);
	}

	irp_free_resources(irp, IRP_RESOURCE_TYPE_VISITED);
	write_scope_end(env);
}

static void write_irg(write_env_t *env, ir_graph *irg)
{
	write_symbol(env, "irg");
	write_entity_ref(env, get_irg_entity(irg));
	write_type_ref(env, get_irg_frame_type(irg));
	write_scope_begin(env);
	ir_reserve_resources(irg, IR_RESOURCE_IRN_VISITED);
	inc_irg_visited(irg);
	assert(deq_empty(&env->write_queue));
	deq_push_pointer_right(&env->write_queue, irg->anchor);
	do {
		ir_node *node = deq_pop_pointer_left(ir_node, &env->write_queue);
		write_node_recursive(node, env);
	} while (!deq_empty(&env->write_queue));
	ir_free_resources(irg, IR_RESOURCE_IRN_VISITED);
	write_scope_end(env);
}

/* Exports the whole irp to the given file in a textual form. */
void ir_export_file(FILE *file)
{
	write_env_t my_env;
	write_env_t *env = &my_env;

	memset(env, 0, sizeof(*env));
	env->file         = file;
	deq_init(&env->write_queue);
	deq_init(&env->entity_queue);

	writers_init();
	write_modes(env);

	write_typegraph(env);

	foreach_irp_irg(i, irg) {
		write_irg(env, irg);
	}

	write_symbol(env, "constirg");
	write_node_ref(env, get_const_code_irg()->current_block);
	write_scope_begin(env);
	walk_const_code(NULL, write_node_cb, env);
	write_scope_end(env);

	write_program(env);

	deq_free(&env->entity_queue);
	deq_free(&env->write_queue);
}



static void read_c(read_env_t *env)
{
	int c = fgetc(env->file);
	env->c = c;
	if (c == '\n')
		env->line++;
}

/** Returns the first non-whitespace character or EOF. **/
static void skip_ws(read_env_t *env)
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

static void skip_to(read_env_t *env, char to_ch)
{
	while (env->c != to_ch && env->c != EOF) {
		read_c(env);
	}
}

static bool expect_char(read_env_t *env, char ch)
{
	skip_ws(env);
	if (env->c != ch) {
		parse_error(env, "Unexpected char '%c', expected '%c'\n",
		            env->c, ch);
		return false;
	}
	read_c(env);
	return true;
}

#define EXPECT(c) if (expect_char(env, (c))) {} else return

static char *read_word(read_env_t *env)
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

static char *read_string(read_env_t *env)
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

static ident *read_ident(read_env_t *env)
{
	char  *str = read_string(env);
	ident *res = new_id_from_str(str);
	obstack_free(&env->obst, str);
	return res;
}

static ident *read_symbol(read_env_t *env)
{
	char  *str = read_word(env);
	ident *res = new_id_from_str(str);
	obstack_free(&env->obst, str);
	return res;
}

/*
 * reads a "quoted string" or alternatively the token NULL
 */
static char *read_string_null(read_env_t *env)
{
	skip_ws(env);
	if (env->c == 'N') {
		char *str = read_word(env);
		if (streq(str, "NULL")) {
			obstack_free(&env->obst, str);
			return NULL;
		}
	} else if (env->c == '"') {
		return read_string(env);
	}

	parse_error(env, "Expected \"string\" or NULL\n");
	exit(1);
}

static ident *read_ident_null(read_env_t *env)
{
	char *str = read_string_null(env);
	if (str == NULL)
		return NULL;

	ident *res = new_id_from_str(str);
	obstack_free(&env->obst, str);
	return res;
}

static long read_long(read_env_t *env)
{
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

	char *str    = (char*)obstack_finish(&env->obst);
	long  result = atol(str);
	obstack_free(&env->obst, str);

	return result;
}

int read_int(read_env_t *env)
{
	return (int) read_long(env);
}

unsigned read_unsigned(read_env_t *env)
{
	return (unsigned) read_long(env);
}

size_t read_size_t(read_env_t *env)
{
	/* FIXME */
	return (size_t) read_unsigned(env);
}

static void expect_list_begin(read_env_t *env)
{
	skip_ws(env);
	if (env->c != '[') {
		parse_error(env, "Expected list, got '%c'\n", env->c);
		exit(1);
	}
	read_c(env);
}

static bool list_has_next(read_env_t *env)
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

static void *get_id(read_env_t *env, long id)
{
	id_entry key;
	key.id = id;

	id_entry *entry = set_find(id_entry, env->idset, &key, sizeof(key),
	                           (unsigned) id);
	return entry ? entry->elem : NULL;
}

static void set_id(read_env_t *env, long id, void *elem)
{
	id_entry key;
	key.id   = id;
	key.elem = elem;
	(void)set_insert(id_entry, env->idset, &key, sizeof(key), (unsigned) id);
}

static ir_node *get_node_or_null(read_env_t *env, long nodenr)
{
	ir_node *node = (ir_node *) get_id(env, nodenr);
	if (node && node->kind != k_ir_node) {
		parse_error(env, "Irn ID %ld collides with something else\n",
		            nodenr);
		return NULL;
	}
	return node;
}

static ir_type *get_type(read_env_t *env, long typenr)
{
	ir_type *type = (ir_type *) get_id(env, typenr);
	if (type == NULL) {
		parse_error(env, "Type %ld not defined (yet?)\n", typenr);
		return get_unknown_type();
	}
	if (type->kind != k_type) {
		parse_error(env, "Object %ld is not a type (but should be)\n", typenr);
		return get_unknown_type();
	}
	return type;
}

ir_type *read_type_ref(read_env_t *env)
{
	char *str = read_word(env);
	if (streq(str, "unknown")) {
		obstack_free(&env->obst, str);
		return get_unknown_type();
	} else if (streq(str, "code")) {
		obstack_free(&env->obst, str);
		return get_code_type();
	}
	long nr = atol(str);
	obstack_free(&env->obst, str);

	return get_type(env, nr);
}

static ir_entity *create_error_entity(void)
{
	ir_entity *res = new_entity(get_glob_type(), new_id_from_str("error"),
	                            get_unknown_type());
	return res;
}

static ir_entity *get_entity(read_env_t *env, long entnr)
{
	ir_entity *entity = (ir_entity *) get_id(env, entnr);
	if (entity == NULL) {
		parse_error(env, "unknown entity: %ld\n", entnr);
		return create_error_entity();
	}
	if (!is_entity(entity)) {
		parse_error(env, "Object %ld is not an entity (but should be)\n",
		            entnr);
		return create_error_entity();
	}

	return entity;
}

ir_entity *read_entity_ref(read_env_t *env)
{
	long nr = read_long(env);
	return get_entity(env, nr);
}

ir_mode *read_mode_ref(read_env_t *env)
{
	char *str = read_string(env);
	for (size_t i = 0, n = ir_get_n_modes(); i < n; i++) {
		ir_mode *mode = ir_get_mode(i);
		if (streq(str, get_mode_name(mode))) {
			obstack_free(&env->obst, str);
			return mode;
		}
	}

	parse_error(env, "unknown mode \"%s\"\n", str);
	return mode_ANY;
}

static const char *get_typetag_name(typetag_t typetag)
{
	switch (typetag) {
	case tt_align:               return "align";
	case tt_builtin_kind:        return "builtin kind";
	case tt_cond_jmp_predicate:  return "cond_jmp_predicate";
	case tt_initializer:         return "initializer kind";
	case tt_keyword:             return "keyword";
	case tt_linkage:             return "linkage";
	case tt_loop:                return "loop";
	case tt_mode_arithmetic:     return "mode_arithmetic";
	case tt_pin_state:           return "pin state";
	case tt_segment:             return "segment";
	case tt_throws:              return "throws";
	case tt_tpo:                 return "type";
	case tt_type_state:          return "type state";
	case tt_visibility:          return "visibility";
	case tt_volatility:          return "volatility";
	}
	return "<UNKNOWN>";
}

/**
 * Read and decode an enum constant.
 */
static unsigned read_enum(read_env_t *env, typetag_t typetag)
{
	char    *str  = read_word(env);
	unsigned code = symbol(str, typetag);

	if (code != SYMERROR) {
		obstack_free(&env->obst, str);
		return code;
	}

	parse_error(env, "invalid %s: \"%s\"\n", get_typetag_name(typetag), str);
	return 0;
}

ir_align read_align(read_env_t *env)
{
	return (ir_align)read_enum(env, tt_align);
}

ir_builtin_kind read_builtin_kind(read_env_t *env)
{
	return (ir_builtin_kind)read_enum(env, tt_builtin_kind);
}

cond_jmp_predicate read_cond_jmp_predicate(read_env_t *env)
{
	return (cond_jmp_predicate)read_enum(env, tt_cond_jmp_predicate);
}

static ir_initializer_kind_t read_initializer_kind(read_env_t *env)
{
	return (ir_initializer_kind_t)read_enum(env, tt_initializer);
}

static ir_mode_arithmetic read_mode_arithmetic(read_env_t *env)
{
	return (ir_mode_arithmetic)read_enum(env, tt_mode_arithmetic);
}

bool read_pinned(read_env_t *env)
{
	return read_enum(env, tt_pin_state) == op_pin_state_pinned;
}

static ir_type_state read_type_state(read_env_t *env)
{
	return (ir_type_state)read_enum(env, tt_type_state);
}

static ir_visibility read_visibility(read_env_t *env)
{
	return (ir_visibility)read_enum(env, tt_visibility);
}

static ir_linkage read_linkage(read_env_t *env)
{
	return (ir_linkage)read_enum(env, tt_linkage);
}

ir_volatility read_volatility(read_env_t *env)
{
	return (ir_volatility)read_enum(env, tt_volatility);
}

bool read_throws(read_env_t *env)
{
	return (bool)read_enum(env, tt_throws);
}

bool read_loop(read_env_t *env)
{
	return (bool)read_enum(env, tt_loop);
}

static keyword_t read_keyword(read_env_t *env)
{
	return (keyword_t)read_enum(env, tt_keyword);
}

ir_relation read_relation(read_env_t *env)
{
	return (ir_relation)read_long(env);
}

ir_tarval *read_tarval_ref(read_env_t *env)
{
	ir_mode   *tvmode = read_mode_ref(env);
	char      *str    = read_word(env);
	ir_tarval *tv     = ir_tarval_from_ascii(str, tvmode);
	obstack_free(&env->obst, str);

	return tv;
}

ir_switch_table *read_switch_table_ref(read_env_t *env)
{
	size_t           n_entries = read_size_t(env);
	ir_switch_table *table     = ir_new_switch_table(env->irg, n_entries);

	for (size_t i = 0; i < n_entries; ++i) {
		long       pn  = read_long(env);
		ir_tarval *min = read_tarval_ref(env);
		ir_tarval *max = read_tarval_ref(env);
		ir_switch_table_set(table, i, min, max, pn);
	}
	return table;
}

static ir_initializer_t *read_initializer(read_env_t *env)
{
	ir_initializer_kind_t ini_kind = read_initializer_kind(env);

	switch (ini_kind) {
	case IR_INITIALIZER_CONST: {
		long nr = read_long(env);
		ir_node *node = get_node_or_null(env, nr);
		ir_initializer_t *initializer = create_initializer_const(node);
		if (node == NULL) {
			delayed_initializer_t di;
			di.initializer = initializer;
			di.node_nr     = nr;
			ARR_APP1(delayed_initializer_t, env->delayed_initializers, di);
		}
		return initializer;
	}

	case IR_INITIALIZER_TARVAL:
		return create_initializer_tarval(read_tarval_ref(env));

	case IR_INITIALIZER_NULL:
		return get_initializer_null();

	case IR_INITIALIZER_COMPOUND: {
		size_t n = read_size_t(env);
		ir_initializer_t *ini = create_initializer_compound(n);
		for (size_t i = 0; i < n; i++) {
			ir_initializer_t *curini = read_initializer(env);
			set_initializer_compound_value(ini, i, curini);
		}
		return ini;
	}
	}

	panic("unknown initializer kind");
}

static bool type_matches(const ir_type *t, tp_opcode op, unsigned size,
		unsigned align, ir_type_state state, unsigned flags)
{
	// This is only set after we deserialized the type, so we have
	// to account for that.
	flags |= state == layout_fixed ? tf_layout_fixed : 0;
	return t->opcode == op
		&& t->size == size
		&& t->align == align
		&& t->flags == flags;
}

static bool streq_null(const char *a, const char *b)
{
	return a == b || (!a == !b && streq(a, b));
}

/** Reads a type description and remembers it by its id. */
static void read_type(read_env_t *env)
{
	long           typenr = read_long(env);
	tp_opcode      opcode = (tp_opcode) read_enum(env, tt_tpo);
	unsigned       size   = (unsigned) read_long(env);
	unsigned       align  = (unsigned) read_long(env);
	ir_type_state  state  = read_type_state(env);
	unsigned       flags  = (unsigned) read_long(env);
	ir_type       *type;

	// We have to be careful not to duplicate types
	// which where initialized via ir_init().
	// That would destroy idempotency for `ir_export . ir_import`
	// and bloat the resulting IR files.

	if (maybe_initial_type) {
		ir_type *candidate = NULL;
		for (int i = 0; i < n_initial_types; ++i) {
			ir_type *t = get_irp_type(i);
			if (get_type_nr(t) == typenr) {
				candidate = t;
				break;
			}
		}
		if (candidate && type_matches(candidate, opcode, size, align, state, flags)) {
			type = candidate;
			skip_to(env, '\n');
			goto extend_env;
		} else {
			maybe_initial_type = false;
		}
	}

	switch (opcode) {
	case tpo_array: {
		ir_type *const elemtype = read_type_ref(env);
		unsigned const length   = read_unsigned(env);
		type = new_type_array(elemtype, length);
		set_type_size(type, size);
		goto finish_type;
	}

	case tpo_class: {
		ident *id = read_ident_null(env);

		if (typenr == (long) IR_SEGMENT_GLOBAL)
			type = get_glob_type();
		else
			type = new_type_class(id);
		set_type_size(type, size);
		goto finish_type;
	}

	case tpo_method: {
		unsigned callingconv = read_unsigned(env);
		mtp_additional_properties addprops
			= (mtp_additional_properties) read_long(env);
		size_t const nparams  = read_size_t(env);
		size_t const nresults = read_size_t(env);
		bool   const is_variadic = read_long(env);
		type = new_type_method(nparams, nresults, is_variadic, callingconv, addprops);

		for (size_t i = 0; i < nparams; i++) {
			long ptypenr = read_long(env);
			ir_type *paramtype = get_type(env, ptypenr);

			set_method_param_type(type, i, paramtype);
		}
		for (size_t i = 0; i < nresults; i++) {
			long ptypenr = read_long(env);
			ir_type *restype = get_type(env, ptypenr);

			set_method_res_type(type, i, restype);
		}

		goto finish_type;
	}

	case tpo_pointer: {
		ir_type *points_to = get_type(env, read_long(env));
		type = new_type_pointer(points_to);
		goto finish_type;
	}

	case tpo_primitive: {
		ir_mode *mode = read_mode_ref(env);
		type = new_type_primitive(mode);
		set_type_size(type, size);
		goto finish_type;
	}

	case tpo_struct: {
		ident *id = read_ident_null(env);
		type = new_type_struct(id);
		set_type_size(type, size);
		goto finish_type;
	}

	case tpo_union: {
		ident *id = read_ident_null(env);
		type = new_type_union(id);
		set_type_size(type, size);
		goto finish_type;
	}

	case tpo_segment: {
		ident *id = read_ident_null(env);
		type = new_type_segment(id, 0);
		goto finish_type;
	}

	case tpo_code:
	case tpo_unknown:
	case tpo_uninitialized:
		parse_error(env, "can't import this type kind (%d)", opcode);
		return;
	}
	parse_error(env, "unknown type kind: \"%d\"\n", opcode);
	skip_to(env, '\n');
	return;

finish_type:
	if (align > 0)
		set_type_alignment(type, align);
	type->flags = flags;

	if (state == layout_fixed)
		ARR_APP1(ir_type *, env->fixedtypes, type);

extend_env:
	set_id(env, typenr, type);
}

static void read_unknown_entity(read_env_t *env)
{
	long       entnr  = read_long(env);
	ir_entity *entity = get_unknown_entity();
	set_id(env, entnr, entity);
}

/** Reads an entity description and remembers it by its id. */
static void read_entity(read_env_t *env, ir_entity_kind kind)
{
	long           entnr      = read_long(env);
	ident         *name       = NULL;
	ident         *ld_name    = NULL;
	ir_visibility  visibility = ir_visibility_external;
	ir_linkage     linkage    = IR_LINKAGE_DEFAULT;
	ir_type       *owner      = NULL;
	ir_entity     *entity     = NULL;

	if (kind != IR_ENTITY_LABEL && kind != IR_ENTITY_PARAMETER) {
		name    = read_ident(env);
		ld_name = read_ident_null(env);
	}

	visibility = read_visibility(env);
	expect_list_begin(env);
	while (list_has_next(env)) {
		linkage |= read_linkage(env);
	}

	ir_type *type = read_type_ref(env);
	if (kind != IR_ENTITY_LABEL)
		owner = read_type_ref(env);

	ir_volatility volatility = read_volatility(env);

	switch (kind) {
	case IR_ENTITY_ALIAS: {
		ir_entity *aliased = read_entity_ref(env);
		entity = new_alias_entity(owner, name, aliased, type, visibility);
		break;
	}
	case IR_ENTITY_NORMAL:
		entity = new_entity(owner, name, type);
		if (ld_name != NULL)
			set_entity_ld_ident(entity, ld_name);
		const char *str = read_word(env);
		if (streq(str, "initializer")) {
			ir_initializer_t *initializer = read_initializer(env);
			if (initializer != NULL)
				set_entity_initializer(entity, initializer);
		} else if (streq(str, "none")) {
			/* do nothing */
		} else {
			parse_error(env, "expected 'initializer' or 'none' got '%s'\n", str);
		}
		break;
	case IR_ENTITY_COMPOUND_MEMBER:
		assert(is_compound_type(owner));
		int         offset          = read_int(env);
		unsigned    bitfield_offset = read_int(env);
		unsigned    bitfield_size   = read_int(env);

		// Try to deduplicate compound_members added by ir_init()
		for (int i = 0, n = get_compound_n_members(owner); i < n; ++i) {
			ir_entity *e = get_compound_member(owner, i);
			if (e->type == type
				&& streq_null(e->name, name)
				&& streq_null(e->ld_name, ld_name)
				&& get_entity_offset(e) == offset
				&& get_entity_bitfield_offset(e) == bitfield_offset
				&& get_entity_bitfield_size(e) == bitfield_size) {
				entity = e;
				goto skip_init;
			}
		}

		entity = new_entity(owner, name, type);
		if (ld_name != NULL)
			set_entity_ld_ident(entity, ld_name);
		set_entity_offset(entity, offset);
		set_entity_bitfield_offset(entity, bitfield_offset);
		set_entity_bitfield_size(entity, bitfield_size);
skip_init:
		break;
	case IR_ENTITY_METHOD:
		entity = new_entity(owner, name, type);
		if (ld_name != NULL)
			set_entity_ld_ident(entity, ld_name);
		add_entity_additional_properties(
			entity, (mtp_additional_properties) read_long(env));
		break;
	case IR_ENTITY_PARAMETER: {
		char  *str = read_word(env);
		size_t parameter_number;
		if (streq(str, "va_start")) {
			parameter_number = IR_VA_START_PARAMETER_NUMBER;
		} else {
			parameter_number = atol(str);
		}
		obstack_free(&env->obst, str);
		entity = new_parameter_entity(owner, parameter_number, type);
		set_entity_offset(entity, read_int(env));
		set_entity_bitfield_offset(entity, read_unsigned(env));
		set_entity_bitfield_size(entity, read_unsigned(env));
		break;
	}
	case IR_ENTITY_LABEL: {
		ir_label_t nr = get_irp_next_label_nr();
		entity = new_label_entity(nr);
		break;
	}
	case IR_ENTITY_SPILLSLOT:
	case IR_ENTITY_UNKNOWN:
		panic("read_entity with unexpected kind");
	}

	set_entity_volatility(entity, volatility);
	set_entity_visibility(entity, visibility);
	set_entity_linkage(entity, linkage);

	set_id(env, entnr, entity);
}

/** Parses the whole type graph. */
static void read_typegraph(read_env_t *env)
{
	ir_graph *old_irg = env->irg;

	EXPECT('{');

	env->irg = get_const_code_irg();

	/* parse all types first */
	while (true) {
		keyword_t kwkind;
		skip_ws(env);
		if (env->c == '}') {
			read_c(env);
			break;
		}

		kwkind = read_keyword(env);
		switch (kwkind) {
		case kw_type:
			read_type(env);
			break;

		case kw_entity:
			read_entity(env, IR_ENTITY_NORMAL);
			break;
		case kw_alias:
			read_entity(env, IR_ENTITY_ALIAS);
			break;
		case kw_label:
			read_entity(env, IR_ENTITY_LABEL);
			break;
		case kw_method:
			read_entity(env, IR_ENTITY_METHOD);
			break;
		case kw_compound_member:
			read_entity(env, IR_ENTITY_COMPOUND_MEMBER);
			break;
		case kw_parameter:
			read_entity(env, IR_ENTITY_PARAMETER);
			break;
		case kw_unknown:
			read_unknown_entity(env);
			break;
		default:
			parse_error(env, "type graph element not supported yet: %d\n", kwkind);
			skip_to(env, '\n');
			break;
		}
	}
	env->irg = old_irg;
}

ir_node *read_node_ref(read_env_t *env)
{
	long     nr   = read_long(env);
	ir_node *node = get_node_or_null(env, nr);
	if (node == NULL) {
		parse_error(env, "node %ld not defined (yet?)\n", nr);
		return new_r_Bad(env->irg, mode_ANY);
	}
	return node;
}

int read_preds(read_env_t *env)
{
	expect_list_begin(env);
	assert(obstack_object_size(&env->preds_obst) == 0);
	while (list_has_next(env)) {
		ir_node *pred = read_node_ref(env);
		obstack_grow(&env->preds_obst, &pred, sizeof(pred));
	}
	return obstack_object_size(&env->preds_obst) / sizeof(ir_node*);
}

static void read_preds_delayed(read_env_t *env, ir_node *node)
{
	expect_list_begin(env);
	assert(obstack_object_size(&env->preds_obst) == 0);
	obstack_blank(&env->preds_obst, sizeof(delayed_pred_t));
	int n_preds = 0;
	while (list_has_next(env)) {
		long pred_nr = read_long(env);
		obstack_grow(&env->preds_obst, &pred_nr, sizeof(pred_nr));
		++n_preds;
	}
	delayed_pred_t *d = (delayed_pred_t*) obstack_finish(&env->preds_obst);
	d->node    = node;
	d->n_preds = n_preds;

	ARR_APP1(const delayed_pred_t*, env->delayed_preds, d);
}

static ir_node *read_ASM(read_env_t *env)
{
	ir_node *block    = read_node_ref(env);
	ir_node *mem      = read_node_ref(env);
	ident   *asm_text = read_ident(env);

	expect_list_begin(env);
	ir_asm_constraint *constraints = NEW_ARR_F(ir_asm_constraint, 0);
	while (list_has_next(env)) {
		ir_asm_constraint constraint;
		constraint.in_pos     = read_int(env);
		constraint.out_pos    = read_int(env);
		constraint.constraint = read_ident(env);
		constraint.mode       = read_mode_ref(env);
		ARR_APP1(ir_asm_constraint, constraints, constraint);
	}

	expect_list_begin(env);
	ident **clobbers = NEW_ARR_F(ident*, 0);
	while (list_has_next(env)) {
		ident *clobber = read_ident(env);
		ARR_APP1(ident*, clobbers, clobber);
	}

	ir_cons_flags flags = cons_none;
	if (!read_pinned(env))
		flags |= cons_floats;
	if (read_throws(env))
		flags |= cons_throws_exception;

	int       n_in = read_preds(env);
	ir_node **in   = (ir_node**)obstack_finish(&env->preds_obst);

	ir_node *newnode = new_r_ASM(block, mem, n_in, in, asm_text,
	                             ARR_LEN(constraints), constraints,
	                             ARR_LEN(clobbers), clobbers, flags);
	obstack_free(&env->preds_obst, in);
	DEL_ARR_F(clobbers);
	DEL_ARR_F(constraints);
	return newnode;
}

static ir_node *read_Phi(read_env_t *env)
{
	ir_node   *block = read_node_ref(env);
	ir_mode   *mode  = read_mode_ref(env);
	const bool loop  = read_loop(env);
	ir_node   *res   = loop ? new_r_Phi_loop(block, 0, NULL)
	                        : new_r_Phi(block, 0, NULL, mode);
	read_preds_delayed(env, res);
	return res;
}

static ir_node *read_Block(read_env_t *env)
{
	ir_node *res = new_r_Block(env->irg, 0, NULL);
	read_preds_delayed(env, res);
	return res;
}

static ir_node *read_labeled_Block(read_env_t *env)
{
	ir_node   *res    = new_r_Block(env->irg, 0, NULL);
	ir_entity *entity = read_entity_ref(env);
	read_preds_delayed(env, res);
	set_Block_entity(res, entity);
	return res;
}

static ir_node *read_Anchor(read_env_t *env)
{
	ir_node *res = new_r_Anchor(env->irg);
	read_preds_delayed(env, res);
	return res;
}

static pmap *node_readers;

void register_node_reader(char const *const name, read_node_func *const func)
{
	ident *const id = new_id_from_str(name);
	pmap_insert(node_readers, id, (void*)func);
}

static ir_node *read_node(read_env_t *env)
{
	ident          *id   = read_symbol(env);
	read_node_func *func = pmap_get(read_node_func, node_readers, id);
	long            nr   = read_long(env);
	ir_node        *res;
	if (func == NULL) {
		parse_error(env, "Unknown nodetype '%s'", get_id_str(id));
		skip_to(env, '\n');
		res = new_r_Bad(env->irg, mode_ANY);
	} else {
		res = func(env);
	}
	set_id(env, nr, res);
	return res;
}

static void readers_init(void)
{
	assert(node_readers == NULL);
	node_readers = pmap_create();
	register_node_reader("Anchor", read_Anchor);
	register_node_reader("ASM",    read_ASM);
	register_node_reader("Block",  read_Block);
	register_node_reader("BlockL", read_labeled_Block);
	register_node_reader("Phi",    read_Phi);
	register_generated_node_readers();
}

static void read_graph(read_env_t *env, ir_graph *irg)
{
	env->irg           = irg;
	env->delayed_preds = NEW_ARR_F(const delayed_pred_t*, 0);

	EXPECT('{');
	while (true) {
		skip_ws(env);
		if (env->c == '}' || env->c == EOF) {
			read_c(env);
			break;
		}

		read_node(env);
	}

	/* resolve delayed preds */
	for (size_t i = 0, n = ARR_LEN(env->delayed_preds); i < n; ++i) {
		const delayed_pred_t *dp  = env->delayed_preds[i];
		ir_node             **ins = ALLOCAN(ir_node*, dp->n_preds);
		for (int i = 0; i < dp->n_preds; ++i) {
			long     pred_nr = dp->preds[i];
			ir_node *pred    = get_node_or_null(env, pred_nr);
			if (pred == NULL) {
				parse_error(env, "predecessor %ld of a node not defined\n",
				            pred_nr);
				goto next_delayed_pred;
			}
			ins[i] = pred;
		}
		set_irn_in(dp->node, dp->n_preds, ins);
		if (is_Anchor(dp->node)) {
			foreach_irn_in(get_irg_anchor(irg), a, old) {
				exchange(old, ins[a]);
			}
		}
next_delayed_pred: ;
	}
	DEL_ARR_F(env->delayed_preds);
	env->delayed_preds = NULL;
}

static ir_graph *read_irg(read_env_t *env)
{
	ir_entity *irgent    = get_entity(env, read_long(env));
	ir_graph  *irg       = new_ir_graph(irgent, 0);
	ir_type   *frame     = read_type_ref(env);
	ir_type   *old_frame = get_irg_frame_type(irg);
	set_irg_frame_type(irg, frame);
	// Free the old frame type in order to retain idempotency
	free_type(old_frame);
	read_graph(env, irg);
	irg_finalize_cons(irg);
	return irg;
}

static void read_modes(read_env_t *env)
{
	EXPECT('{');

	while (true) {
		keyword_t kwkind;

		skip_ws(env);
		if (env->c == '}' || env->c == EOF) {
			read_c(env);
			break;
		}

		kwkind = read_keyword(env);
		switch (kwkind) {
		case kw_int_mode: {
			const char *name = read_string(env);
			ir_mode_arithmetic arith = read_mode_arithmetic(env);
			assert(arith == irma_twos_complement);
			(void)arith;
			int size = read_long(env);
			int sign = read_long(env);
			unsigned modulo_shift = read_long(env);
			new_int_mode(name, size, sign, modulo_shift);
			break;
		}
		case kw_reference_mode: {
			const char *name = read_string(env);
			ir_mode_arithmetic arith = read_mode_arithmetic(env);
			assert(arith == irma_twos_complement);
			(void)arith;
			int size = read_long(env);
			unsigned modulo_shift = read_long(env);
			ir_mode *mode = new_reference_mode(name, size, modulo_shift);
			set_reference_offset_mode(mode, read_mode_ref(env));
			int is_mode_P = read_int(env);
			if (is_mode_P)
				set_modeP(mode);
			break;
		}
		case kw_float_mode: {
			const char *name = read_string(env);
			ir_mode_arithmetic arith = read_mode_arithmetic(env);
			int exponent_size = read_long(env);
			int mantissa_size = read_long(env);
			float_int_conversion_overflow_style_t overflow =
				(float_int_conversion_overflow_style_t)read_long(env);
			new_float_mode(name, arith, exponent_size, mantissa_size,
			               overflow);
			break;
		}

		default:
			skip_to(env, '\n');
			break;
		}
	}
}

static void read_program(read_env_t *env)
{
	EXPECT('{');

	while (true) {
		skip_ws(env);
		if (env->c == '}') {
			read_c(env);
			break;
		}

		keyword_t kwkind = read_keyword(env);
		switch (kwkind) {
		case kw_segment_type: {
			ir_segment_t  segment = (ir_segment_t) read_enum(env, tt_segment);
			ir_type      *type    = read_type_ref(env);
			set_segment_type(segment, type);
			break;
		}
		case kw_asm: {
			ident *text = read_ident(env);
			add_irp_asm(text);
			break;
		}
		default:
			parse_error(env, "unexpected keyword %d\n", kwkind);
			skip_to(env, '\n');
		}
	}
}

int ir_import(const char *filename)
{
	FILE *file = fopen(filename, "rt");
	if (file == NULL) {
		perror(filename);
		return 1;
	}

	int res = ir_import_file(file, filename);
	fclose(file);
	return res;
}

int ir_import_file(FILE *input, const char *inputname)
{
	read_env_t          myenv;
	int                 oldoptimize = get_optimize();
	read_env_t         *env         = &myenv;

	readers_init();
	symtbl_init();

	memset(env, 0, sizeof(*env));
	obstack_init(&env->obst);
	obstack_init(&env->preds_obst);
	env->idset      = new_set(id_cmp, 128);
	env->fixedtypes = NEW_ARR_F(ir_type *, 0);
	env->inputname  = inputname;
	env->file       = input;
	env->line       = 1;
	env->delayed_initializers = NEW_ARR_F(delayed_initializer_t, 0);

	/* read first character */
	read_c(env);

	/* if the first line starts with '#', it contains a comment. */
	if (env->c == '#')
		skip_to(env, '\n');

	set_optimize(0);

	n_initial_types = get_irp_n_types();
	maybe_initial_type = true;

	while (true) {
		keyword_t kw;

		skip_ws(env);
		if (env->c == EOF)
			break;

		kw = read_keyword(env);
		switch (kw) {
		case kw_modes:
			read_modes(env);
			break;

		case kw_typegraph:
			read_typegraph(env);
			break;

		case kw_irg:
			read_irg(env);
			break;

		case kw_constirg: {
			ir_graph *constirg = get_const_code_irg();
			long bodyblockid = read_long(env);
			set_id(env, bodyblockid, constirg->current_block);
			read_graph(env, constirg);
			break;
		}

		case kw_program:
			read_program(env);
			break;

		default: {
			parse_error(env, "Unexpected keyword %d at toplevel\n", kw);
			exit(1);
		}
		}
	}

	for (size_t i = 0, n = ARR_LEN(env->fixedtypes); i < n; i++)
		set_type_state(env->fixedtypes[i], layout_fixed);

	DEL_ARR_F(env->fixedtypes);

	/* resolve delayed initializers */
	for (size_t i = 0, n = ARR_LEN(env->delayed_initializers); i < n; ++i) {
		const delayed_initializer_t *di   = &env->delayed_initializers[i];
		ir_node                     *node = get_node_or_null(env, di->node_nr);
		if (node == NULL) {
			parse_error(env, "node %ld mentioned in an initializer was never defined\n",
			            di->node_nr);
			continue;
		}
		assert(di->initializer->kind == IR_INITIALIZER_CONST);
		di->initializer->consti.value = node;
	}
	DEL_ARR_F(env->delayed_initializers);
	env->delayed_initializers = NULL;

	del_set(env->idset);

	set_optimize(oldoptimize);

	obstack_free(&env->preds_obst, NULL);
	obstack_free(&env->obst, NULL);

	pmap_destroy(node_readers);
	node_readers = NULL;

	return env->read_errors;
}
