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
#include "adt/set.h"

#define SYMERROR ((unsigned) ~0)

typedef struct io_env_t
{
	FILE *file;
	set *idset;               /**< id_entry set, which maps from file ids to new Firm elements */
	int ignoreblocks;
	int line, col;
	ir_type **fixedtypes;
} io_env_t;

typedef struct lex_state_t
{
	long offs;
	int line, col;
} lex_state_t;

typedef enum typetag_t
{
	tt_align,
	tt_allocation,
	tt_builtin,
	tt_cond_kind,
	tt_cond_jmp_predicate,
	tt_initializer,
	tt_iro,
	tt_keyword,
	tt_mode_arithmetic,
	tt_peculiarity,
	tt_pin_state,
	tt_tpo,
	tt_type_state,
	tt_variability,
	tt_visibility,
	tt_volatility
} typetag_t;

typedef enum keyword_t
{
	kw_constirg,
	kw_entity,
	kw_frametype,
	kw_irg,
	kw_mode,
	kw_modes,
	kw_valuetype,
	kw_type,
	kw_typegraph
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
 * Calculate a hash value for a string.
 */
static unsigned string_hash(const char *str, int len)
{
	return str[0] * 27893 ^ str[len-1] * 81 ^ str[len >> 1];
}

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

/** Initializes the symbol table. May be called more than once without problems. */
static void symtbl_init(void)
{
	symbol_t key;

	/* Only initialize once */
	if (symtbl != NULL)
		return;

	symtbl = new_set(symbol_cmp, 256);

#define INSERT(s, tt, cod)                                       \
	key.str = (s);                                               \
	key.typetag = (tt);                                          \
	key.code = (cod);                                            \
	set_insert(symtbl, &key, sizeof(key), string_hash(s, sizeof(s)-1) + tt * 17)

#define INSERTENUM(tt, e) INSERT(#e, tt, e)
#define INSERTKEYWORD(k) INSERT(#k, tt_keyword, kw_##k)

	INSERT("array", tt_tpo, tpo_array);
	INSERT("class", tt_tpo, tpo_class);
	INSERT("method", tt_tpo, tpo_method);
	INSERT("pointer", tt_tpo, tpo_pointer);
	INSERT("primitive", tt_tpo, tpo_primitive);
	INSERT("struct", tt_tpo, tpo_struct);
	INSERT("union", tt_tpo, tpo_union);
	INSERT("Unknown", tt_tpo, tpo_unknown);

	INSERTKEYWORD(constirg);
	INSERTKEYWORD(entity);
	INSERTKEYWORD(frametype);
	INSERTKEYWORD(irg);
	INSERTKEYWORD(mode);
	INSERTKEYWORD(modes);
	INSERTKEYWORD(valuetype);
	INSERTKEYWORD(type);
	INSERTKEYWORD(typegraph);

#include "gen_irio_lex.inl"

	INSERTENUM(tt_align, align_non_aligned);
	INSERTENUM(tt_align, align_is_aligned);

	INSERTENUM(tt_allocation, allocation_automatic);
	INSERTENUM(tt_allocation, allocation_parameter);
	INSERTENUM(tt_allocation, allocation_dynamic);
	INSERTENUM(tt_allocation, allocation_static);

	INSERTENUM(tt_builtin, ir_bk_trap);
	INSERTENUM(tt_builtin, ir_bk_debugbreak);
	INSERTENUM(tt_builtin, ir_bk_return_address);
	INSERTENUM(tt_builtin, ir_bk_frame_addess);
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

	INSERTENUM(tt_cond_kind, dense);
	INSERTENUM(tt_cond_kind, fragmentary);

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

	INSERTENUM(tt_peculiarity, peculiarity_description);
	INSERTENUM(tt_peculiarity, peculiarity_inherited);
	INSERTENUM(tt_peculiarity, peculiarity_existent);

	INSERTENUM(tt_pin_state, op_pin_state_floats);
	INSERTENUM(tt_pin_state, op_pin_state_pinned);
	INSERTENUM(tt_pin_state, op_pin_state_exc_pinned);
	INSERTENUM(tt_pin_state, op_pin_state_mem_pinned);

	INSERTENUM(tt_type_state, layout_undefined);
	INSERTENUM(tt_type_state, layout_fixed);

	INSERTENUM(tt_variability, variability_uninitialized);
	INSERTENUM(tt_variability, variability_initialized);
	INSERTENUM(tt_variability, variability_part_constant);
	INSERTENUM(tt_variability, variability_constant);

	INSERTENUM(tt_visibility, visibility_local);
	INSERTENUM(tt_visibility, visibility_external_visible);
	INSERTENUM(tt_visibility, visibility_external_allocated);

	INSERTENUM(tt_volatility, volatility_non_volatile);
	INSERTENUM(tt_volatility, volatility_is_volatile);

#undef INSERTKEYWORD
#undef INSERTENUM
#undef INSERT
}

/** Returns the according symbol value for the given string and tag, or SYMERROR if none was found. */
static unsigned symbol(const char *str, typetag_t typetag)
{
	symbol_t key, *entry;

	key.str = str;
	key.typetag = typetag;

	entry = set_find(symtbl, &key, sizeof(key), string_hash(str, strlen(str)) + typetag * 17);
	return entry ? entry->code : SYMERROR;
}

static void *get_id(io_env_t *env, long id)
{
	id_entry key, *entry;
	key.id = id;

	entry = set_find(env->idset, &key, sizeof(key), (unsigned) id);
	return entry ? entry->elem : NULL;
}

static void set_id(io_env_t *env, long id, void *elem)
{
	id_entry key;
	key.id = id;
	key.elem = elem;
	set_insert(env->idset, &key, sizeof(key), (unsigned) id);
}

static void write_mode(io_env_t *env, ir_mode *mode)
{
	fputs(get_mode_name(mode), env->file);
	fputc(' ', env->file);
}

static void write_tarval(io_env_t *env, tarval *tv)
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

static void write_cond_kind(io_env_t *env, ir_node *irn)
{
	fputs(get_cond_kind_name(get_Cond_kind(irn)), env->file);
	fputc(' ', env->file);
}

static void write_cond_jmp_predicate(io_env_t *env, ir_node *irn)
{
	fputs(get_cond_jmp_predicate_name(get_Cond_jmp_pred(irn)), env->file);
	fputc(' ', env->file);
}

static void write_initializer(io_env_t *env, ir_initializer_t *ini)
{
	FILE *f = env->file;
	ir_initializer_kind_t ini_kind = get_initializer_kind(ini);

	fputs(get_initializer_kind_name(ini_kind), f);
	fputc(' ', f);

	switch (ini_kind)
	{
		case IR_INITIALIZER_CONST:
			fprintf(f, "%ld ", get_irn_node_nr(get_initializer_const_value(ini)));
			break;

		case IR_INITIALIZER_TARVAL:
			write_tarval(env, get_initializer_tarval_value(ini));
			break;

		case IR_INITIALIZER_NULL:
			break;

		case IR_INITIALIZER_COMPOUND:
		{
			unsigned i, n = get_initializer_compound_n_entries(ini);
			fprintf(f, "%d ", n);
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
	fprintf(env->file, "\t%s %ld %s \"%s\" %u %u %s %s ",
			is_frame_type(tp) ? "frametype" : is_value_param_type(tp) ? "valuetype" : "type",
			get_type_nr(tp),
			get_type_tpop_name(tp),
			get_type_name(tp),
			get_type_size_bytes(tp),
			get_type_alignment_bytes(tp),
			get_type_state_name(get_type_state(tp)),
			get_visibility_name(get_type_visibility(tp)));
}

static void export_type_pre(io_env_t *env, ir_type *tp)
{
	FILE *f = env->file;

	/* skip types to be handled by post walker */
	switch (get_type_tpop_code(tp))
	{
		case tpo_array:
		case tpo_method:
		case tpo_pointer:
			return;
	}

	export_type_common(env, tp);

	switch (get_type_tpop_code(tp))
	{
		case tpo_class:
			/* TODO: inheritance stuff not supported yet */
			printf("Inheritance of classes not supported yet!\n");
			break;

		case tpo_primitive:
			write_mode(env, get_type_mode(tp));
			break;

		case tpo_struct:
			break;

		case tpo_union:
			break;

		case tpo_unknown:
			break;

		default:
			printf("export_type_pre: Unknown type code \"%s\".\n", get_type_tpop_name(tp));
			break;
	}
	fputc('\n', f);
}

static void export_type_post(io_env_t *env, ir_type *tp)
{
	FILE *f = env->file;
	int i, n, nparams, nresults;

	/* skip types already handled by pre walker */
	switch (get_type_tpop_code(tp))
	{
		case tpo_class:
		case tpo_primitive:
		case tpo_struct:
		case tpo_union:
		case tpo_unknown:
			return;
	}

	export_type_common(env, tp);

	switch (get_type_tpop_code(tp))
	{
		case tpo_array:
			n = get_array_n_dimensions(tp);
			fprintf(f, "%i %ld ", n, get_type_nr(get_array_element_type(tp)));
			for (i = 0; i < n; i++) {
				ir_node *lower = get_array_lower_bound(tp, i);
				ir_node *upper = get_array_upper_bound(tp, i);

				if (is_Const(lower))
					fprintf(f, "%ld ", get_tarval_long(get_Const_tarval(lower)));
				else
					panic("Lower array bound is not constant");

				if (is_Const(upper))
					fprintf(f, "%ld ", get_tarval_long(get_Const_tarval(upper)));
				else if (is_Unknown(upper))
					fputs("unknown ", f);
				else
					panic("Upper array bound is not constant");
			}
			break;

		case tpo_method:
			nparams  = get_method_n_params(tp);
			nresults = get_method_n_ress(tp);
			fprintf(f, "0x%X 0x%X %i %i ", get_method_calling_convention(tp),
				get_method_additional_properties(tp), nparams, nresults);
			for (i = 0; i < nparams; i++)
				fprintf(f, "%ld ", get_type_nr(get_method_param_type(tp, i)));
			for (i = 0; i < nresults; i++)
				fprintf(f, "%ld ", get_type_nr(get_method_res_type(tp, i)));
			fprintf(f, "%d ", get_method_first_variadic_param_index(tp));
			break;

		case tpo_pointer:
			write_mode(env, get_type_mode(tp));
			fprintf(f, "%ld ", get_type_nr(get_pointer_points_to_type(tp)));
			break;

		default:
			printf("export_type: Unknown type code \"%s\".\n", get_type_tpop_name(tp));
			break;
	}
	fputc('\n', f);
}

static void export_entity(io_env_t *env, ir_entity *ent)
{
	ir_type *owner = get_entity_owner(ent);
	fprintf(env->file, "\tentity %ld \"%s\" \"%s\" %ld %ld %d %u %s %s %s %s %s ",
			get_entity_nr(ent),
			get_entity_name(ent),
			ent->ld_name ? get_id_str(ent->ld_name) : "",
			get_type_nr(get_entity_type(ent)),
			get_type_nr(owner),
			get_entity_offset(ent),
			(unsigned) get_entity_offset_bits_remainder(ent),
			get_allocation_name(get_entity_allocation(ent)),
			get_visibility_name(get_entity_visibility(ent)),
			get_variability_name(get_entity_variability(ent)),
			get_peculiarity_name(get_entity_peculiarity(ent)),
			get_volatility_name(get_entity_volatility(ent)));

	/* TODO: inheritance stuff for class entities not supported yet */
	if (is_Class_type(owner) && owner != get_glob_type())
		printf("Inheritance of class entities not supported yet!\n");

	if (get_entity_variability(ent) != variability_uninitialized &&
	    get_entity_visibility(ent) != visibility_external_allocated)
	{
		if (is_compound_entity(ent)) {
			if (has_entity_initializer(ent)) {
				fputs("initializer ", env->file);
				write_initializer(env, get_entity_initializer(ent));
			} else {
				int i, n = get_compound_ent_n_values(ent);
				fputs("noninitializer ", env->file);
				fprintf(env->file, "%d ", n);
				for (i = 0; i < n; i++) {
					ir_entity *member = get_compound_ent_value_member(ent, i);
					ir_node   *irn    = get_compound_ent_value(ent, i);
					fprintf(env->file, "%ld %ld ", get_entity_nr(member), get_irn_node_nr(irn));
				}
			}
		} else {
			ir_node *irn = get_atomic_ent_value(ent);
			fprintf(env->file, "%ld ", get_irn_node_nr(irn));
		}
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

	switch (get_kind(tore.ent))
	{
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

static void export_node(ir_node *irn, void *ctx)
{
	io_env_t *env = (io_env_t *) ctx;
	int i, n;
	unsigned opcode = get_irn_opcode(irn);

	if (env->ignoreblocks && opcode == iro_Block)
		return;

	n = get_irn_arity(irn);

	fprintf(env->file, "\t%s %ld [ ", get_irn_opname(irn), get_irn_node_nr(irn));

	for (i = -1; i < n; i++) {
		ir_node *pred = get_irn_n(irn, i);
		if (pred == NULL) {
			/* Anchor node may have NULL predecessors */
			assert(is_Anchor(irn));
			fputs("-1 ", env->file);
		} else {
			fprintf(env->file, "%ld ", get_irn_node_nr(pred));
		}
	}

	fprintf(env->file, "] { ");

	switch (opcode)
	{
		#include "gen_irio_export.inl"
	}
	fputs("}\n", env->file);
}

static void export_modes(io_env_t *env)
{
	int i, n_modes = get_irp_n_modes();

	fputs("modes {\n", env->file);

	for (i = 0; i < n_modes; i++) {
		ir_mode *mode = get_irp_mode(i);
		switch (get_mode_sort(mode))
		{
			case irms_auxiliary:
			case irms_control_flow:
			case irms_memory:
			case irms_internal_boolean:
				/* skip "internal" modes, which may not be user defined */
				continue;
		}

		fprintf(env->file, "\tmode \"%s\" 0x%X %d %d %s %d %d ", get_mode_name(mode),
			get_mode_sort(mode), get_mode_size_bits(mode), get_mode_sign(mode),
			get_mode_arithmetic_name(get_mode_arithmetic(mode)), get_mode_modulo_shift(mode),
			get_mode_n_vector_elems(mode));
		if (mode_is_reference(mode)) {
			write_mode(env, get_reference_mode_signed_eq(mode));
			write_mode(env, get_reference_mode_unsigned_eq(mode));
		}
		fputc('\n', env->file);
	}

	fputs("}\n\n", env->file);
}

/** Exports the whole irp to the given file in a textual form. */
void ir_export(const char *filename)
{
	io_env_t env;
	int i, n_irgs = get_irp_n_irgs();

	env.file = fopen(filename, "wt");
	if (!env.file) {
		perror(filename);
		return;
	}

	export_modes(&env);

	fputs("typegraph {\n", env.file);

	type_walk_plus_frames(export_type_or_ent_pre, export_type_or_ent_post, &env);
	/* TODO: Visit frame types and "types for value params"? */

	for (i = 0; i < n_irgs; i++) {
		ir_graph *irg = get_irp_irg(i);
		ir_type *valuetype = get_irg_value_param_type(irg);

		fprintf(env.file, "}\n\nirg %ld %ld %ld {\n", get_entity_nr(get_irg_entity(irg)),
			get_type_nr(get_irg_frame_type(irg)),
			valuetype == NULL ? -1 : get_type_nr(valuetype));

		env.ignoreblocks = 0;
		irg_block_walk_graph(irg, NULL, export_node, &env);

		env.ignoreblocks = 1;
		irg_walk_anchors(irg, NULL, export_node, &env);
	}

	fprintf(env.file, "}\n\nconstirg %ld {\n", get_irn_node_nr(get_const_code_irg()->current_block));

	walk_const_code(NULL, export_node, &env);
	fputs("}\n", env.file);

	fclose(env.file);
}

/** Exports the given irg to the given file. */
void ir_export_irg(ir_graph *irg, const char *filename)
{
	io_env_t env;

	env.file = fopen(filename, "wt");
	if (!env.file) {
		perror(filename);
		return;
	}

	export_modes(&env);

	fputs("typegraph {\n", env.file);

	type_walk_irg(irg, export_type_or_ent_pre, export_type_or_ent_post, &env);

	fprintf(env.file, "}\n\nirg %ld {\n", get_entity_nr(get_irg_entity(irg)));

	env.ignoreblocks = 0;
	irg_block_walk_graph(irg, NULL, export_node, &env);

	env.ignoreblocks = 1;
	irg_walk_anchors(irg, NULL, export_node, &env);

	/* TODO: Only output needed constants */
	fputs("}\n\nconstirg {\n", env.file);
	walk_const_code(NULL, export_node, &env);
	fputs("}\n", env.file);

	fclose(env.file);
}

static void save_lex_state(io_env_t *env, lex_state_t *state)
{
	state->offs = ftell(env->file);
	state->line = env->line;
	state->col  = env->col;
}

static void restore_lex_state(io_env_t *env, lex_state_t *state)
{
	fseek(env->file, state->offs, SEEK_SET);
	env->line = state->line;
	env->col  = state->col;
}

static int read_c(io_env_t *env)
{
	int ch = fgetc(env->file);
	switch (ch)
	{
		case '\t':
			env->col += 4;
			break;

		case '\n':
			env->col = 0;
			env->line++;
			break;

		default:
			env->col++;
			break;
	}
	return ch;
}

/** Returns the first non-whitespace character or EOF. **/
static int skip_ws(io_env_t *env)
{
	while (1)
	{
		int ch = read_c(env);
		switch (ch)
		{
			case ' ':
			case '\t':
			case '\n':
			case '\r':
				break;

			default:
				return ch;
		}
	}
}

static void skip_to(io_env_t *env, char to_ch)
{
	int ch;
	do
		ch = read_c(env);
	while (ch != to_ch && ch != EOF);
}

static int expect_char(io_env_t *env, char ch)
{
	int curch = skip_ws(env);
	if (curch != ch) {
		printf("Unexpected char '%c', expected '%c' in line %i:%i\n", curch, ch, env->line, env->col);
		return 0;
	}
	return 1;
}

#define EXPECT(c) if (expect_char(env, (c))) {} else return 0
#define EXPECT_OR_EXIT(c) if (expect_char(env, (c))) {} else exit(1)

inline static const char *read_str_to(io_env_t *env, char *buf, size_t bufsize)
{
	size_t i;
	for (i = 0; i < bufsize - 1; i++) {
		int ch = read_c(env);
		if (ch == EOF) break;
		switch (ch)
		{
			case ' ':
			case '\t':
			case '\n':
			case '\r':
				if (i != 0)
					goto endofword;
				i--;	/* skip whitespace */
				break;

			default:
				buf[i] = ch;
				break;
		}
	}
endofword:
	buf[i] = 0;
	return buf;
}

static const char *read_str(io_env_t *env)
{
	static char buf[1024];
	return read_str_to(env, buf, sizeof(buf));
}

static const char *read_qstr_to(io_env_t *env, char *buf, size_t bufsize)
{
	size_t i;
	EXPECT_OR_EXIT('\"');
	for (i = 0; i < bufsize - 1; i++) {
		int ch = read_c(env);
		if (ch == EOF) {
			printf("Unexpected end of quoted string!\n");
			exit(1);
		}
		if (ch == '\"') break;

		buf[i] = ch;
	}
	if (i == bufsize - 1) {
		printf("Quoted string too long!\n");
		exit(1);
	}
	buf[i] = 0;
	return buf;
}

static const char *read_qstr(io_env_t *env)
{
	static char buf[1024];
	return read_qstr_to(env, buf, sizeof(buf));
}

static long read_long2(io_env_t *env, char **endptr)
{
	static char buf[1024];
	return strtol(read_str_to(env, buf, sizeof(buf)), endptr, 0);
}

static long read_long(io_env_t *env)
{
	return read_long2(env, NULL);
}

static ir_node *get_node_or_null(io_env_t *env, long nodenr)
{
	ir_node *node = (ir_node *) get_id(env, nodenr);
	if (node && node->kind != k_ir_node)
		panic("Irn ID %ld collides with something else in line %i:%i\n", nodenr, env->line, env->col);
	return node;
}

static ir_node *get_node(io_env_t *env, long nodenr)
{
	ir_node *node = get_node_or_null(env, nodenr);
	if (!node)
		panic("Unknown node: %ld in line %i:%i\n", nodenr, env->line, env->col);

	return node;
}

static ir_node *get_node_or_dummy(io_env_t *env, long nodenr)
{
	ir_node *node = get_node_or_null(env, nodenr);
	if (node == NULL) {
		node = new_Dummy(mode_X);
		set_id(env, nodenr, node);
	}
	return node;
}

static ir_type *get_type(io_env_t *env, long typenr)
{
	ir_type *type = (ir_type *) get_id(env, typenr);
	if (type == NULL)
		panic("Unknown type: %ld in line %i:%i\n", typenr, env->line, env->col);
	else if (type->kind != k_type)
		panic("Type ID %ld collides with something else in line %i:%i\n", typenr, env->line, env->col);
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
		printf("Unknown entity: %ld in line %i:%i\n", entnr, env->line, env->col);
		exit(1);
	} else if (entity->kind != k_entity)
		panic("Entity ID %ld collides with something else in line %i:%i\n", entnr, env->line, env->col);
	return entity;
}

static ir_entity *read_entity(io_env_t *env)
{
	return get_entity(env, read_long(env));
}

static ir_mode *read_mode(io_env_t *env)
{
	static char buf[128];
	int i, n;

	read_str_to(env, buf, sizeof(buf));

	n = get_irp_n_modes();
	for (i = 0; i < n; i++) {
		ir_mode *mode = get_irp_mode(i);
		if (!strcmp(buf, get_mode_name(mode)))
			return mode;
	}

	printf("Unknown mode \"%s\" in line %i:%i\n", buf, env->line, env->col);
	return mode_ANY;
}

static const char *get_typetag_name(typetag_t typetag)
{
	switch (typetag)
	{
		case tt_align:       return "align";
		case tt_allocation:  return "allocation";
		case tt_builtin:     return "builtin kind";
		case tt_initializer: return "initializer kind";
		case tt_iro:         return "opcode";
		case tt_peculiarity: return "peculiarity";
		case tt_pin_state:   return "pin state";
		case tt_tpo:         return "type";
		case tt_type_state:  return "type state";
		case tt_variability: return "variability";
		case tt_visibility:  return "visibility";
		case tt_volatility:  return "volatility";
		default: return "<UNKNOWN>";
	}
}

/**
 * Read and decode an enum constant.
 */
static unsigned read_enum(io_env_t *env, typetag_t typetag)
{
	static char buf[128];
	unsigned code = symbol(read_str_to(env, buf, sizeof(buf)), typetag);
	if (code != SYMERROR)
		return code;

	printf("Invalid %s: \"%s\" in %i:%i\n", get_typetag_name(typetag), buf, env->line, env->col);
	return 0;
}

#define read_align(env)              ((ir_align)              read_enum(env, tt_align))
#define read_allocation(env)         ((ir_allocation)         read_enum(env, tt_allocation))
#define read_builtin_kind(env)       ((ir_builtin_kind)       read_enum(env, tt_builtin))
#define read_cond_kind(env)          ((cond_kind)             read_enum(env, tt_cond_kind))
#define read_cond_jmp_predicate(env) ((cond_jmp_predicate)    read_enum(env, tt_cond_jmp_predicate))
#define read_initializer_kind(env)   ((ir_initializer_kind_t) read_enum(env, tt_initializer))
#define read_mode_arithmetic(env)    ((ir_mode_arithmetic)    read_enum(env, tt_mode_arithmetic))
#define read_peculiarity(env)        ((ir_peculiarity)        read_enum(env, tt_peculiarity))
#define read_pin_state(env)          ((op_pin_state)          read_enum(env, tt_pin_state))
#define read_type_state(env)         ((ir_type_state)         read_enum(env, tt_type_state))
#define read_variability(env)        ((ir_variability)        read_enum(env, tt_variability))
#define read_visibility(env)         ((ir_visibility)         read_enum(env, tt_visibility))
#define read_volatility(env)         ((ir_volatility)         read_enum(env, tt_volatility))

static ir_cons_flags get_cons_flags(io_env_t *env)
{
	ir_cons_flags flags = cons_none;

	op_pin_state pinstate = read_pin_state(env);
	switch (pinstate)
	{
		case op_pin_state_floats: flags |= cons_floats; break;
		case op_pin_state_pinned: break;
		default:
			panic("Error in %i:%i: Invalid pinstate: %s", env->line, env->col, get_op_pin_state_name(pinstate));
	}

	if (read_volatility(env) == volatility_is_volatile)
		flags |= cons_volatile;
	if (read_align(env) == align_non_aligned)
		flags |= cons_unaligned;

	return flags;
}

static tarval *read_tv(io_env_t *env)
{
	static char buf[128];
	ir_mode *tvmode = read_mode(env);
	read_str_to(env, buf, sizeof(buf));
	return new_tarval_from_str(buf, strlen(buf), tvmode);
}

static ir_initializer_t *read_initializer(io_env_t *env)
{
	FILE *f = env->file;
	ir_initializer_kind_t ini_kind = read_initializer_kind(env);

	switch (ini_kind)
	{
		case IR_INITIALIZER_CONST:
		{
			ir_node *irn = get_node_or_dummy(env, read_long(env));
			return create_initializer_const(irn);
		}

		case IR_INITIALIZER_TARVAL:
			return create_initializer_tarval(read_tv(env));

		case IR_INITIALIZER_NULL:
			return get_initializer_null();

		case IR_INITIALIZER_COMPOUND:
		{
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
static void import_type(io_env_t *env, keyword_t kwkind)
{
	int            i;
	ir_type       *type;
	long           typenr = read_long(env);
	const char    *tpop   = read_str(env);
	const char    *name   = read_qstr(env);
	unsigned       size   = (unsigned) read_long(env);
	unsigned       align  = (unsigned) read_long(env);
	ir_type_state  state  = read_type_state(env);
	ir_visibility  vis    = read_visibility(env);

	ident         *id     = new_id_from_str(name);

	const char    *kindstr;

	if (kwkind == kw_frametype) {
		if (symbol(tpop, tt_tpo) != tpo_class) {
			printf("Frame type must be a class type in line %i:%i\n", env->line, env->col);
			skip_to(env, '\n');
			return;
		}

		type = new_type_frame(id);
		set_type_size_bytes(type, size);

		kindstr = "frametype";
	} else if (kwkind == kw_valuetype) {
		if (symbol(tpop, tt_tpo) != tpo_struct) {
			printf("Value type must be a struct type in line %i:%i\n", env->line, env->col);
			skip_to(env, '\n');
			return;
		}

		type = new_type_value(id);
		set_type_size_bytes(type, size);

		kindstr = "valuetype";
	} else {
		switch (symbol(tpop, tt_tpo))
		{
			case tpo_array:
			{
				int ndims = (int) read_long(env);
				long elemtypenr = read_long(env);
				ir_type *elemtype = get_type(env, elemtypenr);

				type = new_type_array(id, ndims, elemtype);
				for (i = 0; i < ndims; i++) {
					const char *str = read_str(env);
					if (strcmp(str, "unknown") != 0) {
						long lowerbound = strtol(str, NULL, 0);
						set_array_lower_bound_int(type, i, lowerbound);
					}
					str = read_str(env);
					if (strcmp(str, "unknown") != 0) {
						long upperbound = strtol(str, NULL, 0);
						set_array_upper_bound_int(type, i, upperbound);
					}
				}
				set_type_size_bytes(type, size);
				break;
			}

			case tpo_class:
				type = new_type_class(id);
				set_type_size_bytes(type, size);
				break;

			case tpo_method:
			{
				unsigned callingconv = (unsigned) read_long(env);
				unsigned addprops    = (unsigned) read_long(env);
				int nparams          = (int)      read_long(env);
				int nresults         = (int)      read_long(env);
				int variaindex;

				type = new_type_method(id, nparams, nresults);

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

			case tpo_pointer:
			{
				ir_mode *mode = read_mode(env);
				ir_type *pointsto = get_type(env, read_long(env));
				type = new_type_pointer(id, pointsto, mode);
				break;
			}

			case tpo_primitive:
			{
				ir_mode *mode = read_mode(env);
				type = new_type_primitive(id, mode);
				break;
			}

			case tpo_struct:
				type = new_type_struct(id);
				set_type_size_bytes(type, size);
				break;

			case tpo_union:
				type = new_type_union(id);
				set_type_size_bytes(type, size);
				break;

			case tpo_unknown:
				return;   /* ignore unknown type */

			default:
				if (typenr != 0)  /* ignore global type */
					printf("Unknown type kind: \"%s\" in line %i:%i\n", tpop, env->line, env->col);
				skip_to(env, '\n');
				return;
		}
		kindstr = "type";
	}

	set_type_alignment_bytes(type, align);
	set_type_visibility(type, vis);

	if (state == layout_fixed)
		ARR_APP1(ir_type *, env->fixedtypes, type);

	set_id(env, typenr, type);
	printf("Insert %s %s %ld\n", kindstr, name, typenr);
}

/** Reads an entity description and remembers it by its id. */
static void import_entity(io_env_t *env)
{
	char          buf[1024], buf2[1024];
	long          entnr       = read_long(env);
	const char   *name        = read_qstr_to(env, buf, sizeof(buf));
	const char   *ld_name     = read_qstr_to(env, buf2, sizeof(buf2));
	long          typenr      = read_long(env);
	long          ownertypenr = read_long(env);

	ir_type   *type      = get_type(env, typenr);
	ir_type   *ownertype = !ownertypenr ? get_glob_type() : get_type(env, ownertypenr);
	ir_entity *entity    = new_entity(ownertype, new_id_from_str(name), type);

	if (*ld_name)
		set_entity_ld_ident(entity, new_id_from_str(ld_name));
	set_entity_offset     (entity, (int) read_long(env));
	set_entity_offset_bits_remainder(entity, (unsigned char) read_long(env));
	set_entity_allocation (entity, read_allocation(env));
	set_entity_visibility (entity, read_visibility(env));
	set_entity_variability(entity, read_variability(env));
	set_entity_peculiarity(entity, read_peculiarity(env));
	set_entity_volatility (entity, read_volatility(env));

	if (get_entity_variability(entity) != variability_uninitialized &&
	    get_entity_visibility(entity) != visibility_external_allocated)
	{
		if (is_compound_entity(entity)) {
			if (!strcmp(read_str_to(env, buf2, sizeof(buf2)), "initializer")) {
				set_entity_initializer(entity, read_initializer(env));
			} else {
				int i, n = (int) read_long(env);
				for (i = 0; i < n; i++) {
					ir_entity *member = get_entity(env, read_long(env));
					ir_node   *irn    = get_node_or_dummy(env, read_long(env));
					add_compound_ent_value(entity, irn, member);
				}
			}
		} else {
			ir_node *irn = get_node_or_dummy(env, read_long(env));
			set_atomic_ent_value(entity, irn);
		}
	}

	set_id(env, entnr, entity);
	printf("Insert entity %s %ld\n", name, entnr);
}

/** Parses the whole type graph. */
static int parse_typegraph(io_env_t *env)
{
	const char *kind;
	keyword_t kwkind;
	lex_state_t oldstate;

	EXPECT('{');

	save_lex_state(env, &oldstate);

	current_ir_graph = get_const_code_irg();

	/* parse all types first */
	while (1) {
		kind = read_str(env);
		if (kind[0] == '}' && kind[1] == '\0')
			break;

		kwkind = (keyword_t) symbol(kind, tt_keyword);
		switch (kwkind)
		{
			case kw_type:
			case kw_frametype:
			case kw_valuetype:
				import_type(env, kwkind);
				break;

			default:
				skip_to(env, '\n');
				break;
		}
	}

	/* now parse rest */
	restore_lex_state(env, &oldstate);

	while (1) {
		kind = read_str(env);
		if (kind[0] == '}' && kind[1] == '\0')
			break;

		switch (symbol(kind, tt_keyword))
		{
			case kw_type:
			case kw_frametype:
			case kw_valuetype:
				skip_to(env, '\n');
				break;

			case kw_entity:
				import_entity(env);
				break;

			default:
				printf("Type graph element not supported yet: \"%s\"\n", kind);
				skip_to(env, '\n');
				break;
		}
	}
	return 1;
}

static int read_node_header(io_env_t *env, long *nodenr, long **preds, const char **nodename)
{
	int numpreds;
	*nodename = read_str(env);
	if ((*nodename)[0] == '}' && !(*nodename)[1]) return -1;  /* end-of-graph */

	*nodenr = read_long(env);

	ARR_RESIZE(ir_node *, *preds, 0);

	EXPECT('[');
	for (numpreds = 0; !feof(env->file); numpreds++) {
		char *endptr;
		ARR_APP1(long, *preds, read_long2(env, &endptr));
		if (*endptr == ']') break;
	}
	return numpreds;
}

/** Parses an IRG. */
static int parse_graph(io_env_t *env, ir_graph *irg)
{
	long       *preds = NEW_ARR_F(long, 16);
	ir_node   **prednodes = NEW_ARR_F(ir_node *, 16);
	int         i, numpreds, ret = 1;
	long        nodenr;
	const char *nodename;
	ir_node    *node, *newnode;

	current_ir_graph = irg;

	EXPECT('{');

	while (1) {
		numpreds = read_node_header(env, &nodenr, &preds, &nodename);
		if (numpreds == -1) break;  /* end-of-graph */
		if (!numpreds) {
			printf("Node %s %ld is missing predecessors!", nodename, nodenr);
			ret = 0;
			break;
		}

		ARR_RESIZE(ir_node *, prednodes, numpreds);
		for (i = 0; i < numpreds - 1; i++)
			prednodes[i] = get_node_or_dummy(env, preds[i + 1]);

		node = get_node_or_null(env, nodenr);
		newnode = NULL;

		EXPECT('{');

		switch (symbol(nodename, tt_iro))
		{
			case iro_End:
			{
				ir_node *newendblock = get_node(env, preds[0]);
				newnode = get_irg_end(current_ir_graph);
				exchange(get_nodes_block(newnode), newendblock);
				for (i = 0; i < numpreds - 1; i++)
					add_irn_n(newnode, prednodes[i]);
				break;
			}

			case iro_Start:
			{
				ir_node *newstartblock = get_node(env, preds[0]);
				newnode = get_irg_start(current_ir_graph);
				exchange(get_nodes_block(newnode), newstartblock);
				break;
			}

			case iro_Block:
			{
				if (preds[0] != nodenr) {
					printf("Invalid block: preds[0] != nodenr (%ld != %ld)\n",
						preds[0], nodenr);
					ret = 0;
					goto endloop;
				}

				newnode = new_Block(numpreds - 1, prednodes);
				break;
			}

			case iro_Anchor:
				newnode = current_ir_graph->anchor;
				for (i = 0; i < numpreds - 1; i++)
					set_irn_n(newnode, i, prednodes[i]);
				set_irn_n(newnode, -1, get_node(env, preds[0]));
				break;

			case iro_SymConst:
			{
				long entnr = read_long(env);
				union symconst_symbol sym;
				sym.entity_p = get_entity(env, entnr);
				newnode = new_SymConst(mode_P, sym, symconst_addr_ent);
				break;
			}

			#include "gen_irio_import.inl"

			default:
				goto notsupported;
		}

		EXPECT('}');

		if (!newnode) {
notsupported:
			panic("Node type not supported yet: %s in line %i:%i\n", nodename, env->line, env->col);
		}

		if (node)
			exchange(node, newnode);
		/* Always update hash entry to avoid more uses of id nodes */
		set_id(env, nodenr, newnode);
		/* printf("Insert %s %ld\n", nodename, nodenr); */
	}

endloop:
	DEL_ARR_F(preds);
	DEL_ARR_F(prednodes);

	return ret;
}

static int parse_modes(io_env_t *env)
{
	const char *kind;
	keyword_t kwkind;

	EXPECT('{');

	while (1) {
		kind = read_str(env);
		if (kind[0] == '}' && kind[1] == '\0')
			break;

		kwkind = (keyword_t) symbol(kind, tt_keyword);
		switch (kwkind)
		{
			case kw_mode:
			{
				const char *name = read_qstr(env);
				ir_mode_sort sort = (ir_mode_sort) read_long(env);
				int size = read_long(env);
				int sign = read_long(env);
				ir_mode_arithmetic arith = read_mode_arithmetic(env);
				unsigned modulo_shift = read_long(env);
				int vector_elems = read_long(env);

				ir_mode *mode = new_ir_mode(name, sort, size, sign, arith, modulo_shift);

				if (mode_is_reference(mode))
				{
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

/** Imports an previously exported textual representation of an (maybe partial) irp */
void ir_import(const char *filename)
{
	int oldoptimize = get_optimize();
	firm_verification_t oldver = get_node_verification_mode();
	io_env_t ioenv;
	io_env_t *env = &ioenv;
	int i, n;

	symtbl_init();

	memset(env, 0, sizeof(*env));
	env->idset = new_set(id_cmp, 128);
	env->fixedtypes = NEW_ARR_F(ir_type *, 0);

	env->file = fopen(filename, "rt");
	if (!env->file) {
		perror(filename);
		exit(1);
	}

	set_optimize(0);
	do_node_verification(FIRM_VERIFICATION_OFF);

	while (1) {
		const char *str = read_str(env);
		if (!*str) break;
		switch (symbol(str, tt_keyword))
		{
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

			case kw_constirg:
			{
				ir_graph *constirg = get_const_code_irg();
				long bodyblockid = read_long(env);
				set_id(env, bodyblockid, constirg->current_block);
				if (!parse_graph(env, constirg)) goto end;
				break;
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

	fclose(env->file);
}
