/**
 * Dumps global variables and constants as ia32 assembler.
 * @author Christian Wuerdig
 * @date 04.11.2005
 * @version $Id$
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "obst.h"
#include "tv.h"
#include "irnode.h"
#include "entity.h"
#include "irprog.h"
#include "error.h"

#include "../be.h"

#include "ia32_emitter.h"
#include "ia32_gen_decls.h"

typedef struct obstack obstack_t;

typedef struct _ia32_decl_env {
	obstack_t *rodata_obst;
	obstack_t *data_obst;
	obstack_t *bss_obst;
	obstack_t *ctor_obst;
	const be_main_env_t *main_env;
} ia32_decl_env_t;

/************************************************************************/

/**
 * output a tarval
 */
static void dump_arith_tarval(obstack_t *obst, tarval *tv, int bytes)
{
	switch (bytes) {

	case 1:
		obstack_printf(obst, "0x%02x", get_tarval_sub_bits(tv, 0));
		break;

	case 2:
		obstack_printf(obst, "0x%02x%02x", get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		break;

	case 4:
		obstack_printf(obst, "0x%02x%02x%02x%02x",
			get_tarval_sub_bits(tv, 3), get_tarval_sub_bits(tv, 2), get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		break;

	case 8:
		obstack_printf(obst, "0x%02x%02x%02x%02x%02x%02x%02x%02x",
			get_tarval_sub_bits(tv, 7), get_tarval_sub_bits(tv, 6), get_tarval_sub_bits(tv, 5), get_tarval_sub_bits(tv, 4),
			get_tarval_sub_bits(tv, 3), get_tarval_sub_bits(tv, 2), get_tarval_sub_bits(tv, 1), get_tarval_sub_bits(tv, 0));
		break;

	case 10:
	case 12:
		break;

	default:
		fprintf(stderr, "Try to dump an tarval with %d bytes\n", bytes);
		assert(0);
	}
}

static tarval *get_atomic_init_tv(ir_node *init)
{
	ir_mode *mode = get_irn_mode(init);

	switch (get_irn_opcode(init)) {

	case iro_Cast:
		return get_atomic_init_tv(get_Cast_op(init));

	case iro_Conv:
		return get_atomic_init_tv(get_Conv_op(init));

	case iro_Const:
		return get_Const_tarval(init);

	case iro_SymConst:
		switch (get_SymConst_kind(init)) {
		case symconst_ofs_ent:
			return new_tarval_from_long(get_entity_offset(get_SymConst_entity(init)), mode);
			break;

		case symconst_type_size:
			return new_tarval_from_long(get_type_size_bytes(get_SymConst_type(init)), mode);

		case symconst_type_align:
			return new_tarval_from_long(get_type_alignment_bytes(get_SymConst_type(init)), mode);

		case symconst_enum_const:
			return get_enumeration_value(get_SymConst_enum(init));

		default:
			return NULL;
		}

		default:
			return NULL;
	}

	return NULL;
}

/*
 * dump an atomic value
 */
static void do_dump_atomic_init(obstack_t *obst, ir_node *init)
{
	ir_mode *mode = get_irn_mode(init);
	int bytes     = get_mode_size_bytes(mode);
	tarval *tv;

	switch (get_irn_opcode(init)) {

	case iro_Cast:
		do_dump_atomic_init(obst, get_Cast_op(init));
		return;

	case iro_Conv:
		do_dump_atomic_init(obst, get_Conv_op(init));
		return;

	case iro_Const:
		tv = get_Const_tarval(init);

		/* beware of old stuff */
		//assert(! mode_is_reference(mode));

		/* it's a arithmetic value */
		dump_arith_tarval(obst, tv, bytes);
		return;

	case iro_SymConst:
		switch (get_SymConst_kind(init)) {
		case symconst_addr_name:
			obstack_printf(obst, "%s", get_id_str(get_SymConst_name(init)));
			break;

		case symconst_addr_ent:
			obstack_printf(obst, "%s", get_entity_ld_name(get_SymConst_entity(init)));
			break;

		case symconst_ofs_ent:
			obstack_printf(obst, "%d", get_entity_offset(get_SymConst_entity(init)));
			break;

		case symconst_type_size:
			obstack_printf(obst, "%d", get_type_size_bytes(get_SymConst_type(init)));
			break;

		case symconst_type_align:
			obstack_printf(obst, "%d", get_type_alignment_bytes(get_SymConst_type(init)));
			break;

		case symconst_enum_const:
			tv = get_enumeration_value(get_SymConst_enum(init));
			dump_arith_tarval(obst, tv, bytes);
			break;

		default:
			assert(!"dump_atomic_init(): don't know how to init from this SymConst");
		}
		return;

		case iro_Add:
			do_dump_atomic_init(obst, get_Add_left(init));
			obstack_printf(obst, " + ");
			do_dump_atomic_init(obst, get_Add_right(init));
			return;

		case iro_Sub:
			do_dump_atomic_init(obst, get_Sub_left(init));
			obstack_printf(obst, " - ");
			do_dump_atomic_init(obst, get_Sub_right(init));
			return;

		case iro_Mul:
			do_dump_atomic_init(obst, get_Mul_left(init));
			obstack_printf(obst, " * ");
			do_dump_atomic_init(obst, get_Mul_right(init));
			return;

		default:
			assert(0 && "dump_atomic_init(): unknown IR-node");
	}
}

/*
 * dumps the type for given size (.byte, .long, ...)
 */
static void dump_size_type(obstack_t *obst, int size) {
	switch (size) {

	case 1:
		obstack_printf(obst, "\t.byte\t");
		break;

	case 2:
		obstack_printf(obst, "\t.value\t");
		break;

	case 4:
		obstack_printf(obst, "\t.long\t");
		break;

	case 8:
		obstack_printf(obst, "\t.quad\t");
		break;

	case 10:
	case 12:
		/* handled in arith */
		break;

	default:
		fprintf(stderr, "Try to dump a type with %d bytes\n", size);
		assert(0);
	}
}

/*
 * dump an atomic value
 */
static void dump_atomic_init(obstack_t *obst, ir_node *init)
{
	ir_mode *mode = get_irn_mode(init);
	int bytes     = get_mode_size_bytes(mode);

	dump_size_type(obst, bytes);
	do_dump_atomic_init(obst, init);
	obstack_printf(obst, "\n");
}

/************************************************************************/
/* Routines to dump global variables                                    */
/************************************************************************/

/**
 * Determine if an entity is a string constant
 * @param ent The entity
 * @return 1 if it is a string constant, 0 otherwise
 */
static int ent_is_string_const(entity *ent)
{
	int res = 0;
	ir_type *ty;

	ty = get_entity_type(ent);

	/* if it's an array */
	if (is_Array_type(ty)) {
		ir_type *elm_ty = get_array_element_type(ty);

		/* and the array's element type is primitive */
		if (is_Primitive_type(elm_ty)) {
			ir_mode *mode = get_type_mode(elm_ty);

			/*
			* and the mode of the element type is an int of
			* the same size as the byte mode
			*/
			if (mode_is_int(mode)
				&& get_mode_size_bits(mode) == get_mode_size_bits(mode_Bs))
			{
				int i, c, n;

				n = get_compound_ent_n_values(ent);
				for (i = 0; i < n; ++i) {
					ir_node *irn = get_compound_ent_value(ent, i);
					if(get_irn_opcode(irn) != iro_Const)
						return 0;

					c = (int) get_tarval_long(get_Const_tarval(irn));

					if((i < n - 1 && !(isgraph(c) || isspace(c)))
						|| (i == n - 1 && c != '\0'))
						return 0;
				}

				res = 1;
			}
		}
	}

	return res;
}

/**
 * Dump a atring constant.
 * No checks are made!!
 * @param obst The obst to dump on.
 * @param ent The entity to dump.
 */
static void dump_string_cst(obstack_t *obst, entity *ent)
{
	int i, n;

	obstack_printf(obst, "\t.string \"");
	n = get_compound_ent_n_values(ent);

	for (i = 0; i < n-1; ++i) {
		ir_node *irn;
		int c;

		irn = get_compound_ent_value(ent, i);
		c = (int) get_tarval_long(get_Const_tarval(irn));

		switch (c) {
		case '"' : obstack_printf(obst, "\\\""); break;
		case '\n': obstack_printf(obst, "\\n"); break;
		case '\r': obstack_printf(obst, "\\r"); break;
		case '\t': obstack_printf(obst, "\\t"); break;
		case '\\': obstack_printf(obst, "\\\\"); break;
		default  :
			if (isprint(c))
				obstack_printf(obst, "%c", c);
			else
				obstack_printf(obst, "%O", c);
			break;
		}
	}
	obstack_printf(obst, "\"\n");
}

static void dump_array_init(obstack_t *obst, entity *ent)
{
	const ir_type *ty = get_entity_type(ent);
	int i;
	int filler;
	int size = 0;

	/* potential spare values should be already included! */
	for (i = 0; i < get_compound_ent_n_values(ent); ++i) {
		entity *step = get_compound_ent_value_member(ent, i);
		ir_type *stype = get_entity_type(step);

		if (get_type_mode(stype)) {
			int align = (get_type_alignment_bits(stype) + 7) >> 3;
			int n     = size % align;

			if (n > 0) {
				obstack_printf(obst, "\t.zero\t%d\n", align - n);
				size += align - n;
			}
		}
		dump_atomic_init(obst, get_compound_ent_value(ent, i));
		size += get_type_size_bytes(stype);
	}
	filler = get_type_size_bytes(ty) - size;

	if (filler > 0)
		obstack_printf(obst, "\t.skip\t%d\n", filler);
}

enum normal_or_bitfield_kind {
	NORMAL = 0,
	BITFIELD
};

typedef struct {
	enum normal_or_bitfield_kind kind;
	union {
		ir_node *value;
		unsigned char bf_val;
	} v;
} normal_or_bitfield;

static void dump_compound_init(obstack_t *obst, entity *ent)
{
	ir_type *ty = get_entity_type(ent);
	normal_or_bitfield *vals;
	int type_size;
	int i, j;
	int res;

	res = compute_compound_ent_array_indices(ent);
	if(!res) {
		panic("Couldn't emit initializer for entity '%s'\n", get_entity_ld_name(ent));
	}

	/*
	 * in the worst case, every entity allocates one byte, so the type
	 * size should be equal or bigger the number of fields
	 */
	type_size = get_type_size_bytes(ty);
	vals = alloca(type_size * sizeof(vals[0]));
	memset(vals, 0, type_size * sizeof(vals[0]));

	/* collect the values and store them at the offsets */
	for(i = 0; i < get_compound_ent_n_values(ent); ++i) {
		const compound_graph_path *path = get_compound_ent_value_path(ent, i);
		int path_len = get_compound_graph_path_length(path);
		int offset = get_compound_ent_value_offset_bytes(ent, i);
		int offset_bits = get_compound_ent_value_offset_bit_remainder(ent, i);
		ir_node *value = get_compound_ent_value(ent, i);
		entity *last_ent = get_compound_graph_path_node(path, path_len - 1);
		int value_len = get_type_size_bits(get_entity_type(last_ent));

		if(offset_bits != 0 || value_len % 8 != 0) {
			tarval *shift, *shifted;
			tarval *tv = get_atomic_init_tv(value);
			if(tv == NULL) {
				panic("Couldn't get numeric value for bitfield initializer '%s'\n",
				      get_entity_ld_name(ent));
			}
			shift = new_tarval_from_long(offset_bits, mode_Is);
			shifted = tarval_shl(tv, shift);
			if(shifted == tarval_bad || shifted == tarval_undefined) {
				panic("Couldn't shift numeric value for bitfield initializer '%s'\n",
				      get_entity_ld_name(ent));
			}

			for(j = 0; j < 4 && value_len > 0; ++j) {
				assert(offset + j < type_size);
				assert(vals[offset + j].kind == BITFIELD || vals[offset + j].v.value == NULL);
				vals[offset + j].kind = BITFIELD;
				vals[offset + j].v.bf_val |= get_tarval_sub_bits(shifted, j);
				value_len -= 8 - offset_bits;
				offset_bits = 0;
			}
		} else {
			assert(offset < type_size);
			assert(vals[offset].kind == NORMAL);
			assert(vals[offset].v.value == NULL);
			vals[offset].v.value = value;
		}
	}

	/* now write them sorted */
	for(i = 0; i < type_size; ) {
		int space = 0, skip = 0;
		if (vals[i].kind == NORMAL) {
		   if(vals[i].v.value != NULL) {
   			   dump_atomic_init(obst, vals[i].v.value);
   			   skip = get_mode_size_bytes(get_irn_mode(vals[i].v.value)) - 1;
		   } else {
			   space = 1;
		   }
		} else {
			obstack_printf(obst, "\t.byte\t%d\n", vals[i].v.bf_val);
		}

		++i;
		space = 0;
		while(i < type_size && vals[i].kind == NORMAL && vals[i].v.value == NULL) {
			++space;
			++i;
		}
		space -= skip;
		assert(space >= 0);

		/* a gap */
		if(space > 0)
			obstack_printf(obst, "\t.skip\t%d\n", space);
	}
}

static void dump_global(ia32_decl_env_t *env, entity *ent)
{
	obstack_t *obst;
	ir_type *type = get_entity_type(ent);
	const char *ld_name = get_entity_ld_name(ent);
	ir_variability variability = get_entity_variability(ent);
	ir_visibility visibility = get_entity_visibility(ent);
	int align = get_type_alignment_bytes(type);

	obst = env->data_obst;
	if(is_Method_type(type)) {
		if(get_method_img_section(ent) == section_constructors) {
			obst = env->ctor_obst;
			obstack_printf(obst, ".balign\t%d\n", align);
			dump_size_type(obst, align);
			obstack_printf(obst, "%s\n", ld_name);
		}
		return;
	} else if(variability == variability_constant) {
		/* a constant entity, put it on the rdata */
		obst = env->rodata_obst;
	} else if(variability == variability_uninitialized) {
		/* uninitialized entity put it in bss segment */
		obst = env->bss_obst;
	}

	be_dbg_variable(env->main_env->db_handle, obst, ent);

	/* global or not global */
	if(visibility == visibility_external_visible) {
		obstack_printf(obst, ".global\t%s\n", ld_name);
	} else if(visibility == visibility_external_allocated) {
		obstack_printf(obst, ".global\t%s\n", ld_name);
		/* we can return now... */
		return;
	}
	/* alignment */
	if(align > 1) {
		obstack_printf(obst, ".balign\t%d\n", align);
	}

	obstack_printf(obst, "%s:\n", ld_name);

	if(variability == variability_uninitialized) {
		obstack_printf(obst, "\t.zero %d\n", get_type_size_bytes(type));
		return;
	}

	if (is_atomic_type(type)) {
		dump_atomic_init(obst, get_atomic_ent_value(ent));
		return;
	}

	if (ent_is_string_const(ent)) {
		dump_string_cst(obst, ent);
		return;
	}

	if (is_Array_type(type)) {
		dump_array_init(obst, ent);
		return;
	}

	if (is_compound_type(type)) {
		dump_compound_init(obst, ent);
		return;
	}

	assert(0 && "unsupported type");
}

/**
 * Dumps declarations of global variables and the initialization code.
 */
static void ia32_dump_globals(ir_type *gt, ia32_decl_env_t *env)
{
	int i, n = get_compound_n_members(gt);

	for (i = 0; i < n; i++) {
		entity *ent = get_compound_member(gt, i);
		dump_global(env, ent);
	}
}

/************************************************************************/

void ia32_gen_decls(FILE *out, const be_main_env_t *main_env) {
	ia32_decl_env_t env;
	obstack_t rodata, data, bss, ctor;
	int    size;
	char   *cp;

	/* dump the global type */
	obstack_init(&rodata);
	obstack_init(&data);
	obstack_init(&bss);

	if (main_env->options->opt_profile)
		obstack_init(&ctor);

	env.rodata_obst = &rodata;
	env.data_obst   = &data;
	env.bss_obst    = &bss;
	env.ctor_obst   = main_env->options->opt_profile ? &ctor : NULL;
	env.main_env    = main_env;

	ia32_dump_globals(get_glob_type(), &env);

	size = obstack_object_size(&data);
	cp   = obstack_finish(&data);
	if (size > 0) {
		ia32_switch_section(out, SECTION_DATA);
		fwrite(cp, 1, size, out);
	}

	size = obstack_object_size(&rodata);
	cp   = obstack_finish(&rodata);
	if (size > 0) {
		ia32_switch_section(out, SECTION_RODATA);
		fwrite(cp, 1, size, out);
	}

	size = obstack_object_size(&bss);
	cp   = obstack_finish(&bss);
	if (size > 0) {
		ia32_switch_section(out, SECTION_COMMON);
		fwrite(cp, 1, size, out);
	}

	if (main_env->options->opt_profile) {
		size = obstack_object_size(&ctor);
		cp   = obstack_finish(&ctor);
		if (size > 0) {
			ia32_switch_section(out, SECTION_CTOR);
			fwrite(cp, 1, size, out);
		}
		obstack_free(&ctor, NULL);
	}

	obstack_free(&rodata, NULL);
	obstack_free(&data, NULL);
	obstack_free(&bss, NULL);

	/* dump the Thread Local Storage */
	obstack_init(&data);

	env.rodata_obst = &data;
	env.data_obst   = &data;
	env.bss_obst   = &data;
	env.ctor_obst   = NULL;

	ia32_dump_globals(get_tls_type(), &env);

	size = obstack_object_size(&data);
	cp   = obstack_finish(&data);
	if (size > 0) {
		ia32_switch_section(out, SECTION_TLS);
		fprintf(out, ".balign\t%d\n", 32);
		fwrite(cp, 1, size, out);
	}

	obstack_free(&data, NULL);
}
