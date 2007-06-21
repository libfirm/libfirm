/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief   Stabs support.
 * @author  Michael Beck
 * @date    11.9.2006
 * @version $Id$
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "obst.h"
#include "irprog.h"
#include "irgraph.h"
#include "tv.h"
#include "xmalloc.h"
#include "pmap.h"
#include "pdeq.h"
#include "irtools.h"
#include "obst.h"
#include "array.h"
#include "be_dbgout.h"
#include "beabi.h"

/* Non-Stab Symbol and Stab Symbol Types */
enum stabs_types {
	N_UNDF    = 0x00,    /**< 0: Undefined symbol */
	N_ABS     = 0x02,    /**< 2: File scope absolute symbol */
	N_TEXT    = 0x04,    /**< 4: File scope text symbol */
	N_DATA    = 0x06,    /**< 6: File scope data symbol */
	N_BSS     = 0x08,    /**< 8: File scope BSS symbol */
	N_INDR    = 0x0A,    /**< 10: Symbol is indirected to another symbol */
	N_FN_SEQ  = 0x0C,    /**< 12: Same as N_FN, for Sequent compilers */
	N_COMM    = 0x12,    /**< 18: Common--visible after shared library dynamic link */
	N_SETA    = 0x14,    /**< 20: Absolute set element */
	N_SETT    = 0x17,    /**< 23: Text segment set element */
	N_SETD    = 0x18,    /**< 24: Data segment set element */
	N_SETB    = 0x1A,    /**< 26: BSS segment set element */
	N_SETV    = 0x1C,    /**< 28: Pointer to set vector */
	N_WARNING = 0x1E,    /**< 30: Print a warning message during linking */
	N_FN      = 0x1F,    /**< 31: File name of a `.o' file */
	N_GSYM    = 0x20,    /**< 32: Global symbol */
	N_FNAME   = 0x22,    /**< 34: Function name (for BSD Fortran) */
	N_FUN     = 0x24,    /**< 36: Function name */
	N_STSYM   = 0x26,    /**< 38: Data segment file-scope variable */
	N_LCSYM   = 0x28,    /**< 40: BSS segment file-scope variable */
	N_MAIN    = 0x2A,    /**< 42: Name of main routine */
	N_ROSYM   = 0x2C,    /**< 44: Variable in .rodata section */
	N_PC      = 0x30,    /**< 48: Global symbol (for Pascal) */
	N_NSYMS   = 0x32,    /**< 50: Number of symbols (according to Ultrix V4.0) */
	N_NOMAP   = 0x34,    /**< 52: No DST map */
	N_OBJ     = 0x38,    /**< 56: Object file (Solaris2) */
	N_OPT     = 0x3C,    /**< 60: Debugger options (Solaris2) */
	N_RSYM    = 0x40,    /**< 64: Register variable */
	N_M2C     = 0x42,    /**< 66: Modula-2 compilation unit */
	N_SLINE   = 0x44,    /**< 68: Line number in text segment */
	N_DSLINE  = 0x46,    /**< 70: Line number in data segment */
	N_BSLINE  = 0x48,    /**< 72: Line number in bss segment */
	N_BROWS   = 0x48,    /**< 72: Sun source code browser, path to `.cb' file */
	N_DEFD    = 0x4A,    /**< 74: GNU Modula2 definition module dependency */
	N_FLINE   = 0x4C,    /**< 76: Function start/body/end line numbers (Solaris2) */
	N_EHDECL  = 0x50,    /**< 80: GNU C++ exception variable */
	N_MOD2    = 0x50,    /**< 80: Modula2 info "for imc" (according to Ultrix V4.0) */
	N_CATCH   = 0x54,    /**< 84: GNU C++ catch clause */
	N_SSYM    = 0x60,    /**< 96: Structure of union element */
	N_ENDM    = 0x62,    /**< 98: Last stab for module (Solaris2) */
	N_SO      = 0x64,    /**< 100: Path and name of source file */
	N_LSYM    = 0x80,    /**< 128: Stack variable */
	N_BINCL   = 0x82,    /**< 130: Beginning of an include file (Sun only) */
	N_SOL     = 0x84,    /**< 132: Name of include file */
	N_PSYM    = 0xA0,    /**< 160: Parameter variable */
	N_EINCL   = 0xA2,    /**< 162: End of an include file */
	N_ENTRY   = 0xA4,    /**< 164: Alternate entry point */
	N_LBRAC   = 0xC0,    /**< 192: Beginning of a lexical block */
	N_EXCL    = 0xC2,    /**< 194: Place holder for a deleted include file */
	N_SCOPE   = 0xC4,    /**< 196: Modula2 scope information (Sun linker) */
	N_RBRAC   = 0xE0,    /**< 224: End of a lexical block */
	N_BCOMM   = 0xE2,    /**< 226: Begin named common block */
	N_ECOMM   = 0xE4,    /**< 228: End named common block */
	N_ECOML   = 0xE8,    /**< 232: Member of a common block */
	N_WITH    = 0xEA,    /**< 234: Pascal with statement: type,,0,0,offset (Solaris2) */
	N_NBTEXT  = 0xF0,    /**< 240: Gould non-base registers */
	N_NBDATA  = 0xF2,    /**< 242: Gould non-base registers */
	N_NBBSS   = 0xF4,    /**< 244: Gould non-base registers */
	N_NBSTS   = 0xF6,    /**< 246: Gould non-base registers */
	N_NBLCS   = 0xF8,    /**< 248: Gould non-base registers */
};

/**
 * The stabs handle.
 */
typedef struct stabs_handle {
	dbg_handle              base;         /**< the base class */
	FILE                    *f;           /**< the file write to */
	ir_entity               *cur_ent;     /**< current method entity */
	const be_stack_layout_t *layout;      /**< current stack layout */
	unsigned                next_type_nr; /**< next type number */
	pmap                    *type_map;    /**< a map from type to type number */
	const char              *main_file;   /**< name of the main source file */
	const char              *curr_file;   /**< name of teh current source file */
} stabs_handle;

/**
 * Returns the stabs type number of a Firm type.
 */
static unsigned get_type_number(stabs_handle *h, ir_type *tp) {
	pmap_entry *entry;
	unsigned num;

	if (tp == NULL) {
		/* map to the void type */
		return 0;
	}
	entry = pmap_find(h->type_map, tp);
	if (! entry) {
		num = h->next_type_nr++;
		pmap_insert(h->type_map, tp, INT_TO_PTR(num));
	} else {
		num = PTR_TO_INT(entry->value);
	}
	return num;
}  /* get_type_number */

/**
 * generate the void type.
 */
static void gen_void_type(stabs_handle *h) {
	fprintf(h->f, "\t.stabs\t\"void:t%u=%u\",%d,0,0,0\n", 0, 0, N_LSYM);
}  /* gen_void_type */

typedef struct walker_env {
	stabs_handle *h;
	waitq        *wq;
} wenv_t;

/* a type is not ready: put it on the wait queue */
#define SET_TYPE_NOT_READY(wq, tp) \
  do { \
    set_type_link(tp, (void *)1);  \
    waitq_put(wq, tp);             \
  } while(0)

/* a the is ready */
#define SET_TYPE_READY(tp)     set_type_link(tp, NULL)

/* check whether a type is ready */
#define IS_TYPE_READY(tp)      (get_type_link(tp) == NULL)

#ifdef EXPLICITE_PTR_TYPES
#define SKIP_PTR(tp)  tp
#else
#define SKIP_PTR(tp)   (is_Pointer_type(tp) ? get_pointer_points_to_type(tp) : tp)
#endif

/**
 * Generates a primitive type.
 *
 * @param h    the stabs handle
 * @param tp   the type
 */
static void gen_primitive_type(stabs_handle *h, ir_type *tp)
{
	ir_mode *mode = get_type_mode(tp);
	unsigned type_num;

	SET_TYPE_READY(tp);
	if (mode == mode_T) {
    /* jack and FirmJC compiler use mode_T for the void type.
		Ignore it here as it's name is remapped to "void". */
		return;
	}  /* if */

	if (0 && get_mode_size_bits(mode) & 7) {
		/* this is a bitfield type, ignore it */
		return;
	}  /* if */

	type_num = get_type_number(h, tp);

	if (mode_is_int(mode)) {
		char buf[64];
		fprintf(h->f, "\t.stabs\t\"%s:t%u=r%u;", get_type_name(tp), type_num, type_num);
		tarval_snprintf(buf, sizeof(buf), get_mode_min(mode));
		fprintf(h->f, "%s;", buf);
		tarval_snprintf(buf, sizeof(buf), get_mode_max(mode));
		fprintf(h->f, "%s;\",%d,0,0,0\n", buf, N_LSYM);
	} else if (mode_is_float(mode)) {
		int size = get_type_size_bytes(tp);
		fprintf(h->f, "\t.stabs\t\"%s:t%u=r1;%d;0;\",%d,0,0,0\n", get_type_name(tp), type_num, size, N_LSYM);
	}
}  /* gen_primitive_type */

/**
 * Generates an enum type
 *
 * @param h    the stabs handle
 * @param tp   the type
 */
static void gen_enum_type(stabs_handle *h, ir_type *tp) {
	unsigned type_num = get_type_number(h, tp);
	int i, n;

	SET_TYPE_READY(tp);
	fprintf(h->f, "\t.stabs\t\"%s:T%u=e", get_type_name(tp), type_num);
	for (i = 0, n = get_enumeration_n_enums(tp); i < n; ++i) {
		ir_enum_const *ec = get_enumeration_const(tp, i);
		char buf[64];

		tarval_snprintf(buf, sizeof(buf), get_enumeration_value(ec));
		fprintf(h->f, "%s:%s,", get_enumeration_name(ec), buf);
	}
	fprintf(h->f, ";\",%d,0,0,0\n", N_LSYM);
}  /* gen_enum_type */

/**
 * Generates a pointer type
 *
 * @param env  the walker environment
 * @param tp   the type
 */
static void gen_pointer_type(wenv_t *env, ir_type *tp) {
	stabs_handle *h       = env->h;
	unsigned     type_num = get_type_number(h, tp);
	ir_type      *el_tp   = get_pointer_points_to_type(tp);
	unsigned     el_num   = get_type_number(h, el_tp);

	SET_TYPE_READY(tp);
	if (! IS_TYPE_READY(el_tp))
		waitq_put(env->wq, el_tp);
	fprintf(h->f, "\t.stabs\t\"%s:t%u=*%u\",%d,0,0,0\n",
		get_type_name(tp), type_num, el_num, N_LSYM);
}  /* gen_pointer_type */

/**
 * Generates a struct/union type
 *
 * @param env  the walker environment
 * @param tp   the type
 */
static void gen_struct_union_type(wenv_t *env, ir_type *tp) {
	stabs_handle *h       = env->h;
	unsigned     type_num = get_type_number(h, tp);
	int          i, n;
	char         desc = 's';

	SET_TYPE_READY(tp);
	if (is_Struct_type(tp)) {
		desc = 's';
		if (get_type_mode(tp) != NULL) {
			/* this is a bitfield type, ignore it safely */
			return;
		}
	}
	else if (is_Union_type(tp))
		desc = 'u';

	fprintf(h->f, "\t.stabs\t\"%s:Tt%u=%c%d",
		get_type_name(tp), type_num, desc, get_type_size_bytes(tp));

	for (i = 0, n = get_compound_n_members(tp); i < n; ++i) {
		ir_entity *ent = get_compound_member(tp, i);
		ir_type   *mtp = get_entity_type(ent);
		int ofs, size;

		if (! IS_TYPE_READY(mtp))
			waitq_put(env->wq, mtp);
		ofs  = get_entity_offset(ent);
		if (is_Struct_type(mtp) && get_type_mode(mtp) != NULL) {
			/* this structure is a bitfield, skip */
			int i, n;

			for (i = 0, n = get_struct_n_members(mtp); i < n; ++i) {
				ir_entity *ent = get_struct_member(mtp, i);
				ir_type *tp = get_entity_type(ent);
				int bofs;

				type_num = get_type_number(h, tp);
				size = get_type_size_bits(tp);
				bofs = (ofs + get_entity_offset(ent)) * 8 + get_entity_offset_bits_remainder(ent);

				/* name:type, bit offset from the start of the struct', number of bits in the element. */
				fprintf(h->f, "%s:%u,%d,%d;", get_entity_name(ent), type_num, bofs, size);
			}
		} else {
			/* no bitfield */
			type_num = get_type_number(h, mtp);
			size = get_type_size_bits(mtp);

			/* name:type, bit offset from the start of the struct', number of bits in the element. */
			fprintf(h->f, "%s:%u,%d,%d;", get_entity_name(ent), type_num, ofs, size);
		}
	}
	fprintf(h->f, ";\",%d,0,0,0\n", N_LSYM);
}  /* gen_struct_type */

/**
 * Generates an array type
 *
 * @param env  the walker environment
 * @param tp   the type
 */
static void gen_array_type(wenv_t *env, ir_type *tp) {
	stabs_handle *h       = env->h;
	unsigned     type_num = get_type_number(h, tp);
	int          i, n = get_array_n_dimensions(tp);
	ir_type      *etp = get_array_element_type(tp);
	int          *perm;

	SET_TYPE_READY(tp);
	if (! IS_TYPE_READY(etp))
		waitq_put(env->wq, etp);

	NEW_ARR_A(int, perm, n);
	for (i = 0; i < n; ++i) {
		perm[i] = get_array_order(tp, i);
	}
	fprintf(h->f, "\t.stabs\t\"%s:t%u=a", get_type_name(tp), type_num);

	for (i = 0; i < n; ++i) {
		int dim = perm[i];

		if (is_Const(get_array_lower_bound(tp, dim)) && is_Const(get_array_upper_bound(tp, dim))) {
			long min = get_array_lower_bound_int(tp, dim);
			long max = get_array_upper_bound_int(tp, dim);

			/* FIXME r1 must be integer type, but seems to work for now */
			fprintf(h->f, "r1;%ld;%ld;", min, max-1);
		}
	}

	type_num = get_type_number(h, etp);
	fprintf(h->f, "%d\",%d,0,0,0\n", type_num, N_LSYM);
}  /* gen_array_type */

/**
 * Generates a method type
 *
 * @param env  the walker environment
 * @param tp   the type
 */
static void gen_method_type(wenv_t *env, ir_type *tp) {
	stabs_handle *h       = env->h;
	unsigned     type_num = get_type_number(h, tp);
	ir_type *rtp = NULL;
	unsigned res_type_num;
	int i, n = get_method_n_ress(tp);

	SET_TYPE_READY(tp);
	if (n > 0) {
		rtp = get_method_res_type(tp, 0);
		if (! IS_TYPE_READY(rtp))
			waitq_put(env->wq, rtp);
	}
	res_type_num = get_type_number(h, rtp);

	fprintf(h->f, "\t.stabs\t\"%s:t%u=f%u", get_type_name(tp), type_num, res_type_num);

	/* handle more than one return type */
	for (i = 1; i < n; ++i) {
		rtp = get_method_res_type(tp, i);
		if (! IS_TYPE_READY(rtp))
			waitq_put(env->wq, rtp);
		res_type_num = get_type_number(h, rtp);
		fprintf(h->f, ",%u", res_type_num);
	}
	fprintf(h->f, "\",%d,0,0,0\n", N_LSYM);
}  /* gen_method_type */

/**
 * type-walker: generate declaration for simple types,
 * put all other types on a wait queue
 */
static void walk_type(type_or_ent *tore, void *ctx)
{
	wenv_t *env = ctx;
	ir_type  *tp;

	if (get_kind(tore) == k_type) {
		tp = (ir_type *)tore;

		/* ignore the unknown type */
		if (tp == firm_unknown_type)
			return;
	} else {
		return;
	}  /* if */

	switch (get_type_tpop_code(tp)) {
	case tpo_class:
		if (tp == get_glob_type()) {
			SET_TYPE_READY(tp);
			break;
		}
		/* fall through */
	case tpo_struct:
	case tpo_union:
		gen_struct_union_type(env, tp);
		break;

	case tpo_enumeration:
		gen_enum_type(env->h, tp);
		break;

	case tpo_primitive:
		gen_primitive_type(env->h, tp);
		break;

	case tpo_method:
		gen_method_type(env, tp);
		break;

	case tpo_array:
		gen_array_type(env, tp);
		break;

	case tpo_pointer:
		gen_pointer_type(env, tp);
		break;

	case tpo_unknown:
		/* the unknown type: ignore */
		SET_TYPE_READY(tp);
		break;
	default:
		assert(! "Unknown tpop code");
	}  /* switch */
}  /* walk_type */

/**
 * generate declaration for all types
 */
static void finish_types(wenv_t *env)
{
	waitq *wq = env->wq;
	ir_type *tp;

	while (! waitq_empty(wq)) {
		tp = waitq_get(wq);
		if (IS_TYPE_READY(tp))
			continue;

		switch (get_type_tpop_code(tp)) {
		case tpo_method:
			gen_method_type(env, tp);
			break;
		case tpo_class:
		case tpo_union:
		case tpo_struct:
			gen_struct_union_type(env, tp);
			break;
		case tpo_enumeration:
			gen_enum_type(env->h, tp);
			break;
		case tpo_primitive:
			gen_primitive_type(env->h, tp);
			break;
		case tpo_array:
			gen_array_type(env, tp);
			break;
		case tpo_pointer:
			gen_pointer_type(env, tp);
			break;
		case tpo_unknown:
			/* the unknown type: ignore */
			SET_TYPE_READY(tp);
			break;
		default:
			assert(! "Unknown tpop code");
		}  /* switch */
	}  /* while */
}  /* finish_types */

/**
 * generate all types.
 */
static void gen_types(stabs_handle *h) {
	wenv_t env;

	env.h  = h;
	env.wq = new_waitq();
	type_walk(NULL, walk_type, &env);
	finish_types(&env);
	del_waitq(env.wq);
}  /* gen_types */


/* -------------------------- I/F ----------------------------- */

/**
 * start a new source object (compilation unit)
 */
static void stabs_so(dbg_handle *handle, const char *filename) {
	stabs_handle *h = (stabs_handle *)handle;
	h->main_file = h->curr_file = filename;
	fprintf(h->f, "\t.stabs\t\"%s\",%d,0,0,.Ltext0\n", filename, N_SO);
}  /* stabs_so */

/**
 * end an include file
 */
static void stabs_include_end(dbg_handle *handle) {
	stabs_handle *h = (stabs_handle *)handle;
	h->curr_file = h->main_file;
}  /* stabs_include_end */

/**
 * start an include file
 */
static void stabs_include_begin(dbg_handle *handle, const char *filename) {
	stabs_handle *h = (stabs_handle *)handle;
	if (h->main_file != h->curr_file)
		stabs_include_end(handle);
	h->curr_file = filename;
	fprintf(h->f, "\t.stabs\t\"%s\",%d,0,0,0\n", filename, N_SOL);
}  /* stabs_include_begin */

/**
 * Main Program
 */
static void stabs_main_program(dbg_handle *handle) {
	stabs_handle *h = (stabs_handle *)handle;
	ir_graph *irg = get_irp_main_irg();
	if (irg) {
		fprintf(h->f, "\t.stabs\t\"%s\",%d,0,0,0\n", get_entity_name(get_irg_entity(irg)), N_MAIN);
	}
}  /* stabs_main_program */

/**
 * prints a line number
 */
static void stabs_line(dbg_handle *handle, unsigned lineno, const char *address) {
	stabs_handle *h = (stabs_handle *)handle;
	fprintf(h->f, "\t.stabn\t%d, 0, %u, %s-%s\n", N_SLINE, lineno, address, get_entity_ld_name(h->cur_ent));
}  /* stabs_line */

/**
 * dump the stabs for a method begin
 */
static void stabs_method_begin(dbg_handle *handle, ir_entity *ent, const be_stack_layout_t *layout) {
	stabs_handle *h = (stabs_handle *)handle;
	ir_type      *mtp, *rtp;
	unsigned     type_num;
	int          i, n, between_size;

	h->cur_ent = ent;
	h->layout  = layout;

	/* create the method entry */
	mtp = get_entity_type(ent);
	if (is_lowered_type(mtp))
		mtp = get_associated_type(mtp);
	if (get_method_n_ress(mtp) > 0)
		rtp = get_method_res_type(mtp, 0);
	else
		rtp = NULL;
	type_num = get_type_number(h, rtp);
	fprintf(h->f, "\t.stabs\t\"%s:%c%u\",%u,0,0,%s\n",
		get_entity_name(ent),
		get_entity_visibility(ent) == visibility_external_visible ? 'F' : 'f',
		type_num,
		N_FUN,
		get_entity_ld_name(ent));

	/* create parameter entries */
	between_size = get_type_size_bytes(layout->between_type);
	for (i = 0, n = get_method_n_params(mtp); i < n; ++i) {
		ir_type *ptp      = get_method_param_type(mtp, i);
        const char *name  = get_method_param_name(mtp, i);
		unsigned type_num = get_type_number(h, ptp);
        char buf[16];
        int ofs = 0;
		ir_entity *stack_ent;

        if (! name) {
          snprintf(buf, sizeof(buf), "arg%d", i);
          name = buf;
        }
		/* check if this parameter has a stack entity. If it has, it
		   it transmitted on the stack, else in a register */
		stack_ent = layout->param_map[i];
		if (stack_ent) {
			ofs = get_entity_offset(stack_ent) + between_size;
		}
		fprintf(h->f, "\t.stabs\t\"%s:p%u\",%d,0,0,%d\n", name, type_num, N_PSYM, ofs);
	}
}  /* stabs_method_begin */

/**
 * dump the stabs for a method end
 */
static void stabs_method_end(dbg_handle *handle) {
	stabs_handle            *h = (stabs_handle *)handle;
	ir_entity               *ent = h->cur_ent;
	const be_stack_layout_t *layout = h->layout;
	const char              *ld_name = get_entity_ld_name(ent);
	int                     i, n, frame_size;
	static unsigned         scope_nr = 0;

	/* create entries for automatic variables on the stack */
	frame_size = get_type_size_bytes(layout->frame_type);
	for (i = 0, n = get_compound_n_members(layout->frame_type); i < n; ++i) {
		ir_entity *ent = get_compound_member(layout->frame_type, i);
		ir_type *tp;
		int ofs;
		unsigned type_num;

		/* ignore spill slots and other helper objects */
		if (is_entity_compiler_generated(ent))
			continue;

		tp = get_entity_type(ent);
		/* should not happen in backend but ... */
		if (is_Method_type(tp))
			continue;
		type_num = get_type_number(h, tp);
		ofs      = -frame_size + get_entity_offset(ent);

		fprintf(h->f, "\t.stabs\t\"%s:%u\",%d,0,0,%d\n",
			get_entity_name(ent), type_num, N_LSYM, ofs);
	}
	/* we need a lexical block here */
	fprintf(h->f, "\t.stabn\t%d,0,0,%s-%s\n", N_LBRAC, ld_name, ld_name);
	fprintf(h->f, "\t.stabn\t%d,0,0,.Lscope%u-%s\n", N_RBRAC, scope_nr, ld_name);
	fprintf(h->f, ".Lscope%u:\n", scope_nr);
	++scope_nr;

	h->cur_ent = NULL;
	h->layout  = NULL;
}  /* stabs_method_end */

/**
 * dump types
 */
static void stabs_types(dbg_handle *handle) {
	stabs_handle *h = (stabs_handle *)handle;

	/* allocate the zero for the void type */
	h->next_type_nr++;
	gen_void_type(h);
	gen_types(h);
}  /* stabs_types */

/**
 * dump a variable in the global type
 */
static void stabs_variable(dbg_handle *handle, struct obstack *obst, ir_entity *ent) {
	stabs_handle *h = (stabs_handle *)handle;
	unsigned tp_num = get_type_number(h, get_entity_type(ent));
	char buf[1024];

	if (get_entity_visibility(ent) == visibility_external_visible) {
		/* a global variable */
		snprintf(buf, sizeof(buf), "\t.stabs\t\"%s:G%u\",%d,0,0,0\n",
			get_entity_name(ent), tp_num, N_GSYM);
	} else { /* some kind of local */
		ir_variability variability = get_entity_variability(ent);
		int kind = N_STSYM;

		if (variability == variability_uninitialized)
			kind = N_LCSYM;
		else if (variability == variability_constant)
			kind = N_ROSYM;
		snprintf(buf, sizeof(buf), "\t.stabs\t\"%s:S%u\",%d,0,0,%s\n",
			get_entity_name(ent), tp_num, kind, get_entity_ld_name(ent));
	}
	buf[sizeof(buf) - 1] = '\0';

	if (obst)
		obstack_printf(obst, "%s", buf);
	else
		fprintf(h->f, "%s", buf);
}  /* stabs_variable */

/**
 * Close the stabs handler.
 */
static void stabs_close(dbg_handle *handle) {
	stabs_handle *h = (stabs_handle *)handle;
	pmap_destroy(h->type_map);
	free(h);
}  /* stabs_close */

/** The stabs operations. */
static const debug_ops stabs_ops = {
	stabs_close,
	stabs_so,
    stabs_include_begin,
    stabs_include_end,
	stabs_main_program,
	stabs_method_begin,
	stabs_method_end,
	stabs_line,
	stabs_types,
	stabs_variable
};

/* Opens the NULL handler */
dbg_handle *be_nulldbg_open(void) {
	return NULL;
}  /* be_nulldbg_open */

/* Opens a stabs handler */
dbg_handle *be_stabs_open(FILE *out) {
	stabs_handle *h = xmalloc(sizeof(*h));

	h->base.ops     = &stabs_ops;
	h->f            = out;
	h->cur_ent      = NULL;
	h->layout       = NULL;
	h->next_type_nr = 0;
	h->type_map     = pmap_create_ex(64);
	h->main_file    = NULL;
	h->curr_file    = NULL;

	return &h->base;
}  /* stabs_open */

/** close a debug handler. */
void be_dbg_close(dbg_handle *h) {
	if (h && h->ops->close)
		h->ops->close(h);
}  /* be_dbg_close */

/**
 * start a new source object (compilation unit)
 */
void be_dbg_so(dbg_handle *h, const char *filename) {
	if (h && h->ops->so)
		h->ops->so(h, filename);
}  /* be_dbg_begin */

/**
 * start an include file
 */
void be_dbg_include_begin(dbg_handle *h, const char *filename) {
	if (h && h->ops->include_begin)
		h->ops->include_begin(h, filename);
}  /* stabs_include_begin */

/**
 * end an include file
 */
void be_dbg_include_end(dbg_handle *h) {
	if (h && h->ops->include_end)
		h->ops->include_end(h);
}  /* stabs_include_end */

/**
 * Main program
 */
void be_dbg_main_program(dbg_handle *h) {
	if (h && h->ops->main_program)
		h->ops->main_program(h);
}  /* be_dbg_main_program */

/** debug for a method begin */
void be_dbg_method_begin(dbg_handle *h, ir_entity *ent, const be_stack_layout_t *layout) {
	if (h && h->ops->method_begin)
		h->ops->method_begin(h, ent, layout);
}  /* be_dbg_method_begin */

/** debug for a method end */
void be_dbg_method_end(dbg_handle *h) {
	if (h && h->ops->method_end)
		h->ops->method_end(h);
}  /* be_dbg_method_end */

/** debug for line number */
void be_dbg_line(dbg_handle *h, unsigned lineno, const char *address) {
	if (h && h->ops->line)
		h->ops->line(h, lineno, address);
}  /* be_dbg_line */

/** dump types */
void be_dbg_types(dbg_handle *h) {
	if (h && h->ops->types)
		h->ops->types(h);
}  /* be_dbg_types */

/** dump a global */
void be_dbg_variable(dbg_handle *h, struct obstack *obst, ir_entity *ent) {
	if (h && h->ops->variable)
		h->ops->variable(h, obst, ent);
}  /* be_dbg_variable */
