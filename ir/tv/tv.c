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

/*
 * Project:     libFIRM
 * File name:   ir/tv/tv.c
 * Purpose:     Representation of and static computations on target machine
 *              values.
 * Author:      Mathias Heil
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003-2007 Universität Karlsruhe
 */

/*
 *    Values are stored in a format depending upon chosen arithmetic
 *    module. Default uses strcalc and fltcalc.
 *
 */

/* This implementation assumes:
 *  - target has IEEE-754 floating-point arithmetic.  */


#ifdef HAVE_CONFIG_H
# include "config.h"
#endif


#include <assert.h>         /* assertions */
#include <stdlib.h>         /* atoi() */
#ifdef HAVE_STRING_H
# include <string.h>         /* nice things for strings */
#endif
#ifdef HAVE_STRINGS_H
#include <strings.h>        /* strings.h also includes bsd only function strcasecmp */
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include "tv_t.h"
#include "set.h"            /* to store tarvals in */
#include "entity_t.h"       /* needed to store pointers to entities */
#include "irmode_t.h"
#include "irnode.h"         /* defines boolean return values (pnc_number)*/
#include "strcalc.h"
#include "fltcalc.h"
#include "irtools.h"
#include "xmalloc.h"
#include "firm_common.h"

/** Size of hash tables.  Should correspond to average number of distinct constant
    target values */
#define N_CONSTANTS 2048

/* get the integer overflow mode */
#define GET_OVERFLOW_MODE() int_overflow_mode

/* unused, float to int doesn't work yet */
enum float_to_int_mode {
	TRUNCATE,
	ROUND
};

#define GET_FLOAT_TO_INT_MODE() TRUNCATE

#define SWITCH_NOINFINITY 0
#define SWITCH_NODENORMALS 0

/****************************************************************************
 *   local definitions and macros
 ****************************************************************************/
#ifndef NDEBUG
#  define TARVAL_VERIFY(a) tarval_verify((a))
#else
#  define TARVAL_VERIFY(a) ((void)0)
#endif

#define INSERT_TARVAL(tv) ((tarval*)set_insert(tarvals, (tv), sizeof(tarval), hash_tv((tv))))
#define FIND_TARVAL(tv) ((tarval*)set_find(tarvals, (tv), sizeof(tarval), hash_tv((tv))))

#define INSERT_VALUE(val, size) (set_insert(values, (val), size, hash_val((val), size)))
#define FIND_VALUE(val, size) (set_find(values, (val), size, hash_val((val), size)))

#define fail_verify(a) _fail_verify((a), __FILE__, __LINE__)

/****************************************************************************
 *   private variables
 ****************************************************************************/
static struct set *tarvals = NULL;   /* container for tarval structs */
static struct set *values = NULL;    /* container for values */
static tarval_int_overflow_mode_t int_overflow_mode = TV_OVERFLOW_WRAP;

/****************************************************************************
 *   private functions
 ****************************************************************************/
#ifndef NDEBUG
static int hash_val(const void *value, unsigned int length);
static int hash_tv(tarval *tv);
static void _fail_verify(tarval *tv, const char* file, int line)
{
	/* print a memory image of the tarval and throw an assertion */
	if (tv)
		printf("%s:%d: Invalid tarval:\n  mode: %s\n value: [%p]\n", file, line, get_mode_name(tv->mode), tv->value);
	else
		printf("%s:%d: Invalid tarval (null)", file, line);
	assert(0);
}
#ifdef __GNUC__
INLINE static void tarval_verify(tarval *tv) __attribute__ ((unused));
#endif

INLINE static void tarval_verify(tarval *tv)
{
	assert(tv);
	assert(tv->mode);
	assert(tv->value);

	if ((tv == tarval_bad) || (tv == tarval_undefined)) return;
	if ((tv == tarval_b_true) || (tv == tarval_b_false)) return;

	if (!FIND_TARVAL(tv)) fail_verify(tv);
	if (tv->length > 0 && !FIND_VALUE(tv->value, tv->length)) fail_verify(tv);

	return;
}
#endif /* NDEBUG */

/** Hash a tarval. */
static int hash_tv(tarval *tv) {
	return (PTR_TO_INT(tv->value) ^ PTR_TO_INT(tv->mode)) + tv->length;
}

/** Hash a value. Treat it as a byte array. */
static int hash_val(const void *value, unsigned int length) {
	unsigned int i;
	unsigned int hash = 0;

	/* scramble the byte - array */
	for (i = 0; i < length; ++i) {
		hash += (hash << 5) ^ (hash >> 27) ^ ((char*)value)[i];
		hash += (hash << 11) ^ (hash >> 17);
	}

	return hash;
}

/** finds tarval with value/mode or creates new tarval */
static tarval *get_tarval(const void *value, int length, ir_mode *mode) {
	tarval tv;

	tv.kind   = k_tarval;
	tv.mode   = mode;
	tv.length = length;
	if (length > 0) {
		/* if there already is such a value, it is returned, else value
		 * is copied into the set */
		tv.value = INSERT_VALUE(value, length);
	} else {
		tv.value = value;
	}
	/* if there is such a tarval, it is returned, else tv is copied
	 * into the set */
	return (tarval *)INSERT_TARVAL(&tv);
}

/**
 * handle overflow
 */
static tarval *get_tarval_overflow(const void *value, int length, ir_mode *mode)
{
	switch (get_mode_sort(mode)) {
	case irms_int_number:
		if (sc_comp(value, get_mode_max(mode)->value) == 1) {
			switch (GET_OVERFLOW_MODE()) {
			case TV_OVERFLOW_SATURATE:
				return get_mode_max(mode);
			case TV_OVERFLOW_WRAP:
				{
					char *temp = alloca(sc_get_buffer_length());
					sc_val_from_ulong(-1, temp);
					sc_and(temp, value, temp);
					/* the sc_ module expects that all bits are set ... */
					sign_extend(temp, mode);
					return get_tarval(temp, length, mode);
				}
			case TV_OVERFLOW_BAD:
				return tarval_bad;
			default:
				return get_tarval(value, length, mode);
			}
		}
		if (sc_comp(value, get_mode_min(mode)->value) == -1) {
			switch (GET_OVERFLOW_MODE()) {
			case TV_OVERFLOW_SATURATE:
				return get_mode_min(mode);
			case TV_OVERFLOW_WRAP: {
				char *temp = alloca(sc_get_buffer_length());
				sc_val_from_ulong(-1, temp);
				sc_and(temp, value, temp);
				return get_tarval(temp, length, mode);
			}
			case TV_OVERFLOW_BAD:
				return tarval_bad;
			default:
				return get_tarval(value, length, mode);
			}
		}
		break;

	case irms_float_number:
		if (SWITCH_NOINFINITY && fc_is_inf(value)) {
			return fc_is_negative(value)?get_mode_min(mode):get_mode_max(mode);
		}

		if (SWITCH_NODENORMALS && fc_is_subnormal(value)) {
			return get_mode_null(mode);
		}
		break;

	default:
		break;
	}
	return get_tarval(value, length, mode);
}

/*
 *   public variables declared in tv.h
 */
static tarval reserved_tv[4];

tarval *tarval_bad       = &reserved_tv[0];
tarval *tarval_undefined = &reserved_tv[1];
tarval *tarval_b_false   = &reserved_tv[2];
tarval *tarval_b_true    = &reserved_tv[3];

/*
 *   public functions declared in tv.h
 */

/*
 * Constructors =============================================================
 */
tarval *new_tarval_from_str(const char *str, size_t len, ir_mode *mode)
{
	assert(str);
	assert(len);
	assert(mode);

	switch (get_mode_sort(mode)) {
	case irms_control_flow:
	case irms_memory:
	case irms_auxiliary:
		assert(0);
		break;

	case irms_internal_boolean:
		/* match [tT][rR][uU][eE]|[fF][aA][lL][sS][eE] */
		if (strcasecmp(str, "true")) return tarval_b_true;
		else if (strcasecmp(str, "false")) return tarval_b_true;
		else
			/* XXX This is C semantics */
			return atoi(str) ? tarval_b_true : tarval_b_false;

	case irms_float_number:
		switch(get_mode_size_bits(mode)) {
		case 32:
			fc_val_from_str(str, len, 8, 23, NULL);
			break;
		case 64:
			fc_val_from_str(str, len, 11, 52, NULL);
			break;
		case 80:
			fc_val_from_str(str, len, 15, 64, NULL);
			break;
		}
		return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);

		case irms_reference:
			/* same as integer modes */
		case irms_int_number:
		case irms_character:
			sc_val_from_str(str, len, NULL, mode);
			return get_tarval(sc_get_buffer(), sc_get_buffer_length(), mode);
	}

	assert(0);  /* can't be reached, can it? */
	return NULL;
}

/*
 * helper function, create a tarval from long
 */
tarval *new_tarval_from_long(long l, ir_mode *mode) {
	assert(mode);

	switch(get_mode_sort(mode))   {
	case irms_internal_boolean:
		/* XXX C semantics ! */
		return l ? tarval_b_true : tarval_b_false ;

	case irms_reference:
		/* same as integer modes */
	case irms_int_number:
	case irms_character:
		sc_val_from_long(l, NULL);
		return get_tarval(sc_get_buffer(), sc_get_buffer_length(), mode);

	case irms_float_number:
		return new_tarval_from_double((long double)l, mode);

	default:
		assert(0 && "unsupported mode sort");
	}
	return NULL;
}

/* returns non-zero if can be converted to long */
int tarval_is_long(tarval *tv) {
	mode_sort sort = get_mode_sort(tv->mode);

	if (sort != irms_int_number && sort != irms_character) return 0;

	if (get_mode_size_bits(tv->mode) > (sizeof(long) << 3)) {
		/* the value might be too big to fit in a long */
		sc_max_from_bits(sizeof(long) << 3, 0, NULL);
		if (sc_comp(sc_get_buffer(), tv->value) == -1) {
			/* really doesn't fit */
			return 0;
		}
	}
	return 1;
}

/* this might overflow the machine's long, so use only with small values */
long get_tarval_long(tarval* tv) {
	assert(tarval_is_long(tv) && "tarval too big to fit in long");

	return sc_val_to_long(tv->value);
}

tarval *new_tarval_from_double(long double d, ir_mode *mode) {
	assert(mode && (get_mode_sort(mode) == irms_float_number));

	switch (get_mode_size_bits(mode)) {
	case 32:
		fc_val_from_float(d, 8, 23, NULL);
		break;
	case 64:
		fc_val_from_float(d, 11, 52, NULL);
		break;
	case 80:
		fc_val_from_float(d, 15, 64, NULL);
		break;
	}
	return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);
}

/* returns non-zero if can be converted to double */
int tarval_is_double(tarval *tv) {
	assert(tv);

	return (get_mode_sort(tv->mode) == irms_float_number);
}

long double get_tarval_double(tarval *tv) {
	assert(tarval_is_double(tv));

	return fc_val_to_float(tv->value);
}


/*
 * Access routines for tarval fields ========================================
 */

/* get the mode of the tarval */
ir_mode *(get_tarval_mode)(const tarval *tv) {
	return _get_tarval_mode(tv);
}

/*
 * Special value query functions ============================================
 *
 * These functions calculate and return a tarval representing the requested
 * value.
 * The functions get_mode_{Max,Min,...} return tarvals retrieved from these
 * functions, but these are stored on initialization of the irmode module and
 * therefore the irmode functions should be prefered to the functions below.
 */

tarval *(get_tarval_bad)(void) {
	return _get_tarval_bad();
}

tarval *(get_tarval_undefined)(void) {
	return _get_tarval_undefined();
}

tarval *(get_tarval_b_false)(void) {
	return _get_tarval_b_false();
}

tarval *(get_tarval_b_true)(void) {
	return _get_tarval_b_true();
}

tarval *get_tarval_max(ir_mode *mode) {
	assert(mode);

	if (get_mode_n_vector_elems(mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	switch(get_mode_sort(mode)) {
	case irms_reference:
	case irms_control_flow:
	case irms_memory:
	case irms_auxiliary:
		assert(0);
		break;

	case irms_internal_boolean:
		return tarval_b_true;

	case irms_float_number:
		switch(get_mode_size_bits(mode)) {
		case 32:
			fc_get_max(8, 23, NULL);
			break;
		case 64:
			fc_get_max(11, 52, NULL);
			break;
		case 80:
			fc_get_max(15, 64, NULL);
			break;
		}
		return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);

		case irms_int_number:
		case irms_character:
			sc_max_from_bits(get_mode_size_bits(mode), mode_is_signed(mode), NULL);
			return get_tarval(sc_get_buffer(), sc_get_buffer_length(), mode);
	}
	return tarval_bad;
}

tarval *get_tarval_min(ir_mode *mode) {
	assert(mode);

	if (get_mode_n_vector_elems(mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	switch(get_mode_sort(mode)) {
	case irms_reference:
	case irms_control_flow:
	case irms_memory:
	case irms_auxiliary:
		assert(0);
		break;

	case irms_internal_boolean:
		return tarval_b_false;

	case irms_float_number:
		switch(get_mode_size_bits(mode)) {
		case 32:
			fc_get_min(8, 23, NULL);
			break;
		case 64:
			fc_get_min(11, 52, NULL);
			break;
		case 80:
			fc_get_min(15, 64, NULL);
			break;
		}
		return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);

		case irms_int_number:
		case irms_character:
			sc_min_from_bits(get_mode_size_bits(mode), mode_is_signed(mode), NULL);
			return get_tarval(sc_get_buffer(), sc_get_buffer_length(), mode);
	}
	return tarval_bad;
}

/** The bit pattern for the pointer NULL */
static long _null_value;

tarval *get_tarval_null(ir_mode *mode) {
	assert(mode);

	if (get_mode_n_vector_elems(mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	switch(get_mode_sort(mode)) {
	case irms_control_flow:
	case irms_memory:
	case irms_auxiliary:
	case irms_internal_boolean:
		assert(0);
		break;

	case irms_float_number:
		return new_tarval_from_double(0.0, mode);

	case irms_int_number:
	case irms_character:
		return new_tarval_from_long(0l,  mode);

	case irms_reference:
		return new_tarval_from_long(_null_value, mode);
	}
	return tarval_bad;
}

tarval *get_tarval_one(ir_mode *mode) {
	assert(mode);

	if (get_mode_n_vector_elems(mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	switch(get_mode_sort(mode)) {
	case irms_control_flow:
	case irms_memory:
	case irms_auxiliary:
	case irms_internal_boolean:
	case irms_reference:
		assert(0);
		break;

	case irms_float_number:
		return new_tarval_from_double(1.0, mode);

	case irms_int_number:
	case irms_character:
		return new_tarval_from_long(1l, mode);
		break;
	}
	return tarval_bad;
}

tarval *get_tarval_minus_one(ir_mode *mode) {
	assert(mode);

	if (get_mode_n_vector_elems(mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	switch(get_mode_sort(mode)) {
	case irms_control_flow:
	case irms_memory:
	case irms_auxiliary:
	case irms_internal_boolean:
	case irms_reference:
		assert(0);
		break;

	case irms_float_number:
		return mode_is_signed(mode) ? new_tarval_from_double(-1.0, mode) : tarval_bad;

	case irms_int_number:
	case irms_character:
		return mode_is_signed(mode) ? new_tarval_from_long(-1l, mode) : tarval_bad;
	}
	return tarval_bad;
}

tarval *get_tarval_nan(ir_mode *mode) {
	assert(mode);

	if (get_mode_n_vector_elems(mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	if (get_mode_sort(mode) == irms_float_number) {
		switch(get_mode_size_bits(mode)) {
		case 32:
			fc_get_qnan(8, 23, NULL);
			break;
		case 64:
			fc_get_qnan(11, 52, NULL);
			break;
		case 80:
			fc_get_qnan(15, 64, NULL);
			break;
		}
		return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);
	} else {
		assert(0 && "tarval is not floating point");
		return tarval_bad;
	}
}

tarval *get_tarval_plus_inf(ir_mode *mode) {
	assert(mode);

	if (get_mode_n_vector_elems(mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	if (get_mode_sort(mode) == irms_float_number) {
		switch(get_mode_size_bits(mode)) {
		case 32:
			fc_get_plusinf(8, 23, NULL);
			break;
		case 64:
			fc_get_plusinf(11, 52, NULL);
			break;
		case 80:
			fc_get_plusinf(15, 64, NULL);
			break;
		}
		return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);
	} else {
		assert(0 && "tarval is not floating point");
		return tarval_bad;
	}
}

tarval *get_tarval_minus_inf(ir_mode *mode) {
	assert(mode);

	if (get_mode_n_vector_elems(mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	if (get_mode_sort(mode) == irms_float_number) {
		switch(get_mode_size_bits(mode)) {
		case 32:
			fc_get_minusinf(8, 23, NULL);
			break;
		case 64:
			fc_get_minusinf(11, 52, NULL);
			break;
		case 80:
			fc_get_minusinf(15, 64, NULL);
			break;
		}
		return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);
	} else {
		assert(0 && "tarval is not floating point");
		return tarval_bad;
	}
}

/*
 * Arithmethic operations on tarvals ========================================
 */

/*
 * test if negative number, 1 means 'yes'
 */
int tarval_is_negative(tarval *a) {
	assert(a);

	if (get_mode_n_vector_elems(a->mode) > 1) {
		/* vector arithmetic not implemented yet */
		assert(0 && "tarval_is_negative is not allowed for vector modes");
		return 0;
	}

	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
		if (!mode_is_signed(a->mode)) return 0;
		else
			return sc_comp(a->value, get_mode_null(a->mode)->value) == -1 ? 1 : 0;

	case irms_float_number:
		return fc_comp(a->value, get_mode_null(a->mode)->value) == -1 ? 1 : 0;

	default:
		assert(0 && "not implemented");
		return 0;
	}
}

/*
 * test if null, 1 means 'yes'
 */
int tarval_is_null(tarval *a) {
	ir_mode *m = get_tarval_mode(a);

	return a == get_tarval_null(m);
}

/*
 * test if one, 1 means 'yes'
 */
int tarval_is_one(tarval *a) {
	ir_mode *m = get_tarval_mode(a);

	return a == get_tarval_one(m);
}

/*
 * comparison
 */
pn_Cmp tarval_cmp(tarval *a, tarval *b) {
	assert(a);
	assert(b);

	if (a == tarval_bad || b == tarval_bad) {
		assert(0 && "Comparison with tarval_bad");
		return pn_Cmp_False;
	}

	if (a == tarval_undefined || b == tarval_undefined)
		return pn_Cmp_False;

	if (a->mode != b->mode)
		return pn_Cmp_False;

	if (get_mode_n_vector_elems(a->mode) > 1) {
		/* vector arithmetic not implemented yet */
		assert(0 && "cmp not implemented for vector modes");
	}

	/* Here the two tarvals are unequal and of the same mode */
	switch (get_mode_sort(a->mode)) {
	case irms_control_flow:
	case irms_memory:
	case irms_auxiliary:
	case irms_reference:
		if (a == b)
			return pn_Cmp_Eq;
		return pn_Cmp_False;

	case irms_float_number:
		/*
		 * BEWARE: we cannot compare a == b here, because
		 * a NaN is always Unordered to any other value, even to itself!
		 */
		switch (fc_comp(a->value, b->value)) {
		case -1: return pn_Cmp_Lt;
		case  0: return pn_Cmp_Eq;
		case  1: return pn_Cmp_Gt;
		case  2: return pn_Cmp_Uo;
		default: return pn_Cmp_False;
		}
	case irms_int_number:
	case irms_character:
		if (a == b)
			return pn_Cmp_Eq;
		return sc_comp(a->value, b->value) == 1 ? pn_Cmp_Gt : pn_Cmp_Lt;

	case irms_internal_boolean:
		if (a == b)
			return pn_Cmp_Eq;
		return a == tarval_b_true ? pn_Cmp_Gt : pn_Cmp_Lt;
	}
	return pn_Cmp_False;
}

/*
 * convert to other mode
 */
tarval *tarval_convert_to(tarval *src, ir_mode *m) {
	char *buffer;

	assert(src);
	assert(m);

	if (src->mode == m) return src;

	if (get_mode_n_vector_elems(src->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	switch (get_mode_sort(src->mode)) {
	case irms_control_flow:
	case irms_memory:
	case irms_auxiliary:
		break;

		/* cast float to something */
	case irms_float_number:
		switch (get_mode_sort(m)) {
		case irms_float_number:
			switch (get_mode_size_bits(m)) {
			case 32:
				fc_cast(src->value, 8, 23, NULL);
				break;
			case 64:
				fc_cast(src->value, 11, 52, NULL);
				break;
			case 80:
				fc_cast(src->value, 15, 64, NULL);
				break;
			default:
				break;
			}
			return get_tarval(fc_get_buffer(), fc_get_buffer_length(), m);

		case irms_int_number:
			switch (GET_FLOAT_TO_INT_MODE()) {
			case TRUNCATE:
				fc_int(src->value, NULL);
				break;
			case ROUND:
				fc_rnd(src->value, NULL);
				break;
			default:
				assert(0);
				break;
			}
			/* FIXME: floating point unit can't produce a value in integer
			 * representation
			 * an intermediate representation is needed here first. */
			/*  return get_tarval(); */
			return tarval_bad;

		default:
			/* the rest can't be converted */
			return tarval_bad;
		}
		break;

	/* cast int/characters to something */
	case irms_int_number:
	case irms_character:
		switch (get_mode_sort(m)) {
		case irms_int_number:
		case irms_character:
			buffer = alloca(sc_get_buffer_length());
			memcpy(buffer, src->value, sc_get_buffer_length());
			sign_extend(buffer, src->mode);
			return get_tarval_overflow(buffer, src->length, m);

		case irms_internal_boolean:
			/* XXX C semantics */
			if (src == get_mode_null(src->mode)) return tarval_b_false;
			else return tarval_b_true;

		case irms_float_number:
			/* XXX floating point unit does not understand internal integer
			 * representation, convert to string first, then create float from
			 * string */
			buffer = alloca(100);
			/* decimal string representation because hexadecimal output is
			 * interpreted unsigned by fc_val_from_str, so this is a HACK */
			snprintf(buffer, 100, "%s",
				sc_print(src->value, get_mode_size_bits(src->mode), SC_DEC, mode_is_signed(src->mode)));
			buffer[100 - 1] = '\0';
			switch (get_mode_size_bits(m)) {
			case 32:
				fc_val_from_str(buffer, 0, 8, 23, NULL);
				break;
			case 64:
				fc_val_from_str(buffer, 0, 11, 52, NULL);
				break;
			case 80:
				fc_val_from_str(buffer, 0, 15, 64, NULL);
				break;
			}
			return get_tarval(fc_get_buffer(), fc_get_buffer_length(), m);

		case irms_reference:
			/* allow 0 to be casted */
			if (src == get_mode_null(src->mode))
				return get_mode_null(m);
			break;

		default:
			break;
		}
		break;

	case irms_internal_boolean:
		switch (get_mode_sort(m)) {
		case irms_int_number:
			if (src == tarval_b_true) return get_mode_one(m);
			else return get_mode_null(m);

		default:
			break;
		}
		break;

	case irms_reference:
		break;
	}

	return tarval_bad;
}

/*
 * bitwise negation
 */
tarval *tarval_not(tarval *a) {
	char *buffer;

	assert(a);

	/* works for vector mode without changes */

	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
		buffer = alloca(sc_get_buffer_length());
		sc_not(a->value, buffer);
		return get_tarval(buffer, a->length, a->mode);

	case irms_internal_boolean:
		if (a == tarval_b_true)
			return tarval_b_false;
		if (a == tarval_b_false)
			return tarval_b_true;
		return tarval_bad;

	default:
		assert(0 && "bitwise negation is only allowed for integer and boolean");
		return tarval_bad;
	}
}

/*
 * arithmetic negation
 */
tarval *tarval_neg(tarval *a) {
	char *buffer;

	assert(a);
	assert(mode_is_num(a->mode)); /* negation only for numerical values */

	/* note: negation is allowed even for unsigned modes. */

	if (get_mode_n_vector_elems(a->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
		buffer = alloca(sc_get_buffer_length());
		sc_neg(a->value, buffer);
		return get_tarval_overflow(buffer, a->length, a->mode);

	case irms_float_number:
		fc_neg(a->value, NULL);
		return get_tarval_overflow(fc_get_buffer(), fc_get_buffer_length(), a->mode);

	default:
		return tarval_bad;
	}
}

/*
 * addition
 */
tarval *tarval_add(tarval *a, tarval *b) {
	char *buffer;

	assert(a);
	assert(b);
	assert(a->mode == b->mode);

	if (get_mode_n_vector_elems(a->mode) > 1 || get_mode_n_vector_elems(b->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	switch (get_mode_sort(a->mode)) {
	case irms_character:
	case irms_int_number:
		/* modes of a,b are equal, so result has mode of a as this might be the character */
		buffer = alloca(sc_get_buffer_length());
		sc_add(a->value, b->value, buffer);
		return get_tarval_overflow(buffer, a->length, a->mode);

	case irms_float_number:
		fc_add(a->value, b->value, NULL);
		return get_tarval_overflow(fc_get_buffer(), fc_get_buffer_length(), a->mode);

	default:
		return tarval_bad;
	}
}

/*
 * subtraction
 */
tarval *tarval_sub(tarval *a, tarval *b) {
	char *buffer;

	assert(a);
	assert(b);
	assert(a->mode == b->mode);

	if (get_mode_n_vector_elems(a->mode) > 1 || get_mode_n_vector_elems(b->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}
	switch (get_mode_sort(a->mode)) {
	case irms_character:
	case irms_int_number:
		/* modes of a,b are equal, so result has mode of a as this might be the character */
		buffer = alloca(sc_get_buffer_length());
		sc_sub(a->value, b->value, buffer);
		return get_tarval_overflow(buffer, a->length, a->mode);

	case irms_float_number:
		fc_sub(a->value, b->value, NULL);
		return get_tarval_overflow(fc_get_buffer(), fc_get_buffer_length(), a->mode);

	default:
		return tarval_bad;
	}
}

/*
 * multiplication
 */
tarval *tarval_mul(tarval *a, tarval *b) {
	char *buffer;

	assert(a);
	assert(b);
	assert(a->mode == b->mode);

	if (get_mode_n_vector_elems(a->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
		/* modes of a,b are equal */
		buffer = alloca(sc_get_buffer_length());
		sc_mul(a->value, b->value, buffer);
		return get_tarval_overflow(buffer, a->length, a->mode);

	case irms_float_number:
		fc_mul(a->value, b->value, NULL);
		return get_tarval_overflow(fc_get_buffer(), fc_get_buffer_length(), a->mode);

	default:
		return tarval_bad;
	}
}

/*
 * floating point division
 */
tarval *tarval_quo(tarval *a, tarval *b) {
	assert(a);
	assert(b);
	assert((a->mode == b->mode) && mode_is_float(a->mode));

	if (get_mode_n_vector_elems(a->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	fc_div(a->value, b->value, NULL);
	return get_tarval_overflow(fc_get_buffer(), fc_get_buffer_length(), a->mode);
}

/*
 * integer division
 * overflow is impossible, but look out for division by zero
 */
tarval *tarval_div(tarval *a, tarval *b) {
	assert(a);
	assert(b);
	assert((a->mode == b->mode) && mode_is_int(a->mode));

	if (get_mode_n_vector_elems(a->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	/* x/0 error */
	if (b == get_mode_null(b->mode)) return tarval_bad;
	/* modes of a,b are equal */
	sc_div(a->value, b->value, NULL);
	return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * remainder
 * overflow is impossible, but look out for division by zero
 */
tarval *tarval_mod(tarval *a, tarval *b) {
	assert(a);
	assert(b);
	assert((a->mode == b->mode) && mode_is_int(a->mode));

	if (get_mode_n_vector_elems(a->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	/* x/0 error */
	if (b == get_mode_null(b->mode)) return tarval_bad;
	/* modes of a,b are equal */
	sc_mod(a->value, b->value, NULL);
	return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * absolute value
 */
tarval *tarval_abs(tarval *a) {
	char *buffer;

	assert(a);
	assert(mode_is_num(a->mode));

	if (get_mode_n_vector_elems(a->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	switch (get_mode_sort(a->mode)) {
	case irms_int_number:
		if (sc_comp(a->value, get_mode_null(a->mode)->value) == -1) {
			buffer = alloca(sc_get_buffer_length());
			sc_neg(a->value, buffer);
			return get_tarval_overflow(buffer, a->length, a->mode);
		}
		return a;

	case irms_float_number:
		if (fc_comp(a->value, get_mode_null(a->mode)->value) == -1) {
			fc_neg(a->value, NULL);
			return get_tarval_overflow(fc_get_buffer(), fc_get_buffer_length(), a->mode);
		}
		return a;

	default:
		return tarval_bad;
	}
	return tarval_bad;
}

/*
 * bitwise and
 */
tarval *tarval_and(tarval *a, tarval *b) {
	assert(a);
	assert(b);
	assert(a->mode == b->mode);

	/* works even for vector modes */

	switch(get_mode_sort(a->mode)) {
	case irms_internal_boolean:
		return (a == tarval_b_false) ? a : b;

	case irms_int_number:
		sc_and(a->value, b->value, NULL);
		return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);

	default:
		assert(0 && "operation not defined on mode");
		return tarval_bad;
	}
}

/*
 * bitwise or
 */
tarval *tarval_or (tarval *a, tarval *b) {
	assert(a);
	assert(b);
	assert(a->mode == b->mode);

	/* works even for vector modes */

	switch (get_mode_sort(a->mode)) {
	case irms_internal_boolean:
		return (a == tarval_b_true) ? a : b;

	case irms_int_number:
		sc_or(a->value, b->value, NULL);
		return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);

	default:
		assert(0 && "operation not defined on mode");
		return tarval_bad;
	}
}

/*
 * bitwise exclusive or (xor)
 */
tarval *tarval_eor(tarval *a, tarval *b) {
	assert(a);
	assert(b);
	assert((a->mode == b->mode));

	/* works even for vector modes */

	switch (get_mode_sort(a->mode)) {
	case irms_internal_boolean:
		return (a == b)? tarval_b_false : tarval_b_true;

	case irms_int_number:
		sc_xor(a->value, b->value, NULL);
		return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);

	default:
		assert(0 && "operation not defined on mode");
		return tarval_bad;;
	}
}

/*
 * bitwise left shift
 */
tarval *tarval_shl(tarval *a, tarval *b) {
	char *temp_val = NULL;

	assert(a);
	assert(b);
	assert(mode_is_int(a->mode) && mode_is_int(b->mode));

	if (get_mode_n_vector_elems(a->mode) > 1 || get_mode_n_vector_elems(a->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	if (get_mode_modulo_shift(a->mode) != 0) {
		temp_val = alloca(sc_get_buffer_length());

		sc_val_from_ulong(get_mode_modulo_shift(a->mode), temp_val);
		sc_mod(b->value, temp_val, temp_val);
	} else
		temp_val = (char*)b->value;

	sc_shl(a->value, temp_val, get_mode_size_bits(a->mode), mode_is_signed(a->mode), NULL);
	return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * bitwise unsigned right shift
 */
tarval *tarval_shr(tarval *a, tarval *b) {
	char *temp_val = NULL;

	assert(a);
	assert(b);
	assert(mode_is_int(a->mode) && mode_is_int(b->mode));

	if (get_mode_n_vector_elems(a->mode) > 1 || get_mode_n_vector_elems(a->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	if (get_mode_modulo_shift(a->mode) != 0) {
		temp_val = alloca(sc_get_buffer_length());

		sc_val_from_ulong(get_mode_modulo_shift(a->mode), temp_val);
		sc_mod(b->value, temp_val, temp_val);
	} else
		temp_val = (char*)b->value;

	sc_shr(a->value, temp_val, get_mode_size_bits(a->mode), mode_is_signed(a->mode), NULL);
	return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * bitwise signed right shift
 */
tarval *tarval_shrs(tarval *a, tarval *b) {
	char *temp_val = NULL;

	assert(a);
	assert(b);
	assert(mode_is_int(a->mode) && mode_is_int(b->mode));

	if (get_mode_n_vector_elems(a->mode) > 1 || get_mode_n_vector_elems(a->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	if (get_mode_modulo_shift(a->mode) != 0) {
		temp_val = alloca(sc_get_buffer_length());

		sc_val_from_ulong(get_mode_modulo_shift(a->mode), temp_val);
		sc_mod(b->value, temp_val, temp_val);
	} else
		temp_val = (char*)b->value;

	sc_shrs(a->value, temp_val, get_mode_size_bits(a->mode), mode_is_signed(a->mode), NULL);
	return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * bitwise rotation
 */
tarval *tarval_rot(tarval *a, tarval *b) {
	char *temp_val = NULL;

	assert(a);
	assert(b);
	assert(mode_is_int(a->mode) && mode_is_int(b->mode));

	if (get_mode_n_vector_elems(a->mode) > 1 || get_mode_n_vector_elems(a->mode) > 1) {
		/* vector arithmetic not implemented yet */
		return tarval_bad;
	}

	if (get_mode_modulo_shift(a->mode) != 0) {
		temp_val = alloca(sc_get_buffer_length());

		sc_val_from_ulong(get_mode_modulo_shift(a->mode), temp_val);
		sc_mod(b->value, temp_val, temp_val);
	} else
		temp_val = (char*)b->value;

	sc_rot(a->value, temp_val, get_mode_size_bits(a->mode), mode_is_signed(a->mode), NULL);
	return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * carry flag of the last operation
 */
int tarval_carry(void) {
	return sc_had_carry();
}

/*
 * Output of tarvals
 */
int tarval_snprintf(char *buf, size_t len, tarval *tv) {
	static const tarval_mode_info default_info = { TVO_NATIVE, NULL, NULL };

	const char *str;
	char tv_buf[100];
	const tarval_mode_info *mode_info;
	const char *prefix, *suffix;

	mode_info = tv->mode->tv_priv;
	if (! mode_info)
		mode_info = &default_info;
	prefix = mode_info->mode_prefix ? mode_info->mode_prefix : "";
	suffix = mode_info->mode_suffix ? mode_info->mode_suffix : "";

	switch (get_mode_sort(tv->mode)) {
	case irms_reference:
		if (tv == tv->mode->null) return snprintf(buf, len, "NULL");
		/* fall through */
	case irms_int_number:
	case irms_character:
		switch (mode_info->mode_output) {

		case TVO_DECIMAL:
			str = sc_print(tv->value, get_mode_size_bits(tv->mode), SC_DEC, mode_is_signed(tv->mode));
			break;

		case TVO_OCTAL:
			str = sc_print(tv->value, get_mode_size_bits(tv->mode), SC_OCT, 0);
			break;

		case TVO_HEX:
		case TVO_NATIVE:
		default:
			str = sc_print(tv->value, get_mode_size_bits(tv->mode), SC_HEX, 0);
			break;
		}
		return snprintf(buf, len, "%s%s%s", prefix, str, suffix);

	case irms_float_number:
		switch (mode_info->mode_output) {
		case TVO_HEX:
			return snprintf(buf, len, "%s%s%s", prefix, fc_print(tv->value, tv_buf, sizeof(tv_buf), FC_PACKED), suffix);

		case TVO_HEXFLOAT:
			return snprintf(buf, len, "%s%s%s", prefix, fc_print(tv->value, tv_buf, sizeof(tv_buf), FC_HEX), suffix);

		case TVO_FLOAT:
		case TVO_NATIVE:
		default:
			return snprintf(buf, len, "%s%s%s", prefix, fc_print(tv->value, tv_buf, sizeof(tv_buf), FC_DEC), suffix);
		}
		break;

	case irms_internal_boolean:
		switch (mode_info->mode_output) {

		case TVO_DECIMAL:
		case TVO_OCTAL:
		case TVO_HEX:
		case TVO_BINARY:
			return snprintf(buf, len, "%s%c%s", prefix, (tv == tarval_b_true) ? '1' : '0', suffix);

		case TVO_NATIVE:
		default:
			return snprintf(buf, len, "%s%s%s", prefix, (tv == tarval_b_true) ? "true" : "false", suffix);
		}

	case irms_control_flow:
	case irms_memory:
	case irms_auxiliary:
		return snprintf(buf, len, "<TV_OVERFLOW_BAD>");
	}

	return 0;
}

/**
 * Output of tarvals to stdio.
 */
int tarval_printf(tarval *tv) {
	char buf[1024];
	int res;

	res = tarval_snprintf(buf, sizeof(buf), tv);
	assert(res < sizeof(buf) && "buffer to small for tarval_snprintf");
	printf(buf);
	return res;
}

char *get_tarval_bitpattern(tarval *tv) {
	int i, j, pos = 0;
	int n = get_mode_size_bits(tv->mode);
	int bytes = (n + 7) / 8;
	char *res = xmalloc((n + 1) * sizeof(char));
	unsigned char byte;

	for(i = 0; i < bytes; i++) {
		byte = get_tarval_sub_bits(tv, i);
		for(j = 1; j < 256; j <<= 1)
			if(pos < n)
				res[pos++] = j & byte ? '1' : '0';
	}

	res[n] = '\0';

	return res;
}

/*
 * access to the bitpattern
 */
unsigned char get_tarval_sub_bits(tarval *tv, unsigned byte_ofs) {
	switch (get_mode_sort(tv->mode)) {
	case irms_int_number:
	case irms_character:
		return sc_sub_bits(tv->value, tv->length, byte_ofs);

	case irms_float_number:
		return fc_sub_bits(tv->value, get_mode_size_bits(tv->mode), byte_ofs);

	default:
		return 0;
	}
}

/*
 * Specify the output options of one mode.
 *
 * This functions stores the modinfo, so DO NOT DESTROY it.
 *
 * Returns zero on success.
 */
int  set_tarval_mode_output_option(ir_mode *mode, const tarval_mode_info *modeinfo) {
	assert(mode);

	mode->tv_priv = modeinfo;
	return 0;
}

/*
 * Returns the output options of one mode.
 *
 * This functions returns the mode info of a given mode.
 */
const tarval_mode_info *get_tarval_mode_output_option(ir_mode *mode) {
	assert(mode);

	return mode->tv_priv;
}

/*
 * Identifying tarvals values for algebraic simplifications.
 *
 * Returns:
 *   - TV_CLASSIFY_NULL    for additive neutral or the NULL tarval for reference modes,
 *   - TV_CLASSIFY_ONE     for multiplicative neutral,
 *   - TV_CLASSIFY_ALL_ONE for bitwise-and neutral
 *   - TV_CLASSIFY_OTHER   else
 */
tarval_classification_t classify_tarval(tarval *tv) {
	if (!tv || tv == tarval_bad) return TV_CLASSIFY_OTHER;

	if (tv == get_mode_null(tv->mode))
		return TV_CLASSIFY_NULL;
	else if (tv == get_mode_one(tv->mode))
		return TV_CLASSIFY_ONE;
	else if ((get_mode_sort(tv->mode) == irms_int_number)
		&& (tv == new_tarval_from_long(-1, tv->mode)))
		return TV_CLASSIFY_ALL_ONE;

	return TV_CLASSIFY_OTHER;
}

/*
 * Returns non-zero if a given (integer) tarval has only one single bit
 * set.
 */
int is_single_bit_tarval(tarval *tv) {
	int i, l;
	int bits;

	if (!tv || tv == tarval_bad) return 0;
	if (! mode_is_int(tv->mode)) return 0;

	l = get_mode_size_bytes(tv->mode);
	for (bits = 0, i = l - 1; i >= 0; --i) {
		unsigned char v = get_tarval_sub_bits(tv, (unsigned)i);

		/* check for more than one bit in these */
		if (v) {
			if (v & (v-1))
				return 0;
			if (++bits > 1)
				return 0;
		}
	}
	return bits;
}

/*
 * Sets the overflow mode for integer operations.
 */
void tarval_set_integer_overflow_mode(tarval_int_overflow_mode_t ov_mode) {
	int_overflow_mode = ov_mode;
}

/**
 * Get the overflow mode for integer operations.
 */
tarval_int_overflow_mode_t tarval_get_integer_overflow_mode(void) {
	return int_overflow_mode;
}

/**
 * default mode_info for output as HEX
 */
static const tarval_mode_info hex_output = {
	TVO_HEX,
	"0x",
	NULL,
};

/*
 * Initialization of the tarval module: called before init_mode()
 */
void init_tarval_1(long null_value) {
	_null_value = null_value;

	/* initialize the sets holding the tarvals with a comparison function and
	 * an initial size, which is the expected number of constants */
	tarvals = new_set(memcmp, N_CONSTANTS);
	values  = new_set(memcmp, N_CONSTANTS);
	/* init strcalc with precision of 68 to support floating point values with 64
	 * bit mantissa (needs extra bits for rounding and overflow) */
	init_strcalc(68);
	init_fltcalc(0);
}

/*
 * Initialization of the tarval module: called after init_mode()
 */
void init_tarval_2(void) {
	tarval_bad->kind        = k_tarval;
	tarval_bad->mode        = mode_BAD;
	tarval_bad->value       = INT_TO_PTR(resid_tarval_bad);

	tarval_undefined->kind  = k_tarval;
	tarval_undefined->mode  = mode_ANY;
	tarval_undefined->value = INT_TO_PTR(resid_tarval_undefined);

	tarval_b_true->kind     = k_tarval;
	tarval_b_true->mode     = mode_b;
	tarval_b_true->value    = INT_TO_PTR(resid_tarval_b_true);

	tarval_b_false->kind    = k_tarval;
	tarval_b_false->mode    = mode_b;
	tarval_b_false->value   = INT_TO_PTR(resid_tarval_b_false);

	/*
	 * assign output modes that are compatible with the
	 * old implementation: Hex output
	 */
	set_tarval_mode_output_option(mode_U,  &hex_output);
	set_tarval_mode_output_option(mode_C,  &hex_output);
	set_tarval_mode_output_option(mode_Bs, &hex_output);
	set_tarval_mode_output_option(mode_Bu, &hex_output);
	set_tarval_mode_output_option(mode_Hs, &hex_output);
	set_tarval_mode_output_option(mode_Hu, &hex_output);
	set_tarval_mode_output_option(mode_Is, &hex_output);
	set_tarval_mode_output_option(mode_Iu, &hex_output);
	set_tarval_mode_output_option(mode_Ls, &hex_output);
	set_tarval_mode_output_option(mode_Lu, &hex_output);
	set_tarval_mode_output_option(mode_P,  &hex_output);
	}

/* free all memory occupied by tarval. */
void finish_tarval(void) {
	finish_strcalc ();
	finish_fltcalc ();
	del_set(tarvals); tarvals = NULL;
	del_set(values);  values = NULL;
}

/****************************************************************************
 *   end of tv.c
 ****************************************************************************/
