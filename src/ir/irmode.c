/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Data modes of operations.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Mathias Heil
 */
#include "irmode_t.h"

#include "array.h"
#include "ident.h"
#include "irhooks.h"
#include "irprog_t.h"
#include "obst.h"
#include "panic.h"
#include "strcalc.h"
#include "tv_t.h"
#include "util.h"
#include <stdbool.h>
#include <stdlib.h>

/** Obstack to hold all modes. */
static struct obstack modes;

/** The list of all currently existing modes. */
static ir_mode **mode_list;

static bool modes_are_equal(const ir_mode *m, const ir_mode *n)
{
	if (m->sort != n->sort)
		return false;
	if (m->sort == irms_auxiliary || m->sort == irms_data)
		return streq(m->name, n->name);
	return m->arithmetic        == n->arithmetic
	    && m->size              == n->size
	    && m->sign              == n->sign
	    && m->modulo_shift      == n->modulo_shift
	    && m->int_conv_overflow == n->int_conv_overflow;
}

/**
 * searches the modes obstack for the given mode and returns
 * a pointer on an equal mode already in the array, NULL if
 * none found
 */
static ir_mode *find_mode(const ir_mode *m)
{
	for (size_t i = 0, n_modes = ARR_LEN(mode_list); i < n_modes; ++i) {
		ir_mode *n = mode_list[i];
		if (modes_are_equal(n, m))
			return n;
	}
	return NULL;
}

ir_mode *mode_T;
ir_mode *mode_X;
ir_mode *mode_M;
ir_mode *mode_BB;
ir_mode *mode_ANY;
ir_mode *mode_BAD;

ir_mode *mode_F;
ir_mode *mode_D;

ir_mode *mode_Bs;
ir_mode *mode_Bu;
ir_mode *mode_Hs;
ir_mode *mode_Hu;
ir_mode *mode_Is;
ir_mode *mode_Iu;
ir_mode *mode_Ls;
ir_mode *mode_Lu;

ir_mode *mode_b;
ir_mode *mode_P;

ir_mode *get_modeT(void)   { return mode_T;   }
ir_mode *get_modeF(void)   { return mode_F;   }
ir_mode *get_modeD(void)   { return mode_D;   }
ir_mode *get_modeBs(void)  { return mode_Bs;  }
ir_mode *get_modeBu(void)  { return mode_Bu;  }
ir_mode *get_modeHs(void)  { return mode_Hs;  }
ir_mode *get_modeHu(void)  { return mode_Hu;  }
ir_mode *get_modeIs(void)  { return mode_Is;  }
ir_mode *get_modeIu(void)  { return mode_Iu;  }
ir_mode *get_modeLs(void)  { return mode_Ls;  }
ir_mode *get_modeLu(void)  { return mode_Lu;  }
ir_mode *get_modeb(void)   { return mode_b;   }
ir_mode *get_modeP(void)   { return mode_P;   }
ir_mode *get_modeX(void)   { return mode_X;   }
ir_mode *get_modeM(void)   { return mode_M;   }
ir_mode *get_modeBB(void)  { return mode_BB;  }
ir_mode *get_modeANY(void) { return mode_ANY; }
ir_mode *get_modeBAD(void) { return mode_BAD; }

void set_modeP(ir_mode *p)
{
	assert(mode_is_reference(p));
	mode_P = p;
}

/*
 * Creates a new mode.
 */
static ir_mode *alloc_mode(const char *name, ir_mode_sort sort,
                           ir_mode_arithmetic arithmetic, unsigned bit_size,
                           int sign, unsigned modulo_shift)
{
	ir_mode *mode_tmpl = OALLOCZ(&modes, ir_mode);

	mode_tmpl->name         = new_id_from_str(name);
	mode_tmpl->sort         = sort;
	mode_tmpl->size         = bit_size;
	mode_tmpl->sign         = sign ? 1 : 0;
	mode_tmpl->modulo_shift = modulo_shift;
	mode_tmpl->arithmetic   = arithmetic;
	return mode_tmpl;
}

static ir_mode *register_mode(ir_mode *mode)
{
	/* does any of the existing modes have the same properties? */
	ir_mode *old = find_mode(mode);
	if (old != NULL) {
		/* remove new mode from obstack */
		obstack_free(&modes, mode);
		return old;
	}

	mode->kind = k_ir_mode;
	mode->type = new_type_primitive(mode);
	ARR_APP1(ir_mode*, mode_list, mode);
	init_mode_values(mode);
	hook_new_mode(mode);
	return mode;
}

ir_mode *new_int_mode(const char *name, unsigned bit_size, int sign,
                      unsigned modulo_shift)
{
	if (bit_size >= (unsigned)sc_get_precision())
		panic("cannot create mode: more bits than tarval module maximum");

	ir_mode *result = alloc_mode(name, irms_int_number, irma_twos_complement,
	                             bit_size, sign, modulo_shift);
	return register_mode(result);
}

ir_mode *new_reference_mode(const char *name, unsigned bit_size,
                            unsigned modulo_shift)
{
	if (bit_size >= (unsigned)sc_get_precision())
		panic("cannot create mode: more bits than tarval module maximum");

	ir_mode *result = alloc_mode(name, irms_reference, irma_twos_complement,
	                             bit_size, 0, modulo_shift);
	ir_mode *res = register_mode(result);

	/* Construct offset mode if none is set yet. */
	if (res->offset_mode == NULL) {
		char buf[16];
		snprintf(buf, sizeof(buf), "I%u", bit_size);
		ir_mode *offset_mode = new_int_mode(buf, bit_size, 1, modulo_shift);
		res->offset_mode = offset_mode;
	}
	return res;
}

ir_mode *new_float_mode(const char *name, ir_mode_arithmetic arithmetic,
                        unsigned exponent_size, unsigned mantissa_size,
                        float_int_conversion_overflow_style_t conv_overflow)
{
	bool     explicit_one = false;
	unsigned bit_size     = exponent_size + mantissa_size + 1;

	if (arithmetic == irma_x86_extended_float) {
		explicit_one = true;
	} else if (arithmetic != irma_ieee754) {
		panic("arithmetic %s invalid for float");
	}
	if (exponent_size >= 256)
		panic("exponents >= 256 bits not supported");
	if (mantissa_size >= 256)
		panic("mantissa >= 256 bits not supported");
	if (exponent_size >= (unsigned)sc_get_precision())
		panic("cannot create mode: more bits than tarval module maximum");
	if (mantissa_size >= (unsigned)sc_get_precision())
		panic("cannot create mode: more bits than tarval module maximum");

	ir_mode *result
		= alloc_mode(name, irms_float_number, arithmetic, bit_size, 1, 0);
	result->int_conv_overflow        = conv_overflow;
	result->float_desc.exponent_size = exponent_size;
	result->float_desc.mantissa_size = mantissa_size;
	result->float_desc.explicit_one  = explicit_one;
	return register_mode(result);
}

ir_mode *new_non_arithmetic_mode(const char *name, unsigned bit_size)
{
	ir_mode *result = alloc_mode(name, irms_data, irma_none, bit_size, 0, 0);
	return register_mode(result);
}

static ir_mode *new_non_data_mode(const char *name)
{
	ir_mode *result = alloc_mode(name, irms_auxiliary, irma_none, 0, 0, 0);
	return register_mode(result);
}

ident *(get_mode_ident)(const ir_mode *mode)
{
	return get_mode_ident_(mode);
}

const char *get_mode_name(const ir_mode *mode)
{
	return get_id_str(mode->name);
}

unsigned (get_mode_size_bits)(const ir_mode *mode)
{
	return get_mode_size_bits_(mode);
}

unsigned (get_mode_size_bytes)(const ir_mode *mode)
{
	return get_mode_size_bytes_(mode);
}

ir_mode_arithmetic (get_mode_arithmetic)(const ir_mode *mode)
{
	return get_mode_arithmetic_(mode);
}

unsigned int (get_mode_modulo_shift)(const ir_mode *mode)
{
	return get_mode_modulo_shift_(mode);
}

ir_tarval *get_mode_min(const ir_mode *mode)
{
	assert(mode_is_data(mode));

	return mode->min;
}

ir_tarval *get_mode_max(const ir_mode *mode)
{
	assert(mode_is_data(mode));

	return mode->max;
}

ir_tarval *get_mode_null(const ir_mode *mode)
{
	assert(mode_is_data(mode));
	return mode->null;
}

ir_tarval *get_mode_one(const ir_mode *mode)
{
	assert(mode_is_data(mode));
	return mode->one;
}

ir_tarval *get_mode_all_one(const ir_mode *mode)
{
	assert(mode_is_data(mode));
	return mode->all_one;
}

ir_tarval *get_mode_infinite(const ir_mode *mode)
{
	assert(mode_is_float(mode));
	return mode->infinity;
}

int (mode_is_signed)(const ir_mode *mode)
{
	return mode_is_signed_(mode);
}

int (mode_is_float)(const ir_mode *mode)
{
	return mode_is_float_(mode);
}

int (mode_is_int)(const ir_mode *mode)
{
	return mode_is_int_(mode);
}

int (mode_is_reference)(const ir_mode *mode)
{
	return mode_is_reference_(mode);
}

int (mode_is_num)(const ir_mode *mode)
{
	return mode_is_num_(mode);
}

int (mode_is_data)(const ir_mode *mode)
{
	return mode_is_data_(mode);
}

unsigned (get_mode_mantissa_size)(const ir_mode *mode)
{
	return get_mode_mantissa_size_(mode);
}

unsigned (get_mode_exponent_size)(const ir_mode *mode)
{
	return get_mode_exponent_size_(mode);
}

float_int_conversion_overflow_style_t get_mode_float_int_overflow(
		const ir_mode *mode)
{
	return mode->int_conv_overflow;
}

int smaller_mode(const ir_mode *sm, const ir_mode *lm)
{
	assert(sm != NULL);
	assert(lm != NULL);
	if (sm == lm) return true;

	switch (get_mode_sort(sm)) {
	case irms_int_number:
		switch (get_mode_sort(lm)) {
		case irms_int_number:
			if (get_mode_arithmetic(sm) != get_mode_arithmetic(lm))
				return false;

			/* only two complement implemented */
			assert(get_mode_arithmetic(sm) == irma_twos_complement);

			/* integers are convertable if
			 * - both have the same sign and lm is the larger one
			 * - lm is signed and is at least one bit larger (the sign) */
			unsigned sm_bits = get_mode_size_bits(sm);
			unsigned lm_bits = get_mode_size_bits(lm);
			if (mode_is_signed(sm)) {
				if (!mode_is_signed(lm))
					return false;
			} else {
				if (mode_is_signed(lm))
					return sm_bits < lm_bits;
			}
			return sm_bits <= lm_bits;

		case irms_auxiliary:
		case irms_data:
		case irms_internal_boolean:
		case irms_reference:
		case irms_float_number:
			/* int to float works if the float is large enough */
			return false;
		}
		panic("invalid mode_sort");

	case irms_float_number:
		return get_mode_arithmetic(sm) == get_mode_arithmetic(lm)
		    && mode_is_float(lm)
		    && get_mode_size_bits(lm) >= get_mode_size_bits(sm);

	case irms_auxiliary:
	case irms_data:
	case irms_internal_boolean:
	case irms_reference:
		/* do exist machines out there with different pointer lengths ?*/
		return false;
	}

	panic("invalid mode_sort");
}

int values_in_mode(const ir_mode *sm, const ir_mode *lm)
{
	assert(sm != NULL);
	assert(lm != NULL);
	if (sm == lm)
		return true;

	if (sm == mode_b)
		return mode_is_int(lm) || mode_is_float(lm);

	ir_mode_arithmetic larith = get_mode_arithmetic(lm);
	ir_mode_arithmetic sarith = get_mode_arithmetic(sm);
	switch (larith) {
	case irma_x86_extended_float:
	case irma_ieee754:
		if (sarith == irma_ieee754 || sarith == irma_x86_extended_float) {
			return get_mode_size_bits(sm) <= get_mode_size_bits(lm);
		} else if (sarith == irma_twos_complement) {
			unsigned int_mantissa
				= get_mode_size_bits(sm) - (mode_is_signed(sm) ? 1 : 0);
			unsigned float_mantissa = get_mode_mantissa_size(lm) + 1;
			return int_mantissa <= float_mantissa;
		}
		break;
	case irma_twos_complement:
		if (sarith == irma_twos_complement)
			return get_mode_size_bits(sm) <= get_mode_size_bits(lm);
		break;
	case irma_none:
		break;
	}
	return false;
}

ir_mode *get_reference_offset_mode(const ir_mode *mode)
{
	assert(mode_is_reference(mode));
	return mode->offset_mode;
}

void set_reference_offset_mode(ir_mode *ref_mode, ir_mode *int_mode)
{
	assert(mode_is_reference(ref_mode));
	assert(mode_is_int(int_mode));
	assert(get_mode_size_bits(ref_mode) == get_mode_size_bits(int_mode));
	ref_mode->offset_mode = int_mode;
}

void init_mode(void)
{
	obstack_init(&modes);
	mode_list = NEW_ARR_F(ir_mode*, 0);

	/* initialize predefined modes */
	mode_BB  = new_non_data_mode("BB");
	mode_X   = new_non_data_mode("X");
	mode_M   = new_non_data_mode("M");
	mode_T   = new_non_data_mode("T");
	mode_ANY = new_non_data_mode("ANY");
	mode_BAD = new_non_data_mode("BAD");
	mode_b   = alloc_mode("b", irms_internal_boolean, irma_none, 1, 0, 0);
	mode_b   = register_mode(mode_b);

	mode_F   = new_float_mode("F", irma_ieee754,  8, 23, ir_overflow_min_max);
	mode_D   = new_float_mode("D", irma_ieee754, 11, 52, ir_overflow_min_max);

	mode_Bs  = new_int_mode("Bs", 8,  1, 32);
	mode_Bu  = new_int_mode("Bu", 8,  0, 32);
	mode_Hs  = new_int_mode("Hs", 16, 1, 32);
	mode_Hu  = new_int_mode("Hu", 16, 0, 32);
	mode_Is  = new_int_mode("Is", 32, 1, 32);
	mode_Iu  = new_int_mode("Iu", 32, 0, 32);
	mode_Ls  = new_int_mode("Ls", 64, 1, 64);
	mode_Lu  = new_int_mode("Lu", 64, 0, 64);
}

ir_mode *find_unsigned_mode(const ir_mode *mode)
{
	ir_mode n = *mode;

	/* allowed for reference mode */
	if (mode->sort == irms_reference)
		n.sort = irms_int_number;

	assert(mode_is_int(&n));
	n.sign = 0;
	return find_mode(&n);
}

ir_mode *find_signed_mode(const ir_mode *mode)
{
	assert(mode_is_int(mode));
	ir_mode n = *mode;
	n.sign = 1;
	return find_mode(&n);
}

ir_mode *find_double_bits_int_mode(const ir_mode *mode)
{
	assert(mode_is_int(mode) && mode->arithmetic == irma_twos_complement);
	ir_mode n = *mode;
	n.size = 2*mode->size;
	if (n.modulo_shift != 0 && n.modulo_shift < n.size)
		n.modulo_shift = n.size;
	return find_mode(&n);
}

int mode_has_signed_zero(const ir_mode *mode)
{
	switch (mode->arithmetic) {
	case irma_ieee754:
	case irma_x86_extended_float:
		return true;
	case irma_none:
	case irma_twos_complement:
		return false;
	}
	panic("invalid arithmetic mode");
}

int mode_overflow_on_unary_Minus(const ir_mode *mode)
{
	switch (mode->arithmetic) {
	case irma_twos_complement:
		return true;
	case irma_ieee754:
	case irma_x86_extended_float:
	case irma_none:
		return false;
	}
	panic("invalid arithmetic mode");
}

int mode_wrap_around(const ir_mode *mode)
{
	switch (mode->arithmetic) {
	case irma_twos_complement:
	case irma_none:
		return true;
	case irma_ieee754:
	case irma_x86_extended_float:
		return false;
	}
	panic("invalid arithmetic mode");
}

int is_reinterpret_cast(const ir_mode *src, const ir_mode *dst)
{
	if (src == dst)
		return true;
	if (get_mode_size_bits(src) != get_mode_size_bits(dst))
		return false;
	ir_mode_arithmetic ma = get_mode_arithmetic(src);
	if (ma != get_mode_arithmetic(dst))
		return false;

	return ma == irma_twos_complement;
}

ir_type *(get_type_for_mode) (const ir_mode *mode)
{
	return get_type_for_mode_(mode);
}

size_t ir_get_n_modes(void)
{
	return ARR_LEN(mode_list);
}

ir_mode *ir_get_mode(size_t num)
{
	assert(num < ARR_LEN(mode_list));
	return mode_list[num];
}

void finish_mode(void)
{
	obstack_free(&modes, 0);
	DEL_ARR_F(mode_list);

	mode_T   = NULL;
	mode_X   = NULL;
	mode_M   = NULL;
	mode_BB  = NULL;
	mode_ANY = NULL;
	mode_BAD = NULL;

	mode_F   = NULL;
	mode_D   = NULL;

	mode_Bs  = NULL;
	mode_Bu  = NULL;
	mode_Hs  = NULL;
	mode_Hu  = NULL;
	mode_Is  = NULL;
	mode_Iu  = NULL;
	mode_Ls  = NULL;
	mode_Lu  = NULL;

	mode_b   = NULL;

	mode_P   = NULL;
}
