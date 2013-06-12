/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Data modes of operations.
 * @author   Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Mathias Heil
 */
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>

#include "irprog_t.h"
#include "irmode_t.h"
#include "ident.h"
#include "tv_t.h"
#include "obst.h"
#include "irhooks.h"
#include "array.h"
#include "error.h"
#include "pattern_dmp.h"

/** Obstack to hold all modes. */
static struct obstack modes;

/** The list of all currently existing modes. */
static ir_mode **mode_list;

static bool modes_are_equal(const ir_mode *m, const ir_mode *n)
{
	return m->sort         == n->sort &&
	       m->arithmetic   == n->arithmetic &&
	       m->size         == n->size &&
	       m->sign         == n->sign &&
	       m->modulo_shift == n->modulo_shift;
}

/**
 * searches the modes obstack for the given mode and returns
 * a pointer on an equal mode already in the array, NULL if
 * none found
 */
static ir_mode *find_mode(const ir_mode *m)
{
	size_t i, n_modes;
	for (i = 0, n_modes = ARR_LEN(mode_list); i < n_modes; ++i) {
		ir_mode *n = mode_list[i];
		if (modes_are_equal(n, m))
			return n;
	}
	return NULL;
}

/**
 * sets special values of modes
 */
static void set_mode_values(ir_mode* mode)
{
	switch (get_mode_sort(mode))    {
	case irms_reference:
	case irms_int_number:
	case irms_float_number:
		mode->min  = get_tarval_min(mode);
		mode->max  = get_tarval_max(mode);
		mode->null = get_tarval_null(mode);
		mode->one  = get_tarval_one(mode);
		mode->minus_one = get_tarval_minus_one(mode);
		if (get_mode_sort(mode) != irms_float_number) {
			mode->all_one = get_tarval_all_one(mode);
		} else {
			mode->all_one = tarval_bad;
		}
		break;

	case irms_internal_boolean:
		mode->min  = tarval_b_false;
		mode->max  = tarval_b_true;
		mode->null = tarval_b_false;
		mode->one  = tarval_b_true;
		mode->minus_one = tarval_bad;
		mode->all_one = tarval_b_true;
		break;

	case irms_control_flow:
	case irms_block:
	case irms_tuple:
	case irms_any:
	case irms_bad:
	case irms_memory:
		mode->min  = tarval_bad;
		mode->max  = tarval_bad;
		mode->null = tarval_bad;
		mode->one  = tarval_bad;
		mode->minus_one = tarval_bad;
		break;
	}
}

ir_mode *mode_T;
ir_mode *mode_X;
ir_mode *mode_M;
ir_mode *mode_BB;
ir_mode *mode_ANY;
ir_mode *mode_BAD;

ir_mode *mode_F;
ir_mode *mode_D;
ir_mode *mode_Q;

ir_mode *mode_Bs;
ir_mode *mode_Bu;
ir_mode *mode_Hs;
ir_mode *mode_Hu;
ir_mode *mode_Is;
ir_mode *mode_Iu;
ir_mode *mode_Ls;
ir_mode *mode_Lu;
ir_mode *mode_LLs;
ir_mode *mode_LLu;

ir_mode *mode_b;
ir_mode *mode_P;

ir_mode *mode_P_code;
ir_mode *mode_P_data;

ir_mode *get_modeT(void) { return mode_T; }
ir_mode *get_modeF(void) { return mode_F; }
ir_mode *get_modeD(void) { return mode_D; }
ir_mode *get_modeQ(void) { return mode_Q; }
ir_mode *get_modeBs(void) { return mode_Bs; }
ir_mode *get_modeBu(void) { return mode_Bu; }
ir_mode *get_modeHs(void) { return mode_Hs; }
ir_mode *get_modeHu(void) { return mode_Hu; }
ir_mode *get_modeIs(void) { return mode_Is; }
ir_mode *get_modeIu(void) { return mode_Iu; }
ir_mode *get_modeLs(void) { return mode_Ls; }
ir_mode *get_modeLu(void) { return mode_Lu; }
ir_mode *get_modeLLs(void){ return mode_LLs; }
ir_mode *get_modeLLu(void){ return mode_LLu; }
ir_mode *get_modeb(void) { return mode_b; }
ir_mode *get_modeP(void) { return mode_P; }
ir_mode *get_modeX(void) { return mode_X; }
ir_mode *get_modeM(void) { return mode_M; }
ir_mode *get_modeBB(void) { return mode_BB; }
ir_mode *get_modeANY(void) { return mode_ANY; }
ir_mode *get_modeBAD(void) { return mode_BAD; }


ir_mode *(get_modeP_code)(void)
{
	return get_modeP_code_();
}

ir_mode *(get_modeP_data)(void)
{
	return get_modeP_data_();
}

void set_modeP_code(ir_mode *p)
{
	assert(mode_is_reference(p));
	mode_P_code = p;
}

void set_modeP_data(ir_mode *p)
{
	assert(mode_is_reference(p));
	mode_P_data = p;
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
	mode_tmpl->link         = NULL;
	mode_tmpl->tv_priv      = NULL;
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
	set_mode_values(mode);
	hook_new_mode(mode);
	return mode;
}

ir_mode *new_int_mode(const char *name, ir_mode_arithmetic arithmetic,
                      unsigned bit_size, int sign, unsigned modulo_shift)
{
	ir_mode *result = alloc_mode(name, irms_int_number, arithmetic, bit_size,
	                             sign, modulo_shift);
	return register_mode(result);
}

ir_mode *new_reference_mode(const char *name, ir_mode_arithmetic arithmetic,
                            unsigned bit_size, unsigned modulo_shift)
{
	ir_mode *result = alloc_mode(name, irms_reference, arithmetic, bit_size,
	                             0, modulo_shift);
	return register_mode(result);
}

ir_mode *new_float_mode(const char *name, ir_mode_arithmetic arithmetic,
                        unsigned exponent_size, unsigned mantissa_size)
{
	bool     explicit_one = false;
	unsigned bit_size     = exponent_size + mantissa_size + 1;
	ir_mode *result;

	if (arithmetic == irma_x86_extended_float) {
		explicit_one = true;
		bit_size++;
	} else if (arithmetic != irma_ieee754) {
		panic("Arithmetic %s invalid for float");
	}
	if (exponent_size >= 256)
		panic("Exponents >= 256 bits not supported");
	if (mantissa_size >= 256)
		panic("Mantissa >= 256 bits not supported");

	result = alloc_mode(name, irms_float_number, arithmetic, bit_size, 1, 0);
	result->float_desc.exponent_size = exponent_size;
	result->float_desc.mantissa_size = mantissa_size;
	result->float_desc.explicit_one  = explicit_one;
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

int (get_mode_sign)(const ir_mode *mode)
{
	return get_mode_sign_(mode);
}

ir_mode_arithmetic (get_mode_arithmetic)(const ir_mode *mode)
{
	return get_mode_arithmetic_(mode);
}


unsigned int (get_mode_modulo_shift)(const ir_mode *mode)
{
	return get_mode_modulo_shift_(mode);
}

void *(get_mode_link)(const ir_mode *mode)
{
	return get_mode_link_(mode);
}

void (set_mode_link)(ir_mode *mode, void *l)
{
	set_mode_link_(mode, l);
}

ir_tarval *get_mode_min(ir_mode *mode)
{
	assert(mode);
	assert(mode_is_data(mode));

	return mode->min;
}

ir_tarval *get_mode_max(ir_mode *mode)
{
	assert(mode);
	assert(mode_is_data(mode));

	return mode->max;
}

ir_tarval *get_mode_null(ir_mode *mode)
{
	assert(mode);
	assert(mode_is_datab(mode));

	return mode->null;
}

ir_tarval *get_mode_one(ir_mode *mode)
{
	assert(mode);
	assert(mode_is_datab(mode));

	return mode->one;
}

ir_tarval *get_mode_minus_one(ir_mode *mode)
{
	assert(mode);
	assert(mode_is_data(mode));

	return mode->minus_one;
}

ir_tarval *get_mode_all_one(ir_mode *mode)
{
	assert(mode);
	assert(mode_is_datab(mode));
	return mode->all_one;
}

ir_tarval *get_mode_infinite(ir_mode *mode)
{
	assert(mode);
	assert(mode_is_float(mode));

	return get_tarval_plus_inf(mode);
}

ir_tarval *get_mode_NAN(ir_mode *mode)
{
	assert(mode);
	assert(mode_is_float(mode));

	return get_tarval_nan(mode);
}

int is_mode(const void *thing)
{
	return get_kind(thing) == k_ir_mode;
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

int (mode_is_datab)(const ir_mode *mode)
{
	return mode_is_datab_(mode);
}

int (mode_is_dataM)(const ir_mode *mode)
{
	return mode_is_dataM_(mode);
}

unsigned (get_mode_mantissa_size)(const ir_mode *mode)
{
	return get_mode_mantissa_size_(mode);
}

unsigned (get_mode_exponent_size)(const ir_mode *mode)
{
	return get_mode_exponent_size_(mode);
}

int smaller_mode(const ir_mode *sm, const ir_mode *lm)
{
	int sm_bits, lm_bits;

	assert(sm);
	assert(lm);

	if (sm == lm) return 1;

	sm_bits = get_mode_size_bits(sm);
	lm_bits = get_mode_size_bits(lm);

	switch (get_mode_sort(sm)) {
	case irms_int_number:
		switch (get_mode_sort(lm)) {
		case irms_int_number:
			if (get_mode_arithmetic(sm) != get_mode_arithmetic(lm))
				return 0;

			/* only two complement implemented */
			assert(get_mode_arithmetic(sm) == irma_twos_complement);

			/* integers are convertable if
			 *   - both have the same sign and lm is the larger one
			 *   - lm is the signed one and is at least two bits larger
			 *     (one for the sign, one for the highest bit of sm)
			 *   - sm & lm are two_complement and lm has greater or equal number of bits
			 */
			if (mode_is_signed(sm)) {
				if (!mode_is_signed(lm))
					return 0;
				return sm_bits <= lm_bits;
			} else {
				if (mode_is_signed(lm)) {
					return sm_bits < lm_bits;
				}
				return sm_bits <= lm_bits;
			}

		case irms_float_number:
			/* int to float works if the float is large enough */
			return 0;

		default:
			break;
		}
		break;

	case irms_float_number:
		if (get_mode_arithmetic(sm) == get_mode_arithmetic(lm)) {
			if ( (get_mode_sort(lm) == irms_float_number)
				&& (get_mode_size_bits(lm) >= get_mode_size_bits(sm)) )
				return 1;
		}
		break;

	case irms_reference:
		/* do exist machines out there with different pointer lengths ?*/
		return 0;

	case irms_internal_boolean:
		return mode_is_int(lm);

	default:
		break;
	}

	/* else */
	return 0;
}

int values_in_mode(const ir_mode *sm, const ir_mode *lm)
{
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
			unsigned int_mantissa   = get_mode_size_bits(sm) - (mode_is_signed(sm) ? 1 : 0);
			unsigned float_mantissa = get_mode_mantissa_size(lm) + 1;
			return int_mantissa <= float_mantissa;
		}
		break;
	case irma_twos_complement:
		if (sarith == irma_twos_complement) {
			return get_mode_size_bits(sm) <= get_mode_size_bits(lm);
		}
		break;
	case irma_none:
		break;
	}
	return false;
}

ir_mode *get_reference_mode_signed_eq(ir_mode *mode)
{
	assert(mode_is_reference(mode));
	return mode->eq_signed;
}

void set_reference_mode_signed_eq(ir_mode *ref_mode, ir_mode *int_mode)
{
	assert(mode_is_reference(ref_mode));
	assert(mode_is_int(int_mode));
	ref_mode->eq_signed = int_mode;
}

ir_mode *get_reference_mode_unsigned_eq(ir_mode *mode)
{
	assert(mode_is_reference(mode));
	return mode->eq_unsigned;
}

void set_reference_mode_unsigned_eq(ir_mode *ref_mode, ir_mode *int_mode)
{
	assert(mode_is_reference(ref_mode));
	assert(mode_is_int(int_mode));
	ref_mode->eq_unsigned = int_mode;
}

static ir_mode *new_internal_mode(const char *name, ir_mode_sort sort)
{
	ir_mode *mode = alloc_mode(name, sort, irma_none, 0, 0, 0);
	return register_mode(mode);
}

void init_mode(void)
{
	obstack_init(&modes);
	mode_list = NEW_ARR_F(ir_mode*, 0);

	/* initialize predefined modes */
	mode_BB  = new_internal_mode("BB",  irms_block);
	mode_X   = new_internal_mode("X",   irms_control_flow);
	mode_M   = new_internal_mode("M",   irms_memory);
	mode_T   = new_internal_mode("T",   irms_tuple);
	mode_ANY = new_internal_mode("ANY", irms_any);
	mode_BAD = new_internal_mode("BAD", irms_bad);
	mode_b   = new_internal_mode("b",   irms_internal_boolean);

	mode_F   = new_float_mode("F", irma_ieee754,  8, 23);
	mode_D   = new_float_mode("D", irma_ieee754, 11, 52);
	mode_Q   = new_float_mode("Q", irma_ieee754, 15, 112);

	mode_Bs  = new_int_mode("Bs",  irma_twos_complement, 8,   1, 32);
	mode_Bu  = new_int_mode("Bu",  irma_twos_complement, 8,   0, 32);
	mode_Hs  = new_int_mode("Hs",  irma_twos_complement, 16,  1, 32);
	mode_Hu  = new_int_mode("Hu",  irma_twos_complement, 16,  0, 32);
	mode_Is  = new_int_mode("Is",  irma_twos_complement, 32,  1, 32);
	mode_Iu  = new_int_mode("Iu",  irma_twos_complement, 32,  0, 32);
	mode_Ls  = new_int_mode("Ls",  irma_twos_complement, 64,  1, 64);
	mode_Lu  = new_int_mode("Lu",  irma_twos_complement, 64,  0, 64);
	mode_LLs = new_int_mode("LLs", irma_twos_complement, 128, 1, 128);
	mode_LLu = new_int_mode("LLu", irma_twos_complement, 128, 0, 128);

	mode_P   = new_reference_mode("P", irma_twos_complement, 32, 32);
	set_reference_mode_signed_eq(mode_P, mode_Is);
	set_reference_mode_unsigned_eq(mode_P, mode_Iu);

	/* set the machine specific modes to the predefined ones */
	mode_P_code = mode_P;
	mode_P_data = mode_P;
}

ir_mode *find_unsigned_mode(const ir_mode *mode)
{
	ir_mode n = *mode;

	/* allowed for reference mode */
	if (mode->sort == irms_reference)
		n.sort = irms_int_number;

	assert(n.sort == irms_int_number);
	n.sign = 0;
	return find_mode(&n);
}

ir_mode *find_signed_mode(const ir_mode *mode)
{
	ir_mode n = *mode;

	assert(mode->sort == irms_int_number);
	n.sign = 1;
	return find_mode(&n);
}

ir_mode *find_double_bits_int_mode(const ir_mode *mode)
{
	ir_mode n = *mode;

	assert(mode->sort == irms_int_number && mode->arithmetic == irma_twos_complement);

	n.size = 2*mode->size;
	return find_mode(&n);
}

int mode_has_signed_zero(const ir_mode *mode)
{
	switch (mode->arithmetic) {
	case irma_ieee754:
	case irma_x86_extended_float:
		return 1;
	case irma_none:
	case irma_twos_complement:
		return 0;
	}
	panic("invalid arithmetic mode");
}

int mode_overflow_on_unary_Minus(const ir_mode *mode)
{
	switch (mode->arithmetic) {
	case irma_twos_complement:
		return 1;
	case irma_ieee754:
	case irma_x86_extended_float:
	case irma_none:
		return 0;
	}
	panic("invalid arithmetic mode");
}

int mode_wrap_around(const ir_mode *mode)
{
	switch (mode->arithmetic) {
	case irma_twos_complement:
	case irma_none:
		return 1;
	case irma_ieee754:
	case irma_x86_extended_float:
		return 0;
	}
	panic("invalid arithmetic mode");
}

int is_reinterpret_cast(const ir_mode *src, const ir_mode *dst)
{
	ir_mode_arithmetic ma;

	if (src == dst)
		return 1;
	if (get_mode_size_bits(src) != get_mode_size_bits(dst))
		return 0;
	ma = get_mode_arithmetic(src);
	if (ma != get_mode_arithmetic(dst))
		return 0;

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

	mode_P      = NULL;
	mode_P_code = NULL;
	mode_P_data = NULL;
}
