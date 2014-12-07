/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Data modes of operations -- private header.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Mathias Heil,
 *          Michael Beck
 */
#ifndef FIRM_IR_IRMODE_T_H
#define FIRM_IR_IRMODE_T_H

#include "irtypes.h"
#include "irmode.h"

#define get_mode_ident(mode)           get_mode_ident_(mode)
#define get_mode_sort(mode)            get_mode_sort_(mode)
#define get_mode_size_bits(mode)       get_mode_size_bits_(mode)
#define get_mode_size_bytes(mode)      get_mode_size_bytes_(mode)
#define get_mode_sign(mode)            get_mode_sign_(mode)
#define get_mode_arithmetic(mode)      get_mode_arithmetic_(mode)
#define get_mode_modulo_shift(mode)    get_mode_modulo_shift_(mode)
#define mode_is_signed(mode)           mode_is_signed_(mode)
#define mode_is_float(mode)            mode_is_float_(mode)
#define mode_is_int(mode)              mode_is_int_(mode)
#define mode_is_reference(mode)        mode_is_reference_(mode)
#define mode_is_num(mode)              mode_is_num_(mode)
#define mode_is_data(mode)             mode_is_data_(mode)
#define get_type_for_mode(mode)        get_type_for_mode_(mode)
#define get_mode_mantissa_size(mode)   get_mode_mantissa_size_(mode)
#define get_mode_exponent_size(mode)   get_mode_exponent_size_(mode)

static inline ident *get_mode_ident_(const ir_mode *mode)
{
	return mode->name;
}

static inline ir_mode_sort get_mode_sort_(const ir_mode *mode)
{
	return mode->sort;
}

static inline unsigned get_mode_size_bits_(const ir_mode *mode)
{
	return mode->size;
}

static inline unsigned get_mode_size_bytes_(const ir_mode *mode)
{
	unsigned size = get_mode_size_bits_(mode);
	if ((size & 7) != 0) return (unsigned) -1;
	return size >> 3;
}

static inline int get_mode_sign_(const ir_mode *mode)
{
	return mode->sign;
}

static inline ir_mode_arithmetic get_mode_arithmetic_(const ir_mode *mode)
{
	return mode->arithmetic;
}

static inline unsigned int get_mode_modulo_shift_(const ir_mode *mode)
{
	return mode->modulo_shift;
}

static inline int mode_is_signed_(const ir_mode *mode)
{
	return mode->sign;
}

static inline int mode_is_float_(const ir_mode *mode)
{
	return get_mode_sort(mode) == irms_float_number;
}

static inline int mode_is_int_(const ir_mode *mode)
{
	return get_mode_sort(mode) == irms_int_number;
}

static inline int mode_is_reference_(const ir_mode *mode)
{
	return get_mode_sort(mode) == irms_reference;
}

static inline int mode_is_num_(const ir_mode *mode)
{
	return (get_mode_sort(mode) & irmsh_is_num) != 0;
}

static inline int mode_is_data_(const ir_mode *mode)
{
	return (get_mode_sort(mode) & irmsh_is_data) != 0;
}

static inline ir_type *get_type_for_mode_(const ir_mode *mode)
{
	return mode->type;
}

static inline unsigned get_mode_mantissa_size_(const ir_mode *mode)
{
	return mode->float_desc.mantissa_size;
}

static inline unsigned get_mode_exponent_size_(const ir_mode *mode)
{
	return mode->float_desc.exponent_size;
}

/** mode module initialization, call once before use of any other function **/
void init_mode(void);

/** mode module finalization. frees all memory.  */
void finish_mode(void);

#endif
