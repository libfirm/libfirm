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

#include "irmode.h"

#include <stdbool.h>
#include "compiler.h"
#include "firm_common.h"

#define get_mode_ident(mode)           get_mode_ident_(mode)
#define get_mode_sort(mode)            get_mode_sort_(mode)
#define get_mode_size_bits(mode)       get_mode_size_bits_(mode)
#define get_mode_size_bytes(mode)      get_mode_size_bytes_(mode)
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

/** Helper values for ir_mode_sort. */
enum ir_mode_sort_helper {
	irmsh_is_num  = 0x10, /**< mode represents a number */
	irmsh_is_data = 0x20, /**< mode represents data that can be in a register */
};

/**
 * These values represent the different mode classes of value representations.
 */
typedef enum ir_mode_sort {
	irms_auxiliary        = 0,
	irms_internal_boolean = 1 | irmsh_is_data,
	irms_data             = 2 | irmsh_is_data,
	irms_reference        = 3 | irmsh_is_data,
	irms_int_number       = 4 | irmsh_is_data | irmsh_is_num,
	irms_float_number     = 5 | irmsh_is_data | irmsh_is_num,
} ir_mode_sort;

/**
 * A descriptor for an IEEE754 float value.
 */
typedef struct float_descriptor_t {
	unsigned char exponent_size;    /**< size of exponent in bits */
	unsigned char mantissa_size;    /**< size of mantissa in bits */
	bool          explicit_one;     /**< set if the leading one is explicit */
} float_descriptor_t;

/**
 * Contains relevant information about a mode.
 *
 * Necessary information about a mode is stored in this struct which is used by
 * the tarval module to perform calculations and comparisons of values of a
 * such described mode.
 */
struct ir_mode {
	firm_kind          kind;       /**< Distinguishes this thing from others */
	ident             *name;       /**< Name ident of this mode */
	ir_type           *type;       /**< Corresponding primitive type */
	ir_mode_sort       sort;       /**< Coarse classification of this mode */
	ir_mode_arithmetic arithmetic; /**< Class of possible arithmetic ops */
	unsigned           size;       /**< Size of the mode in Bits. */
	bool               sign:1;     /**< Whether mode has a sign bit. */
	ENUMBF(float_int_conversion_overflow_style_t)
	                   int_conv_overflow:1;
	/** For shift operations the effective shift amount will be calculated
	 * modulo this value. */
	unsigned           modulo_shift;
	float_descriptor_t float_desc; /**< Floatingpoint descriptor */

	ir_tarval          *min;       /**< smallest representable (real) value */
	ir_tarval          *max;       /**< biggest representable (real) value */
	ir_tarval          *null;      /**< The value 0 */
	ir_tarval          *one;       /**< The value 1 */
	ir_tarval          *all_one;   /**< The value where all bits are set */
	ir_tarval          *infinity;  /**< The (positive) infinity value */
	/** For reference modes, a signed integer mode used to add/subtract
	 * offsets. */
	ir_mode            *offset_mode;
};

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
