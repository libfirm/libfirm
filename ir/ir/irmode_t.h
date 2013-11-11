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

#define get_modeP_code()               get_modeP_code_()
#define get_modeP_data()               get_modeP_data_()
#define get_mode_ident(mode)           get_mode_ident_(mode)
#define get_mode_sort(mode)            get_mode_sort_(mode)
#define get_mode_size_bits(mode)       get_mode_size_bits_(mode)
#define get_mode_size_bytes(mode)      get_mode_size_bytes_(mode)
#define get_mode_sign(mode)            get_mode_sign_(mode)
#define get_mode_arithmetic(mode)      get_mode_arithmetic_(mode)
#define get_mode_modulo_shift(mode)    get_mode_modulo_shift_(mode)
#define get_mode_link(mode)            get_mode_link_(mode)
#define set_mode_link(mode, l)         set_mode_link_(mode, l)
#define mode_is_signed(mode)           mode_is_signed_(mode)
#define mode_is_float(mode)            mode_is_float_(mode)
#define mode_is_int(mode)              mode_is_int_(mode)
#define mode_is_reference(mode)        mode_is_reference_(mode)
#define mode_is_num(mode)              mode_is_num_(mode)
#define mode_is_data(mode)             mode_is_data_(mode)
#define get_type_for_mode(mode)        get_type_for_mode_(mode)
#define get_mode_mantissa_size(mode)   get_mode_mantissa_size_(mode)
#define get_mode_exponent_size(mode)   get_mode_exponent_size_(mode)

static inline ir_mode *get_modeP_code_(void) { return mode_P_code; }

static inline ir_mode *get_modeP_data_(void) { return mode_P_data; }

static inline ident *get_mode_ident_(const ir_mode *mode) { return mode->name; }

static inline ir_mode_sort get_mode_sort_(const ir_mode *mode) { return mode->sort; }

static inline unsigned get_mode_size_bits_(const ir_mode *mode) { return mode->size; }

static inline unsigned get_mode_size_bytes_(const ir_mode *mode)
{
	unsigned size = get_mode_size_bits_(mode);
	if ((size & 7) != 0) return (unsigned) -1;
	return size >> 3;
}

static inline int get_mode_sign_(const ir_mode *mode) { return mode->sign; }

static inline ir_mode_arithmetic get_mode_arithmetic_(const ir_mode *mode) { return mode->arithmetic; }

static inline unsigned int get_mode_modulo_shift_(const ir_mode *mode) { return mode->modulo_shift; }

static inline void *get_mode_link_(const ir_mode *mode) { return mode->link; }

static inline void set_mode_link_(ir_mode *mode, void *l) { mode->link = l; }

/* Functions to check, whether a mode is signed, float, int, num, data.
   For more exact definitions read the corresponding pages
   in the firm documentation or the following enumeration

   The set of "float" is defined as:
   ---------------------------------
   float = {irm_F, irm_D, irm_E}

   The set of "int" is defined as:
   -------------------------------
   int   = {irm_Bs, irm_Bu, irm_Hs, irm_Hu, irm_Is, irm_Iu, irm_Ls, irm_Lu}

   The set of "num" is defined as:
   -------------------------------
   num   = {irm_F, irm_D, irm_E, irm_Bs, irm_Bu, irm_Hs, irm_Hu,
            irm_Is, irm_Iu, irm_Ls, irm_Lu}
            = {float || int}

   The set of "data" is defined as:
   -------------------------------
   data  = {irm_F, irm_D, irm_E irm_Bs, irm_Bu, irm_Hs, irm_Hu,
            irm_Is, irm_Iu, irm_Ls, irm_Lu, irm_C, irm_U, irm_P}
            = {num || irm_C || irm_U || irm_P}
 */

static inline int mode_is_signed_(const ir_mode *mode)
{
	return mode->sign;
}

static inline int mode_is_float_(const ir_mode *mode)
{
	return (get_mode_sort(mode) == irms_float_number);
}

static inline int mode_is_int_(const ir_mode *mode)
{
	return (get_mode_sort(mode) == irms_int_number);
}

static inline int mode_is_reference_(const ir_mode *mode)
{
	return (get_mode_sort(mode) == irms_reference);
}

static inline int mode_is_num_(const ir_mode *mode)
{
	return (get_mode_sort(mode) & irmsh_is_num);
}

static inline int mode_is_data_(const ir_mode *mode)
{
	return (get_mode_sort(mode) & irmsh_is_data);
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
