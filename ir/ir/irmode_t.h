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
 * @brief   Data modes of operations -- private header.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Mathias Heil,
 *          Michael Beck
 * @version $Id$
 */
#ifndef FIRM_IR_IRMODE_T_H
#define FIRM_IR_IRMODE_T_H

#include <assert.h>
#include "irmode.h"
#include "tv.h"

/**
 * Contains relevant information about a mode.
 *
 * Necessary information about a mode is stored in this struct
 * which is used by the tarval module to perform calculations
 * and comparisons of values of a such described mode.
 *
 * ATTRIBUTES:
 *  -  modecode code:           An unambiguous int (enum) for the mode
 *  -  ident *name:             Name of this mode. Two modes are different if the name is different.
 *  -  mode_sort sort:          sort of mode specifying possible usage categories
 *  -  int    size:             size of the mode in Bits.
 *  -  unsigned sign:1:         signedness of this mode
 *  -  ... more to come
 *  -  modulo_shift             specifies for modes of kind irms_int_number
 *                              whether shift applies modulo to value of bits to shift
 *
 * SEE ALSO:
 *    The tech report 1999-44 describing FIRM and predefined modes
 *    tarval.h
 */
struct ir_mode {
	firm_kind         kind;       /**< distinguishes this node from others */
	modecode          code;       /**< unambiguous identifier of a mode */
	ident             *name;      /**< Name ident of this mode */

	/* ----------------------------------------------------------------------- */
	/* On changing this struct you have to evaluate the mode_are_equal function!*/
	mode_sort         sort;          /**< coarse classification of this mode:
                                          int, float, reference ...
                                          (see irmode.h) */
	mode_arithmetic   arithmetic;    /**< different arithmetic operations possible with a mode */
	int               size;          /**< size of the mode in Bits. */
	unsigned          sign:1;        /**< signedness of this mode */
	unsigned int      modulo_shift;  /**< number of bits a values of this mode will be shifted */
	unsigned          vector_elem;   /**< if this is not equal 1, this is a vector mode with
                                          vector_elem number of elements, size contains the size
                                          of all bits and must be dividable by vector_elem */

	/* ----------------------------------------------------------------------- */
	tarval            *min;         /**< the minimum value that can be expressed */
	tarval            *max;         /**< the maximum value that can be expressed */
	tarval            *null;        /**< the value 0 */
	tarval            *one;         /**< the value 1 */
	tarval            *minus_one;   /**< the value -1 */
	ir_mode           *eq_signed;   /**< For pointer modes, the equivalent signed integer one. */
	ir_mode           *eq_unsigned; /**< For pointer modes, the equivalent unsigned integer one. */
	void              *link;        /**< To store some intermediate information */
	const void        *tv_priv;     /**< tarval module will save private data here */
};


/* ------------------------------- *
 * inline functions                *
 * ------------------------------- */
extern ir_mode *mode_P_code, *mode_P_data;

static INLINE ir_mode *
_get_modeP_code(void) { return mode_P_code; }

static INLINE ir_mode *
_get_modeP_data(void) { return mode_P_data; }

static INLINE modecode
_get_mode_modecode(const ir_mode *mode) { return mode->code; }

static INLINE ident *
_get_mode_ident(const ir_mode *mode) { return mode->name; }

static INLINE mode_sort
_get_mode_sort(const ir_mode* mode) { return mode->sort; }

static INLINE int
_get_mode_size_bits(const ir_mode *mode) { return mode->size; }

static INLINE int
_get_mode_size_bytes(const ir_mode *mode) {
	int size = _get_mode_size_bits(mode);
	if ((size & 7) != 0) return -1;
	return size >> 3;
}

static INLINE int
_get_mode_sign(const ir_mode *mode) { return mode->sign; }

static INLINE int
_get_mode_arithmetic(const ir_mode *mode) { return mode->arithmetic; }

static INLINE unsigned int
_get_mode_modulo_shift(const ir_mode *mode) { return mode->modulo_shift; }

static INLINE unsigned int
_get_mode_vector_elems(const ir_mode *mode) { return mode->vector_elem; }

static INLINE void *
_get_mode_link(const ir_mode *mode) { return mode->link; }

static INLINE void
_set_mode_link(ir_mode *mode, void *l) { mode->link = l; }

/* Functions to check, whether a modecode is signed, float, int, num, data,
   datab or dataM. For more exact definitions read the corresponding pages
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

   The set of "datab" is defined as:
   ---------------------------------
   datab = {irm_F, irm_D, irm_E, irm_Bs, irm_Bu, irm_Hs, irm_Hu,
            irm_Is, irm_Iu, irm_Ls, irm_Lu, irm_C, irm_U, irm_P, irm_b}
            = {data || irm_b }

   The set of "dataM" is defined as:
   ---------------------------------
   dataM = {irm_F, irm_D, irm_E, irm_Bs, irm_Bu, irm_Hs, irm_Hu,
            irm_Is, irm_Iu, irm_Ls, irm_Lu, irm_C, irm_U, irm_P, irm_M}
            = {data || irm_M}
*/

static INLINE int
_mode_is_signed(const ir_mode *mode) {
	assert(mode);
	return mode->sign;
}

static INLINE int
_mode_is_float(const ir_mode *mode) {
	assert(mode);
	return (_get_mode_sort(mode) == irms_float_number);
}

static INLINE int
_mode_is_int(const ir_mode *mode) {
	assert(mode);
	return (_get_mode_sort(mode) == irms_int_number);
}

static INLINE int
_mode_is_character(const ir_mode *mode) {
	assert(mode);
	return (_get_mode_sort(mode) == irms_character);
}

static INLINE int
_mode_is_reference(const ir_mode *mode) {
	assert(mode);
	return (_get_mode_sort(mode) == irms_reference);
}

static INLINE int
_mode_is_num(const ir_mode *mode) {
	assert(mode);
	return (_mode_is_int(mode) || _mode_is_float(mode));
}

static INLINE int
_mode_is_numP(const ir_mode *mode) {
	assert(mode);
	return (_mode_is_int(mode) || _mode_is_float(mode) || _mode_is_reference(mode));
}

static INLINE int
_mode_is_data(const ir_mode *mode) {
	assert(mode);
	return (_mode_is_numP(mode) || _get_mode_sort(mode) == irms_character);
}

static INLINE int
_mode_is_datab(const ir_mode *mode) {
	assert(mode);
	return (_mode_is_data(mode) || _get_mode_sort(mode) == irms_internal_boolean);
}

static INLINE int
_mode_is_dataM(const ir_mode *mode) {
	assert(mode);
	return (_mode_is_data(mode) || _get_mode_modecode(mode) == irm_M);
}

static INLINE int
_mode_is_float_vector(const ir_mode *mode) {
	assert(mode);
	return (_get_mode_sort(mode) == irms_float_number) && (_get_mode_vector_elems(mode) > 1);
}

static INLINE int
_mode_is_int_vector(const ir_mode *mode) {
	assert(mode);
	return (_get_mode_sort(mode) == irms_int_number) && (_get_mode_vector_elems(mode) > 1);
}

/** mode module initialization, call once before use of any other function **/
void init_mode(void);

/** mode module finalization. frees all memory.  */
void finish_mode(void);

#define get_modeP_code()               _get_modeP_code()
#define get_modeP_data()               _get_modeP_data()
#define get_mode_modecode(mode)        _get_mode_modecode(mode)
#define get_mode_ident(mode)           _get_mode_ident(mode)
#define get_mode_sort(mode)            _get_mode_sort(mode)
#define get_mode_size_bits(mode)       _get_mode_size_bits(mode)
#define get_mode_size_bytes(mode)      _get_mode_size_bytes(mode)
#define get_mode_sign(mode)            _get_mode_sign(mode)
#define get_mode_arithmetic(mode)      _get_mode_arithmetic(mode)
#define get_mode_modulo_shift(mode)    _get_mode_modulo_shift(mode)
#define get_mode_n_vector_elems(mode)  _get_mode_vector_elems(mode)
#define get_mode_link(mode)            _get_mode_link(mode)
#define set_mode_link(mode, l)         _set_mode_link(mode, l)
#define mode_is_signed(mode)           _mode_is_signed(mode)
#define mode_is_float(mode)            _mode_is_float(mode)
#define mode_is_int(mode)              _mode_is_int(mode)
#define mode_is_character(mode)        _mode_is_character(mode)
#define mode_is_reference(mode)        _mode_is_reference(mode)
#define mode_is_num(mode)              _mode_is_num(mode)
#define mode_is_numP(mode)             _mode_is_numP(mode)
#define mode_is_data(mode)             _mode_is_data(mode)
#define mode_is_datab(mode)            _mode_is_datab(mode)
#define mode_is_dataM(mode)            _mode_is_dataM(mode)
#define mode_is_float_vector(mode)     _mode_is_float_vector(mode)
#define mode_is_int_vector(mode)       _mode_is_int_vector(mode)

#endif
