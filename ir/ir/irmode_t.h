/*
 * Project:     libFIRM
 * File name:   ir/ir/irmode_t.h
 * Purpose:     Data modes of operations -- private header.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier, Mathias Heil
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


/**
 * @file irmode_t.h
 */

# ifndef _IRMODE_T_H_
# define _IRMODE_T_H_

# include "irmode.h"
# include "tv.h"

/** This struct is supposed to completely define a mode. **/
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
  int               align;         /**< mode alignment in Bits. */
  unsigned          sign:1;        /**< signedness of this mode */
  unsigned int      modulo_shift;  /**< number of bits a valus of this mode will be shifted */
  unsigned          vector_elem;   /**< if this is not equal 1, this is a vector mode with
				        vector_elem number of elements, size contains the size
					of all bits and must be dividable by vector_elem */

  /* ----------------------------------------------------------------------- */
  tarval            *min;
  tarval            *max;
  tarval            *null;
  tarval            *one;
  void              *link;      /**< To store some intermediate information */
  const void        *tv_priv;   /**< tarval module will save private data here */
};

/* ------------------------------- *
 * inline functions                *
 * ------------------------------- */
extern ir_mode *mode_P_mach;

static INLINE ir_mode *
__get_modeP_mach(void) { return mode_P_mach; }

static INLINE void
__set_modeP_mach(ir_mode *p) {
  assert(mode_is_reference(p));
  mode_P_mach = p;
}

static INLINE modecode
__get_mode_modecode(const ir_mode *mode) { return mode->code; }

static INLINE ident *
__get_mode_ident(const ir_mode *mode) { return mode->name; }

static INLINE mode_sort
__get_mode_sort(const ir_mode* mode) { return mode->sort; }

static INLINE int
__get_mode_size_bits(const ir_mode *mode) { return mode->size; }

static INLINE int
__get_mode_size_bytes(const ir_mode *mode) {
  int size = __get_mode_size_bits(mode);
  if ((size & 7) != 0) return -1;
  return size >> 3;
}

static INLINE int
__get_mode_align_bits(const ir_mode *mode) { return mode->align; }

static INLINE int
__get_mode_align_bytes(const ir_mode *mode) {
  int align = __get_mode_align_bits(mode);
  if ((align & 7) != 0) return -1;
  return align >> 3;
}

static INLINE int
__get_mode_sign(const ir_mode *mode) { return mode->sign; }

static INLINE int
__get_mode_arithmetic(const ir_mode *mode) { return mode->arithmetic; }

static INLINE unsigned int
__get_mode_modulo_shift(const ir_mode *mode) { return mode->modulo_shift; }

static INLINE unsigned int
__get_mode_vector_elems(const ir_mode *mode) { return mode->vector_elem; }

static INLINE void *
__get_mode_link(const ir_mode *mode) { return mode->link; }

static INLINE void
__set_mode_link(ir_mode *mode, void *l) { mode->link = l; }

/* Functions to check, whether a modecode is signed, float, int, num, data,
   datab or dataM. For more exact definitions read the corresponding pages
   in the firm documentation or the followingenumeration

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
__mode_is_signed(const ir_mode *mode) {
  assert(mode);
  return mode->sign;
}

static INLINE int
__mode_is_float(const ir_mode *mode) {
  assert(mode);
  return (__get_mode_sort(mode) == irms_float_number);
}

static INLINE int
__mode_is_int(const ir_mode *mode) {
  assert(mode);
  return (__get_mode_sort(mode) == irms_int_number);
}

static INLINE int
__mode_is_character(const ir_mode *mode) {
  assert(mode);
  return (__get_mode_sort(mode) == irms_character);
}

static INLINE int
__mode_is_reference(const ir_mode *mode) {
  assert(mode);
  return (__get_mode_sort(mode) == irms_reference);
}

static INLINE int
__mode_is_num(const ir_mode *mode) {
  assert(mode);
  return (__mode_is_int(mode) || __mode_is_float(mode));
}

static INLINE int
__mode_is_numP(const ir_mode *mode) {
  assert(mode);
  return (__mode_is_int(mode) || __mode_is_float(mode) || __mode_is_reference(mode));
}

static INLINE int
__mode_is_data(const ir_mode *mode) {
  assert(mode);
  return (__mode_is_num(mode) || __get_mode_sort(mode) == irms_character || __get_mode_sort(mode) == irms_reference);
}

static INLINE int
__mode_is_datab(const ir_mode *mode) {
  assert(mode);
  return (__mode_is_data(mode) || __get_mode_sort(mode) == irms_internal_boolean);
}

static INLINE int
__mode_is_dataM(const ir_mode *mode) {
  assert(mode);
  return (__mode_is_data(mode) || __get_mode_modecode(mode) == irm_M);
}

static INLINE int
__mode_is_float_vector(const ir_mode *mode) {
  assert(mode);
  return (__get_mode_sort(mode) == irms_float_number) && (__get_mode_vector_elems(mode) > 1);
}

static INLINE int
__mode_is_int_vector(const ir_mode *mode) {
  assert(mode);
  return (__get_mode_sort(mode) == irms_int_number) && (__get_mode_vector_elems(mode) > 1);
}

#define get_modeP_mach()             __get_modeP_mach()
#define set_modeP_mach(p)            __set_modeP_mach(p)
#define get_mode_modecode(mode)      __get_mode_modecode(mode)
#define get_mode_ident(mode)         __get_mode_ident(mode)
#define get_mode_sort(mode)          __get_mode_sort(mode)
#define get_mode_size_bits(mode)     __get_mode_size_bits(mode)
#define get_mode_size_bytes(mode)    __get_mode_size_bytes(mode)
#define get_mode_align(mode)         __get_mode_align(mode)
#define get_mode_sign(mode)          __get_mode_sign(mode)
#define get_mode_arithmetic(mode)    __get_mode_arithmetic(mode)
#define get_mode_modulo_shift(mode)  __get_mode_modulo_shift(mode)
#define get_mode_vector_elems(mode)  __get_mode_vector_elems(mode)
#define get_mode_link(mode)          __get_mode_link(mode)
#define set_mode_link(mode, l)       __set_mode_link(mode, l)
#define mode_is_signed(mode)         __mode_is_signed(mode)
#define mode_is_float(mode)          __mode_is_float(mode)
#define mode_is_int(mode)            __mode_is_int(mode)
#define mode_is_character(mode)      __mode_is_character(mode)
#define mode_is_reference(mode)      __mode_is_reference(mode)
#define mode_is_num(mode)            __mode_is_num(mode)
#define mode_is_numP(mode)           __mode_is_numP(mode)
#define mode_is_data(mode)           __mode_is_data(mode)
#define mode_is_datab(mode)          __mode_is_datab(mode)
#define mode_is_dataM(mode)          __mode_is_dataM(mode)
#define mode_is_float_vector(mode)   __mode_is_float_vector(mode)
#define mode_is_int_vector(mode)     __mode_is_int_vector(mode)

#endif /* _IRMODE_T_H_ */
