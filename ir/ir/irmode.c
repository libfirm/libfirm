/*
 * Project:     libFIRM
 * File name:   ir/ir/irmode.c
 * Purpose:     Data modes of operations.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier, Mathias Heil
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <stdlib.h>
# include <stddef.h>
# include <string.h>
# include <stdbool.h>

# include "irmode_t.h"
# include "ident.h"
# include "tv.h"
# include "obst.h"

#if 0
static long long count = 0;
#  define ANNOUNCE() printf(__FILE__": call no. %lld (%s)\n", count++, __FUNCTION__)
#else
#  define ANNOUNCE() ((void)0)
#endif

/* * *
 * local values
 * * */


/** dynamic array to hold all modes */
static struct obstack modes;

/** number of defined modes */
static int num_modes;

/* * *
 * local functions
 * * */

/**
 * Compare modes that don't need to have their code field
 * correctly set
 *
 * TODO: Add other fields
 **/
INLINE static int modes_are_equal(const ir_mode *m, const ir_mode *n)
{
  if (m == n) return 1;
  if (m->sort         == n->sort &&
      m->arithmetic   == n->arithmetic &&
      m->size         == n->size &&
      m->sign         == n->sign  &&
      m->modulo_shift == n->modulo_shift &&
      m->vector_elem  == n->vector_elem)
    return 1;

  return 0;
}

/*
 * calculates the next obstack address
 */
static void *next_obstack_adr(struct obstack *o, void *p, size_t s)
{
  PTR_INT_TYPE adr = __PTR_TO_INT((char *)p);
  int mask = obstack_alignment_mask(o);

  adr += s + mask;

  return __INT_TO_PTR(adr & ~mask);
}

/**
 * searches the modes obstack for the given mode and returns
 * a pointer on an equal mode already in the array, NULL if
 * none found
 */
static ir_mode *find_mode(const ir_mode *m)
{
  ir_mode *n, *nn;
  struct _obstack_chunk	*p;

  p  = modes.chunk;
  n  = (ir_mode *)p->contents;
  nn = next_obstack_adr(&modes, n, sizeof(*n));
  for (; (char *)nn <= modes.next_free;) {
    assert(is_mode(n));
    if (modes_are_equal(n, m))
      return n;

    n  = nn;
    nn = next_obstack_adr(&modes, n, sizeof(*n));
  }

  for (p = p->prev; p; p = p->prev) {
    n  = (ir_mode *)p->contents;
    nn = next_obstack_adr(&modes, n, sizeof(*n));
    for (; (char *)nn < p->limit;) {
      assert(is_mode(n));
      if (modes_are_equal(n, m))
	return n;

      n  = nn;
      nn = next_obstack_adr(&modes, n, sizeof(*n));
    }
  }

  return NULL;
}

/**
 * sets special values of modes
 */
static void set_mode_values(ir_mode* mode)
{
  switch (get_mode_sort(mode))
  {
    case irms_character:
    case irms_int_number:
    case irms_float_number:
      mode->min = get_tarval_min(mode);
      mode->max = get_tarval_max(mode);
      mode->null = get_tarval_null(mode);
      mode->one = get_tarval_one(mode);
      break;

    case irms_internal_boolean:
      mode->min = tarval_b_false;
      mode->max = tarval_b_true;
      mode->null = tarval_b_false;
      mode->one = tarval_b_true;
      break;

    case irms_reference:
      mode->min = tarval_bad;
      mode->max = tarval_bad;
      mode->null = (get_mode_modecode(mode) == irm_P) ? tarval_P_void : tarval_bad;
      mode->one = tarval_bad;
      break;

    case irms_auxiliary:
    case irms_memory:
    case irms_control_flow:
      mode->min = tarval_bad;
      mode->max = tarval_bad;
      mode->null = tarval_bad;
      mode->one = tarval_bad;
      break;
  }
}

/* * *
 * globals defined in irmode.h
 * * */

/* --- Predefined modes --- */

/* FIRM internal modes: */
ir_mode *mode_T;
ir_mode *mode_X;
ir_mode *mode_M;
ir_mode *mode_BB;
ir_mode *mode_ANY;
ir_mode *mode_BAD;

/* predefined numerical modes: */
ir_mode *mode_F;    /* float */
ir_mode *mode_D;    /* double */
ir_mode *mode_E;    /* long double */

ir_mode *mode_Bs;   /* integral values, signed and unsigned */
ir_mode *mode_Bu;   /* 8 bit */
ir_mode *mode_Hs;   /* 16 bit */
ir_mode *mode_Hu;
ir_mode *mode_Is;   /* 32 bit */
ir_mode *mode_Iu;
ir_mode *mode_Ls;   /* 64 bit */
ir_mode *mode_Lu;

ir_mode *mode_C;
ir_mode *mode_U;
ir_mode *mode_b;
ir_mode *mode_P;

/* machine specific modes */
ir_mode *mode_P_mach;	/* machine specific pointer mode */

/* * *
 * functions defined in irmode.h
 * * */

/* JNI access functions */
INLINE ir_mode *get_modeT(void) { ANNOUNCE(); return mode_T; }
INLINE ir_mode *get_modeF(void) { ANNOUNCE(); return mode_F; }
INLINE ir_mode *get_modeD(void) { ANNOUNCE(); return mode_D; }
INLINE ir_mode *get_modeE(void) { ANNOUNCE(); return mode_E; }
INLINE ir_mode *get_modeBs(void) { ANNOUNCE(); return mode_Bs; }
INLINE ir_mode *get_modeBu(void) { ANNOUNCE(); return mode_Bu; }
INLINE ir_mode *get_modeHs(void) { ANNOUNCE(); return mode_Hs; }
INLINE ir_mode *get_modeHu(void) { ANNOUNCE(); return mode_Hu; }
INLINE ir_mode *get_modeIs(void) { ANNOUNCE(); return mode_Is; }
INLINE ir_mode *get_modeIu(void) { ANNOUNCE(); return mode_Iu; }
INLINE ir_mode *get_modeLs(void) { ANNOUNCE(); return mode_Ls; }
INLINE ir_mode *get_modeLu(void) { ANNOUNCE(); return mode_Lu; }
INLINE ir_mode *get_modeC(void) { ANNOUNCE(); return mode_C; }
INLINE ir_mode *get_modeU(void) { ANNOUNCE(); return mode_U; }
INLINE ir_mode *get_modeb(void) { ANNOUNCE(); return mode_b; }
INLINE ir_mode *get_modeP(void) { ANNOUNCE(); return mode_P; }
INLINE ir_mode *get_modeX(void) { ANNOUNCE(); return mode_X; }
INLINE ir_mode *get_modeM(void) { ANNOUNCE(); return mode_M; }
INLINE ir_mode *get_modeBB(void) { ANNOUNCE(); return mode_BB; }
INLINE ir_mode *get_modeANY(void) { ANNOUNCE(); return mode_ANY; }
INLINE ir_mode *get_modeBAD(void) { ANNOUNCE(); return mode_BAD; }


ir_mode *(get_modeP_mach)(void) {
  ANNOUNCE();
  return __get_modeP_mach();
}

void (set_modeP_mach)(ir_mode *p) {
  ANNOUNCE();
  __set_modeP_mach(p);
}

/**
 * Registers a new mode.
 *
 * @param new_mode  The new mode template.
 */
static ir_mode *register_mode(const ir_mode* new_mode)
{
  ir_mode *mode = NULL;

  ANNOUNCE();
  assert(new_mode);

  /* copy mode struct to modes array */
  mode = (ir_mode*)obstack_copy(&modes, new_mode, sizeof(ir_mode));

  mode->kind = k_ir_mode;
  if (num_modes >= irm_max) mode->code = num_modes;
  num_modes++;

  set_mode_values(mode);

  return mode;
}

/*
 * Creates a new mode.
 */
ir_mode *new_ir_mode(const char *name, mode_sort sort, int bit_size, int sign,
		     mode_arithmetic arithmetic, unsigned int modulo_shift )
{
  ir_mode mode_tmpl;
  ir_mode *mode;

  mode_tmpl.name         = new_id_from_str(name);
  mode_tmpl.sort         = sort;
  mode_tmpl.size         = bit_size;
  mode_tmpl.sign         = sign ? 1 : 0;
  mode_tmpl.modulo_shift = (mode_tmpl.sort == irms_int_number) ? modulo_shift : 0;
  mode_tmpl.vector_elem  = 1;
  mode_tmpl.arithmetic   = arithmetic;
  mode_tmpl.link         = NULL;
  mode_tmpl.tv_priv      = NULL;

  mode = find_mode(&mode_tmpl);
  if (mode)
  {
    return mode;
  }

  /* sanity checks */
  switch (sort)
  {
    case irms_auxiliary:
    case irms_control_flow:
    case irms_memory:
    case irms_internal_boolean:
      assert(0 && "internal modes cannot be user defined");
      return NULL;

    case irms_float_number:
    case irms_int_number:
    case irms_reference:
    case irms_character:
      return register_mode(&mode_tmpl);
  }
  return NULL; /* to shut up gcc */
}

/*
 * Creates a new vector mode.
 */
ir_mode *new_ir_vector_mode(const char *name, mode_sort sort, int bit_size, unsigned num_of_elem, int sign,
		     mode_arithmetic arithmetic, unsigned int modulo_shift )
{
  ir_mode mode_tmpl;
  ir_mode *mode;

  mode_tmpl.name         = new_id_from_str(name);
  mode_tmpl.sort         = sort;
  mode_tmpl.size         = bit_size * num_of_elem;
  mode_tmpl.sign         = sign ? 1 : 0;
  mode_tmpl.modulo_shift = (mode_tmpl.sort == irms_int_number) ? modulo_shift : 0;
  mode_tmpl.vector_elem  = num_of_elem;
  mode_tmpl.arithmetic   = arithmetic;
  mode_tmpl.link         = NULL;
  mode_tmpl.tv_priv      = NULL;

  mode = find_mode(&mode_tmpl);
  if (mode)
    return mode;

  if (num_of_elem <= 1) {
    assert(0 && "vector modes should have at least 2 elements");
    return NULL;
  }

  /* sanity checks */
  switch (sort)
  {
    case irms_auxiliary:
    case irms_control_flow:
    case irms_memory:
    case irms_internal_boolean:
      assert(0 && "internal modes cannot be user defined");
      return NULL;

    case irms_reference:
    case irms_character:
      assert(0 && "only integer and floating point modes can be vectorized");
      return NULL;

    case irms_float_number:
      assert(0 && "not yet implemented");
      return NULL;

    case irms_int_number:
      return register_mode(&mode_tmpl);
  }
  return NULL; /* to shut up gcc */
}

/* Functions for the direct access to all attributes od a ir_mode */
modecode
(get_mode_modecode)(const ir_mode *mode)
{
  ANNOUNCE();
  return __get_mode_modecode(mode);
}

ident *
(get_mode_ident)(const ir_mode *mode)
{
  ANNOUNCE();
  return __get_mode_ident(mode);
}

const char *
get_mode_name(const ir_mode *mode)
{
  ANNOUNCE();
  return get_id_str(mode->name);
}

mode_sort
(get_mode_sort)(const ir_mode* mode)
{
  ANNOUNCE();
  return __get_mode_sort(mode);
}

int
(get_mode_size_bits)(const ir_mode *mode)
{
  ANNOUNCE();
  return __get_mode_size_bits(mode);
}

int
(get_mode_size_bytes)(const ir_mode *mode) {
  ANNOUNCE();
  return __get_mode_size_bytes(mode);
}

int
(get_mode_sign)(const ir_mode *mode)
{
  ANNOUNCE();
  return __get_mode_sign(mode);
}

int
(get_mode_arithmetic)(const ir_mode *mode)
{
  ANNOUNCE();
  return get_mode_arithmetic(mode);
}


/* Attribute modulo shift specifies for modes of kind irms_int_number
 *  whether shift applies modulo to value of bits to shift.  Asserts
 *  if mode is not irms_int_number.
 */
unsigned int
(get_mode_modulo_shift)(const ir_mode *mode) {
  return __get_mode_modulo_shift(mode);
}

unsigned int
(get_mode_n_vector_elems)(const ir_mode *mode) {
  return __get_mode_vector_elems(mode);
}

void *
(get_mode_link)(const ir_mode *mode)
{
  ANNOUNCE();
  return __get_mode_link(mode);
}

void
(set_mode_link)(ir_mode *mode, void *l)
{
  __set_mode_link(mode, l);
}

tarval *
get_mode_min (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  assert(get_mode_modecode(mode) < num_modes);
  assert(mode_is_data(mode));

  return mode->min;
}

tarval *
get_mode_max (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  assert(get_mode_modecode(mode) < num_modes);
  assert(mode_is_data(mode));

  return mode->max;
}

tarval *
get_mode_null (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  assert(get_mode_modecode(mode) < num_modes);
  assert(mode_is_data(mode));

  return mode->null;
}

tarval *
get_mode_one (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  assert(get_mode_modecode(mode) < num_modes);
  assert(mode_is_data(mode));

  return mode->one;
}

tarval *
get_mode_infinite(ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  assert(get_mode_modecode(mode) < num_modes);
  assert(mode_is_float(mode));

  return get_tarval_inf(mode);
}

tarval *
get_mode_NAN(ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  assert(get_mode_modecode(mode) < num_modes);
  assert(mode_is_float(mode));

  return get_tarval_nan(mode);
}

int
is_mode (void *thing) {
  if (get_kind(thing) == k_ir_mode)
    return 1;
  else
    return 0;
}

int
(mode_is_signed)(const ir_mode *mode) {
  ANNOUNCE();
  return __mode_is_signed(mode);
}

int
(mode_is_float)(const ir_mode *mode) {
  ANNOUNCE();
  return __mode_is_float(mode);
}

int
(mode_is_int)(const ir_mode *mode) {
  ANNOUNCE();
  return __mode_is_int(mode);
}

int
(mode_is_character)(const ir_mode *mode) {
  ANNOUNCE();
  return __mode_is_character(mode);
}

int
(mode_is_reference)(const ir_mode *mode) {
  ANNOUNCE();
  return __mode_is_reference(mode);
}

int
(mode_is_num)(const ir_mode *mode) {
  ANNOUNCE();
  return __mode_is_num(mode);
}

int
(mode_is_numP)(const ir_mode *mode) {
  ANNOUNCE();
  return __mode_is_numP(mode);
}

int
(mode_is_data)(const ir_mode *mode) {
  ANNOUNCE();
  return __mode_is_data(mode);
}

int
(mode_is_datab)(const ir_mode *mode) {
  ANNOUNCE();
  return __mode_is_datab(mode);
}

int
(mode_is_dataM)(const ir_mode *mode) {
  ANNOUNCE();
  return __mode_is_dataM(mode);
}

int
(mode_is_float_vector)(const ir_mode *mode) {
  ANNOUNCE();
  return __mode_is_float_vector(mode);
}

int
(mode_is_int_vector)(const ir_mode *mode) {
  ANNOUNCE();
  return __mode_is_int_vector(mode);
}

/* Returns true if sm can be converted to lm without loss. */
int
smaller_mode(const ir_mode *sm, const ir_mode *lm)
{
  int sm_bits, lm_bits;

  ANNOUNCE();
  assert(sm);
  assert(lm);

  if (sm == lm) return 1;

  sm_bits = get_mode_size_bits(sm);
  lm_bits = get_mode_size_bits(lm);

  switch(get_mode_sort(sm))
  {
    case irms_int_number:
      switch(get_mode_sort(lm))
      {
        case irms_int_number:
          /* integers are convertable if
           *   - both have the same sign and lm is the larger one
           *   - lm is the signed one and is at least two bits larger
           *     (one for the sign, one for the highest bit of sm)
	   *   - sm & lm are two_complement and lm has greater or equal number of bits
           */
          if (   get_mode_arithmetic(sm) == get_mode_arithmetic(lm)
	      && get_mode_arithmetic(sm) == irma_twos_complement) {
	    return lm_bits >= sm_bits;
	  }
          else if (mode_is_signed(sm))
          {
            if ( mode_is_signed(lm) && (lm_bits >= sm_bits) )
              return 1;
          }
          else if (mode_is_signed(lm))
          {
            if (lm_bits > sm_bits + 1)
              return 1;
          }
          else if (lm_bits >= sm_bits)
          {
            return 1;
          }
          break;

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
       /* do exist machines out there with different pointer lenghts ?*/
      return 0;

    default:
      break;
  }

  /* else */
  return 0;
}

/* initialization, build the default modes */
void
init_mode (void)
{
  ir_mode newmode;
  ANNOUNCE();
  /* init flexible array */

  obstack_init(&modes);

  num_modes  =  0;
  /* initialize predefined modes */

  /* Internal Modes */
  newmode.arithmetic   = irma_none;
  newmode.size         = 0;
  newmode.sign         = 0;
  newmode.modulo_shift = 0;
  newmode.vector_elem  = 0;
  newmode.link         = NULL;
  newmode.tv_priv      = NULL;

  /* Control Flow Modes*/
  newmode.sort    = irms_control_flow;

  /* Basic Block */
  newmode.name    = new_id_from_chars("BB", 2);
  newmode.code    = irm_BB;

  mode_BB = register_mode(&newmode);

/* eXecution */
  newmode.name    = new_id_from_chars("X", 1);
  newmode.code    = irm_X;

  mode_X = register_mode(&newmode);

  /* Memory Modes */
  newmode.sort    = irms_memory;

  /* Memory */
  newmode.name    = new_id_from_chars("M", 1);
  newmode.code    = irm_M;

  mode_M = register_mode(&newmode);

  /* Auxiliary Modes */
  newmode.sort    = irms_auxiliary,

  /* Tuple */
  newmode.name    = new_id_from_chars("T", 1);
  newmode.code    = irm_T;

  mode_T = register_mode(&newmode);

  /* ANY */
  newmode.name    = new_id_from_chars("ANY", 3);
  newmode.code    = irm_ANY;

  mode_ANY = register_mode(&newmode);

  /* BAD */
  newmode.name    = new_id_from_chars("BAD", 3);
  newmode.code    = irm_BAD;

  mode_BAD = register_mode(&newmode);

  /* Internal Boolean Modes */
  newmode.sort    = irms_internal_boolean;

  /* boolean */
  newmode.name    = new_id_from_chars("b", 1);
  newmode.code    = irm_b;

  mode_b = register_mode(&newmode);

/* Data Modes */
  newmode.vector_elem = 1;

  /* Float Number Modes */
  newmode.sort       = irms_float_number;
  newmode.arithmetic = irma_ieee754;

  /* float */
  newmode.name    = new_id_from_chars("F", 1);
  newmode.code    = irm_F;
  newmode.sign    = 1;
  newmode.size    = 32;

  mode_F = register_mode(&newmode);

  /* double */
  newmode.name    = new_id_from_chars("D", 1);
  newmode.code    = irm_D;
  newmode.sign    = 1;
  newmode.size    = 64;

  mode_D = register_mode(&newmode);

  /* extended */
  newmode.name    = new_id_from_chars("E", 1);
  newmode.code    = irm_E;
  newmode.sign    = 1;
  newmode.size    = 80;

  mode_E = register_mode(&newmode);

  /* Integer Number Modes */
  newmode.sort         = irms_int_number;
  newmode.arithmetic   = irma_twos_complement;

  /* signed byte */
  newmode.name         = new_id_from_chars("Bs", 2);
  newmode.code         = irm_Bs;
  newmode.sign         = 1;
  newmode.size         = 8;
  newmode.modulo_shift = 32;

  mode_Bs = register_mode(&newmode);

  /* unsigned byte */
  newmode.name         = new_id_from_chars("Bu", 2);
  newmode.code         = irm_Bu;
  newmode.arithmetic   = irma_twos_complement;
  newmode.sign         = 0;
  newmode.size         = 8;
  newmode.modulo_shift = 32;

  mode_Bu = register_mode(&newmode);

  /* signed short integer */
  newmode.name         = new_id_from_chars("Hs", 2);
  newmode.code         = irm_Hs;
  newmode.sign         = 1;
  newmode.size         = 16;
  newmode.modulo_shift = 32;

  mode_Hs = register_mode(&newmode);

  /* unsigned short integer */
  newmode.name         = new_id_from_chars("Hu", 2);
  newmode.code         = irm_Hu;
  newmode.sign         = 0;
  newmode.size         = 16;
  newmode.modulo_shift = 32;

  mode_Hu = register_mode(&newmode);

  /* signed integer */
  newmode.name         = new_id_from_chars("Is", 2);
  newmode.code         = irm_Is;
  newmode.sign         = 1;
  newmode.size         = 32;
  newmode.modulo_shift = 32;

  mode_Is = register_mode(&newmode);

  /* unsigned integer */
  newmode.name         = new_id_from_chars("Iu", 2);
  newmode.code         = irm_Iu;
  newmode.sign         = 0;
  newmode.size         = 32;
  newmode.modulo_shift = 32;

  mode_Iu = register_mode(&newmode);

  /* signed long integer */
  newmode.name         = new_id_from_chars("Ls", 2);
  newmode.code         = irm_Ls;
  newmode.sign         = 1;
  newmode.size         = 64;
  newmode.modulo_shift = 64;

  mode_Ls = register_mode(&newmode);

  /* unsigned long integer */
  newmode.name         = new_id_from_chars("Lu", 2);
  newmode.code         = irm_Lu;
  newmode.sign         = 0;
  newmode.size         = 64;
  newmode.modulo_shift = 64;

  mode_Lu = register_mode(&newmode);

  /* Character Modes */
  newmode.sort         = irms_character;
  newmode.arithmetic   = irma_none;

  /* Character */
  newmode.name         = new_id_from_chars("C", 1);
  newmode.code         = irm_C;
  newmode.sign         = 0;
  newmode.size         = 8;
  newmode.modulo_shift = 32;

  mode_C = register_mode(&newmode);

  /* Unicode character */
  newmode.name         = new_id_from_chars("U", 1);
  newmode.code         = irm_U;
  newmode.sign         = 0;
  newmode.size         = 16;
  newmode.modulo_shift = 32;

  mode_U = register_mode(&newmode);

  /* Reference Modes */
  newmode.sort    = irms_reference;
  newmode.arithmetic = irma_twos_complement;

  /* pointer */
  newmode.name         = new_id_from_chars("P", 1);
  newmode.code         = irm_P;
  newmode.sign         = 0;
  newmode.size         = 32;
  newmode.modulo_shift = 0;

  mode_P = register_mode(&newmode);

  /* set the machine specific modes to the predifined ones */
  mode_P_mach = mode_P;
}

/* find a signed mode for an unsigned integer mode */
ir_mode *find_unsigned_mode(const ir_mode *mode) {
  ir_mode n = *mode;

  if (mode->sort != irms_int_number);
  n.sign = 0;
  return find_mode(&n);
}

/* find an unsigned mode for a signed integer mode */
ir_mode *find_signed_mode(const ir_mode *mode) {
  ir_mode n = *mode;

  assert(mode->sort == irms_int_number);
  n.sign = 1;
  return find_mode(&n);
}

/* finds a integer mode with 2*n bits for an integer mode with n bits. */
ir_mode *find_double_bits_int_mode(const ir_mode *mode) {
  ir_mode n = *mode;

  assert(mode->sort == irms_int_number && mode->arithmetic == irma_twos_complement);

  n.size = 2*mode->size;
  return find_mode(&n);
}

void finish_mode(void) {
  obstack_free(&modes, 0);

  mode_T = NULL;
  mode_X = NULL;
  mode_M = NULL;
  mode_BB = NULL;
  mode_ANY = NULL;
  mode_BAD = NULL;

  mode_F = NULL;
  mode_D = NULL;
  mode_E = NULL;

  mode_Bs = NULL;
  mode_Bu = NULL;
  mode_Hs = NULL;
  mode_Hu = NULL;
  mode_Is = NULL;
  mode_Iu = NULL;
  mode_Ls = NULL;
  mode_Lu = NULL;

  mode_C = NULL;
  mode_U = NULL;
  mode_b = NULL;
  mode_P = NULL;

  mode_P_mach = NULL;
}
