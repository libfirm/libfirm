/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irmode_t.h"
# include "ident.h"
# include <stdlib.h>
# include <stddef.h>
# include <string.h>
# include "tv.h"
# include "obst.h"
# include "misc.h"

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
  if (0 == memcmp(&m->sort, &n->sort, offsetof(ir_mode,min) - offsetof(ir_mode,sort))) return 1;

  return 0;
}

/**
 * searches the modes obstack for the given mode and returns
 * a pointer on an equal mode already in the array, NULL if
 * none found
 */
static ir_mode *find_mode(const ir_mode *m)
{
  ir_mode *n;
  struct _obstack_chunk	*p;

  p=modes.chunk;
  for( n=(ir_mode*) p->contents; (char *)n < modes.next_free; n+=sizeof(ir_mode) )
  {
    if(modes_are_equal(n,m)) return n;
  }

  for (p = p->prev; p; p = p->prev)
  {
    for( n=(ir_mode*) p->contents; (char *)n < p->limit; n+=sizeof(ir_mode) )
    {
      if(modes_are_equal(n,m)) return n;
    }
  }

  return NULL;
}

/**
 * sets special values of modes
 */
static void set_mode_values(ir_mode* mode)
{
  mode->min = get_tarval_min(mode);
  mode->max= get_tarval_max(mode);
  mode->null= get_tarval_null(mode);
  mode->one= get_tarval_one(mode);
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

/**
 * Registers a new mode if not defined yet, else returns
 * the "equivalent" one.
 */
static ir_mode *register_mode(const ir_mode* new_mode)
{
  ir_mode *mode = NULL;

  ANNOUNCE();
  assert(new_mode);



  /* copy mode struct to modes array */
  mode=(ir_mode*) obstack_copy(&modes, new_mode, sizeof(ir_mode));

  mode->kind = k_ir_mode;
  if(num_modes>=irm_max) mode->code = num_modes;
  num_modes++;

  if(mode->sort==irms_int_number || mode->sort==irms_float_number || mode->sort==irms_character) set_mode_values(mode);

  return mode;
}

/*
 * Creates a new mode.
 */
ir_mode *new_ir_mode(const char *name, mode_sort sort, int bit_size, int align, int sign, mode_arithmetic arithmetic )
{
  ir_mode mode_tmpl;
  ir_mode *mode;

  /* sanity checks */
  switch (sort)
  {
    case irms_auxiliary:
    case irms_control_flow:
    case irms_memory:
    case irms_internal_boolean:
      assert(0 && "internal modes cannot be user defined");
      return NULL;
      break;

    case irms_float_number:
      assert(0 && "not yet implemented");
      return NULL;
      break;

    case irms_int_number:
    case irms_reference:
    case irms_character:
      break;
  }
  mode_tmpl.name        = new_id_from_str(name);
  mode_tmpl.sort        = sort;
  mode_tmpl.size        = bit_size;
  mode_tmpl.align       = align;
  mode_tmpl.sign        = sign ? 1 : 0;
  mode_tmpl.arithmetic  = arithmetic;
  mode_tmpl.tv_priv     = NULL;

  /* first check if there already is a matching mode */
  mode = find_mode(&mode_tmpl);
  if (mode)
  {
    return mode;
  }
  else
  {
    return register_mode(&mode_tmpl);
  }
}

/* Functions for the direct access to all attributes od a ir_mode */
modecode
get_mode_modecode(const ir_mode *mode)
{
  ANNOUNCE();
  return mode->code;
}

ident *
get_mode_ident(const ir_mode *mode)
{
  ANNOUNCE();
  return mode->name;
}

const char *
get_mode_name(const ir_mode *mode)
{
  ANNOUNCE();
  return id_to_str(mode->name);
}

mode_sort
get_mode_sort(const ir_mode* mode)
{
  ANNOUNCE();
  return mode->sort;
}

INLINE int
get_mode_size_bits(const ir_mode *mode)
{
  ANNOUNCE();
  return mode->size;
}

int get_mode_size_bytes(const ir_mode *mode) {
  int size = get_mode_size_bits(mode);
  ANNOUNCE();
  if ((size & 7) != 0) return -1;
  return size >> 3;
}

int
get_mode_align (const ir_mode *mode)
{
  ANNOUNCE();
  return mode->align;
}

int
get_mode_sign (const ir_mode *mode)
{
  ANNOUNCE();
  return mode->sign;
}

int get_mode_arithmetic (const ir_mode *mode)
{
  ANNOUNCE();
  return mode->arithmetic;
}

void* get_mode_link(const ir_mode *mode)
{
  ANNOUNCE();
  return mode->link;
}

void set_mode_link(ir_mode *mode, void *l)
{
  mode->link=l;
  return;
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
  assert(thing);
  if (get_kind(thing) == k_ir_mode)
    return 1;
  else
    return 0;
}

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

#ifdef MODE_ACCESS_DEFINES
#  undef mode_is_signed
#  undef mode_is_float
#  undef mode_is_int
#  undef mode_is_num
#  undef mode_is_data
#  undef mode_is_datab
#  undef mode_is_dataM
#endif
int
mode_is_signed (const ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return mode->sign;
}

int
mode_is_float (const ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (get_mode_sort(mode) == irms_float_number);
}

int
mode_is_int (const ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (get_mode_sort(mode) == irms_int_number);
}

int mode_is_character (const ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (get_mode_sort(mode) == irms_character);
}

int mode_is_reference (const ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (get_mode_sort(mode) == irms_reference);
}

int
mode_is_num (const ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (mode_is_int(mode) || mode_is_float(mode));
}

int
mode_is_data (const ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (mode_is_num(mode) || get_mode_sort(mode) == irms_character || get_mode_sort(mode) == irms_reference);
}

int
mode_is_datab (const ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (mode_is_data(mode) || get_mode_sort(mode) == irms_internal_boolean);
}

int
mode_is_dataM (const ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (mode_is_data(mode) || get_mode_modecode(mode) == irm_M);
}
#ifdef MODE_ACCESS_DEFINES
#  define mode_is_signed(mode) (mode)->sign
#  define mode_is_float(mode) ((mode)->sort == irms_float_number)
#  define mode_is_int(mode) ((mode)->sort == irms_int_number)
#  define mode_is_num(mode) (((mode)->sort == irms_float_number) || ((mode)->sort == irms_int_number))
#  define mode_is_data(mode) (((mode)->sort == irms_float_number) || ((mode)->sort == irms_int_number) || ((mode)->sort == irms_character) || ((mode)->sort == irms_reference))
#  define mode_is_datab(mode) (((mode)->sort == irms_float_number) || ((mode)->sort == irms_int_number) || ((mode)->sort == irms_character) || ((mode)->sort == irms_reference) || ((mode)->sort == irms_internal_boolean))
#  define mode_is_dataM(mode) (((mode)->sort == irms_float_number) || ((mode)->sort == irms_int_number) || ((mode)->sort == irms_character) || ((mode)->sort == irms_reference) || ((mode)->code == irm_M))
#endif
/* Returns true if sm can be converted to lm without loss. */
int
smaller_mode(const ir_mode *sm, const ir_mode *lm)
{
  ANNOUNCE();
  assert(sm);
  assert(lm);

  if (sm == lm) return 1;

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
           */
          if (mode_is_signed(sm))
          {
            if ( mode_is_signed(lm) && (get_mode_size_bits(lm) > get_mode_size_bits(sm)) )
              return 1;
          }
          else if (mode_is_signed(lm))
          {
            if (get_mode_size_bits(lm) > get_mode_size_bits(sm) + 1)
              return 1;
          }
          else if (get_mode_size_bits(lm) > get_mode_size_bits(sm))
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
      /* XXX currently only the three standard 32,64,80 bit floats
       * are supported which can safely be converted */
      if ( (get_mode_sort(lm) == irms_float_number)
           && (get_mode_size_bits(lm) > get_mode_size_bits(sm)) )
         return 1;
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

/* ** initialization ** */
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
  newmode.arithmetic = irma_none;
  newmode.size    = 0;
  newmode.align   = 0;
  newmode.sign    = 0;
  newmode.tv_priv = NULL;

  /* Control Flow Modes*/
  newmode.sort    = irms_control_flow;

  /* Basic Block */
  newmode.name    = id_from_str("BB", 2);
  newmode.code    = irm_BB;

  mode_BB = register_mode(&newmode);

/* eXecution */
  newmode.name    = id_from_str("X", 1);
  newmode.code    = irm_X;

  mode_X = register_mode(&newmode);

  /* Memory Modes */
  newmode.sort    = irms_memory;

  /* Memory */
  newmode.name    = id_from_str("M", 1);
  newmode.code    = irm_M;

  mode_M = register_mode(&newmode);

  /* Auxiliary Modes */
  newmode.sort    = irms_auxiliary,

  /* Tuple */
  newmode.name    = id_from_str("T", 1);
  newmode.code    = irm_T;

  mode_T = register_mode(&newmode);

  /* ANY */
  newmode.name    = id_from_str("ANY", 3);
  newmode.code    = irm_ANY;

  mode_ANY = register_mode(&newmode);

  /* BAD */
  newmode.name    = id_from_str("BAD", 3);
  newmode.code    = irm_BAD;

  mode_BAD = register_mode(&newmode);

  /* Internal Boolean Modes */
  newmode.sort    = irms_internal_boolean;

  /* boolean */
  newmode.name    = id_from_str("b", 1);
  newmode.code    = irm_b;

  mode_b = register_mode(&newmode);

/* Data Modes */

  /* Float Number Modes */
  newmode.sort    = irms_float_number;
  newmode.arithmetic = irma_ieee754;

  /* float */
  newmode.name    = id_from_str("F", 1);
  newmode.code    = irm_F;
  newmode.sign    = 1;
  newmode.align   = 4;
  newmode.size    = 32;

  mode_F = register_mode(&newmode);

  /* double */
  newmode.name    = id_from_str("D", 1);
  newmode.code    = irm_D;
  newmode.sign    = 1;
  newmode.align   = 4;
  newmode.size    = 64;

  mode_D = register_mode(&newmode);

  /* extended */
  newmode.name    = id_from_str("E", 1);
  newmode.code    = irm_E;
  newmode.sign    = 1;
  newmode.align   = 4;
  newmode.size    = 80;

  mode_E = register_mode(&newmode);

  /* Integer Number Modes */
  newmode.sort    = irms_int_number;
  newmode.arithmetic = irma_twos_complement;

  /* signed byte */
  newmode.name    = id_from_str("Bs", 2);
  newmode.code    = irm_Bs;
  newmode.sign    = 1;
  newmode.align   = 1;
  newmode.size    = 8;

  mode_Bs = register_mode(&newmode);

  /* unsigned byte */
  newmode.name    = id_from_str("Bu", 2);
  newmode.code    = irm_Bu;
  newmode.arithmetic = irma_twos_complement;
  newmode.sign    = 0;
  newmode.align   = 1;
  newmode.size    = 8;

  mode_Bu = register_mode(&newmode);

  /* signed short integer */
  newmode.name    = id_from_str("Hs", 2);
  newmode.code    = irm_Hs;
  newmode.sign    = 1;
  newmode.align   = 2;
  newmode.size    = 16;

  mode_Hs = register_mode(&newmode);

  /* unsigned short integer */
  newmode.name    = id_from_str("Hu", 2);
  newmode.code    = irm_Hu;
  newmode.sign    = 0;
  newmode.align   = 2;
  newmode.size    = 16;

  mode_Hu = register_mode(&newmode);

  /* signed integer */
  newmode.name    = id_from_str("Is", 2);
  newmode.code    = irm_Is;
  newmode.sign    = 1;
  newmode.align   = 4;
  newmode.size    = 32;

  mode_Is = register_mode(&newmode);

  /* unsigned integer */
  newmode.name    = id_from_str("Iu", 2);
  newmode.code    = irm_Iu;
  newmode.sign    = 0;
  newmode.align   = 4;
  newmode.size    = 32;

  mode_Iu = register_mode(&newmode);

  /* signed long integer */
  newmode.name    = id_from_str("Ls", 2);
  newmode.code    = irm_Ls;
  newmode.sign    = 1;
  newmode.align   = 4;
  newmode.size    = 64;

  mode_Ls = register_mode(&newmode);

  /* unsigned long integer */
  newmode.name    = id_from_str("Lu", 2);
  newmode.code    = irm_Lu;
  newmode.sign    = 0;
  newmode.align   = 4;
  newmode.size    = 64;

  mode_Lu = register_mode(&newmode);

  /* Integer Number Modes */
  newmode.sort    = irms_character;
  newmode.arithmetic = irma_none;

  /* Character */
  newmode.name    = id_from_str("C", 1);
  newmode.code    = irm_C;
  newmode.sign    = 0;
  newmode.align   = 1;
  newmode.size    = 8;

  mode_C = register_mode(&newmode);

  /* Unicode character */
  newmode.name    = id_from_str("U", 1);
  newmode.code    = irm_U;
  newmode.sign    = 0;
  newmode.align   = 2;
  newmode.size    = 16;

  mode_U = register_mode(&newmode);

  /* Reference Modes */
  newmode.sort    = irms_reference;
  newmode.arithmetic = irma_twos_complement;

  /* pointer */
  newmode.name    = id_from_str("P", 1);
  newmode.code    = irm_P;
  newmode.sign    = 0;
  newmode.align   = 4;
  newmode.size    = 32;

  mode_P = register_mode(&newmode);
}
