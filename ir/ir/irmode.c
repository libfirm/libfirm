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
# include <malloc.h>
# include <stddef.h>
# include <string.h>
# include "tv.h"
# include "array.h"

#if 0
static long long count = 0;
#  define ANNOUNCE() printf(__FILE__": call no. %lld (%s)\n", count++, __FUNCTION__)
#else
#  define ANNOUNCE() ((void)0)
#endif

/* * *
 * local values
 * * */


/* dynamic array to hold all modes */
static ir_mode * modes;
/* dynamic arrays to hold special modes' values */
static tarval** modes_min;
static tarval** modes_max;
static tarval** modes_null;
static tarval** modes_one;

/* number of defined modes */
static int num_modes;

/* * *
 * local functions
 * * */

/**
 * compare modes that don't need to have their code field
 * correctly set
 */
static int modes_are_equal(ir_mode *m, ir_mode *n)
{
  if (m == n) return 1;
  if ( (m->sort == n->sort) && (m->size == n->size) &&
      (m->align == n->align) && (m->sign == n->sign) &&
      (m->name == n->name) )
    return 1;

  return 0;
}

/**
 * searches the modes array for the given mode and returns
 * a pointer on an equal mode already in the array, NULL if
 * none found
 */
static ir_mode *find_mode(ir_mode *m)
{
  int i;

  for (i = 0; i < num_modes; i++)
  {
    if (modes_are_equal(m, &modes[i])) return &modes[i];
  }

  return NULL;
}

/**
 * sets special values of modes
 */
static void set_mode_values(ir_mode* mode)
{
  modes_min[get_mode_modecode(mode)] = get_tarval_min(mode);
  modes_max[get_mode_modecode(mode)] = get_tarval_max(mode);
  modes_null[get_mode_modecode(mode)] = get_tarval_null(mode);
  modes_one[get_mode_modecode(mode)] = get_tarval_one(mode);
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
INLINE ir_mode *get_modeT() { ANNOUNCE(); return mode_T; }
INLINE ir_mode *get_modeF() { ANNOUNCE(); return mode_F; }
INLINE ir_mode *get_modeD() { ANNOUNCE(); return mode_D; }
INLINE ir_mode *get_modeE() { ANNOUNCE(); return mode_E; }
INLINE ir_mode *get_modeBs() { ANNOUNCE(); return mode_Bs; }
INLINE ir_mode *get_modeBu() { ANNOUNCE(); return mode_Bu; }
INLINE ir_mode *get_modeHs() { ANNOUNCE(); return mode_Hs; }
INLINE ir_mode *get_modeHu() { ANNOUNCE(); return mode_Hu; }
INLINE ir_mode *get_modeIs() { ANNOUNCE(); return mode_Is; }
INLINE ir_mode *get_modeIu() { ANNOUNCE(); return mode_Iu; }
INLINE ir_mode *get_modeLs() { ANNOUNCE(); return mode_Ls; }
INLINE ir_mode *get_modeLu() { ANNOUNCE(); return mode_Lu; }
INLINE ir_mode *get_modeC() { ANNOUNCE(); return mode_C; }
INLINE ir_mode *get_modeU() { ANNOUNCE(); return mode_U; }
INLINE ir_mode *get_modeb() { ANNOUNCE(); return mode_b; }
INLINE ir_mode *get_modeP() { ANNOUNCE(); return mode_P; }
INLINE ir_mode *get_modeX() { ANNOUNCE(); return mode_X; }
INLINE ir_mode *get_modeM() { ANNOUNCE(); return mode_M; }
INLINE ir_mode *get_modeBB() { ANNOUNCE(); return mode_BB; }

/* ** Constructor ** */
ir_mode *
register_mode(ir_mode* new_mode)
{
  ir_mode *mode;

  ANNOUNCE();
  assert(new_mode);

  /* first check if there already is a matching mode */
  mode = find_mode(new_mode);
  if (mode) return mode;

  /* sanity checks */
  switch (new_mode->sort)
  {
    case auxiliary:
    case internal_boolean:
      assert(0 && "internal modes cannot be user defined");
      return NULL;
      break;

    case float_number:
      assert(0 && "not yet implemented");
      return NULL;
      break;

    case int_number:
    case reference:
    case character:
      break;

    default:
      assert(0);
      return NULL;
  }

  /* copy mode struct to modes array */
  ARR_EXTEND(ir_mode, modes, 1);
  ARR_EXTEND(tarval*, modes_min, 1);
  ARR_EXTEND(tarval*, modes_max, 1);
  ARR_EXTEND(tarval*, modes_null, 1);
  ARR_EXTEND(tarval*, modes_one, 1);
  mode = &modes[num_modes];

  memcpy(mode, new_mode, sizeof(ir_mode));
  mode->code = num_modes;
  num_modes++;

  set_mode_values(mode);

  return mode;
}

/* Functions for the direct access to all attributes of a ir_mode */
#ifdef MODE_ACCESS_DEFINES
#  undef get_mode_modecode
#  undef get_mode_ident
#  undef get_mode_name
#  undef get_mode_sort
#  undef get_mode_size_bits
#  undef get_mode_align
#  undef get_mode_sign
#endif

modecode
get_mode_modecode(ir_mode *mode)
{
  ANNOUNCE();
  return mode->code;
}

ident *
get_mode_ident(ir_mode *mode)
{
  ANNOUNCE();
  return mode->name;
}

const char *
get_mode_name(ir_mode *mode)
{
  ANNOUNCE();
  return id_to_str(mode->name);
}

mode_sort
get_mode_sort(ir_mode* mode)
{
  ANNOUNCE();
  return mode->sort;
}

INLINE int
get_mode_size_bits(ir_mode *mode)
{
  ANNOUNCE();
  return mode->size;
}

int get_mode_size_bytes(ir_mode *mode) {
  ANNOUNCE();
  int size = get_mode_size_bits(mode);
  if ((size % 8) != 0) return -1;
  return size / 8;
}

int
get_mode_align (ir_mode *mode)
{
  ANNOUNCE();
  return mode->align;
}

int
get_mode_sign (ir_mode *mode)
{
  ANNOUNCE();
  return mode->sign;
}

#ifdef MODE_ACCESS_DEFINES
#  define get_mode_modecode(mode) (mode)->code
#  define get_mode_ident(mode) (mode)->name
#  define get_mode_name(mode) id_to_str((mode)->name)
#  define get_mode_sort(mode) (mode)->sort
#  define get_mode_size_bits(mode) (mode)->size
#  define get_mode_align(mode) (mode)->align
#  define get_mode_sign(mode) (mode)->sign
#endif

tarval *
get_mode_min (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  assert(get_mode_modecode(mode) < num_modes);
  assert(mode_is_data(mode));

  return modes_min[get_mode_modecode(mode)];
}

tarval *
get_mode_max (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  assert(get_mode_modecode(mode) < num_modes);
  assert(mode_is_data(mode));

  return modes_max[get_mode_modecode(mode)];
}

tarval *
get_mode_null (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  assert(get_mode_modecode(mode) < num_modes);
  assert(mode_is_data(mode));

  return modes_null[get_mode_modecode(mode)];
}

tarval *
get_mode_one (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  assert(get_mode_modecode(mode) < num_modes);
  assert(mode_is_data(mode));

  return modes_one[get_mode_modecode(mode)];
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
mode_is_signed (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return mode->sign;
}

int
mode_is_float (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (get_mode_sort(mode) == float_number);
}

int
mode_is_int (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (get_mode_sort(mode) == int_number);
}

int
mode_is_num (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (mode_is_int(mode) || mode_is_float(mode));
}

int
mode_is_data (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (mode_is_num(mode) || get_mode_sort(mode) == character || get_mode_sort(mode) == reference);
}

int
mode_is_datab (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (mode_is_data(mode) || get_mode_sort(mode) == internal_boolean);
}

int
mode_is_dataM (ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);
  return (mode_is_data(mode) || get_mode_modecode(mode) == irm_M);
}
#ifdef MODE_ACCESS_DEFINES
#  define mode_is_signed(mode) (mode)->sign
#  define mode_is_float(mode) ((mode)->sort == float_number)
#  define mode_is_int(mode) ((mode)->sort == int_number)
#  define mode_is_num(mode) (((mode)->sort == float_number) || ((mode)->sort == int_number))
#  define mode_is_data(mode) (((mode)->sort == float_number) || ((mode)->sort == int_number) || ((mode)->sort == character) || ((mode)->sort == reference))
#  define mode_is_datab(mode) (((mode)->sort == float_number) || ((mode)->sort == int_number) || ((mode)->sort == character) || ((mode)->sort == reference) || ((mode)->sort == internal_boolean))
#  define mode_is_dataM(mode) (((mode)->sort == float_number) || ((mode)->sort == int_number) || ((mode)->sort == character) || ((mode)->sort == reference) || ((mode)->code == irm_M))
#endif
/* Returns true if sm can be converted to lm without loss. */
int
smaller_mode(ir_mode *sm, ir_mode *lm)
{
  ANNOUNCE();
  assert(sm);
  assert(lm);

  if (sm == lm) return 1;

  switch(get_mode_sort(sm))
  {
    case int_number:
      switch(get_mode_sort(lm))
      {
        case int_number:
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

        case float_number:
          /* int to float works if the float is large enough */
          return 0;

        default:
          break;
      }
      break;

    case float_number:
      /* XXX currently only the three standard 32,64,80 bit floats
       * are supported which can safely be converted */
      if ( (get_mode_sort(lm) == float_number)
           && (get_mode_size_bits(lm) > get_mode_size_bits(sm)) )
         return 1;
      break;

    case reference:
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
  ANNOUNCE();
  /* init flexible array */
  modes      = NEW_ARR_F(ir_mode, irm_max);
  modes_min  = NEW_ARR_F(tarval*, irm_max);
  modes_max  = NEW_ARR_F(tarval*, irm_max);
  modes_null = NEW_ARR_F(tarval*, irm_max);
  modes_one  = NEW_ARR_F(tarval*, irm_max);

  /* initialize predefined modes */
  /* Basic Block */
  mode_BB = &modes[irm_BB];
  mode_BB->name = id_from_str("BB", 2);
  mode_BB->code = irm_BB;
  mode_BB->sort = auxiliary;
  mode_BB->sign = 0;
  mode_BB->tv_priv = NULL;

  /* eXecution */
  mode_X = &modes[irm_X];
  mode_X->name = id_from_str("X", 1);
  mode_X->code = irm_X;
  mode_X->sort = auxiliary;
  mode_X->sign = 0;
  mode_X->tv_priv = NULL;

  /* Memory */
  mode_M = &modes[irm_M];
  mode_M->name = id_from_str("M", 1);
  mode_M->code = irm_M;
  mode_M->sort = auxiliary;
  mode_M->sign = 0;
  mode_M->tv_priv = NULL;

  /* Tuple */
  mode_T = &modes[irm_T];
  mode_T->name = id_from_str("T", 1);
  mode_T->code = irm_T;
  mode_T->sign = 0;
  mode_T->tv_priv = NULL;

  /* boolean */
  mode_b = &modes[irm_b];
  mode_b->name = id_from_str("b", 1);
  mode_b->code = irm_b;
  mode_b->sort = internal_boolean;
  mode_b->sign = 0;
  mode_b->tv_priv = NULL;

  /* float */
  mode_F = &modes[irm_F];
  mode_F->name = id_from_str("F", 1);
  mode_F->code = irm_F;
  mode_F->sort = float_number;
  mode_F->sign = 1;
  mode_F->align = 32;
  mode_F->size = 32;
  mode_F->tv_priv = NULL;

  set_mode_values(mode_F);

  /* double */
  mode_D = &modes[irm_D];
  mode_D->name = id_from_str("D", 1);
  mode_D->code = irm_D;
  mode_D->sort = float_number;
  mode_D->sign = 1;
  mode_D->align = 32;
  mode_D->size = 64;
  mode_D->tv_priv = NULL;

  set_mode_values(mode_D);

  /* extended */
  mode_E = &modes[irm_E];
  mode_E->name = id_from_str("E", 1);
  mode_E->code = irm_E;
  mode_E->sort = float_number;
  mode_E->sign = 1;
  mode_E->align = 32;
  mode_E->size = 80;
  mode_E->tv_priv = NULL;

  set_mode_values(mode_E);

  /* signed byte */
  mode_Bs = &modes[irm_Bs];
  mode_Bs->name = id_from_str("Bs", 2);
  mode_Bs->code = irm_Bs;
  mode_Bs->sort = int_number;
  mode_Bs->sign = 1;
  mode_Bs->align = 8;
  mode_Bs->size = 8;
  mode_Bs->tv_priv = NULL;

  set_mode_values(mode_Bs);

  /* unsigned byte */
  mode_Bu = &modes[irm_Bu];
  mode_Bu->name = id_from_str("Bu", 2);
  mode_Bu->code = irm_Bu;
  mode_Bu->sort = int_number;
  mode_Bu->sign = 0;
  mode_Bu->align = 8;
  mode_Bu->size = 8;
  mode_Bu->tv_priv = NULL;

  set_mode_values(mode_Bu);

  /* signed short integer */
  mode_Hs = &modes[irm_Hs];
  mode_Hs->name = id_from_str("Hs", 2);
  mode_Hs->code = irm_Hs;
  mode_Hs->sort = int_number;
  mode_Hs->sign = 1;
  mode_Hs->align = 16;
  mode_Hs->size = 16;
  mode_Hs->tv_priv = NULL;

  set_mode_values(mode_Hs);

  /* unsigned short integer */
  mode_Hu = &modes[irm_Hu];
  mode_Hu->name = id_from_str("Hu", 2);
  mode_Hu->code = irm_Hu;
  mode_Hu->sort = int_number;
  mode_Hu->sign = 0;
  mode_Hu->align = 16;
  mode_Hu->size = 16;
  mode_Hu->tv_priv = NULL;

  set_mode_values(mode_Hu);

  /* signed integer */
  mode_Is = &modes[irm_Is];
  mode_Is->name = id_from_str("Is", 2);
  mode_Is->code = irm_Is;
  mode_Is->sort = int_number;
  mode_Is->sign = 1;
  mode_Is->align = 32;
  mode_Is->size = 32;
  mode_Is->tv_priv = NULL;

  set_mode_values(mode_Is);

  /* unsigned integer */
  mode_Iu = &modes[irm_Iu];
  mode_Iu->name = id_from_str("Iu", 2);
  mode_Iu->code = irm_Iu;
  mode_Iu->sort = int_number;
  mode_Iu->sign = 0;
  mode_Iu->align = 32;
  mode_Iu->size = 32;
  mode_Iu->tv_priv = NULL;

  set_mode_values(mode_Iu);

  /* signed long integer */
  mode_Ls = &modes[irm_Ls];
  mode_Ls->name = id_from_str("Ls", 2);
  mode_Ls->code = irm_Ls;
  mode_Ls->sort = int_number;
  mode_Ls->sign = 1;
  mode_Ls->align = 32;
  mode_Ls->size = 64;
  mode_Ls->tv_priv = NULL;

  set_mode_values(mode_Ls);

  /* unsigned long integer */
  mode_Lu = &modes[irm_Lu];
  mode_Lu->name = id_from_str("Lu", 2);
  mode_Lu->code = irm_Lu;
  mode_Lu->sort = int_number;
  mode_Lu->sign = 0;
  mode_Lu->align = 32;
  mode_Lu->size = 64;
  mode_Lu->tv_priv = NULL;

  set_mode_values(mode_Lu);

  /* Character */
  mode_C = &modes[irm_C];
  mode_C->name = id_from_str("C", 1);
  mode_C->code = irm_C;
  mode_C->sort = character;
  mode_C->sign = 0;
  mode_C->align = 8;
  mode_C->size = 8;
  mode_C->tv_priv = NULL;

  set_mode_values(mode_C);

  /* Unicode character */
  mode_U = &modes[irm_U];
  mode_U->name = id_from_str("U", 1);
  mode_U->code = irm_U;
  mode_U->sort = character;
  mode_U->sign = 0;
  mode_U->align = 16;
  mode_U->size = 16;
  mode_U->tv_priv = NULL;

  set_mode_values(mode_U);

  /* pointer */
  mode_P = &modes[irm_P];
  mode_P->name = id_from_str("P", 1);
  mode_P->code = irm_P;
  mode_P->sort = reference;
  mode_P->sign = 0;
  mode_P->align = 32;
  mode_P->size = 32;
  mode_P->tv_priv = NULL;

  num_modes = irm_max;
}
