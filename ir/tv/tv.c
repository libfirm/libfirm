/* TV --- Target Values, aka Constant Table.
   Copyright (C) 1995, 1996 Christian von Roques */

/* $Id$ */

/****i* tv/implementation
 *
 * AUTHORS
 *    Christian von Roques
 *    Matthias Heil
 *
 * NOTES
 *    Internal storage for tarvals, 1st draft:
 *   Integers as well as pointers are stored in a hex formatted string holding
 *   16 characters. Booleans are not stored as there are only two of them.
 *
 *    Floats are just reinterpreted as byte strings, because I am not sure if
 *   there is loss if I convert float to long double and back and furthermore
 *   the implementation of a fully ieee compatible floating point emulation
 *   is not sensible for now
 *    With this information it is easy to decide the kind of stored value:
 *   Integers have size 16, floats 4, doubles 8, long doubles 12.
 ******/

/* This implementation assumes:
   * both host and target have IEEE-754 floating-point arithmetic.  */

/* !!! float and double divides MUST NOT SIGNAL !!! */
/* @@@ query the floating-point expception status flags */

/* @@@ Problem: All Values are stored twice, once as Univ_*s and a 2nd
   time in their real target mode. :-( */

#define MAX_INT_LENGTH 8
#define CHAR_BUFFER_SIZE ((MAX_INT_LENGTH) * 2)

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>         /* assertions */
#include <stdlib.h>         /* atoi() */
#include <string.h>         /* nice things for strings */

#include <malloc.h>
#include "tv_t.h"
#include "set.h"            /* to store tarvals in */
#include "tune.h"           /* some constants */
#include "entity_t.h"       /* needed to store pointers to entities */
#include "irmode.h"         /* defines modes etc */
#include "irmode_t.h"
#include "irnode.h"         /* defines boolean return values */
#include "xprintf.h"
#include "xp_help.h"
#include "host.h"
#include "strcalc.h"
#include "fltcalc.h"

/****************************************************************************
 *   local definitions and macros
 ****************************************************************************/
#ifndef NDEBUG
#  define TARVAL_VERIFY(a) tarval_verify((a))
#else
#  define TARVAL_VERIFY(a) ((void)0)
#endif

#define INSERT_TARVAL(tv) ((tarval*)set_insert(tarvals, (tv), sizeof(tarval), hash_tv((tv))))
#define FIND_TARVAL(tv) ((tarval*)set_find(tarvals, (tv), sizeof(tarval), hash_tv((tv))))

#define INSERT_VALUE(val, size) (set_insert(values, (val), size, hash_val((val), size)))
#define FIND_VALUE(val, size) (set_find(values, (val), size, hash_val((val), size)))

#define fail_verify(a) _fail_verify((a), __FILE__, __LINE__)
#if 0
static long long count = 0;
#  define ANNOUNCE() printf(__FILE__": call no. %lld (%s)\n", count++, __FUNCTION__);
#else
#  define ANNOUNCE() ((void)0)
#endif
/****************************************************************************
 *   private variables
 ****************************************************************************/
static struct set *tarvals;   /* container for tarval structs */
static struct set *values;    /* container for values */

/****************************************************************************
 *   private functions
 ****************************************************************************/
#ifndef NDEBUG
static int hash_val(const void *value, unsigned int length);
static int hash_tv(tarval *tv);
static void _fail_verify(tarval *tv, const char* file, int line)
{
  /* print a memory image of the tarval and throw an assertion */
  if (tv)
    printf("%s:%d: Invalid tarval:\n  mode: %s\n value: [%p]\n", file, line, get_mode_name(tv->mode), tv->value);
  else
    printf("%s:%d: Invalid tarval (null)", file, line);
  assert(0);
}

static void tarval_verify(tarval *tv)
{
  assert(tv);
  assert(tv->mode);
  assert(tv->value);

  if ((tv == tarval_bad) || (tv == tarval_undefined)) return;
  if ((tv == tarval_b_true) || (tv == tarval_b_false)) return;

  if (!FIND_TARVAL(tv)) fail_verify(tv);
  if (tv->length > 0 && !FIND_VALUE(tv->value, tv->length)) fail_verify(tv);

  return;
}
#endif /* NDEBUG */

static int hash_tv(tarval *tv)
{
  return ((unsigned int)tv->value ^ (unsigned int)tv->mode) + tv->length;
}

static int hash_val(const void *value, unsigned int length)
{
  unsigned int i;
  unsigned int hash = 0;

  /* scramble the byte - array */
  for (i = 0; i < length; i++)
  {
    hash += (hash << 5) ^ (hash >> 27) ^ ((char*)value)[i];
    hash += (hash << 11) ^ (hash >> 17);
  }

  return hash;
}

/* finds tarval with value/mode or creates new tarval */
static tarval *get_tarval(const void *value, int length, ir_mode *mode)
{
  tarval tv;

  tv.mode = mode;
  tv.length = length;
  if (length > 0)
    /* if there already is such a value, it is returned, else value
     * is copied into the set */
    tv.value = INSERT_VALUE(value, length);
  else
    tv.value = value;

  /* if there is such a tarval, it is returned, else tv is copied
   * into the set */
  return (tarval *)INSERT_TARVAL(&tv);
}

/**
 * Returns non-zero if a tarval overflows.
 *
 * @todo Implementation did not work on all modes
 */
static int overflows(tarval *tv)
{
  switch (get_mode_sort(tv->mode))
  {
    case irms_character:
    case irms_int_number:
      if (sc_comp(tv->value, get_mode_max(tv->mode)->value) == 1) return 1;
      if (sc_comp(tv->value, get_mode_min(tv->mode)->value) == -1) return 1;
      break;

    case irms_float_number:
      if (fc_comp(tv->value, get_mode_max(tv->mode)->value) == 1) return 1;
      if (fc_comp(tv->value, get_mode_min(tv->mode)->value) == -1) return 1;
      break;

    default:
      break;
  }

  return 0;
}

/*
 *   public variables declared in tv.h
 */
tarval *tarval_bad;
tarval *tarval_undefined;
tarval *tarval_b_false;
tarval *tarval_b_true;
tarval *tarval_P_void;

/*
 *   public functions declared in tv.h
 */

/*
 * Constructors =============================================================
 */
tarval *new_tarval_from_str(const char *str, size_t len, ir_mode *mode)
{
  ANNOUNCE();
  assert(str);
  assert(len);
  assert(mode);

  switch (get_mode_sort(mode))
  {
    case irms_auxiliary:
      assert(0);
      break;

    case irms_internal_boolean:
      /* match tTrRuUeE/fFaAlLsSeE */
      if (strcasecmp(str, "true")) return tarval_b_true;
      else if (strcasecmp(str, "false")) return tarval_b_true;
      else
	return atoi(str) ? tarval_b_true : tarval_b_false;

    case irms_float_number:
      fc_val_from_str(str, len);
      return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);

    case irms_int_number:
    case irms_character:
      sc_val_from_str(str, len);
      return get_tarval(sc_get_buffer(), sc_get_buffer_length(), mode);

    case irms_reference:
      return get_tarval(str, len, mode);
  }

  assert(0);  /* can't be reached, can it? */
  return NULL;
}

#if 0
int tarval_is_str(tarval *tv)
{
  ANNOUNCE();
  assert(tv);

  return ((get_mode_sort(tv->mode) == reference) && (tv->value != NULL) && (tv->length > 0));
}
char *tarval_to_str(tarval *tv)
{
  ANNOUNCE();
  assert(tarval_is_str(tv));
  return (char *)tv->value;
}
#endif

/*
 * helper function, creta a tarval from long
 */
tarval *new_tarval_from_long(long l, ir_mode *mode)
{
  ANNOUNCE();
  assert(mode && !(get_mode_sort(mode) == irms_auxiliary));

  switch(get_mode_sort(mode))
  {
    case irms_internal_boolean:
      /* XXX C-Semantics ! */
      return l ? tarval_b_true : tarval_b_false ;

    case irms_int_number:
    case irms_character:
      sc_val_from_long(l);
      return get_tarval(sc_get_buffer(), sc_get_buffer_length(), mode);

    case irms_float_number:
      return new_tarval_from_double((long double)l, mode);

    case irms_reference:
      return l ? tarval_bad : get_tarval(NULL, 0, mode);  /* null pointer or tarval_bad */

    default:
      assert(0);
  }
  return NULL;
}

/* returns non-zero if can be converted to long */
int tarval_is_long(tarval *tv)
{
  ANNOUNCE();
  return ((get_mode_sort(tv->mode) == irms_int_number) || (get_mode_sort(tv->mode) == irms_character));
}

/* this might overflow the machine's long, so use only with small values */
long tarval_to_long(tarval* tv)
{
  ANNOUNCE();
  assert(tv && get_mode_sort(tv->mode) == irms_int_number);

  return sc_val_to_long(tv->value); /* might overflow */
}

tarval *new_tarval_from_double(long double d, ir_mode *mode)
{
  ANNOUNCE();
  assert(mode && (get_mode_sort(mode) == irms_float_number));

  fc_val_from_float(d);
  return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);
}

/* returns non-zero if can be converted to double */
int tarval_is_double(tarval *tv)
{
  ANNOUNCE();
  assert(tv);

  return (get_mode_sort(tv->mode) == irms_float_number);
}

long double tarval_to_double(tarval *tv)
{
  ANNOUNCE();
  assert(tarval_is_double(tv));

  return fc_val_to_float(tv->value);
}

/* The tarval represents the address of the entity.  As the address must
   be constant the entity must have as owner the global type. */
tarval *new_tarval_from_entity (entity *ent, ir_mode *mode)
{
  ANNOUNCE();
  assert(ent);
  assert(mode && (get_mode_sort(mode) == irms_reference));

  return get_tarval((void *)ent, 0, mode);
}
int tarval_is_entity(tarval *tv)
{
  ANNOUNCE();
  assert(tv);
  /* tv->value == NULL means dereferencing a null pointer */
  return ((get_mode_sort(tv->mode) == irms_reference) && (tv->value != NULL) && (tv->length == 0));
}

entity *tarval_to_entity(tarval *tv)
{
  ANNOUNCE();
  assert(tv);

  if (tarval_is_entity(tv))
    return (entity *)tv->value;
  else {
    assert(0 && "tarval did not represent an entity");
    return NULL;
  }
}

void free_tarval_entity(entity *ent) {
  /* There can be a tarval referencing this entity.  Even if the
     tarval is not used by the code any more, it can still reference
     the entity as tarvals live indepently of the entity referenced.
     Further the tarval is hashed into a set. If a hash function
     evaluation happens to collide with this tarval, we will vrfy that
     it contains a proper entity and we will crash if the entity is
     freed.

     Unluckily, tarvals can neither be changed nor deleted, and to find
     one, all existing reference modes have to be tried -> a facility
     to retrieve all modes of a kind is needed. */
  ANNOUNCE();
}

/*
 * Access routines for tarval fields ========================================
 */
ir_mode *get_tarval_mode (tarval *tv)       /* get the mode of the tarval */
{
  ANNOUNCE();
  assert(tv);
  return tv->mode;
}

/*
 * Special value query functions ============================================
 *
 * These functions calculate and return a tarval representing the requested
 * value.
 * The functions get_mode_{Max,Min,...} return tarvals retrieved from these
 * functions, but these are stored on initialization of the irmode module and
 * therefore the irmode functions should be prefered to the functions below.
 */

tarval *get_tarval_bad(void)
{
  ANNOUNCE();
  return tarval_bad;
}
tarval *get_tarval_undefined(void)
{
  ANNOUNCE();
  return tarval_undefined;
}
tarval *get_tarval_b_false(void)
{
  ANNOUNCE();
  return tarval_b_false;
}
tarval *get_tarval_b_true(void)
{
  ANNOUNCE();
  return tarval_b_true;
}
tarval *get_tarval_P_void(void)
{
  ANNOUNCE();
  return tarval_P_void;
}

tarval *get_tarval_max(ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);

  switch(get_mode_sort(mode))
  {
    case irms_reference:
    case irms_auxiliary:
      assert(0);
      break;

    case irms_internal_boolean:
      return tarval_b_true;

    case irms_float_number:
      fc_get_max(get_mode_size_bits(mode));
      return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);

    case irms_int_number:
    case irms_character:
      sc_max_from_bits(get_mode_size_bits(mode), mode_is_signed(mode));
      return get_tarval(sc_get_buffer(), sc_get_buffer_length(), mode);
  }
  return tarval_bad;
}

tarval *get_tarval_min(ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);

  switch(get_mode_sort(mode))
  {
    case irms_reference:
    case irms_auxiliary:
      assert(0);
      break;

    case irms_internal_boolean:
      return tarval_b_false;

    case irms_float_number:
      fc_get_min(get_mode_size_bits(mode));
      return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);

    case irms_int_number:
    case irms_character:
      sc_min_from_bits(get_mode_size_bits(mode), mode_is_signed(mode));
      return get_tarval(sc_get_buffer(), sc_get_buffer_length(), mode);
  }
  return tarval_bad;
}

tarval *get_tarval_null(ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);

  switch(get_mode_sort(mode))
  {
    case irms_auxiliary:
    case irms_internal_boolean:
      assert(0);
      break;

    case irms_float_number:
      return new_tarval_from_double(0.0, mode);

    case irms_int_number:
    case irms_character:
      return new_tarval_from_long(0l,  mode);

    case irms_reference:
      return tarval_P_void;
  }
  return tarval_bad;
}

tarval *get_tarval_one(ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);

  switch(get_mode_sort(mode))
  {
    case irms_auxiliary:
    case irms_internal_boolean:
    case irms_reference:
      assert(0);
      break;

    case irms_float_number:
      return new_tarval_from_double(1.0, mode);

    case irms_int_number:
    case irms_character:
      return new_tarval_from_long(1l, mode);
      break;
  }
  return tarval_bad;
}

tarval *get_tarval_nan(ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);

  if (get_mode_sort(mode) == irms_float_number) {
    fc_get_nan();
    return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);
  }
  else {
    assert(0 && "tarval is not floating point");
    return tarval_bad;
  }
}

tarval *get_tarval_inf(ir_mode *mode)
{
  ANNOUNCE();
  assert(mode);

  if (get_mode_sort(mode) == irms_float_number) {
    fc_get_inf();
    return get_tarval(fc_get_buffer(), fc_get_buffer_length(), mode);
  }
  else {
    assert(0 && "tarval is not floating point");
    return tarval_bad;
  }
}

/*
 * Arithmethic operations on tarvals ========================================
 */

/*
 * test if negative number, 1 means 'yes'
 */
int tarval_is_negative(tarval *a)
{
  ANNOUNCE();
  assert(a);

  switch (get_mode_sort(a->mode))
  {
    case irms_int_number:
      if (!mode_is_signed(a->mode)) return 0;
      else
	return sc_comp(a->value, get_mode_null(a->mode)->value) == -1 ? 1 : 0;

    case irms_float_number:
      return fc_comp(a->value, get_mode_null(a->mode)->value) == -1 ? 1 : 0;

    default:
      assert(0 && "not implemented");
      return 0;
  }
}

/*
 * comparison
 */
pnc_number tarval_cmp(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);

  if (a == tarval_bad || b == tarval_bad) assert(0 && "Comparison with tarval_bad");
  if (a == tarval_undefined || b == tarval_undefined) return False;
  if (a == b) return Eq;
  if (get_tarval_mode(a) != get_tarval_mode(b)) return Uo;

  /* Here the two tarvals are unequal and of the same mode */
  switch (get_mode_sort(a->mode))
  {
    case irms_auxiliary:
      return False;

    case irms_float_number:
      return (fc_comp(a->value, b->value)==1)?(Gt):(Lt);

    case irms_int_number:
    case irms_character:
      return (sc_comp(a->value, b->value)==1)?(Gt):(Lt);

    case irms_internal_boolean:
      return (a == tarval_b_true)?(Gt):(Lt);

    case irms_reference:
      return Uo;
  }
  return False;
}

/*
 * convert to other mode
 */
tarval *tarval_convert_to(tarval *src, ir_mode *m)
{
  ANNOUNCE();
  tarval tv;

  assert(src);
  assert(m);

  if (src->mode == m) return src;

  switch (get_mode_sort(src->mode))
  {
    case irms_auxiliary:
      break;

    case irms_float_number:
      break;

    case irms_int_number:
      switch (get_mode_sort(m))
      {
        case irms_int_number:
        case irms_character:
          tv.mode = m;
          tv.length = src->length;
          tv.value = src->value;
          if (overflows(&tv))
          {
            return tarval_bad;
          }
          return INSERT_TARVAL(&tv);

        case irms_internal_boolean:
          /* XXX C semantics */
          if (src == get_mode_null(src->mode)) return tarval_b_false;
          else return tarval_b_true;

        default:
          break;
      }
      break;

    case irms_internal_boolean:
      switch (get_mode_sort(m))
      {
        case irms_int_number:
          if (src == tarval_b_true) return get_mode_one(m);
          else return get_mode_null(m);

        default:
          break;
      }
      break;

    case irms_character:
      break;
    case irms_reference:
      break;
  }

  return tarval_bad;
}

/*
 * negation
 */
tarval *tarval_neg(tarval *a)
{
  ANNOUNCE();
  assert(a);
  assert(mode_is_num(a->mode)); /* negation only for numerical values */
  assert(mode_is_signed(a->mode)); /* negation is difficult without negative numbers, isn't it */

  switch (get_mode_sort(a->mode))
  {
    case irms_int_number:
      sc_neg(a->value);
      return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);

    case irms_float_number:
      fc_neg(a->value);
      return get_tarval(fc_get_buffer(), fc_get_buffer_length(), a->mode);

    default:
      return tarval_bad;
  }
}

/*
 * addition
 */
tarval *tarval_add(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert((a->mode == b->mode) || (get_mode_sort(a->mode) == irms_character && mode_is_int(b->mode)));

  switch (get_mode_sort(a->mode))
  {
    case irms_character:
    case irms_int_number:
      /* modes of a,b are equal, so result has mode of a as this might be the character */
      sc_add(a->value, b->value);
      /* FIXME: Check for overflow */
      return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);

    case irms_float_number:
      /* FIXME: Overflow/Underflow/transition to inf when mode < 80bit */
      fc_add(a->value, b->value);
      return get_tarval(fc_get_buffer(), fc_get_buffer_length(), a->mode);

    default:
      return tarval_bad;
  }
}

/*
 * subtraction
 */
tarval *tarval_sub(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert((a->mode == b->mode) || (get_mode_sort(a->mode) == irms_character && mode_is_int(b->mode)));

  switch (get_mode_sort(a->mode))
  {
    case irms_character:
    case irms_int_number:
      /* modes of a,b are equal, so result has mode of a as this might be the character */
      sc_sub(a->value, b->value);
      /* FIXME: check for overflow */
      return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);

    case irms_float_number:
      /* FIXME: Overflow/Underflow/transition to inf when mode < 80bit */
      fc_add(a->value, b->value);
      return get_tarval(fc_get_buffer(), fc_get_buffer_length(), a->mode);

    default:
      return tarval_bad;
  }
}

/*
 * multiplication
 */
tarval *tarval_mul(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert((a->mode == b->mode) && mode_is_num(a->mode));

  switch (get_mode_sort(a->mode))
  {
    case irms_int_number:
      /* modes of a,b are equal */
      sc_mul(a->value, b->value);
      /* FIXME: check for overflow */
      return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);

    case irms_float_number:
      /* FIXME: Overflow/Underflow/transition to inf when mode < 80bit */
      fc_add(a->value, b->value);
      return get_tarval(fc_get_buffer(), fc_get_buffer_length(), a->mode);

    default:
      return tarval_bad;
  }
}

/*
 * floating point division
 */
tarval *tarval_quo(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert((a->mode == b->mode) && mode_is_float(a->mode));

  /* FIXME: Overflow/Underflow/transition to inf when mode < 80bit */
  fc_div(a->value, b->value);
  return get_tarval(fc_get_buffer(), fc_get_buffer_length(), a->mode);
}

/*
 * integer division
 */
tarval *tarval_div(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert((a->mode == b->mode) && mode_is_int(a->mode));

  /* modes of a,b are equal */
  sc_div(a->value, b->value);
  return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * remainder
 */
tarval *tarval_mod(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert((a->mode == b->mode) && mode_is_int(a->mode));

  /* modes of a,b are equal */
  sc_mod(a->value, b->value);
  return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * absolute value
 */
tarval *tarval_abs(tarval *a)
{
  ANNOUNCE();
  assert(a);
  assert(mode_is_num(a->mode));

  switch (get_mode_sort(a->mode))
  {
    case irms_int_number:
      if (sc_comp(a->value, get_mode_null(a->mode)->value) == -1)
      {
        sc_neg(a->value);
        return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
      }
      return a;

    case irms_float_number:
      break;

    default:
      return tarval_bad;
  }
  return tarval_bad;
}

/*
 * bitwise and
 */
tarval *tarval_and(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert((a->mode == b->mode) && mode_is_int(a->mode));

  sc_and(a->value, b->value);
  return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * bitwise or
 */
tarval *tarval_or (tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert((a->mode == b->mode) && mode_is_int(a->mode));

  sc_or(a->value, b->value);
  return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * bitwise exclusive or (xor)
 */
tarval *tarval_eor(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert((a->mode == b->mode) && mode_is_int(a->mode));

  sc_or(a->value, b->value);
  return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * bitwise left shift
 */
tarval *tarval_shl(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert(mode_is_int(a->mode) && mode_is_int(b->mode));

  sc_shl(a->value, b->value, get_mode_size_bits(a->mode), mode_is_signed(a->mode));
  return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * bitwise unsigned right shift
 */
tarval *tarval_shr(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert(mode_is_int(a->mode) && mode_is_int(b->mode));

  sc_shr(a->value, b->value, get_mode_size_bits(a->mode), mode_is_signed(a->mode));
  return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * bitwise signed right shift
 */
tarval *tarval_shrs(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert(mode_is_int(a->mode) && mode_is_int(b->mode));

  sc_shrs(a->value, b->value, get_mode_size_bits(a->mode), mode_is_signed(a->mode));
  return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * bitwise rotation
 */
tarval *tarval_rot(tarval *a, tarval *b)
{
  ANNOUNCE();
  assert(a);
  assert(b);
  assert(mode_is_int(a->mode) && mode_is_int(b->mode));

  sc_rot(a->value, b->value, get_mode_size_bits(a->mode), mode_is_signed(a->mode));
  return get_tarval(sc_get_buffer(), sc_get_buffer_length(), a->mode);
}

/*
 * Output of tarvals
 */
int tarval_print(XP_PAR1, const xprintf_info *info ATTRIBUTE((unused)), XP_PARN)
{
  static const tarval_mode_info default_info = { TVO_NATIVE, NULL, NULL };

  tarval *tv;
  const char *str;
  char buf[100];
  const tarval_mode_info *mode_info;
  const char *prefix, *suffix;

  ANNOUNCE();

  tv = XP_GETARG(tarval *, 0);
  mode_info = tv->mode->tv_priv;
  if (! mode_info)
    mode_info = &default_info;
  prefix = mode_info->mode_prefix ? mode_info->mode_prefix : "";
  suffix = mode_info->mode_suffix ? mode_info->mode_suffix : "";

  switch (get_mode_sort(tv->mode))
  {
    case irms_int_number:
    case irms_character:
      switch (mode_info->mode_output) {

      case TVO_DECIMAL:
        str = sc_print(tv->value, get_mode_size_bits(tv->mode), SC_DEC);
	break;

      case TVO_OCTAL:
        str = sc_print(tv->value, get_mode_size_bits(tv->mode), SC_OCT);
	break;

      case TVO_HEX:
      case TVO_NATIVE:
      default:
        str = sc_print(tv->value, get_mode_size_bits(tv->mode), SC_HEX);
	break;
      }
      return XPF3R("%s%s%s", prefix, str, suffix);

    case irms_float_number:
      return XPF3R("%s%s%s", prefix, fc_print_dec(tv->value, buf, sizeof(buf)), suffix);

    case irms_reference:
      if (tv->value != NULL)
        if (tarval_is_entity(tv))
          if (get_entity_peculiarity((entity *)tv->value) == existent)
            return XPF1R("&(%I)", get_entity_ld_ident((entity *)tv->value));
          else
            return XPSR("NULL");
        else
          return XPMR((char*)tv->value, tv->length);
      else
        return XPSR("void");

    case irms_internal_boolean:
      switch (mode_info->mode_output) {

      case TVO_DECIMAL:
      case TVO_OCTAL:
      case TVO_HEX:
      case TVO_BINARY:
        return XPF3R("%s%c%s", prefix, (tv == tarval_b_true) ? '1' : '0', suffix);

      case TVO_NATIVE:
      default:
        return XPF3R("%s%s%s", prefix, (tv == tarval_b_true) ? "true" : "false", suffix);
      }

    case irms_auxiliary:
      return XPSR("<BAD>");
  }

  return 0;
}

/*
 * Output of tarvals
 */
int tarval_snprintf(char *buf, size_t len, tarval *tv)
{
  static const tarval_mode_info default_info = { TVO_NATIVE, NULL, NULL };

  const char *str;
  char tv_buf[100];
  const tarval_mode_info *mode_info;
  const char *prefix, *suffix;

  ANNOUNCE();

  mode_info = tv->mode->tv_priv;
  if (! mode_info)
    mode_info = &default_info;
  prefix = mode_info->mode_prefix ? mode_info->mode_prefix : "";
  suffix = mode_info->mode_suffix ? mode_info->mode_suffix : "";

  switch (get_mode_sort(tv->mode))
  {
    case irms_int_number:
    case irms_character:
      switch (mode_info->mode_output) {

      case TVO_DECIMAL:
        str = sc_print(tv->value, get_mode_size_bits(tv->mode), SC_DEC);
	break;

      case TVO_OCTAL:
        str = sc_print(tv->value, get_mode_size_bits(tv->mode), SC_OCT);
	break;

      case TVO_HEX:
      case TVO_NATIVE:
      default:
        str = sc_print(tv->value, get_mode_size_bits(tv->mode), SC_HEX);
	break;
      }
      return snprintf(buf, len, "%s%s%s", prefix, str, suffix);

    case irms_float_number:
      return snprintf(buf, len, "%s%s%s", prefix, fc_print_dec(tv->value, tv_buf, sizeof(tv_buf)), suffix);

    case irms_reference:
      if (tv->value != NULL)
        if (tarval_is_entity(tv))
          if (get_entity_peculiarity((entity *)tv->value) == existent)
            return snprintf(buf, len, "&(%s)", id_to_str(get_entity_ld_ident((entity *)tv->value)));
          else
            return snprintf(buf, len, "NULL");
        else {
	  if (size > tv->length) {
	    memcpy(buf, tv->value, tv->length);
	    buf[tv->length] = '\0';
	  }
	  else {
	    /* truncated */
	    memcpy(buf, tv->value, size-1);
	    buf[size-1] = '\0';
	  }
          return tv->length;
	}
      else
        return snprintf(buf, len, "void");

    case irms_internal_boolean:
      switch (mode_info->mode_output) {

      case TVO_DECIMAL:
      case TVO_OCTAL:
      case TVO_HEX:
      case TVO_BINARY:
        return snprintf(buf, len, "%s%c%s", prefix, (tv == tarval_b_true) ? '1' : '0', suffix);

      case TVO_NATIVE:
      default:
        return snprintf(buf, len, "%s%s%s", prefix, (tv == tarval_b_true) ? "true" : "false", suffix);
      }

    case irms_auxiliary:
      return snprintf(buf, len, "<BAD>");
  }

  return 0;
}

char *tarval_bitpattern(tarval *tv)
{
  return NULL;
}

/*
 * access to the bitpattern
 */
unsigned char tarval_sub_bits(tarval *tv, unsigned byte_ofs)
{
  switch (get_mode_sort(tv->mode)) {
    case irms_int_number:
    case irms_character:
      return sc_sub_bits(tv->value, tv->length, byte_ofs);

    case irms_float_number:
      return fc_sub_bits(tv->value, get_mode_size_bits(tv->mode), byte_ofs);

    default:
      return 0;
  }
}

/*
 * Specify the output options of one mode.
 *
 * This functions stores the modinfo, so DO NOT DESTROY it.
 *
 * Returns zero on success.
 */
int tarval_set_mode_output_option(ir_mode *mode, const tarval_mode_info *modeinfo)
{
  assert(mode);

  mode->tv_priv = modeinfo;
  return 0;
}

/* Identifying some tarvals ??? */
/* Implemented in old tv.c as such:
 *   return 0 for additive neutral,
 *   1 for multiplicative neutral,
 *   -1 for bitwise-and neutral
 *   2 else
 *
 * Implemented for completeness */
long tarval_classify(tarval *tv)
{
  ANNOUNCE();
  if (!tv || tv == tarval_bad) return 2;

  if (tv == get_mode_null(tv->mode)) return 0;
  else if (tv == get_mode_one(tv->mode)) return 1;
  else if ((get_mode_sort(tv->mode) == irms_int_number)
           && (tv == new_tarval_from_long(-1, tv->mode))) return -1;

  return 2;
}

/*
 * Initialization of the tarval module: called before init_mode()
 */
void init_tarval_1(void)
{
  ANNOUNCE();
  /* initialize the sets holding the tarvals with a comparison function and
   * an initial size, which is the expected number of constants */
  tarvals = new_set(memcmp, TUNE_NCONSTANTS);
  values = new_set(memcmp, TUNE_NCONSTANTS);
  /* init with default precision */
  init_strcalc(0);
  /* init_fltcalc(0); not yet*/
}

/**
 * default mode_info for output as HEX
 */
static const tarval_mode_info hex_output = {
  TVO_HEX,
  "0x",
  NULL,
};

/*
 * Initialization of the tarval module: called after init_mode()
 */
void init_tarval_2(void)
{
  ANNOUNCE();

  tarval_bad = (tarval*)malloc(sizeof(tarval));
  tarval_bad->mode = NULL;

  tarval_undefined = (tarval*)malloc(sizeof(tarval));
  tarval_undefined->mode = NULL;

  tarval_b_true = (tarval*)malloc(sizeof(tarval));
  tarval_b_true->mode = mode_b;

  tarval_b_false = (tarval*)malloc(sizeof(tarval));
  tarval_b_false->mode = mode_b;

  tarval_P_void = (tarval*)malloc(sizeof(tarval));
  tarval_P_void->mode = mode_P;

  /*
   * assign output modes that are compatible with the
   * old implementation: Hex output
   */
  tarval_set_mode_output_option(mode_U,  &hex_output);
  tarval_set_mode_output_option(mode_C,  &hex_output);
  tarval_set_mode_output_option(mode_Bs, &hex_output);
  tarval_set_mode_output_option(mode_Bu, &hex_output);
  tarval_set_mode_output_option(mode_Hs, &hex_output);
  tarval_set_mode_output_option(mode_Hu, &hex_output);
  tarval_set_mode_output_option(mode_Hs, &hex_output);
  tarval_set_mode_output_option(mode_Hu, &hex_output);
  tarval_set_mode_output_option(mode_Is, &hex_output);
  tarval_set_mode_output_option(mode_Iu, &hex_output);
  tarval_set_mode_output_option(mode_Ls, &hex_output);
  tarval_set_mode_output_option(mode_Lu, &hex_output);
}

/****************************************************************************
 *   end of tv.c
 ****************************************************************************/
