/* TV --- Target Values, aka Constant Table.
   Copyright (C) 1995, 1996 Christian von Roques */

/* $Id$ */

/* This implementation assumes:
   * target characters/strings can be represented as type `char'/`char *',
   * host's type `long'/`unsigned long' can hold values of mode `l'/`L',
   * both host and target have two's complement integral arithmetic,
     host's C operators `/' and `%' match target's div and mod.
     target_max_<mode> == (1<<k)-1 for some k>0
     target_min_<mode> == -target_max_<mode>-1
     target_max_<Mode> == target_max_<mode>-target_min_<mode>
   * both host and target have IEEE-754 floating-point arithmetic.  */

/* !!! float and double divides MUST NOT SIGNAL !!! */
/* @@@ query the floating-point expception status flags */

/* @@@ ToDo: tarval_convert_to is not fully implemented! */
/* @@@ Problem: All Values are stored twice, once as Univ_*s and a 2nd
   time in their real target mode. :-( */
/* @@@ Perhaps use a set instead of a pset: new tarvals allocated on
   stack, copied into set by tarval_identify() if really new.  If
   tarval_identify() discards often enough, the extra copy for kept
   values is cheaper than the extra obstack_alloc()/free() for
   discarded ones.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "xprintf.h"
#include <assert.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "pset.h"
#define TOBSTACK_ID "tv"
#include "obst.h"
#include "ieee754.h"
#include "tune.h"
#include "ident_t.h"
#include "tv_t.h"
#include "entity_t.h"
#include "irmode.h"

static struct obstack tv_obst;	/* obstack for all the target values */
static pset *tarvals;		/* pset containing pointers to _all_ tarvals */

/* currently building an object with tarval_start() & friends ? */
#define BUILDING obstack_object_size (&tv_obst)

/* bcopy is not ISO C */
#define bcopy(X, Y, Z) memcpy((Y), (X), (Z))


/* special tarvals: */
tarval *tarval_bad;
tarval *tarval_b_false;
tarval *tarval_b_true;
tarval *tarval_D_NaN;
tarval *tarval_D_Inf;
tarval *tarval_P_void;
tarval *tarval_mode_null[irm_max];
tarval *tarval_mode_min[irm_max];
tarval *tarval_mode_max[irm_max];

tarval *get_tarval_bad      ()              { return tarval_bad;     }
tarval *get_tarval_b_false  ()              { return tarval_b_false; }
tarval *get_tarval_b_true   ()              { return tarval_b_true;  }
tarval *get_tarval_D_NaN    ()              { return tarval_D_NaN;   }
tarval *get_tarval_D_Inf    ()              { return tarval_D_Inf;   }
tarval *get_tarval_P_void   ()              { return tarval_P_void;  }
tarval *get_tarval_mode_null(ir_mode *mode)
  { return tarval_mode_null[get_mode_modecode(mode)]; }
tarval *get_tarval_mode_min (ir_mode *mode)
  { return tarval_mode_min[get_mode_modecode(mode)];  }
tarval *get_tarval_mode_max (ir_mode *mode)
  { return tarval_mode_max[get_mode_modecode(mode)];  }

# if 0
/* @@@ depends on order of ir_mode */
static tarval_sInt min_sInt[8] = {
  TARGET_SIMIN (c), 0,
  TARGET_SIMIN (h), 0,
  TARGET_SIMIN (i), 0,
  TARGET_SIMIN (l), 0
};
static tarval_sInt max_sInt[8] = {
  TARGET_SIMAX (c), TARGET_UIMAX (C),
  TARGET_SIMAX (h), TARGET_UIMAX (H),
  TARGET_SIMAX (i), TARGET_UIMAX (I),
  TARGET_SIMAX (l), TARGET_UIMAX (L)
};
# endif

/* Used to be in irmode.h, replaced now. */
# define is_Int(m) ((m) <= irm_Lu && (m) >= irm_Bs) /* old */

/* return a mode-specific value */

tarval_F
tv_val_F (tarval *tv)
{
  return tv->u.F;
}

tarval_D
tv_val_D (tarval *tv)
{
  return tv->u.D;
}

tarval_E
tv_val_E (tarval *tv)
{
  return tv->u.E;
}

tarval_sInt
tv_val_sInt (tarval *tv)
{
  return tv->u.sInt;
}

tarval_uInt
tv_val_uInt (tarval *tv)
{
  return tv->u.uInt;
}

tarval_P
tv_val_P (tarval *tv)
{
  return tv->u.P;
}

bool
tv_val_b (tarval *tv)
{
  return tv->u.b;
}


/* Overflows `sInt' signed integral `mode'?  */
static INLINE bool
sInt_overflow (tarval_sInt sInt, ir_mode *mode)
{
  assert (is_Int(get_mode_modecode(mode)));
  return (get_mode_min(mode) && get_mode_max(mode)  /* only valid after firm initialization */
	  && (sInt < tv_val_sInt (get_mode_min(mode))
	      || tv_val_sInt (get_mode_max(mode)) < sInt));
}


/* Overflows `uInt' unsigned integral `mode'?  */
static INLINE bool
uInt_overflow (tarval_uInt uInt, ir_mode *mode)
{
  assert (is_Int(get_mode_modecode(mode)));
  return (get_mode_max(mode)   /* only valid after firm initialization */
	  && tv_val_uInt (get_mode_max(mode)) < uInt);
}


#ifndef NDEBUG
void
_tarval_vrfy (const tarval *val)
{
  assert (val);
  switch (get_mode_modecode(val->mode)) {
    /* floating */
  case irm_F:
  case irm_D:
  case irm_E:
    break;
    /* integral */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    assert (!uInt_overflow (val->u.uInt, val->mode)); break;
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    assert (!sInt_overflow (val->u.sInt, val->mode)); break;
  case irm_C:
    break;
  case irm_P:
    if (val->u.P.ent)
      assert (val->u.P.ent->kind == k_entity);
    assert (   val->u.P.xname || val->u.P.ent
	    || !tarval_P_void || (val == tarval_P_void));
    break;
  case irm_b:
    assert ((unsigned)val->u.b <= 1); break;
  default:
    assert (val->mode == mode_T);
  }
}
#endif


#ifdef STATS

void
tarval_stats (void)
{
  pset_stats (tarvals);
}

#endif


/* Return the canonical tarval * for tv.
   May destroy everything allocated on tv_obst after tv!  */
static tarval *
tarval_identify (tarval *tv)
{
  tarval *o;

  o = pset_insert (tarvals, tv, tarval_hash (tv));

  if (o != tv) {
    obstack_free (&tv_obst, (void *)tv);
  }

  TARVAL_VRFY (o);
  return o;
}


/* Return 0 iff a equals b.  Bitwise identical NaNs compare equal.  */
static int
tarval_cmp (const void *p, const void *q)
{
  const tarval *a = p;
  const tarval *b = q;

  TARVAL_VRFY (a);
  TARVAL_VRFY (b);

  if (a == b) return 0;
  if ((void *)a->mode - (void *)b->mode)
    return (void *)a->mode - (void *)b->mode;

  switch (get_mode_modecode(a->mode)) {
    /* floating */
  case irm_F:
    return memcmp (&a->u.F, &b->u.F, sizeof (a->u.F));
  case irm_D:
    return memcmp (&a->u.D, &b->u.D, sizeof (a->u.D));
  case irm_E:
    return memcmp (&a->u.E, &b->u.E, sizeof (a->u.E));
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    if (sizeof (int) == sizeof (tarval_uInt)) {
      return a->u.uInt - b->u.uInt;
    }
    return a->u.uInt != b->u.uInt;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    if (sizeof (int) == sizeof (tarval_sInt)) {
      return a->u.sInt - b->u.sInt;
    }
    return a->u.sInt != b->u.sInt;
  case irm_C:
    return a->u.C - b->u.C;
  case irm_P:
    if (a->u.P.ent || b->u.P.ent)
      return (char *)a->u.P.ent - (char *)b->u.P.ent;
    if (a->u.P.xname && b->u.P.xname)
      return strcmp (a->u.P.xname, b->u.P.xname);
    return a->u.P.xname - b->u.P.xname;
  case irm_b:
    return a->u.b - b->u.b;
  default: assert (0);
  }
}


unsigned
tarval_hash (tarval *tv)
{
  unsigned h;

  h = get_mode_modecode(tv->mode) * 0x421u;
  switch (get_mode_modecode(tv->mode)) {
  case irm_T:
    h = 0x94b527ce; break;
  case irm_F:
    /* quick & dirty */
    { union { float f; unsigned u; } u;
      assert (sizeof (float) <= sizeof (unsigned));
      u.u = 0; u.f = tv->u.F;
      h ^= u.u;
      break;
    }
  case irm_D:
    /* quick & dirty */
    { union { double d; unsigned u[2]; } u;
      assert (sizeof (double) <= 2*sizeof (unsigned));
      u.u[0] = u.u[1] = 0; u.d = tv->u.D;
      h ^= u.u[0] ^ u.u[1];
      break;
    }
  case irm_E:
    { union { long double e; unsigned u[3]; } u;
      assert (sizeof (long double) <= 3*sizeof (unsigned));
      u.u[0] = u.u[1] = u.u[2] = 0; u.e = tv->u.E;
      h ^= u.u[0] ^ u.u[1] ^ u.u[2];
      break;
    }
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    h ^= tv->u.uInt; break;
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    h ^= tv->u.sInt; break;
  case irm_C:
    h ^= tv->u.C; break;
  case irm_P:
    if (tv->u.P.ent) {
      /* @@@ lower bits not random, watch for collisions; perhaps
	 replace by tv->u.p.ent - (entity *)0 */
      h ^= ((char *)tv->u.P.ent - (char *)0) / 64;
    } else if (tv->u.P.xname) {
      /* Of course, strlen() in a hash function is a mistake, but this
         case should be really rare.  */
      h ^= ID_HASH (tv->u.P.xname, strlen (tv->u.P.xname));
    } else {			/* void */
      h^= 0x2b592b88;
    }
    break;
  case irm_b:
    h ^= tv->u.b; break;
  default:
    assert(0);
  }
  return h;
}


/*** ***************** Initialization ************************************* ***/

void
tarval_init_1 (void)
{
  obstack_init (&tv_obst);
  obstack_alignment_mask (&tv_obst) = ALIGNOF (tarval) - 1;
  assert (IS_POW2 (ALIGNOF (tarval)));

  /* initialize the target value table */
  tarvals = new_pset (tarval_cmp, TUNE_NCONSTANTS);
}

void
tarval_init_2 (void)
{
  tarval *tv;
  union ieee754_double x;

  /* assumed by tarval_hash(): */
  assert (sizeof (float) * CHAR_BIT == 32);
  assert (sizeof (double) * CHAR_BIT == 64);

# if 0
  /* assumed by tarval_sInt & friends: */
  assert (   (irm_C == irm_c+1) && (irm_h == irm_C+1)
	  && (irm_H == irm_h+1) && (irm_i == irm_H+1)
	  && (irm_I == irm_i+1) && (irm_l == irm_I+1)
	  && (irm_L == irm_l+1));

  /* assumed everywhere: */
  for (i = 0;  i <= irm_L-irm_c;  i += 2) {
    assert (   IS_POW2 (max_sInt[i+1]+1)
	    && (min_sInt[i] == -max_sInt[i]-1)
	    && ((tarval_uInt)max_sInt[i+1] == (tarval_uInt)max_sInt[i]-min_sInt[i]));
  }
# endif


  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_T;
  tarval_bad = tarval_identify (tv);

  tarval_b_false = tarval_from_long (mode_b, 0);
  tarval_b_true = tarval_from_long (mode_b, 1);

  /* IsInf <-> exponent == 0x7ff && ! (bits | fraction_low) */
  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_D;
  x.ieee.negative = 0;
  x.ieee.exponent = 0x7ff;
  x.ieee.mantissa0 = 0;
  x.ieee.mantissa1 = 0;
  tv->u.D = x.d;
  tarval_D_Inf = tarval_identify (tv);

  /* IsNaN <-> exponent==0x7ff  && (qnan_bit | bits | fraction_low) */
  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_D;
  x.ieee_nan.negative = 0;
  x.ieee_nan.exponent = 0x7ff;
  x.ieee_nan.quiet_nan = 1;	/* @@@ quiet or signalling? */
  x.ieee_nan.mantissa0 = 42;
  x.ieee_nan.mantissa1 = 0;
  assert(x.d != x.d /* x.d is NaN */);
  tv->u.D = x.d;
  tarval_D_NaN = tarval_identify (tv);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_P;
  tv->u.P.xname = NULL;
  tv->u.P.ent = NULL;
  tv->u.P.tv = NULL;
  tarval_P_void = tarval_identify (tv);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));


  tarval_mode_null [irm_F] = tarval_from_long (mode_F, 0);
  tarval_mode_null [irm_D] = tarval_from_long (mode_D, 0);
  tarval_mode_null [irm_E] = tarval_from_long (mode_E, 0);
  tarval_mode_null [irm_Bs] = tarval_from_long (mode_Bs, 0);
  tarval_mode_null [irm_Bu] = tarval_from_long (mode_Bu, 0);
  tarval_mode_null [irm_Hs] = tarval_from_long (mode_Hs, 0);
  tarval_mode_null [irm_Hu] = tarval_from_long (mode_Hu, 0);
  tarval_mode_null [irm_Is] = tarval_from_long (mode_Is, 0);
  tarval_mode_null [irm_Iu] = tarval_from_long (mode_Iu, 0);
  tarval_mode_null [irm_Ls] = tarval_from_long (mode_Ls, 0);
  tarval_mode_null [irm_Lu] = tarval_from_long (mode_Lu, 0);
  tarval_mode_null [irm_C] = tarval_from_long (mode_C, 0);
  tarval_mode_null [irm_b] = tarval_b_false;
  tarval_mode_null [irm_P] = tarval_P_void;
}


/*** ********************** Constructors for tarvals ********************** ***/

/* copy from src to dst len chars omitting '_'. */
static char *
stripcpy (char *dst, const char *src, size_t len)
{
  char *d = dst;

  while (len--) {
    if (*src == '_') src++;
    else *d++ = *src++;
  }
  *d = 0;			/* make it 0-terminated. */

  return dst;
}

tarval *
tarval_F_from_str (const char *s, size_t len)
{
  tarval *tv;
  char *buf;
  char *eptr;

  assert (!BUILDING);

  buf = alloca (len+1);
  stripcpy (buf, s, len);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_F;
  tv->u.F = (float)strtod (buf, &eptr);
  assert (eptr == buf+strlen(buf));

  return tarval_identify (tv);
}


tarval *
tarval_D_from_str (const char *s, size_t len)
{
  tarval *tv;
  char *buf;
  char *eptr;

  assert (!BUILDING);

  buf = alloca (len+1);
  stripcpy (buf, s, len);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_D;
  tv->u.D = strtod (buf, &eptr);
  assert (eptr == buf+strlen(buf));

  return tarval_identify (tv);
}

tarval *
tarval_int_from_str (const char *s, size_t len, int base, ir_mode *m)
{
  long val;
  char *eptr;
  char *buf;

  assert (mode_is_int(m));
  assert (!BUILDING);

  buf = alloca (len+1);
  stripcpy (buf, s, len);

  errno = 0;
  val = strtol(buf, &eptr, base);    /* strtoll */
  assert (eptr == buf+strlen(buf));
  if ((errno == ERANGE)               &&
      ((m == mode_Ls) || (m == mode_Lu))  ) {
    printf("WARNING: Constant %s not representable. Continuing with %ld.\n",
	   s, val);
  }

  return tarval_from_long(m, val);
}

/* Create a tarval with mode `m' and value `val' casted to the type that
   represents such tarvals on host.  The resulting value must be legal
   for mode `m'.  */
tarval *
tarval_from_long (ir_mode *m, long val)
{
  tarval *tv;

  assert (!BUILDING);

  if (m == mode_T) return tarval_bad;

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = m;
  switch (get_mode_modecode(m)) {
    /* floating */
  case irm_F:
    tv->u.F = val; break;
  case irm_D:
    tv->u.D = val; break;
  case irm_E:
    /* @@@ not yet implemented */
    break;
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    tv->u.uInt = val; break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    tv->u.sInt = val; break;
  case irm_P:
    assert(!val);
    obstack_free (&tv_obst, tv);
    return tarval_P_void;
  case irm_C:
    tv->u.C = val; break;
  case irm_b:
    tv->u.b = !!val;		/* u.b must be 0 or 1 */
    break;
  default:
    assert(0);
  }

  return tarval_identify (tv);
}


tarval *
tarval_P_from_str (const char *xname)
{
  tarval *tv;

  assert (!BUILDING);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = mode_P;
  tv->u.P.xname = obstack_copy0 (&tv_obst, xname, strlen (xname));
  tv->u.P.ent = NULL;
  tv->u.P.tv = NULL;
  return tarval_identify (tv);
}


tarval *
tarval_P_from_entity (entity *ent)
{
  tarval *tv;

  assert (!BUILDING);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = mode_P;
  tv->u.P.xname = NULL;
  tv->u.P.ent = ent;
  tv->u.P.tv = NULL;
  return tarval_identify (tv);
}


/* Routines for building a tarval step by step follow.
   Legal calling sequences:
     tarval_start()
     No constructors except tarval_append() and tarval_append1 ()
     tarval_finish_as() or tarval_cancel() */

/* Begin building a tarval.  */
void
tarval_start (void)
{
  assert (!BUILDING);
  obstack_blank (&tv_obst, sizeof (tarval));
}


/* Append `n' chars from `p' to the tarval currently under construction.  */
void
tarval_append (const char *p, size_t n)
{
  assert (BUILDING);
  obstack_grow (&tv_obst, p, n);
}


/* Append `ch' to the tarval currently under construction.  */
void
tarval_append1 (char ch)
{
  assert (BUILDING);
  obstack_1grow (&tv_obst, ch);
}


/* Finish the tarval currently under construction and give id mode `m'.
   `m' must be irm_Bu,
   Return NULL if the value does not make sense for this mode, this
   can only happen in mode C.  */
tarval *
tarval_finish_as (ir_mode *m)
{
  int size = obstack_object_size (&tv_obst) - sizeof (tarval);
  tarval *tv;
  unsigned char *p;
  char ch = 0;			/* initialized to shut up gcc */

  assert (BUILDING && (size >= 0));
  if (m == mode_Bu) {
    if (size != 1) return tarval_cancel();
    p = (unsigned char *)obstack_base (&tv_obst) + sizeof (tarval);
    ch = *p;
    obstack_blank (&tv_obst, -size);
  }
  tv = obstack_finish (&tv_obst);
  p = (unsigned char *)tv + sizeof (tarval);
  tv->mode = m;

  switch (get_mode_modecode(m)) {
  case irm_Bu:
    tv->u.uInt = ch;
    break;
  case irm_P:
    tv->u.P.tv = NULL;
    break;
  default:
    assert (0);
  }

  return tarval_identify (tv);
}


/* Cancel tarval building and return tarval_bad.  */
tarval *
tarval_cancel (void)
{
  assert (BUILDING);
  obstack_free (&tv_obst, obstack_finish (&tv_obst));
  return tarval_bad;
}


/*** ****************** Arithmethic operations on tarvals ***************** ***/

/* Return `src' converted to mode `m' if representable, else NULL.
   @@@ lots of conversions missing */
tarval *
tarval_convert_to (tarval *src, ir_mode *m)
{
  tarval *tv;

  if (m == src->mode) return src;

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = m;

  switch (get_mode_modecode(src->mode)) {

  case irm_D:
    if (m != mode_F) goto fail;
    tv->u.F = src->u.D;
    break;

  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    switch (get_mode_modecode(m)) {
    case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
      tv->u.sInt = src->u.sInt;
      if (sInt_overflow (tv->u.sInt, m)) goto fail;
      break;

    case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
      tv->u.uInt = src->u.sInt;
      if (uInt_overflow (tv->u.uInt, m)) goto fail;
      break;

    case irm_b:
      tv->u.b = !!src->u.sInt;
      break;

    default: goto fail;
    }

  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    switch (get_mode_modecode(m)) {
    case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
      tv->u.sInt = src->u.uInt;
      if (sInt_overflow (tv->u.sInt, m)) goto fail;
      break;

    case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
      tv->u.uInt = src->u.uInt;
      if (uInt_overflow (tv->u.uInt, m)) goto fail;
      break;

    case irm_b:
      tv->u.b = !!src->u.uInt;
      break;

    default: goto fail;
    }
    break;

  case irm_b:
    switch (get_mode_modecode(m)) {
    case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
      tv->u.sInt = src->u.b;
      break;

    case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
      tv->u.uInt = src->u.b;

    default: goto fail;
    }
    break;

  default:
  fail:
    obstack_free (&tv_obst, tv);
    return NULL;
  }

  return tarval_identify (tv);
}


/* GL Why are there no ArmRoq comments, why is this not used? */
tarval *
tarval_neg (tarval *a)
{
  tarval *tv;

  TARVAL_VRFY (a);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* floating */
  case irm_F: tv->u.F = -a->u.F; break;
  case irm_D: tv->u.D = -a->u.D; break;
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    tv->u.uInt = -a->u.uInt & tv_val_uInt (get_mode_max(a->mode));
    break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    tv->u.sInt = -a->u.sInt;
    if (   sInt_overflow (tv->u.sInt, a->mode)
	|| ((tv->u.sInt >= 0) == (a->u.sInt >= 0))) {
      obstack_free (&tv_obst, tv);
      return NULL;
    }
    break;
  case irm_b: tv->u.b = !a->u.b; break;
  default: assert(0);
  }

  return tarval_identify (tv);
}


/* Compare `a' with `b'.
   Return one of irpn_Lt, irpn_Eq, irpn_Gt, irpn_Uo, or irpn_False if
   result is unknown.  */
ir_pncmp
tarval_comp (tarval *a, tarval *b)
{

  TARVAL_VRFY (a);
  TARVAL_VRFY (b);

  assert (a->mode == b->mode);

  switch (get_mode_modecode(a->mode)) {
    /* floating */
  case irm_F: return (  a->u.F == b->u.F ? irpn_Eq
		      : a->u.F > b->u.F ? irpn_Gt
		      : a->u.F < b->u.F ? irpn_Lt
		      : irpn_Uo);
  case irm_D: return (  a->u.D == b->u.D ? irpn_Eq
		      : a->u.D > b->u.D ? irpn_Gt
		      : a->u.D < b->u.D ? irpn_Lt
		      : irpn_Uo);
  case irm_E: return (  a->u.E == b-> u.E ? irpn_Eq
              : a->u.E > b->u.E ? irpn_Gt
              : a->u.E < b->u.E ? irpn_Lt
              : irpn_Uo);
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    return (  a->u.uInt == b->u.uInt ? irpn_Eq
	    : a->u.uInt > b->u.uInt ? irpn_Gt
	    : irpn_Lt);
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    return (  a->u.sInt == b->u.sInt ? irpn_Eq
	    : a->u.sInt > b->u.sInt ? irpn_Gt
	    : irpn_Lt);
  case irm_b: return (  a->u.b == b->u.b ? irpn_Eq
		      : a->u.b > b->u.b ? irpn_Gt
		      : irpn_Lt);
  /* The following assumes that pointers are unsigned, which is valid
     for all sane CPUs (transputers are insane). */
  case irm_P: return (  a == b ? irpn_Eq
		      : a == tarval_P_void ? irpn_Lt
		      : b == tarval_P_void ? irpn_Gt
		      : irpn_False); /* unknown */
  default: assert (0);
  }
}


/* Return `a+b' if computable, else NULL.  Modes must be equal.  */
tarval *
tarval_add (tarval *a, tarval *b)
{
  tarval *tv;

  TARVAL_VRFY (a); TARVAL_VRFY (b);
  assert (a->mode == b->mode);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* floating */
  case irm_F: tv->u.F = a->u.F + b->u.F; break;	/* @@@ overflow etc */
  case irm_D: tv->u.D = a->u.D + b->u.D; break; /* @@@ dto. */
  case irm_E: tv->u.E = a->u.E + b->u.E; break; /* @@@ dto. */
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    tv->u.uInt = (a->u.uInt + b->u.uInt) & tv_val_uInt (get_mode_max(a->mode));
    break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    tv->u.sInt = a->u.sInt + b->u.sInt;
    if (   sInt_overflow (tv->u.sInt, a->mode)
	|| ((tv->u.sInt > a->u.sInt) ^ (b->u.sInt > 0))) {
      obstack_free (&tv_obst, tv);
      return NULL;
    }
    break;
  case irm_b: tv->u.b = a->u.b | b->u.b; break;	/* u.b is in canonical form */
  default: assert(0);
  }

  return tarval_identify (tv);
}


/* Return `a-b' if computable, else NULL.  Modes must be equal.  */
tarval *
tarval_sub (tarval *a, tarval *b)
{
  tarval *tv;

  TARVAL_VRFY (a); TARVAL_VRFY (b);
  assert (a->mode == b->mode);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* floating */
  case irm_F: tv->u.F = a->u.F - b->u.F; break;	/* @@@ overflow etc */
  case irm_D: tv->u.D = a->u.D - b->u.D; break; /* @@@ dto. */
  case irm_E: tv->u.E = a->u.E - b->u.E; break; /* @@@ dto. */
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    tv->u.uInt = (a->u.uInt - b->u.uInt) & tv_val_uInt (get_mode_max(a->mode));
    break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    tv->u.sInt = a->u.sInt - b->u.sInt;
    if (   sInt_overflow (tv->u.sInt, a->mode)
	|| ((tv->u.sInt > a->u.sInt) ^ (b->u.sInt < 0))) {
      obstack_free (&tv_obst, tv);
      return NULL;
    }
    break;
  case irm_b: tv->u.b = a->u.b & ~b->u.b; break; /* u.b is in canonical form */
  default: assert(0);
  }

  return tarval_identify (tv);
}

/* Return `a*b' if computable, else NULL.  Modes must be equal.  */
tarval *
tarval_mul (tarval *a, tarval *b)
{
  tarval *tv;

  TARVAL_VRFY (a); TARVAL_VRFY (b);
  assert (a->mode == b->mode);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* floating */
  case irm_F: tv->u.F = a->u.F * b->u.F; break;	/* @@@ overflow etc */
  case irm_D: tv->u.D = a->u.D * b->u.D; break; /* @@@ dto. */
  case irm_E: tv->u.E = a->u.E * b->u.E; break; /* @@@ dto. */
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    tv->u.uInt = (a->u.uInt * b->u.uInt) & tv_val_uInt (get_mode_max(a->mode));
    break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    tv->u.sInt = a->u.sInt * b->u.sInt;
    if (   sInt_overflow (tv->u.sInt, a->mode)
	|| (b->u.sInt && (tv->u.sInt / b->u.sInt != a->u.sInt))) {
      obstack_free (&tv_obst, tv);
      return NULL;
    }
    break;
  case irm_b: tv->u.b = a->u.b & b->u.b; break;	/* u.b is in canonical form */
  default: assert(0);
  }

  return tarval_identify (tv);
}


/* Return floating-point `a/b' if computable, else NULL.
   Modes must be equal, non-floating-point operands are converted to irm_D.  */
tarval *
tarval_quo (tarval *a, tarval *b)
{
  tarval *tv;

  TARVAL_VRFY (a); TARVAL_VRFY (b);
  assert (a->mode == b->mode);

  switch (get_mode_modecode(a->mode)) {
    /* floating */
  case irm_F:
    tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
    tv->mode = mode_F;
    tv->u.F = a->u.F / b->u.F;	/* @@@ overflow etc */
    break;
  case irm_D:
    tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
    tv->mode = mode_D;
    tv->u.D = a->u.D / b->u.D;	/* @@@ overflow etc */
    break;
  default:
    a = tarval_convert_to (a, mode_D);
    b = tarval_convert_to (b, mode_D);
    return a && b ? tarval_quo (a, b) : NULL;
  }

  return tarval_identify (tv);
}


/* Return `a/b' if computable, else NULL.  Modes must be equal.  */
tarval *
tarval_div (tarval *a, tarval *b)
{
  tarval *tv;

  TARVAL_VRFY (a); TARVAL_VRFY (b);
  assert (a->mode == b->mode);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* floating */
  case irm_F: tv->u.F = floor (a->u.F / b->u.F); break; /* @@@ overflow etc */
  case irm_D: tv->u.D = floor (a->u.D / b->u.D); break; /* @@@ dto. */
  case irm_E: tv->u.E = floor (a->u.E / b->u.E); break; /* @@@ dto. */
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    if (!b->u.uInt) goto fail;
    tv->u.uInt = a->u.uInt / b->u.uInt;
    break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    if (   !b->u.sInt
	|| ((b->u.sInt == -1) && (a->u.sInt == tv_val_sInt (get_mode_max(a->mode)) ))) {
    fail:
      obstack_free (&tv_obst, tv);
      return NULL;
    }
    tv->u.sInt = a->u.sInt / b->u.sInt;
    break;
  case irm_b: tv->u.b = a->u.b ^ b->u.b; break;	/* u.b is in canonical form */
  default: assert(0);
  }

  return tarval_identify (tv);
}


/* Return `a%b' if computable, else NULL.  Modes must be equal.  */
tarval *
tarval_mod (tarval *a, tarval *b)
{
  tarval *tv;

  TARVAL_VRFY (a); TARVAL_VRFY (b);
  assert (a->mode == b->mode);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* floating */
  case irm_F: tv->u.F = fmod (a->u.F, b->u.F); break; /* @@@ overflow etc */
  case irm_D: tv->u.D = fmod (a->u.D, b->u.D); break; /* @@@ dto */
  case irm_E: tv->u.E = fmod (a->u.E, b->u.E); break; /* @@@ dto. */
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    if (!b->u.uInt) goto fail;
    tv->u.uInt = a->u.uInt % b->u.uInt;
    break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    if (!b->u.sInt) {
    fail:
      obstack_free (&tv_obst, tv);
      return NULL;
    }
    tv->u.sInt = a->u.sInt % b->u.sInt;
    break;
  case irm_b: tv->u.b = a->u.b ^ b->u.b; break;	/* u.b is in canonical form */
  default: assert(0);
  }

  return tarval_identify (tv);
}

/* Return |a| if computable, else Null. */
/*  is -max == min?? */
tarval *
tarval_abs (tarval *a) {
  TARVAL_VRFY (a);
  if (tv_is_negative(a)) return tarval_neg(a);
  return a;
}

int
tv_is_negative(tarval *a) {
  TARVAL_VRFY (a);
  switch (get_mode_modecode(a->mode)) {
    /* floating */
  case irm_F: return (a->u.F<0);
  case irm_D: return (a->u.D<0);
  case irm_E: return (a->u.E<0);
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    return 0;
    break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    return (a->u.sInt < 0);
    break;
  case irm_b: break;
  default: assert(0);
  }

  return 0;
}


/* Return `a&b'.  Modes must be equal.  */
tarval *
tarval_and (tarval *a, tarval *b)
{
  tarval *tv;

  TARVAL_VRFY (a); TARVAL_VRFY (b);
  assert (a->mode == b->mode);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    tv->u.uInt = a->u.uInt & b->u.uInt; break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    tv->u.sInt = a->u.sInt & b->u.sInt; break;
  case irm_b: tv->u.b = a->u.b & b->u.b; break;	/* u.b is in canonical form */
  default: assert(0);
  }

  return tarval_identify (tv);
}


/* Return `a|b'.  Modes must be equal.  */
tarval *
tarval_or (tarval *a, tarval *b)
{
  tarval *tv;

  TARVAL_VRFY (a); TARVAL_VRFY (b);
  assert (a->mode == b->mode);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    tv->u.uInt = a->u.uInt | b->u.uInt; break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    tv->u.sInt = a->u.sInt | b->u.sInt; break;
  case irm_b: tv->u.b = a->u.b | b->u.b; break;	/* u.b is in canonical form */
  default: assert(0);
  }

  return tarval_identify (tv);
}


/* Return `a^b'.  Modes must be equal.  */
tarval *
tarval_eor (tarval *a, tarval *b)
{
  tarval *tv;

  TARVAL_VRFY (a); TARVAL_VRFY (b);
  assert (a->mode == b->mode);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    tv->u.uInt = a->u.uInt ^ b->u.uInt; break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    tv->u.sInt = a->u.sInt ^ b->u.sInt; break;
  case irm_b: tv->u.b = a->u.b ^ b->u.b; break;	/* u.b is in canonical form */
  default: assert(0);
  }

  return tarval_identify (tv);
}


/* Return `a<<b' if computable, else NULL.  */
tarval *
tarval_shl (tarval *a, tarval *b)
{
  int b_is_huge;
  long shift;
  tarval *tv;

  TARVAL_VRFY (a); TARVAL_VRFY (b);

  shift = tarval_ord (b, &b_is_huge);
  if (   b_is_huge
      || (shift < 0)
      || ((shift >= get_mode_size(mode_Ls)*target_bits))) {
    return NULL;
  }

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    tv->u.uInt = a->u.uInt << shift;
    break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    tv->u.sInt = a->u.sInt << shift;
    break;
  default: assert (0);
  }

  return tarval_identify (tv);
}


/* Return `a>>b' if computable, else NULL.
   The interpretation of >> (sign extended or not) is implementaion
   dependent, i.e. this is neither shr nor shrs!! */
tarval *
tarval_shr (tarval *a, tarval *b)
{
  int b_is_huge;
  long shift;
  tarval *tv;

  TARVAL_VRFY (a); TARVAL_VRFY (b);

  shift = tarval_ord (b, &b_is_huge);
  if (   b_is_huge
      || (shift < 0)
      || ((shift >= get_mode_size(mode_Ls)*target_bits))) {
    return NULL;
  }

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    tv->u.uInt = a->u.uInt >> shift;
    break;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    tv->u.sInt = a->u.sInt >> shift;
    break;
  default: assert (0);
  }

  return tarval_identify (tv);
}


/* Classify `tv', which may be NULL.
   Return 0 if `tv' is the additive neutral element, 1 if `tv' is the
   multiplicative neutral element, and -1 if `tv' is the neutral
   element of bitwise and.  */
long
tarval_classify (tarval *tv)
{
  if (!tv) return 2;

  TARVAL_VRFY (tv);

  switch (get_mode_modecode(tv->mode)) {
    /* floating */
  case irm_F: case irm_D: case irm_E:
    return 2;
    /* unsigned */
  case irm_Bu:
    return (long)((tv->u.uInt+1) & tv_val_uInt (get_mode_max(mode_Bu))) - 1;
  case irm_Hu:
    return (long)((tv->u.uInt+1) & tv_val_uInt (get_mode_max(mode_Hu))) - 1;
  case irm_Iu:
    return (long)((tv->u.uInt+1) & tv_val_uInt (get_mode_max(mode_Iu))) - 1;
  case irm_Lu:
    return (long)((tv->u.uInt+1) & tv_val_uInt (get_mode_max(mode_Lu))) - 1;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    return tv->u.sInt;
  case irm_b:
    return tv->u.b;
  default:
    return 2;
  }
}

/* Convert `tv' into type `long', set `fail' if not representable.
   If `fail' gets set for an unsigned `tv', the correct result can be
   obtained by casting the result to `unsigned long'.  */
long
tarval_ord (tarval *tv, int *fail)
{
  TARVAL_VRFY (tv);

  switch (get_mode_modecode(tv->mode)) {
    /* unsigned */
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu:
    *fail = tv->u.uInt > tv_val_uInt (get_mode_max(mode_Ls));
    return tv->u.uInt;
    /* signed */
  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls:
    *fail = 0;
    return tv->u.sInt;
  case irm_b:
    *fail = 0;
    return tv->u.b;
  default: ;
    *fail = 1;
    return 0;
  }
}

int
tarval_print (XP_PAR1, const xprintf_info *info ATTRIBUTE((unused)), XP_PARN)
{
  tarval *val = XP_GETARG (tarval *, 0);
  int printed;
  char buf[40];

  TARVAL_VRFY (val);

  switch (get_mode_modecode(val->mode)) {

  case irm_T:			/* none */
    printed = XPSR ("<bad>");
    break;

  case irm_F:			/* float */
    sprintf (buf, "%1.9e", (float)(val->u.F));
    printed = XPF1R ("%s", buf);
    break;
  case irm_D:			/* double */
    printed = XPF1R ("%1.30g", (double)(val->u.D));
    break;

  case irm_C:           /* character */
    if ((isprint (val->u.C)) &&
	(val->u.C != '\\')   && (val->u.C != '\'')) {
      printed = XPF1R ("'%c'", val->u.C);
    } else {
      printed = XPF1R ("0x%x", (unsigned long)val->u.C);
    }
    break;

  case irm_Bs: case irm_Hs: case irm_Is: case irm_Ls: /* signed num */
    printed = XPF1R ("%ld", (long)val->u.sInt);
    break;
  case irm_Bu: case irm_Hu: case irm_Iu: case irm_Lu: /* unsigned num */
    printed = XPF1R ("%lu", (unsigned long)val->u.uInt);
    break;

  case irm_P:			/* pointer */
    if (val->u.P.xname) {
      printed = XPR (val->u.P.xname);
    } else if (val->u.P.ent) {
      if (get_entity_peculiarity(val->u.P.ent) == existent)
	printed = XPF1R ("&(%I)", get_entity_ld_ident(val->u.P.ent));
      else
	printed = XPSR ("(NULL)");
    } else {
      assert (val == tarval_P_void);
      printed = XPSR ("(void)");
    }
    break;

  case irm_b:			/* boolean */
    if (val->u.b) printed = XPSR ("true");
    else	  printed = XPSR ("false");
    break;

  case irm_M:			/* memory */
  case irm_BB:			/* region */
  default:
    assert (0);
  }

  return printed;
}


ir_mode *
get_tv_mode (tarval *tv)
{
  return tv->mode;
}

/* Returns the entity if the tv is a pointer to an entity, else
   returns NULL; */
entity *get_tv_entity(tarval *tv) {
  entity *ent = NULL;

  if (tv->mode == mode_P) {
    if (tv->u.P.xname) {
      assert(0);
      /* not an entity */
    } else if (tv->u.P.ent) {
      ent = tv->u.P.ent;
    } else {
      /* not an entity */
    }
  }
  return ent;
}
