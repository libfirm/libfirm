/* TV --- Target Values, aka Constant Table.
   Copyright (C) 1995, 1996 Christian von Roques */

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

/* special tarvals: */
tarval *tarval_bad;
tarval *tarval_b_false;
tarval *tarval_b_true;
tarval *tarval_d_NaN;
tarval *tarval_d_Inf;
tarval *tarval_p_void;
tarval *tarval_mode_null[irm_max];

# if 0
/* @@@ depends on order of ir_mode */
static tarval_chil min_chil[8] = {
  TARGET_SIMIN (c), 0,
  TARGET_SIMIN (h), 0,
  TARGET_SIMIN (i), 0,
  TARGET_SIMIN (l), 0
};
static tarval_chil max_chil[8] = {
  TARGET_SIMAX (c), TARGET_UIMAX (C),
  TARGET_SIMAX (h), TARGET_UIMAX (H),
  TARGET_SIMAX (i), TARGET_UIMAX (I),
  TARGET_SIMAX (l), TARGET_UIMAX (L)
};
# endif

/* return a mode-specific value */

tarval_f
tv_val_f (tarval *tv)
{
  return tv->u.f;
}

tarval_d
tv_val_d (tarval *tv)
{
  return tv->u.d;
}

tarval_chil
tv_val_chil (tarval *tv)
{
  return tv->u.chil;
}

tarval_CHIL
tv_val_CHIL (tarval *tv)
{
  return tv->u.CHIL;
}

tarval_Z
tv_val_Z (tarval *tv)
{
  return tv->u.Z;
}

tarval_p
tv_val_p (tarval *tv)
{
  return tv->u.p;
}

bool
tv_val_b (tarval *tv)
{
  return tv->u.b;
}

tarval_B
tv_val_B (tarval *tv)
{
  return tv->u.B;
}

tarval_s
tv_val_s (tarval *tv)
{
  return tv->u.s;
}


/* Overflows `chil' signed integral `mode'?  */
static inline bool
chil_overflow (tarval_chil chil, ir_mode *mode)
{
  assert (is_chilCHIL(get_mode_modecode(mode)));
  return (get_mode_min(mode) && get_mode_max(mode)  /* only valid after firm initialization */
	  && (chil < tv_val_chil (get_mode_min(mode))
	      || tv_val_chil (get_mode_max(mode)) < chil));
}


/* Overflows `CHIL' unsigned integral `mode'?  */
static inline bool
CHIL_overflow (tarval_CHIL CHIL, ir_mode *mode)
{
  assert (is_chilCHIL(get_mode_modecode(mode)));
  return (get_mode_max(mode)   /* only valid after firm initialization */
	  && tv_val_CHIL (get_mode_max(mode)) < CHIL);
}


#ifndef NDEBUG
void
_tarval_vrfy (const tarval *val)
{
  assert (val);
  switch (get_mode_modecode(val->mode)) {
    /* floating */
  case irm_f:
  case irm_d:
    break;
    /* integral */
  case irm_C: case irm_H: case irm_I: case irm_L:
    assert (!CHIL_overflow (val->u.CHIL, val->mode)); break;
  case irm_c: case irm_h: case irm_i: case irm_l:
    assert (!chil_overflow (val->u.chil, val->mode)); break;
  case irm_Z:
    break;
    /* strange */
  case irm_p:
    if (val->u.p.ent)
      assert (val->u.p.ent->kind == k_entity);
    assert (   val->u.p.xname || val->u.p.ent
	    || !tarval_p_void || (val == tarval_p_void));
    break;
  case irm_s:
  case irm_S:
    assert (val->u.s.p); break;
  case irm_B:
    assert (val->u.B.p); break;
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
  case irm_f:
    return memcmp (&a->u.f, &b->u.f, sizeof (a->u.f));
  case irm_d:
    return memcmp (&a->u.d, &b->u.d, sizeof (a->u.d));
    /* unsigned */
  case irm_C: case irm_H: case irm_I: case irm_L:
    if (sizeof (int) == sizeof (tarval_CHIL)) {
      return a->u.CHIL - b->u.CHIL;
    }
    return a->u.CHIL != b->u.CHIL;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    if (sizeof (int) == sizeof (tarval_chil)) {
      return a->u.chil - b->u.chil;
    }
    return a->u.chil != b->u.chil;
  case irm_Z:
    return mpz_cmp (&a->u.Z, &b->u.Z);
    /* strange */
  case irm_p:
    if (a->u.p.ent || b->u.p.ent)
      return (char *)a->u.p.ent - (char *)b->u.p.ent;
    if (a->u.p.xname && b->u.p.xname)
      return strcmp (a->u.p.xname, b->u.p.xname);
    return a->u.p.xname - b->u.p.xname;
  case irm_b:
    return a->u.b - b->u.b;
  case irm_B:
    return (  a->u.B.n - b->u.B.n
	    ? memcmp (a->u.B.p, b->u.B.p, a->u.B.n)
	    : a->u.B.n - b->u.B.n);
  case irm_s: case irm_S:
    return (  a->u.s.n == b->u.s.n
	    ? memcmp (a->u.s.p, b->u.s.p, a->u.s.n)
	    : a->u.s.n - b->u.s.n);
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
  case irm_f:
    /* quick & dirty */
    { union { float f; unsigned u; } u;
      assert (sizeof (float) <= sizeof (unsigned));
      u.u = 0; u.f = tv->u.f;
      h ^= u.u;
      break;
    }
  case irm_d:
    /* quick & dirty */
    { union { double d; unsigned u[2]; } u;
      assert (sizeof (double) <= 2*sizeof (unsigned));
      u.u[0] = u.u[1] = 0; u.d = tv->u.d;
      h ^= u.u[0] ^ u.u[1];
      break;
    }
  case irm_C: case irm_H: case irm_I: case irm_L:
    h ^= tv->u.CHIL; break;
  case irm_c: case irm_h: case irm_i: case irm_l:
    h ^= tv->u.chil; break;
  case irm_Z:
    h ^= mpz_get_ui (&tv->u.Z); break;
  case irm_p:
    if (tv->u.p.ent) {
      /* @@@ lower bits not random, watch for collisions; perhaps
	 replace by tv->u.p.ent - (entity *)0 */
      h ^= ((char *)tv->u.p.ent - (char *)0) / 64;
    } else if (tv->u.p.xname) {
      /* Of course, strlen() in a hash function is a mistake, but this
         case should be really rare.  */
      h ^= ID_HASH (tv->u.p.xname, strlen (tv->u.p.xname));
    } else {			/* void */
      h^= 0x2b592b88;
    }
    break;
  case irm_b:
    h ^= tv->u.b; break;
  case irm_B:
    h ^= tv->u.B.n; break; /* @@@ not really good */
  case irm_s:
    h ^= tv->u.s.p[0]<<12 ^ tv->u.s.p[tv->u.s.n]<<4 ^ tv->u.s.n; break;
  case irm_S:
    h ^= tv->u.s.p[0]<<4 ^ tv->u.s.p[tv->u.s.n]<<12 ^ tv->u.s.n; break;
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
  /* assumed by tarval_chil & friends: */
  assert (   (irm_C == irm_c+1) && (irm_h == irm_C+1)
	  && (irm_H == irm_h+1) && (irm_i == irm_H+1)
	  && (irm_I == irm_i+1) && (irm_l == irm_I+1)
	  && (irm_L == irm_l+1));

  /* assumed everywhere: */
  for (i = 0;  i <= irm_L-irm_c;  i += 2) {
    assert (   IS_POW2 (max_chil[i+1]+1)
	    && (min_chil[i] == -max_chil[i]-1)
	    && ((tarval_CHIL)max_chil[i+1] == (tarval_CHIL)max_chil[i]-min_chil[i]));
  }
# endif


  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_T;
  tarval_bad = tarval_identify (tv);

  tarval_b_false = tarval_from_long (mode_b, 0);
  tarval_b_true = tarval_from_long (mode_b, 1);

  /* IsInf <-> exponent == 0x7ff && ! (bits | fraction_low) */
  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_d;
  x.ieee.negative = 0;
  x.ieee.exponent = 0x7ff;
  x.ieee.mantissa0 = 0;
  x.ieee.mantissa1 = 0;
  tv->u.d = x.d;
  tarval_d_Inf = tarval_identify (tv);

  /* IsNaN <-> exponent==0x7ff  && (qnan_bit | bits | fraction_low) */
  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_d;
  x.ieee_nan.negative = 0;
  x.ieee_nan.exponent = 0x7ff;
  x.ieee_nan.quiet_nan = 1;	/* @@@ quiet or signalling? */
  x.ieee_nan.mantissa0 = 42;
  x.ieee_nan.mantissa1 = 0;
  assert(x.d != x.d /* x.d is NaN */);
  tv->u.d = x.d;
  tarval_d_NaN = tarval_identify (tv);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_p;
  tv->u.p.xname = NULL;
  tv->u.p.ent = NULL;
  tv->u.p.tv = NULL;
  tarval_p_void = tarval_identify (tv);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));


  tarval_mode_null [irm_f] = tarval_from_long (mode_f, 0);
  tarval_mode_null [irm_d] = tarval_from_long (mode_d, 0);
  tarval_mode_null [irm_c] = tarval_from_long (mode_c, 0);
  tarval_mode_null [irm_C] = tarval_from_long (mode_C, 0);
  tarval_mode_null [irm_h] = tarval_from_long (mode_h, 0);
  tarval_mode_null [irm_H] = tarval_from_long (mode_H, 0);
  tarval_mode_null [irm_i] = tarval_from_long (mode_i, 0);
  tarval_mode_null [irm_I] = tarval_from_long (mode_I, 0);
  tarval_mode_null [irm_l] = tarval_from_long (mode_l, 0);
  tarval_mode_null [irm_L] = tarval_from_long (mode_L, 0);
  tarval_mode_null [irm_b] = tarval_b_false;
  tarval_mode_null [irm_p] = tarval_p_void;
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
tarval_Z_from_str (const char *s, size_t len, int base)
{
  tarval *tv;
  char *buf;

  assert (!BUILDING);

  buf = alloca (len+1);
  stripcpy (buf, s, len);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_Z;
  if (mpz_init_set_str (&tv->u.Z, buf, base)) assert (0);

  return tarval_identify (tv);
}


tarval *
tarval_B_from_str (const char *s, size_t len)
{
  tarval *tv;
  size_t n;			/* size of B */
  const char *r;		/* read ptr */
  unsigned x;			/* bit store */
  int b;			/* bits in x */
  int shift;			/* base shift */

  assert (!BUILDING);
  assert (len >= 3);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_B;

  assert (s[0] == '0');
  switch (s[1]) {
  case 'o':
  case 'O': shift = 3; break;
  case 'x':
  case 'X': shift = 4; break;
  default: assert(0);
  }

  r = s+len;			/* set r past input */
  s += 2;			/* skip header */
  x = 0; b = 0; n = 0;
  while (--r >= s) {
    int d;			/* digit */

    if (*r == '_') continue;	/* skip _ styropor */
    if (('0' <= *r) && (*r <= '9')) {
      d = *r - '0';
    } else if (('a' <= *r) && (*r <= 'f')) {
      d = *r - 'a' + 10;
    } else { assert (('A' <= *r) && (*r <= 'F'));
      d = *r - 'A' + 10;
    }

    x |= d << b;		/* insert d into x above the b present bits */
    b += shift;			/* x now contains shift more bits */

    if (b >= 8) {		/* we've accumulated at least a byte */
      char c = x & 0xFF;	/* extract the lower 8 bits from x */
      obstack_grow (&tv_obst, &c, 1); /* and stuff them into B */
      x >>= 8;			/* remove the lower 8 bits from x */
      b -= 8;			/* x now contains 8 bits fewer */
      ++n;			/* B grew a byte */
    }
  }

  if (b >= 0) {			/* flush the rest of the bits */
    char c = x;			/* extract them */
    obstack_grow (&tv_obst, &c, 1); /* and stuff them into B */
    ++n;			/* B grew a byte */
  }

  { unsigned char *p = obstack_finish (&tv_obst);
    unsigned char *q = p + n;

    tv->u.B.p = p;
    tv->u.B.n = n;
    /* reverse p in place */
    while (p < q) { char c = *p; *p++ = *q; *q-- = c; }
  }

  return tarval_identify (tv);
}


tarval *
tarval_d_from_str (const char *s, size_t len)
{
  tarval *tv;
  char *buf;
  char *eptr;

  assert (!BUILDING);

  buf = alloca (len+1);
  stripcpy (buf, s, len);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = mode_d;
  tv->u.d = strtod (buf, &eptr);
  assert (eptr == buf+strlen(buf));

  return tarval_identify (tv);
}


tarval *
tarval_s_from_str (const char *s, size_t len)
{
  tarval *tv;

  assert (!BUILDING);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = mode_s;
  tv->u.s.n = len;
  tv->u.s.p = obstack_copy (&tv_obst, s, len);

  return tarval_identify (tv);
}

tarval *
tarval_S_from_str (const char *s, size_t len)
{
  tarval *tv;

  assert (!BUILDING);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = mode_S;
  tv->u.s.n = len;
  tv->u.s.p = obstack_copy (&tv_obst, s, len);

  return tarval_identify (tv);
}


/* Create a tarval with mode `m' and value `i' casted to the type that
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
  case irm_f:
    tv->u.f = val; break;
  case irm_d:
    tv->u.d = val; break;
    /* unsigned */
  case irm_C: case irm_H: case irm_I: case irm_L:
    tv->u.CHIL = val; break;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    tv->u.chil = val; break;
  case irm_Z:
    mpz_init_set_si (&tv->u.Z, val);
    break;
    /* strange */
  case irm_p:
    assert(!val);
    obstack_free (&tv_obst, tv);
    return tarval_p_void;
  case irm_b:
    tv->u.b = !!val;		/* u.b must be 0 or 1 */
    break;
  default:
    assert(0);
  }

  return tarval_identify (tv);
}


tarval *
tarval_p_from_str (const char *xname)
{
  tarval *tv;

  assert (!BUILDING);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = mode_p;
  tv->u.p.xname = obstack_copy0 (&tv_obst, xname, strlen (xname));
  tv->u.p.ent = NULL;
  tv->u.p.tv = NULL;
  return tarval_identify (tv);
}


tarval *
tarval_p_from_entity (entity *ent)
{
  tarval *tv;

  assert (!BUILDING);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = mode_p;
  tv->u.p.xname = NULL;
  tv->u.p.ent = ent;
  tv->u.p.tv = NULL;
  return tarval_identify (tv);
}


/* Routines for building a tarval step by step follow.
   Legal calling sequences:
     tarval_start()
     No contructors except tarval_append() and tarval_append1 ()
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
   `m' must be irm_C, irm_B, irm_s or irm_S.
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
  if (m == mode_C) {
    if (size != 1) return tarval_cancel();
    p = (unsigned char *)obstack_base (&tv_obst) + sizeof (tarval);
    ch = *p;
    obstack_blank (&tv_obst, -size);
  }
  tv = obstack_finish (&tv_obst);
  p = (unsigned char *)tv + sizeof (tarval);
  tv->mode = m;

  switch (get_mode_modecode(m)) {
  case irm_C:
    tv->u.CHIL = ch;
    break;
  case irm_B:
    tv->u.B.n = size;
    tv->u.B.p = p;
    break;
  case irm_s:
  case irm_S:
    tv->u.s.n = size;
    tv->u.s.p = p;
    break;
  case irm_p:
    tv->u.p.tv = NULL;
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

  case irm_d:
    if (m != mode_f) goto fail;
    tv->u.f = src->u.d;
    break;

  case irm_Z:
    switch (get_mode_modecode(m)) {

    case irm_C: case irm_H: case irm_I: case irm_L:
      if (mpz_cmp_si (&src->u.Z, 0) < 0) goto fail;
      if (mpz_size (&src->u.Z) > 1) goto fail;
      tv->u.CHIL = mpz_get_ui (&src->u.Z);
      if (CHIL_overflow (tv->u.CHIL, m)) goto fail;
      break;

    case irm_c: case irm_h: case irm_i: case irm_l:
      tv->u.chil = mpz_get_si (&src->u.Z);
      if (chil_overflow (tv->u.chil, m)) goto fail;
      break;

    case irm_b:
      tv ->u.b = !mpz_cmp_ui (&src->u.Z, 0);
      break;

    case irm_p:
      if (mpz_cmp_ui (&src->u.Z, 0)) goto fail;
      obstack_free (&tv_obst, tv);
      return tarval_p_void;

    default: goto fail;
    }
    break;

  case irm_c: case irm_h: case irm_i: case irm_l:
    switch (get_mode_modecode(m)) {
    case irm_c: case irm_h: case irm_i: case irm_l:
      tv->u.chil = src->u.chil;
      if (chil_overflow (tv->u.chil, m)) goto fail;
      break;

    case irm_C: case irm_H: case irm_I: case irm_L:
      tv->u.CHIL = src->u.chil;
      if (CHIL_overflow (tv->u.CHIL, m)) goto fail;
      break;

    case irm_Z:
      mpz_init_set_si (&tv->u.Z, src->u.chil);
      break;

    case irm_b:
      tv->u.b = !!src->u.chil;
      break;

    default: goto fail;
    }

  case irm_C: case irm_H: case irm_I: case irm_L:
    switch (get_mode_modecode(m)) {
    case irm_c: case irm_h: case irm_i: case irm_l:
      tv->u.chil = src->u.CHIL;
      if (chil_overflow (tv->u.chil, m)) goto fail;
      break;

    case irm_C: case irm_H: case irm_I: case irm_L:
      tv->u.CHIL = src->u.CHIL;
      if (CHIL_overflow (tv->u.CHIL, m)) goto fail;
      break;

    case irm_Z:
      mpz_init_set_ui (&tv->u.Z, src->u.CHIL);
      break;

    case irm_b:
      tv->u.b = !!src->u.CHIL;
      break;

    default: goto fail;
    }
    break;

  case irm_b:
    switch (get_mode_modecode(m)) {
    case irm_c: case irm_h: case irm_i: case irm_l:
      tv->u.chil = src->u.b;
      break;

    case irm_C: case irm_H: case irm_I: case irm_L:
      tv->u.CHIL = src->u.b;

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


tarval *
tarval_neg (tarval *a)
{
  tarval *tv;

  TARVAL_VRFY (a);

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* floating */
  case irm_f: tv->u.f = -a->u.f; break;
  case irm_d: tv->u.d = -a->u.d; break;
    /* unsigned */
  case irm_C: case irm_H: case irm_I: case irm_L:
    tv->u.CHIL = -a->u.CHIL & tv_val_CHIL (get_mode_max(a->mode));
    break;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    tv->u.chil = -a->u.chil;
    if (   chil_overflow (tv->u.chil, a->mode)
	|| ((tv->u.chil >= 0) == (a->u.chil >= 0))) {
      obstack_free (&tv_obst, tv);
      return NULL;
    }
    break;
  case irm_Z:
    mpz_init (&tv->u.Z);
    mpz_neg (&tv->u.Z, &a->u.Z);
    break;
    /* strange */
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
  case irm_f: return (  a->u.f == b->u.f ? irpn_Eq
		      : a->u.f > b->u.f ? irpn_Gt
		      : a->u.f < b->u.f ? irpn_Lt
		      : irpn_Uo);
  case irm_d: return (  a->u.d == b->u.d ? irpn_Eq
		      : a->u.d > b->u.d ? irpn_Gt
		      : a->u.d < b->u.d ? irpn_Lt
		      : irpn_Uo);
    /* unsigned */
  case irm_C: case irm_H: case irm_I: case irm_L:
    return (  a->u.CHIL == b->u.CHIL ? irpn_Eq
	    : a->u.CHIL > b->u.CHIL ? irpn_Gt
	    : irpn_Lt);
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    return (  a->u.chil == b->u.chil ? irpn_Eq
	    : a->u.chil > b->u.chil ? irpn_Gt
	    : irpn_Lt);
  case irm_Z:
    { int cmp = mpz_cmp (&a->u.Z, &b->u.Z);
      return (  cmp == 0 ? irpn_Eq
	      : cmp > 0 ? irpn_Gt
	      : irpn_Lt);
    }
    /* strange */
  case irm_b: return (  a->u.b == b->u.b ? irpn_Eq
		      : a->u.b > b->u.b ? irpn_Gt
		      : irpn_Lt);
  /* The following assumes that pointers are unsigned, which is valid
     for all sane CPUs (transputers are insane). */
  case irm_p: return (  a == b ? irpn_Eq
		      : a == tarval_p_void ? irpn_Lt
		      : b == tarval_p_void ? irpn_Gt
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
  case irm_f: tv->u.f = a->u.f + b->u.f; break;	/* @@@ overflow etc */
  case irm_d: tv->u.d = a->u.d + b->u.d; break; /* @@@ dto. */
    /* unsigned */
  case irm_C: case irm_H: case irm_I: case irm_L:
    tv->u.CHIL = (a->u.CHIL + b->u.CHIL) & tv_val_CHIL (get_mode_max(a->mode));
    break;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    tv->u.chil = a->u.chil + b->u.chil;
    if (   chil_overflow (tv->u.chil, a->mode)
	|| ((tv->u.chil > a->u.chil) ^ (b->u.chil > 0))) {
      obstack_free (&tv_obst, tv);
      return NULL;
    }
    break;
  case irm_Z:
    mpz_init (&tv->u.Z);
    mpz_add (&tv->u.Z, &a->u.Z, &b->u.Z);
    break;
    /* strange */
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
  case irm_f: tv->u.f = a->u.f - b->u.f; break;	/* @@@ overflow etc */
  case irm_d: tv->u.d = a->u.d - b->u.d; break; /* @@@ dto. */
    /* unsigned */
  case irm_C: case irm_H: case irm_I: case irm_L:
    tv->u.CHIL = (a->u.CHIL - b->u.CHIL) & tv_val_CHIL (get_mode_max(a->mode));
    break;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    tv->u.chil = a->u.chil - b->u.chil;
    if (   chil_overflow (tv->u.chil, a->mode)
	|| ((tv->u.chil > a->u.chil) ^ (b->u.chil < 0))) {
      obstack_free (&tv_obst, tv);
      return NULL;
    }
    break;
  case irm_Z:
    mpz_init (&tv->u.Z);
    mpz_sub (&tv->u.Z, &a->u.Z, &b->u.Z);
    break;
    /* strange */
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
  case irm_f: tv->u.f = a->u.f * b->u.f; break;	/* @@@ overflow etc */
  case irm_d: tv->u.d = a->u.d * b->u.d; break; /* @@@ dto. */
    /* unsigned */
  case irm_C: case irm_H: case irm_I: case irm_L:
    tv->u.CHIL = (a->u.CHIL * b->u.CHIL) & tv_val_CHIL (get_mode_max(a->mode));
    break;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    tv->u.chil = a->u.chil * b->u.chil;
    if (   chil_overflow (tv->u.chil, a->mode)
	|| (b->u.chil && (tv->u.chil / b->u.chil != a->u.chil))) {
      obstack_free (&tv_obst, tv);
      return NULL;
    }
    break;
  case irm_Z:
    mpz_init (&tv->u.Z);
    mpz_mul (&tv->u.Z, &a->u.Z, &b->u.Z);
    break;
    /* strange */
  case irm_b: tv->u.b = a->u.b & b->u.b; break;	/* u.b is in canonical form */
  default: assert(0);
  }

  return tarval_identify (tv);
}


/* Return floating-point `a/b' if computable, else NULL.
   Modes must be equal, non-floating-point operands are converted to irm_d.  */
tarval *
tarval_quo (tarval *a, tarval *b)
{
  tarval *tv;

  TARVAL_VRFY (a); TARVAL_VRFY (b);
  assert (a->mode == b->mode);

  switch (get_mode_modecode(a->mode)) {
    /* floating */
  case irm_f:
    tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
    tv->mode = mode_f;
    tv->u.f = a->u.f / b->u.f;	/* @@@ overflow etc */
    break;
  case irm_d:
    tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
    tv->mode = mode_d;
    tv->u.d = a->u.d / b->u.d;	/* @@@ overflow etc */
    break;
  default:
    a = tarval_convert_to (a, mode_d);
    b = tarval_convert_to (b, mode_d);
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
  case irm_f: tv->u.f = floor (a->u.f / b->u.f); break; /* @@@ overflow etc */
  case irm_d: tv->u.d = floor (a->u.d / b->u.d); break; /* @@@ dto. */
    /* unsigned */
  case irm_C: case irm_H: case irm_I: case irm_L:
    if (!b->u.CHIL) goto fail;
    tv->u.CHIL = a->u.CHIL / b->u.CHIL;
    break;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    if (   !b->u.chil
	|| ((b->u.chil == -1) && (a->u.chil == tv_val_chil (get_mode_max(a->mode)) ))) {
    fail:
      obstack_free (&tv_obst, tv);
      return NULL;
    }
    tv->u.chil = a->u.chil / b->u.chil;
    break;
  case irm_Z:
    if (!mpz_cmp_ui (&b->u.Z, 0)) goto fail;
    mpz_init (&tv->u.Z);
    mpz_div (&tv->u.Z, &a->u.Z, &b->u.Z);
    break;
    /* strange */
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
  case irm_f: tv->u.f = fmod (a->u.f, b->u.f); break; /* @@@ overflow etc */
  case irm_d: tv->u.d = fmod (a->u.d, b->u.d); break; /* @@@ dto */
    /* unsigned */
  case irm_C: case irm_H: case irm_I: case irm_L:
    if (!b->u.CHIL) goto fail;
    tv->u.CHIL = a->u.CHIL % b->u.CHIL;
    break;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    if (!b->u.chil) {
    fail:
      obstack_free (&tv_obst, tv);
      return NULL;
    }
    tv->u.chil = a->u.chil % b->u.chil;
    break;
  case irm_Z:
    if (!mpz_cmp_ui (&b->u.Z, 0)) goto fail;
    mpz_init (&tv->u.Z);
    mpz_mod (&tv->u.Z, &a->u.Z, &b->u.Z);
    break;
    /* strange */
  case irm_b: tv->u.b = a->u.b ^ b->u.b; break;	/* u.b is in canonical form */
  default: assert(0);
  }

  return tarval_identify (tv);
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
  case irm_C: case irm_H: case irm_I: case irm_L:
    tv->u.CHIL = a->u.CHIL & b->u.CHIL; break;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    tv->u.chil = a->u.chil & b->u.chil; break;
  case irm_Z:
    mpz_init (&tv->u.Z);
    mpz_and (&tv->u.Z, &a->u.Z, &b->u.Z);
    break;
    /* strange */
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
  case irm_C: case irm_H: case irm_I: case irm_L:
    tv->u.CHIL = a->u.CHIL | b->u.CHIL; break;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    tv->u.chil = a->u.chil | b->u.chil; break;
  case irm_Z:
    mpz_init (&tv->u.Z);
    mpz_ior (&tv->u.Z, &a->u.Z, &b->u.Z);
    break;
    /* strange */
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

#if 1 /* see case irm_Z below */
  if (a->mode == mode_Z) return NULL;
#endif

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));

  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* unsigned */
  case irm_C: case irm_H: case irm_I: case irm_L:
    tv->u.CHIL = a->u.CHIL ^ b->u.CHIL; break;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    tv->u.chil = a->u.chil ^ b->u.chil; break;
  case irm_Z:
#if 0 /* gmp-1.3.2 declares but does not define mpz_xor() */
    mpz_init (&tv->u.Z);
    mpz_xor (&tv->u.Z, &a->u.Z, &b->u.Z);
#endif
    break;
    /* strange */
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
      || ((shift >= get_mode_size(mode_l)*target_bits) && (a->mode != mode_Z))) {
    return NULL;
  }

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* unsigned */
  case irm_C: case irm_H: case irm_I: case irm_L:
    tv->u.CHIL = a->u.CHIL << shift;
    break;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    tv->u.chil = a->u.chil << shift;
    break;
  case irm_Z:
    mpz_init (&tv->u.Z);
    mpz_mul_2exp (&tv->u.Z, &a->u.Z, shift);
    break;
  default: assert (0);
  }

  return tarval_identify (tv);
}


/* Return `a>>b' if computable, else NULL.  */
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
      || ((shift >= get_mode_size(mode_l)*target_bits) && (a->mode != mode_Z))) {
    return NULL;
  }

  tv = (tarval *)obstack_alloc (&tv_obst, sizeof (tarval));
  tv->mode = a->mode;

  switch (get_mode_modecode(a->mode)) {
    /* unsigned */
  case irm_C: case irm_H: case irm_I: case irm_L:
    tv->u.CHIL = a->u.CHIL >> shift;
    break;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    tv->u.chil = a->u.chil >> shift;
    break;
  case irm_Z:
    mpz_init (&tv->u.Z);
    mpz_div_2exp (&tv->u.Z, &a->u.Z, shift);
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
  case irm_f: case irm_d:
    return 2;
    /* unsigned */
  case irm_C:
    return (long)((tv->u.CHIL+1) & tv_val_CHIL (get_mode_max(mode_C))) - 1;
  case irm_H:
    return (long)((tv->u.CHIL+1) & tv_val_CHIL (get_mode_max(mode_H))) - 1;
  case irm_I:
    return (long)((tv->u.CHIL+1) & tv_val_CHIL (get_mode_max(mode_I))) - 1;
  case irm_L:
    return (long)((tv->u.CHIL+1) & tv_val_CHIL (get_mode_max(mode_L))) - 1;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    return tv->u.chil;
  case irm_Z:
    if      (mpz_cmp_si (&tv->u.Z, 0)) return 0;
    else if (mpz_cmp_si (&tv->u.Z, 1)) return 1;
    else if (mpz_cmp_si (&tv->u.Z,-1)) return -1;
    return 2;
    /* strange */
  case irm_b:
    return tv->u.b;
  default:
    return 2;
  }
}


bool
tarval_s_fits (tarval *tv, long min, long max) {
  return ((  mpz_cmp_si (&tv->u.Z, min) >= 0)
	  && mpz_cmp_si (&tv->u.Z, max) <= 0);
}

bool
tarval_u_fits (tarval *tv, unsigned long max) {
  return ((  mpz_sgn (&tv->u.Z) >= 0)
	  && mpz_cmp_si (&tv->u.Z, max) <= 0);
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
  case irm_C: case irm_H: case irm_I: case irm_L:
    *fail = tv->u.CHIL > tv_val_CHIL (get_mode_max(mode_l));
    return tv->u.CHIL;
    /* signed */
  case irm_c: case irm_h: case irm_i: case irm_l:
    *fail = 0;
    return tv->u.chil;
  case irm_Z:
    *fail = (   (mpz_cmp_si (&tv->u.Z, tv_val_chil(get_mode_max(mode_l))) > 0)
	     || (mpz_cmp_si (&tv->u.Z, tv_val_chil(get_mode_max(mode_l))) < 0));
    return mpz_get_si (&tv->u.Z);
    /* strange */
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

  TARVAL_VRFY (val);

  switch (get_mode_modecode(val->mode)) {

  case irm_T:			/* none */
    printed = XPSR ("<bad>");
    break;

  case irm_f:			/* float */
    printed = XPF1R ("%g", (double)(val->u.f));
    break;
  case irm_d:			/* double */
    printed = XPF1R ("%g", (double)(val->u.d));
    break;

  case irm_c:			/* signed char */
  case irm_C:			/* unsigned char */
    if (isprint (val->u.chil)) {
      printed = XPF1R ("'%c'", val->u.chil);
    } else {
      printed = XPF1R ("'\\%03o'", val->u.chil);
    }
    break;

  case irm_h: case irm_i: case irm_l: /* signed num */
    printed = XPF1R ("%ld", (long)val->u.chil);
    break;
  case irm_H: case irm_I: case irm_L: /* unsigned num */
    printed = XPF1R ("%lu", (unsigned long)val->u.CHIL);
    break;

  case irm_Z:			/* mp int */
    printed = XPF1R ("%Z", &val->u.Z);
    break;

  case irm_p:			/* pointer */
    if (val->u.p.xname) {
      printed = XPR (val->u.p.xname);
    } else if (val->u.p.ent) {
      printed = XPF1R ("(%I)", val->u.p.ent->name);
    } else {
      assert (val == tarval_p_void);
      printed = XPSR ("(void)");
    }
    break;

  case irm_b:			/* boolean */
    if (val->u.b) printed = XPSR ("true");
    else	  printed = XPSR ("false");
    break;

  case irm_B:			/* universal bits */
    printed = XPSR ("<@@@ some bits>");
    break;

  case irm_s:			/* string */
  case irm_S:
    { size_t i;
      char *buf = alloca (val->u.s.n + 2);
      char *bp;

      printed = 0;
      buf[0] = '\'';
      bp = buf + 1;
      for (i = 0;  i < val->u.s.n;  ++i) {
	if (isprint (val->u.s.p[i])) {
	  *bp++ = val->u.s.p[i];
	} else {
	  if (bp != buf) {
	    XPM (buf, bp-buf);
	    bp = buf;
	  }
	  XPF1 ("'\\%03o'", val->u.s.p[i]);
	}
      }
      *bp++ = '\'';
      XPM (buf, bp-buf);
      break;
    }


  case irm_M:			/* memory */
  case irm_R:			/* region */
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
