/* Declarations for Target Values.
   Copyright (C) 1995, 1996 Christian von Roques
*/

/* $Id$ */

/*
Discussion of new interface, proposals by Prof. Waite:
(email of 13.6.2001)
> 1. You say that you plan to replace the tv module.  That replacement is
>    absolutely essential for an ANSI C translator:  Section 6.1.3.2 of the
>    standard says that the representation of an integer_constant depends
>    upon its value as well as any suffixes that are attached to it.  The
>    possible Firm modes for such a constant are i, I, l, and L.  The
>    current tv module provides only one integer conversion routine, and
>    that requires conversion by the client.  Since the type of the value
>    argument is long, this may preclude the representation of an unsigned
>    long constant.
>
>    There is a similar problem with floating constants.  Floating
>    constants can be suffixed in C, and the mode depends upon the suffix.
>    It can indicate that the constant is of type long double, which your
>    current tv module is incapable of representing.
>
>    Your tv module interface accepts two kinds of information: modes and
>    values.  Values obtained from the program text might be uninterpreted
>    strings, strings interpreted as integers, and strings interpreted as
>    reals.  Values provided by the compiler are usually integers.  Modes are
>    always Firm modes.  It seems to me that the tv module should provide
>    tarval* constructors for three of the four kinds of values.  Each of these
>    constructors should have an ir_mode parameter and one or more parameters
>    appropriate for the kind of value.  As is currently the case, one
>    constructor should be provided for both compiler-generated integers and
>    source strings interpreted as integers.  (This avoids problems of
>    different conversion radices -- the client does the conversion.)  For
>    symmetry, the constructor for source strings interpreted as reals should
>    accept a long double parameter and require the client to do the
>    conversion.

*/

#ifndef _TV_H_
#define _TV_H_

# include "irmode.h"
# include "entity.h"
# include "bool.h"

#ifndef _TARVAL_TYPEDEF_
#define _TARVAL_TYPEDEF_
typedef struct tarval tarval;
#endif

#include "gmp.h"
#undef __need_size_t		/* erroneously defined by 1.3.2's gmp.h */

/* how to represent target types on host */
typedef float  tarval_f;
typedef double tarval_d;
typedef long   tarval_chil;
typedef unsigned long tarval_CHIL;
typedef MP_INT tarval_Z;
typedef struct {
  /* if ent then xname is missing or mangled from ent,
     else if xname then xname is a linker symbol that is not mangled
       from an entity,
     else this is tarval_p_void.
     if this tarval describes a symbolic address of another tarval, tv points
     to this val */
  const char *xname;
  entity *ent;
  tarval *tv;
} tarval_p;
typedef struct {
  unsigned char *p;		/* may contain embedded 0, not 0-terminated */
  size_t n;
} tarval_s;
typedef tarval_s tarval_B;

struct tarval {
  union {
    tarval_f f;			/* float */
    tarval_d d;			/* double */
    tarval_chil chil;		/* signed integral */
    tarval_CHIL CHIL;		/* unsigned integral */
    tarval_Z Z;                 /* universal int */
    tarval_p p;			/* pointer */
    bool b;			/* boolean */
    tarval_B B;			/* universal bits */
    tarval_s s;			/* string */
  } u;
  ir_mode *mode;
};

extern tarval *tarval_bad;
/* We should have a tarval_undefined */
extern tarval *tarval_b_false;
extern tarval *tarval_b_true;
extern tarval *tarval_d_NaN;
extern tarval *tarval_d_Inf;
extern tarval *tarval_p_void;
extern tarval *tarval_mode_null[];
extern tarval *tarval_mode_min[];
extern tarval *tarval_mode_max[];

void tarval_init_1 (void);
void tarval_init_2 (void);

/* Hash function on tarvals */
unsigned tarval_hash (tarval *);

/* ************************ Constructors for tarvals ************************ */
tarval *tarval_Z_from_str (const char *, size_t, int base);
tarval *tarval_B_from_str (const char *, size_t);
tarval *tarval_f_from_str (const char *, size_t);
tarval *tarval_d_from_str (const char *, size_t);
tarval *tarval_s_from_str (const char *, size_t);
tarval *tarval_S_from_str (const char *, size_t);
tarval *tarval_int_from_str (const char *, size_t, int base, ir_mode *m);
tarval *tarval_from_long  (ir_mode *, long);

tarval *tarval_p_from_str (const char *);
/* The tarval represents the address of the entity.  As the address must
   be constant the entity must have as owner the global type. */
tarval *tarval_p_from_entity (entity *);

tarval *tarval_convert_to (tarval *, ir_mode *);

/* Building an irm_C, irm_s, irm_S or irm_B target value step by step. */
void tarval_start (void);
void tarval_append (const char *, size_t);
void tarval_append1 (char);
tarval *tarval_finish_as (ir_mode *);
tarval *tarval_cancel (void); /* returns tarval_bad */


/* The flags for projecting a comparison result */
typedef enum {
  irpn_False=0,		/* 0000 false */
  irpn_Eq,		/* 0001 equal */
  irpn_Lt,		/* 0010 less */
  irpn_Le,		/* 0011 less or equal */
  irpn_Gt,		/* 0100 greater */
  irpn_Ge,		/* 0101 greater of equal */
  irpn_Lg,		/* 0110 less or greater */
  irpn_Leg,		/* 0111 less, equal or greater = ordered */
  irpn_Uo,		/* 1000 unordered */
  irpn_Ue,		/* 1001 unordered or equal */
  irpn_Ul,		/* 1010 unordered or less */
  irpn_Ule,		/* 1011 unordered, less or equal */
  irpn_Ug,		/* 1100 unordered or greater */
  irpn_Uge,		/* 1101 unordered, greater or equal */
  irpn_Ne,		/* 1110 unordered, less or greater = not equal */
  irpn_True,		/* 1111 true */
  irpn_notmask = irpn_Leg
} ir_pncmp;

/* ******************** Arithmethic operations on tarvals ******************** */
/* Compare a with b and return an ir_pncmp describing the relation
   between a and b.  This is either Uo, Lt, Eq, Gt, or False if a or b
   are symbolic pointers which can not be compared at all. */
ir_pncmp tarval_comp (tarval *a, tarval *b);

tarval *tarval_neg (tarval *a);
tarval *tarval_add (tarval *, tarval *);
tarval *tarval_sub (tarval *, tarval *);
tarval *tarval_mul (tarval *, tarval *);
tarval *tarval_quo (tarval *, tarval *);
tarval *tarval_div (tarval *, tarval *);
tarval *tarval_mod (tarval *, tarval *);
tarval *tarval_abs (tarval *);
tarval *tarval_and (tarval *, tarval *);
tarval *tarval_or  (tarval *, tarval *);
tarval *tarval_eor (tarval *, tarval *);
tarval *tarval_shl (tarval *, tarval *);
tarval *tarval_shr (tarval *, tarval *);

/* Identifying some tarvals */
long tarval_classify (tarval *);
long tarval_ord (tarval *, int *fail);

/* moved to tv_t.h
   int tarval_print (XP_PAR1, const xprintf_info *, XP_PARN); */

/* return a mode-specific value */

tarval_f tv_val_f (tarval *tv);
tarval_d tv_val_d (tarval *tv);
tarval_chil tv_val_chil (tarval *tv);
tarval_CHIL tv_val_CHIL (tarval *tv);
tarval_Z tv_val_Z (tarval *tv);
tarval_p tv_val_p (tarval *tv);
bool     tv_val_b (tarval *tv);
tarval_B tv_val_B (tarval *tv);
tarval_s tv_val_s (tarval *tv);

ir_mode *get_tv_mode (tarval *tv);
/* Returns the entity if the tv is a pointer to an entity, else
   returns NULL; */
entity *get_tv_entity(tarval *tv);

/* Returns 0 if tv is positive, else > 0. @@@ not tested! */
int tv_is_negative(tarval *a);
#endif  /* _TV_H_ */
