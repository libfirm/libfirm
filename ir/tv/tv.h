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
# include <stdbool.h>

#ifndef _TARVAL_TYPEDEF_
#define _TARVAL_TYPEDEF_
typedef struct tarval tarval;
#endif

/* how to represent target types on host */
typedef float  tarval_F;
typedef double tarval_D;
typedef long double tarval_E;
typedef long   tarval_sInt;
typedef unsigned long tarval_uInt;
typedef char tarval_C;
typedef unsigned short tarval_U;   /* 16 bit ?! wchar could be defined as char...  */
typedef struct {
  /** if ent then xname is missing or mangled from ent,
     else if xname then xname is a linker symbol that is not mangled
       from an entity,
     else this is tarval_p_void.
     if this tarval describes a symbolic address of another tarval, tv points
     to this val */
  const char *xname;
  entity *ent;
  tarval *tv;
} tarval_P;

struct tarval {
  union {
    tarval_F F;         /**< float */
    tarval_D D;         /**< double */
    tarval_E E;         /**< extended */
    tarval_sInt sInt;   /**< signed integral */
    tarval_uInt uInt;   /**< unsigned integral */
    tarval_C C;         /**< character */
    tarval_U U;         /**< unicode character */
    tarval_P P;         /**< pointer */
    bool b;             /**< boolean */
  } u;
  ir_mode *mode;
};


extern tarval *tarval_bad;                  tarval *get_tarval_bad();
/* We should have a tarval_undefined */
extern tarval *tarval_b_false;              tarval *get_tarval_b_false  ();
extern tarval *tarval_b_true;               tarval *get_tarval_b_true   ();
extern tarval *tarval_D_NaN;                tarval *get_tarval_D_NaN    ();
extern tarval *tarval_D_Inf;                tarval *get_tarval_D_Inf    ();
extern tarval *tarval_P_void;               tarval *get_tarval_P_void   ();
extern tarval *tarval_mode_null[];          tarval *get_tarval_mode_null(ir_mode *mode);
/* @@@ These are not initialized!! Don't use. */
extern tarval *tarval_mode_min[];           tarval *get_tarval_mode_min (ir_mode *mode);
extern tarval *tarval_mode_max[];           tarval *get_tarval_mode_max (ir_mode *mode);

void tarval_init_1 (void);
void tarval_init_2 (void);

/* ************************ Constructors for tarvals ************************ */
tarval *tarval_F_from_str (const char *s, size_t len);
tarval *tarval_D_from_str (const char *s, size_t len);
tarval *tarval_int_from_str (const char *s, size_t len, int base, ir_mode *m);
tarval *tarval_from_long  (ir_mode *m, long val);

tarval *tarval_P_from_str (const char *xname);
/* The tarval represents the address of the entity.  As the address must
   be constant the entity must have as owner the global type. */
tarval *tarval_P_from_entity (entity *ent);

tarval *tarval_convert_to (tarval *src, ir_mode *m);

/* Building an irm_C, irm_s, irm_S or irm_B target value step by step. */
void tarval_start (void);
void tarval_append (const char *p, size_t n);
void tarval_append1 (char ch);
tarval *tarval_finish_as (ir_mode *m);
tarval *tarval_cancel (void); /* returns tarval_bad */

/* The flags for projecting a comparison result */
typedef enum {
  irpn_False=0,	/**< 0000 false */
  irpn_Eq,		/**< 0001 equal */
  irpn_Lt,		/**< 0010 less */
  irpn_Le,		/**< 0011 less or equal */
  irpn_Gt,		/**< 0100 greater */
  irpn_Ge,		/**< 0101 greater of equal */
  irpn_Lg,		/**< 0110 less or greater */
  irpn_Leg,		/**< 0111 less, equal or greater = ordered */
  irpn_Uo,		/**< 1000 unordered */
  irpn_Ue,		/**< 1001 unordered or equal */
  irpn_Ul,		/**< 1010 unordered or less */
  irpn_Ule,		/**< 1011 unordered, less or equal */
  irpn_Ug,		/**< 1100 unordered or greater */
  irpn_Uge,		/**< 1101 unordered, greater or equal */
  irpn_Ne,		/**< 1110 unordered, less or greater = not equal */
  irpn_True		/**< 1111 true */
  /*irpn_notmask = irpn_Leg  @@@ removed for JNI builder */
} ir_pncmp;
#define irpn_notmask irpn_Leg

/* ******************** Arithmethic operations on tarvals ******************** */
/* Compare a with b and return an ir_pncmp describing the relation
   between a and b.  This is either Uo, Lt, Eq, Gt, or False if a or b
   are symbolic pointers which can not be compared at all. */
ir_pncmp tarval_comp (tarval *a, tarval *b);

tarval *tarval_neg (tarval *a);
tarval *tarval_add (tarval *a, tarval *b);
tarval *tarval_sub (tarval *a, tarval *b);
tarval *tarval_mul (tarval *a, tarval *b);
tarval *tarval_quo (tarval *a, tarval *b);
tarval *tarval_div (tarval *a, tarval *b);
tarval *tarval_mod (tarval *a, tarval *b);
tarval *tarval_abs (tarval *a);
tarval *tarval_and (tarval *a, tarval *b);
tarval *tarval_or  (tarval *a, tarval *b);
tarval *tarval_eor (tarval *a, tarval *b);
tarval *tarval_shl (tarval *a, tarval *b);
tarval *tarval_shr (tarval *a, tarval *b);

/* Identifying some tarvals */
long tarval_classify (tarval *tv);
long tarval_ord (tarval *tv, int *fail);

/* return a mode-specific value */
tarval_F tv_val_F (tarval *tv);
tarval_D tv_val_D (tarval *tv);
tarval_sInt tv_val_sInt (tarval *tv);
tarval_uInt tv_val_uInt (tarval *tv);
/* @@@ temporarily removed.
   jni builder can not deal with the return value.
   All definitions of types are interpreted as pointer values until
   type analysis exists for crecoder.
   tarval_p tv_val_p (tarval *tv);
*/
bool     tv_val_b (tarval *tv);

ir_mode *get_tv_mode (tarval *tv);
/* Returns the entity if the tv is a pointer to an entity, else
   returns NULL; */
entity *get_tv_entity(tarval *tv);

/* Returns 0 if tv is positive, else > 0. @@@ not tested! */
int tv_is_negative(tarval *a);
#endif  /* _TV_H_ */
