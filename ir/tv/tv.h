/* Declarations for Target Values.
   Copyright (C) 1995, 1996 Christian von Roques */

#ifndef _TV_H
#define _TV_H

# include "irmode.h"
# include "entity.h"
# include "xprintf.h"

typedef struct tarval tarval;

#include <gmp.h>
#undef __need_size_t		/* erroneously defined by 1.3.2's gmp.h */
/* #include "deftab.h" */

/* how to represent target types on host */
typedef float tarval_f;
typedef double tarval_d;
typedef long tarval_chil;
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

/************************* Constructors for tarvals *************************/
tarval *tarval_Z_from_str (const char *, size_t, int base);
tarval *tarval_B_from_str (const char *, size_t);
tarval *tarval_d_from_str (const char *, size_t);
tarval *tarval_s_from_str (const char *, size_t);
tarval *tarval_S_from_str (const char *, size_t);
tarval *tarval_from_long (ir_mode *, long);
tarval *tarval_p_from_str (const char *);
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
  irpn_False=0,		/* false */
  irpn_Eq,		/* equal */
  irpn_Lt,		/* less */
  irpn_Le,		/* less or equal */
  irpn_Gt,		/* greater */
  irpn_Ge,		/* greater of equal */
  irpn_Lg,		/* less or greater */
  irpn_Leg,		/* less, equal or greater = ordered */
  irpn_Uo,		/* unordered */
  irpn_Ue,		/* unordered or equal */
  irpn_Ul,		/* unordered or less */
  irpn_Ule,		/* unordered, less or equal */
  irpn_Ug,		/* unordered or greater */
  irpn_Uge,		/* unordered, greater or equal */
  irpn_Ne,		/* unordered, less or greater = not equal */
  irpn_True,		/* true */
  irpn_notmask = irpn_Leg
} ir_pncmp;

/********************* Arithmethic operations on tarvals *********************/
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
tarval *tarval_and (tarval *, tarval *);
tarval *tarval_or  (tarval *, tarval *);
tarval *tarval_eor (tarval *, tarval *);
tarval *tarval_shl (tarval *, tarval *);
tarval *tarval_shr (tarval *, tarval *);

/* Identifying some tarvals */
long tarval_classify (tarval *);
long tarval_ord (tarval *, int *fail);

int tarval_print (XP_PAR1, const xprintf_info *, XP_PARN);

/* return a mode-specific value */

tarval_f tv_val_f (tarval *tv);
tarval_d tv_val_d (tarval *tv);
tarval_chil tv_val_chil (tarval *tv);
tarval_CHIL tv_val_CHIL (tarval *tv);
tarval_Z tv_val_Z (tarval *tv);
tarval_p tv_val_p (tarval *tv);
bool tv_val_b (tarval *tv);
tarval_B tv_val_B (tarval *tv);
tarval_s tv_val_s (tarval *tv);

#ifdef NDEBUG
#define TARVAL_VRFY(val) ((void)0)
#else
#define TARVAL_VRFY(val) _tarval_vrfy ((val))
extern void _tarval_vrfy (const tarval *);
#endif

#ifdef STATS
void tarval_stats (void);
#else
#define tarval_stats() ((void)0)
#endif

ir_mode *get_tv_mode (tarval *tv);

#endif
