/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer
**
irmode.h Modes for ir operators.

This module specifies the modes that type the firm nodes.  See
UKA tech report 1999-14 for more information about modes.


@@@ This file is at the level of the original fiasco.  It needs to be ported
to the version of the tech report!!! This will be done with the
reimplementation of the tarval module.
*/

/* $Id$ */

# ifndef _IRMODE_H_
# define _IRMODE_H_

#include "ident.h"
#include "bool.h"

# define target_bits 8

# define NUM_MODES 20

#ifndef _TARVAL_TYPEDEF_
#define _TARVAL_TYPEDEF_
typedef struct tarval tarval;
#endif

typedef enum { /* irm is short for `ir mode' */
  irm_T,                        /* former irm_N */
  irm_f, irm_d,                 /* 1, 2 */
  irm_c, irm_C, irm_h, irm_H, irm_i, irm_I, irm_l, irm_L, /* 3 .. 10 */
  irm_B, irm_b,                 /* 11, 12 */
  irm_p,
  irm_s, irm_S,                 /* 14, 15 */
  irm_X, irm_M, irm_R, irm_Z,
  irm_max
  /*  according to tech report 1999-14:
  irm_BB, irm_X,                 // basic block, execution
  irm_F, irm_D, irm_E,           // float, double, extended
  irm_B, irm_Bu, irm_H, irm_Hu, irm_I, irm_Iu, irm_L, irm_Lu,
                                 // byte(8), short(16), int(32), long(64)
  irm_C,                         // character
  irm_P,                         // pointer
  irm_b,                         // internal boolean
  irm_M,                         // memory
  irm_T,                         // tuple
  irm_max */
} modecode;

typedef struct ir_mode ir_mode;

extern ir_mode *mode_T; /* tuple (none) */
extern ir_mode *mode_f; /* signed float */
extern ir_mode *mode_d; /* signed double */
/* extern ir_mode *mode_e; * signed extended */
extern ir_mode *mode_c; /* signed byte (former char) */
extern ir_mode *mode_C; /* unsigned byte (former char) */
extern ir_mode *mode_h; /* signed short integer */
extern ir_mode *mode_H; /* unsigened short integer */
extern ir_mode *mode_i; /* signed integer */
extern ir_mode *mode_I; /* unsigned integer */
extern ir_mode *mode_l; /* signed long integer */
extern ir_mode *mode_L; /* unsigned long integer */
extern ir_mode *mode_B; /* bits */             /* oblivious */
/* extern ir_mode *mode_c;  * char */
extern ir_mode *mode_p; /* pointer */
extern ir_mode *mode_b; /* internal boolean */
extern ir_mode *mode_s;                        /* oblivious  */
extern ir_mode *mode_S;                        /* oblivious */
extern ir_mode *mode_X; /* execution */
extern ir_mode *mode_M; /* memory */
extern ir_mode *mode_R; /* block */
extern ir_mode *mode_Z; /* infinit integers */ /* oblivious */

/* Access routines for JNI Interface */
ir_mode *get_mode_T();
ir_mode *get_mode_f();
ir_mode *get_mode_d();
ir_mode *get_mode_c();
ir_mode *get_mode_C();
ir_mode *get_mode_h();
ir_mode *get_mode_H();
ir_mode *get_mode_i();
ir_mode *get_mode_I();
ir_mode *get_mode_l();
ir_mode *get_mode_L();
ir_mode *get_mode_B();
ir_mode *get_mode_p();
ir_mode *get_mode_b();
ir_mode *get_mode_s();
ir_mode *get_mode_S();
ir_mode *get_mode_X();
ir_mode *get_mode_M();
ir_mode *get_mode_R();
ir_mode *get_mode_Z();

/* An enum for this mode */
modecode get_mode_modecode (ir_mode *mode);
/* void  set_mode_modecode (ir_mode *mode, modecode code);  */

/* The ident of this mode */
ident   *get_mode_ident    (ir_mode *mode);
/* void  set_mode_ident    (ir_mode *mode, ident *id);    */

/* The name of this mode */
const char *get_mode_name  (ir_mode *mode);
/* void  set_mode_name     (ir_mode *mode, char *name);    */

/* The size of values of the mode in bytes. */
int      get_mode_size     (ir_mode *mode);
/* void  set_mode_size     (ir_mode *mode, int size);       */

/* The alignment of values of the mode. */
int      get_mode_ld_align (ir_mode *mode);
/* void  set_mode_ld_align (ir_mode *mode, int ld_align); */

/* The smallest representable value */
tarval  *get_mode_min      (ir_mode *mode);
/* void  set_mode_min      (ir_mode *mode, tarval *min); */

/* The biggest representable value */
tarval  *get_mode_max      (ir_mode *mode);
/* void  set_mode_max      (ir_mode *mode, tarval *max); */

/* The value Zero represented in this mode */
tarval  *get_mode_null     (ir_mode *mode);
/* void  set_mode_null     (ir_mode *mode, tarval *null); */

/* Returns 1 if mode is signed, else 0. */
unsigned get_mode_fsigned  (ir_mode *mode);
/* void  set_mode_fsigned  (ir_mode *mode, unsigned fsigned); */

/* Returns 1 if mode is float, else 0. */
unsigned get_mode_ffloat   (ir_mode *mode);
/* void  set_mode_ffloat   (ir_mode *mode, unsigned ffloat); */


/* Test for a certain class of modes. */
int mode_is_signed (ir_mode *mode);
int mode_is_float (ir_mode *mode);
int mode_is_int (ir_mode *mode);
int mode_is_num (ir_mode *mode);
int mode_is_data (ir_mode *mode);
int mode_is_datab (ir_mode *mode);
int mode_is_dataM (ir_mode *mode);

/* Returns true if sm can be converted to lm without loss
   according to firm definiton */
bool smaller_mode(ir_mode *sm, ir_mode *lm);

# endif /* _IRMODE_H_ */
