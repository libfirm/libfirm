/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Christian Schaefer
*
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
#include <stdbool.h>

# define target_bits 8

# define NUM_MODES 20

#ifndef _TARVAL_TYPEDEF_
#define _TARVAL_TYPEDEF_
typedef struct tarval tarval;
#endif

typedef enum { /* irm is short for `ir mode' */
  /*  according to tech report 1999-14: */
  irm_BB, irm_X,                 // basic block, execution
  irm_F, irm_D, irm_E,           // float(32), double(64), extended(80)
  irm_Bs, irm_Bu, irm_Hs, irm_Hu, irm_Is, irm_Iu, irm_Ls, irm_Lu,
                                 // byte(8), short(16), int(32), long(64)
  irm_C,                         // character
  irm_P,                         // pointer
  irm_b,                         // internal boolean
  irm_M,                         // memory
  irm_T,                         // tuple
  irm_U,                         // unicode character
  irm_max
} modecode;

typedef struct ir_mode ir_mode;

extern ir_mode *mode_T;  /* tuple (none) */
extern ir_mode *mode_F;	 /* signed float(32) */
extern ir_mode *mode_D;  /* signed double(64) */
extern ir_mode *mode_E;  /* signed extended(80) */
extern ir_mode *mode_Bs; /* signed byte (former char) */
extern ir_mode *mode_Bu;  /* unsigned byte (former char) */
extern ir_mode *mode_Hs; /* signed short integer */
extern ir_mode *mode_Hu; /* unsigened short integer */
extern ir_mode *mode_Is; /* signed integer */
extern ir_mode *mode_Iu; /* unsigned integer */
extern ir_mode *mode_Ls; /* signed long integer */
extern ir_mode *mode_Lu; /* unsigned long integer */
extern ir_mode *mode_C;  /* char */
extern ir_mode *mode_U;  /* unicode char */
extern ir_mode *mode_P;  /* pointer */
extern ir_mode *mode_b;  /* internal boolean */
extern ir_mode *mode_X;  /* execution */
extern ir_mode *mode_M;	 /* memory */
extern ir_mode *mode_BB; /* block */

/* Access routines for JNI Interface */
ir_mode *get_modeT();
ir_mode *get_modeF();
ir_mode *get_modeD();
ir_mode *get_modeE();
ir_mode *get_modeBs();
ir_mode *get_modeBu();
ir_mode *get_modeHs();
ir_mode *get_modeHu();
ir_mode *get_modeIs();
ir_mode *get_modeIu();
ir_mode *get_modeLs();
ir_mode *get_modeLu();
ir_mode *get_modeC();
ir_mode *get_modeU();
ir_mode *get_modeP();
ir_mode *get_modeb();
ir_mode *get_modeX();
ir_mode *get_modeM();
ir_mode *get_modeBB();

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
            = {num || irm_C || irm_P}

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
