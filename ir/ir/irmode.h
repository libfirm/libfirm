/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer
**
irmode.h Modes for ir operators.

This module specifies the modes that type the firm nodes.  See
UKA tech report 1999-44 for more information about modes.


@@@ This file is at the level of the original fiasco.  It needs to be ported
to the version of the tech report!!! This will be done with the reimplementation
of the tarval module.
*/

# ifndef _IRMODE_H_
# define _IRMODE_H_

#include "ident.h"

# define target_bits 8

# define NUM_MODES 20

typedef enum { /* irm is short for `ir mode' */
  irm_T,                        /* former irm_N */
  irm_f, irm_d, /* irm_e, */
  irm_c, irm_C, irm_h, irm_H, irm_i, irm_I, irm_l, irm_L,
  irm_B, irm_b,
  irm_p,
  irm_s, irm_S,
  irm_X, irm_M, irm_R, irm_Z,
  irm_max
  /*  according to tech report 1999-44:
  irm_BB, irm_X,                 // basic block, execution
  irm_F, irm_D, irm_E,           // float, double, extended
  irm_B, irm_Bu, irm_H, irm_Hu, irm_I, irm_Iu, irm_L, irm_Lu,
                                 // byte, short, int, long
  irm_C,                         // character
  irm_P,                         // pointer
  irm_b,                         // internal boolean
  irm_M,                         // memory
  irm_T,                         // tuple
  irm_max */
} modecode;

typedef struct {
  modecode code;
  ident *name;            /* Name of this mode */
  int    size;            /* size of the mode in Bytes. */
  int    ld_align;        /* ld means log2 */
  struct tarval *min;     /* largest value to be represented by this mode */
  struct tarval *max;     /* smallest value to be represented by this mode */
  struct tarval *null;    /* Representation of zero in this mode */
  unsigned fsigned:1;     /* signedness of this mode */
  unsigned ffloat:1;      /* true if this is a float */
} ir_mode;

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
extern ir_mode *mode_T; /* tuple (none) */

void init_mode (void);

modecode get_modecode_of_mode (ir_mode *mode);
/* void set_modecode_of_mode (ir_mode *mode, modecode code); */
ident *get_ident_of_mode (ir_mode *mode);
ident *get_mode_ident (ir_mode *mode);

/* void set_ident_of_mode (ir_mode *mode, ident *name); */
int get_size_of_mode (ir_mode *mode);
/* void set_size_of_mode (ir_mode *mode, int size); */
int get_ld_align_of_mode (ir_mode *mode);
/* void set_ld_align_of_mode (ir_mode *mode, int ld_align); */
struct tarval *get_min_of_mode (ir_mode *mode);
/* void set_min_of_mode (ir_mode *mode, struct tarval *min); */
struct tarval *get_max_of_mode (ir_mode *mode);
/* void set_max_of_mode (ir_mode *mode, struct tarval *max); */
struct tarval *get_null_of_mode (ir_mode *mode);
/* void set_null_of_mode (ir_mode *mode, struct tarval *null); */
unsigned get_fsigned_of_mode (ir_mode *mode);
/* void set_fsigned_of_mode (ir_mode *mode, unsigned fsigned); */
unsigned get_ffloat_of_mode (ir_mode *mode);
/* void set_ffloat_of_mode (ir_mode *mode, unsigned ffloat); */

int mode_is_signed (ir_mode *mode);
int mode_is_float (ir_mode *mode);
int mode_is_int (ir_mode *mode);
# define is_chilCHIL(m) ((m) <= irm_L && (m) >= irm_c) /* old */
int mode_is_num (ir_mode *mode);
int mode_is_data (ir_mode *mode);
int mode_is_datab (ir_mode *mode);
int mode_is_dataM (ir_mode *mode);

# endif /* _IRMODE_H_ */
