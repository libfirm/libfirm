/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irmode_t.h"
# include <malloc.h>
# include <stddef.h>
# include "tv.h"

ir_mode *mode_T;
ir_mode *mode_f;
ir_mode *mode_d;
ir_mode *mode_c;
ir_mode *mode_C;
ir_mode *mode_h;
ir_mode *mode_H;
ir_mode *mode_i;
ir_mode *mode_I;
ir_mode *mode_l;
ir_mode *mode_L;
ir_mode *mode_B;
ir_mode *mode_b;
ir_mode *mode_p;
ir_mode *mode_s;
ir_mode *mode_S;
ir_mode *mode_X;
ir_mode *mode_M;
ir_mode *mode_R;
ir_mode *mode_Z;

void
init_mode (void)
{
  /* allocate all modes. We need to memset them as tarval_vrfy
     reads fields before they are initialized: It compares to
     min/max when tarvals for min/max are allocated!  */
  mode_T = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_T, 0, sizeof(ir_mode));
  mode_f = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_f, 0, sizeof(ir_mode));
  mode_d = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_d, 0, sizeof(ir_mode));
  mode_c = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_c, 0, sizeof(ir_mode));
  mode_C = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_C, 0, sizeof(ir_mode));
  mode_h = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_h, 0, sizeof(ir_mode));
  mode_H = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_H, 0, sizeof(ir_mode));
  mode_i = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_i, 0, sizeof(ir_mode));
  mode_I = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_I, 0, sizeof(ir_mode));
  mode_l = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_l, 0, sizeof(ir_mode));
  mode_L = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_L, 0, sizeof(ir_mode));
  mode_B = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_B, 0, sizeof(ir_mode));
  mode_b = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_b, 0, sizeof(ir_mode));
  mode_p = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_p, 0, sizeof(ir_mode));
  mode_s = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_s, 0, sizeof(ir_mode));
  mode_S = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_S, 0, sizeof(ir_mode));
  mode_X = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_X, 0, sizeof(ir_mode));
  mode_M = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_M, 0, sizeof(ir_mode));
  mode_R = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_R, 0, sizeof(ir_mode));
  mode_Z = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_Z, 0, sizeof(ir_mode));

  mode_T->code = irm_T;
  mode_f->code = irm_f;
  mode_d->code = irm_d;
  mode_c->code = irm_c;
  mode_C->code = irm_C;
  mode_h->code = irm_h;
  mode_H->code = irm_H;
  mode_i->code = irm_i;
  mode_I->code = irm_I;
  mode_l->code = irm_l;
  mode_L->code = irm_L;
  mode_B->code = irm_B;
  mode_b->code = irm_b;
  mode_p->code = irm_p;
  mode_s->code = irm_s;
  mode_S->code = irm_S;
  mode_X->code = irm_X;
  mode_M->code = irm_M;
  mode_R->code = irm_R;
  mode_Z->code = irm_Z;

  /* initialize all modes */

  mode_T->name = id_from_str ("T", 1);
  mode_T->fsigned = 0;
  mode_T->ffloat = 0;

  /* float */
  mode_f->name = id_from_str ("f", 1);
  mode_f->fsigned = 1;
  mode_f->ffloat = 1;

  /* double */
  mode_d->name = id_from_str ("d", 1);
  mode_d->fsigned = 1;
  mode_d->ffloat = 1;

  /* signed char */
  mode_c->name = id_from_str ("c", 1);
  mode_c->fsigned = 1;
  mode_c->ffloat = 0;
  mode_c->ld_align = 0;
  mode_c->size = 1;
  mode_c->min = tarval_from_long (mode_l, 0xffffff80);
  mode_c->max = tarval_from_long (mode_l, 0x0000007f);
  mode_c->null = tarval_from_long (mode_c, 0);

  /* unsigned char */
  mode_C->name = id_from_str ("C", 1);
  mode_C->fsigned = 0;
  mode_C->ffloat = 0;
  mode_C->ld_align = 0;
  mode_C->size = 1;
  mode_C->min = tarval_from_long (mode_L, 0x00000000);
  mode_C->max = tarval_from_long (mode_L, 0x000000ff);
  mode_C->null = tarval_from_long (mode_C, 0);

  /* signed short integer */
  mode_h->name = id_from_str ("h", 1);
  mode_h->fsigned = 1;
  mode_h->ffloat = 0;
  mode_h->ld_align = 1;
  mode_h->size = 4;
  mode_h->min = tarval_from_long (mode_l, 0xffff8000);
  mode_h->max = tarval_from_long (mode_l, 0x00007fff);
  mode_h->null = tarval_from_long (mode_h, 0);

  /* unsigened short integer */
  mode_H->name = id_from_str ("H", 1);
  mode_H->fsigned = 0;
  mode_H->ffloat = 0;
  mode_H->ld_align = 1;
  mode_H->size = 4;
  mode_H->min = tarval_from_long (mode_L, 0x00000000);
  mode_H->max = tarval_from_long (mode_L, 0x0000ffff);
  mode_H->null = tarval_from_long (mode_H, 0);

  /* signed integer */
  mode_i->name = id_from_str ("i", 1);
  mode_i->fsigned = 1;
  mode_i->ffloat = 0;
  mode_i->ld_align = 2;
  mode_i->size = 4;
  mode_i->min = tarval_from_long (mode_l, 0x80000000);
  mode_i->max = tarval_from_long (mode_l, 0x7fffffff);
  mode_i->null = tarval_from_long (mode_i, 0);

  /* unsigned integer */
  mode_I->name = id_from_str ("I", 1);
  mode_I->fsigned = 0;
  mode_I->ffloat = 0;
  mode_I->ld_align = 2;
  mode_I->size = 4;
  mode_I->min = tarval_from_long (mode_L, 0x00000000);
  mode_I->max = tarval_from_long (mode_L, 0xffffffff);
  mode_I->null = tarval_from_long (mode_I, 0);

  /* signed long integer */
  mode_l->name = id_from_str ("l", 1);
  mode_l->fsigned = 1;
  mode_l->ffloat = 0;
  mode_l->ld_align = 2;
  mode_l->size = 4;
  mode_l->min = tarval_from_long (mode_l, 0x80000000);
  mode_l->max = tarval_from_long (mode_l, 0x7fffffff);
  mode_I->null = tarval_from_long (mode_l, 0);

  /* unsigned long integer */
  mode_L->name = id_from_str ("L", 1);
  mode_L->fsigned = 0;
  mode_L->ffloat = 0;
  mode_L->ld_align = 2;
  mode_L->size = 4;
  mode_L->min = tarval_from_long (mode_L, 0x00000000);
  mode_L->max = tarval_from_long (mode_L, 0xffffffff);
  mode_L->null = tarval_from_long (mode_L, 0);

  /* universal bits */
  mode_B->name = id_from_str ("B", 1);
  mode_B->fsigned = 0;
  mode_B->ffloat = 0;

  /* boolean */
  mode_b->name = id_from_str ("b", 1);
  mode_b->fsigned = 0;
  mode_b->ffloat = 0;

  /* pointer */
  mode_p->name = id_from_str ("p", 1);
  mode_p->fsigned = 0;
  mode_p->ffloat = 0;
  mode_p->ld_align = 2;
  mode_p->size = 4;
  mode_p->min = tarval_from_long (mode_L, 0x00000000);
  mode_p->max = tarval_from_long (mode_L, 0xffffffff);
  mode_p->null = tarval_from_long (mode_p, 0);

  mode_s->name = id_from_str ("s", 1);
  mode_s->fsigned = 0;
  mode_s->ffloat = 0;

  mode_S->name = id_from_str ("S", 1);
  mode_S->fsigned = 0;
  mode_S->ffloat = 0;

  /* Execution */
  mode_X->name = id_from_str ("X", 1);
  mode_X->fsigned = 0;
  mode_X->ffloat = 0;

  /* Memory */
  mode_M->name = id_from_str ("M", 1);
  mode_M->fsigned = 0;
  mode_M->ffloat = 0;

  mode_R->name = id_from_str ("R", 1);
  mode_R->fsigned = 0;
  mode_R->ffloat = 0;

  mode_Z->name = id_from_str ("Z", 1);
  mode_Z->fsigned = 1;
  mode_Z->ffloat = 0;
}

/* Functions for the direct access to all attributes od a ir_mode */

modecode
get_mode_modecode (ir_mode *mode)
{
  return mode->code;
}

/*
inline void
set_mode_modecode (ir_mode *mode, modecode code)
{
  mode->code = code;
}
*/

ident *
get_mode_ident (ir_mode *mode)
{
  return mode->name;
}

/*
inline void
set_mode_ident (ir_mode *mode, ident *name)
{
  mode->name = name;
}
*/

inline const char *
get_mode_name       (ir_mode *mode) {
  assert(mode);
  return id_to_str(mode->name);
}
/* void  set_mode_name       (ir_mode *mode, char *name);    */

int
get_mode_size (ir_mode *mode)
{
  return mode->size;
}
/*
inline void
set_mode_size (ir_mode *mode, int size)
{
  mode->size = size;
}
*/

int
get_mode_ld_align (ir_mode *mode)
{
  return mode->ld_align;
}

/*
inline void
set_mode_ld_align (ir_mode *mode, int ld_align)
{
  mode->ld_align = ld_align;
}
*/

tarval *
get_mode_min (ir_mode *mode)
{
  return mode->min;
}

/*
inline void
set_mode_min (ir_mode *mode, tarval *min)
{
mode->min = min;
}
*/

tarval *
get_mode_max (ir_mode *mode)
{
  return mode->max;
}

/*
inline void
set_mode_max (ir_mode *mode, tarval *max)
{
  mode->max = max;
}
*/

tarval *
get_mode_null (ir_mode *mode)
{
  return mode->null;
}

/*
inline void
set_mode_null (ir_mode *mode, tarval *null)
{
  mode->null = null;
}
*/

unsigned
get_mode_fsigned (ir_mode *mode)
{
  return mode->fsigned;
}

/*
inline voida
set_mode_fsigned (ir_mode *mode, unsigned fsigned)
{
  mode->fsigned = fsigned;
}
*/

unsigned
get_mode_ffloat (ir_mode *mode)
{
  return mode->ffloat;
}

/*
inline void
set_mode_ffloat (ir_mode *mode, unsigned ffloat)
{
  mode->ffloat = ffloat;
}
*/

/* Functions to check, whether a modecode is signed, float, int, num, data,
   datab or dataM. For more exact definitions read the corresponding pages
   in the firm documentation or the followingenumeration

   The set of "float" is defined as:
   ---------------------------------
   float = {irm_f, irm_d}

   The set of "int" is defined as:
   -------------------------------
   int   = {irm_c, irm_C, irm_h, irm_H, irm_i, irm_I, irm_l, irm_L}

   The set of "num" is defined as:
   -------------------------------
   num   = {irm_f, irm_d, irm_c, irm_C, irm_h, irm_H,
            irm_i, irm_I, irm_l, irm_L}
            = {float || int}

   The set of "data" is defined as:
   -------------------------------
   data  = {irm_f, irm_d, irm_c, irm_C, irm_h, irm_H,
            irm_i, irm_I, irm_l, irm_L, irm_p}
            = {num || irm_p}

   The set of "datab" is defined as:
   ---------------------------------
   datab = {irm_f, irm_d, irm_c, irm_C, irm_h, irm_H,
            irm_i, irm_I, irm_l, irm_L, irm_p, irm_b, irm_B}
            = {data || irm_b || irm_B}

   The set of "dataM" is defined as:
   ---------------------------------
   dataM = {irm_f, irm_d, irm_c, irm_C, irm_h, irm_H, irm_i,
            irm_I, irm_l, irm_L, irm_p, irm_M}
            = {data || irm_M}
*/

int
mode_is_signed (ir_mode *mode)
{
   int res;
   unsigned fsigned;
   fsigned = get_mode_fsigned (mode);
   if (fsigned == 1) {
     res = 1;
    }
   else {
     res = 0;
   }
   return res;
}

int
mode_is_float (ir_mode *mode)
{
   int res;
   unsigned ffloat;
   ffloat = get_mode_ffloat (mode);
   if (ffloat == 1) {
      res = 1;
    }
   else {
     res = 0;
   }
   return res;
}


int
mode_is_int (ir_mode *mode)
{
   int res;
   modecode code;
   code = get_mode_modecode (mode);
   if ((code >= irm_c) &&  (code <= irm_L)) {
      res = 1;
    }
   else {
     res = 0;
   }
   return res;
}


int
mode_is_num (ir_mode *mode)
{
  int res;
  if (mode_is_int (mode) || mode_is_float (mode)) {
    res = 1;
  }
  else {
    res = 0;
  }
  return res;
}

int
mode_is_data (ir_mode *mode)
{
  int res;
  modecode code;
  code = get_mode_modecode (mode);
  if (mode_is_num (mode) || code == irm_p) {
    res = 1;
  }
  else {
    res = 0;
  }
  return res;
}

int
mode_is_datab (ir_mode *mode)
{
  int res;
  modecode code;
  code = get_mode_modecode (mode);
  if (mode_is_data (mode) || code == irm_b || code == irm_B) {
    res = 1;
  }
  else {
    res = 0;
  }
  return res;
}

int
mode_is_dataM (ir_mode *mode)
{
  int res;
  modecode code;
  code = get_mode_modecode (mode);
  if (mode_is_data (mode) || code == irm_M) {
    res = 1;
  }
  else {
    res = 0;
  }
  return res;
}
