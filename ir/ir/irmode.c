/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
* All rights reserved.
*
* Authors: Martin Trapp, Christian Schaefer
*
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include "irmode_t.h"
# include <malloc.h>
# include <stddef.h>
# include <string.h>
# include "tv.h"

ir_mode *mode_T;
ir_mode *mode_F;
ir_mode *mode_D;
ir_mode *mode_E;
ir_mode *mode_Bs;
ir_mode *mode_Bu;
ir_mode *mode_Hs;
ir_mode *mode_Hu;
ir_mode *mode_Is;
ir_mode *mode_Iu;
ir_mode *mode_Ls;
ir_mode *mode_Lu;
ir_mode *mode_C;
ir_mode *mode_U;
ir_mode *mode_b;
ir_mode *mode_P;
ir_mode *mode_X;
ir_mode *mode_M;
ir_mode *mode_BB;

INLINE ir_mode *get_modeT() { return mode_T; }
INLINE ir_mode *get_modeF() { return mode_F; }
INLINE ir_mode *get_modeD() { return mode_D; }
INLINE ir_mode *get_modeE() { return mode_E; }
INLINE ir_mode *get_modeBs() { return mode_Bs; }
INLINE ir_mode *get_modeBu() { return mode_Bu; }
INLINE ir_mode *get_modeHs() { return mode_Hs; }
INLINE ir_mode *get_modeHu() { return mode_Hu; }
INLINE ir_mode *get_modeIs() { return mode_Is; }
INLINE ir_mode *get_modeIu() { return mode_Iu; }
INLINE ir_mode *get_modeLs() { return mode_Ls; }
INLINE ir_mode *get_modeLu() { return mode_Lu; }
INLINE ir_mode *get_modeC() { return mode_C; }
INLINE ir_mode *get_modeU() { return mode_U; }
INLINE ir_mode *get_modeb() { return mode_b; }
INLINE ir_mode *get_modeP() { return mode_P; }
INLINE ir_mode *get_modeX() { return mode_X; }
INLINE ir_mode *get_modeM() { return mode_M; }
INLINE ir_mode *get_modeBB() { return mode_BB; }

void
init_mode (void)
{
  /* allocate all modes. We need to memset them as tarval_vrfy
     reads fields before they are initialized: It compares to
     min/max when tarvals for min/max are allocated!  */
  mode_T  = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_T, 0, sizeof(ir_mode));
  mode_F  = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_F, 0, sizeof(ir_mode));
  mode_D  = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_D, 0, sizeof(ir_mode));
  mode_E  = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_E, 0, sizeof(ir_mode));
  mode_Bs = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_Bs, 0, sizeof(ir_mode));
  mode_Bu = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_Bu, 0, sizeof(ir_mode));
  mode_Hs = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_Hs, 0, sizeof(ir_mode));
  mode_Hu = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_Hu, 0, sizeof(ir_mode));
  mode_Is = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_Is, 0, sizeof(ir_mode));
  mode_Iu = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_Iu, 0, sizeof(ir_mode));
  mode_Ls = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_Ls, 0, sizeof(ir_mode));
  mode_Lu = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_Lu, 0, sizeof(ir_mode));
  mode_C  = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_C, 0, sizeof(ir_mode));
  mode_U  = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_U, 0, sizeof(ir_mode));
  mode_b  = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_b, 0, sizeof(ir_mode));
  mode_P  = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_P, 0, sizeof(ir_mode));
  mode_X  = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_X, 0, sizeof(ir_mode));
  mode_M  = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_M, 0, sizeof(ir_mode));
  mode_BB = (ir_mode *) malloc (sizeof (ir_mode)); memset(mode_BB, 0, sizeof(ir_mode));

  mode_T->code = irm_T;
  mode_F->code = irm_F;
  mode_D->code = irm_D;
  mode_E->code = irm_E;
  mode_Bs->code = irm_Bs;
  mode_Bu->code = irm_Bu;
  mode_Hs->code = irm_Hs;
  mode_Hu->code = irm_Hu;
  mode_Is->code = irm_Is;
  mode_Iu->code = irm_Iu;
  mode_Ls->code = irm_Ls;
  mode_Lu->code = irm_Lu;
  mode_C->code = irm_C;
  mode_U->code = irm_U;
  mode_b->code = irm_b;
  mode_P->code = irm_P;
  mode_X->code = irm_X;
  mode_M->code = irm_M;
  mode_BB->code = irm_BB;

  /* initialize all modes */

  mode_T->name = id_from_str ("T", 1);
  mode_T->fsigned = 0;
  mode_T->ffloat = 0;

  /* float */
  mode_F->name = id_from_str ("F", 1);
  mode_F->fsigned = 1;
  mode_F->ffloat = 1;
  mode_F->ld_align = 4;
  mode_F->size = 4;

  /* double */
  mode_D->name = id_from_str ("D", 1);
  mode_D->fsigned = 1;
  mode_D->ffloat = 1;
  mode_D->ld_align = 4;
  mode_D->size = 8;

  /* extended */
  mode_E->name = id_from_str ("E", 1);
  mode_E->fsigned = 1;
  mode_E->ffloat = 1;
  mode_E->ld_align = 4;
  mode_E->size = 12;

  /* signed byte */
  mode_Bs->name = id_from_str ("Bs", 1);
  mode_Bs->fsigned = 1;
  mode_Bs->ffloat = 0;
  mode_Bs->ld_align = 1;
  mode_Bs->size = 1;
  mode_Bs->min = tarval_from_long (mode_Ls, 0xffffff80);
  mode_Bs->max = tarval_from_long (mode_Ls, 0x0000007f);
  mode_Bs->null = tarval_from_long (mode_Bs, 0);

  /* unsigned byte */
  mode_Bu->name = id_from_str ("Bu", 1);
  mode_Bu->fsigned = 0;
  mode_Bu->ffloat = 0;
  mode_Bu->ld_align = 1;
  mode_Bu->size = 1;
  mode_Bu->min = tarval_from_long (mode_Lu, 0x00000000);
  mode_Bu->max = tarval_from_long (mode_Lu, 0x000000ff);
  mode_Bu->null = tarval_from_long (mode_Bu, 0);

  /* signed short integer */
  mode_Hs->name = id_from_str ("Hs", 1);
  mode_Hs->fsigned = 1;
  mode_Hs->ffloat = 0;
  mode_Hs->ld_align = 2;
  mode_Hs->size = 2;
  mode_Hs->min = tarval_from_long (mode_Ls, 0xffff8000);
  mode_Hs->max = tarval_from_long (mode_Ls, 0x00007fff);
  mode_Hs->null = tarval_from_long (mode_Hs, 0);

  /* unsigned short integer */
  mode_Hu->name = id_from_str ("Hu", 1);
  mode_Hu->fsigned = 0;
  mode_Hu->ffloat = 0;
  mode_Hu->ld_align = 2;
  mode_Hu->size = 2;
  mode_Hu->min = tarval_from_long (mode_Lu, 0x00000000);
  mode_Hu->max = tarval_from_long (mode_Lu, 0x0000ffff);
  mode_Hu->null = tarval_from_long (mode_Hu, 0);

  /* signed integer */
  mode_Is->name = id_from_str ("Is", 1);
  mode_Is->fsigned = 1;
  mode_Is->ffloat = 0;
  mode_Is->ld_align = 4;
  mode_Is->size = 4;
  mode_Is->min = tarval_from_long (mode_Ls, 0x80000000);
  mode_Is->max = tarval_from_long (mode_Ls, 0x7fffffff);
  mode_Is->null = tarval_from_long (mode_Is, 0);

  /* unsigned integer */
  mode_Iu->name = id_from_str ("Iu", 1);
  mode_Iu->fsigned = 0;
  mode_Iu->ffloat = 0;
  mode_Iu->ld_align = 4;
  mode_Iu->size = 4;
  mode_Iu->min = tarval_from_long (mode_Lu, 0x00000000);
  mode_Iu->max = tarval_from_long (mode_Lu, 0xffffffff);
  mode_Iu->null = tarval_from_long (mode_Iu, 0);

  /* signed long integer */
  mode_Ls->name = id_from_str ("Ls", 1);
  mode_Ls->fsigned = 1;
  mode_Ls->ffloat = 0;
  mode_Ls->ld_align = 8;
  mode_Ls->size = 8;
  mode_Ls->min = tarval_from_long (mode_Ls, 0x80000000);
  mode_Ls->max = tarval_from_long (mode_Ls, 0x7fffffff);
  mode_Ls->null = tarval_from_long (mode_Ls, 0);

  /* unsigned long integer */
  mode_Lu->name = id_from_str ("Lu", 1);
  mode_Lu->fsigned = 0;
  mode_Lu->ffloat = 0;
  mode_Lu->ld_align = 8;
  mode_Lu->size = 8;
  mode_Lu->min = tarval_from_long (mode_Lu, 0x00000000);
  mode_Lu->max = tarval_from_long (mode_Lu, 0xffffffff);
  mode_Lu->null = tarval_from_long (mode_Lu, 0);

  /* character */
  mode_C->name = id_from_str ("C", 1);
  mode_C->fsigned = 0;
  mode_C->ffloat = 0;
  mode_C->ld_align = 1;
  mode_C->size = 1;
  mode_C->min = tarval_from_long (mode_Ls, 0xffffff80);
  mode_C->max = tarval_from_long (mode_Ls, 0x0000007f);
  mode_C->null = tarval_from_long (mode_C, 0);

  /* unicode character */
  mode_C->name = id_from_str ("U", 1);
  mode_C->fsigned = 0;
  mode_C->ffloat = 0;
  mode_C->ld_align = 1;
  mode_C->size = 2;
  mode_C->min = tarval_from_long (mode_Ls, 0xffff8000);
  mode_C->max = tarval_from_long (mode_Ls, 0x00007fff);
  mode_C->null = tarval_from_long (mode_U, 0);

  /* boolean */
  mode_b->name = id_from_str ("b", 1);
  mode_b->fsigned = 0;
  mode_b->ffloat = 0;

  /* pointer */
  mode_P->name = id_from_str ("P", 1);
  mode_P->fsigned = 0;
  mode_P->ffloat = 0;
  mode_P->ld_align = 4;
  mode_P->size = 4;
  mode_P->min = tarval_from_long (mode_Lu, 0x00000000);
  mode_P->max = tarval_from_long (mode_Lu, 0xffffffff);
  mode_P->null = tarval_from_long (mode_P, 0);

  /* Execution */
  mode_X->name = id_from_str ("X", 1);
  mode_X->fsigned = 0;
  mode_X->ffloat = 0;

  /* Memory */
  mode_M->name = id_from_str ("M", 1);
  mode_M->fsigned = 0;
  mode_M->ffloat = 0;

  mode_BB->name = id_from_str ("BB", 1);
  mode_BB->fsigned = 0;
  mode_BB->ffloat = 0;
}

/* Functions for the direct access to all attributes od a ir_mode */

modecode
get_mode_modecode (ir_mode *mode)
{
  return mode->code;
}

/*
INLINE void
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
INLINE void
set_mode_ident (ir_mode *mode, ident *name)
{
  mode->name = name;
}
*/

INLINE const char *
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
INLINE void
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
INLINE void
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
INLINE void
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
INLINE void
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
INLINE void
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
INLINE voida
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
INLINE void
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
            irm_Is, irm_Iu, irm_Ls, irm_Lu, irm_C, irm_U, irm_p}
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

INLINE int
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


INLINE int
mode_is_int (ir_mode *mode)
{
   int res;
   modecode code;
   code = get_mode_modecode (mode);
   if ((code >= irm_Bs) &&  (code <= irm_Lu)) {
      res = 1;
    }
   else {
     res = 0;
   }
   return res;
}


INLINE int
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

INLINE int
mode_is_data (ir_mode *mode)
{
  int res;
  modecode code;
  code = get_mode_modecode (mode);
  if (mode_is_num (mode) ||
      code == irm_C || code == irm_U || code == irm_P) {
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
  if (mode_is_data (mode) || code == irm_b ) {
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

/* Returns true if sm can be converted to lm without loss. */
bool
smaller_mode(ir_mode *sm, ir_mode *lm) {
  if ((mode_is_int(sm) && mode_is_int(lm)) &&
      get_mode_modecode(sm) <= get_mode_modecode(lm))
    return true;
  if ((mode_is_float(sm) && mode_is_float(lm)) &&
      get_mode_modecode(sm) <= get_mode_modecode(lm))
    return true;
  return(false);
}
