/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/typalise.h
 * Purpose:     Compute rough approximations of pointer types
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# ifndef _TYPALISE_H_
# define _TYPALISE_H_

# include "lset.h"

# include "type.h"

/*
  Data Types and Structures
*/
typedef enum typalise_kind_enum {
  type_invalid = 0,             /* invalid (only set at deletion time) */
  type_exact = 1,               /* this and only this type (res.type) */
  type_types = 2,               /* these types (res.types) */
  type_type  = 3                /* this type and sub types (res.type) */
} typalise_kind;

typedef struct typalise
{
  typalise_kind kind;
  union
  {
    type *type;                 /* for kind == kind_exact and kind == kind_type */
    lset_t *types;              /* for kind == kind_types */
  } res;
  int id;
} typalise_t;

/*
  Protos
*/
/**
   Given a set of graphs and a typalise_t,  return the method (s) in
   the set that are supported by the typalise_t.  Also, deallocates
   the given set.
*/
lset_t *filter_for_ta (lset_t*, typalise_t*);

/**
   For the given ptr, do a quick check about what (class) types may be
   brought along on it.
*/
typalise_t *typalise (ir_node*);

# endif /* not defined _TYPALISE_H_ */


/*
  $Log$
  Revision 1.1  2004/10/21 11:09:37  liekweg
  Moved memwalk stuf into irmemwalk
  Moved lset stuff into lset
  Moved typalise stuff into typalise


 */
