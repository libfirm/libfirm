/* -*- c -*- */

/*
  Project:     libFIRM
  File name:   ir/ana/pto_name.h
  Purpose:     Names for abstract objects
  Author:      Florian
  Modified by:
  Created:     Sat Nov 13 19:35:27 CET 2004
  CVS-ID:      $Id$
  Copyright:   (c) 1999-2004 Universität Karlsruhe
  Licence:     This file is protected by the GPL -  GNU GENERAL PUBLIC LICENSE.
*/


# ifndef _PTO_NAME_
# define _PTO_NAME_

# include "pto_comp.h"          /* for pto_t */
# include "irnode.h"
# include "type.h"
# include "qset.h"

/* ===================================================
   Global Defines:
   =================================================== */

/* ===================================================
   Global Data Types:
   =================================================== */
typedef enum desc_kind_enum {
  none,
  object,
  array
} desc_kind_t;

/* abstract super class for all descriptors */
typedef struct desc_str
{
  int id;
  desc_kind_t kind;
  type *tp;
  ir_node *node;                /* allocation node */
  struct desc_str *prev;        /* linked list */
} desc_t;

/* object descriptor */
typedef struct obj_desc_str
{
  int id;
  desc_kind_t kind;
  type *tp;
  ir_node *node;                /* allocation node */
  struct desc_str *prev;        /* linked list */

  int n_fields;
  entity **fields;
  qset_t **values;
} obj_desc_t;

/* array descriptor */
typedef struct arr_desc_str
{
  int id;
  desc_kind_t kind;
  type *tp;
  ir_node *node;                /* allocation node */
  struct desc_str *prev;        /* linked list */

  qset_t *value;
} arr_desc_t;

/* ===================================================
   Global Prototypes:
   =================================================== */
/* Dump all names to a file of the given name */
void pto_dump (const char*);

/* Find the given descriptor's entry for the given entity */
qset_t *get_entry (desc_t*, entity*);

/* get a new descriptor for the given type at the given node */
desc_t *new_name (type*, ir_node*);

/* get a new descriptor for the given (presumably static) entity */
desc_t *new_ent_name (entity*);

/* Initialise the name module */
void pto_name_init (void);

/* Cleanup the name module */
void pto_name_cleanup (void);

/* ===================================================
   Global Variables:
   =================================================== */


# endif /* not defined _PTO_NAME_ */



/*
  $Log$
  Revision 1.3  2004/11/30 14:47:54  liekweg
  fix initialisation; do correct iteration

  Revision 1.2  2004/11/24 14:53:56  liekweg
  Bugfixes

  Revision 1.1  2004/11/18 16:37:34  liekweg
  rewritten


*/
