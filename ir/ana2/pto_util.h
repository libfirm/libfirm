/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/ana2/pto_util.c
 * Purpose:     Pto Utilities
 * Author:      Florian
 * Modified by:
 * Created:     Mon 18 Oct 2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# ifndef _PTO_UTIL_H_
# define _PTO_UTIL_H_

# include "irgraph.h"
# include "qset.h"

# define N_INITIAL_OBJS         5

typedef enum obj_kind_enum {
  obj_kind_obj,
  obj_kind_array,
} obj_kind_t;

typedef struct obj_desc_str
{
  obj_kind_t kind;              /* written obj_kind_obj or obj_kind_array */
  struct obj_desc_str *next;    /* link all descrs into a linked list */
# ifdef PTO_DUMMY
  int is_dummy;                 /* allow to filter out dummy objects */
# endif /* defined PTO_DUMMY */
  type *tp;                     /* type of described object */
} obj_desc_t;

typedef struct obj_obj_desc_str
{
  obj_kind_t kind;              /* always written obj_kind_obj */
  struct obj_desc_str *next;    /* link all descrs into a linked list */
# ifdef PTO_DUMMY
  int is_dummy;                 /* allow to filter out dummy objects */
# endif /* defined PTO_DUMMY */
  type *tp;                     /* type of described object */
  entity **fields;              /* fields of described object */
  int n_fields;                 /* number of fields */
  qset_t **vals;                /* qsets of values of the fields */
} obj_obj_desc_t;

typedef struct obj_arr_desc_str
{
  obj_kind_t kind;              /* always written obj_kind_array */
  struct obj_desc_str *next;    /* link all descrs into a linked list */
# ifdef PTO_DUMMY
  int is_dummy;                 /* allow to filter out dummy objects */
# endif /* defined PTO_DUMMY */
  type *tp;                     /* (array) type of described object */
  qset_t *val;                  /* all values of 'the' 'field' */
} obj_arr_desc_t;

typedef struct pto_str
{
  void *kind;                   /* always written to &pto_id */
  ir_node *node;                /* Rubbish: node for which this pto_t was constructed */
  qset_t *objs;                 /* qset of obj_desc_t* */
} pto_t;


/* Get the entity of a ptr */
entity *get_ptr_ent (ir_node*);

/* Ctors for the pto types */
obj_desc_t *obj_desc_new (type*);

# ifdef PTO_DUMMY
/* Mark an obj desc as a dummy */
void obj_desc_set_dummy (obj_desc_t*);

/* Say whether an obj desc is a dummy */
int obj_desc_is_dummy (obj_desc_t*);
# endif /* defined PTO_DUMMY */


/* Deallocate an obj desc */
void obj_desc_delete (obj_desc_t*);

/* List all obj descs that have been created.  If tp is given non-NULL,
  only descs for this type are listed, else all types are listed. */
void obj_desc_list_all (type*);

/* Create a new pto value containing a name of the given type. */
pto_t *pto_new_name (ir_node*, type*);

/* Create a new pto value containing no names. */
pto_t *pto_new_empty (ir_node*);

/* Deallocate a pto */
void pto_delete (pto_t*);

/* Sanity checking on a pto_t */
void check_pto (pto_t*);

/*   Add the given name to the given pto. */
void pto_add_name (pto_t*, obj_desc_t*);

/* Add all the given names to the given pto. */
void pto_add_all_names (pto_t*, qset_t*);


/* Find the arguments of a graph. For a method that has n args, the
  result array has 'n+1' entries, the last of which is written NULL.
  If an argument is not used, it is also written NULL in this array. */
ir_node **find_irg_args (ir_graph*);
/* int is_field (entity*); */


/* Perform a lookup of the contents of the given field in the given pto */
qset_t *pto_lookup (obj_desc_t*, entity*);

# endif /* not defined _PTO_UTIL_H_ */


/*
  $Log$
  Revision 1.2  2004/11/04 14:58:38  liekweg
  expanded pto, added initialisation, added debugging printing

  Revision 1.1  2004/10/22 15:10:51  liekweg
  moved utils to pto_util


 */
