/* -*- c -*- */
/*
 * Project:     libFIRM
 * File name:   ir/external/read.c
 * Purpose:     Read descriptions of external effects
 * Author:      Florian
 * Modified by: Boris Boesler
 * Created:     11.10.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _READ_H_
#define _READ_H_

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/encoding.h>

#include "type.h"
#include "entity.h"
#include "ident.h"

# define MY_ENCODING "ISO-8859-1"

# define CHECK(ptr,msg)     assert (ptr && msg)

# define NODE_NAME(n, m) (0 == xmlStrcmp (n->name, (const xmlChar*) #m))
# define CHECK_NAME(n, m) assert (0 == xmlStrcmp (n->name, (const xmlChar*) #m))

# define NEW(T)     (T*) malloc (sizeof (T))


/* first, the xml structures */

typedef struct type_str
{
  const ident *type_ident;
  const ident *id;              /* id for references */
  type *f_tp;                   /* firm type */
  struct type_str *prev;
} type_t;

typedef struct entity_str
{
  const ident *ent_ident;        /* name of entity */
  const ident *tp_ident;         /* name of type/class */
  const ident *id;               /* id for references */
  const ident *owner;            /* id of owner */
  entity *f_ent;                 /* firm entity */
  struct entity_str *prev;
} entity_t;

/* now the xml nodes */
typedef enum eff_node_kind {
  eff_arg,      // done
  eff_valref,   // eliminated
  eff_select,   // eliminated
  eff_load,     // done
  eff_store,    // done
  eff_alloc,    // done
  eff_call,     // done
  eff_unknown,  // done
  eff_join,     // TODO
  eff_raise,    // TODO
  eff_ret       // done
} eff_node_kind_t;


typedef struct arg_str
{
  const ident *type_ident;
  int num;
} arg_t;

typedef struct valref_str
{
  int dummy;
} valref_t;

typedef struct select_str
{
  entity_t *ent;
} select_t;

typedef struct load_str
{
  const ident *ptrrefid;     /* id of valref node enclosed in select, or -1 */
  entity_t *ent;
} load_t;

typedef struct store_str
{
  const ident *ptrrefid;     /* id of ptr valref node enclosed in select, or -1 */
  const ident *valrefid;     /* id of val valref node enclosed in select, or -1 */
  entity_t *ent;
} store_t;

typedef struct alloc_str
{
  const ident *tp_id;
} alloc_t;

typedef struct call_str
{
  const ident *valrefid;     /* id of enclosed valref node, or -1 */
  entity_t *ent;             /* called entity */
  int n_args;
  const ident **args;
} call_t;

typedef struct unknown_str
{
  int dummy;
} unknown_t;

typedef struct join_str
{
  int n_ins;
  const ident **ins;
} join_t;

typedef struct ret_str
{
  const ident *ret_id;
} ret_t;                     /* returned value, or NO_ID */

typedef struct raise_str
{
  const ident *valref;       /* what was that one for? */
  const ident *tp_id;
} raise_t;

/* dummy type for all other effects */
typedef struct eff_str
{
  eff_node_kind_t kind;
  const ident *id;           /* identifier to access this node */
  union {
    arg_t arg;
    valref_t valref;
    select_t select;
    load_t load;
    store_t store;
    alloc_t alloc;
    call_t call;
    unknown_t unknown;
    join_t join;
    ret_t ret;
    raise_t raise;
  } effect;
  ir_node *firmnode;
  struct eff_str *next; /* effects with values are stored in proc.values */
} eff_t;

typedef struct proc_str
{
  const ident *proc_ident;   /* name of procedure */
  const ident *typeid;
  int n_effs;
  eff_t **effs;
  struct proc_str *next;
  eff_t *values;             /* @@@ TODO hash set */
} proc_t;


typedef struct mod_str
{
  const ident *id;
  type_t *types;             /* types in module *//* @@@ TODO hash set */
  entity_t *entities;        /* entities in module *//* @@@ TODO hash set */
  proc_t *procs;             /* methods with effects */
  struct mod_str *next;      /* unused - only one module possible */
} module_t;


/*
  The public intyerface
*/
/** read the file and build the graphs */
void create_abstraction(const char *filename);


#endif /* defined _READ_H_ */

/*
  $Log$
  Revision 1.6  2004/10/22 13:13:27  boesler
  replaced char* by idents, minor fix in Firm codegen for call

  Revision 1.5  2004/10/21 15:31:55  boesler
  added lots of stuff:
  - build abstract syntax trees
  - build Firm graphs for many effects, still todos

  Revision 1.1  2004/10/11 09:31:06  liekweg
  First Import of XML reading procs --flo

*/
