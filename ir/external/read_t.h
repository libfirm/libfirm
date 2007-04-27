/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief    Read descriptions of external effects, private header
 * @author   Florian, Boris Boesler
 * @date     11.10.2004
 * @version  $Id$
 */
#ifndef FIRM_EXTERNAL_READ_T_H
#define FIRM_EXTERNAL_READ_T_H

#include "firm_types.h"

/* first, the xml structures */

typedef struct type_str
{
  ident *type_ident;
  ident *id;                    /* id for references */
  ir_type *f_tp;                /* firm type */
  struct type_str *prev;
} type_t;

typedef struct entity_str
{
  ident *ent_ident;            /* name of entity */
  ident *tp_ident;             /* name of type/class */
  ident *id;                   /* id for references */
  ident *owner;                /* id of owner */
  ir_entity *f_ent;            /* firm entity */
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
  ident *type_ident;
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
  ident *ptrrefid;     /* id of valref node enclosed in select, or -1 */
  entity_t *ent;
} load_t;

typedef struct store_str
{
  ident *ptrrefid;     /* id of ptr valref node enclosed in select, or -1 */
  ident *valrefid;     /* id of val valref node enclosed in select, or -1 */
  entity_t *ent;
} store_t;

typedef struct alloc_str
{
  ident *tp_id;
} alloc_t;

typedef struct call_str
{
  ident *valrefid;     /* id of enclosed valref node, or -1 */
  entity_t *ent;       /* called entity */
  int n_args;
  ident **args;
} call_t;

typedef struct unknown_str
{
  int dummy;
} unknown_t;

typedef struct join_str
{
  int n_ins;
  ident **ins;
} join_t;

typedef struct ret_str
{
  ident *ret_id;
} ret_t;                     /* returned value, or NO_ID */

typedef struct raise_str
{
  ident *valref;       /* what was that one for? */
  ident *tp_id;
} raise_t;

/* dummy type for all other effects */
typedef struct eff_str
{
  eff_node_kind_t kind;
  ident *id;           /* identifier to access this node */
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
  ident *proc_ident;         /* name of procedure */
  ident *ownerid;
  int n_effs;
  eff_t **effs;
  struct proc_str *next;
  eff_t *values;             /* @@@ TODO hash set */
} proc_t;


typedef struct mod_str
{
  ident *id;
  type_t *types;             /* types in module *//* @@@ TODO hash set */
  entity_t *entities;        /* entities in module *//* @@@ TODO hash set */
  proc_t *procs;             /* methods with effects */
  struct mod_str *next;      /* unused - only one module possible */
} module_t;

#endif

/*
  $Log$
  Revision 1.5  2007/02/02 12:38:35  matze
  entity is ir_entity now

  Revision 1.4  2006/12/15 12:37:40  matze
  fix warnings

  Revision 1.3  2006/06/09 11:26:35  firm
  renamed type to ir_type

  Revision 1.2  2004/12/10 15:14:16  beck
  Removed unused header files
  move xml macros to read.c, freeing the header from libxml depency

  Revision 1.1  2004/10/25 13:52:24  boesler
  seperated read.h (public interface) and read_t.h (types)

  Revision 1.6  2004/10/22 13:13:27  boesler
  replaced char* by idents, minor fix in Firm codegen for call

  Revision 1.5  2004/10/21 15:31:55  boesler
  added lots of stuff:
  - build abstract syntax trees
  - build Firm graphs for many effects, still todos

  Revision 1.1  2004/10/11 09:31:06  liekweg
  First Import of XML reading procs --flo

*/
