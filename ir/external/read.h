/* -*- c -*- */

/*
 * Project:     libFIRM
 * File name:   ir/external/read.c
 * Purpose:     Read descriptions of external effects
 * Author:      Florian
 * Modified by:
 * Created:     11.10.2004
 * CVS-ID:      $$
 * Copyright:   (c) 1999-2004 Universität Karlsruhe
 * Licence:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

# ifndef _READ_H_
# define _READ_H_

# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <assert.h>

# include <libxml/xmlmemory.h>
# include <libxml/parser.h>
# include <libxml/encoding.h>

# include "type.h"
# include "entity.h"

/* we need strdup */
# ifndef _GNU_SOURCE
#  define _GNU_SOURCE
# endif /* ! defined _GNU_SOURCE */

# define MY_ENCODING "ISO-8859-1"

/* quick hack until we get a cmd line flag or something: just add
   '-DEXTERN_VERBOSE' to 'CFLAGS' */
# ifndef EXTERN_VERBOSE
#  define EXTERN_VERBOSE 1
# else
#  define EXTERN_VERBOSE 0
# endif /* defined EXTERN_VERBOSE */

# define CHECK(ptr,msg)     assert (ptr && msg)

# define NODE_NAME(n, m) (0 == xmlStrcmp (n->name, (const xmlChar*) #m))
# define CHECK_NAME(n, m) assert (0 == xmlStrcmp (n->name, (const xmlChar*) #m))

# define NEW(T)     (T*) malloc (sizeof (T))

/* first, the xml structures */

typedef struct type_str
{
  const char *name;
  int id;
  entity *f_tp;                 /* firm type */
  struct type_str *prev;        /* linked list */
} type_t;

typedef struct entity_str
{
  const char *name;
  const char *tp_name;
  int id;
  type *f_ent;                  /* firm entity */
  struct entity_str *prev;      /* linked list */
} entity_t;

/* now the xml nodes */
typedef enum eff_node_kind {
  eff_arg,
  eff_valref,
  eff_select,
  eff_load,
  eff_store,
  eff_alloc,
  eff_call,
  eff_unknown,
  eff_join,
  eff_raise,
  eff_ret
} eff_node_kind_t;

/* dummy type for all other effects */
typedef struct eff_str
{
  eff_node_kind_t kind;
} eff_t;

typedef struct effs_str
{
  char *procname;
  int n_effs;
  eff_t **effs;
  struct effs_str *next;
} effs_t;

typedef struct arg_str
{
  eff_node_kind_t kind;
  int id;
  int num;
} arg_t;

typedef struct valref_str
{
  eff_node_kind_t kind;
  int refid;
} valref_t;

typedef struct select_str
{
  eff_node_kind_t kind;
  int valrefid;                 /* id of enclosed valref node, or -1 */
  entity_t *ent;
} select_t;

typedef struct load_str
{
  eff_node_kind_t kind;
  int id;
  int ptrrefid;                 /* id of valref node enclosed in select, or -1 */
  entity_t *ent;
} load_t;

typedef struct store_str
{
  eff_node_kind_t kind;
  int ptrrefid;                 /* id of ptr valref node enclosed in select, or -1 */
  int valrefid;                 /* id of val valref node enclosed in select, or -1 */
  entity_t *ent;
} store_t;

typedef struct alloc_str
{
  eff_node_kind_t kind;
  int id;
  int tp_id;
} alloc_t;

typedef struct call_str
{
  eff_node_kind_t kind;
  int id;
  int valrefid;                 /* id of enclosed valref node, or -1 */
  entity_t *ent;                /* called entity */
  int n_args;
  int *args;
} call_t;

typedef struct unknown_str
{
  eff_node_kind_t kind;
  int id;
} unknown_t;

typedef struct join_str
{
  eff_node_kind_t kind;
  int id;
  int n_ins;
  int *ins;
} join_t;

typedef struct ret_str
{
  eff_node_kind_t kind;
  int ret_id;                   /* returned value, or -1 */
} ret_t;

typedef struct raise_str
{
  eff_node_kind_t kind;
  /* struct eff_str *next; */
  int valref;                   /* what was that one for? */
  int tp_id;
} raise_t;

/*
  The public interface
*/

/** get the type entry with the given name */
type_t *getTypeByName (const char*);

/** get the type entry with the given Id */
type_t *getTypeById (const int);

/** get the entity entry that has the given names */
entity_t *getEntityByNames (const char*, const char*);

/** get the entity entry that has the given Id */
entity_t *getEntityById (const int);

/** get the effect entry for the given name */
effs_t *getEffectByName (const char*);

/** initialise the data structures */
void extern_init (void);

/** read in the file of the given name */
void extern_read (const char*);

/** clean up our mess */
void extern_cleanup (void);

# endif /* defined _READ_H_ */

/*
  $Log$
  Revision 1.3  2004/10/13 08:39:59  liekweg
  need strdup

  Revision 1.2  2004/10/11 15:56:09  liekweg
  Cleanup, comments ...
  Added init func --flo

  Revision 1.1  2004/10/11 09:31:06  liekweg
  First Import of XML reading procs --flo

*/
