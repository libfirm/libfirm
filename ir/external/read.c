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

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

/* get prototype for alloca somehow */
#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

# include <string.h>

#include "read_t.h"
#include "read.h"
#include "irprog.h"
#include "irgraph.h"
#include "pseudo_irg.h"
#include "ircons.h"
#include "irmode.h"
#include "irdump.h"
#include "irvrfy.h"
#include "type.h"
#include "tv.h"

#define VERBOSE_PRINTING 0

#if VERBOSE_PRINTING
# define VERBOSE_PRINT(s) fprintf s
#else
# define VERBOSE_PRINT(s)
#endif

#define NO_ID NULL

static type_t *types = NULL;
static entity_t *entities = NULL;
static proc_t *procs = NULL;
static module_t *modules = NULL;

/* @@@ HACK */
static module_t *current_module = NULL;

#if VERBOSE_PRINTING
/* this is only used inside a VERBOSE_PRINT() call */
static char *effect_string[] = {
  "arg",
  "valref",
  "select",
  "load",
  "store",
  "alloc",
  "call",
  "unknown",
  "join",
  "raise",
  "ret"
};
#endif /* defined VERBOSE_PRINTING */

static const ident*
getNodeModuleIdent (xmlNodePtr node)
{
  const char *mod_str = (const char*) xmlGetProp (node, BAD_CAST "module");

  if (NULL == mod_str) {
    return (NULL);
  } else {
    const ident *res = new_id_from_str (mod_str);
    return (res);
  }
}

static const char*
getNodeProcName (xmlNodePtr node)
{
  const char *proc_str = (const char*) xmlGetProp (node, BAD_CAST "procname");
  assert (proc_str);
  return (proc_str);
}

# ifdef NEEDED
static char*
getNodeClassName (xmlNodePtr node)
{
  char *proc_str = (char*) xmlGetProp (node, BAD_CAST "class");
  assert (proc_str);
  return ( (proc_str));
}
# endif /* defined NEEDED */

static const char*
getNodeId (xmlNodePtr node)
{
  const char *id_str = (const char*) xmlGetProp (node, BAD_CAST "id");
  assert (id_str);
  return (id_str);
}

static const char *
getNodeRefId (xmlNodePtr node)
{
  const char *refid_str = (char*) xmlGetProp (node, BAD_CAST "refid");
  assert (refid_str);
  return ((refid_str));
}

static const char*
getNodeTypeId (xmlNodePtr node)
{
  const char *type_str = (char*) xmlGetProp (node, BAD_CAST "type");
  assert (type_str);
  return ((type_str));
}

static const char
*getNodeTypeStr (xmlNodePtr node)
{
  const char *type_str = (char*) xmlGetProp (node, BAD_CAST "type");
  assert (type_str);
  return (type_str);
}

static const char*
getNodeOwnerStr (xmlNodePtr node)
{
  const char *owner_str = (char*) xmlGetProp (node, BAD_CAST "owner");
  assert (owner_str);
  return (owner_str);
}

static const char
*getNodeEntityStr (xmlNodePtr node)
{
  const char *ent_str = (char*) xmlGetProp (node, BAD_CAST "entity");
  assert (ent_str);

  return (ent_str);
}


/*
  was Public Interface
*/
# ifdef NEEDED
static
type_t *getTypeByIdent (const ident *id)
{
  type_t *curr = types; // @@@ TODO module -> types

  while (NULL != curr) {
    if (id == curr -> type_ident) {
      return (curr);
    }
    curr = curr->prev;
  }

  return (NULL);
}
# endif /* defined NEEDED */

# ifdef NEEDED
static
type_t *getTypeById (const ident *id)
{
  type_t *curr = types; // which ones?

  while (NULL != curr) {
    if (id == curr -> id) {
      return (curr);
    }
    curr = curr->prev;
  }

  return (NULL);
}
# endif /* defined NEEDED */

# ifdef NEEDED
static
entity_t *getEntityByIdents (const ident *name, const ident *tp_ident)
{
  entity_t *curr = entities; // TODO module -> entities

  while (NULL != curr) {
    if ((name == curr -> ent_ident)
    && (tp_ident == curr -> tp_ident)) {
      return (curr);
    }
    curr = curr->prev;
  }

  return (NULL);
}
# endif /* defined NEEDED */

static
entity_t *getEntityById (const ident *id)
{
  entity_t *curr = entities;

  while (NULL != curr) {
    if (id == curr -> id) {
      return (curr);
    }
    curr = curr->prev;
  }

  return (NULL);
}

# ifdef NEEDED
static
proc_t *getEffectByName (const ident *proc_ident)
{
  proc_t *curr_effs = procs;

  while (NULL != curr_effs) {
    if (proc_ident == curr_effs -> proc_ident) {
      return (curr_effs);
    }
    curr_effs = curr_effs->next;
  }

  return (NULL);
}
# endif /* defined NEEDED */

static
xmlNodePtr get_any_valid_child(xmlNodePtr elem)
{
  xmlNodePtr child;

  assert(elem && "no element");
  child = elem -> xmlChildrenNode;
  while(child && (NODE_NAME (child, comment))) {
    child = child -> next;
  }
  return(child);
}

static
xmlNodePtr get_valid_child(xmlNodePtr elem)
{
  xmlNodePtr child;

  child = get_any_valid_child(elem);
  assert(child && "lost child in deep black forest");
  return(child);
}

/*
 * parse XML structure and construct an additional structure
 */
static eff_t *
parseArg (xmlDocPtr doc, xmlNodePtr argelm)
{
  const char *id;
  const char *typeid;
  int num;
  char *num_str;
  eff_t *arg;

  CHECK_NAME (argelm, arg);
  VERBOSE_PRINT ((stdout, "arg node \t0x%08x\n", (int) argelm));

  id = getNodeId (argelm);
  VERBOSE_PRINT ((stdout, "arg->id = \"%s\"\n", id));
  num_str = (char*) xmlGetProp (argelm, BAD_CAST "number");
  num = atoi (num_str);
  VERBOSE_PRINT ((stdout, "arg->no = \"%d\"\n", num));

  typeid = getNodeTypeStr (argelm);

  arg = NEW (eff_t);
  arg -> kind = eff_arg;
  arg -> id = new_id_from_str(id);
  arg -> effect.arg.num = num;
  arg -> effect.arg.type_ident = new_id_from_str(typeid);

  return (arg);
}

static eff_t*
parseValref (xmlDocPtr doc, xmlNodePtr valelm)
{
  const char *ref_id;
  eff_t *valref;

  CHECK_NAME (valelm, valref);
  VERBOSE_PRINT ((stdout, "valref node \t0x%08x\n", (int) valelm));

  ref_id = getNodeRefId (valelm);
  VERBOSE_PRINT ((stdout, "val->refid = \"%s\"\n", ref_id));

  valref = NEW (eff_t);
  valref->kind = eff_valref;
  valref-> id = new_id_from_str(ref_id);

  return (valref);
}

static eff_t*
parseSelect (xmlDocPtr doc, xmlNodePtr selelm)
{
  const ident *entity_id = new_id_from_str(getNodeEntityStr (selelm));
  entity_t *ent;
  xmlNodePtr child;
  eff_t *valref = NULL;
  eff_t *sel = NEW (eff_t);
  sel->kind = eff_select;

  CHECK_NAME (selelm, select);
  VERBOSE_PRINT ((stdout, "select node \t0x%08x\n", (int) selelm));

  ent = getEntityById (entity_id);
  assert(ent && "entity not found");
  VERBOSE_PRINT ((stdout, "select entity %s\n", get_id_str(ent -> ent_ident)));

  child = selelm->xmlChildrenNode;

  if (child) {
    valref = parseValref (doc, child);
  }

  sel-> id = valref ? valref-> id : NO_ID;
  sel-> effect.select.ent = ent;

  if (valref) {
    free (valref);
  }

  return (sel);
}

static eff_t*
parseLoad (xmlDocPtr doc, xmlNodePtr loadelm)
{
  const ident *id;
  xmlNodePtr child;
  eff_t *sel;
  eff_t *load = NEW (eff_t);
  load->kind = eff_load;

  CHECK_NAME (loadelm, load);
  VERBOSE_PRINT ((stdout, "load node \t0x%08x\n", (int) loadelm));
  id = new_id_from_str(getNodeId (loadelm));

  child = get_valid_child(loadelm);
  if(NODE_NAME (child, select)) {
    sel = parseSelect (doc, child);
    load-> effect.load.ent = sel-> effect.select.ent;
    VERBOSE_PRINT ((stdout, "load entity \t%s\n",
            get_id_str(load -> effect.load.ent -> ent_ident)));
  }
  else {
    sel = parseValref (doc, child);
    load-> effect.load.ent = NULL;
  }

  load-> id = id;
  load-> effect.load.ptrrefid = sel-> id;

  free (sel);

  return (load);
}

static eff_t*
parseStore (xmlDocPtr doc, xmlNodePtr storeelm)
{
  xmlNodePtr child;
  eff_t *sel;
  eff_t *valref;
  eff_t *store = NEW (eff_t);
  store->kind = eff_store;

  CHECK_NAME (storeelm, store);
  VERBOSE_PRINT ((stdout, "store node \t0x%08x\n", (int) storeelm));

  child = get_valid_child(storeelm);
  if(NODE_NAME (child, select)) {
    sel = parseSelect (doc, child);
    store-> effect.store.ent = sel-> effect.select.ent;
  }
  else {
    sel = parseValref (doc, child);
    store-> effect.store.ent = NULL;
  }

  child = child->next;
  valref = parseValref (doc, child);

  store-> effect.store.ptrrefid = sel-> id;
  store-> effect.store.valrefid = valref-> id;

  free (sel);
  free (valref);

  return (store);
}

static eff_t*
parseAlloc (xmlDocPtr doc, xmlNodePtr allocelm)
{
  const ident *id;
  const ident *type_id;
  eff_t *alloc = NEW (eff_t); /* ...! */
  alloc->kind = eff_alloc;

  CHECK_NAME (allocelm, alloc);
  VERBOSE_PRINT ((stdout, "alloc node \t0x%08x\n", (int) allocelm));
  id = new_id_from_str(getNodeId (allocelm));
  VERBOSE_PRINT ((stdout, "alloc->id = \"%s\"\n", get_id_str(id)));
  type_id = new_id_from_str(getNodeTypeId (allocelm));
  VERBOSE_PRINT ((stdout, "alloc->type_id = \"%s\"\n", get_id_str(type_id)));

  alloc-> id = id;
  alloc-> effect.alloc.tp_id = type_id;

  return (alloc);
}

static eff_t*
parseCall (xmlDocPtr doc, xmlNodePtr callelm)
{
  const ident *id;
  xmlNodePtr child;
  eff_t *sel;
  xmlNodePtr arg;
  int n_args;
  eff_t *call = NEW (eff_t);
  call->kind = eff_call;

  CHECK_NAME (callelm, call);
  VERBOSE_PRINT ((stdout, "call node \t0x%08x\n", (int) callelm));
  id = new_id_from_str(getNodeId (callelm));
  VERBOSE_PRINT ((stdout, "call->id = \"%s\"\n", get_id_str(id)));

  child = get_valid_child(callelm);
  if(NODE_NAME (child, select)) {
    sel = parseSelect (doc, child);
    call-> effect.call.ent = sel-> effect.select.ent;
  }
  else {
    sel = parseValref (doc, child);
    call-> effect.call.ent = NULL;
  }

  arg = child = child->next;
  n_args = 0;

  while (NULL != child) {
    n_args ++;
    child = child->next;
  }

  call-> id = id;
  call-> effect.call.valrefid = sel-> id;
  call-> effect.call.n_args = n_args;
  call-> effect.call.args = NULL;

  free (sel);

  if (0 != n_args) {
    const ident **args = (const ident**) malloc(n_args * sizeof(const ident*));
    int i = 0;

    while (NULL != arg) {
      eff_t *valref = parseValref (doc, arg);
      args [i ++] = valref-> id;
      free (valref);
      arg = arg->next;
    }

    call-> effect.call.args = args;
  }

  return (call);
}

static eff_t*
parseJoin (xmlDocPtr doc, xmlNodePtr joinelm)
{
  const ident *id;
  int n_ins;
  const ident **ins;
  int i;
  xmlNodePtr child;
  eff_t *join = NEW (eff_t);
  join->kind = eff_join;

  CHECK_NAME (joinelm, join);
  VERBOSE_PRINT ((stdout, "join node \t0x%08x\n", (int) joinelm));
  id = new_id_from_str(getNodeId (joinelm));
  VERBOSE_PRINT ((stdout, "join->id = \"%s\"\n", get_id_str(id)));

  child = get_valid_child(joinelm);
  n_ins = 0;

  while (NULL != child) {
    n_ins ++;
    child = child->next;
  }

  ins = (const ident **) malloc (n_ins * sizeof (const ident *) );
  i = 0;
  child = get_valid_child(joinelm);

  while (NULL != child) {
    eff_t *valref = parseValref (doc, child);
    ins [i ++] = valref-> id;
    free(valref);
    child = child->next;
  }

  join-> id = id;
  join-> effect.join.n_ins = n_ins;
  join-> effect.join.ins = ins;

  return (join);
}

static eff_t*
parseUnknown (xmlDocPtr doc, xmlNodePtr unknownelm)
{
  const ident *id;
  eff_t *unknown = NEW (eff_t);
  unknown->kind = eff_unknown;

  CHECK_NAME (unknownelm, unknown);
  VERBOSE_PRINT ((stdout, "unknown node \t0x%08x\n", (int) unknownelm));
  id = new_id_from_str(getNodeId (unknownelm));
  unknown-> id = id;

  return (unknown);
}

static eff_t*
parseReturn (xmlDocPtr doc, xmlNodePtr retelm)
{
  xmlNodePtr child;
  eff_t *ret = NEW (eff_t);
  ret->kind = eff_ret;

  CHECK_NAME (retelm, ret);
  VERBOSE_PRINT ((stdout, "ret node \t0x%08x\n", (int) retelm));

  child = get_any_valid_child(retelm);

  if (child) {
    eff_t *valref = parseValref (doc, child);
    ret-> effect.ret.ret_id = valref-> id;
    free (valref);
  } else {
    ret-> effect.ret.ret_id = NO_ID;
  }

  return (ret);
}

static eff_t*
parseRaise (xmlDocPtr doc, xmlNodePtr raiseelm)
{
  const char *tp_id;
  eff_t *valref;
  xmlNodePtr child;
  eff_t *raise = NEW (eff_t);
  raise->kind = eff_raise;

  CHECK_NAME (raiseelm, raise);
  VERBOSE_PRINT ((stdout, "raise node \t0x%08x\n", (int) raiseelm));
  tp_id = getNodeTypeId (raiseelm);
  VERBOSE_PRINT ((stdout, "raise->type = \"%s\"\n", tp_id));
  child = get_valid_child(raiseelm);

  assert (NULL != child);

  valref = parseValref (doc, child);
  raise-> effect.raise.valref = valref-> id;
  raise-> effect.raise.tp_id = new_id_from_str(tp_id);
  free (valref);

  return (raise);
}


/*
  Types and Entities
*/

/** parse a type node and insert it into the list */
static void
parseType (xmlDocPtr doc, xmlNodePtr typeelm)
{
  type_t *type;
  const char *tp_id = getNodeId (typeelm);
  VERBOSE_PRINT ((stdout, "type node \t0x%08x (%s)\n", (int) typeelm, tp_id));
  VERBOSE_PRINT ((stdout, "type = \"%s\"\n", getNodeTypeStr (typeelm)));

  type = (type_t*) malloc (sizeof (type_t));
  type -> type_ident = new_id_from_str(getNodeTypeStr (typeelm));
  type -> id         = new_id_from_str(tp_id);

  type->prev = types;
  types = type;
}

/** parse an entity node and insert it into the list */
static void
parseEntity (xmlDocPtr doc, xmlNodePtr entelm)
{
  entity_t *ent = NEW (entity_t);

  /* parse it */
  const char *ent_id = getNodeId (entelm);
  /* fprintf (stdout, "entity node \t0x%08x (%d)\n", (int) entelm, ent_id); */
  VERBOSE_PRINT ((stdout, "ent  = \"%s.%s\"\n",
          getNodeTypeStr (entelm),
          getNodeEntityStr (entelm)));


  ent -> ent_ident = new_id_from_str (getNodeEntityStr (entelm));
  ent -> tp_ident  = new_id_from_str (getNodeTypeStr   (entelm));
  ent -> owner     = new_id_from_str (getNodeOwnerStr  (entelm));
  ent -> id = new_id_from_str(ent_id);

  ent->prev = entities;
  entities = ent;
}

/** parse any effect, and turn it into an eff_t (TODO) */
static void
parseEffect (xmlDocPtr doc, xmlNodePtr effelm)
{
  xmlNodePtr cur;
  const char *procname = getNodeProcName (effelm);
  const char *ownerid = getNodeOwnerStr (effelm);
  proc_t *curr_effs = NULL;
  int i = 0;
  int n_effs = 0;

  VERBOSE_PRINT ((stdout, "effect for method \"%s\"\n", procname));

  cur = effelm -> xmlChildrenNode;
  while (NULL != cur) {
    n_effs ++;
    cur = cur->next;
  }
  VERBOSE_PRINT ((stdout, "has %d effects\n", n_effs));

  curr_effs = NEW (proc_t);
  curr_effs -> proc_ident = new_id_from_str(procname);
  curr_effs -> ownerid = new_id_from_str(ownerid);
  curr_effs->effs = (eff_t**) malloc (n_effs * sizeof (eff_t*));

  cur = effelm -> xmlChildrenNode;
  while (NULL != cur) {
    eff_t *eff = NULL;

    if (NODE_NAME (cur, arg)) {
      eff = (eff_t*) parseArg (doc, cur);
    } else if (NODE_NAME (cur, load)) {
      eff = (eff_t*) parseLoad (doc, cur);
    } else if (NODE_NAME (cur, store)) {
      eff = (eff_t*) parseStore (doc, cur);
    } else if (NODE_NAME (cur, alloc)) {
      eff = (eff_t*) parseAlloc (doc, cur);
    } else if (NODE_NAME (cur, call)) {
      eff = (eff_t*) parseCall (doc, cur);
    } else if (NODE_NAME (cur, join)) {
      eff = (eff_t*) parseJoin (doc, cur);
    } else if (NODE_NAME (cur, unknown)) {
      eff = (eff_t*) parseUnknown (doc, cur);
    } else if (NODE_NAME (cur, ret)) {
      eff = (eff_t*) parseReturn (doc, cur);
    } else if (NODE_NAME (cur, raise)) {
      eff = (eff_t*) parseRaise (doc, cur);
    } else if (NODE_NAME (cur, comment)) {
      /* comment */
      --n_effs;
    } else {
      fprintf (stderr, "wrong element \"%s\"\n", BAD_CAST cur->name);
      exit (EXIT_FAILURE);
    }
    if(eff) {
      VERBOSE_PRINT ((stdout, "effect %p@%d\n", (void*)eff, i));
      curr_effs -> effs[i++] = eff;
    }
    cur = cur -> next;
  }
  assert((i == n_effs) && "incorrect number of effects");
  curr_effs -> n_effs = n_effs;
  curr_effs -> next = procs;
  procs = curr_effs;
}


static
void read_extern (const char *filename)
{
  /* xmlNsPtr ns = NULL; */           /* no namespace for us */
  xmlDocPtr doc;                /* whole document */
  xmlNodePtr cur;               /* current node */
  ident *mod_id;
  module_t *module;

  /* i've got no idea what the VERSION cast is all about. voodoo
     programming at its finest. */
  LIBXML_TEST_VERSION xmlKeepBlanksDefault (0);
  VERBOSE_PRINT((stdout, "read file %s\n", filename));
  doc = xmlParseFile (filename);
  CHECK (doc, "xmlParseFile");

  cur = xmlDocGetRootElement (doc);
  CHECK (cur, "xmlDocGetRootElement");

  if (! NODE_NAME (cur, effects)) {
    fprintf (stderr,"root node \"%s\" != \"effects\"\n", BAD_CAST cur->name);
    xmlFreeDoc (doc);
    exit (EXIT_FAILURE);
  }

  mod_id = getNodeModuleIdent (cur);
  if (NULL != mod_id) {
    VERBOSE_PRINT ((stdout, "effects for \"%s\"\n",
            get_id_str(mod_id)));
  }
  else {
    VERBOSE_PRINT ((stdout, "effects \t0x%08x\n", (int) cur));
  }

  /* parse entities */
  cur = cur->xmlChildrenNode;
  while (cur != NULL) {
    if (NODE_NAME (cur, type)) {
      parseType (doc, cur);
    } else if (NODE_NAME (cur, entity)) {
      parseEntity (doc, cur);
    } else if (NODE_NAME (cur, effect)) {
      parseEffect (doc, cur);
    } else if ((NODE_NAME (cur, comment))) {
      /* comment */
    } else {
      fprintf (stderr, "wrong element \"%s\"\n", BAD_CAST cur->name);
      exit (EXIT_FAILURE);
    }
    cur = cur->next;
  }

  module = NEW(module_t);
  module -> id = mod_id;
  module -> types = types;
  module -> entities = entities;
  module -> procs = procs;

  types = NULL;
  entities = NULL;
  procs = NULL;

  module -> next = modules;
  modules = module;
}

/********************************************************************/

/*
 * free additional structure
 */
static
void freeArg (eff_t *arg)
{
  VERBOSE_PRINT ((stdout, "free arg node \t0x%08x\n", (int) arg));
  free(arg);
  return;
}

static
void freeValref (eff_t *valref)
{
  VERBOSE_PRINT ((stdout, "free valref node \t0x%08x\n", (int) valref));
  free(valref);
  return;
}

static
void freeSelect (eff_t *sel)
{
  VERBOSE_PRINT ((stdout, "free select node \t0x%08x\n", (int) sel));
  free(sel);
  return;
}

static
void freeLoad (eff_t *load)
{
  VERBOSE_PRINT ((stdout, "free load node \t0x%08x\n", (int) load));
  free (load);
  return;
}

static
void freeStore (eff_t *store)
{
  VERBOSE_PRINT ((stdout, "free store node \t0x%08x\n", (int) store));
  free (store);
  return;
}

static
void freeAlloc (eff_t *alloc)
{
  VERBOSE_PRINT ((stdout, "free alloc node \t0x%08x\n", (int) alloc));
  free(alloc);
  return;
}

static
void freeCall (eff_t *call)
{
  VERBOSE_PRINT ((stdout, "free call node \t0x%08x\n", (int) call));
  free(call -> effect.call.args);
  free(call);
  return;
}

static
void freeJoin (eff_t *join)
{
  VERBOSE_PRINT ((stdout, "free join node \t0x%08x\n", (int) join));
  free(join -> effect.join.ins);
  free(join);
  return;
}

static
void freeUnknown (eff_t *unknown)
{
  VERBOSE_PRINT ((stdout, "free unknown node \t0x%08x\n", (int) unknown));
  free(unknown);
  return;
}

static
void freeReturn (eff_t *ret)
{
  VERBOSE_PRINT ((stdout, "free ret node \t0x%08x\n", (int) ret));
  free(ret);
  return;
}

static
void freeRaise (eff_t *raise)
{
  VERBOSE_PRINT ((stdout, "free raise node \t0x%08x\n", (int) raise));
  free (raise);
  return;
}


static
void freeProcEffs(proc_t *proc)
{
  int i;
  int num;

  VERBOSE_PRINT ((stdout, "free effect for method \"%s\"\n",
          get_id_str(proc -> proc_ident)));

  num = proc -> n_effs;
  for(i = 0; i < num; i++) {
    switch(proc -> effs[i] -> kind) {
    case eff_arg:
      freeArg(proc -> effs[i]);
      break;
    case eff_valref:
      freeValref(proc -> effs[i]);
      break;
    case eff_select:
      freeSelect(proc -> effs[i]);
      break;
    case eff_load:
      freeLoad(proc -> effs[i]);
      break;
    case eff_store:
      freeStore(proc -> effs[i]);
      break;
    case eff_alloc:
      freeAlloc(proc -> effs[i]);
      break;
    case eff_call:
      freeCall(proc -> effs[i]);
      break;
    case eff_unknown:
      freeUnknown(proc -> effs[i]);
      break;
    case eff_join:
      freeJoin(proc -> effs[i]);
      break;
    case eff_raise:
      freeRaise(proc -> effs[i]);
      break;
    case eff_ret:
      freeReturn(proc -> effs[i]);
      break;
    default:
      assert(0 && "try to free an unknown effect");
      break;
    }
  }
  free(proc -> effs);
  proc -> effs = NULL;
}

static
void freeModuleProcs(module_t *module)
{
  proc_t *next_proc, *proc;

  VERBOSE_PRINT ((stdout, "free procs for module \"%s\"\n",
          get_id_str(module -> id)));

  proc = module -> procs;
  while(proc) {
    next_proc = proc -> next;
    freeProcEffs(proc);
    free(proc);
    proc = next_proc;
  }
}

static
void free_data(void)
{
  module_t *module, *next_module;

  module = modules;
  while(module) {
    freeModuleProcs(module);
    next_module = module -> next;
    free(module);
    module = next_module;
  }
}

/********************************************************************/

static
type_t *find_type_in_module(module_t *module, const ident *typeid)
{
  type_t *type;

  for(type = module -> types; type; type = type -> prev) {
    VERBOSE_PRINT((stdout, "test typeid %s\n", get_id_str(type -> id)));
    if(type -> id == typeid) {
      VERBOSE_PRINT((stdout, "found\n"));
      return(type);
    }
  }
  VERBOSE_PRINT((stdout, "did not find type id %s\n", get_id_str(typeid)));
  return(NULL);
}

/********************************************************************/

static void add_value_to_proc(proc_t *proc, eff_t *eff)
{
  eff -> next = proc -> values;
  proc -> values = eff;
}


eff_t *find_valueid_in_proc_effects(const ident *id, proc_t *proc)
{
  eff_t *val;

  val = proc -> values;
  while(val) {
    if(id == val -> id) {
      return(val);
    }
    val = val -> next;
  }
  return(NULL);
}

static void create_abstract_return(ir_graph *irg, proc_t *proc, eff_t *eff)
{
  ir_node *x;
  eff_t *eff_res;

  VERBOSE_PRINT((stdout, "create effect:return in %s\n",
         get_id_str(proc -> proc_ident)));
  if(NO_ID == eff -> effect.ret.ret_id) {
    /* return void */
    x = new_Return (get_store(), 0, NULL);
  }
  else {
    ir_node *in[1];

    /* return one value */
    eff_res = find_valueid_in_proc_effects(eff -> effect.ret.ret_id, proc);
    assert(eff_res -> firmnode && "firm in effect not set");
    in[0] = eff_res -> firmnode;
    x = new_Return (get_store(), 1, in);
  }
  eff -> firmnode = x;

  /* Now we generated all instructions for this block and all its predecessor
   * blocks so we can mature it.  (There are not too much.) */
  mature_immBlock (get_irg_current_block(irg));

  /* This adds the in edge of the end block which originates at the return statement.
   * The return node passes controlflow to the end block.  */
  add_immBlock_pred (get_irg_end_block(irg), x);
}


static void create_abstract_arg(ir_graph *irg, proc_t *proc, eff_t *eff)
{
  ir_node *arg;
  entity *ent;
  ir_mode *mode;
  type *typ;
  int num;

  VERBOSE_PRINT((stdout, "create effect:arg %d in %s\n",
         eff -> effect.arg.num, get_id_str(proc -> proc_ident)));
  ent = get_irg_entity(irg);
  typ = get_entity_type(ent);

  /* read argument eff -> effect.arg.num and place in values list */
  num = get_method_n_params(typ);
  assert((num >= eff -> effect.arg.num) && "number too big");
  typ = get_method_param_type(typ, eff -> effect.arg.num);
  mode = get_type_mode(typ);

  arg = new_Proj(get_irg_args(irg), mode, eff -> effect.arg.num);
  eff -> firmnode = arg;

  add_value_to_proc(proc, eff);
}


static void create_abstract_load(ir_graph *irg, proc_t *proc, eff_t *eff)
{
  ir_node *sel, *load;
  entity *ent;
  ir_mode *mode;
  eff_t *addr;

  VERBOSE_PRINT((stdout, "create load in %s\n",
         get_id_str(proc -> proc_ident)));

  if(eff -> effect.load.ent) {
    ent = eff -> effect.load.ent -> f_ent;
    VERBOSE_PRINT((stdout, "load from %s\n", get_entity_name(ent)));
  }
  else {
    VERBOSE_PRINT((stdout, "store to memory\n"));
    ent = NULL;
  }

  addr = find_valueid_in_proc_effects(eff -> effect.load.ptrrefid, proc);
  assert(addr && "no address for load");
  /* if addr is Unknown, set proper mode */
  if(iro_Unknown == get_irn_opcode(addr -> firmnode)) {
    set_irn_mode(addr -> firmnode, mode_P);
  }

  if(ent) {
    sel = new_simpleSel(get_store(), addr -> firmnode, ent);
    mode = get_type_mode(get_entity_type(ent));
  }
  else {
    sel = addr -> firmnode;
    mode = mode_ANY;
  }
  load = new_Load(get_store(), sel, mode);
  set_store(new_Proj(load, mode_M, 0));
  eff -> firmnode = new_Proj(load, mode, 2);

  add_value_to_proc(proc, eff);
}


static void create_abstract_store(ir_graph *irg, proc_t *proc, eff_t *eff)
{
  ir_node *sel, *store;
  entity *ent;
  eff_t *addr, *val;

  VERBOSE_PRINT((stdout, "create store in %s\n",
         get_id_str(proc -> proc_ident)));

  if(eff -> effect.store.ent) {
    ent = eff -> effect.store.ent -> f_ent;
    VERBOSE_PRINT((stdout, "store to entity %s\n", get_entity_name(ent)));
  }
  else {
    VERBOSE_PRINT((stdout, "store to memory\n"));
    ent = NULL;
  }

  addr = find_valueid_in_proc_effects(eff -> effect.store.ptrrefid, proc);
  assert(addr && "no address for store");
  /* if addr is Unknown, set propper mode */
  if(iro_Unknown == get_irn_opcode(addr -> firmnode)) {
    set_irn_mode(addr -> firmnode, mode_P);
  }

  val = find_valueid_in_proc_effects(eff -> effect.store.valrefid, proc);
  assert(val && "no address for store");
  /* if addr is Unknown, set propper mode */
  if(iro_Unknown == get_irn_opcode(val -> firmnode)) {
    set_irn_mode(val -> firmnode, get_type_mode(get_entity_type(ent)));
  }

  if(ent) {
    sel = new_simpleSel(get_store(), addr -> firmnode, ent);
  }
  else {
    sel = addr -> firmnode;
  }
  store = new_Store(get_store(), sel, val -> firmnode);
  set_store(new_Proj(store, mode_M, 0));
  eff -> firmnode = store;
}


static void create_abstract_alloc(ir_graph *irg, proc_t *proc, eff_t *eff)
{
  type *ftype;
  ir_node *alloc;
  type_t *xtype;
  symconst_symbol sym;

  VERBOSE_PRINT((stdout, "create alloc in %s\n",
         get_id_str(proc -> proc_ident)));

  xtype = find_type_in_module(current_module, eff -> effect.alloc.tp_id);
  assert(xtype && "type not found");
  ftype = xtype -> f_tp;

  sym.type_p = ftype;
  alloc = new_Alloc(get_store(), new_SymConst(sym, symconst_size), ftype,
            heap_alloc);
  set_store(new_Proj(alloc, mode_M, 0));
  eff -> firmnode = new_Proj(alloc, mode_P, 2);

  add_value_to_proc(proc, eff);
}


static void create_abstract_unknown(ir_graph *irg, proc_t *proc, eff_t *eff)
{
  ir_node *unknown;

  VERBOSE_PRINT((stdout, "create unknown in %s\n",
         get_id_str(proc -> proc_ident)));

  unknown = new_Unknown(mode_ANY);
  eff -> firmnode = unknown;

  add_value_to_proc(proc, eff);
}


static void create_abstract_call(ir_graph *irg, proc_t *proc, eff_t *eff)
{
  ir_node *sel, *call;
  entity *ent;
  eff_t *addr;
  ir_node **irns;
  int i, num;
  type *mtype;
  int mik; /* is method somehow known? */

  VERBOSE_PRINT((stdout, "create call in %s\n",
         get_id_str(proc -> proc_ident)));

  if(eff -> effect.call.ent) {
    ent = eff -> effect.call.ent -> f_ent;
    VERBOSE_PRINT((stdout, "call %s\n", get_entity_name(ent)));
  }
  else {
    ent = NULL;
    VERBOSE_PRINT((stdout, "call something in memory\n"));
  }

  addr = find_valueid_in_proc_effects(eff -> effect.call.valrefid, proc);
  assert(addr && "no address for load");
  /* if addr is Unknown, set propper mode */
  if(iro_Unknown == get_irn_opcode(addr -> firmnode)) {
    set_irn_mode(addr -> firmnode, mode_P);
  }

  if(ent) {
    /* the address */
    sel = new_simpleSel(get_store(), addr -> firmnode, ent);
    /* mthod type */
    mtype = get_entity_type(ent);
    mik = true;
  }
  else {
    /* the address */
    sel = addr -> firmnode;
    /* mthod type */
    mtype = get_unknown_type();
    mik = false;
  }

  /* the args */
  num = eff -> effect.call.n_args;
  VERBOSE_PRINT((stdout, "number of args given: %d\n", num));
  if(mik) {
    VERBOSE_PRINT((stdout, "number of args expected: %d\n",
           get_method_n_params(mtype)));
  }
  irns = alloca(num * sizeof(ir_node*));
  for(i = 0; i < num; i++) {
    irns[i] = find_valueid_in_proc_effects(eff -> effect.call.args[i], proc)
      -> firmnode;
    if(iro_Unknown == get_irn_opcode(irns[i])) {
      if(mik) {
    set_irn_mode(irns[i], get_type_mode(get_method_param_type(mtype, i)));
      }
      else {
    set_irn_mode(irns[i], mode_ANY);
      }
    }
  }
  call = new_Call(get_store(), sel, num, irns, mtype);
  set_store(new_Proj(call, mode_M, 0));
  if(mik && (0 != get_method_n_ress(mtype))) {
    eff -> firmnode = new_Proj(call,
                   get_type_mode(get_method_res_type(mtype, 0)),
                   0);
    add_value_to_proc(proc, eff); /* result can be accessed */
  }
  else {
    eff -> firmnode = NULL; /* result can not be accessed */
  }
}

static void create_abstract_join (ir_graph *irg, proc_t *proc, eff_t *eff)
{
  ir_node **ins    = NULL;
  ir_node *unknown = NULL;
  ir_node *cond    = NULL;
  ir_node *block   = NULL;
  ir_node *c_block = NULL;
  ir_node *phi     = NULL;
  ir_mode *join_md = mode_ANY;
  int n_ins = -1;
  int i;

  VERBOSE_PRINT((stdout, "create join in %s\n",
         get_id_str(proc -> proc_ident)));

  assert (eff_join == eff->kind);

  n_ins = eff->effect.join.n_ins;

  /* seems like current_block is not always mature at this point */
  mature_immBlock (get_cur_block ());

  block = get_cur_block ();     /* remember this so we can put the ProjXs into it */

  /* jump based on an unknown condition so all values are possible */
  unknown = new_Unknown (mode_Iu);
  cond    = new_Cond (unknown);

  c_block   = new_immBlock ();  /* for the Phi after the branch(es) */

  ins = (ir_node**) malloc (n_ins * sizeof (ir_node*));
  for (i = 0; i < n_ins; i ++) {
    ir_node *projX   = NULL;
    ir_node *s_block = NULL;
    ir_node *jmp     = NULL;
    eff_t *in_eff;

    /* make sure the projX is in the 'switch' block */
    set_cur_block (block);
    projX   = new_Proj (cond, mode_X, (long) i);

    /* this also sets current_block, so the rest of the code ends up there: */
    s_block = new_immBlock ();

    add_immBlock_pred (s_block, projX);
    mature_immBlock (s_block);

    in_eff = find_valueid_in_proc_effects (eff->effect.join.ins [i], proc);

    ins [i] = in_eff->firmnode;

    /* need to find a suitable mode for the Phi node */
    if (mode_ANY != get_irn_mode (ins [i])) {
      join_md = get_irn_mode (ins [i]);
    }

    jmp = new_Jmp ();
    add_immBlock_pred (c_block, jmp);
  }

  set_cur_block (c_block);

  phi = new_Phi (n_ins, ins, join_md);

  mature_immBlock (c_block);
  memset (ins, 0x00, n_ins * sizeof (ir_node*));
  free (ins);

  eff->firmnode = phi;

  add_value_to_proc (proc, eff);
}

static void create_abstract_raise (ir_graph *irg, proc_t *proc, eff_t *eff)
{
  ir_node *block   = NULL;
  ir_node *unknown = NULL;
  ir_node *cond    = NULL;

  /* seems like current_block is not always mature at this point */
  mature_immBlock (get_cur_block ());
  block = get_cur_block ();     /* remember this so we can put the ProjXs into it */

  /* jump based on an unknown condition so both values are possible */
  unknown = new_Unknown (mode_Iu);
  cond    = new_Cond (unknown);

  /* one branch for 'throw-exception' case */
  {
    ir_node *projX = new_Proj (cond, mode_X, 1L);
    ir_node *b_exc = new_immBlock ();
    ir_node *obj   = NULL;
    ir_node *thrw  = NULL;
    eff_t *thrw_eff = NULL;

    add_immBlock_pred (b_exc, projX);

    thrw_eff = find_valueid_in_proc_effects (eff->effect.raise.valref, proc);
    obj = thrw_eff->firmnode;

    thrw = new_Raise (get_store (), obj);
    /* exc-jump to end block */
    thrw = new_Proj (thrw, mode_X, 0L);

    add_immBlock_pred (get_irg_end_block (irg), thrw);
    mature_immBlock (get_cur_block ());
  }

  set_cur_block (block);     /* back to the first block */

  /* one branch for 'non-exception' case */
  {
    ir_node *projX = new_Proj (cond, mode_X, 0);
    new_immBlock ();            /* also sets current_block */
    add_immBlock_pred (get_cur_block (), projX);
    mature_immBlock (get_cur_block ());
    /* continue building in current_block */
  }

}

static void create_abstract_firm(module_t *module, proc_t *proc, entity *fent)
{
  eff_t *eff;
  ir_graph *irg;
  int i, num;

  /* test entity */
  assert(visibility_external_allocated == get_entity_visibility(fent)
     && peculiarity_existent == get_entity_peculiarity(fent)
     && "not an abstract entity");
  /* create irg in entity */
  irg = new_pseudo_ir_graph(fent, 0);
  set_irg_inline_property(irg, irg_inline_forbidden);

  /* @@@ If the spec says so: */
  set_entity_visibility(fent, visibility_local);

  VERBOSE_PRINT((stdout, "create effects for %s\n",
         get_id_str(proc -> proc_ident)));

  /* create effects in irg */
  num = proc -> n_effs;
  for(i = 0; i < num; i++) {
    eff = proc -> effs[i];
    VERBOSE_PRINT((stdout,
           "create effect \"%s\"\n", effect_string[(int)eff -> kind]));
    switch(eff -> kind) {
    case eff_ret:
      create_abstract_return(irg, proc, eff);
      break;
    case eff_arg:
      create_abstract_arg(irg, proc, eff);
      break;
    case eff_load:
      create_abstract_load(irg, proc, eff);
      break;
    case eff_store:
      create_abstract_store(irg, proc, eff);
      break;
    case eff_unknown:
      create_abstract_unknown(irg, proc, eff);
      break;
    case eff_alloc:
      create_abstract_alloc(irg, proc, eff);
      break;
    case eff_call:
      create_abstract_call(irg, proc, eff);
      break;
    case eff_join:
      create_abstract_join(irg, proc, eff);
      break;
    case eff_raise:
      create_abstract_raise(irg, proc, eff);
      break;
    default:
      assert(0 && "effect not implemented");
      break;
    }
  }

  /* close irg in entity */
  /* Now we can mature the end block as all it's predecessors are known. */
  mature_immBlock (get_irg_end_block(irg));

  /* Verify the graph.  Finds some very bad errors in the graph. */
  VERBOSE_PRINT((stdout, "verify graph\n"));
  irg_vrfy(irg);
  VERBOSE_PRINT((stdout, "finalize construction\n"));
  finalize_cons (irg);
}

/********************************************************************/

static void assign_firm_entity(module_t *module, entity_t *xmlent)
{
  int i, num;
  type_t *typ;
  type *type;
  entity *ent;

  VERBOSE_PRINT((stdout, "assign entity %s to typeid %s\n",
         get_id_str(xmlent -> ent_ident),
         get_id_str(xmlent -> owner)));

  typ = find_type_in_module(module, xmlent -> owner);
  assert(typ && "class not found in module");
  type = typ -> f_tp;
  assert(is_class_type(type));

  num = get_class_n_members(type);
  ent = NULL;
  for(i = 0; i < num; i++) {
    ent = get_class_member(type, i);
    VERBOSE_PRINT((stdout, "compare entity %s and %s\n",
           get_id_str(xmlent -> ent_ident), get_entity_name(ent)));

    if(get_entity_ident(ent) == xmlent -> ent_ident) {
      break;
    }
    ent = NULL;
  }
  assert(ent && "did not find a entity");

  xmlent -> f_ent = ent;
}

/********************************************************************/
/* must be primitive type or class type */
static void assign_firm_type(type_t *xmltype)
{
  int i;
  type *typ = NULL;
  int num;

  VERBOSE_PRINT((stdout, "assign firm type to type %s\n",
                 get_id_str(xmltype -> type_ident)));

  /* is it global type? */
  typ = get_glob_type();
  if(xmltype -> type_ident == get_type_ident(typ)) {
    /* yes */
    xmltype -> f_tp = typ;
    VERBOSE_PRINT((stdout, "is global type %s\n", get_type_name(typ)));
  } else {
    num = get_irp_n_types();
    for(i = 0; i < num; i++) {
      typ = get_irp_type(i);
      VERBOSE_PRINT((stdout, "test type %s\n", get_type_name(typ)));
      if(xmltype -> type_ident == get_type_ident(typ)) {
        VERBOSE_PRINT((stdout, "found type %s\n", get_type_name(typ)));
        xmltype -> f_tp = typ;
        break;
      }
      typ = NULL;
    }
  }
  assert(typ && "did not find a type");
}

/********************************************************************/
static
void create_abstract_proc_effect(module_t *module, proc_t *proc)
{
  int i, num;
  type *class_typ = NULL;
  type_t *type;
  entity *fent;

  /* find the class of a procedure */
  VERBOSE_PRINT((stdout, "do find owner id %s\n", get_id_str(proc -> ownerid)));
  type = find_type_in_module(module, proc -> ownerid);
  assert(type && "class not found in module");

  class_typ = get_glob_type();
  VERBOSE_PRINT((stdout, "test type %s\n", get_type_name(class_typ)));
  if(type -> type_ident != get_type_ident(class_typ)) {
    /* find module as class */
    num = get_irp_n_types();
    for(i = 0; i < num; i++) {
      class_typ = get_irp_type(i);
      VERBOSE_PRINT((stdout, "test type %s\n", get_type_name(class_typ)));
      if(is_class_type(class_typ)
     && (type -> type_ident == get_type_ident(class_typ))) {
    /* found class type */
    VERBOSE_PRINT((stdout, "found type %s\n", get_type_name(class_typ)));
    break;
      }
      class_typ = NULL;
    }
  }
  else {
    VERBOSE_PRINT((stdout, "found global type %s\n", get_type_name(class_typ)));
  }
  assert(class_typ && "type not found");
  assert(is_class_type(class_typ) && "is not a class type");
  type -> f_tp = class_typ;

  /* find entity for procedure in class */
  VERBOSE_PRINT((stdout, "find method %s\n",
         get_id_str(proc -> proc_ident)));

  num = get_class_n_members(class_typ);
  fent = NULL;
  for(i = 0; i < num; i++) {
    fent = get_class_member(class_typ, i);
    VERBOSE_PRINT((stdout, "test proc %s\n", get_entity_name(fent)));
    if(proc -> proc_ident == get_entity_ident(fent)) {
      VERBOSE_PRINT((stdout, "found proc %s\n",
             get_id_str(proc -> proc_ident)));
      /* @@@ TODO check args types - not in xml yet */
      /* create Firm stuff */
      create_abstract_firm(module, proc, fent);
      return;
    }
    else {
      fent = NULL;
    }
  }

  /* fail */
  fprintf(stderr,
      "method %s not found\nNo effects generated\nCandidates are:\n",
      get_id_str(proc -> proc_ident));
  for(i = 0; i < num; i++) {
    fent = get_class_member(class_typ, i);
    fprintf(stderr, "%s\n", get_entity_name(fent));
  }
  //assert(fent && "procedure not found in class");
}

static
void create_abstract_module(module_t *module)
{
  proc_t *proc;
  type_t *type;
  entity_t *ent;

  VERBOSE_PRINT((stdout, "create an abstraction for module %s\n",
         get_id_str(module -> id)));

  VERBOSE_PRINT((stdout, "--handle types for module\n"));
  for(type = module -> types; type; type = type -> prev) {
    assign_firm_type(type);
  }

  VERBOSE_PRINT((stdout, "--handle entities for module\n"));
  /* @@@ TODO */
  for(ent = module -> entities; ent; ent = ent -> prev) {
    assign_firm_entity(module, ent);
  }

  VERBOSE_PRINT((stdout, "--handle procs for module\n"));
  for(proc = module -> procs; proc; proc = proc -> next) {
    create_abstract_proc_effect(module, proc);
  }
}


void create_abstraction(const char *filename)
{
  module_t *module;

  /* read and parse XML file */
  read_extern(filename);

  /* finished reading and parsing here */
  /* build FIRM graphs */
  module = modules;
  while(module) {
    current_module = module;
    create_abstract_module(module);
    module = module -> next;
  }
  current_module = NULL;

  /* free data structures */
  free_data();

  types = NULL;
  entities = NULL;
  procs = NULL;
  modules = NULL;
}


void free_abstraction(void) {
  int i, n_pseudo_irgs = get_irp_n_pseudo_irgs();
  for (i = 0; i < n_pseudo_irgs; ++i) {
    ir_graph *p_irg = get_irp_pseudo_irg(i);
    set_entity_visibility(get_irg_entity(p_irg), visibility_external_allocated);
    // @@@ free_pseudo_ir_graph(p_irg);
  }
}


/********************************************************************/


/*
 * $Log$
 * Revision 1.17  2004/11/23 14:17:31  liekweg
 * fenced out currently unneeded static functions
 *
 * Revision 1.16  2004/11/11 12:24:52  goetz
 * fixes
 *
 * Revision 1.15  2004/11/11 09:28:32  goetz
 * treat pseudo irgs special
 * parse 'local' from xml files
 *
 * Revision 1.14  2004/11/10 14:42:00  boesler
 * be more helpful if a method does not exist
 *
 * Revision 1.13  2004/11/05 14:00:53  liekweg
 * added raise
 *
 * Revision 1.12  2004/11/02 14:30:31  liekweg
 * fixed multi-input join (thx, Boris) --flo
 *
 * Revision 1.11  2004/10/29 18:51:53  liekweg
 * Added Join
 *
 * Revision 1.10  2004/10/25 13:52:24  boesler
 * seperated read.h (public interface) and read_t.h (types)
 *
 * Revision 1.9  2004/10/22 13:51:35  boesler
 * prohibit inlining of pseudo ir_graphs
 *
 * Revision 1.8  2004/10/22 13:13:27  boesler
 * replaced char* by idents, minor fix in Firm codegen for call
 *
 * Revision 1.7  2004/10/21 15:31:55  boesler
 * added lots of stuff:
 * - build abstract syntax trees
 * - build Firm graphs for many effects, still todos
 *
 * Revision 1.5  2004/10/18 12:48:20  liekweg
 * avoid warning
 *
 * Revision 1.4  2004/10/14 11:31:53  liekweg
 * ...
 *
 * Revision 1.3  2004/10/13 13:36:28  rubino
 * fix for strdup
 *
 * Revision 1.2  2004/10/11 15:56:09  liekweg
 * Cleanup, comments ...
 * Added init func --flo
 *
 * Revision 1.1  2004/10/11 09:31:06  liekweg
 * First Import of XML reading procs --flo
 *
 */
