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

# include "read.h"

static type_t *types = NULL;
static entity_t *entities = NULL;
static effs_t *effs = NULL;

static int _ent_id = 0;
/* static int _node_id = 0; */

static const char*
getNodeModule (xmlNodePtr node)
{
  char *mod_str = (char*) xmlGetProp (node, BAD_CAST "module");

  if (NULL == mod_str) {
    return (NULL);
  } else {
    const char *res = strdup (mod_str);

    return (res);
  }
}

static char*
getNodeProcName (xmlNodePtr node)
{
  char *proc_str = (char*) xmlGetProp (node, BAD_CAST "procname");

  assert (proc_str);

  return (strdup (proc_str));
}

static const int
getNodeId (xmlNodePtr node)
{
  char *id_str = (char*) xmlGetProp (node, BAD_CAST "id");
  int id;
  assert (id_str);
  id = atoi (id_str+1);

  return (id);
}

static const int
getNodeRefId (xmlNodePtr node)
{
  char *refid_str = (char*) xmlGetProp (node, BAD_CAST "refid");
  int refid;
  assert (refid_str);
  refid = atoi (refid_str+1);

  return (refid);
}

static const int
getNodeTypeId (xmlNodePtr node)
{
  char *type_str = (char*) xmlGetProp (node, BAD_CAST "type");
  int type_id;
  assert (type_str);
  type_id = atoi (type_str+1);

  return (type_id);
}

static const char
*getNodeTypeStr (xmlNodePtr node)
{
  const char *type_str = (char*) xmlGetProp (node, BAD_CAST "type");
  assert (type_str);

  return (type_str);
}

static const char
*getNodeEntityStr (xmlNodePtr node)
{
  const char *ent_str = (char*) xmlGetProp (node, BAD_CAST "entity");
  assert (ent_str);

  return (ent_str);
}

static arg_t *
parseArg (xmlDocPtr doc, xmlNodePtr argelm)
{
  int id;
  int num;
  char *num_str;
  arg_t *arg;

  CHECK_NAME (argelm, arg);
  /* fprintf (stdout, "arg node \t0x%08x\n", (int) argelm); */

  id = getNodeId (argelm);
  /* fprintf (stdout, "arg->id = \"%d\"\n", id); */
  num_str = (char*) xmlGetProp (argelm, BAD_CAST "number");
  num = atoi (num_str);
  /* fprintf (stdout, "arg->no = \"%d\"\n", no); */

  arg = NEW (arg_t);
  arg->kind = eff_arg;
  arg->id = id;
  arg->num = num;

  return (arg);
}

static valref_t
*parseValref (xmlDocPtr doc, xmlNodePtr valelm)
{
  int ref_id;
  valref_t *valref;
  CHECK_NAME (valelm, valref);
  /* fprintf (stdout, "valref node \t0x%08x\n", (int) valelm); */

  ref_id = getNodeRefId (valelm);
  /* fprintf (stdout, "val->refid = \"%d\"\n", ref_id); */

  valref = NEW (valref_t);
  valref->kind = eff_valref;
  valref->refid = ref_id;

  return (valref);
}

static select_t
*parseSelect (xmlDocPtr doc, xmlNodePtr selelm)
{
  char *entity_str = (char*) xmlGetProp (selelm, BAD_CAST "entity");
  const int entity_id = atoi (entity_str+1);
  entity_t *ent;
  xmlNodePtr child;
  valref_t *valref = NULL;
  select_t *sel = NEW (select_t);
  sel->kind = eff_select;

  CHECK_NAME (selelm, select);
  /* fprintf (stdout, "select node \t0x%08x\n", (int) selelm); */

  ent = getEntityById (entity_id);

  child = selelm->xmlChildrenNode;

  if (child) {
    valref = parseValref (doc, child);
  }

  sel->valrefid = valref ? valref->refid : -1;
  sel->ent = ent;

  if (valref) {
    free (valref);
  }

  return (sel);
}

static load_t
*parseLoad (xmlDocPtr doc, xmlNodePtr loadelm)
{
  int id;
  xmlNodePtr child;
  select_t *sel;
  load_t *load = NEW (load_t);
  load->kind = eff_load;

  CHECK_NAME (loadelm, load);
  /* fprintf (stdout, "load node \t0x%08x\n", (int) loadelm); */
  id = getNodeId (loadelm);

  child = loadelm->xmlChildrenNode;

  sel = parseSelect (doc, child);

  load->id = id;
  load->ptrrefid = sel->valrefid;
  load->ent = sel->ent;

  free (sel);

  return (load);
}

static store_t
*parseStore (xmlDocPtr doc, xmlNodePtr storeelm)
{
  xmlNodePtr child;
  select_t *sel;
  valref_t *valref;
  store_t *store = NEW (store_t);
  store->kind = eff_store;

  CHECK_NAME (storeelm, store);
  /* fprintf (stdout, "store node \t0x%08x\n", (int) storeelm); */

  child = storeelm->xmlChildrenNode;

  sel = parseSelect (doc, child);

  child = child->next;

  valref = parseValref (doc, child);

  store->ent = sel->ent;
  store->ptrrefid = sel->valrefid;
  store->valrefid = valref->refid;

  free (sel);
  free (valref);

  return (store);
}

static alloc_t
*parseAlloc (xmlDocPtr doc, xmlNodePtr allocelm)
{
  int id;
  int type_id;
  alloc_t *alloc = NEW (alloc_t); /* ...! */
  alloc->kind = eff_alloc;

  CHECK_NAME (allocelm, alloc);
  /* fprintf (stdout, "alloc node \t0x%08x\n", (int) allocelm); */
  id = getNodeId (allocelm);
  /* fprintf (stdout, "alloc->id = \"%d\"\n", id); */
  type_id = getNodeTypeId (allocelm);
  /* fprintf (stdout, "alloc->type_id = \"%d\"\n", type_id); */

  alloc->id = id;
  alloc->tp_id = type_id;

  return (alloc);
}

static call_t
*parseCall (xmlDocPtr doc, xmlNodePtr callelm)
{
  int id;
  xmlNodePtr child;
  select_t *sel;
  xmlNodePtr arg;
  int n_args;
  call_t *call = NEW (call_t);
  call->kind = eff_call;

  CHECK_NAME (callelm, call);
  /* fprintf (stdout, "call node \t0x%08x\n", (int) callelm); */
  id = getNodeId (callelm);
  /* fprintf (stdout, "call->id = \"%d\"\n", id); */

  child = callelm->xmlChildrenNode;

  sel = parseSelect (doc, child);

  arg = child = child->next;

  n_args = 0;

  while (NULL != child) {
    n_args ++;

    child = child->next;
  }

  call->id = id;
  call->valrefid = sel->valrefid;
  call->ent = sel->ent;
  call->n_args = n_args;
  call->args = NULL;

  free (sel);

  if (0 != n_args) {
    int *args = (int*) malloc (n_args * sizeof (int) );
    int i = 0;

    while (NULL != arg) {
      valref_t *valref = parseValref (doc, arg);
      args [i ++] = valref->refid;
      free (valref);

      arg = arg->next;
    }

    call->args = args;
  }

  return (call);
}

static join_t
*parseJoin (xmlDocPtr doc, xmlNodePtr joinelm)
{
  int id;
  int n_ins;
  int *ins;
  int i;
  xmlNodePtr child;
  join_t *join = NEW (join_t);
  join->kind = eff_join;

  CHECK_NAME (joinelm, join);
  /* fprintf (stdout, "join node \t0x%08x\n", (int) joinelm); */
  id = getNodeId (joinelm);
  /* fprintf (stdout, "join->id = \"%d\"\n", id); */

  child = joinelm->xmlChildrenNode;

  n_ins = 0;

  while (NULL != child) {
    n_ins ++;
    child = child->next;
  }

  ins = (int*) malloc (n_ins * sizeof (int) );
  i = 0;

  child = joinelm->xmlChildrenNode;

  while (NULL != child) {
    valref_t *valref = parseValref (doc, child);
    ins [i ++] = valref->refid;

    child = child->next;
  }

  join->id = id;
  join->n_ins = n_ins;
  join->ins = ins;

  return (join);
}

static unknown_t
*parseUnknown (xmlDocPtr doc, xmlNodePtr unknownelm)
{
  int id;
  unknown_t *unknown = NEW (unknown_t);
  unknown->kind = eff_unknown;

  CHECK_NAME (unknownelm, unknown);
  /* fprintf (stdout, "unknown node \t0x%08x\n", (int) unknownelm); */
  id = getNodeId (unknownelm);

  unknown->id = id;

  return (unknown);
}

static ret_t
*parseReturn (xmlDocPtr doc, xmlNodePtr retelm)
{
  xmlNodePtr child;
  ret_t *ret = NEW (ret_t);
  ret->kind = eff_ret;

  CHECK_NAME (retelm, ret);
  /* fprintf (stdout, "ret node \t0x%08x\n", (int) retelm); */

  child = retelm->xmlChildrenNode;

  if (child) {
    valref_t *valref = parseValref (doc, child);
    ret->ret_id = valref->refid;
    free (valref);
  } else {
    ret->ret_id = -1;
  }

  return (ret);
}

static raise_t
*parseRaise (xmlDocPtr doc, xmlNodePtr raiseelm)
{
  int tp_id;
  valref_t *valref;
  xmlNodePtr child;
  raise_t *raise = NEW (raise_t);
  raise->kind = eff_raise;

  CHECK_NAME (raiseelm, raise);
  /* fprintf (stdout, "raise node \t0x%08x\n", (int) raiseelm); */

  tp_id = getNodeTypeId (raiseelm);
  /* fprintf (stdout, "raise->type = \"%d\"\n", tp_id); */

  child = raiseelm->xmlChildrenNode;

  assert (NULL != child);

  valref = parseValref (doc, child);

  raise->valref = valref->refid;
  raise->tp_id = tp_id;

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
  const int tp_id = getNodeId (typeelm);
  /* fprintf (stdout, "type node \t0x%08x (%d)\n", (int) typeelm, tp_id); */
  fprintf (stdout, "type = \"%s\"\n", getNodeTypeStr (typeelm));

  type = (type_t*) malloc (sizeof (type_t));

  type->name = (char*) strdup (getNodeTypeStr (typeelm));
  type->id = tp_id;

  type->prev = types;
  types = type;

  if (_ent_id <= tp_id) {
    _ent_id = tp_id+1;
  }
}

/** parse an entity node and insert it into the list */
static void
parseEntity (xmlDocPtr doc, xmlNodePtr entelm)
{
  entity_t *ent = NEW (entity_t);

  /* parse it */
  const int ent_id = getNodeId (entelm);
  /* fprintf (stdout, "entity node \t0x%08x (%d)\n", (int) entelm, ent_id); */
  fprintf (stdout, "ent  = \"%s.%s\"\n",
           getNodeTypeStr (entelm),
           getNodeEntityStr (entelm));


  ent->name    = (char*) strdup (getNodeEntityStr (entelm));
  ent->tp_name = (char*) strdup (getNodeTypeStr   (entelm));
  ent->id = ent_id;

  ent->prev = entities;
  entities = ent;

  if (_ent_id <= ent_id) {
    _ent_id = ent_id+1;
  }
}

/** parse any effect, and turn it into an eff_t (TODO) */
static void
parseEffect (xmlDocPtr doc, xmlNodePtr effelm)
{
  xmlNodePtr cur = effelm->xmlChildrenNode;
  char *procname = getNodeProcName (effelm);
  effs_t *curr_effs = NULL;
  int i = 0;
  int n_effs = 0;

  fprintf (stdout, "effect for \"%s\"\n", procname);

  while (NULL != cur) {
    n_effs ++;
    cur = cur->next;
  }

  curr_effs = NEW (effs_t);
  curr_effs->procname = procname;
  curr_effs->n_effs = n_effs;
  curr_effs->effs = (eff_t**) malloc (n_effs * sizeof (eff_t*));

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
    } else {
      fprintf (stderr, "wrong element \"%s\"\n", BAD_CAST cur->name);
      exit (EXIT_FAILURE);
    }

    cur = cur->next;

    curr_effs->effs [i ++] = eff;
  }

  curr_effs->next = effs;
  effs = curr_effs;
}

/*
  Public Interface
*/
type_t *getTypeByName (const char *name)
{
  type_t *curr = types;

  while (NULL != curr) {
    if (0 == strcmp (name, curr->name)) {
      return (curr);
    }

    curr = curr->prev;
  }

  return (NULL);
}

type_t *getTypeById (const int id)
{
  type_t *curr = types;

  while (NULL != curr) {
    if (id == curr->id) {
      return (curr);
    }

    curr = curr->prev;
  }

  return (NULL);
}

entity_t *getEntityByNames (const char *name, const char *tp_name)
{
  entity_t *curr = entities;

  while (NULL != curr) {
    if ((0 == strcmp (name, curr->name)) && (0 == strcmp (tp_name, curr->tp_name))) {
      return (curr);
    }

    curr = curr->prev;
  }

  return (NULL);
}

entity_t *getEntityById (const int id)
{
  entity_t *curr = entities;

  while (NULL != curr) {
    if (id == curr->id) {
      return (curr);
    }

    curr = curr->prev;
  }

  return (NULL);
}

effs_t *getEffectByName (const char *procname)
{
  effs_t *curr_effs = effs;

  while (NULL != curr_effs) {
    if (0 == strcmp (procname, curr_effs->procname)) {
      return (curr_effs);
    }

    curr_effs = curr_effs->next;
  }

  return (NULL);
}

void extern_init ()
{
  /* nothing to do */
}

void extern_read (const char *filename)
{
  /* xmlNsPtr ns = NULL; */           /* no namespace for us */
  xmlDocPtr doc;                /* whole document */
  xmlNodePtr cur;               /* current node */

  /* i've got no idea what the VERSION cast is all about. voodoo
     programming at its finest. */
  LIBXML_TEST_VERSION xmlKeepBlanksDefault (0);
  doc = xmlParseFile (filename);
  CHECK (doc, "xmlParseFile");

  cur = xmlDocGetRootElement (doc);
  CHECK (cur, "xmlDocGetRootElement");

  if (! NODE_NAME (cur, effects)) {
    fprintf (stderr,"root node \"%s\" != \"effects\"\n", BAD_CAST cur->name);
    xmlFreeDoc (doc);

    exit (EXIT_FAILURE);
  }

  if (EXTERN_VERBOSE) {
    const char *mod_str = getNodeModule (cur);

    if (NULL != mod_str) {
      fprintf (stdout, "effects for \"%s\"\n", mod_str);
    } else {
      fprintf (stdout, "effects \t0x%08x\n", (int) cur);
    }
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
}

/** clean up our mess */
void extern_cleanup ()
{
  /* the types */
  {
    type_t *tp = types;

    while (NULL != tp) {
      type_t *curr = tp;
      tp = tp->prev;

      free ((char*) curr->name);
      memset (curr, 0x00, sizeof (type_t));
      free (curr);
    }

    types = NULL;
  }

  /* the ennities */
  {
    entity_t *ent = entities;

    while (NULL != ent) {
      entity_t *curr = ent;
      ent = ent->prev;

      free ((char*) curr->name);
      free ((char*) curr->tp_name);
      memset (curr, 0x00, sizeof (entity_t));
      free (curr);
    }

    entities = NULL;
  }

  /* the effs */
  {
    effs_t *eff = effs;

    while (NULL != eff) {
      int i;
      effs_t *curr = eff;
      eff = eff->next;

      for (i = 0; i < curr->n_effs; i ++) {
        free (curr->effs [i]);
      }

      free (curr);
    }
  }

  effs = NULL;
}


void test_getEffectByName ()
{
  /* test getEffectByName */
  char *names [] = {
    "store_unknown_proc",
    "rise_something",
    "other_fake_proc",
    "ret_alloc",
    "mash_args",
    "ret_arg",
    "empty_external",
    "no_this_doesn't_really_exist",
    "my_fake_proc",
    NULL
  };

  int i = 0;

  while (NULL != names [i]) {
    effs_t *the_eff = getEffectByName (names [i]);

    if (the_eff) {
      fprintf (stdout, "Effect for \"%s\" is at 0x%08x\n",
               names [i], (int) the_eff);
    } else {
      fprintf (stdout, "Effect for \"%s\" not found\n",
               names [i]);
    }
    i ++;
  }
}




/*
 * $Log$
 * Revision 1.2  2004/10/11 15:56:09  liekweg
 * Cleanup, comments ...
 * Added init func --flo
 *
 * Revision 1.1  2004/10/11 09:31:06  liekweg
 * First Import of XML reading procs --flo
 *
 */
