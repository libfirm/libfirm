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


# ifdef HAVE_CONFIG_H
#  include <config.h>
# endif

# include "pto_util.h"

# include <malloc.h>
# include <assert.h>
# include <string.h>
# include <obstack.h>

# include "irnode.h"
# include "irgwalk.h"
# include "xmalloc.h"
# include "entity.h"

int get_pto_verbose (void);     /* grrr, can't include pto.h */

# define DBGPRINT(lvl, msg) if (get_pto_verbose () > lvl) { fprintf msg; }

static int pto_id = 0;          /* only for pto_t->kind */

static obj_desc_t *obj_descrs = NULL; /* list of all descrs */

/*
  See whether the given entity is a field.
*/
static int is_field (entity *ent)
{
  type *tp = get_entity_type (ent);

  if (is_primitive_type (tp) || is_pointer_type (tp)) {
    return (TRUE);
  } else {
    return (FALSE);
  }
}

/*
  Helper to collect_fields(type*): collect all fields of the given
  clazz and its super classes into the given obstack.
*/
# define obstack_chunk_alloc xmalloc
# define obstack_chunk_free free

static void _collect_fields (type *clazz, struct obstack *obst)
{
  int n_members = get_class_n_members (clazz);
  int n_supers  = get_class_n_supertypes (clazz);
  int i;

  for (i = 0; i < n_members; i ++) {
    entity *ent = get_class_member (clazz, i);

    if (is_field (ent)) {
      obstack_ptr_grow (obst, ent);

      DBGPRINT (4, (stdout, "%s: add entity \"%s.%s\"\n",
                    __FUNCTION__,
                    get_type_name (clazz),
                    get_entity_name (ent)));
    }
  }

  for (i = 0; i < n_supers; i ++) {
    type *s_clazz = get_class_supertype (clazz, i);

    _collect_fields (s_clazz, obst);
  }
}

/*
  Collect the fields of the given class and its super classes into an array.
  The last entry of the array is written NULL.
*/
static entity **collect_fields (type *clazz)
{
  if (NULL != get_type_link (clazz)) {
    DBGPRINT (3, (stdout, "%s: reusing field list for \"%s\"\n",
                  __FUNCTION__, get_type_name (clazz)));

    return ((entity **) get_type_link (clazz));
  } else {
    DBGPRINT (2, (stdout, "%s: new field list for \"%s\"\n",
                  __FUNCTION__, get_type_name (clazz)));
  }

  struct obstack obst;

  obstack_init (&obst);

  _collect_fields (clazz, &obst);

  /* append terminating NULL */
  int *the_null = NULL;
  obstack_ptr_grow (&obst, the_null);

  int n_fields = obstack_object_size (&obst) / sizeof (void*);

  entity ** fields = (entity**) xmalloc (n_fields * sizeof (entity*));
  void *tmp = obstack_finish (&obst);

  memcpy (fields, tmp, n_fields * sizeof (entity*));

  obstack_free (&obst, NULL);

  set_type_link (clazz, (void*) fields);

  return (fields);
}
# undef obstack_chunk_alloc
# undef obstack_chunk_free


/*
  Environment for find_irg_args
*/
typedef struct find_irg_args_env {
  ir_node **args;
  ir_node *arg;
} find_irg_args_env_t;

/*
  Helper for find_irg_args
*/
static void find_irg_arg (ir_node *node, void *env)
{
  find_irg_args_env_t *arg_env = (find_irg_args_env_t*) env;

  if (iro_Proj == get_irn_opcode (node)) {
    if (arg_env->arg == get_Proj_pred (node)) {
      long n = get_Proj_proj (node);

      assert (! arg_env->args [n]);

      arg_env->args [n] = node;
    }
  }
}

/*
  Print the given obj desc to stdout
*/
static void obj_desc_print (obj_desc_t *obj)
{
  if (obj_kind_obj == obj->kind) {
    /* object desc */
    obj_obj_desc_t *obj_obj = (obj_obj_desc_t*) obj;
    int i;

    fprintf (stdout, "obj desc 0x%08x for type \"%s\"\n",
             (int) obj, get_type_name (obj->tp));

    for (i = 0; i < obj_obj->n_fields; i ++) {
      fprintf (stdout, " \"%s\" -> ", get_entity_name (obj_obj->fields [i]));
      qset_print (obj_obj->vals [i], stdout);
    }

  } else if (obj_kind_array == obj->kind) {
    /* array desc */
    obj_arr_desc_t *obj_arr = (obj_arr_desc_t*) obj;

    fprintf (stdout, "arr desc 0x%08x for type \"%s\"\n",
             (int) obj, get_type_name (obj->tp));

    fprintf (stdout, " [] -> ");
    qset_print (obj_arr->val, stdout);
  } else {
    fprintf (stderr, "%s:%i: Invalid desc\n", __FILE__, __LINE__);
  }
}

/*
  Get the entity of a ptr
*/
entity *get_ptr_ent (ir_node *ptr)
{
  entity *ent = NULL;
  const opcode ptr_op = get_irn_opcode (ptr);
  switch (ptr_op) {
  case (iro_Sel): {
    ent = get_Sel_entity (ptr);
  } break;

  case (iro_SymConst): {
    ent = get_SymConst_entity (ptr);
  } break;

  default: {
    fprintf (stderr, "%s: no ent for ptr=%s[%ld]\n",
             __FUNCTION__,
             get_op_name (get_irn_op (ptr)),
             get_irn_node_nr (ptr));
    assert (0);
  }
  }

  return (ent);
}


/*
  Ctors for the pto types
*/

/*
  Create a new Object Description for the given type
*/
obj_desc_t *obj_desc_new (type *tp)
{
  int i;
  obj_desc_t *res = NULL;

  if (is_array_type (tp)) {
    obj_arr_desc_t *arr_res = (obj_arr_desc_t*) xmalloc (sizeof (obj_arr_desc_t));

    arr_res->val = qset_new (N_INITIAL_OBJS);

    res = (obj_desc_t*) arr_res;
    res->kind = obj_kind_array;

    DBGPRINT (0, (stdout, "%s: new ARRAY desc 0x%08x for %s\n",
             __FUNCTION__, (int) res, get_type_name (tp)));
  } else if (is_class_type (tp)) {
    obj_obj_desc_t *obj_res = (obj_obj_desc_t*) xmalloc (sizeof (obj_obj_desc_t));
    int n_fields = 0;
    entity **tmp;

    DBGPRINT (0, (stdout, "%s: new CLAZZ desc 0x%08x for %s\n",
             __FUNCTION__, (int) obj_res, get_type_name (tp)));

    obj_res->fields = collect_fields (tp);

    /* grr, must count */
    for (tmp = obj_res->fields; NULL != *tmp; tmp ++) {
      n_fields ++;
    }
    obj_res->n_fields = n_fields;

    obj_res->vals = (qset_t **) xmalloc (n_fields * sizeof (qset_t*));

    for (i = 0; i < n_fields; i ++) {
      obj_res->vals [i] = qset_new (N_INITIAL_OBJS);
    }

    res = (obj_desc_t*) obj_res;
    res->kind = obj_kind_obj;
  } else {
    assert (0 && "unknown type");
  }

  res->tp = tp;

  res->next = obj_descrs;
  obj_descrs = res;
# ifdef PTO_DUMMY
  res->is_dummy = FALSE;
# endif /* defined PTO_DUMMY */

  return (res);
}

# ifdef PTO_DUMMY
/*
  Mark an obj desc as a dummy
*/
void obj_desc_set_dummy (obj_desc_t *obj_desc)
{
  obj_desc->is_dummy = TRUE;
}

/*
  Say whether an obj desc is a dummy
*/
int obj_desc_is_dummy (obj_desc_t *obj_desc)
{
  return (obj_desc->is_dummy);
}
# endif /* defined PTO_DUMMY */

/*
  Deallocate an obj desc
*/
void obj_desc_delete (obj_desc_t *obj_desc)
{
  if (obj_kind_obj == obj_desc->kind) {
    /* object desc */
    obj_obj_desc_t *obj_obj = (obj_obj_desc_t*) obj_desc;
    int i;

    memset (obj_obj->fields, 0x00, obj_obj->n_fields * sizeof (entity*));
    free (obj_obj->fields);

    for (i = 0; i < obj_obj->n_fields; i ++) {
      qset_delete (obj_obj->vals [i]);
    }

    memset (obj_obj->vals, 0x00, obj_obj->n_fields * sizeof (entity*));
    free (obj_obj->vals);

    memset (obj_obj, 0x00, sizeof (obj_obj_desc_t));

    free (obj_obj);
  } else {
    /* array desc */
    obj_arr_desc_t *obj_arr = (obj_arr_desc_t*) obj_desc;

    qset_delete (obj_arr->val);

    memset (obj_arr, 0x00, sizeof (obj_arr_desc_t));

    free (obj_arr);
  }
}

/*
  List all obj descs that have been created.  If tp is given non-NULL,
  only descs for this type are listed, else all types are listed.
*/
void obj_desc_list_all (type *tp)
{
  obj_desc_t *cur = obj_descrs;

  while (NULL != cur) {
    if ((NULL == tp) || (tp == cur->tp)) {
      obj_desc_print (cur);
    }

    cur = cur->next;
  }
}

/*
  Create a new pto value containing no names.
*/
pto_t *pto_new_empty (ir_node *node)
{
  pto_t *pto = (pto_t*) xmalloc (sizeof (pto_t));

  pto->kind = &pto_id;
  pto->node = node;
  pto->objs = qset_new (N_INITIAL_OBJS);

  return (pto);
}

/*
  Create a new pto value containing a name of the given type.
*/
pto_t *pto_new_name (ir_node *node, type *tp)
{
  pto_t *pto = pto_new_empty (node);
  obj_desc_t *desc = obj_desc_new (tp);

  qset_insert (pto->objs, desc);

  return (pto);
}

/*
  Deallocate a pto
*/
void pto_delete (pto_t *pto)
{
  qset_delete (pto->objs);

  memset (pto, 0x00, sizeof (pto_t));

  free (pto);
}


/*
  Pto types misc ops
*/
/*
  Sanity checking on a pto_t
*/
void check_pto (pto_t *pto)
{
  assert (&pto_id == pto->kind);
}

/*
  Add the given name to the given pto.
*/
void pto_add_name (pto_t *pto, obj_desc_t *obj)
{
  qset_insert (pto->objs, obj);
}

/*
  Add all the given names to the given pto.
*/
void pto_add_all_names (pto_t *pto, qset_t *objs)
{
  qset_insert_all (pto->objs, objs);
}


/*
  Find the arguments of a graph. For a method that has n args, the
  result array has 'n+1' entries, the last of which is written NULL.
*/
ir_node **find_irg_args (ir_graph *graph)
{
  type *tp = get_entity_type (get_irg_entity (graph));
  const int n_args = get_method_n_params (tp);
  ir_node **args = (ir_node**) xmalloc (sizeof (ir_node*) * (n_args+1));
  ir_node *arg = get_irg_args (graph);
  find_irg_args_env_t *arg_env =
    (find_irg_args_env_t*) xmalloc (sizeof (find_irg_args_env_t));

  arg_env->args = args;
  arg_env->arg  = arg;

  /* or use get_irg_end ?!? */
  {
    ir_graph *save = get_current_ir_graph ();
    set_current_ir_graph (graph);
    irg_walk (get_irg_end (graph), find_irg_arg, NULL, arg_env);
    set_current_ir_graph (save);
  }

  free (arg_env);

  args [n_args] = NULL;

  return (args);
}

/*
  Perform a lookup of the contents of the given field in the given pto
*/
qset_t *pto_lookup (obj_desc_t *obj_desc, entity *ent)
{
  qset_t *res = NULL;

  if (obj_kind_obj == obj_desc->kind) {
    /* obj lookup */
    obj_obj_desc_t *obj_obj = (obj_obj_desc_t*) obj_desc;
    int i;

    assert (NULL != ent);

    for (i = 0; NULL != obj_obj->fields [i]; i ++) {
      if (obj_obj->fields [i] == ent) {
        break;
      }
    }

    assert (obj_obj->fields [i]); /* this *must* find a field */

    res = obj_obj->vals [i];
  } else {
    /* array lookup */
    obj_arr_desc_t *arr_obj = (obj_arr_desc_t*) obj_desc;

    assert (NULL == ent);

    res = arr_obj->val;
  }

  return (res);
}

/*
  Enter the given ptr values into the given field of the given pto
*/
void pto_enter (obj_desc_t *obj_desc, entity *ent, pto_t *pto)
{
  if (obj_kind_obj == obj_desc) {
    /* obj enter */
    obj_obj_desc_t *obj_obj = (obj_obj_desc_t*) obj_desc;
    int i;

    for (i = 0; NULL != obj_obj->fields [i]; i ++) {
      if (obj_obj->fields [i] == ent) {
        break;
      }
    }

    assert (obj_obj->fields [i]); /* this *must* find a field */

    qset_insert_all (obj_obj->vals [i], pto->objs);
  } else {
    /* array enter */
    assert (0 && "array enter not yet implemented");
  }
}


/*
  $Log$
  Revision 1.3  2004/11/04 14:58:38  liekweg
  expanded pto, added initialisation, added debugging printing

  Revision 1.2  2004/10/25 11:59:45  liekweg
  Copy Only works

  Revision 1.1  2004/10/22 15:10:51  liekweg
  moved utils to pto_util


 */
