/*
 * Project:     libFIRM
 * File name:   ir/tr/entity.c
 * Purpose:     Representation of all program known entities.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
# include <stddef.h>

#include "firm_common_t.h"

# include "xmalloc.h"
# include "entity_t.h"
# include "mangle.h"
# include "typegmod.h"
# include "array.h"

/* All this is needed to build the constant node for methods: */
# include "irprog_t.h"
# include "ircons.h"
# include "tv_t.h"

#if DEBUG_libfirm
# include "irdump.h"  /* for output if errors occur. */
#endif

# include "callgraph.h"  /* for dumping debug output */

/*******************************************************************/
/** general                                                       **/
/*******************************************************************/

entity *unknown_entity = NULL; entity *get_unknown_entity(void) { return unknown_entity; }
#define UNKNOWN_ENTITY_NAME "unknown_entity"

static INLINE entity *
new_rd_entity (dbg_info *db, type *owner, ident *name, type *type);

void
init_entity (void)
{
  symconst_symbol sym;

  assert(unknown_type && "Call init_type before init_entity!");
  assert(!unknown_entity && "Call init_entity only once!");
  unknown_entity = new_rd_entity(NULL, unknown_type, new_id_from_str(UNKNOWN_ENTITY_NAME), unknown_type);
  set_entity_visibility(unknown_entity, visibility_external_allocated);
  set_entity_ld_ident(unknown_entity, get_entity_ident(unknown_entity));

  sym.entity_p = unknown_entity;
  current_ir_graph = get_const_code_irg();
  unknown_entity->value = new_SymConst(sym, symconst_addr_ent);
}


/*-----------------------------------------------------------------*/
/* ENTITY                                                          */
/*-----------------------------------------------------------------*/

static INLINE void insert_entity_in_owner (entity *ent) {
  type *owner = ent->owner;
  switch (get_type_tpop_code(owner)) {
  case tpo_class: {
    add_class_member (owner, ent);
  } break;
  case tpo_struct: {
    add_struct_member (owner, ent);
  } break;
  case tpo_union: {
    add_union_member (owner, ent);
  } break;
  case tpo_array: {
    set_array_element_entity(owner, ent);
  } break;
  default: assert(0);
  }
}

/**
 * creates a new entity
 */
static INLINE entity *
new_rd_entity (dbg_info *db, type *owner, ident *name, type *type)
{
  entity *res;
  ir_graph *rem;

  assert(!id_contains_char(name, ' ') && "entity name should not contain spaces");

  res = xmalloc(sizeof(*res));
  memset(res, 0, sizeof(*res));
  res->kind = k_entity;
  res->owner = owner;
  res->name = name;
  res->type = type;

  if (get_type_tpop(type) == type_method)
    res->allocation = allocation_static;
  else
    res->allocation = allocation_automatic;

  res->visibility = visibility_local;
  res->offset = -1;
  if (is_method_type(type)) {
    symconst_symbol sym;
    sym.entity_p = res;
    res->variability = variability_constant;
    rem = current_ir_graph;
    current_ir_graph = get_const_code_irg();
    res->value = new_SymConst(sym, symconst_addr_ent);
    current_ir_graph = rem;
  } else {
    res->variability = variability_uninitialized;
    res->value  = NULL;
    res->values = NULL;
    res->val_paths = NULL;
  }
  res->peculiarity   = peculiarity_existent;
  res->volatility    = volatility_non_volatile;
  res->stickyness    = stickyness_unsticky;
  res->ld_name       = NULL;
  if (is_class_type(owner)) {
    res->overwrites    = NEW_ARR_F(entity *, 0);
    res->overwrittenby = NEW_ARR_F(entity *, 0);
  } else {
    res->overwrites    = NULL;
    res->overwrittenby = NULL;
  }
  res->irg = NULL;

  //res->accesses = NULL;

#ifdef DEBUG_libfirm
  res->nr = get_irp_new_node_nr();
#endif /* DEBUG_libfirm */

  res->visit = 0;
  set_entity_dbg_info(res, db);

  return res;
}

entity *
new_d_entity (type *owner, ident *name, type *type, dbg_info *db) {
  entity *res;

  assert_legal_owner_of_ent(owner);
  res = new_rd_entity(db, owner, name, type);
  /* Remember entity in it's owner. */
  insert_entity_in_owner (res);

  return res;
}

entity *
new_entity (type *owner, ident *name, type *type) {
  return new_d_entity(owner, name, type, NULL);
}




static void free_entity_attrs(entity *ent) {
  int i;
  if (get_type_tpop(get_entity_owner(ent)) == type_class) {
    DEL_ARR_F(ent->overwrites);    ent->overwrites = NULL;
    DEL_ARR_F(ent->overwrittenby); ent->overwrittenby = NULL;
  } else {
    assert(ent->overwrites == NULL);
    assert(ent->overwrittenby == NULL);
  }
  /* if (ent->values) DEL_ARR_F(ent->values); *//* @@@ warum nich? */
  if (ent->val_paths) {
    if (is_compound_entity(ent))
      for (i = 0; i < get_compound_ent_n_values(ent); i++)
    if (ent->val_paths[i]) ;
    /* free_compound_graph_path(ent->val_paths[i]) ;  * @@@ warum nich? */
    /* Geht nich: wird mehrfach verwendet!!! ==> mehrfach frei gegeben. */
    /* DEL_ARR_F(ent->val_paths); */
  }
  ent->val_paths = NULL;
  ent->values = NULL;
}

entity *
copy_entity_own (entity *old, type *new_owner) {
  entity *newe;
  assert(old && old->kind == k_entity);
  assert_legal_owner_of_ent(new_owner);

  if (old->owner == new_owner) return old;
  newe = xmalloc(sizeof(*newe));
  memcpy (newe, old, sizeof(*newe));
  newe->owner = new_owner;
  if (is_class_type(new_owner)) {
    newe->overwrites    = NEW_ARR_F(entity *, 0);
    newe->overwrittenby = NEW_ARR_F(entity *, 0);
  }
#ifdef DEBUG_libfirm
  newe->nr = get_irp_new_node_nr();
#endif

  insert_entity_in_owner (newe);

  return newe;
}

entity *
copy_entity_name (entity *old, ident *new_name) {
  entity *newe;
  assert(old && old->kind == k_entity);

  if (old->name == new_name) return old;
  newe = xmalloc(sizeof(*newe));
  memcpy(newe, old, sizeof(*newe));
  newe->name = new_name;
  newe->ld_name = NULL;
  if (is_class_type(newe->owner)) {
    newe->overwrites    = DUP_ARR_F(entity *, old->overwrites);
    newe->overwrittenby = DUP_ARR_F(entity *, old->overwrittenby);
  }
#ifdef DEBUG_libfirm
  newe->nr = get_irp_new_node_nr();
#endif

  insert_entity_in_owner (newe);

  return newe;
}


void
free_entity (entity *ent) {
  assert(ent && ent->kind == k_entity);
  free_entity_attrs(ent);
  ent->kind = k_BAD;
  free(ent);
}

/* Outputs a unique number for this node */
long
get_entity_nr(entity *ent) {
  assert(ent && ent->kind == k_entity);
#ifdef DEBUG_libfirm
  return ent->nr;
#else
  return 0;
#endif
}

const char *
(get_entity_name)(const entity *ent) {
  return __get_entity_name(ent);
}

ident *
(get_entity_ident)(const entity *ent) {
  return get_entity_ident(ent);
}

/*
void   set_entitye_ld_name  (entity *, char *ld_name);
void   set_entity_ld_ident (entity *, ident *ld_ident);
*/

type *
(get_entity_owner)(entity *ent) {
  return __get_entity_owner(ent);
}

void
set_entity_owner (entity *ent, type *owner) {
  assert(ent && ent->kind == k_entity);
  assert_legal_owner_of_ent(owner);
  ent->owner = owner;
}

void   /* should this go into type.c? */
assert_legal_owner_of_ent(type *owner) {
  assert(get_type_tpop_code(owner) == tpo_class ||
          get_type_tpop_code(owner) == tpo_union ||
          get_type_tpop_code(owner) == tpo_struct ||
      get_type_tpop_code(owner) == tpo_array);   /* Yes, array has an entity
                            -- to select fields! */
}

ident *
(get_entity_ld_ident)(entity *ent) {
  return __get_entity_ld_ident(ent);
}

void
(set_entity_ld_ident)(entity *ent, ident *ld_ident) {
   __set_entity_ld_ident(ent, ld_ident);
}

const char *
(get_entity_ld_name)(entity *ent) {
  return __get_entity_ld_name(ent);
}

type *
(get_entity_type)(entity *ent) {
  return __get_entity_type(ent);
}

void
(set_entity_type)(entity *ent, type *type) {
  __set_entity_type(ent, type);
}

ent_allocation
(get_entity_allocation)(const entity *ent) {
  return __get_entity_allocation(ent);
}

void
(set_entity_allocation)(entity *ent, ent_allocation al) {
  __set_entity_allocation(ent, al);
}

/* return the name of the visibility */
const char *get_allocation_name(ent_allocation all)
{
#define X(a)    case a: return #a
  switch (all) {
    X(allocation_automatic);
    X(allocation_parameter);
    X(allocation_dynamic);
    X(allocation_static);
    default: return "BAD VALUE";
  }
#undef X
}


ent_visibility
(get_entity_visibility)(const entity *ent) {
  return __get_entity_visibility(ent);
}

void
set_entity_visibility (entity *ent, ent_visibility vis) {
  assert(ent && ent->kind == k_entity);
  if (vis != visibility_local)
    assert((ent->allocation == allocation_static) ||
       (ent->allocation == allocation_automatic));
  /* @@@ Test that the owner type is not local, but how??
         && get_class_visibility(get_entity_owner(ent)) != local));*/
  ent->visibility = vis;
}

/* return the name of the visibility */
const char *get_visibility_name(ent_visibility vis)
{
#define X(a)    case a: return #a
  switch (vis) {
    X(visibility_local);
    X(visibility_external_visible);
    X(visibility_external_allocated);
    default: return "BAD VALUE";
  }
#undef X
}

ent_variability
(get_entity_variability)(const entity *ent) {
  return __get_entity_variability(ent);
}

void
set_entity_variability (entity *ent, ent_variability var)
{
  assert(ent && ent->kind == k_entity);
  if (var == variability_part_constant)
    assert(is_class_type(ent->type) || is_struct_type(ent->type));

  if ((is_compound_type(ent->type)) &&
      (ent->variability == variability_uninitialized) && (var != variability_uninitialized)) {
    /* Allocate datastructures for constant values */
    ent->values    = NEW_ARR_F(ir_node *, 0);
    ent->val_paths = NEW_ARR_F(compound_graph_path *, 0);
  }
  if ((is_atomic_type(ent->type)) &&
      (ent->variability == variability_uninitialized) && (var != variability_uninitialized)) {
    /* Set default constant value. */
    ent->value = new_rd_Unknown(get_const_code_irg(), get_type_mode(ent->type));
  }

  if ((is_compound_type(ent->type)) &&
      (var == variability_uninitialized) && (ent->variability != variability_uninitialized)) {
    /* Free datastructures for constant values */
    DEL_ARR_F(ent->values);    ent->values    = NULL;
    DEL_ARR_F(ent->val_paths); ent->val_paths = NULL;
  }
  ent->variability = var;
}

/* return the name of the variablity */
const char *get_variability_name(ent_variability var)
{
#define X(a)    case a: return #a
  switch (var) {
    X(variability_uninitialized);
    X(variability_initialized);
    X(variability_part_constant);
    X(variability_constant);
    default: return "BAD VALUE";
  }
#undef X
}

ent_volatility
(get_entity_volatility)(const entity *ent) {
  return __get_entity_volatility(ent);
}

void
(set_entity_volatility)(entity *ent, ent_volatility vol) {
  __set_entity_volatility(ent, vol);
}

/* return the name of the volatility */
const char *get_volatility_name(ent_volatility var)
{
#define X(a)    case a: return #a
  switch (var) {
    X(volatility_non_volatile);
    X(volatility_is_volatile);
    default: return "BAD VALUE";
  }
#undef X
}

peculiarity
(get_entity_peculiarity)(const entity *ent) {
  return __get_entity_peculiarity(ent);
}

void
(set_entity_peculiarity)(entity *ent, peculiarity pec) {
  __set_entity_peculiarity(ent, pec);
}

/* return the name of the peculiarity */
const char *get_peculiarity_name(peculiarity var)
{
#define X(a)    case a: return #a
  switch (var) {
    X(peculiarity_description);
    X(peculiarity_inherited);
    X(peculiarity_existent);
    default: return "BAD VALUE";
  }
#undef X
}

/* Get the entity's stickyness */
ent_stickyness
(get_entity_stickyness)(const entity *ent) {
  return __get_entity_stickyness(ent);
}

/* Set the entity's stickyness */
void
(set_entity_stickyness)(entity *ent, ent_stickyness stickyness) {
  __set_entity_stickyness(ent, stickyness);
}

/* Set has no effect for existent entities of type method. */
ir_node *
get_atomic_ent_value(entity *ent)
{
  assert(ent && is_atomic_entity(ent));
  assert(ent->variability != variability_uninitialized);
  return skip_Id (ent->value);
}

void
set_atomic_ent_value(entity *ent, ir_node *val) {
  assert(is_atomic_entity(ent) && (ent->variability != variability_uninitialized));
  if (is_method_type(ent->type) && (ent->peculiarity == peculiarity_existent))
    return;
  ent->value = val;
}

/* Returns true if the the node is representable as code on
 *  const_code_irg. */
int is_irn_const_expression(ir_node *n) {
  ir_mode *m;

  /* we are in danger iff an exception will arise. TODO: be more precisely,
   * for instance Div. will NOT rise if divisor != 0
   */
  if (is_binop(n) && !is_fragile_op(n))
    return is_irn_const_expression(get_binop_left(n)) && is_irn_const_expression(get_binop_right(n));

  m = get_irn_mode(n);
  switch(get_irn_opcode(n)) {
  case iro_Const:
  case iro_SymConst:
  case iro_Unknown:
    return true; break;
  case iro_Conv:
  case iro_Cast:
    return is_irn_const_expression(get_irn_n(n, 0));
  default:
    return false;
    break;
  }
  return false;
}


ir_node *copy_const_value(ir_node *n) {
  ir_node *nn;
  ir_mode *m;

  /* @@@ GL I think  we should implement this using the routines from irgopt for
     dead node elimination/inlineing. */

  m = get_irn_mode(n);
  switch(get_irn_opcode(n)) {
  case iro_Const:
    nn = new_Const(m, get_Const_tarval(n));     set_Const_type(nn, get_Const_type(n));
    //nn = new_rd_Const_type(get_irn_dbg_info(n), current_ir_graph, get_cur_block(),
    //		   m,  get_Const_tarval(n), get_Const_type(n));
    break;
  case iro_SymConst:
    nn = new_d_SymConst_type(NULL, get_SymConst_symbol(n), get_SymConst_kind(n),
			     get_SymConst_value_type(n));
    break;
  case iro_Add:
    nn = new_Add(copy_const_value(get_Add_left(n)),
		 copy_const_value(get_Add_right(n)), m); break;
  case iro_Sub:
    nn = new_Sub(copy_const_value(get_Sub_left(n)),
		 copy_const_value(get_Sub_right(n)), m); break;
  case iro_Mul:
    nn = new_Mul(copy_const_value(get_Mul_left(n)),
		 copy_const_value(get_Mul_right(n)), m); break;
  case iro_And:
    nn = new_And(copy_const_value(get_And_left(n)),
		 copy_const_value(get_And_right(n)), m); break;
  case iro_Or:
    nn = new_Or(copy_const_value(get_Or_left(n)),
		copy_const_value(get_Or_right(n)), m); break;
  case iro_Eor:
    nn = new_Eor(copy_const_value(get_Eor_left(n)),
		 copy_const_value(get_Eor_right(n)), m); break;
  case iro_Cast:
    nn = new_Cast(copy_const_value(get_Cast_op(n)), get_Cast_type(n)); break;
  case iro_Conv:
    nn = new_Conv(copy_const_value(get_Conv_op(n)), m); break;
  case iro_Unknown:
    nn = new_Unknown(m); break;
  default:
    DDMN(n);
    assert(0 && "opcode invalid or not implemented");
    nn = NULL;
    break;
  }
  return nn;
}

compound_graph_path *
new_compound_graph_path(type *tp, int length) {
  compound_graph_path *res;

  assert(is_type(tp) && is_compound_type(tp));
  assert(length > 0);

  res = xmalloc(sizeof(*res) + (length-1) * sizeof(res->nodes[0]));
  memset(res, 0, sizeof(*res) + (length-1) * sizeof(res->nodes[0]));
  res->kind         = k_ir_compound_graph_path;
  res->tp           = tp;
  res->len          = length;
  res->arr_indicees = xcalloc(length, sizeof(res ->arr_indicees[0]));

  return res;
}

void
free_compound_graph_path (compound_graph_path *gr) {
  assert(gr && is_compound_graph_path(gr));
  gr->kind = k_BAD;
  free(gr ->arr_indicees);
  free(gr);
}

int
is_compound_graph_path(void *thing) {
  return (get_kind(thing) == k_ir_compound_graph_path);
}

/* checks whether nodes 0..pos are correct (all lie on a path.) */
/* @@@ not implemented */
int is_proper_compound_graph_path(compound_graph_path *gr, int pos) {
  int i;
  entity *node;
  type *owner = gr->tp;
  for (i = 0; i <= pos; i++) {
    node = get_compound_graph_path_node(gr, i);
    if (get_entity_owner(node) != owner) return false;
    owner = get_entity_type(node);
  }
  if (pos == get_compound_graph_path_length(gr))
    if (!is_atomic_type(owner)) return false;
  return true;
}

int
get_compound_graph_path_length(compound_graph_path *gr) {
  assert(gr && is_compound_graph_path(gr));
  return gr->len;
}

entity *
get_compound_graph_path_node(compound_graph_path *gr, int pos) {
  assert(gr && is_compound_graph_path(gr));
  assert(pos >= 0 && pos < gr->len);
  return gr->nodes[pos];
}

void
set_compound_graph_path_node(compound_graph_path *gr, int pos, entity *node) {
  assert(gr && is_compound_graph_path(gr));
  assert(pos >= 0 && pos < gr->len);
  assert(is_entity(node));
  gr->nodes[pos] = node;
  assert(is_proper_compound_graph_path(gr, pos));
}

int
get_compound_graph_path_array_index(compound_graph_path *gr, int pos) {
  assert(gr && is_compound_graph_path(gr));
  assert(pos >= 0 && pos < gr->len);
  return gr->arr_indicees[pos];
}

void
set_compound_graph_path_array_index(compound_graph_path *gr, int pos, int index) {
  assert(gr && is_compound_graph_path(gr));
  assert(pos >= 0 && pos < gr->len);
  gr->arr_indicees[pos] = index;
}

/* A value of a compound entity is a pair of value and the corresponding path to a member of
   the compound. */
void
add_compound_ent_value_w_path(entity *ent, ir_node *val, compound_graph_path *path) {
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  ARR_APP1 (ir_node *, ent->values, val);
  ARR_APP1 (compound_graph_path *, ent->val_paths, path);
}

void
set_compound_ent_value_w_path(entity *ent, ir_node *val, compound_graph_path *path, int pos) {
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  ent->values[pos] = val;
  ent->val_paths[pos] = path;
}

int
get_compound_ent_n_values(entity *ent) {
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  return (ARR_LEN (ent->values));
}

ir_node  *
get_compound_ent_value(entity *ent, int pos) {
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  return ent->values[pos];
}

compound_graph_path *
get_compound_ent_value_path(entity *ent, int pos) {
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  return ent->val_paths[pos];
}

void
remove_compound_ent_value(entity *ent, entity *value_ent) {
  int i;
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  for (i = 0; i < (ARR_LEN (ent->val_paths)); i++) {
    compound_graph_path *path = ent->val_paths[i];
    if (path->nodes[path->len-1] == value_ent) {
      for(; i < (ARR_LEN (ent->val_paths))-1; i++) {
        ent->val_paths[i] = ent->val_paths[i+1];
        ent->values[i]   = ent->values[i+1];
      }
      ARR_SETLEN(entity*,  ent->val_paths, ARR_LEN(ent->val_paths) - 1);
      ARR_SETLEN(ir_node*, ent->values,    ARR_LEN(ent->values)    - 1);
      break;
    }
  }
}

void
add_compound_ent_value(entity *ent, ir_node *val, entity *member) {
  compound_graph_path *path;
  type *owner_tp = get_entity_owner(ent);
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  path = new_compound_graph_path(owner_tp, 1);
  path->nodes[0] = member;
  if (is_array_type(owner_tp)) {
    int max;
    int i;

    assert(get_array_n_dimensions(owner_tp) == 1 && has_array_lower_bound(owner_tp, 0));
    max = get_array_lower_bound_int(owner_tp, 0) -1;
    for (i = 0; i < get_compound_ent_n_values(ent); ++i) {
      int index = get_compound_graph_path_array_index(get_compound_ent_value_path(ent, i), 0);
      if (index > max) {
        max = index;
      }
    }
    path->arr_indicees[0] = max + 1;
  }
  add_compound_ent_value_w_path(ent, val, path);
}

/* Copies the firm subgraph referenced by val to const_code_irg and adds
   the node as constant initialization to ent.
   The subgraph may not contain control flow operations.
void
copy_and_add_compound_ent_value(entity *ent, ir_node *val, entity *member) {
  ir_graph *rem = current_ir_graph;

  assert(get_entity_variability(ent) != variability_uninitialized);
  current_ir_graph = get_const_code_irg();

  val = copy_const_value(val);
  add_compound_ent_value(ent, val, member);
  current_ir_graph = rem;
  }*/

/* Copies the value i of the entity to current_block in current_ir_graph.
ir_node *
copy_compound_ent_value(entity *ent, int pos) {
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  return copy_const_value(ent->values[pos+1]);
  }*/

entity   *
get_compound_ent_value_member(entity *ent, int pos) {
  compound_graph_path *path;
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  path = get_compound_ent_value_path(ent, pos);

  return get_compound_graph_path_node(path, get_compound_graph_path_length(path)-1);
}

void
set_compound_ent_value(entity *ent, ir_node *val, entity *member, int pos) {
  compound_graph_path *path;
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  path = get_compound_ent_value_path(ent, pos);
  set_compound_graph_path_node(path, 0, member);
  set_compound_ent_value_w_path(ent, val, path, pos);
}

void
set_array_entity_values(entity *ent, tarval **values, int num_vals) {
  int i;
  ir_graph *rem = current_ir_graph;
  type *arrtp = get_entity_type(ent);
  ir_node *val;
  type *elttp = get_array_element_type(arrtp);

  assert(is_array_type(arrtp));
  assert(get_array_n_dimensions(arrtp) == 1);
  /* One bound is sufficient, the number of constant fields makes the
     size. */
  assert(get_array_lower_bound (arrtp, 0) || get_array_upper_bound (arrtp, 0));
  assert(get_entity_variability(ent) != variability_uninitialized);
  current_ir_graph = get_const_code_irg();

  for (i = 0; i < num_vals; i++) {
    val = new_Const_type(values[i], elttp);
    add_compound_ent_value(ent, val, get_array_element_entity(arrtp));
    set_compound_graph_path_array_index(get_compound_ent_value_path(ent, i), 0, i);
  }
  current_ir_graph = rem;
}

int  get_compound_ent_value_offset_bits(entity *ent, int pos) {
  compound_graph_path *path;
  int i, path_len;
  int offset = 0;

  assert(get_type_state(get_entity_type(ent)) == layout_fixed);

  path = get_compound_ent_value_path(ent, pos);
  path_len = get_compound_graph_path_length(path);

  for (i = 0; i < path_len; ++i) {
    entity *node = get_compound_graph_path_node(path, i);
    type *node_tp = get_entity_type(node);
    type *owner_tp = get_entity_owner(node);
    if (is_array_type(owner_tp)) {
      int size  = get_type_size_bits(node_tp);
      int align = get_type_alignment_bits(node_tp);
      if (size < align)
        size = align;
      else {
        assert(size % align == 0);
        /* ansonsten aufrunden */
      }
      offset += size * get_compound_graph_path_array_index(path, i);
    } else {
      offset += get_entity_offset_bits(node);
    }
  }
  return offset;
}

int  get_compound_ent_value_offset_bytes(entity *ent, int pos) {
  int offset = get_compound_ent_value_offset_bits(ent, pos);
  assert(offset % 8 == 0);
  return offset >> 3;
}


static void init_index(type *arr) {
  int init;
  int dim = 0;

  assert(get_array_n_dimensions(arr) == 1);

  if (has_array_lower_bound(arr, dim))
    init = get_array_lower_bound_int(arr, 0) -1;
  else
    init = get_array_upper_bound_int(arr, 0) +1;

  set_entity_link(get_array_element_entity(arr), (void *)init);
}


static int get_next_index(entity *elem_ent) {
  type *arr = get_entity_owner(elem_ent);
  int next;
  int dim = 0;

  assert(get_array_n_dimensions(arr) == 1);

  if (has_array_lower_bound(arr, dim)) {
    next = (int)get_entity_link(elem_ent) +1;
    if (has_array_upper_bound(arr, dim)) {
      int upper = get_array_upper_bound_int(arr, dim);
      if (next == upper) next = get_array_lower_bound_int(arr, dim);
    }
  } else {
    next = (int)get_entity_link(elem_ent) -1;
    if (has_array_lower_bound(arr, dim)) {
      int upper = get_array_upper_bound_int(arr, dim);
      if (next == upper) next = get_array_upper_bound_int(arr, dim);
    }
  }

  set_entity_link(elem_ent, (void *)next);
  return next;
}

/* Compute the array indices in compound graph paths of initialized entities.
 *
 *  All arrays must have fixed lower and upper bounds.  One array can
 *  have an open bound.  If there are several open bounds, we do
 *  nothing.  There must be initializer elements for all array
 *  elements.  Uses the link field in the array element entities.  The
 *  array bounds must be representable as ints.
 *
 *  (If the bounds are not representable as ints we have to represent
 *  the indices as firm nodes.  But the still we must be able to
 *  evaluate the index against the upper bound.)
 */
void compute_compound_ent_array_indicees(entity *ent) {
  type *tp = get_entity_type(ent);
  int i, n_vals;
  entity *unknown_bound_entity = NULL;

  if (!is_compound_type(tp) ||
      (ent->variability == variability_uninitialized)) return ;

  n_vals = get_compound_ent_n_values(ent);
  if (n_vals == 0) return;

  /* We can not compute the indexes if there is more than one array
     with an unknown bound.  For this remember the first entity that
     represents such an array. It could be ent. */
  if (is_array_type(tp)) {
    int dim = 0;

    assert(get_array_n_dimensions(tp) == 1 && "other not implemented");
    if (!has_array_lower_bound(tp, dim) || !has_array_upper_bound(tp, dim))
     unknown_bound_entity = ent;
  }

  /* Initialize the entity links to lower bound -1 and test all path elements
     for known bounds. */
  for (i = 0; i < n_vals; ++i) {
    compound_graph_path *path = get_compound_ent_value_path(ent, i);
    int j, path_len =  get_compound_graph_path_length(path);
    for (j = 0; j < path_len; ++j) {
      entity *node = get_compound_graph_path_node(path, j);
      type *elem_tp = get_entity_type(node);

      if (is_array_type(elem_tp)) {
        int dim = 0;
        assert(get_array_n_dimensions(elem_tp) == 1 && "other not implemented");
        if (!has_array_lower_bound(elem_tp, dim) || !has_array_upper_bound(elem_tp, dim)) {
          if (!unknown_bound_entity) unknown_bound_entity = node;
          if (node != unknown_bound_entity) return;
        }

        init_index(elem_tp);
      }
    }
  }

  /* Finally compute the indexes ... */
  for (i = 0; i < n_vals; ++i) {
    compound_graph_path *path = get_compound_ent_value_path(ent, i);
    int j, path_len =  get_compound_graph_path_length(path);
    for (j = 0; j < path_len; ++j) {
      entity *node = get_compound_graph_path_node(path, j);
      type *owner_tp = get_entity_owner(node);
      if (is_array_type(owner_tp))
        set_compound_graph_path_array_index (path, j, get_next_index(node));
    }
  }

}

/** resize: double the allocated buffer */
static int *resize (int *buf, int *size) {
  int new_size =  *size * 2;
  int *new_buf = xcalloc(new_size, sizeof(new_buf[0]));
  memcpy(new_buf, buf, *size);
  free(buf);
  *size = new_size;
  return new_buf;
}

/* We sort the elements by placing them at their bit offset in an
   array where each entry represents one bit called permutation.  In
   fact, we do not place the values themselves, as we would have to
   copy two things, the value and the path.  We only remember the
   position in the old order. Each value should have a distinct
   position in the permutation.

   A second iteration now permutes the actual elements into two
   new arrays. */
void sort_compound_ent_values(entity *ent) {
  type *tp;
  int i, n_vals;
  int tp_size;
  int size;
  int *permutation;

  int next;
  ir_node **my_values;
  compound_graph_path **my_paths;

  assert(get_type_state(get_entity_type(ent)) == layout_fixed);

  tp      = get_entity_type(ent);
  n_vals  = get_compound_ent_n_values(ent);
  tp_size = get_type_size_bits(tp);

  if (!is_compound_type(tp)                           ||
      (ent->variability == variability_uninitialized) ||
      (get_type_state(tp) != layout_fixed)            ||
      (n_vals == 0)                                     ) return;

  /* estimated upper bound for size. Better: use flexible array ... */
  size = ((tp_size > (n_vals * 32)) ? tp_size : (n_vals * 32)) * 4;
  permutation = xcalloc(size, sizeof(permutation[0]));

  for (i = 0; i < n_vals; ++i) {
    int pos = get_compound_ent_value_offset_bits(ent, i);
    while (pos >= size) {
      permutation = resize(permutation, &size);
    }
    assert(pos < size);
    assert(permutation[pos] == 0 && "two values with the same offset");
    permutation[pos] = i + 1;         /* We initialized with 0, so we can not distinguish entry 0.
                     So inc all entries by one. */
    //fprintf(stderr, "i: %d, pos: %d \n", i, pos);
  }

  next = 0;
  my_values = NEW_ARR_F(ir_node *, n_vals);
  my_paths  = NEW_ARR_F(compound_graph_path *, n_vals);
  for (i = 0; i < size; ++i) {
    int pos = permutation[i];
    if (pos) {
      //fprintf(stderr, "pos: %d i: %d  next %d \n", i, pos, next);
      assert(next < n_vals);
      pos--;   /* We increased the pos by one */
      my_values[next] = get_compound_ent_value     (ent, pos);
      my_paths [next] = get_compound_ent_value_path(ent, pos);
      next++;
    }
  }
  free(permutation);

  DEL_ARR_F(ent->values);
  ent->values = my_values;
  DEL_ARR_F(ent->val_paths);
  ent->val_paths = my_paths;
}

int
(get_entity_offset_bytes)(const entity *ent) {
  return __get_entity_offset_bytes(ent);
}

int
(get_entity_offset_bits)(const entity *ent) {
  return __get_entity_offset_bits(ent);
}

void
(set_entity_offset_bytes)(entity *ent, int offset) {
  __set_entity_offset_bytes(ent, offset);
}

void
(set_entity_offset_bits)(entity *ent, int offset) {
  __set_entity_offset_bits(ent, offset);
}

void
add_entity_overwrites(entity *ent, entity *overwritten) {
  assert(ent && is_class_type(get_entity_owner(ent)));
  ARR_APP1(entity *, ent->overwrites, overwritten);
  ARR_APP1(entity *, overwritten->overwrittenby, ent);
}

int
get_entity_n_overwrites(entity *ent) {
  assert(ent && is_class_type(get_entity_owner(ent)));
  return (ARR_LEN(ent->overwrites));
}

int
get_entity_overwrites_index(entity *ent, entity *overwritten) {
  int i;
  assert(ent && is_class_type(get_entity_owner(ent)));
  for (i = 0; i < get_entity_n_overwrites(ent); i++)
    if (get_entity_overwrites(ent, i) == overwritten)
      return i;
  return -1;
}

entity *
get_entity_overwrites   (entity *ent, int pos) {
  assert(ent && is_class_type(get_entity_owner(ent)));
  assert(pos < get_entity_n_overwrites(ent));
  return ent->overwrites[pos];
}

void
set_entity_overwrites   (entity *ent, int pos, entity *overwritten) {
  assert(ent && is_class_type(get_entity_owner(ent)));
  assert(pos < get_entity_n_overwrites(ent));
  ent->overwrites[pos] = overwritten;
}

void
remove_entity_overwrites(entity *ent, entity *overwritten) {
  int i;
  assert(ent && is_class_type(get_entity_owner(ent)));
  for (i = 0; i < (ARR_LEN (ent->overwrites)); i++)
    if (ent->overwrites[i] == overwritten) {
      for(; i < (ARR_LEN (ent->overwrites))-1; i++)
    ent->overwrites[i] = ent->overwrites[i+1];
      ARR_SETLEN(entity*, ent->overwrites, ARR_LEN(ent->overwrites) - 1);
      break;
    }
}

void
add_entity_overwrittenby   (entity *ent, entity *overwrites) {
  assert(ent && is_class_type(get_entity_owner(ent)));
  add_entity_overwrites(overwrites, ent);
}

int
get_entity_n_overwrittenby (entity *ent) {
  assert(ent && is_class_type(get_entity_owner(ent)));
  return (ARR_LEN (ent->overwrittenby));
}

int
get_entity_overwrittenby_index(entity *ent, entity *overwrites) {
  int i;
  assert(ent && is_class_type(get_entity_owner(ent)));
  for (i = 0; i < get_entity_n_overwrittenby(ent); i++)
    if (get_entity_overwrittenby(ent, i) == overwrites)
      return i;
  return -1;
}

entity *
get_entity_overwrittenby   (entity *ent, int pos) {
  assert(ent && is_class_type(get_entity_owner(ent)));
  assert(pos < get_entity_n_overwrittenby(ent));
  return ent->overwrittenby[pos];
}

void
set_entity_overwrittenby   (entity *ent, int pos, entity *overwrites) {
  assert(ent && is_class_type(get_entity_owner(ent)));
  assert(pos < get_entity_n_overwrittenby(ent));
  ent->overwrittenby[pos] = overwrites;
}

void    remove_entity_overwrittenby(entity *ent, entity *overwrites) {
  int i;
  assert(ent  && is_class_type(get_entity_owner(ent)));
  for (i = 0; i < (ARR_LEN (ent->overwrittenby)); i++)
    if (ent->overwrittenby[i] == overwrites) {
      for(; i < (ARR_LEN (ent->overwrittenby))-1; i++)
    ent->overwrittenby[i] = ent->overwrittenby[i+1];
      ARR_SETLEN(entity*, ent->overwrittenby, ARR_LEN(ent->overwrittenby) - 1);
      break;
    }
}

/* A link to store intermediate information */
void *
(get_entity_link)(const entity *ent) {
  return __get_entity_link(ent);
}

void
(set_entity_link)(entity *ent, void *l) {
  __set_entity_link(ent, l);
}

ir_graph *
(get_entity_irg)(const entity *ent) {
  return __get_entity_irg(ent);
}

void
set_entity_irg(entity *ent, ir_graph *irg) {
  assert(ent && is_method_type(get_entity_type(ent)));
  /* Wie kann man die Referenz auf einen IRG löschen, z.B. wenn die
   * Methode selbst nicht mehr aufgerufen werden kann, die Entität
   * aber erhalten bleiben soll?  Wandle die Entitaet in description oder
   * inherited um! */
  /* assert(irg); */
  assert((irg  && ent->peculiarity == peculiarity_existent) ||
	 (!irg && (ent->peculiarity == peculiarity_existent)
	  && (ent -> visibility == visibility_external_allocated)) ||
         (!irg && ent->peculiarity == peculiarity_description) ||
         (!irg && ent->peculiarity == peculiarity_inherited));
  ent->irg = irg;
}

int
(is_entity)(const void *thing) {
  return __is_entity(thing);
}

int is_atomic_entity(entity *ent) {
  type* t = get_entity_type(ent);
  assert(ent && ent->kind == k_entity);
  return (is_primitive_type(t) || is_pointer_type(t) ||
      is_enumeration_type(t) || is_method_type(t));
}

int is_compound_entity(entity *ent) {
  type* t = get_entity_type(ent);
  assert(ent && ent->kind == k_entity);
  return (is_class_type(t) || is_struct_type(t) ||
      is_array_type(t) || is_union_type(t));
}

/**
 * @todo not implemented!!! */
bool equal_entity(entity *ent1, entity *ent2) {
  fprintf(stderr, " calling unimplemented equal entity!!! \n");
  return true;
}


unsigned long get_entity_visited(entity *ent) {
  assert(ent && ent->kind == k_entity);
  return ent->visit;
}
void        set_entity_visited(entity *ent, unsigned long num) {
  assert(ent && ent->kind == k_entity);
  ent->visit = num;
}
/* Sets visited field in entity to entity_visited. */
void        mark_entity_visited(entity *ent) {
  assert(ent && ent->kind == k_entity);
  ent->visit = type_visited;
}


bool entity_visited(entity *ent) {
  assert(ent && ent->kind == k_entity);
  return get_entity_visited(ent) >= type_visited;
}

bool entity_not_visited(entity *ent) {
  assert(ent && ent->kind == k_entity);
  return get_entity_visited(ent) < type_visited;
}

/* Need two routines because I want to assert the result. */
static entity *resolve_ent_polymorphy2 (type *dynamic_class, entity *static_ent) {
  int i, n_overwrittenby;
  entity *res = NULL;

  if (get_entity_owner(static_ent) == dynamic_class) return static_ent;

  n_overwrittenby = get_entity_n_overwrittenby(static_ent);
  for (i = 0; i < n_overwrittenby; ++i) {
    res = resolve_ent_polymorphy2(dynamic_class, get_entity_overwrittenby(static_ent, i));
    if (res)
      break;
  }

  return res;
}

/** Resolve polymorphy in the inheritance relation.
 *
 * Returns the dynamically referenced entity if the static entity and the
 * dynamic type are given.
 * Search downwards in overwritten tree. */
entity *resolve_ent_polymorphy(type *dynamic_class, entity *static_ent) {
  entity *res;
  assert(static_ent && static_ent->kind == k_entity);

  res = resolve_ent_polymorphy2(dynamic_class, static_ent);
#if DEBUG_libfirm
  if (!res) {
    printf(" Could not find entity "); DDME(static_ent);
    printf("  in "); DDMT(dynamic_class);
    printf("\n");
    dump_entity(static_ent);
    dump_type(get_entity_owner(static_ent));
    dump_type(dynamic_class);
  }
#endif
  assert(res);
  return res;
}
