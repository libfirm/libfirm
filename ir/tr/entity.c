/*
 * Project:     libFIRM
 * File name:   ir/tr/entity.c
 * Purpose:     Representation of all program known entities.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier, Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STDDEF_H
# include <stddef.h>
#endif
#ifdef HAVE_MALLOC_H
# include <malloc.h>
#endif
#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif

#include "firm_common_t.h"

#include "xmalloc.h"
#include "entity_t.h"
#include "mangle.h"
#include "typegmod.h"
#include "array.h"
#include "irtools.h"
#include "irhooks.h"
#include "irprintf.h"

/* All this is needed to build the constant node for methods: */
#include "irprog_t.h"
#include "ircons.h"
#include "tv_t.h"
#include "irdump.h"  /* for output if errors occur. */

#include "callgraph.h"  /* for dumping debug output */

/*******************************************************************/
/** general                                                       **/
/*******************************************************************/

ir_entity *unknown_entity = NULL;

ir_entity *get_unknown_entity(void) { return unknown_entity; }

#define UNKNOWN_ENTITY_NAME "unknown_entity"

/*-----------------------------------------------------------------*/
/* ENTITY                                                          */
/*-----------------------------------------------------------------*/

static INLINE void insert_entity_in_owner(ir_entity *ent) {
  ir_type *owner = ent->owner;
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
 * Creates a new entity. This entity is NOT inserted in the owner type.
 *
 * @param db     debug info for this entity
 * @param owner  the owner type of the new entity
 * @param name   the name of the new entity
 * @param type   the type of the new entity
 *
 * @return the new created entity
 */
static INLINE ir_entity *
new_rd_entity(dbg_info *db, ir_type *owner, ident *name, ir_type *type)
{
  ir_entity *res;
  ir_graph *rem;

  assert(!id_contains_char(name, ' ') && "entity name should not contain spaces");

  res = xmalloc(sizeof(*res));
  memset(res, 0, sizeof(*res));

  res->kind    = k_entity;
  res->name    = name;
  res->ld_name = NULL;
  res->type    = type;
  res->owner   = owner;

  res->allocation           = allocation_automatic;
  res->visibility           = visibility_local;
  res->volatility           = volatility_non_volatile;
  res->stickyness           = stickyness_unsticky;
  res->peculiarity          = peculiarity_existent;
  res->final                = 0;
  res->compiler_gen         = 0;
  res->offset               = -1;
  res->offset_bit_remainder = 0;
  res->link                 = NULL;
  res->repr_class           = NULL;

  if (is_Method_type(type)) {
    symconst_symbol sym;
    sym.entity_p            = res;
    rem                     = current_ir_graph;
    current_ir_graph        = get_const_code_irg();
    res->value              = new_SymConst(sym, symconst_addr_ent);
    current_ir_graph        = rem;
    res->allocation         = allocation_static;
    res->variability        = variability_constant;
    res->attr.mtd_attr.irg_add_properties = mtp_property_inherited;
    res->attr.mtd_attr.vtable_number      = VTABLE_NUM_NOT_SET;
    res->attr.mtd_attr.param_access       = NULL;
    res->attr.mtd_attr.param_weight       = NULL;
    res->attr.mtd_attr.irg                = NULL;
    res->attr.mtd_attr.section            = section_text;
  }
  else if (is_compound_type(type)) {
    res->variability = variability_uninitialized;
    res->value       = NULL;
    res->attr.cmpd_attr.values    = NULL;
    res->attr.cmpd_attr.val_paths = NULL;
  }
  else {
    res->variability = variability_uninitialized;
    res->value       = NULL;
  }

  if (is_Class_type(owner)) {
    res->overwrites    = NEW_ARR_F(ir_entity *, 0);
    res->overwrittenby = NEW_ARR_F(ir_entity *, 0);
  } else {
    res->overwrites    = NULL;
    res->overwrittenby = NULL;
  }

#ifdef DEBUG_libfirm
  res->nr = get_irp_new_node_nr();
#endif /* DEBUG_libfirm */

  res->visit = 0;
  set_entity_dbg_info(res, db);

  return res;
}

ir_entity *
new_d_entity(ir_type *owner, ident *name, ir_type *type, dbg_info *db) {
  ir_entity *res;

  assert(is_compound_type(owner));
  res = new_rd_entity(db, owner, name, type);
  /* Remember entity in it's owner. */
  insert_entity_in_owner(res);

  hook_new_entity(res);
  return res;
}

ir_entity *
new_entity(ir_type *owner, ident *name, ir_type *type) {
  return new_d_entity(owner, name, type, NULL);
}

/**
 * Free entity attributes.
 *
 * @param ent  the entity
 */
static void free_entity_attrs(ir_entity *ent) {
  int i;
  if (get_type_tpop(get_entity_owner(ent)) == type_class) {
    DEL_ARR_F(ent->overwrites);    ent->overwrites = NULL;
    DEL_ARR_F(ent->overwrittenby); ent->overwrittenby = NULL;
  } else {
    assert(ent->overwrites == NULL);
    assert(ent->overwrittenby == NULL);
  }
  if (is_compound_entity(ent)) {
    if (ent->attr.cmpd_attr.val_paths) {
      for (i = 0; i < get_compound_ent_n_values(ent); i++)
        if (ent->attr.cmpd_attr.val_paths[i]) {
          /* free_compound_graph_path(ent->attr.cmpd_attr.val_paths[i]) ;  * @@@ warum nich? */
          /* Geht nich: wird mehrfach verwendet!!! ==> mehrfach frei gegeben. */
          /* DEL_ARR_F(ent->attr.cmpd_attr.val_paths); */
        }
      ent->attr.cmpd_attr.val_paths = NULL;
    }
    /* if (ent->attr.cmpd_attr.values) DEL_ARR_F(ent->attr.cmpd_attr.values); *//* @@@ warum nich? */
    ent->attr.cmpd_attr.values = NULL;
  }
  else if (is_method_entity(ent)) {
    if (ent->attr.mtd_attr.param_access) {
      DEL_ARR_F(ent->attr.mtd_attr.param_access);
      ent->attr.mtd_attr.param_access = NULL;
    }
    if (ent->attr.mtd_attr.param_weight) {
      DEL_ARR_F(ent->attr.mtd_attr.param_weight);
      ent->attr.mtd_attr.param_weight = NULL;
    }
  }
}

ir_entity *
copy_entity_own(ir_entity *old, ir_type *new_owner) {
  ir_entity *newe;
  assert(is_entity(old));
  assert(is_compound_type(new_owner));

  if (old->owner == new_owner) return old;
  newe = xmalloc(sizeof(*newe));
  memcpy(newe, old, sizeof(*newe));
  newe->owner = new_owner;
  if (is_Class_type(new_owner)) {
    newe->overwrites    = NEW_ARR_F(ir_entity *, 0);
    newe->overwrittenby = NEW_ARR_F(ir_entity *, 0);
  }
#ifdef DEBUG_libfirm
  newe->nr = get_irp_new_node_nr();
#endif

  insert_entity_in_owner(newe);

  return newe;
}

ir_entity *
copy_entity_name(ir_entity *old, ident *new_name) {
  ir_entity *newe;
  assert(old && old->kind == k_entity);

  if (old->name == new_name) return old;
  newe = xmalloc(sizeof(*newe));
  memcpy(newe, old, sizeof(*newe));
  newe->name = new_name;
  newe->ld_name = NULL;
  if (is_Class_type(newe->owner)) {
    newe->overwrites    = DUP_ARR_F(ir_entity *, old->overwrites);
    newe->overwrittenby = DUP_ARR_F(ir_entity *, old->overwrittenby);
  }
#ifdef DEBUG_libfirm
  newe->nr = get_irp_new_node_nr();
#endif

  insert_entity_in_owner (newe);

  return newe;
}


void
free_entity (ir_entity *ent) {
  assert(ent && ent->kind == k_entity);
  free_entity_attrs(ent);
  ent->kind = k_BAD;
  free(ent);
}

/* Outputs a unique number for this node */
long
get_entity_nr(ir_entity *ent) {
  assert(ent && ent->kind == k_entity);
#ifdef DEBUG_libfirm
  return ent->nr;
#else
  return (long)PTR_TO_INT(ent);
#endif
}

const char *
(get_entity_name)(const ir_entity *ent) {
  return _get_entity_name(ent);
}

ident *
(get_entity_ident)(const ir_entity *ent) {
  return _get_entity_ident(ent);
}

void
(set_entity_ident)(ir_entity *ent, ident *id) {
  _set_entity_ident(ent, id);
}

ir_type *
(get_entity_owner)(ir_entity *ent) {
  return _get_entity_owner(ent);
}

void
set_entity_owner(ir_entity *ent, ir_type *owner) {
  assert(is_entity(ent));
  assert(is_compound_type(owner));
  ent->owner = owner;
}

ident *
(get_entity_ld_ident)(ir_entity *ent) {
  return _get_entity_ld_ident(ent);
}

void
(set_entity_ld_ident)(ir_entity *ent, ident *ld_ident) {
   _set_entity_ld_ident(ent, ld_ident);
}

const char *
(get_entity_ld_name)(ir_entity *ent) {
  return _get_entity_ld_name(ent);
}

ir_type *
(get_entity_type)(ir_entity *ent) {
  return _get_entity_type(ent);
}

void
(set_entity_type)(ir_entity *ent, ir_type *type) {
  _set_entity_type(ent, type);
}

ir_allocation
(get_entity_allocation)(const ir_entity *ent) {
  return _get_entity_allocation(ent);
}

void
(set_entity_allocation)(ir_entity *ent, ir_allocation al) {
  _set_entity_allocation(ent, al);
}

/* return the name of the visibility */
const char *get_allocation_name(ir_allocation all)
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


ir_visibility
(get_entity_visibility)(const ir_entity *ent) {
  return _get_entity_visibility(ent);
}

void
set_entity_visibility(ir_entity *ent, ir_visibility vis) {
  assert(ent && ent->kind == k_entity);
  if (vis != visibility_local)
    assert((ent->allocation == allocation_static) ||
       (ent->allocation == allocation_automatic));
  /* @@@ Test that the owner type is not local, but how??
         && get_class_visibility(get_entity_owner(ent)) != local));*/
  ent->visibility = vis;
}

/* return the name of the visibility */
const char *get_visibility_name(ir_visibility vis)
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

ir_variability
(get_entity_variability)(const ir_entity *ent) {
  return _get_entity_variability(ent);
}

void
set_entity_variability (ir_entity *ent, ir_variability var)
{
  assert(ent && ent->kind == k_entity);
  if (var == variability_part_constant)
    assert(is_Class_type(ent->type) || is_Struct_type(ent->type));

  if ((is_compound_type(ent->type)) &&
      (ent->variability == variability_uninitialized) && (var != variability_uninitialized)) {
    /* Allocate data structures for constant values */
    ent->attr.cmpd_attr.values    = NEW_ARR_F(ir_node *, 0);
    ent->attr.cmpd_attr.val_paths = NEW_ARR_F(compound_graph_path *, 0);
  }
  if ((is_atomic_type(ent->type)) &&
      (ent->variability == variability_uninitialized) && (var != variability_uninitialized)) {
    /* Set default constant value. */
    ent->value = new_rd_Unknown(get_const_code_irg(), get_type_mode(ent->type));
  }

  if ((is_compound_type(ent->type)) &&
      (var == variability_uninitialized) && (ent->variability != variability_uninitialized)) {
    /* Free data structures for constant values */
    DEL_ARR_F(ent->attr.cmpd_attr.values);    ent->attr.cmpd_attr.values    = NULL;
    DEL_ARR_F(ent->attr.cmpd_attr.val_paths); ent->attr.cmpd_attr.val_paths = NULL;
  }
  ent->variability = var;
}

/* return the name of the variability */
const char *get_variability_name(ir_variability var)
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

ir_volatility
(get_entity_volatility)(const ir_entity *ent) {
  return _get_entity_volatility(ent);
}

void
(set_entity_volatility)(ir_entity *ent, ir_volatility vol) {
  _set_entity_volatility(ent, vol);
}

/* return the name of the volatility */
const char *get_volatility_name(ir_volatility var)
{
#define X(a)    case a: return #a
  switch (var) {
    X(volatility_non_volatile);
    X(volatility_is_volatile);
    default: return "BAD VALUE";
  }
#undef X
}

ir_peculiarity
(get_entity_peculiarity)(const ir_entity *ent) {
  return _get_entity_peculiarity(ent);
}

void
(set_entity_peculiarity)(ir_entity *ent, ir_peculiarity pec) {
  _set_entity_peculiarity(ent, pec);
}

/* Checks if an entity cannot be overridden anymore. */
int (get_entity_final)(const ir_entity *ent) {
  return _get_entity_final(ent);
}

/* Sets/resets the final flag of an entity. */
void (set_entity_final)(ir_entity *ent, int final) {
  _set_entity_final(ent, final);
}

/* Checks if an entity is compiler generated */
int is_entity_compiler_generated(const ir_entity *ent) {
  assert(is_entity(ent));
  return ent->compiler_gen;
}

/* Sets/resets the compiler generated flag */
void set_entity_compiler_generated(ir_entity *ent, int flag) {
  assert(is_entity(ent));
  ent->compiler_gen = flag ? 1 : 0;
}

/* Get the entity's stickyness */
ir_stickyness
(get_entity_stickyness)(const ir_entity *ent) {
  return _get_entity_stickyness(ent);
}

/* Set the entity's stickyness */
void
(set_entity_stickyness)(ir_entity *ent, ir_stickyness stickyness) {
  _set_entity_stickyness(ent, stickyness);
}

/* Set has no effect for existent entities of type method. */
ir_node *
get_atomic_ent_value(ir_entity *ent)
{
  assert(ent && is_atomic_entity(ent));
  assert(ent->variability != variability_uninitialized);
  return skip_Id (ent->value);
}

void
set_atomic_ent_value(ir_entity *ent, ir_node *val) {
  assert(is_atomic_entity(ent) && (ent->variability != variability_uninitialized));
  if (is_Method_type(ent->type) && (ent->peculiarity == peculiarity_existent))
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
    return 1;
  case iro_Conv:
  case iro_Cast:
    return is_irn_const_expression(get_irn_n(n, 0));
  default:
    break;
  }
  return 0;
}

/*
 * Copies a firm subgraph that complies to the restrictions for
 * constant expressions to current_block in current_ir_graph.
 */
ir_node *copy_const_value(dbg_info *dbg, ir_node *n) {
  ir_node *nn;
  ir_mode *m;

  /* @@@ GL I think  we should implement this using the routines from irgopt for
     dead node elimination/inlineing. */

  m = get_irn_mode(n);
  switch (get_irn_opcode(n)) {
  case iro_Const:
    nn = new_d_Const_type(dbg, m, get_Const_tarval(n), get_Const_type(n));
    break;
  case iro_SymConst:
    nn = new_d_SymConst_type(dbg, get_SymConst_symbol(n), get_SymConst_kind(n),
			     get_SymConst_value_type(n));
    break;
  case iro_Add:
    nn = new_d_Add(dbg, copy_const_value(dbg, get_Add_left(n)),
		 copy_const_value(dbg, get_Add_right(n)), m); break;
  case iro_Sub:
    nn = new_d_Sub(dbg, copy_const_value(dbg, get_Sub_left(n)),
		 copy_const_value(dbg, get_Sub_right(n)), m); break;
  case iro_Mul:
    nn = new_d_Mul(dbg, copy_const_value(dbg, get_Mul_left(n)),
		 copy_const_value(dbg, get_Mul_right(n)), m); break;
  case iro_And:
    nn = new_d_And(dbg, copy_const_value(dbg, get_And_left(n)),
		 copy_const_value(dbg, get_And_right(n)), m); break;
  case iro_Or:
    nn = new_d_Or(dbg, copy_const_value(dbg, get_Or_left(n)),
		copy_const_value(dbg, get_Or_right(n)), m); break;
  case iro_Eor:
    nn = new_d_Eor(dbg, copy_const_value(dbg, get_Eor_left(n)),
		 copy_const_value(dbg, get_Eor_right(n)), m); break;
  case iro_Cast:
    nn = new_d_Cast(dbg, copy_const_value(dbg, get_Cast_op(n)), get_Cast_type(n)); break;
  case iro_Conv:
    nn = new_d_Conv(dbg, copy_const_value(dbg, get_Conv_op(n)), m); break;
  case iro_Unknown:
    nn = new_d_Unknown(m); break;
  default:
    DDMN(n);
    assert(0 && "opcode invalid or not implemented");
    nn = NULL;
    break;
  }
  return nn;
}

/* Creates a new compound graph path. */
compound_graph_path *
new_compound_graph_path(ir_type *tp, int length) {
  compound_graph_path *res;

  assert(is_compound_type(tp));
  assert(length > 0);

  res = xmalloc(sizeof(*res) + (length-1) * sizeof(res->list[0]));
  memset(res, 0, sizeof(*res) + (length-1) * sizeof(res->list[0]));
  res->kind         = k_ir_compound_graph_path;
  res->tp           = tp;
  res->len          = length;

  return res;
}

/* Frees an graph path object */
void free_compound_graph_path (compound_graph_path *gr) {
  assert(gr && is_compound_graph_path(gr));
  gr->kind = k_BAD;
  free(gr);
}

/* Returns non-zero if an object is a compound graph path */
int is_compound_graph_path(const void *thing) {
  return (get_kind(thing) == k_ir_compound_graph_path);
}

/* Checks whether the path up to pos is correct. If the path contains a NULL,
 *  assumes the path is not complete and returns 'true'. */
int is_proper_compound_graph_path(compound_graph_path *gr, int pos) {
  int i;
  ir_entity *node;
  ir_type *owner = gr->tp;

  for (i = 0; i <= pos; i++) {
    node = get_compound_graph_path_node(gr, i);
    if (node == NULL)
      /* Path not yet complete. */
      return 1;
    if (get_entity_owner(node) != owner)
      return 0;
    owner = get_entity_type(node);
  }
  if (pos == get_compound_graph_path_length(gr))
    if (!is_atomic_type(owner))
      return 0;
  return 1;
}

/* Returns the length of a graph path */
int get_compound_graph_path_length(const compound_graph_path *gr) {
  assert(gr && is_compound_graph_path(gr));
  return gr->len;
}

ir_entity *
get_compound_graph_path_node(const compound_graph_path *gr, int pos) {
  assert(gr && is_compound_graph_path(gr));
  assert(pos >= 0 && pos < gr->len);
  return gr->list[pos].node;
}

void
set_compound_graph_path_node(compound_graph_path *gr, int pos, ir_entity *node) {
  assert(gr && is_compound_graph_path(gr));
  assert(pos >= 0 && pos < gr->len);
  assert(is_entity(node));
  gr->list[pos].node = node;
  assert(is_proper_compound_graph_path(gr, pos));
}

int
get_compound_graph_path_array_index(const compound_graph_path *gr, int pos) {
  assert(gr && is_compound_graph_path(gr));
  assert(pos >= 0 && pos < gr->len);
  return gr->list[pos].index;
}

void
set_compound_graph_path_array_index(compound_graph_path *gr, int pos, int index) {
  assert(gr && is_compound_graph_path(gr));
  assert(pos >= 0 && pos < gr->len);
  gr->list[pos].index = index;
}

/* A value of a compound entity is a pair of value and the corresponding path to a member of
   the compound. */
void
add_compound_ent_value_w_path(ir_entity *ent, ir_node *val, compound_graph_path *path) {
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  assert(is_compound_graph_path(path));
  ARR_APP1(ir_node *, ent->attr.cmpd_attr.values, val);
  ARR_APP1(compound_graph_path *, ent->attr.cmpd_attr.val_paths, path);
}

void
set_compound_ent_value_w_path(ir_entity *ent, ir_node *val, compound_graph_path *path, int pos) {
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  assert(is_compound_graph_path(path));
  assert(0 <= pos && pos < ARR_LEN(ent->attr.cmpd_attr.values));
  ent->attr.cmpd_attr.values[pos]    = val;
  ent->attr.cmpd_attr.val_paths[pos] = path;
}

int
get_compound_ent_n_values(ir_entity *ent) {
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  return ARR_LEN(ent->attr.cmpd_attr.values);
}

ir_node *
get_compound_ent_value(ir_entity *ent, int pos) {
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  assert(0 <= pos && pos < ARR_LEN(ent->attr.cmpd_attr.values));
  return ent->attr.cmpd_attr.values[pos];
}

compound_graph_path *
get_compound_ent_value_path(ir_entity *ent, int pos) {
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  assert(0 <= pos && pos < ARR_LEN(ent->attr.cmpd_attr.val_paths));
  return ent->attr.cmpd_attr.val_paths[pos];
}

/**
 * Returns non-zero, if two compound_graph_pathes are equal
 */
static int equal_paths(compound_graph_path *path1, int *visited_indices, compound_graph_path *path2) {
  int i;
  int len1 = get_compound_graph_path_length(path1);
  int len2 = get_compound_graph_path_length(path2);

  if (len2 > len1) return 0;

  for (i = 0; i < len1; i++) {
    ir_type *tp;
    ir_entity *node1 = get_compound_graph_path_node(path1, i);
    ir_entity *node2 = get_compound_graph_path_node(path2, i);

    if (node1 != node2) return 0;

    tp = get_entity_owner(node1);
    if (is_Array_type(tp)) {
      long low;

      /* Compute the index of this node. */
      assert(get_array_n_dimensions(tp) == 1 && "multidim not implemented");

      low = get_array_lower_bound_int(tp, 0);
      if (low + visited_indices[i] < get_compound_graph_path_array_index(path2, i)) {
        visited_indices[i]++;
        return 0;
      }
      else
        assert(low + visited_indices[i] == get_compound_graph_path_array_index(path2, i));
    }
  }
  return 1;
}

/* Returns the position of a value with the given path.
 *  The path must contain array indices for all array element entities. */
int get_compound_ent_pos_by_path(ir_entity *ent, compound_graph_path *path) {
  int i, n_paths = get_compound_ent_n_values(ent);
  int *visited_indices;
  int path_len = get_compound_graph_path_length(path);

  NEW_ARR_A(int *, visited_indices, path_len);
  memset(visited_indices, 0, sizeof(*visited_indices) * path_len);
  for (i = 0; i < n_paths; i ++) {
    if (equal_paths(get_compound_ent_value_path(ent, i), visited_indices, path))
      return i;
  }

#if 0
  {
    int j;
    printf(">>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
      printf("Entity %s : ", get_entity_name(ent));
      for (j = 0; j < get_compound_graph_path_length(path); ++j) {
        ir_entity *node = get_compound_graph_path_node(path, j);
        printf("%s", get_entity_name(node));
        if (is_Array_type(get_entity_owner(node)))
          printf("[%d]", get_compound_graph_path_array_index(path, j));
      }
    printf(">>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
  }
#endif

  assert(0 && "path not found");
  return -1;
}

/* Returns a constant value given the access path.
 *  The path must contain array indices for all array element entities. */
ir_node *get_compound_ent_value_by_path(ir_entity *ent, compound_graph_path *path) {
  return get_compound_ent_value(ent, get_compound_ent_pos_by_path(ent, path));
}


void
remove_compound_ent_value(ir_entity *ent, ir_entity *value_ent) {
  int i;
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  for (i = 0; i < (ARR_LEN(ent->attr.cmpd_attr.val_paths)); ++i) {
    compound_graph_path *path = ent->attr.cmpd_attr.val_paths[i];
    if (path->list[path->len-1].node == value_ent) {
      for (; i < (ARR_LEN(ent->attr.cmpd_attr.val_paths))-1; ++i) {
        ent->attr.cmpd_attr.val_paths[i] = ent->attr.cmpd_attr.val_paths[i+1];
        ent->attr.cmpd_attr.values[i]    = ent->attr.cmpd_attr.values[i+1];
      }
      ARR_SETLEN(ir_entity*,  ent->attr.cmpd_attr.val_paths, ARR_LEN(ent->attr.cmpd_attr.val_paths) - 1);
      ARR_SETLEN(ir_node*, ent->attr.cmpd_attr.values,    ARR_LEN(ent->attr.cmpd_attr.values)    - 1);
      break;
    }
  }
}

void
add_compound_ent_value(ir_entity *ent, ir_node *val, ir_entity *member) {
  compound_graph_path *path;
  ir_type *owner_tp = get_entity_owner(member);
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  path = new_compound_graph_path(get_entity_type(ent), 1);
  path->list[0].node = member;
  if (is_Array_type(owner_tp)) {
    int max;
    int i, n;

    assert(get_array_n_dimensions(owner_tp) == 1 && has_array_lower_bound(owner_tp, 0));
    max = get_array_lower_bound_int(owner_tp, 0) -1;
    for (i = 0, n = get_compound_ent_n_values(ent); i < n; ++i) {
      int index = get_compound_graph_path_array_index(get_compound_ent_value_path(ent, i), 0);
      if (index > max) {
        max = index;
      }
    }
    path->list[0].index = max + 1;
  }
  add_compound_ent_value_w_path(ent, val, path);
}

/* Copies the firm subgraph referenced by val to const_code_irg and adds
   the node as constant initialization to ent.
   The subgraph may not contain control flow operations.
void
copy_and_add_compound_ent_value(ir_entity *ent, ir_node *val, ir_entity *member) {
  ir_graph *rem = current_ir_graph;

  assert(get_entity_variability(ent) != variability_uninitialized);
  current_ir_graph = get_const_code_irg();

  val = copy_const_value(val);
  add_compound_ent_value(ent, val, member);
  current_ir_graph = rem;
  }*/

/* Copies the value i of the entity to current_block in current_ir_graph.
ir_node *
copy_compound_ent_value(ir_entity *ent, int pos) {
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  return copy_const_value(ent->values[pos+1]);
  }*/

ir_entity   *
get_compound_ent_value_member(ir_entity *ent, int pos) {
  compound_graph_path *path;
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  path = get_compound_ent_value_path(ent, pos);

  return get_compound_graph_path_node(path, get_compound_graph_path_length(path)-1);
}

void
set_compound_ent_value(ir_entity *ent, ir_node *val, ir_entity *member, int pos) {
  compound_graph_path *path;
  assert(is_compound_entity(ent) && (ent->variability != variability_uninitialized));
  path = get_compound_ent_value_path(ent, pos);
  set_compound_graph_path_node(path, 0, member);
  set_compound_ent_value_w_path(ent, val, path, pos);
}

void
set_array_entity_values(ir_entity *ent, tarval **values, int num_vals) {
  int i;
  ir_graph *rem = current_ir_graph;
  ir_type *arrtp = get_entity_type(ent);
  ir_node *val;
  ir_type *elttp = get_array_element_type(arrtp);

  assert(is_Array_type(arrtp));
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

/* Return the overall offset of value at position pos in bytes. */
int get_compound_ent_value_offset_bytes(ir_entity *ent, int pos) {
	compound_graph_path *path;
	int path_len, i;
	int offset = 0;

	assert(get_type_state(get_entity_type(ent)) == layout_fixed);

	path = get_compound_ent_value_path(ent, pos);
	path_len = get_compound_graph_path_length(path);

	for (i = 0; i < path_len; ++i) {
		ir_entity *node = get_compound_graph_path_node(path, i);
		ir_type *node_tp = get_entity_type(node);
		ir_type *owner_tp = get_entity_owner(node);

		if (is_Array_type(owner_tp)) {
			int size  = get_type_size_bits(node_tp);
			int align = get_type_alignment_bits(node_tp);
			if(size % align > 0) {
				size += align - (size % align);
			}
			assert(size % 8 == 0);
			size /= 8;
			offset += size * get_compound_graph_path_array_index(path, i - 1);
		} else {
			offset += get_entity_offset(node);
		}
	}

	return offset;
}  /* get_compound_ent_value_offset_bytes */

/* Return the offset in bits from the last byte address. */
int get_compound_ent_value_offset_bit_remainder(ir_entity *ent, int pos) {
	compound_graph_path *path;
	int path_len;
	ir_entity *last_node;

	assert(get_type_state(get_entity_type(ent)) == layout_fixed);

	path = get_compound_ent_value_path(ent, pos);
	path_len = get_compound_graph_path_length(path);
	last_node = get_compound_graph_path_node(path, path_len - 1);

  	return get_entity_offset_bits_remainder(last_node);
}  /* get_compound_ent_value_offset_bit_remainder */

typedef struct {
	int n_elems;      /**< number of elements the array can hold */
	int current_elem; /**< current array index */
	ir_entity *ent;
} array_info;

/* Compute the array indices in compound graph paths of initialized entities.
 *
 *  All arrays must have fixed lower and upper bounds.  One array can
 *  have an open bound.  If there are several open bounds, we do
 *  nothing.  There must be initializer elements for all array
 *  elements.  Uses the link field in the array element entities.  The
 *  array bounds must be representable as ints.
 *
 * WARNING: it is impossible to get this 100% right with the current
 *          design... (in array of structs you cant know when a struct is
 *          really finished and the next array element starts)
 *
 *  (If the bounds are not representable as ints we have to represent
 *  the indices as firm nodes.  But still we must be able to
 *  evaluate the index against the upper bound.)
 */
int compute_compound_ent_array_indices(ir_entity *ent) {
	ir_type *tp = get_entity_type(ent);
	int i, n_vals;
	int max_len = 0;
	array_info *array_infos;

	assert(is_compound_type(tp));

	if (!is_compound_type(tp) ||
		(ent->variability == variability_uninitialized))
		return 1;

	n_vals = get_compound_ent_n_values(ent);
	for(i = 0; i < n_vals; ++i) {
		compound_graph_path *path = get_compound_ent_value_path(ent, i);
		int len = get_compound_graph_path_length(path);
		if(len > max_len)
			max_len = len;
	}

	array_infos = alloca(max_len * sizeof(array_infos[0]));
	memset(array_infos, 0, max_len * sizeof(array_infos[0]));

	for(i = 0; i < n_vals; ++i) {
		compound_graph_path *path = get_compound_ent_value_path(ent, i);
		int path_len = get_compound_graph_path_length(path);
		int j;
		int needadd = 0;
		ir_entity *prev_node = NULL;

		for(j = path_len-1; j >= 0; --j) {
			int dim, dims;
			int n_elems;
			ir_entity *node = get_compound_graph_path_node(path, j);
			const ir_type *node_type = get_entity_type(node);
			array_info *info = &array_infos[j];

			if(is_atomic_entity(node)) {
				needadd = 1;
				set_compound_graph_path_array_index(path, j, -1);
				prev_node = node;
				continue;
			} else if(is_compound_type(node_type) && !is_Array_type(node_type)) {
				int n_members = get_compound_n_members(node_type);
				ir_entity *last = get_compound_member(node_type, n_members - 1);
				if(needadd && last == prev_node) {
					needadd = 1;
				} else {
					needadd = 0;
				}
				set_compound_graph_path_array_index(path, j, -1);
				prev_node = node;
				continue;
			}

			if(info->ent != node) {
				n_elems = 1;
				dims = get_array_n_dimensions(node_type);
				for(dim = 0; dim < dims; ++dim) {
					long lower_bound = 0;
					long upper_bound = -1;

					if(has_array_lower_bound(node_type, 0)) {
						lower_bound = get_array_lower_bound_int(node_type, 0);
					}
					if(has_array_upper_bound(node_type, 0)) {
						upper_bound = get_array_upper_bound_int(node_type, 0);
						assert(upper_bound >= lower_bound);
						n_elems *= (upper_bound - lower_bound);
					} else {
						assert(dim == dims-1);
						n_elems = -1;
					}
				}

				info->ent = node;
				info->n_elems = n_elems;
				info->current_elem = 0;
			}

			set_compound_graph_path_array_index(path, j, info->current_elem);

			if(needadd) {
				info->current_elem++;
				if(info->current_elem >= info->n_elems) {
					needadd = 1;
					info->current_elem = 0;
				} else {
					needadd = 0;
				}
			}

			prev_node = node;
		}
	}

	return 1;
}

int
(get_entity_offset)(const ir_entity *ent) {
  return _get_entity_offset(ent);
}

void
(set_entity_offset)(ir_entity *ent, int offset) {
  _set_entity_offset(ent, offset);
}

unsigned char
(get_entity_offset_bits_remainder)(const ir_entity *ent) {
  return _get_entity_offset_bits_remainder(ent);
}

void
(set_entity_offset_bits_remainder)(ir_entity *ent, unsigned char offset) {
  _set_entity_offset_bits_remainder(ent, offset);
}

void
add_entity_overwrites(ir_entity *ent, ir_entity *overwritten) {
#ifndef NDEBUG
  ir_type *owner     = get_entity_owner(ent);
  ir_type *ovw_ovner = get_entity_owner(overwritten);
  assert(is_Class_type(owner));
  assert(is_Class_type(ovw_ovner));
  assert(! is_class_final(ovw_ovner));
#endif /* NDEBUG */
  ARR_APP1(ir_entity *, ent->overwrites, overwritten);
  ARR_APP1(ir_entity *, overwritten->overwrittenby, ent);
}

int
get_entity_n_overwrites(ir_entity *ent) {
  assert(is_Class_type(get_entity_owner(ent)));
  return (ARR_LEN(ent->overwrites));
}

int
get_entity_overwrites_index(ir_entity *ent, ir_entity *overwritten) {
  int i;
  assert(is_Class_type(get_entity_owner(ent)));
  for (i = 0; i < get_entity_n_overwrites(ent); i++)
    if (get_entity_overwrites(ent, i) == overwritten)
      return i;
  return -1;
}

ir_entity *
get_entity_overwrites   (ir_entity *ent, int pos) {
  assert(is_Class_type(get_entity_owner(ent)));
  assert(pos < get_entity_n_overwrites(ent));
  return ent->overwrites[pos];
}

void
set_entity_overwrites   (ir_entity *ent, int pos, ir_entity *overwritten) {
  assert(is_Class_type(get_entity_owner(ent)));
  assert(pos < get_entity_n_overwrites(ent));
  ent->overwrites[pos] = overwritten;
}

void
remove_entity_overwrites(ir_entity *ent, ir_entity *overwritten) {
  int i;
  assert(is_Class_type(get_entity_owner(ent)));
  for (i = 0; i < (ARR_LEN (ent->overwrites)); i++)
    if (ent->overwrites[i] == overwritten) {
      for(; i < (ARR_LEN (ent->overwrites))-1; i++)
    ent->overwrites[i] = ent->overwrites[i+1];
      ARR_SETLEN(ir_entity*, ent->overwrites, ARR_LEN(ent->overwrites) - 1);
      break;
    }
}

void
add_entity_overwrittenby   (ir_entity *ent, ir_entity *overwrites) {
  add_entity_overwrites(overwrites, ent);
}

int
get_entity_n_overwrittenby (ir_entity *ent) {
  assert(is_Class_type(get_entity_owner(ent)));
  return (ARR_LEN (ent->overwrittenby));
}

int
get_entity_overwrittenby_index(ir_entity *ent, ir_entity *overwrites) {
  int i;
  assert(is_Class_type(get_entity_owner(ent)));
  for (i = 0; i < get_entity_n_overwrittenby(ent); i++)
    if (get_entity_overwrittenby(ent, i) == overwrites)
      return i;
  return -1;
}

ir_entity *
get_entity_overwrittenby   (ir_entity *ent, int pos) {
  assert(is_Class_type(get_entity_owner(ent)));
  assert(pos < get_entity_n_overwrittenby(ent));
  return ent->overwrittenby[pos];
}

void
set_entity_overwrittenby   (ir_entity *ent, int pos, ir_entity *overwrites) {
  assert(is_Class_type(get_entity_owner(ent)));
  assert(pos < get_entity_n_overwrittenby(ent));
  ent->overwrittenby[pos] = overwrites;
}

void    remove_entity_overwrittenby(ir_entity *ent, ir_entity *overwrites) {
  int i;
  assert(is_Class_type(get_entity_owner(ent)));
  for (i = 0; i < (ARR_LEN (ent->overwrittenby)); i++)
    if (ent->overwrittenby[i] == overwrites) {
      for(; i < (ARR_LEN (ent->overwrittenby))-1; i++)
    ent->overwrittenby[i] = ent->overwrittenby[i+1];
      ARR_SETLEN(ir_entity*, ent->overwrittenby, ARR_LEN(ent->overwrittenby) - 1);
      break;
    }
}

/* A link to store intermediate information */
void *
(get_entity_link)(const ir_entity *ent) {
  return _get_entity_link(ent);
}

void
(set_entity_link)(ir_entity *ent, void *l) {
  _set_entity_link(ent, l);
}

ir_graph *
(get_entity_irg)(const ir_entity *ent) {
  return _get_entity_irg(ent);
}

void
set_entity_irg(ir_entity *ent, ir_graph *irg) {
  assert(is_method_entity(ent));
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
  ent->attr.mtd_attr.irg = irg;
}

unsigned get_entity_vtable_number(ir_entity *ent) {
  assert(is_method_entity(ent));
  return ent->attr.mtd_attr.vtable_number;
}

void set_entity_vtable_number(ir_entity *ent, unsigned vtable_number) {
  assert(is_method_entity(ent));
  ent->attr.mtd_attr.vtable_number = vtable_number;
}

/* Returns the section of a method. */
ir_img_section get_method_img_section(const ir_entity *ent) {
  assert(is_method_entity(ent));
  return ent->attr.mtd_attr.section;
}

/* Sets the section of a method. */
void set_method_img_section(ir_entity *ent, ir_img_section section) {
  assert(is_method_entity(ent));
  ent->attr.mtd_attr.section = section;
}

int
(is_entity)(const void *thing) {
  return _is_entity(thing);
}

int is_atomic_entity(ir_entity *ent) {
  ir_type *t      = get_entity_type(ent);
  const tp_op *op = get_type_tpop(t);
  return (op == type_primitive || op == type_pointer ||
      op == type_enumeration || op == type_method);
}

int is_compound_entity(ir_entity *ent) {
  ir_type     *t  = get_entity_type(ent);
  const tp_op *op = get_type_tpop(t);
  return (op == type_class || op == type_struct ||
      op == type_array || op == type_union);
}

int is_method_entity(ir_entity *ent) {
  ir_type *t = get_entity_type(ent);
  return is_Method_type(t);
}

/**
 * @todo not implemented!!! */
int equal_entity(ir_entity *ent1, ir_entity *ent2) {
  fprintf(stderr, " calling unimplemented equal entity!!! \n");
  return 1;
}


unsigned long (get_entity_visited)(ir_entity *ent) {
  return _get_entity_visited(ent);
}

void (set_entity_visited)(ir_entity *ent, unsigned long num) {
  _set_entity_visited(ent, num);
}

/* Sets visited field in ir_entity to entity_visited. */
void (mark_entity_visited)(ir_entity *ent) {
  _mark_entity_visited(ent);
}

int (entity_visited)(ir_entity *ent) {
  return _entity_visited(ent);
}

int (entity_not_visited)(ir_entity *ent) {
  return _entity_not_visited(ent);
}

/* Returns the mask of the additional entity properties. */
unsigned get_entity_additional_properties(ir_entity *ent) {
  ir_graph *irg;

  assert(is_method_entity(ent));

  /* first check, if the graph has additional properties */
  irg = get_entity_irg(ent);

  if (irg)
    return get_irg_additional_properties(irg);

  if (ent->attr.mtd_attr.irg_add_properties & mtp_property_inherited)
    return get_method_additional_properties(get_entity_type(ent));

  return ent->attr.mtd_attr.irg_add_properties;
}

/* Sets the mask of the additional graph properties. */
void set_entity_additional_properties(ir_entity *ent, unsigned property_mask)
{
  ir_graph *irg;

  assert(is_method_entity(ent));

  /* first check, if the graph exists */
  irg = get_entity_irg(ent);
  if (irg)
    set_irg_additional_properties(irg, property_mask);
  else {
    /* do not allow to set the mtp_property_inherited flag or
     * the automatic inheritance of flags will not work */
    ent->attr.mtd_attr.irg_add_properties = property_mask & ~mtp_property_inherited;
  }
}

/* Sets one additional graph property. */
void set_entity_additional_property(ir_entity *ent, mtp_additional_property flag)
{
  ir_graph *irg;

  assert(is_method_entity(ent));

  /* first check, if the graph exists */
  irg = get_entity_irg(ent);
  if (irg)
    set_irg_additional_property(irg, flag);
  else {
    unsigned mask = ent->attr.mtd_attr.irg_add_properties;

    if (mask & mtp_property_inherited)
      mask = get_method_additional_properties(get_entity_type(ent));

    /* do not allow to set the mtp_property_inherited flag or
     * the automatic inheritance of flags will not work */
    ent->attr.mtd_attr.irg_add_properties = mask | (flag & ~mtp_property_inherited);
  }
}

/* Returns the class type that this type info entity represents or NULL
   if ent is no type info entity. */
ir_type *(get_entity_repr_class)(const ir_entity *ent) {
  return _get_entity_repr_class(ent);
}

/* Initialize entity module. */
void firm_init_entity(void)
{
  symconst_symbol sym;

  assert(firm_unknown_type && "Call init_type() before firm_init_entity()!");
  assert(!unknown_entity && "Call firm_init_entity() only once!");

  unknown_entity = new_rd_entity(NULL, firm_unknown_type, new_id_from_str(UNKNOWN_ENTITY_NAME), firm_unknown_type);
  set_entity_visibility(unknown_entity, visibility_external_allocated);
  set_entity_ld_ident(unknown_entity, get_entity_ident(unknown_entity));

  current_ir_graph      = get_const_code_irg();
  sym.entity_p          = unknown_entity;
  unknown_entity->value = new_SymConst(sym, symconst_addr_ent);
}
