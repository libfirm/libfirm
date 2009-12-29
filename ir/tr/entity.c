/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief   Representation of all program known entities.
 * @author  Martin Trapp, Christian Schaefer, Goetz Lindenmaier, Michael Beck
 * @version $Id$
 */
#include "config.h"

#include <string.h>
#include <stdlib.h>
#include <stddef.h>

#include "xmalloc.h"
#include "entity_t.h"
#include "array.h"
#include "irtools.h"
#include "irhooks.h"
#include "irprintf.h"

#include "irprog_t.h"
#include "ircons.h"
#include "tv_t.h"
#include "irdump.h"
#include "irgraph_t.h"
#include "callgraph.h"
#include "error.h"

/*-----------------------------------------------------------------*/
/** general                                                       **/
/*-----------------------------------------------------------------*/

ir_entity *unknown_entity = NULL;

ir_entity *get_unknown_entity(void) { return unknown_entity; }

/** The name of the unknown entity. */
#define UNKNOWN_ENTITY_NAME "unknown_entity"

/*-----------------------------------------------------------------*/
/* ENTITY                                                          */
/*-----------------------------------------------------------------*/

/**
 * Add an entity to it's already set owner type.
 */
static inline void insert_entity_in_owner(ir_entity *ent) {
	ir_type *owner = ent->owner;
	switch (get_type_tpop_code(owner)) {
	case tpo_class:
		add_class_member(owner, ent);
		break;
	case tpo_struct:
		add_struct_member(owner, ent);
		break;
	case tpo_union:
		add_union_member(owner, ent);
		break;
	case tpo_array:
		set_array_element_entity(owner, ent);
		break;
	default:
		panic("Unsupported type kind");
	}
}  /* insert_entity_in_owner */

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
static inline ir_entity *
new_rd_entity(dbg_info *db, ir_type *owner, ident *name, ir_type *type)
{
	ir_entity *res;
	ir_graph *rem;

	assert(!id_contains_char(name, ' ') && "entity name should not contain spaces");

	res = XMALLOCZ(ir_entity);

	res->kind    = k_entity;
	res->name    = name;
	res->ld_name = NULL;
	res->type    = type;
	res->owner   = owner;

	res->allocation           = allocation_automatic;
	res->visibility           = visibility_local;
	res->volatility           = volatility_non_volatile;
	res->aligned              = align_is_aligned;
	res->stickyness           = stickyness_unsticky;
	res->peculiarity          = peculiarity_existent;
	res->usage                = ir_usage_unknown;
	res->final                = 0;
	res->compiler_gen         = 0;
	res->backend_marked       = 0;
	res->offset               = -1;
	res->offset_bit_remainder = 0;
	res->alignment            = 0;
	res->link                 = NULL;
	res->repr_class           = NULL;

	if (is_Method_type(type)) {
		symconst_symbol sym;
		ir_mode *mode = is_Method_type(type) ? mode_P_code : mode_P_data;
		sym.entity_p            = res;
		rem                     = current_ir_graph;
		current_ir_graph        = get_const_code_irg();
		res->value              = new_SymConst(mode, sym, symconst_addr_ent);
		current_ir_graph        = rem;
		res->allocation         = allocation_static;
		res->variability        = variability_constant;
		res->attr.mtd_attr.irg_add_properties = mtp_property_inherited;
		res->attr.mtd_attr.vtable_number      = VTABLE_NUM_NOT_SET;
		res->attr.mtd_attr.param_access       = NULL;
		res->attr.mtd_attr.param_weight       = NULL;
		res->attr.mtd_attr.irg                = NULL;
	} else if (is_compound_type(type)) {
		res->variability = variability_uninitialized;
		res->value       = NULL;
		res->attr.cmpd_attr.values    = NULL;
		res->attr.cmpd_attr.val_paths = NULL;
	} else if (is_code_type(type)) {
		res->attr.code_attr.label = (ir_label_t) -1;
	} else {
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
}  /* new_rd_entity */

ir_entity *
new_d_entity(ir_type *owner, ident *name, ir_type *type, dbg_info *db) {
	ir_entity *res;

	assert(is_compound_type(owner));
	res = new_rd_entity(db, owner, name, type);
	/* Remember entity in it's owner. */
	insert_entity_in_owner(res);

	hook_new_entity(res);
	return res;
}  /* new_d_entity */

ir_entity *
new_entity(ir_type *owner, ident *name, ir_type *type) {
	return new_d_entity(owner, name, type, NULL);
}  /* new_entity */

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
		if (ent->has_initializer) {
			/* TODO: free initializers */
		} else {
			if (ent->attr.cmpd_attr.val_paths) {
				for (i = get_compound_ent_n_values(ent) - 1; i >= 0; --i)
					if (ent->attr.cmpd_attr.val_paths[i]) {
						/* free_compound_graph_path(ent->attr.cmpd_attr.val_paths[i]) ;  * @@@ warum nich? */
						/* Geht nich: wird mehrfach verwendet!!! ==> mehrfach frei gegeben. */
						/* DEL_ARR_F(ent->attr.cmpd_attr.val_paths); */
					}
					ent->attr.cmpd_attr.val_paths = NULL;
			}
			if (ent->attr.cmpd_attr.values) {
				/*DEL_ARR_F(ent->attr.cmpd_attr.values)*/;
			}
			ent->attr.cmpd_attr.values = NULL;
		}
	} else if (is_method_entity(ent)) {
		if (ent->attr.mtd_attr.param_access) {
			DEL_ARR_F(ent->attr.mtd_attr.param_access);
			ent->attr.mtd_attr.param_access = NULL;
		}
		if (ent->attr.mtd_attr.param_weight) {
			DEL_ARR_F(ent->attr.mtd_attr.param_weight);
			ent->attr.mtd_attr.param_weight = NULL;
		}
	}
}  /* free_entity_attrs */

/**
 * Creates a deep copy of an entity.
 */
static ir_entity *deep_entity_copy(ir_entity *old)
{
	ir_entity *newe = XMALLOC(ir_entity);

	*newe = *old;
	if (is_compound_entity(old)) {
		if (old->has_initializer) {
			/* FIXME: the initializers are NOT copied */
		} else {
			newe->attr.cmpd_attr.values    = NULL;
			newe->attr.cmpd_attr.val_paths = NULL;
			if (old->attr.cmpd_attr.values)
				newe->attr.cmpd_attr.values = DUP_ARR_F(ir_node *, old->attr.cmpd_attr.values);

			/* FIXME: the compound graph paths are NOT copied */
			if (old->attr.cmpd_attr.val_paths)
				newe->attr.cmpd_attr.val_paths = DUP_ARR_F(compound_graph_path *, old->attr.cmpd_attr.val_paths);
		}
	} else if (is_method_entity(old)) {
		/* do NOT copy them, reanalyze. This might be the best solution */
		newe->attr.mtd_attr.param_access = NULL;
		newe->attr.mtd_attr.param_weight = NULL;
	}

#ifdef DEBUG_libfirm
	newe->nr = get_irp_new_node_nr();
#endif
	return newe;
}
/*
 * Copies the entity if the new_owner is different from the
 * owner of the old entity,  else returns the old entity.
 */
ir_entity *
copy_entity_own(ir_entity *old, ir_type *new_owner) {
	ir_entity *newe;
	assert(is_entity(old));
	assert(is_compound_type(new_owner));
	assert(get_type_state(new_owner) != layout_fixed);

	if (old->owner == new_owner)
		return old;

	/* create a deep copy so we are safe of aliasing and double-freeing. */
	newe = deep_entity_copy(old);
	newe->owner = new_owner;

	if (is_Class_type(new_owner)) {
		newe->overwrites    = NEW_ARR_F(ir_entity *, 0);
		newe->overwrittenby = NEW_ARR_F(ir_entity *, 0);
	}

	insert_entity_in_owner(newe);
	return newe;
}  /* copy_entity_own */

ir_entity *
copy_entity_name(ir_entity *old, ident *new_name) {
	ir_entity *newe;
	assert(old && old->kind == k_entity);

	if (old->name == new_name) return old;
	newe = deep_entity_copy(old);
	newe->name = new_name;
	newe->ld_name = NULL;

	if (is_Class_type(newe->owner)) {
		newe->overwrites    = DUP_ARR_F(ir_entity *, old->overwrites);
		newe->overwrittenby = DUP_ARR_F(ir_entity *, old->overwrittenby);
	}
	insert_entity_in_owner(newe);

	return newe;
}  /* copy_entity_name */

void
free_entity(ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
	free_entity_attrs(ent);
	ent->kind = k_BAD;
	free(ent);
}  /* free_entity */

/* Outputs a unique number for this node */
long
get_entity_nr(const ir_entity *ent) {
	assert(ent && ent->kind == k_entity);
#ifdef DEBUG_libfirm
	return ent->nr;
#else
	return (long)PTR_TO_INT(ent);
#endif
}  /* get_entity_nr */

const char *
(get_entity_name)(const ir_entity *ent) {
	return _get_entity_name(ent);
}  /* get_entity_name */

ident *
(get_entity_ident)(const ir_entity *ent) {
	return _get_entity_ident(ent);
}  /* get_entity_ident */

void
(set_entity_ident)(ir_entity *ent, ident *id) {
	_set_entity_ident(ent, id);
}  /* set_entity_ident */

ir_type *
(get_entity_owner)(ir_entity *ent) {
	return _get_entity_owner(ent);
}  /* get_entity_owner */

void
set_entity_owner(ir_entity *ent, ir_type *owner) {
	assert(is_entity(ent));
	assert(is_compound_type(owner));
	ent->owner = owner;
}  /* set_entity_owner */

ident *
(get_entity_ld_ident)(ir_entity *ent) {
	return _get_entity_ld_ident(ent);
}  /* get_entity_ld_ident */

void
(set_entity_ld_ident)(ir_entity *ent, ident *ld_ident) {
	_set_entity_ld_ident(ent, ld_ident);
}  /* set_entity_ld_ident */

const char *
(get_entity_ld_name)(ir_entity *ent) {
	return _get_entity_ld_name(ent);
}  /* get_entity_ld_name */

ir_type *
(get_entity_type)(ir_entity *ent) {
	return _get_entity_type(ent);
}  /* get_entity_type */

void
(set_entity_type)(ir_entity *ent, ir_type *type) {
	_set_entity_type(ent, type);
}  /* set_entity_type */

ir_allocation
(get_entity_allocation)(const ir_entity *ent) {
	return _get_entity_allocation(ent);
}  /* get_entity_allocation */

void
(set_entity_allocation)(ir_entity *ent, ir_allocation al) {
	_set_entity_allocation(ent, al);
}  /* set_entity_allocation */

/* return the name of the visibility */
const char *get_allocation_name(ir_allocation al)
{
#define X(a)    case a: return #a
	switch (al) {
	X(allocation_automatic);
	X(allocation_parameter);
	X(allocation_dynamic);
	X(allocation_static);
    default: return "BAD VALUE";
	}
#undef X
}  /* get_allocation_name */

ir_visibility
(get_entity_visibility)(const ir_entity *ent) {
	return _get_entity_visibility(ent);
}  /* get_entity_visibility */

void
set_entity_visibility(ir_entity *ent, ir_visibility vis) {
	assert(ent && ent->kind == k_entity);
	if (vis != visibility_local)
		assert((ent->allocation == allocation_static) ||
		(ent->allocation == allocation_automatic));
		/* @@@ Test that the owner type is not local, but how??
	&& get_class_visibility(get_entity_owner(ent)) != local));*/
	ent->visibility = vis;
}  /* set_entity_visibility */

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
}  /* get_visibility_name */

ir_variability
(get_entity_variability)(const ir_entity *ent) {
	return _get_entity_variability(ent);
}  /* get_entity_variability */

void
set_entity_variability(ir_entity *ent, ir_variability var)
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
		ent->value = new_r_Unknown(get_const_code_irg(), get_type_mode(ent->type));
	}

	if ((is_compound_type(ent->type)) &&
		(var == variability_uninitialized) && (ent->variability != variability_uninitialized)) {
		/* Free data structures for constant values */
		DEL_ARR_F(ent->attr.cmpd_attr.values);    ent->attr.cmpd_attr.values    = NULL;
		DEL_ARR_F(ent->attr.cmpd_attr.val_paths); ent->attr.cmpd_attr.val_paths = NULL;
	}
	ent->variability = var;
}  /* set_entity_variability */

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
}  /* get_variability_name */

ir_volatility
(get_entity_volatility)(const ir_entity *ent) {
	return _get_entity_volatility(ent);
}  /* get_entity_volatility */

void
(set_entity_volatility)(ir_entity *ent, ir_volatility vol) {
	_set_entity_volatility(ent, vol);
}  /* set_entity_volatility */

/* Return the name of the volatility. */
const char *get_volatility_name(ir_volatility var)
{
#define X(a)    case a: return #a
	switch (var) {
	X(volatility_non_volatile);
	X(volatility_is_volatile);
    default: return "BAD VALUE";
	}
#undef X
}  /* get_volatility_name */

ir_align
(get_entity_aligned)(const ir_entity *ent) {
	return _get_entity_aligned(ent);
}

void
(set_entity_aligned)(ir_entity *ent, ir_align a) {
	_set_entity_aligned(ent, a);
}

unsigned
(get_entity_alignment)(const ir_entity *ent) {
	return _get_entity_alignment(ent);
}

void
(set_entity_alignment)(ir_entity *ent, unsigned alignment) {
	_set_entity_alignment(ent, alignment);
}

/* Return the name of the alignment. */
const char *get_align_name(ir_align a)
{
#define X(a)    case a: return #a
	switch (a) {
	X(align_non_aligned);
	X(align_is_aligned);
	default: return "BAD VALUE";
	}
#undef X
}  /* get_align_name */

void
set_entity_label(ir_entity *ent, ir_label_t label)
{
	ent->attr.code_attr.label = label;
}

ir_label_t get_entity_label(const ir_entity *ent)
{
	return ent->attr.code_attr.label;
}

ir_peculiarity
(get_entity_peculiarity)(const ir_entity *ent) {
	return _get_entity_peculiarity(ent);
}  /* get_entity_peculiarity */

void
(set_entity_peculiarity)(ir_entity *ent, ir_peculiarity pec) {
	_set_entity_peculiarity(ent, pec);
}  /* set_entity_peculiarity */

/* Checks if an entity cannot be overridden anymore. */
int (is_entity_final)(const ir_entity *ent) {
	return _is_entity_final(ent);
}  /* is_entity_final */

/* Sets/resets the final flag of an entity. */
void (set_entity_final)(ir_entity *ent, int final) {
	_set_entity_final(ent, final);
}  /* set_entity_final */

/* Checks if an entity is compiler generated */
int (is_entity_compiler_generated)(const ir_entity *ent) {
	return _is_entity_compiler_generated(ent);
}  /* is_entity_compiler_generated */

/* Sets/resets the compiler generated flag */
void (set_entity_compiler_generated)(ir_entity *ent, int flag) {
	_set_entity_compiler_generated(ent, flag);
}  /* set_entity_compiler_generated */

/* Checks if an entity is marked by the backend */
int (is_entity_backend_marked)(const ir_entity *ent) {
	return _is_entity_backend_marked(ent);
}  /* is_entity_backend_marked */

/* Sets/resets the compiler generated flag */
void (set_entity_backend_marked)(ir_entity *ent, int flag) {
	_set_entity_backend_marked(ent, flag);
}  /* set_entity_backend_marked */

ir_entity_usage (get_entity_usage)(const ir_entity *ent) {
	return _get_entity_usage(ent);
}

void (set_entity_usage)(ir_entity *ent, ir_entity_usage flags) {
	_set_entity_usage(ent, flags);
}

/* Get the entity's stickyness */
ir_stickyness
(get_entity_stickyness)(const ir_entity *ent) {
	return _get_entity_stickyness(ent);
}  /* get_entity_stickyness */

/* Set the entity's stickyness */
void
(set_entity_stickyness)(ir_entity *ent, ir_stickyness stickyness) {
	_set_entity_stickyness(ent, stickyness);
}  /* set_entity_stickyness */

/* Set has no effect for existent entities of type method. */
ir_node *
get_atomic_ent_value(ir_entity *ent)
{
	assert(ent && is_atomic_entity(ent));
	assert(ent->variability != variability_uninitialized);
	return skip_Id(ent->value);
}  /* get_atomic_ent_value */

void
set_atomic_ent_value(ir_entity *ent, ir_node *val) {
	assert(is_atomic_entity(ent) && (ent->variability != variability_uninitialized));
	if (is_Method_type(ent->type) && (ent->peculiarity == peculiarity_existent))
		return;
	assert(is_Dummy(val) || get_irn_mode(val) == get_type_mode(ent->type));
	ent->value = val;
}  /* set_atomic_ent_value */

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
	switch (get_irn_opcode(n)) {
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
}  /* is_irn_const_expression */

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
		nn = new_d_Const_type(dbg, get_Const_tarval(n), get_Const_type(n));
		break;
	case iro_SymConst:
		nn = new_d_SymConst_type(dbg, get_irn_mode(n), get_SymConst_symbol(n), get_SymConst_kind(n),
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
		nn = new_Unknown(m); break;
	default:
		assert(0 && "opcode invalid or not implemented");
		nn = NULL;
		break;
	}
	return nn;
}  /* copy_const_value */

/** Return the name of the initializer kind. */
const char *get_initializer_kind_name(ir_initializer_kind_t ini)
{
#define X(a)    case a: return #a
	switch (ini) {
	X(IR_INITIALIZER_CONST);
	X(IR_INITIALIZER_TARVAL);
	X(IR_INITIALIZER_NULL);
	X(IR_INITIALIZER_COMPOUND);
    default: return "BAD VALUE";
	}
#undef X
}

static ir_initializer_t null_initializer = { IR_INITIALIZER_NULL };

ir_initializer_t *get_initializer_null(void)
{
	return &null_initializer;
}

ir_initializer_t *create_initializer_const(ir_node *value)
{
	struct obstack *obst = get_irg_obstack(get_const_code_irg());

	ir_initializer_t *initializer
		= obstack_alloc(obst, sizeof(ir_initializer_const_t));
	initializer->kind         = IR_INITIALIZER_CONST;
	initializer->consti.value = value;

	return initializer;
}

ir_initializer_t *create_initializer_tarval(tarval *tv)
{
	struct obstack *obst = get_irg_obstack(get_const_code_irg());

	ir_initializer_t *initializer
		= obstack_alloc(obst, sizeof(ir_initializer_tarval_t));
	initializer->kind         = IR_INITIALIZER_TARVAL;
	initializer->tarval.value = tv;

	return initializer;
}

ir_initializer_t *create_initializer_compound(unsigned n_entries)
{
	struct obstack *obst = get_irg_obstack(get_const_code_irg());

	size_t i;
	size_t size  = sizeof(ir_initializer_compound_t)
	             + (n_entries-1) * sizeof(ir_initializer_t*);

	ir_initializer_t *initializer = obstack_alloc(obst, size);
	initializer->kind                    = IR_INITIALIZER_COMPOUND;
	initializer->compound.n_initializers = n_entries;

	for(i = 0; i < n_entries; ++i) {
		initializer->compound.initializers[i] = get_initializer_null();
	}

	return initializer;
}

ir_node *get_initializer_const_value(const ir_initializer_t *initializer)
{
	assert(initializer->kind == IR_INITIALIZER_CONST);
	return skip_Id(initializer->consti.value);
}

tarval *get_initializer_tarval_value(const ir_initializer_t *initializer)
{
	assert(initializer->kind == IR_INITIALIZER_TARVAL);
	return initializer->tarval.value;
}

unsigned get_initializer_compound_n_entries(const ir_initializer_t *initializer)
{
	assert(initializer->kind == IR_INITIALIZER_COMPOUND);
	return initializer->compound.n_initializers;
}

void set_initializer_compound_value(ir_initializer_t *initializer,
                                    unsigned index, ir_initializer_t *value)
{
	assert(initializer->kind == IR_INITIALIZER_COMPOUND);
	assert(index < initializer->compound.n_initializers);

	initializer->compound.initializers[index] = value;
}

ir_initializer_t *get_initializer_compound_value(
		const ir_initializer_t *initializer, unsigned index)
{
	assert(initializer->kind == IR_INITIALIZER_COMPOUND);
	assert(index < initializer->compound.n_initializers);

	return initializer->compound.initializers[index];
}

ir_initializer_kind_t get_initializer_kind(const ir_initializer_t *initializer)
{
	return initializer->kind;
}

static void check_entity_initializer(ir_entity *entity)
{
	/* TODO */
	(void) entity;
}

void set_entity_initializer(ir_entity *entity, ir_initializer_t *initializer)
{
	entity->attr.initializer = initializer;
	entity->has_initializer  = 1;
	check_entity_initializer(entity);
}

int has_entity_initializer(const ir_entity *entity)
{
	return entity->has_initializer;
}

ir_initializer_t *get_entity_initializer(const ir_entity *entity)
{
	assert(entity->has_initializer);
	return entity->attr.initializer;
}

int (get_entity_offset)(const ir_entity *ent)
{
	return _get_entity_offset(ent);
}

void (set_entity_offset)(ir_entity *ent, int offset)
{
	_set_entity_offset(ent, offset);
}

unsigned char (get_entity_offset_bits_remainder)(const ir_entity *ent)
{
	return _get_entity_offset_bits_remainder(ent);
}

void (set_entity_offset_bits_remainder)(ir_entity *ent, unsigned char offset)
{
	_set_entity_offset_bits_remainder(ent, offset);
}

void add_entity_overwrites(ir_entity *ent, ir_entity *overwritten)
{
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

int get_entity_n_overwrites(ir_entity *ent)
{
	assert(is_Class_type(get_entity_owner(ent)));
	return (ARR_LEN(ent->overwrites));
}

int get_entity_overwrites_index(ir_entity *ent, ir_entity *overwritten)
{
	int i, n;
	assert(is_Class_type(get_entity_owner(ent)));
	n = get_entity_n_overwrites(ent);
	for (i = 0; i < n; ++i) {
		if (get_entity_overwrites(ent, i) == overwritten)
			return i;
	}
	return -1;
}

ir_entity *get_entity_overwrites(ir_entity *ent, int pos)
{
	assert(is_Class_type(get_entity_owner(ent)));
	assert(pos < get_entity_n_overwrites(ent));
	return ent->overwrites[pos];
}

void set_entity_overwrites(ir_entity *ent, int pos, ir_entity *overwritten)
{
	assert(is_Class_type(get_entity_owner(ent)));
	assert(pos < get_entity_n_overwrites(ent));
	ent->overwrites[pos] = overwritten;
}

void remove_entity_overwrites(ir_entity *ent, ir_entity *overwritten)
{
	int i, n;
	assert(is_Class_type(get_entity_owner(ent)));
	n = ARR_LEN(ent->overwrites);
	for (i = 0; i < n; ++i) {
		if (ent->overwrites[i] == overwritten) {
			for (; i < n - 1; i++)
				ent->overwrites[i] = ent->overwrites[i+1];
			ARR_SETLEN(ir_entity*, ent->overwrites, n - 1);
			break;
		}
	}
}

void add_entity_overwrittenby(ir_entity *ent, ir_entity *overwrites)
{
	add_entity_overwrites(overwrites, ent);
}

int get_entity_n_overwrittenby(ir_entity *ent)
{
	assert(is_Class_type(get_entity_owner(ent)));
	return ARR_LEN(ent->overwrittenby);
}

int get_entity_overwrittenby_index(ir_entity *ent, ir_entity *overwrites)
{
	int i, n;
	assert(is_Class_type(get_entity_owner(ent)));
	n = get_entity_n_overwrittenby(ent);
	for (i = 0; i < n; ++i) {
		if (get_entity_overwrittenby(ent, i) == overwrites)
			return i;
	}
	return -1;
}

ir_entity *get_entity_overwrittenby(ir_entity *ent, int pos)
{
	assert(is_Class_type(get_entity_owner(ent)));
	assert(pos < get_entity_n_overwrittenby(ent));
	return ent->overwrittenby[pos];
}

void set_entity_overwrittenby(ir_entity *ent, int pos, ir_entity *overwrites)
{
	assert(is_Class_type(get_entity_owner(ent)));
	assert(pos < get_entity_n_overwrittenby(ent));
	ent->overwrittenby[pos] = overwrites;
}

void remove_entity_overwrittenby(ir_entity *ent, ir_entity *overwrites)
{
	int i, n;
	assert(is_Class_type(get_entity_owner(ent)));

	n = ARR_LEN(ent->overwrittenby);
	for (i = 0; i < n; ++i) {
		if (ent->overwrittenby[i] == overwrites) {
			for(; i < n - 1; ++i)
				ent->overwrittenby[i] = ent->overwrittenby[i+1];
			ARR_SETLEN(ir_entity*, ent->overwrittenby, n - 1);
			break;
		}
	}
}

void *(get_entity_link)(const ir_entity *ent)
{
	return _get_entity_link(ent);
}

void (set_entity_link)(ir_entity *ent, void *l)
{
	_set_entity_link(ent, l);
}

ir_graph *(get_entity_irg)(const ir_entity *ent)
{
	return _get_entity_irg(ent);
}

void set_entity_irg(ir_entity *ent, ir_graph *irg)
{
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

unsigned get_entity_vtable_number(const ir_entity *ent)
{
	assert(is_method_entity((ir_entity *)ent));
	return ent->attr.mtd_attr.vtable_number;
}

void set_entity_vtable_number(ir_entity *ent, unsigned vtable_number)
{
	assert(is_method_entity(ent));
	ent->attr.mtd_attr.vtable_number = vtable_number;
}

int (is_entity)(const void *thing)
{
	return _is_entity(thing);
}

int is_atomic_entity(ir_entity *ent)
{
	ir_type *t      = get_entity_type(ent);
	const tp_op *op = get_type_tpop(t);
	return (op == type_primitive || op == type_pointer ||
		op == type_enumeration || op == type_method);
}

int is_compound_entity(ir_entity *ent)
{
	ir_type     *t  = get_entity_type(ent);
	const tp_op *op = get_type_tpop(t);
	return (op == type_class || op == type_struct ||
		op == type_array || op == type_union);
}

int is_method_entity(ir_entity *ent)
{
	ir_type *t = get_entity_type(ent);
	return is_Method_type(t);
}

ir_visited_t (get_entity_visited)(ir_entity *ent)
{
	return _get_entity_visited(ent);
}

void (set_entity_visited)(ir_entity *ent, ir_visited_t num)
{
	_set_entity_visited(ent, num);
}

void (mark_entity_visited)(ir_entity *ent)
{
	_mark_entity_visited(ent);
}

int (entity_visited)(ir_entity *ent)
{
	return _entity_visited(ent);
}

int (entity_not_visited)(ir_entity *ent)
{
	return _entity_not_visited(ent);
}

unsigned get_entity_additional_properties(ir_entity *ent)
{
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
ir_type *(get_entity_repr_class)(const ir_entity *ent)
{
	return _get_entity_repr_class(ent);
}

dbg_info *(get_entity_dbg_info)(const ir_entity *ent)
{
	return _get_entity_dbg_info(ent);
}

void (set_entity_dbg_info)(ir_entity *ent, dbg_info *db)
{
	_set_entity_dbg_info(ent, db);
}

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
	/* TODO: we need two unknown_entities here, one for code and one for data */
	unknown_entity->value = new_SymConst(mode_P_data, sym, symconst_addr_ent);
}
