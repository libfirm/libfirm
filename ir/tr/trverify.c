/*
 * Copyright (C) 1995-2011 University of Karlsruhe.  All right reserved.
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
 * @file    tr_inheritance.c
 * @brief   Check types and entities for correctness.
 * @date    29.1.2003
 * @author  Michael Beck, Goetz Lindenmaier
 */
#include "config.h"

#include "irgraph_t.h"
#include "irflag_t.h"
#include "irprintf.h"
#include "irgwalk.h"
#include "error.h"
#include "tv.h"

#ifdef NDEBUG
/*
 * in RELEASE mode, returns ret if the expression expr evaluates to zero
 * in ASSERT mode, asserts the expression expr (and the string string).
 */
#define ASSERT_AND_RET(expr, string, ret)       if (!(expr)) return (ret)

/*
 * in RELEASE mode, returns ret if the expression expr evaluates to zero
 * in ASSERT mode, executes blk if the expression expr evaluates to zero and asserts expr
 */
#define ASSERT_AND_RET_DBG(expr, string, ret, blk)      if (!(expr)) return (ret)
#else
#define ASSERT_AND_RET(expr, string, ret) \
do { \
  if (opt_do_node_verification == FIRM_VERIFICATION_ON) {\
    assert((expr) && string); } \
  if (!(expr)) { \
    if (opt_do_node_verification == FIRM_VERIFICATION_REPORT) \
      fprintf(stderr, #expr " : " string "\n"); \
    firm_verify_failure_msg = #expr " && " string; \
    return (ret); \
  } \
} while (0)

#define ASSERT_AND_RET_DBG(expr, string, ret, blk) \
do { \
  if (!(expr)) { \
    firm_verify_failure_msg = #expr " && " string; \
    if (opt_do_node_verification != FIRM_VERIFICATION_ERROR_ONLY) { blk; } \
    if (opt_do_node_verification == FIRM_VERIFICATION_REPORT) \
      fprintf(stderr, #expr " : " string "\n"); \
    else if (opt_do_node_verification == FIRM_VERIFICATION_ON) { \
      assert((expr) && string); \
    } \
    return (ret); \
  } \
} while (0)

#endif /* NDEBUG */

#ifndef NDEBUG

static const char *firm_verify_failure_msg;

#if 0
/**
 * Show diagnostic if an entity overwrites another one not
 * in direct superclasses.
 */
static void show_ent_not_supertp(ir_entity *ent, ir_entity *ovw)
{
	ir_type *owner = get_entity_owner(ent);
	ir_type *ov_own = get_entity_owner(ovw);
	size_t   i;

	fprintf(stderr, "Type verification error:\n");
	ir_fprintf(stderr, "Entity %+F::%+e owerwrites ", owner, ent);
	ir_fprintf(stderr, "Entity %+F::%+e\n", ov_own, ovw);

	ir_fprintf(stderr, "Supertypes of %+F:\n", owner);
	for (i = 0; i < get_class_n_supertypes(owner); ++i) {
		ir_type *super = get_class_supertype(owner, i);
		ir_fprintf(stderr, " %+F:\n", super);
	}
}
#endif

/**
 * Show diagnostic if an entity overwrites a wrong number of things.
 */
static void show_ent_overwrite_cnt(ir_entity *ent)
{
	ir_type *owner = get_entity_owner(ent);
	size_t i;
	size_t j;
	size_t k;
	bool   found;
	bool   show_stp = false;

	fprintf(stderr, "Type verification error:\n");
	ir_fprintf(stderr, "Entity %t::%e owerwrites\n", owner, ent);
	for (i = 0; i < get_entity_n_overwrites(ent); ++i) {
		ir_entity *ovw = get_entity_overwrites(ent, i);
		ir_type *ov_own = get_entity_owner(ovw);
		size_t n_supertypes = get_class_n_supertypes(owner);

		ir_fprintf(stderr, "  %t::%e\n", ov_own, ovw);
		for (k = 0; k < i; ++k) {
			if (ovw == get_entity_overwrites(ent, k)) {
				ir_fprintf(stderr, "  ->%t::%e entered more than once\n", ov_own, ovw);
				break;
			}
		}

		found = false;
		for (j = 0; j < n_supertypes; ++j) {
			if (ov_own == get_class_supertype(owner, j)) {
				show_stp = found = true;
				break;
			}
		}
		if (! found)
			ir_fprintf(stderr, "  ->%t not in super types of %t\n", ov_own, owner);
	}

	if (show_stp) {
		ir_fprintf(stderr, "Supertypes of %t:\n", owner);
		for (i = 0; i < get_class_n_supertypes(owner); ++i) {
			ir_type *super = get_class_supertype(owner, i);
			ir_fprintf(stderr, " %t:\n", super);
		}
	}
}

#endif /* #ifndef NDEBUG */

/**
 * Check a class
 */
static int check_class(ir_type *tp)
{
	size_t i, n;

	for (i = 0, n = get_class_n_members(tp); i < n; ++i) {
		ir_entity *mem = get_class_member(tp, i);

		ASSERT_AND_RET_DBG(
			tp == get_entity_owner(mem),
			"class member with wrong owner",
			error_ent_wrong_owner,
			ir_fprintf(stderr, "Type verification error:\n%+F %+e(owner %+F)\n",tp, mem, get_entity_owner(mem))
		);
		ASSERT_AND_RET_DBG(
			mem,
			"NULL members not allowed",
			error_null_mem,
			ir_fprintf(stderr, "Type verification error:\n%+F member %zu is NULL\n", tp, i)
		);

		ASSERT_AND_RET_DBG(
			get_entity_n_overwrites(mem) <= get_class_n_supertypes(tp),
			"wrong number of entity overwrites",
			error_wrong_ent_overwrites,
			show_ent_overwrite_cnt(mem)
		);

#if 0
		{
			size_t j, m;
			/* check if the overwrite relation is flat, i.e. every overwrite
			 * is visible in every direct superclass. */
			for (j = 0, m = get_entity_n_overwrites(mem); j < m; ++j) {
				ir_entity *ovw = get_entity_overwrites(mem, j);
				size_t    k, n_super;

				/* Check whether ovw is member of one of tp's supertypes. If so,
				   the representation is correct. */
				for (k = 0, n_super = get_class_n_supertypes(tp); k < n_super; ++k) {
					if (get_class_member_index(get_class_supertype(tp, k), ovw) != INVALID_MEMBER_INDEX) {
						ASSERT_AND_RET_DBG(
							0,
							"overwrites an entity not contained in direct supertype",
							error_ent_not_cont,
							show_ent_not_supertp(mem, ovw)
						);
						break;
					}
				}
			}
		}
#endif
	}
	return 0;
}

/**
 * Check an array.
 */
static int check_array(const ir_type *tp)
{
	size_t i, n_dim = get_array_n_dimensions(tp);

	for (i = 0; i < n_dim; ++i) {
		ASSERT_AND_RET_DBG(
			has_array_lower_bound(tp, i) || has_array_upper_bound(tp, i),
			"array bound missing",
			1,
			ir_fprintf(stderr, "%+F in dimension %zu\n", tp, i)
		);
	}
	return 0;
}


/**
 * Check a primitive.
 */
static int check_primitive(ir_type *tp)
{
	ASSERT_AND_RET_DBG(
		is_mode(get_type_mode(tp)),
		"Primitive type without mode",
		1,
		ir_fprintf(stderr, "%+F\n", tp)
	);
	return 0;
}


/*
 * Checks a type.
 *
 * return
 *  0   if no error encountered
 */
int check_type(ir_type *tp)
{
	switch (get_type_tpop_code(tp)) {
	case tpo_class:
		return check_class(tp);
	case tpo_array:
		return check_array(tp);
	case tpo_primitive:
		return check_primitive(tp);
	default: break;
	}
	return 0;
}

/**
 * checks the visited flag
 */
static int check_visited_flag(ir_graph *irg, ir_node *n)
{
	ASSERT_AND_RET_DBG(
		get_irn_visited(n) <= get_irg_visited(irg),
		"Visited flag of node is larger than that of corresponding irg.",
		0,
		ir_fprintf(stderr, "%+F in %+F\n", n, irg)
	);
	return 1;
}

/**
 * helper environment struct for constant_on_wrong_obstack()
 */
typedef struct myenv {
	int res;
	ir_graph *irg;
} myenv;

/**
 * called by the walker
 */
static void on_irg_storage(ir_node *n, void *data)
{
	myenv *env = (myenv*)data;

	/* We also test whether the setting of the visited flag is legal. */
	env->res = node_is_in_irgs_storage(env->irg, n) &&
	           check_visited_flag(env->irg, n);
}

/**
 * checks whether a given constant IR node is NOT on the
 * constant IR graph.
 */
static int constant_on_wrong_irg(ir_node *n)
{
	myenv env;

	env.res = 1;  /* on right obstack */
	env.irg = get_const_code_irg();

	irg_walk(n, on_irg_storage, NULL, (void *)&env);
	return ! env.res;
}

static int initializer_constant_on_wrong_irg(ir_initializer_t *initializer)
{
	switch (get_initializer_kind(initializer)) {
	case IR_INITIALIZER_NULL:
		return 0;
	case IR_INITIALIZER_TARVAL:
		return 0;
	case IR_INITIALIZER_CONST:
		return constant_on_wrong_irg(get_initializer_const_value(initializer));
	case IR_INITIALIZER_COMPOUND: {
		size_t i, n = get_initializer_compound_n_entries(initializer);
		for (i = 0; i < n; ++i) {
			ir_initializer_t *sub
				= get_initializer_compound_value(initializer, i);
			if (initializer_constant_on_wrong_irg(sub))
				return 1;
		}
		return 0;
	}
	}
	panic("invalid initializer in initializer_on_wrong_irg");
}

/**
 * Check if constants node are NOT on the constant IR graph.
 *
 * @return NON-zero if an entity initializer constant is NOT on
 * the current_ir_graph's obstack.
 */
static int constants_on_wrong_irg(ir_entity *ent)
{
	if (ent->initializer != NULL) {
		return initializer_constant_on_wrong_irg(ent->initializer);
	} else if (entity_has_compound_ent_values(ent)) {
		size_t i, n;
		for (i = 0, n = get_compound_ent_n_values(ent); i < n; ++i) {
			if (constant_on_wrong_irg(get_compound_ent_value(ent, i)))
				return 1;
		}
	}
	return 0;
}

/*
 * Check an entity. Currently, we check only if initialized constants
 * are build on the const irg graph.
 *
 * @return
 *  0   if no error encountered
 *  != 0    a trverify_error_codes code
 */
int check_entity(ir_entity *ent)
{
	ir_type *tp = get_entity_type(ent);

	current_ir_graph =  get_const_code_irg();
	ASSERT_AND_RET_DBG(
		constants_on_wrong_irg(ent) == 0,
		"Contants placed on wrong IRG",
		error_const_on_wrong_irg,
		ir_fprintf(stderr, "%+e not on %+F\n", ent, current_ir_graph)
	);

	/* Originally, this test assumed, that only method entities have
	   pecularity_inherited.  As I changed this, I have to test for method type
	   before doing the test. */
	if (get_entity_peculiarity(ent) == peculiarity_existent
	    && is_method_entity(ent)) {

		ir_entity *impl = get_SymConst_entity(get_atomic_ent_value(ent));
		ASSERT_AND_RET_DBG(
			impl != NULL,
			"inherited method entities must have constant pointing to existent entity.",
			error_inherited_ent_without_const,
			ir_fprintf(stderr, "%+e points to %+e\n", ent, impl)
		);
	}

	if (is_atomic_entity(ent) && ent->initializer != NULL) {
		ir_mode *mode = NULL;
		ir_initializer_t *initializer = ent->initializer;
		switch (initializer->kind) {
		case IR_INITIALIZER_CONST:
			mode = get_irn_mode(get_initializer_const_value(initializer));
			break;
		case IR_INITIALIZER_TARVAL:
			mode = get_tarval_mode(get_initializer_tarval_value(initializer));
			break;
		case IR_INITIALIZER_NULL:
		case IR_INITIALIZER_COMPOUND:
			break;
		}
		ASSERT_AND_RET_DBG(
			mode == NULL || mode == get_type_mode(tp),
			"Mode of constant in entity must match type.",
			error_ent_const_mode,
			ir_fprintf(stderr, "%+e, type %+F(%+F)\n",
			ent, tp, get_type_mode(tp))
		);
	}
	return no_error;
}

/*
 * check types and entities
 */
static void check_tore(type_or_ent tore, void *env)
{
	int *res = (int*)env;
	assert(tore.ent);
	if (is_type(tore.typ)) {
		*res = check_type(tore.typ);
	} else {
		assert(is_entity(tore.ent));
		*res = check_entity(tore.ent);
	}
}

/*
 * Verify types and entities.
 */
int tr_verify(void)
{
	static ident *empty = NULL;
	int           res = no_error;
	ir_type      *constructors;
	ir_type      *destructors;
	ir_type      *thread_locals;
	size_t        i, n;
	ir_segment_t  s;

	if (empty == NULL)
		empty = new_id_from_chars("", 0);

	type_walk(check_tore, NULL, &res);

	for (s = IR_SEGMENT_FIRST; s <= IR_SEGMENT_LAST; ++s) {
		const ir_type *type = get_segment_type(s);
		size_t         e;
		for (e = 0; e < get_compound_n_members(type); ++e) {
			ir_entity *entity = get_compound_member(type, e);
			ASSERT_AND_RET(get_entity_ld_ident(entity) != NULL ||
					get_entity_visibility(entity) == ir_visibility_private,
					"segment members must have a name or visibility_private",
					1);
		}
	}

	constructors = get_segment_type(IR_SEGMENT_CONSTRUCTORS);
	for (i = 0, n = get_compound_n_members(constructors); i < n; ++i) {
		const ir_entity *entity = get_compound_member(constructors, i);
		ASSERT_AND_RET(get_entity_linkage(entity) & IR_LINKAGE_HIDDEN_USER,
		               "entity without LINKAGE_HIDDEN_USER in constructors is pointless",
		               1);
		/* Mach-O doesn't like labels in this section */
		ASSERT_AND_RET(get_entity_ld_ident(entity),
		               "entity in constructors should have ld_ident=''", 1);
	}
	destructors = get_segment_type(IR_SEGMENT_DESTRUCTORS);
	for (i = 0, n = get_compound_n_members(destructors); i < n; ++i) {
		const ir_entity *entity = get_compound_member(destructors, i);
		ASSERT_AND_RET(get_entity_linkage(entity) & IR_LINKAGE_HIDDEN_USER,
		               "entity without LINKAGE_HIDDEN_USER in destructors is pointless",
		               1);
		/* Mach-O doesn't like labels in this section */
		ASSERT_AND_RET(get_entity_ld_ident(entity),
		               "entity in destructors should have ld_ident=''", 1);
	}
	thread_locals = get_segment_type(IR_SEGMENT_THREAD_LOCAL);
	for (i = 0, n = get_compound_n_members(thread_locals); i < n; ++i) {
		const ir_entity *entity = get_compound_member(thread_locals, i);
		/* this is odd and should not be allowed I think */
		ASSERT_AND_RET(!is_method_entity(entity),
		               "method in THREAD_LOCAL segment", 1);
		ASSERT_AND_RET(! (get_entity_linkage(entity) & IR_LINKAGE_CONSTANT),
		               "thread locals must not be constant", 1);
	}

	return res;
}
