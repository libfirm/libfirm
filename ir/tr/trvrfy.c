
#include "trvrfy.h"

static int check_class(type *tp) {
  int i, j, k;
  int found;

  //printf("\n"); DDMT(tp);

  for (i = 0; i < get_class_n_members(tp); i++) {

    entity *mem = get_class_member(tp, i);
    assert(mem && "NULL members not allowed");
    //printf(" %d, %d", get_entity_n_overwrites(mem), get_class_n_supertypes(tp)); DDME(mem);
    if (!mem) return error_null_mem;
    assert(get_entity_n_overwrites(mem) <= get_class_n_supertypes(tp));
    for (j = 0; j < get_entity_n_overwrites(mem); j++) {
      entity *ovw = get_entity_overwrites(mem, j);
      //printf(" overwrites: "); DDME(ovw);
      /* Check whether ovw is member of one of tp's supertypes. If so,
	 the representation is correct. */
      found = false;
      for (k = 0; k < get_class_n_supertypes(tp); k++) {
	if (get_class_member_index(get_class_supertype(tp, k), ovw) >= 0) {
	  found = true;
	  break;
	}
      }
      if (!found) {
	DDMT(tp); DDME(mem);
	assert(found && "overwrites an entity not contained in direct supertype");
	return error_ent_not_cont;
      }
    }

  }
  return 0;
}

static int check_type(type *tp) {
  switch (get_type_tpop_code(tp)) {
  case tpo_class:
    return check_class(tp);
  default: break;
  }
  return 0;
}

struct myenv {
  int res;
  struct obstack *obst;
};

static void on_obstack(ir_node *n; void *env) {
  struct obstack *obst = ((myenv *)env)->obst;

  /* n must be on the obstack obst. */

  ((myenv *)env)->res = 0;
}

static int constant_on_wrong_obstack(ir_node *n) {
  struct myenv env;
  env.res = 0;  /* false, not on wrong obstack */
  env.obst = get_irg_obst(get_const_code_irg());
  irg_walk(n, on_obstack, NULL, (void *)&env);
  return env.res;
}

static int constants_on_wrong_obstack(entity *ent) {
  if (get_entity_variability(ent) == uninitialize) return 0;

  if (is_compound_entity(ent)) {
    int i;
    for (i = 0; i < get_compound_ent_n_values(ent); i++) {
      if (constant_on_wrong_obstack(get_compound_entity_value(ent, i)));
	return 1;
    }
  } else {
    return constant_on_wrong_obstack(get_atomic_entity_value(ent));
  }
  return 0;
}

static int check_entity(entity *ent) {
  if (constants_on_wrong_obstack(ent))
    return error_const_on_wrong_obstack;
  return 0;
}

static void check_tore(type_or_ent *tore, void *env) {
  int *res = env;
  if (is_type(tore)) {
    *res = check_type((type *)tore);
  } else {
    assert(is_entity(tore));
    *res = check_entity((entity *)tore);
  }
}


int tr_vrfy(void) {
  int res;

  type_walk(check_tore, NULL, &res);
  return res;
}
