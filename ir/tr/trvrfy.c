
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


static int check_entity(entity *ent) {
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
