/**
 * @file   lpp_test.c
 * @date   20.07.2005
 * @author Sebastian Hack
 *
 * Small test program for lpp solving
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#include "lpp.h"
#include "lpp_net.h"

int main(int argc, char *argv[])
{
  int i;
  lpp_t *lpp = new_lpp("test", lpp_minimize);

  lpp_add_cst(lpp, "a", lpp_equal, 1.0);

  lpp_add_var(lpp, "x", lpp_binary, 1.0);
  lpp_add_var(lpp, "y", lpp_binary, 1.0);

  lpp_set_factor(lpp, "a", "x", 1.0);
  lpp_set_factor(lpp, "a", "y", 1.0);
  lpp_set_log(lpp, stdout);

  lpp_solve_net(lpp, argc > 1 ? argv[1] : "localhost", argc > 2 ? argv[2] : "dummy");

  for(i = 0; i < lpp->var_next; ++i) {
    lpp_name_t *name = lpp->vars[i];
    printf("%10s %4d %10f\n", name->name, name->nr, name->value);
  }

  return 0;
}
