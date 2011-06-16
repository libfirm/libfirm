/**
 * @file   lpp_show.c
 * @date   26.07.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#include "lpp_net.h"
#include "xmalloc.h"

int main(int argc, char *argv[])
{
  const char *host;
  char **solvers;

  if(argc < 2)
    return 1;

  host = argv[1];

  solvers = lpp_get_solvers(host);
  if(solvers) {
    int i;
    for(i = 0; solvers[i]; ++i) {
      printf("%s\n", solvers[i]);
      xfree(solvers[i]);
    }

    xfree(solvers);
  }

  else
    fprintf(stderr, "lpp server not found\n");

  return 0;
}
