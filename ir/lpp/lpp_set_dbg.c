/**
 * @file   lpp_set_dbg.c
 * @date   04.08.2006
 * @author Sebastian Hack
 *
 * Copyright (C) 2006 Universitaet Karlsruhe
 * Released under the GPL
 */

#include <stdlib.h>
#include <stdio.h>

#include "lpp.h"
#include "lpp_net.h"

int main(int argc, char *argv[])
{
  if(argc < 3) {
	  fprintf(stderr, "syntax: lpp_set_dbg <host> <mask>\n");
  }

  else {
	  const char *host = argv[1];
	  int mask = atoi(argv[2]);
	  lpp_set_dbg(host, mask);
  }
  return 0;
}
