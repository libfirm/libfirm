/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer
**
** irflag --- optimization flags
*/

# include "irflag.h"

int opt_cse = 0;
int opt_constant_folding = 1;
int optimized = 1;

/* set the flags with set_flagname, get the flag with get_flagname */

void
set_opt_cse (int value)
{
  opt_cse = value;
}

int
get_opt_cse (void)
{
  return opt_cse;
}

void
set_opt_constant_folding (int value)
{
  opt_constant_folding=value;
}

int
get_opt_constant_folding (void)
{
  return opt_constant_folding;
}

void
set_optimize (int value)
{
  optimized = value;
}

int
get_optimize (void)
{
  return optimized;
}
