/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer
**
** irflag --- optimization flags
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif


/* 0 - don't do this optimization
   1 - lets see, if there is a better graph */
int opt_cse = 1;
int opt_constant_folding = 1;
int opt_dead_node_elimination = 1;
int optimized = 1;

/* set the flags with set_flagname, get the flag with get_flagname */

inline void
set_opt_cse (int value)
{
  opt_cse = value;
}

inline int
get_opt_cse (void)
{
  return opt_cse;
}

inline void
set_opt_constant_folding (int value)
{
  opt_constant_folding=value;
}

inline int
get_opt_constant_folding (void)
{
  return opt_constant_folding;
}

inline void
set_opt_dead_node_elimination (int value)
{
  opt_dead_node_elimination = value;
}

inline int
get_opt_dead_node_elimination (void)
{
  return opt_dead_node_elimination;
}

inline void
set_optimize (int value)
{
  optimized = value;
}

inline int
get_optimize (void)
{
  return optimized;
}
