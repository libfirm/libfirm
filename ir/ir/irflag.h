/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Christian Schaefer
**
** irflag --- optimization flags
*/

#ifndef _IRFLAG_H_
#define _IRFLAG_H_

extern int opt_cse;
extern int opt_constant_folding;

/* set the flags with set_flagname, get the flag with get_flagname */

void set_opt_cse (int value);
int  get_opt_cse (void);
void set_opt_constant_folding (int value);
int  get_opt_constant_folding (void);
void set_opt_dead_node_elimination (int value);
int  get_opt_dead_node_elimination (void);

void set_optimize (int value);
int  get_optimize (void);

#endif
