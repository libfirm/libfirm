/* Copyright (C) 1998 - 2000 by Universitaet Karlsruhe
** All rights reserved.
**
** Authors: Martin Trapp, Christian Schaefer
**
*/

# include <stdio.h>
# include "ident.h"
# include "firm.h"
# include "mangle.h"
# include "xp_help.h"

void
init_firm (void)
{
  xprintf_register ('I', ident_print);
  xprintf_register ('v', tarval_print);
  xprintf_register ('R', ir_node_print);
  id_init ();
  tarval_init_1 ();
  init_mode ();
  tarval_init_2 ();
  init_mangle ();
  init_op ();
  init_cons ();
}
