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
  /* register the character 'I' as variable for ident outputs. */
  xprintf_register ('I', ident_print);
  /* register the character 'v' as variable for tarval outputs. */
  xprintf_register ('v', tarval_print);
  /* register the character 'R' as variable for ir node outputs. */
  xprintf_register ('R', ir_node_print);
  /* initialize all ident stuff */
  id_init ();
  /* create an obstack and put alle tarvals in a pdeq */
  tarval_init_1 ();
  /* initialize all modes an ir node can consist of */
  init_mode ();
  /* initialize tarvals, and floating point arithmetic */
  tarval_init_2 ();
  /* kind of obstack initialization */
  init_mangle ();
  /* initalize all op codes an irnode can consist of */
  init_op ();
  /* called once for each run of this library, empty at this moment!!! */
  init_cons ();
  /* Builds a construct allowing to access all information to be constructed
     later. */

  init_irprog ();
}
