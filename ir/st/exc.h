/* Copyright (c) 2002 by Universität Karlsruhe (TH).  All Rights Reserved */
//
// Time-stamp: <02/03/22 17:03:05 liekweg>
//

/***
   NAME
     exc
   PURPOSE
     Helper functions for exceptions
   NOTES
     not quite complete
   HISTORY
     liekweg - Mar 4, 2002: Created.
   CVS:
     $Id$
***/

# include "irnode.h"

# ifndef _EXC_H_
# define _EXC_H_

typedef enum {
  exc_invalid,					/* not yet computed */
  exc_normal,					/* normal CF */
  exc_entry,					/* handler entry */
  exc_handler,					/* handler block */
  exc_region,					/* region entry */
  exc_cont						/* cont block */
} exc_t;

bool is_handler_entry (ir_graph*, ir_node*);
bool is_region_entry  (ir_graph*, ir_node*);
bool is_handler_block (ir_graph*, ir_node*);
bool is_cont_entry    (ir_graph*, ir_node*);

# endif /* def _EXC_H_ */
