/* Copyright (c) 2002 by Universität Karlsruhe (TH).  All Rights Reserved */
/*
* Time-stamp: <Thursday, 19.12.2002, 15:45:43 goetz@i44pc2.info.uni-karlsruhe.de>
*/

/**
   NAME
     exc
   PURPOSE
     Helper functions for exceptions
   S
     not quite complete
   HISTORY
     liekweg - Mar 4, 2002: Created.
   CVS:
     $Id$
***/

# include "irnode.h"

# ifndef _EXC_H_
# define _EXC_H_

# include "st.h"
# include "irop.h"
# include "irouts.h"

#include <stdbool.h>

typedef enum {
  exc_invalid = 0,					/* not yet computed */
  exc_normal,					/* normal CF */

  /* must push a new exc context at entry of block: */
  exc_region,					/* region entry */

  /* must pop current exc context at EXIT of block */
  exc_exit,						/* region exit */

  /* must pop current exc context at entry of block */
  exc_handler,					/* handler entry */

  exc_max						/* maximum value of enum for 'bounds checking' */
} exc_t;

const char *exc_to_string (exc_t);

bool is_handler_entry (ir_graph*, ir_node*);
bool is_region_entry  (ir_graph*, ir_node*);
bool is_handler_block (ir_graph*, ir_node*);
bool is_cont_entry    (ir_graph*, ir_node*);

void     set_Block_exc     (ir_node*, exc_t);
exc_t    get_Block_exc     (ir_node*);

void     set_Node_exc      (ir_node*, exc_t);
exc_t    get_Node_exc      (ir_node*);

/* handler handling  @@@ ajacs specific -- not supported  */
void     set_Block_handler (ir_node*, ir_node*);
ir_node* get_Block_handler (ir_node*);

void     set_Node_handler  (ir_node*, ir_node*);
ir_node* get_Node_handler  (ir_node*);

# endif /* def _EXC_H_ */
