/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief     Helper functions for jack exceptions.
 * @author    Florian Liekweg
 * @date      4.3.2002
 * @version   $Id$
 */

/**
   NAME
     exc
   PURPOSE
     Helper functions for exceptions
   S
     not quite complete
***/

# include "irnode.h"

# ifndef _EXC_H_
# define _EXC_H_

# include "st.h"
# include "irop.h"
# include "irouts.h"

#include <stdbool.h>

#ifdef __cplusplus
	extern "C" {
#endif

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

#ifdef __cplusplus
}
#endif

# endif /* def _EXC_H_ */
