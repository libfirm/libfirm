/* -*- c -*- */

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
 * @brief    walk along memory edges
 * @author   Florian
 * @date     Mon 18 Oct 2004
 * @version  $Id$
 */
# ifndef FIRM_ANA2_IRMEMWALK_H
# define FIRM_ANA2_IRMEMWALK_H

# include "irgraph.h"
# include "irgwalk.h"

void irg_walk_mem (ir_graph*, irg_walk_func*, irg_walk_func*, void*);
int get_irg_is_mem_visited (ir_graph*);

# endif


/*
  $Log$
  Revision 1.1  2004/10/21 11:09:37  liekweg
  Moved memwalk stuf into irmemwalk
  Moved lset stuff into lset
  Moved typalise stuff into typalise


 */
