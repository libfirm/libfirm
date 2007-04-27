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
 * Chordal register allocation.
 * @author Christian Wuerdig
 * @date 2005/12/14
 * @cvsid $Id$
 */
#ifndef _BELOWER_H_
#define _BELOWER_H_

#include "beirg.h"

void assure_constraints(be_irg_t *birg);
void lower_nodes_after_ra(be_irg_t *birg, int do_copy);

#endif /* _BELOWER_H_ */
