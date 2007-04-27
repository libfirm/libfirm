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

/*
 * Project:     libFIRM
 * File name:   ir/opt/loop_unrolling.h
 * Purpose:     Loop unrolling.
 * Author:      Beyhan Veliev
 * Modified by:
 * Created:     16.11.2004
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 */

/**
 * @file loop_unrolling.h
 *
 * Loop unrolling.
 *
 * @author Beyhan Veliev
 */
#ifndef _LOOP_UNROLLING_H_
#define _LOOP_UNROLLING_H_

#include "irgraph.h"

/**
 * Do Loop unrolling in the given graph.
 */
void optimize_loop_unrolling(ir_graph *irg);

#endif  /* _LOOP_UNROLLING_H_ */
