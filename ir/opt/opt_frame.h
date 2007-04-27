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
 * File name:   ir/opt/opt_frame.c
 * Purpose:     optimize the frame type
 * Author:      Michael Beck
 * Created:     15.03.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2006 Universität Karlsruhe
 */
#ifndef _OPT_FRAME_H_
#define _OPT_FRAME_H_

/**
 * @file opt_frame.h
 *
 * Optimize the frame type by removing unused type members.
 */

#include "firm_types.h"

/**
 * Optimize the frame type of an irg by removing
 * never touched entities.
 *
 * @param irg  The graph whose frame type will be optimized
 *
 * This function did not change the graph, only it's frame type.
 * The layout state of the frame type will be set to layout_undefined
 * if entities were removed.
 */
void opt_frame_irg(ir_graph *irg);

#endif /* _OPT_FRAME_H_ */
