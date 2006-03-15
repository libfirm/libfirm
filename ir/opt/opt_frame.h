/*
 * Project:     libFIRM
 * File name:   ir/opt/opt_frame.c
 * Purpose:     optimize the frame type
 * Author:      Michael Beck
 * Created:     15.03.2006
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2006 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
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
