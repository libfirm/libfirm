/*
 * Project:     libFIRM
 * File name:   ir/ana/irbackedge_t.h
 * Purpose:     Access function for backedges -- private header.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     7.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _IRBACKEDGE_T_H_
#define _IRBACKEDGE_T_H_

/**
 * Allocate a new backedge array on the obstack for given size.
 */
int *new_backedge_arr(struct obstack *obst, int size);

/**
 * allocate new backedge info for nodes.
 */
void new_backedge_info(ir_node *n);

/**
 * Adapts backedges array to new size.
 * Must be called if in array of irnode is changed.  Else
 * Segmentation faults might occur.
 */
void fix_backedges(struct obstack *obst, ir_node *n);

#endif /* _IRBACKEDGE_T_H_ */
