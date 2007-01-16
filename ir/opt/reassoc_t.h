/*
 * Project:     libFIRM
 * File name:   ir/opt/reassoc_t.h
 * Purpose:     Reassociation
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file reassoc_t.h
 *
 * Reassociation optimization.
 *
 * @author Michael Beck
 */
#ifndef _REASSOC_T_H_
#define _REASSOC_T_H_

#include "reassoc.h"

/**
 * Sets the default reassociation operation for an ir_op_ops.
 *
 * @param code   the opcode for the default operation
 * @param ops    the operations initialized
 *
 * @return
 *    The operations.
 */
ir_op_ops *firm_set_default_reassoc(ir_opcode code, ir_op_ops *ops);

/** Initialise the ressociation optimization */
void firm_init_reassociation(void);

#endif /* _REASSOC_T_H_ */
