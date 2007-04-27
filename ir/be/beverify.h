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
 * Author:    Matthias Braun
 * Date:      05.05.2006
 * Copyright: (c) Universitaet Karlsruhe
 * License:   This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 * CVS-Id:    $Id$
 */

/**
 * @file beverify.h
 *
 * Various verify routines that check a scheduled graph for correctness
 *
 * @author Matthias Braun
 */
#ifndef BEVERIFY_H_
#define BEVERIFY_H_

#include "irgraph.h"

#include "beirg.h"
#include "bearch.h"

/**
 * Verifies, that the register pressure for a given register class doesn't exceed the limit
 * of available registers.
 *
 * @param birg       The backend IRG.
 * @param cls        The register class to check.
 * @param irg        The irg to check.
 * @return			 1 if the pressure is valid, 0 otherwise.
 */
int be_verify_register_pressure(const be_irg_t *birg, const arch_register_class_t* cls, ir_graph *irg);

/**
 * Does some sanity checks on the schedule.
 *
 * @param irg	The irg to check
 * @return		1 if the schedule is valid, 0 otherwise
 */
int be_verify_schedule(const be_irg_t *birg);

/**
 * Verify spillslots
 *
 * @param irg   The irg to check
 * @return      1 if spillslots are valid, 0 otherwise
 */
int be_verify_spillslots(const arch_env_t *arch_env, ir_graph *irg);

/**
 * Verify register allocation: Checks that no 2 live nodes have the same
 * register assigned, also checks that each scheduled node has a register
 * assigned.
 *
 * @param irg  The irg to check
 * @return     1 if verify succeeded, 0 otherwise
 */
int be_verify_register_allocation(const arch_env_t *arch_env, ir_graph *irg);

/**
 * Verify that out edges are valid
 *
 * @param irg  The irg to check
 * @param 	   1 if verify succeeded, 0 otherwise
 */
int be_verify_out_edges(ir_graph *irg);

#endif /* BEVERIFY_H_ */
