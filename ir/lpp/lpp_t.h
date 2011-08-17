/*
 * Copyright (C) 2005-2011 University of Karlsruhe.  All right reserved.
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
 * @author  Sebastian Hack
 */
#ifndef LPP_LPP_T_H
#define LPP_LPP_T_H

#include "lpp.h"
#include "lpp_comm.h"

/**
 * Serialize a lpp to a file descriptor.
 * @param comm The file descriptor.
 * @param lpp The lpp.
 * @param with_names Also send the names of constraints/variables.
 */
void lpp_serialize(lpp_comm_t *comm, const lpp_t *lpp, int with_names);

/**
 * Deserialize an lpp from a file descriptor.
 * @param comm The file descriptor.
 * @param with_names Also receive names of constraints/variables.
 * @return The Problem.
 */
lpp_t *lpp_deserialize(lpp_comm_t *comm);

/**
 * Serialize values of the lpps for a given value kind.
 * This function only serializes values of the given kind.
 * @param fd   The file descriptor to serialize to.
 * @param lpp  The problem.
 * @param kind The value kind.
 */
void lpp_serialize_values(lpp_comm_t *comm, const lpp_t *lpp, lpp_value_kind_t kind);

/**
 * Desrialize values from a stream.
 * @param fd   The file descriptor to read from.
 * @param lpp  The problem to set the values.
 * @param kind The value kind the values shall be assigned.
 */
void lpp_deserialize_values(lpp_comm_t *comm, lpp_t *lpp, lpp_value_kind_t kind);

void lpp_serialize_stats(lpp_comm_t *comm, const lpp_t *lpp);
void lpp_deserialize_stats(lpp_comm_t *comm, lpp_t *lpp);

#endif
