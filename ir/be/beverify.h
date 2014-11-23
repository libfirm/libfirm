/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Various verify routines that check a scheduled graph for correctness.
 * @author      Matthias Braun
 * @date        05.05.2006
 */
#ifndef FIRM_BE_BEVERIFY_H
#define FIRM_BE_BEVERIFY_H

#include <stdbool.h>

#include "bearch.h"
#include "firm_types.h"

/**
 * Verifies, that the register pressure for a given register class doesn't
 * exceed the limit of available registers.
 *
 * @param irg        The irg to check.
 * @param cls        The register class to check.
 * @return           true if the pressure is valid, false otherwise.
 */
bool be_verify_register_pressure(ir_graph *irg,
                                 const arch_register_class_t* cls);

/**
 * Does some sanity checks on the schedule.
 *
 * @param irg   The irg to check
 * @return      true if the schedule is valid, false otherwise
 */
bool be_verify_schedule(ir_graph *irg);

/**
 * Verify spillslots
 */
bool be_verify_spillslots(ir_graph *irg,
                          get_frame_entity_func get_frame_entity);

/**
 * Verify register allocation: Checks that no 2 live nodes have the same
 * register assigned, also checks that each scheduled node has a register
 * assigned.
 *
 * @param irg   The graph to check
 * @param ignore_sp_problems  if true do not report problems involving values
 *                            with arch_register_req_type_produces_sp
 * @return      true if verify succeeded, false otherwise
 */
bool be_verify_register_allocation(ir_graph *irg, bool ignore_sp_problems);

/**
 * Check the given liveness information against a freshly computed one.
 */
void be_liveness_check(be_lv_t *lv);

#endif
