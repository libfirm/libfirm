/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file implements the common parts of IR transformation from
 *              firm into amd64-Firm.
 * @author      Tobias Rapp
 */
#ifndef FIRM_BE_AMD64_AMD64_COMMON_TRANSFORM_H
#define FIRM_BE_AMD64_AMD64_COMMON_TRANSFORM_H

#include "firm_types.h"
#include "bearch_amd64_t.h"
#include "amd64_nodes_attr.h"

/** Constructs an amd64_insn_mode_t from an ir_mode.
  */
amd64_insn_mode_t get_insn_mode_from_mode(const ir_mode *mode);

/** Creates an entity for a constant floating point value.
  */
ir_entity *create_float_const_entity(ir_graph *const irg,
                                     ir_tarval *const tv);

/** Creates an amd64 floating point constant from a given tarval.
  */
ir_node *create_float_const(dbg_info *dbgi, ir_node *block,
                                   ir_tarval *tv);

/** Creates a tarval with the given mode and only
  * the most-significant (first) bit set.
  *
  * @param mode The mode of the resulting tarval, which lso decides
  * between 32 and 64 bit long tarval.
  */
ir_tarval *create_sign_tv(ir_mode *mode);


#endif
