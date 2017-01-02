/*
 * This file is part of libFirm.
 * Copyright (C) 2017 University of Karlsruhe.
 */

#ifndef LIBFIRM_IR_IR_LINK_H
#define LIBFIRM_IR_IR_LINK_H

#include "firm_types.h"

ir_entity *ir_link_entity(ident *name, ir_type *type, ir_entity_kind kind, ir_type *owner, ir_linkage linkage, ir_volatility volatility, ir_visibility visibility);

void ir_adjust_visibility(ir_entity *entity);

#endif
