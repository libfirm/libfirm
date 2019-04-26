/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief  Declares different kind of edges between nodes
 * @date   29.08.2006
 * @author Sebastian Hack
 */
#ifndef FIRM_IR_IREDGEKINDS_H
#define FIRM_IR_IREDGEKINDS_H

#include "firm_types.h"

#include "begin.h"

/** Supported Edge kinds.
 * @ingroup iredges
 */
typedef enum ir_edge_kind_t {
	EDGE_KIND_NORMAL,                    /**< Normal data flow edges. */
	EDGE_KIND_FIRST = EDGE_KIND_NORMAL,
	EDGE_KIND_BLOCK,                     /**< Block to Block control flow edges. */
	EDGE_KIND_LAST = EDGE_KIND_BLOCK,
} ir_edge_kind_t;
ENUM_COUNTABLE(ir_edge_kind_t)

#include "end.h"

#endif
