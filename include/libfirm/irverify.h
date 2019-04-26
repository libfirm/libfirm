/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    Check irnodes for correctness.
 * @author   Christian Schaefer, Goetz Lindenmaier, Till Riedel, Matthias Braun
 */
#ifndef FIRM_IR_IRVERIFY_H
#define FIRM_IR_IRVERIFY_H

#include "firm_types.h"

#include "begin.h"

/**
 * @defgroup irverify  Correctness Tests
 * @{
 */

/**
 * Tests @p node for well-formedness.
 * This mostly tests modes of inputs/outputs. Only local properties are tested
 * global properties are only checked by irg_verify().
 * @return NON-zero if no problems were found
 */
FIRM_API int irn_verify(const ir_node *node);

/**
 * Calls irn_verify() for each node in irg. Also checks some global properties
 * like all (non-phi) operands dominating their points of usage; Also checks
 * if the control flow parts of a graph are valid.
 *
 * @param irg  the IR-graph to check
 * @return NON-zero if no problems were found
 */
FIRM_API int irg_verify(ir_graph *irg);

/**
 * Convenience function: Checks graph for errors, in case of error the graph
 * is dumped to a file with "-assert" suffix and the program aborted.
 */
FIRM_API void irg_assert_verify(ir_graph *irg);

/** @} */

#include "end.h"

#endif
