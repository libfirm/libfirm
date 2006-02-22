/*
 * Project:     libFIRM
 * File name:   ir/opt/data_flow_scalar_replace.h
 * Purpose:     scalar replacement of compounds
 * Author:      Beyhan Veliev
 * Created:
 * CVS-ID:
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef _DATA_FLOW_SCALAR_REPLACE_H_
#define _DATA_FLOW_SCALAR_REPLACE_H_

#include "irgraph.h"


/**
 * Do the scalar replacement optimization.
 * Make a date flow analyze and split the
 * data flow edges.
 *
 * @param irg  the graph which should be optimized
 */
void data_flow_scalar_replacement_opt(ir_graph *irg);

#endif /* _DATA_FLOW_SCALAR_REPLACE_H_*/
