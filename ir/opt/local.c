/*
 * This file is part of libFirm.
 * Copyright (C) 2012 Karlsruhe Institute of Technology.
 */

/**
 * @brief   Apply local opts until fixpoint
 * @author  Andreas Zwinkau
 */
#include "irgopt.h"

void local_opts(ir_graph *irg)
{
	optimize_graph_df(irg);
}
