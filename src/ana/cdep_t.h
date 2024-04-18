/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   control dependence analysis
 * @author  Christoph Mallon
 */
#ifndef FIRM_ANA_CDEP_T_H
#define FIRM_ANA_CDEP_T_H

#include "cdep.h"
#include "irnode_t.h"

/**
 * An entry in the control dependence list.
 */
struct ir_cdep {
	ir_node *node;  /**< A node on which the current block is control dependent on. */
	ir_cdep *next;  /**< Link to the next one if any. */
};

static inline ir_node *_get_cdep_node(const ir_cdep *cdep)
{
	return skip_Id(cdep->node);
}

static inline ir_cdep *_get_cdep_next(const ir_cdep *cdep)
{
	return cdep->next;
}

#define get_cdep_node(cdep)     _get_cdep_node(cdep)
#define get_cdep_next(cdep)     _get_cdep_next(cdep)

#endif
