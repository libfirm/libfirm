/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   attributes attached to all TEMPLATE nodes
 */
#ifndef FIRM_BE_TEMPLATE_TEMPLATE_NODES_ATTR_H
#define FIRM_BE_TEMPLATE_TEMPLATE_NODES_ATTR_H

#include "firm_types.h"

typedef struct TEMPLATE_attr_t TEMPLATE_attr_t;

struct TEMPLATE_attr_t
{
	ir_tarval *value;
	ir_entity *entity;
};

#endif
