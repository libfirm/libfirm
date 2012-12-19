/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declarations for TEMPLATE backend -- private header
 */
#ifndef FIRM_BE_TEMPLATE_BEARCH_TEMPLATE_T_H
#define FIRM_BE_TEMPLATE_BEARCH_TEMPLATE_T_H

#include "debug.h"
#include "TEMPLATE_nodes_attr.h"
#include "be.h"
#include "beemitter.h"
#include "set.h"

typedef struct TEMPLATE_isa_t {
	arch_env_t  base;      /**< must be derived from arch_isa */
} TEMPLATE_isa_t;

#endif
