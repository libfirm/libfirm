/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   declarations for AMD64 backend -- private header
 */
#ifndef FIRM_BE_AMD64_BEARCH_AMD64_T_H
#define FIRM_BE_AMD64_BEARCH_AMD64_T_H

#include "bearch.h"

typedef struct amd64_isa_t            amd64_isa_t;

struct amd64_isa_t {
	arch_env_t  base;      /**< must be derived from arch_isa */
};

#define AMD64_REGISTER_SIZE 8

#endif
