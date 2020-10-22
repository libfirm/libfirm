/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       SPM support
 * @author      Daniel Biester
 * @date        13.08.2020
 */
#ifndef FIRM_BE_SPM_H
#define FIRM_BE_SPM_H
#include "list.h"
typedef struct node_data node_data;

node_data *spm_get_mem_read_node_data(void *id, int size);
node_data *spm_get_mem_write_node_data(void *id, int size);
node_data *spm_get_callee_node_data(ir_entity *ent);

void spm_calculate_dprg_info(void);

void spm_find_memory_allocation(node_data * (*retrieve_spm_node_data)(ir_node *));

void spm_test_call(void);

#endif
