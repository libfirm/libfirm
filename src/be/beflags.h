/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       modifies schedule so flags dependencies are respected.
 * @author      Matthias Braun, Christoph Mallon
 */
#ifndef FIRM_BE_BEFLAGS_H
#define FIRM_BE_BEFLAGS_H

#include "be_types.h"
#include "firm_types.h"
#include <stdbool.h>

/**
 * Callback which rematerializes (=duplicates) a machine node.
 */
typedef ir_node * (*func_rematerialize) (ir_node *node, ir_node *after);

/**
 * Callback function that checks whether a node modifies the flags
 */
typedef bool (*check_modifies_flags) (const ir_node *node);

/**
 * Callback function that checks whether consumers can use the
 * available flags instead of their original ones.
 */
typedef bool (*try_replace_flags) (ir_node *consumers, ir_node *flags_needed, ir_node *available_flags);

/**
 * Walks the schedule and ensures that flags aren't destroyed between producer
 * and consumer of flags. It does so by moving down/rematerialising of the
 * nodes. This does not work across blocks.
 * The callback functions may be NULL if you want to use default
 * implementations.
 */
void be_sched_fix_flags(ir_graph *irg, const arch_register_class_t *flag_cls,
                        func_rematerialize remat_func,
                        check_modifies_flags check_modifies_flags_func,
                        try_replace_flags try_replace_flags_func);

#endif
