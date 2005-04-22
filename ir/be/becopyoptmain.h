/**
 * Main header for the optimization reducing the copies needed for:
 * - phi coalescing
 * - register-constrained nodes
 *
 * Checker included.
 * By request some statistics are collected too.
 *
 * @author Daniel Grund
 * @date 11.04.2005
 */

#ifndef _BECOPYOPTMAIN_H
#define _BECOPYOPTMAIN_H

#include "irgraph.h"

void be_copy_opt_init(void);
void be_copy_opt(ir_graph* irg, const arch_isa_if_t *isa, const arch_register_class_t *cls);

#endif
