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
void be_copy_opt(ir_graph* irg);
void be_copy_opt_done(ir_graph* irg);

#endif
