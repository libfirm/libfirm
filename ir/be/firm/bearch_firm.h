
/**
 * @file   bearch_firm.h
 * @date   11.05.2005
 * @author Sebastian Hack
 *
 * An instruction set architecture made up of firm nodes.
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _BEARCH_FIRM_H
#define _BEARCH_FIRM_H

#include "../bearch_t.h"

extern const arch_isa_if_t firm_isa;
extern const arch_irn_handler_t firm_irn_handler;

/* TODO UGLY*/
int is_Imm(const ir_node *irn);

#endif /* _BEARCH_FIRM_H */
