/*
 * Project:     libFIRM
 * File name:   ir/ir/firmstat.h
 * Purpose:     Statistics for Firm.
 * Author:      Michael Beck
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 2004 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


# ifndef _FIRMSTAT_H_
# define _FIRMSTAT_H_

#ifdef FIRM_STATISTICS

/**
 * initialize the statistics module.
 */
void stat_init(void);

/**
 * A new IR op is registered.
 */
void stat_new_ir_op(const ir_op *op);

/**
 * An IR op is freed.
 */
void stat_free_ir_op(const ir_op *op);

/**
 * A new node is created.
 */
void stat_new_node(const ir_node *node);

#else

#define stat_init()
#define stat_new_ir_op(op)
#define stat_free_ir_op(op)
#define stat_new_node(node)

#endif


#endif /* _FIRMSTAT_H_ */
