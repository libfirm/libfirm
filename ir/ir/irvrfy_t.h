/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief    New checker of irnodes for correctness.
 * @author   Michael Beck
 * @version  $Id$
 */
#ifndef FIRM_IR_IRVRFY_T_H
#define FIRM_IR_IRVRFY_T_H

#include "irflag_t.h"
#include "irvrfy.h"
#include "irdump.h"

extern const char *firm_vrfy_failure_msg;

#ifdef NDEBUG
/*
 * in RELEASE mode, returns ret if the expression expr evaluates to zero
 * in ASSERT mode, asserts the expression expr (and the string string).
 */
#define ASSERT_AND_RET(expr, string, ret)       do { if (!(expr)) return (ret); } while (0)

/*
 * in RELEASE mode, returns ret if the expression expr evaluates to zero
 * in ASSERT mode, executes blk if the expression expr evaluates to zero and asserts expr
 */
#define ASSERT_AND_RET_DBG(expr, string, ret, blk)     do { if (!(expr)) return (ret); } while (0)
#else
#define ASSERT_AND_RET(expr, string, ret) \
do { \
  if (opt_do_node_verification == FIRM_VERIFICATION_ON) {\
    if (!(expr) && current_ir_graph != get_const_code_irg()) \
      dump_ir_block_graph(current_ir_graph, "-assert"); \
    assert((expr) && string); } \
  if (!(expr)) { \
    if (opt_do_node_verification == FIRM_VERIFICATION_REPORT) \
      fprintf(stderr, #expr " : " string "\n"); \
    firm_vrfy_failure_msg = #expr " && " string; \
    return (ret); \
  } \
} while(0)

#define ASSERT_AND_RET_DBG(expr, string, ret, blk) \
do { \
  if (!(expr)) { \
    firm_vrfy_failure_msg = #expr " && " string; \
    if (opt_do_node_verification != FIRM_VERIFICATION_ERROR_ONLY) { blk; } \
    if (opt_do_node_verification == FIRM_VERIFICATION_REPORT) \
      fprintf(stderr, #expr " : " string "\n"); \
    else if (opt_do_node_verification == FIRM_VERIFICATION_ON) { \
      if (!(expr) && current_ir_graph != get_const_code_irg()) \
        dump_ir_block_graph(current_ir_graph, "-assert"); \
      assert((expr) && string); \
    } \
    return (ret); \
  } \
} while(0)

#endif

/**
 * Set the default verify_node and verify_proj_node operation for an ir_op_ops.
 */
void firm_set_default_verifyer(ir_opcode code, ir_op_ops *ops);

#endif
