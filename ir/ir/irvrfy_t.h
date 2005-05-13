/*
 * Project:     libFIRM
 * File name:   ir/ir/irvrfy_t.h
 * Purpose:     New checker of irnodes for correctness.
 * Author:      Michael Beck
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifndef _IRVRFY_T_H_
#define _IRVRFY_T_H_

#include "irvrfy.h"
#include "irdump.h"

extern node_verification_t opt_do_node_verification;
extern const char *firm_vrfy_failure_msg;

#ifdef NDEBUG
/*
 * in RELEASE mode, returns ret if the expression expr evaluates to zero
 * in ASSERT mode, asserts the expression expr (and the string string).
 */
#define ASSERT_AND_RET(expr, string, ret)       if (!(expr)) return (ret)

/*
 * in RELEASE mode, returns ret if the expression expr evaluates to zero
 * in ASSERT mode, executes blk if the expression expr evaluates to zero and asserts expr
 */
#define ASSERT_AND_RET_DBG(expr, string, ret, blk)      if (!(expr)) return (ret)
#else
#define ASSERT_AND_RET(expr, string, ret) \
do { \
  if (opt_do_node_verification == NODE_VERIFICATION_ON) {\
    if (!(expr) && current_ir_graph != get_const_code_irg()) \
    dump_ir_block_graph(current_ir_graph, "-assert"); \
    assert((expr) && string); } \
  if (!(expr)) { \
    if (opt_do_node_verification == NODE_VERIFICATION_REPORT) \
      fprintf(stderr, #expr " : " string "\n"); \
    firm_vrfy_failure_msg = #expr " && " string; \
    return (ret); \
  } \
} while(0)

#define ASSERT_AND_RET_DBG(expr, string, ret, blk) \
do { \
  if (!(expr)) { \
    firm_vrfy_failure_msg = #expr " && " string; \
    if (opt_do_node_verification != NODE_VERIFICATION_ERROR_ONLY) { blk; } \
    if (opt_do_node_verification == NODE_VERIFICATION_REPORT) \
      fprintf(stderr, #expr " : " string "\n"); \
    else if (opt_do_node_verification == NODE_VERIFICATION_ON) \
      assert((expr) && string); \
    return (ret); \
  } \
} while(0)

#endif

/**
 * Set the default verify_node and verify_proj_node operation.
 */
void firm_set_default_verifyer(ir_op *op);

#endif /* _IRVRFY_T_H_ */
