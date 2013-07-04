/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief    New checker of irnodes for correctness.
 * @author   Michael Beck
 */
#ifndef FIRM_IR_IRVERIFY_T_H
#define FIRM_IR_IRVERIFY_T_H

#include "irflag_t.h"
#include "irverify.h"
#include "irdump.h"

#include "beutil.h"

extern const char *firm_verify_failure_msg;

#define ASSERT_AND_RET(expr, string, ret) \
do { \
  if (opt_do_node_verification == FIRM_VERIFICATION_ON) {\
    if (!(expr) && current_ir_graph != get_const_code_irg()) \
      dump_ir_graph(current_ir_graph, "assert"); \
    assert((expr) && string); } \
  if (!(expr)) { \
    if (opt_do_node_verification == FIRM_VERIFICATION_REPORT) \
      fprintf(stderr, #expr " : " string "\n"); \
    firm_verify_failure_msg = #expr " && " string; \
    return (ret); \
  } \
} while(0)

#define ASSERT_AND_RET_DBG(expr, string, ret, blk) \
do { \
  if (!(expr)) { \
    firm_verify_failure_msg = #expr " && " string; \
    if (opt_do_node_verification != FIRM_VERIFICATION_ERROR_ONLY) { blk } \
    if (opt_do_node_verification == FIRM_VERIFICATION_REPORT) \
      fprintf(stderr, #expr " : " string "\n"); \
    else if (opt_do_node_verification == FIRM_VERIFICATION_ON) { \
      if (!(expr) && current_ir_graph != get_const_code_irg()) \
        dump_ir_graph(current_ir_graph, "assert"); \
      assert((expr) && string); \
    } \
    return (ret); \
  } \
} while(0)

/**
 * Set the default verify_node and verify_proj_node operations.
 */
void ir_register_verify_node_ops(void);

#endif
