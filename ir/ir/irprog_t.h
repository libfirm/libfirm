/*
 * Project:     libFIRM
 * File name:   ir/ir/irprog_t.h
 * Purpose:     Entry point to the representation of a whole program 0-- private header.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     2000
 * CVS-ID:      $Id$
 * Copyright:   (c) 2000-2007 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irprog_t.h
 */

#ifndef _FIRM_IR_IRPROG_T_H_
#define _FIRM_IR_IRPROG_T_H_

#ifdef HAVE_CONFIG_H
#include "firm_config.h"
#endif

#include "irprog.h"
#include "irgraph.h"
#include "pseudo_irg.h"
#include "ircgcons.h"
#include "firm_common_t.h"
#include "typegmod.h"
#include "irtypeinfo.h"
#include "tr_inheritance.h"
#include "irmemory.h"

#include "callgraph.h"
#include "field_temperature.h"
#include "execution_frequency.h"

#include "array.h"

/** ir_prog */
struct ir_prog {
  firm_kind kind;                 /**< must be k_ir_prog */
  ident     *name;                /**< A file name or the like. */
  ir_graph  *main_irg;            /**< The entry point to the compiled program
                                       or NULL if no poit exist. */
  ir_graph **graphs;              /**< A list of all graphs in the ir. */
  ir_graph **pseudo_graphs;       /**< A list of all pseudo graphs in the ir. See pseudo_irg.c */
  ir_graph  *const_code_irg;      /**< This ir graph gives the proper environment
                                       to allocate nodes the represent values
                                       of constant entities. It is not meant as
                                       a procedure.  */
  ir_type   *glob_type;           /**< The global type.  Must be a class as it can
                                       have fields and procedures.  */
  ir_type   *tls_type;            /**< The thread local storage type.  Must be a struct as it can
                                       only have fields.  */
  ir_type  **types;               /**< A list of all types in the ir. */
  ir_mode  **modes;               /**< A list of all modes in the ir. */
  ir_op    **opcodes;             /**< A list of all opcodes in the ir. */

  /* -- states of and access to generated information -- */
  irg_phase_state phase_state;    /**< The state of construction. */

  ip_view_state ip_view;          /**< The state of interprocedural view. */

  irg_outs_state outs_state;      /**< The state of out edges of ir nodes. */
  ir_node **ip_outedges;          /**< A huge Array that contains all out edges
                                       in interprocedural view. */
  irg_outs_state trouts_state;    /**< The state of out edges of type information. */

  irg_callee_info_state callee_info_state; /**< Validity of callee information.
                                                Contains the lowest value or all irgs.  */
  ir_typeinfo_state typeinfo_state;    /**< Validity of type information. */
  inh_transitive_closure_state inh_trans_closure_state;  /**< trans closure of inh relations. */

  irp_callgraph_state callgraph_state; /**< The state of the callgraph. */
  ir_loop *outermost_cg_loop;          /**< For callgraph analysis: entry point
                                            to looptree over callgraph. */
  int max_callgraph_loop_depth;        /**< needed in callgraph. */
  int max_callgraph_recursion_depth;   /**< needed in callgraph. */
  double max_method_execution_frequency;  /**< needed in callgraph. */
  irp_temperature_state temperature_state; /**< accumulated temperatures computed? */
  exec_freq_state execfreq_state;        /**< The state of execution frequency information */
  loop_nesting_depth_state lnd_state;  /**< The state of loop nesting depth information. */
  ir_class_cast_state class_cast_state;    /**< The state of cast operations in code. */
  ir_address_taken_computed_state globals_adr_taken_state;  /**< Address taken state of the globals. */

#ifdef DEBUG_libfirm
  long max_node_nr;                   /**< to generate unique numbers for nodes. */
#endif
};

/** Adds mode to the list of modes in irp. */
void  add_irp_mode(ir_mode *mode);

/* INLINE functions */

static INLINE ir_type *
_get_glob_type(void) {
  assert(irp);
  return irp->glob_type = skip_tid(irp->glob_type);
}

static INLINE ir_type *
_get_tls_type(void) {
  assert(irp);
  return irp->tls_type = skip_tid(irp->tls_type);
}

static INLINE int
_get_irp_n_irgs(void) {
  assert (irp && irp->graphs);
  if (get_visit_pseudo_irgs()) return get_irp_n_allirgs();
  return ARR_LEN(irp->graphs);
}

static INLINE ir_graph *
_get_irp_irg(int pos){
  if (get_visit_pseudo_irgs()) return get_irp_allirg(pos);
  assert(0 <= pos && pos <= _get_irp_n_irgs());
  return irp->graphs[pos];
}


static INLINE int
_get_irp_n_types (void) {
  assert (irp && irp->types);
  return ARR_LEN(irp->types);
}

static INLINE ir_type *
_get_irp_type(int pos) {
  assert (irp && irp->types);
  /* Don't set the skip_tid result so that no double entries are generated. */
  return skip_tid(irp->types[pos]);
}

static INLINE int
_get_irp_n_modes(void) {
  assert (irp && irp->modes);
  return ARR_LEN(irp->modes);
}

static INLINE ir_mode *
_get_irp_mode(int pos) {
  assert (irp && irp->modes);
  return irp->modes[pos];
}

static INLINE int
_get_irp_n_opcodes(void) {
  assert (irp && irp->opcodes);
  return ARR_LEN(irp->opcodes);
}

static INLINE ir_op *
_get_irp_opcode(int pos) {
  assert (irp && irp->opcodes);
  return irp->opcodes[pos];
}

#ifdef DEBUG_libfirm
/** Returns a new, unique number to number nodes or the like. */
static INLINE long
get_irp_new_node_nr(void) {
  assert(irp);
  return irp->max_node_nr++;
}
#endif /* DEBUG_libfirm */

static INLINE ir_graph *
_get_const_code_irg(void) {
  return irp->const_code_irg;
}

void           set_irp_ip_outedges(ir_node ** ip_outedges);
ir_node**      get_irp_ip_outedges(void);

/** initializes ir_prog. Constructs only the basic lists */
void init_irprog_1(void);

/** Completes ir_prog. */
void init_irprog_2(void);

#define get_irp_n_irgs()       _get_irp_n_irgs()
#define get_irp_irg(pos)       _get_irp_irg(pos)
#define get_irp_n_types()      _get_irp_n_types()
#define get_irp_type(pos)      _get_irp_type(pos)
#define get_irp_n_modes()      _get_irp_n_modes()
#define get_irp_mode(pos)      _get_irp_mode(pos)
#define get_irp_n_opcodes()    _get_irp_n_opcodes()
#define get_irp_opcode(pos)    _get_irp_opcode(pos)
#define get_const_code_irg()   _get_const_code_irg()
#define get_glob_type()        _get_glob_type()
#define get_tls_type()         _get_tls_type()

#endif /* ifndef _FIRM_IR_IRPROG_T_H_ */
