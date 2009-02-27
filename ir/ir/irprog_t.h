/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief    Entry point to the representation of a whole program 0-- private header.
 * @author   Goetz Lindenmaier
 * @date     2000
 * @version  $Id$
 */
#ifndef FIRM_IR_IRPROG_T_H
#define FIRM_IR_IRPROG_T_H

#include "irprog.h"
#include "irtypes.h"
#include "pseudo_irg.h"
#include "ircgcons.h"
#include "firm_common_t.h"
#include "irtypeinfo.h"
#include "irmemory.h"

#include "callgraph.h"
#include "field_temperature.h"
#include "execution_frequency.h"

#include "array.h"

/** Adds mode to the list of modes in irp. */
void  add_irp_mode(ir_mode *mode);

/* inline functions */
static inline ir_type *
_get_segment_type(ir_segment_t segment)
{
	ir_type *type;

	assert(segment < IR_SEGMENT_COUNT);
	type                        = skip_tid(irp->segment_types[segment]);
	irp->segment_types[segment] = type;
	return type;
}

static inline ir_type *
_get_glob_type(void) {
	return _get_segment_type(IR_SEGMENT_GLOBAL);
}

static inline ir_type *
_get_tls_type(void) {
	return _get_segment_type(IR_SEGMENT_THREAD_LOCAL);
}

static inline int
_get_irp_n_irgs(void) {
	assert (irp && irp->graphs);
	if (get_visit_pseudo_irgs()) return get_irp_n_allirgs();
	return ARR_LEN(irp->graphs);
}

static inline ir_graph *
_get_irp_irg(int pos){
	if (get_visit_pseudo_irgs()) return get_irp_allirg(pos);
	assert(0 <= pos && pos <= ARR_LEN(irp->graphs));
	return irp->graphs[pos];
}


static inline int
_get_irp_n_types(void) {
	assert (irp && irp->types);
	return ARR_LEN(irp->types);
}

static inline ir_type *
_get_irp_type(int pos) {
	assert (irp && irp->types);
	/* Don't set the skip_tid result so that no double entries are generated. */
	return skip_tid(irp->types[pos]);
}

static inline int
_get_irp_n_modes(void) {
	assert (irp && irp->modes);
	return ARR_LEN(irp->modes);
}

static inline ir_mode *
_get_irp_mode(int pos) {
	assert (irp && irp->modes);
	return irp->modes[pos];
}

static inline int
_get_irp_n_opcodes(void) {
	assert (irp && irp->opcodes);
	return ARR_LEN(irp->opcodes);
}

static inline ir_op *
_get_irp_opcode(int pos) {
	assert (irp && irp->opcodes);
	return irp->opcodes[pos];
}

/** Returns a new, unique number to number nodes or the like. */
static inline long
get_irp_new_node_nr(void) {
	assert(irp);
	return irp->max_node_nr++;
}

static inline int
get_irp_new_irg_idx(void) {
	assert(irp);
	return irp->max_irg_idx++;
}

static inline ir_graph *
_get_const_code_irg(void) {
	return irp->const_code_irg;
}

/** Returns a new, unique exception region number. */
static inline ir_exc_region_t
_get_irp_next_region_nr(void) {
	assert(irp);
	return ++irp->last_region_nr;
}

/** Returns a new, unique label number. */
static inline ir_label_t
_get_irp_next_label_nr(void) {
	assert(irp);
	return ++irp->last_label_nr;
}

void           set_irp_ip_outedges(ir_node ** ip_outedges);
ir_node**      get_irp_ip_outedges(void);

/** initializes ir_prog. Constructs only the basic lists */
void init_irprog_1(void);

/** Completes ir_prog. */
void init_irprog_2(void);

/* Inline functions. */
#define get_irp_n_irgs()          _get_irp_n_irgs()
#define get_irp_irg(pos)          _get_irp_irg(pos)
#define get_irp_n_types()         _get_irp_n_types()
#define get_irp_type(pos)         _get_irp_type(pos)
#define get_irp_n_modes()         _get_irp_n_modes()
#define get_irp_mode(pos)         _get_irp_mode(pos)
#define get_irp_n_opcodes()       _get_irp_n_opcodes()
#define get_irp_opcode(pos)       _get_irp_opcode(pos)
#define get_const_code_irg()      _get_const_code_irg()
#define get_segment_type(s)       _get_segment_type(s)
#define get_glob_type()           _get_glob_type()
#define get_tls_type()            _get_tls_type()
#define get_irp_next_region_nr()  _get_irp_next_region_nr()
#define get_irp_next_label_nr()   _get_irp_next_label_nr()

#endif
