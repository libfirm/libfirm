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
 */
#ifndef FIRM_IR_IRPROG_T_H
#define FIRM_IR_IRPROG_T_H

#include "irprog.h"
#include "irtypes.h"
#include "irtypeinfo.h"
#include "irmemory.h"

#include "callgraph.h"

#include "array.h"

/** Adds mode to the list of modes in irp. */
void add_irp_mode(ir_mode *mode);

/* inline functions */
static inline ir_type *get_segment_type_(ir_segment_t segment)
{
	assert(segment <= IR_SEGMENT_LAST);
	return irp->segment_types[segment];
}

static inline ir_type *get_glob_type_(void)
{
	return get_segment_type_(IR_SEGMENT_GLOBAL);
}

static inline ir_type *get_tls_type_(void)
{
	return get_segment_type_(IR_SEGMENT_THREAD_LOCAL);
}

static inline size_t get_irp_n_irgs_(void)
{
	assert(irp && irp->graphs);
	return ARR_LEN(irp->graphs);
}

static inline ir_graph *get_irp_irg_(size_t pos)
{
	assert(pos < ARR_LEN(irp->graphs));
	return irp->graphs[pos];
}

static inline size_t get_irp_n_types_(void)
{
	assert(irp && irp->types);
	return ARR_LEN(irp->types);
}

static inline ir_type *get_irp_type_(size_t pos)
{
	assert(irp->types);
	assert(pos < ARR_LEN(irp->types));
	/* Don't set the skip_tid result so that no double entries are generated. */
	return irp->types[pos];
}

static inline size_t get_irp_n_modes_(void)
{
	assert(irp->modes);
	return ARR_LEN(irp->modes);
}

static inline ir_mode *get_irp_mode_(size_t pos)
{
	assert(irp && irp->modes);
	return irp->modes[pos];
}

static inline size_t get_irp_n_opcodes_(void)
{
	assert(irp && irp->opcodes);
	return ARR_LEN(irp->opcodes);
}

static inline ir_op *get_irp_opcode_(size_t pos)
{
	assert(irp && irp->opcodes);
	return irp->opcodes[pos];
}

/** Returns a new, unique number to number nodes or the like. */
static inline long get_irp_new_node_nr(void)
{
	assert(irp);
	return irp->max_node_nr++;
}

static inline size_t get_irp_new_irg_idx(void)
{
	assert(irp);
	return irp->max_irg_idx++;
}

static inline ir_graph *get_const_code_irg_(void)
{
	return irp->const_code_irg;
}

/** Returns a new, unique label number. */
static inline ir_label_t get_irp_next_label_nr_(void)
{
	assert(irp);
	return ++irp->last_label_nr;
}

/** Whether optimizations should dump irgs */
static inline int get_irp_optimization_dumps_(void)
{
	assert(irp);
	return irp->optimization_dumps;
}

/** Set optimizations to dump irgs */
static inline void enable_irp_optimization_dumps_(void)
{
	assert(irp);
	irp->optimization_dumps = 1;
}

void      set_irp_ip_outedges(ir_node ** ip_outedges);
ir_node** get_irp_ip_outedges(void);

/** initializes ir_prog. Constructs only the basic lists */
void init_irprog_1(void);

/** Completes ir_prog. */
void init_irprog_2(void);

/** Adds type to the list of types in irp. */
void add_irp_type(ir_type *typ);

/** Removes type from the list of types, deallocates it and
    shrinks the list by one. */
void remove_irp_type(ir_type *typ);

/* Inline functions. */
#define get_irp_n_irgs()                 get_irp_n_irgs_()
#define get_irp_irg(pos)                 get_irp_irg_(pos)
#define get_irp_n_types()                get_irp_n_types_()
#define get_irp_type(pos)                get_irp_type_(pos)
#define get_irp_n_modes()                get_irp_n_modes_()
#define get_irp_mode(pos)                get_irp_mode_(pos)
#define get_irp_n_opcodes()              get_irp_n_opcodes_()
#define get_irp_opcode(pos)              get_irp_opcode_(pos)
#define get_const_code_irg()             get_const_code_irg_()
#define get_segment_type(s)              get_segment_type_(s)
#define get_glob_type()                  get_glob_type_()
#define get_tls_type()                   get_tls_type_()
#define get_irp_next_label_nr()          get_irp_next_label_nr_()
#define get_irp_optimization_dumps()     get_irp_optimization_dumps_()
#define enable_irp_optimization_dumps()  enable_irp_optimization_dumps_()

#endif
