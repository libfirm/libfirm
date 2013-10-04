/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   attributes attached to all amd64 nodes
 */
#ifndef FIRM_BE_AMD64_AMD64_NODES_ATTR_H
#define FIRM_BE_AMD64_AMD64_NODES_ATTR_H

#include <stdint.h>

#include "bearch.h"
#include "compiler.h"

typedef struct amd64_attr_t            amd64_attr_t;
typedef struct amd64_SymConst_attr_t   amd64_SymConst_attr_t;
typedef struct amd64_switch_jmp_attr_t amd64_switch_jmp_attr_t;

typedef enum {
	INSN_MODE_64,
	INSN_MODE_32,
	INSN_MODE_16,
	INSN_MODE_8
} amd64_insn_mode_t;

typedef enum {
	AMD64_SEGMENT_DEFAULT,
	AMD64_SEGMENT_CS,
	AMD64_SEGMENT_SS,
	AMD64_SEGMENT_DS,
	AMD64_SEGMENT_ES,
	AMD64_SEGMENT_FS,
	AMD64_SEGMENT_GS,
} amd64_segment_selector_t;

typedef struct amd64_imm_t {
	int64_t    offset;
	ir_entity *symconst;
} amd64_imm_t;

enum {
	NO_INPUT  = 0xFF,
	RIP_INPUT = 0xFE, /* can be used as base_input for PIC code */
};

typedef struct amd64_am_info_t {
	int64_t    offset;
	ir_entity *symconst;
	uint8_t    base_input;
	uint8_t    index_input;
	uint8_t    mem_input;
	unsigned   log_scale : 2; /* 0, 1, 2, 3  (giving scale 1, 2, 4, 8) */
	ENUMBF(amd64_segment_selector_t) segment : 4;
} amd64_am_info_t;

struct amd64_attr_t
{
	except_attr  exc;     /**< the exception attribute. MUST be the first one. */
	ir_mode     *ls_mode; /**< Stores the "input" mode */
	struct amd64_attr_data_bitfield {
		unsigned ins_permuted    : 1;  /**< inputs of node have been permuted
		                                    (for commutative nodes) */
		unsigned cmp_unsigned    : 1;  /**< compare should be unsigned */
		bool     needs_frame_ent : 1;
		ENUMBF(amd64_insn_mode_t) insn_mode : 2;
	} data;
	struct amd64_attr_extended {
		ir_relation relation;           /**< type of compare operation >*/
	} ext;
	amd64_imm_t     imm;
	amd64_am_info_t am;
};

struct amd64_SymConst_attr_t
{
	amd64_attr_t  base;
	ir_entity    *entity;
	unsigned      fp_offset;
};

struct amd64_switch_jmp_attr_t
{
	amd64_attr_t           base;
	const ir_switch_table *table;
	ir_entity             *table_entity;
};

#define CAST_AMD64_ATTR(type,ptr)        ((type *)(ptr))
#define CONST_CAST_AMD64_ATTR(type,ptr)  ((const type *)(ptr))

#endif
