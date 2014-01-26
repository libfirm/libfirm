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
#include "../ia32/x86_cc.h"

typedef struct amd64_attr_t            amd64_attr_t;
typedef struct amd64_switch_jmp_attr_t amd64_switch_jmp_attr_t;
typedef struct amd64_movimm_attr_t     amd64_movimm_attr_t;
typedef struct amd64_cc_attr_t         amd64_cc_attr_t;

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

enum {
	NO_INPUT  = 0xFF,
	RIP_INPUT = 0xFE, /* can be used as base_input for PIC code */
};

typedef struct amd64_am_info_t {
	int64_t    offset;
	ir_entity *entity;
	uint8_t    base_input;
	uint8_t    index_input;
	unsigned   log_scale : 2; /* 0, 1, 2, 3  (giving scale 1, 2, 4, 8) */
	ENUMBF(amd64_segment_selector_t) segment : 4;
} amd64_am_info_t;

struct amd64_attr_t
{
	except_attr  exc;     /**< the exception attribute. MUST be the first one. */
	ir_mode     *ls_mode; /**< Stores the "input" mode */
	struct amd64_attr_data_bitfield {
		bool     has_am_info     : 1;
		bool     needs_frame_ent : 1;
		ENUMBF(amd64_insn_mode_t) insn_mode : 2;
	} data;
	amd64_am_info_t am;
};

struct amd64_movimm_attr_t
{
	amd64_attr_t base;
	int64_t      offset;
	ir_entity   *entity;
};

struct amd64_cc_attr_t
{
	amd64_attr_t         base;
	x86_condition_code_t cc;
};

struct amd64_switch_jmp_attr_t
{
	amd64_attr_t           base;
	const ir_switch_table *table;
	ir_entity             *table_entity;
};

#endif
