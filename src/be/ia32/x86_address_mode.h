/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       This file contains functions for matching firm graphs for
 *              nodes that can be used as address mode for x86 instructions
 * @author      Matthias Braun
 */
#ifndef X86_ADDRESS_MODE_H
#define X86_ADDRESS_MODE_H

#include <stdbool.h>
#include <stdint.h>
#include "x86_node.h"

typedef enum x86_addr_variant_t {
	X86_ADDR_INVALID,
	X86_ADDR_REG,
	X86_ADDR_JUST_IMM,
	X86_ADDR_BASE,
	X86_ADDR_BASE_INDEX,
	X86_ADDR_INDEX,
	X86_ADDR_RIP,
} x86_addr_variant_t;

char const *x86_get_addr_variant_str(x86_addr_variant_t);

static inline bool x86_addr_variant_has_base(x86_addr_variant_t const variant)
{
	return variant == X86_ADDR_BASE || variant == X86_ADDR_BASE_INDEX;
}

static inline bool x86_addr_variant_has_index(x86_addr_variant_t const variant)
{
	return variant == X86_ADDR_INDEX || variant == X86_ADDR_BASE_INDEX;
}

/**
 * The address mode data: Used to match (memory) address mode patterns.
 */
typedef struct x86_address_t {
	ir_node    *base;            /**< value for base register (if any) */
	ir_node    *index;           /**< value for index register (if any). */
	ir_node    *mem;             /**< value for memory input (if any). */
	x86_imm32_t imm;
	unsigned   scale       : 8; /**< An integer scale. {0,1,2,3} */
	ENUMBF(x86_addr_variant_t) variant : 8; /**< Address mode variant */
	bool       tls_segment : 1; /**< Set if AM is relative to TLS */
	bool       ip_base     : 1; /**< Base is instruction pointer (IP) */
} x86_address_t;

/**
 * Additional flags for the address mode creation.
 */
typedef enum x86_create_am_flags_t {
	x86_create_am_normal     = 0,
	/** Ignore non-address-mode markings on the root node. */
	x86_create_am_force      = 1U << 0,
	/** Fold AM, even if the root node has two users. */
	x86_create_am_double_use = 1U << 1,
} x86_create_am_flags_t;

typedef enum x86_segment_selector_t {
	X86_SEGMENT_DEFAULT,
	X86_SEGMENT_CS,
	X86_SEGMENT_SS,
	X86_SEGMENT_DS,
	X86_SEGMENT_ES,
	X86_SEGMENT_FS,
	X86_SEGMENT_GS,
} x86_segment_selector_t;

/**
 * Address mode data. Used as node attribute for nodes supporting address mode.
 */
typedef struct x86_addr_t {
	x86_imm32_t immediate;
	uint8_t     base_input;
	uint8_t     index_input;
	uint8_t     mem_input;
	unsigned    log_scale : 2; /* 0, 1, 2, 3  (giving scale 1, 2, 4, 8) */
	ENUMBF(x86_segment_selector_t) segment : 3;
	ENUMBF(x86_addr_variant_t)     variant : 3;
} x86_addr_t;

static inline bool x86_addrs_equal(const x86_addr_t *const addr0,
                                   const x86_addr_t *const addr1)
{
	return x86_imm32_equal(&addr0->immediate, &addr1->immediate)
	    && addr0->variant == addr1->variant
	    && (!x86_addr_variant_has_base(addr0->variant)
	        || addr0->base_input == addr1->base_input)
	    && (!x86_addr_variant_has_index(addr0->variant)
	        || (addr0->index_input == addr1->index_input
	            && addr0->log_scale == addr1->log_scale))
	    && addr0->segment == addr1->segment;
}

/**
 * Create an address mode for a given node.
 */
void x86_create_address_mode(x86_address_t *addr, ir_node *node,
                             x86_create_am_flags_t);

/**
 * Mark those nodes of the given graph that cannot be used inside an
 * address mode because there values must be materialized in registers.
 */
void x86_calculate_non_address_mode_nodes(ir_graph *irg);

/**
 * Free the non_address_mode information.
 */
void x86_free_non_address_mode_nodes(void);

/**
 * Tells whether the given node is a non address mode node.
 */
bool x86_is_non_address_mode_node(ir_node const *node);

/**
 * mark a node so it will not be used as part of address modes
 */
void x86_mark_non_am(ir_node *node);

/**
 * Emit address in gnu assembler syntax.
 */
void x86_emit_addr(ir_node const *node, x86_addr_t const *addr);

#endif
