/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Interface for machine code output
 * @author      Matthias Braun
 * @date        12.03.2007
 */
#include "bejit.h"

#include "array.h"
#include "beemitter.h"
#include "begnuas.h"
#include "bitfiddle.h"
#include "compiler.h"
#include "entity_t.h"
#include "obst.h"
#include "panic.h"
#include <assert.h>
#include <limits.h>

typedef enum reloc_dest_kind_t {
	RELOC_DEST_CODE_FRAGMENT,
	RELOC_DEST_ENTITY,
} reloc_dest_kind_t;

typedef struct relocation_t {
	uint8_t                   be_kind;
	ENUMBF(reloc_dest_kind_t) dest_kind : 8;
	uint16_t                  offset;
	int32_t                   dest_offset;
	union dest {
		uint16_t   fragment_num;
		ir_entity *entity;
	} dest;
} relocation_t;

typedef struct fragment_info_t {
	unsigned     address;  /**< Address from begin of code segment */
	unsigned     len;      /**< size of the fragments data */
	uint8_t      p2align;  /**< power 2 of two we should align */
	uint8_t      max_skip; /**< Maximum number of bytes to skip for alignment */
	uint16_t     n_relocations;
	relocation_t relocations[];
} fragment_info_t;

struct ir_jit_segment_t {
	struct obstack code_obst;
	struct obstack fragment_info_obst;
	struct obstack fragment_info_arr_obst;
};

struct ir_jit_function_t {
	unsigned          size;
	unsigned          n_fragments;
	char const       *code;
	fragment_info_t **fragment_infos;
};

struct obstack        *code_obst;
static struct obstack *fragment_info_obst;
static struct obstack *fragment_info_arr_obst;

ir_jit_segment_t *be_new_jit_segment(void)
{
	ir_jit_segment_t *const segment = XMALLOCZ(ir_jit_segment_t);
	obstack_init(&segment->code_obst);
	obstack_init(&segment->fragment_info_obst);
	obstack_init(&segment->fragment_info_arr_obst);
	return segment;
}

void be_destroy_jit_segment(ir_jit_segment_t *segment)
{
	obstack_free(&segment->code_obst, NULL);
	obstack_free(&segment->fragment_info_obst, NULL);
	obstack_free(&segment->fragment_info_arr_obst, NULL);
	free(segment);
}

void be_jit_set_entity_addr(ir_entity *entity, void const *address)
{
	assert(is_global_entity(entity));
	entity->attr.global.jit_addr = address;
}

void const *be_jit_get_entity_addr(ir_entity const *const entity)
{
	assert(is_global_entity(entity));
	return entity->attr.global.jit_addr;
}

void be_jit_begin_function(ir_jit_segment_t *const segment)
{
	assert(obstack_object_size(&segment->code_obst) == 0);
	assert(obstack_object_size(&segment->fragment_info_obst) == 0);
	assert(obstack_object_size(&segment->fragment_info_arr_obst) == 0);
	code_obst              = &segment->code_obst;
	fragment_info_obst     = &segment->fragment_info_obst;
	fragment_info_arr_obst = &segment->fragment_info_arr_obst;
}

static void layout_fragments(ir_jit_function_t *const function,
                             unsigned const code_size)
{
	unsigned          const n_fragments    = function->n_fragments;
	fragment_info_t **const fragment_infos = function->fragment_infos;

	unsigned address      = 0;
#ifndef NDEBUG
	unsigned orig_address = 0;
#endif
	for (unsigned i = 0; i < n_fragments; ++i) {
		fragment_info_t *const fragment = fragment_infos[i];
		assert(fragment->address == ~0u);
		assert(fragment->len != ~0u);

		unsigned const align   = 1 << fragment->p2align;
		unsigned const aligned = round_up2(address, align);
		if (aligned - address <= fragment->max_skip)
			address = aligned;

		fragment->address = address;

		address      += fragment->len;
#ifndef NDEBUG
		orig_address += fragment->len;
#endif
	}
	function->size = address;
	assert(code_size == orig_address);
	(void)code_size;
}

ir_jit_function_t *be_jit_finish_function(void)
{
	struct obstack *obst = fragment_info_arr_obst;

	size_t   const size        = obstack_object_size(obst);
	unsigned const n_fragments = size / sizeof(fragment_info_t*);
	assert(size % sizeof(fragment_info_t*) == 0);
	fragment_info_t **const fragment_infos = obstack_finish(obst);

	unsigned const code_size = obstack_object_size(code_obst);

	ir_jit_function_t *const res = OALLOCZ(obst, ir_jit_function_t);
	res->n_fragments    = n_fragments;
	res->fragment_infos = fragment_infos;
	res->code           = obstack_finish(code_obst);

	layout_fragments(res, code_size);

#ifndef NDEBUG
	code_obst              = NULL;
	fragment_info_obst     = NULL;
	fragment_info_arr_obst = NULL;
#endif

	return res;
}

unsigned be_get_function_size(ir_jit_function_t const *const function)
{
	return function->size;
}

unsigned be_begin_fragment(uint8_t const p2align, uint8_t const max_skip)
{
	assert(obstack_object_size(fragment_info_obst) == 0);

	fragment_info_t const fragment = {
		.address  = obstack_object_size(code_obst),
		.len      = ~0u,
		.p2align  = p2align,
		.max_skip = max_skip,
	};
	obstack_grow(fragment_info_obst, &fragment, sizeof(fragment));

	return obstack_object_size(fragment_info_arr_obst)/sizeof(fragment_info_t*);
}

void be_finish_fragment(void)
{
	size_t size = obstack_object_size(fragment_info_obst);
	assert(size >= sizeof(fragment_info_t));

	fragment_info_t *const fragment = obstack_finish(fragment_info_obst);
	obstack_ptr_grow(fragment_info_arr_obst, fragment);

	unsigned const begin = fragment->address;
	unsigned const end   = obstack_object_size(code_obst);
	fragment->len        = end-begin;
	fragment->n_relocations
		= (size - sizeof(fragment_info_t)) / sizeof(relocation_t);

#ifndef NDEBUG
	assert(sizeof(fragment_info_t)
	       + fragment->n_relocations * sizeof(relocation_t) == size);
	fragment->address = ~0u;
#endif
}

static void be_emit_relocation(unsigned const len, relocation_t *const relocation)
{
	fragment_info_t *const fragment = obstack_base(fragment_info_obst);
	unsigned         const begin    = fragment->address;
	unsigned         const now      = obstack_object_size(code_obst);
	relocation->offset = now - begin;

	assert(obstack_object_size(fragment_info_obst) >= sizeof(fragment_info_t));
	obstack_grow(fragment_info_obst, relocation, sizeof(*relocation));

	obstack_blank(code_obst, len);
}

void be_emit_reloc_fragment(unsigned const len, uint8_t const be_kind,
                            unsigned const fragment_num, int32_t const offset)
{
	relocation_t relocation = {
		.be_kind           = be_kind,
		.dest_kind         = RELOC_DEST_CODE_FRAGMENT,
		.dest_offset       = offset,
		.dest.fragment_num = fragment_num,
	};
	be_emit_relocation(len, &relocation);
}

void be_emit_reloc_entity(unsigned const len, uint8_t be_kind,
                          ir_entity *const entity, int32_t const offset)
{
	relocation_t relocation = {
		.be_kind     = be_kind,
		.dest_kind   = RELOC_DEST_ENTITY,
		.dest_offset = offset,
		.dest.entity = entity,
	};
	be_emit_relocation(len, &relocation);
}

static int32_t resolve_relocation_code(ir_jit_function_t const *const function,
                                       relocation_t const *const relocation,
                                       unsigned const relocation_address)
{
	unsigned const fragment_num = relocation->dest.fragment_num;
	assert(fragment_num < function->n_fragments);
	fragment_info_t const *const fragment
		= function->fragment_infos[fragment_num];
	unsigned const dest_address = fragment->address + relocation->dest_offset;
	return (int32_t)dest_address - relocation_address;
}

static unsigned emit_relocation(ir_jit_function_t const *const function,
                                relocation_t const *const relocation,
                                unsigned const relocation_address,
                                char *const relocation_abs,
                                emit_relocation_func const emit)
{
	switch (relocation->dest_kind) {
	case RELOC_DEST_CODE_FRAGMENT: {
		int32_t const dest = resolve_relocation_code(function, relocation,
		                                             relocation_address);
		return emit(relocation_abs, relocation->be_kind, NULL, dest);
	}
	case RELOC_DEST_ENTITY:
		return emit(relocation_abs, relocation->be_kind,
		            relocation->dest.entity, relocation->dest_offset);
	}
	panic("Invalid relocation");
}

static void emit_bytes_as_asm(char const *const begin, char const *const end)
{
	assert(begin <= end);
	for (char const *b = begin; b < end; ++b) {
		be_emit_irprintf("\t.byte 0x%02X\n", (uint8_t)*b);
		be_emit_write_line();
	}
}

static void emit_fragment_as_asm(ir_jit_function_t const *const function,
                                 fragment_info_t const *const fragment,
                                 char const *const fragment_code,
                                 emit_relocation_func const emit)
{
	unsigned        const fragment_address = fragment->address;
	char     const *      b                = fragment_code;
	for (unsigned r = 0, n = fragment->n_relocations; r < n; ++r) {
		relocation_t const *const relocation = &fragment->relocations[r];
		unsigned            const offset     = relocation->offset;
		emit_bytes_as_asm(b, fragment_code + offset);
		unsigned const reloc_address = fragment_address + offset;
		unsigned const reloc_size
			= emit_relocation(function, relocation, reloc_address, NULL, emit);
		b = fragment_code + relocation->offset + reloc_size;
	}
	char const *const end = fragment_code + fragment->len;
	emit_bytes_as_asm(b, end);
}

void be_jit_emit_as_asm(ir_jit_function_t *const function,
                        emit_relocation_func const emit)
{
	/* Move fragments to their final addresses */
	char const *const code         = function->code;
	unsigned          orig_address = 0;
	unsigned          last_address = 0;
	for (size_t i = 0, n = function->n_fragments; i < n; ++i) {
		fragment_info_t const *const fragment = function->fragment_infos[i];
		unsigned               const address  = fragment->address;
		if (address > last_address)
			be_emit_irprintf("\t.p2align %u,,%u\n", fragment->p2align,
			                 fragment->max_skip);

		emit_fragment_as_asm(function, fragment, code + orig_address, emit);

		orig_address += fragment->len;
		last_address = address + fragment->len;
	}
}

static void emit_fragment(ir_jit_function_t const *const function,
						  fragment_info_t const *const fragment,
                          char const *const fragment_code, char *const buffer,
                          emit_relocation_func const emit)
{
	unsigned        const fragment_address = fragment->address;
	char     const *      b                = fragment_code;
	char           *      d                = buffer;
	unsigned              last_offset      = 0;
	for (unsigned r = 0, n = fragment->n_relocations; r < n; ++r) {
		relocation_t const *const relocation = &fragment->relocations[r];
		unsigned            const offset     = relocation->offset;
		unsigned            const len        = offset - last_offset;
		assert(last_offset <= offset);
		memcpy(d, b, len);
		d += len;
		b += len;
		unsigned const reloc_address = fragment_address + offset;
		unsigned const reloc_size
			= emit_relocation(function, relocation, reloc_address, d, emit);
		d += reloc_size;
		b += reloc_size;
		last_offset = offset + reloc_size;
	}
	char const *const end = fragment_code + fragment->len;
	assert(b <= end);
	memcpy(d, b, end-b);
}

void be_jit_emit_memory(char *const buffer, ir_jit_function_t *const function,
                        be_jit_emit_interface_t const *const emitter)
{
	/* Copy fragments and resolve relocations. */
	char const *const code         = function->code;
	unsigned          orig_address = 0;
	unsigned          last_address = 0;
	for (size_t i = 0, n = function->n_fragments; i < n; ++i) {
		fragment_info_t const *const fragment  = function->fragment_infos[i];
		unsigned               const address   = fragment->address;
		unsigned               const nop_bytes = last_address - address;
		assert(address >= last_address);
		if (nop_bytes > 0)
			emitter->nops(buffer + last_address, nop_bytes);

		emit_fragment(function, fragment, code+orig_address, buffer+address,
		              emitter->relocation);

		orig_address += fragment->len;
		last_address = address + fragment->len;
	}
}
