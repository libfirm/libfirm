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
#include <assert.h>
#include <limits.h>

#include "beemitter_binary.h"
#include "obst.h"
#include "panic.h"

static code_fragment_t *first_fragment;
static code_fragment_t *last_fragment;
#ifndef NDEBUG
static const unsigned CODE_FRAGMENT_MAGIC = 0x4643414d;  /* "CFMA" */
#endif

struct obstack code_fragment_obst;

/** returns current fragment (the address stays only valid until the next
    be_emit(8/16/32/entity) call!) */
code_fragment_t *be_get_current_fragment(void)
{
	code_fragment_t *fragment = (code_fragment_t*)obstack_base(&code_fragment_obst);
	assert(obstack_object_size(&code_fragment_obst) >= sizeof(code_fragment_t));
	assert(fragment->magic == CODE_FRAGMENT_MAGIC);

	return fragment;
}

/** allocates a new fragment on the obstack (warning: address is only valid
    till next be_emit */
static void alloc_fragment(void)
{
	code_fragment_t *fragment;

	/* shouldn't have any growing fragments */
	assert(obstack_object_size(&code_fragment_obst) == 0);

	obstack_blank(&code_fragment_obst, sizeof(*fragment));
	fragment = (code_fragment_t*)obstack_base(&code_fragment_obst);
	memset(fragment, 0, sizeof(*fragment));
#ifndef NDEBUG
	fragment->magic = CODE_FRAGMENT_MAGIC;
#endif
	fragment->len        = 0;
	fragment->alignment  = 1;
	fragment->offset     = 0;
	fragment->max_offset = UINT_MAX;
}

static code_fragment_t *finish_fragment(void)
{
	code_fragment_t *fragment = be_get_current_fragment();
	fragment->len
		= obstack_object_size(&code_fragment_obst) - sizeof(*fragment);

	fragment      = (code_fragment_t*) obstack_finish(&code_fragment_obst);
	last_fragment = fragment;

	if (first_fragment == NULL)
		first_fragment = fragment;

	return fragment;
}

void be_start_code_emitter(void)
{
	obstack_init(&code_fragment_obst);
	first_fragment = NULL;
	alloc_fragment();
}

void be_start_new_fragment(void)
{
	finish_fragment();
	alloc_fragment();
}

static void emit(FILE *file, const unsigned char *buffer, size_t len)
{
	size_t i;
	for (i = 0; i < len; ++i) {
		size_t i2;
		fputs("\t.byte ", file);
		for (i2 = i; i2 < i + 30 && i2 < len; ++i2) {
			fprintf(file, "0x%02X", (unsigned)buffer[i2]);
		}
		i = i2;
		fputs("\n", file);
	}
}

static unsigned align(unsigned offset, unsigned alignment)
{
	if (offset % alignment != 0) {
		offset += alignment - (offset % alignment);
	}
	return offset;
}

static bool determine_jumpsize_iteration(
		const binary_emiter_interface_t *interface)
{
	unsigned         offset     = 0;
	unsigned         max_offset = 0;
	bool             changed    = false;
	code_fragment_t *fragment;

	for (fragment = first_fragment; fragment != NULL;
	     fragment = fragment->next) {
	    unsigned alignment = fragment->alignment;

	    /* assure alignment */
	    offset     = align(offset, alignment);
	    max_offset = align(max_offset, alignment);

		if (offset != fragment->offset) {
			changed          = true;
			fragment->offset = offset;
		}
	    fragment->max_offset = max_offset;

		/* advance offset */
		offset     += fragment->len;
		max_offset += fragment->len;
		interface->determine_jumpsize(fragment);
		offset     += fragment->jumpsize_min;
		max_offset += fragment->jumpsize_max;
	}

	return changed;
}

static void determine_offsets(const binary_emiter_interface_t *interface)
{
	bool changed;

	assert(first_fragment->alignment == 1);
	first_fragment->offset     = 0;
	first_fragment->max_offset = 0;

	/* The algorithm calculates a lower and upper bound for the offset of each
	 * fragment. With this information we can calculate a lower and upper bound
	 * for the size of each jump instruction.
	 * A single iteration updates the offset bounds for all fragments and jump
	 * sizes for each fragment. We iterate until we had an iteration where
	 * none of the minimum offsets changed. */
	do {
		changed = determine_jumpsize_iteration(interface);
		/* TODO: we should have an abort mode for the case when the offsets
		   don't converge fast enough. We could simply use a pessimistic
		   solution after a few iterations... */
	} while (changed);
}

void be_emit_entity(ir_entity *entity, bool entity_sign, int offset,
                    bool is_relative)
{
	(void) entity;
	(void) entity_sign;
	(void) offset;
	(void) is_relative;
	panic("not implemented yet");
}

void be_emit_code(FILE *output, const binary_emiter_interface_t *interface)
{
	unsigned offset;

	code_fragment_t *fragment;

	finish_fragment();

	/* determine near/far jumps */
	determine_offsets(interface);

	/* emit code */
	offset = 0;
	for (fragment = first_fragment; fragment != NULL;
	     fragment = fragment->next) {
	    unsigned char *jmpbuffer;
		unsigned nops;

	    /* assure alignment by emitting nops */
	    assert(fragment->offset >= offset);
	    nops = fragment->offset - offset;
	    if (nops > 0) {
			unsigned char *nopbuffer = (unsigned char*)obstack_alloc(&code_fragment_obst, nops);
			interface->create_nops(nopbuffer, nops);
			emit(output, nopbuffer, nops);
			offset = fragment->offset;
			obstack_free(&code_fragment_obst, nopbuffer);
		}

		/* emit the fragment */
		emit(output, fragment->data, fragment->len);
		offset += fragment->len;

		/* emit the jump */
		jmpbuffer = (unsigned char*)obstack_alloc(&code_fragment_obst, fragment->jumpsize_min);
		interface->emit_jump(fragment, jmpbuffer);
		emit(output, jmpbuffer, fragment->jumpsize_min);
		offset += fragment->jumpsize_min;
		obstack_free(&code_fragment_obst, jmpbuffer);
	}
}
