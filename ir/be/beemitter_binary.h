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
 * @brief       Interface for binary machine code output.
 * @author      Matthias Braun
 * @date        12.03.2007
 */
#ifndef FIRM_BE_BEEMITTER_BINARY_H
#define FIRM_BE_BEEMITTER_BINARY_H

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include "firm_types.h"
#include "obst.h"

extern struct obstack code_fragment_obst;

typedef struct code_fragment_t code_fragment_t;
struct code_fragment_t {
#ifndef NDEBUG
	unsigned         magic;
#endif
	unsigned         len;          /**< size of the fragments data (the jump is
	                                    included in the data if its size is
	                                    unknown) */
	unsigned         alignment;    /**< alignment of the fragment in bytes */
	code_fragment_t *next;         /**< pointer to next fragment in line */
	unsigned         offset;       /**< offset of this fragment relative to the
	                                    "start" */
	unsigned         max_offset;   /**< maximum offset */
	int              jump_type;    /**< can be used by the backend to indicate
	                                    the type of jump at the end of this
	                                    fragment */
	void            *jump_data;    /**< can be used by the backend to encode
	                                    additional data for the jump (like its
	                                    destination) */
	code_fragment_t *destination;  /**< destination fragment of this jump */

	unsigned short   jumpsize_min;
	unsigned short   jumpsize_max;

	unsigned char    data[0];      /**< data starts here */
};

typedef struct binary_emiter_interface_t binary_emiter_interface_t;
struct binary_emiter_interface_t {
	/** create @p size of NOP instructions for alignment */
	void (*create_nops) (unsigned char *buffer, unsigned size);

	/**
	 * emits a jump by writing it to the buffer. The code for the jump must be
	 * exactly fragment->jumpsize_min bytes long.
	 * TODO: create a stream API which we can use here...
	 */
	void (*emit_jump) (code_fragment_t *fragment, unsigned char *buffer);

	/**
	 * determines the minimum and maximum size of bytes needed to emit
	 * the jump at the end of the fragment. The function must update the
	 * jumpsize_min and jumpsize_max fields in the fragment.
	 */
	void (*determine_jumpsize) (code_fragment_t *fragment);
};

/** Initializes and prepares for emitting a routine with code "fragments".
 * Creates an initial fragment in which you can emit right away.
 */
void be_start_code_emitter(void);

/** finalizes and emits current procedure */
void be_emit_code(FILE *output, const binary_emiter_interface_t *interface);

/** finish current fragment and return its final address */
code_fragment_t *be_finish_fragment(void);

/** Create a new code fragment (and append it to the previous one) */
void be_start_new_fragment(void);

/** returns current fragment (the address stays only valid until the next
    be_emit(8/16/32/entity) call!) */
code_fragment_t *be_get_current_fragment(void);

/** appends a byte to the current fragment */
static inline void be_emit8(const unsigned char byte)
{
	obstack_1grow(&code_fragment_obst, byte);
}

/** appends a word (16bits) to the current fragment */
static inline void be_emit16(const uint16_t u16)
{
	/* TODO: fix endianess if needed */
	obstack_grow(&code_fragment_obst, &u16, 2);
}

/** appends a dword (32bits) to the current fragment */
static inline void be_emit32(const uint32_t u32)
{
	/* TODO: fix endianess if needed */
	obstack_grow(&code_fragment_obst, &u32, 4);
}

/** leave space where an entity reference is put at the finish stage */
void be_emit_entity(ir_entity *entity, bool entity_sign, int offset,
                    bool is_relative);

#endif
