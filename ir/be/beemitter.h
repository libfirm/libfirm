/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Interface for text output.
 * @author      Matthias Braun
 * @date        12.03.2007
 *
 * This is a framework for emitting line base text used by most backends to
 * emit assembly code.
 */
#ifndef FIRM_BE_BEEMITTER_H
#define FIRM_BE_BEEMITTER_H

#include <stdio.h>
#include "obst.h"

/* don't use the following vars directly, they're only here for the inlines */
extern struct obstack  emit_obst;

/**
 * Emit a character to the (assembler) output.
 */
static inline void be_emit_char(char c)
{
	obstack_1grow(&emit_obst, c);
}

/**
 * Emit a string to the (assembler) output.
 *
 * @param str  the string
 * @param l    the length of the given string
 */
static inline void be_emit_string_len(const char *str, size_t l)
{
	obstack_grow(&emit_obst, str, l);
}

/**
 * Emit a null-terminated string to the (assembler) output.
 *
 * @param str  the null-terminated string
 */
static inline void be_emit_string(const char *str)
{
	size_t len = strlen(str);
	be_emit_string_len(str, len);
}

/**
 * Emit a C string-constant to the (assembler) output.
 *
 * @param str  the null-terminated string constant
 */
#define be_emit_cstring(str) \
	be_emit_string_len(str, sizeof(str) - 1)

/**
 * Initializes an emitter environment.
 *
 * @param F    a file handle where the emitted file is written to.
 */
void be_emit_init(FILE *F);

/**
 * Destroys the given emitter environment.
 */
void be_emit_exit(void);

/**
 * Emit the output of an ir_printf.
 *
 * @param fmt  the ir_printf format
 */
void be_emit_irprintf(const char *fmt, ...);

/**
 * Emit the output of an ir_vprintf.
 *
 * @param fmt  the ir_printf format
 */
void be_emit_irvprintf(const char *fmt, va_list args);

/**
 * Flush the line in the current line buffer to the emitter file.
 */
void be_emit_write_line(void);

/** Return column in current line. Counting starts at 0. */
static inline size_t be_emit_get_column(void)
{
	return obstack_object_size(&emit_obst);
}

#endif
