/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Interface for assembler output.
 * @author      Matthias Braun
 * @date        12.03.2007
 */
#include "beemitter.h"

#include "irprintf.h"
#include "panic.h"

static FILE    *emit_file;
struct obstack  emit_obst;

void be_emit_init(FILE *file)
{
	emit_file = file;
	obstack_init(&emit_obst);
}

void be_emit_exit(void)
{
	obstack_free(&emit_obst, NULL);
}

void be_emit_irvprintf(const char *fmt, va_list args)
{
	ir_obst_vprintf(&emit_obst, fmt, args);
}

void be_emit_irprintf(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	be_emit_irvprintf(fmt, ap);
	va_end(ap);
}

void be_emit_write_line(void)
{
	size_t const len  = obstack_object_size(&emit_obst);
	char  *const line = (char*)obstack_finish(&emit_obst);
	fwrite(line, 1, len, emit_file);
	obstack_free(&emit_obst, line);
}
