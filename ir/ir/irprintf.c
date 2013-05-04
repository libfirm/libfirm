/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   A little printf helper unterstanding firm types
 * @author  Sebastian Hack
 * @date    29.11.2004
 */
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include <ctype.h>

#include "ident.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "entity_t.h"
#include "type_t.h"
#include "tv_t.h"
#include "irprintf.h"
#include "obst.h"
#include "dbginfo_t.h"
#include "irargs_t.h"

void ir_printf(const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	lc_evprintf(firm_get_arg_env(), fmt, args);
	va_end(args);
}

void ir_fprintf(FILE *f, const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	lc_evfprintf(firm_get_arg_env(), f, fmt, args);
	va_end(args);
}

void ir_snprintf(char *buf, size_t n, const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	lc_evsnprintf(firm_get_arg_env(), buf, n, fmt, args);
	va_end(args);
}

void ir_vprintf(const char *fmt, va_list args)
{
	lc_evprintf(firm_get_arg_env(), fmt, args);
}

void ir_vfprintf(FILE *f, const char *fmt, va_list args)
{
	lc_evfprintf(firm_get_arg_env(), f, fmt, args);
}

void ir_vsnprintf(char *buf, size_t len, const char *fmt, va_list args)
{
	lc_evsnprintf(firm_get_arg_env(), buf, len, fmt, args);
}

void ir_obst_vprintf(struct obstack *obst, const char *fmt, va_list args)
{
	lc_evoprintf(firm_get_arg_env(), obst, fmt, args);
}
