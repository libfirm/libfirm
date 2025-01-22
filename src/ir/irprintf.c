/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   A little printf helper understanding firm types
 * @author  Sebastian Hack
 * @date    29.11.2004
 */
#include "irprintf.h"

#include "dbginfo_t.h"
#include "entity_t.h"
#include "ident.h"
#include "irargs_t.h"
#include "irmode_t.h"
#include "irnode_t.h"
#include "obst.h"
#include "tv_t.h"
#include "type_t.h"
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

int ir_printf(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	int res = lc_evprintf(firm_get_arg_env(), fmt, args);
	va_end(args);
	return res;
}

int ir_fprintf(FILE *f, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	int res = lc_evfprintf(firm_get_arg_env(), f, fmt, args);
	va_end(args);
	return res;
}

int ir_snprintf(char *buf, size_t n, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	int res = lc_evsnprintf(firm_get_arg_env(), buf, n, fmt, args);
	va_end(args);
	return res;
}

int ir_vprintf(const char *fmt, va_list args)
{
	return lc_evprintf(firm_get_arg_env(), fmt, args);
}

int ir_vfprintf(FILE *f, const char *fmt, va_list args)
{
	return lc_evfprintf(firm_get_arg_env(), f, fmt, args);
}

int ir_vsnprintf(char *buf, size_t len, const char *fmt, va_list args)
{
	return lc_evsnprintf(firm_get_arg_env(), buf, len, fmt, args);
}

int ir_obst_vprintf(struct obstack *obst, const char *fmt, va_list args)
{
	return lc_evoprintf(firm_get_arg_env(), obst, fmt, args);
}
