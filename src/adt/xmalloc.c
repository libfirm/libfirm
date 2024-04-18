/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       implementation of xmalloc & friends
 * @author      Markus Armbruster
 */
#include "xmalloc.h"

#include "funcattr.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static FIRM_NORETURN xnomem(void)
{
	/* Do not use panic() here, because it might try to allocate memory! */
	fputs("out of memory", stderr);
	abort();
}

void *xmalloc(size_t size)
{
	void *res = malloc(size);

	if (!res) xnomem();
	return res;
}

void *xrealloc(void *ptr, size_t size)
{
	/* ANSI blesses realloc (0, x) but SunOS chokes on it */
	void *res = ptr ? realloc (ptr, size) : malloc (size);

	if (!res) xnomem();
	return res;
}

char *xstrdup(const char *str)
{
	size_t len = strlen (str) + 1;
	return (char*) memcpy(xmalloc(len), str, len);
}
