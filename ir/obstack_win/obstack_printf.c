#include <stdarg.h>
#include <stdio.h>
#include "obstack.h"

#ifdef _WIN32
#define vsnprintf _vsnprintf
#endif

int obstack_vprintf(struct obstack *obst, const char *fmt, va_list ap)
{
	char buf[1024];
	int len;

	len = vsnprintf(buf, sizeof(buf), fmt, ap);
	obstack_grow(obst, buf, len);
	return len;
}

int obstack_printf(struct obstack *obst, const char *fmt, ...)
{
	va_list ap;
	int len;

	va_start(ap, fmt);
	len = obstack_vprintf(obst, fmt, ap);
	va_end(ap);
	return len;
}
