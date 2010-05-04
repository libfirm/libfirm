#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "obstack.h"

#ifdef _WIN32
#define vsnprintf _vsnprintf
#endif

int obstack_printf(struct obstack *obst, const char *fmt, ...)
{
	char    buf[128];
	char   *buffer = buf;
	size_t  size   = lengthof(buf);
	va_list ap;
	int     len;

	for (;;) {
		va_start(ap, fmt);
		len = vsnprintf(buffer, sizeof(buffer), fmt, ap);
		va_end(ap);

		/* snprintf should return -1 only in the error case, but older glibcs
		 * and probably other systems are buggy in this respect and return -1 if
		 * the buffer was too small. We only abort for LARGE unrealistic buffer
		 * sizes here */
		if (len < 0) {
			if (buffer != buf)
				free(buffer);
			if (size > 65536)
				return -1;
			size *= 2;
		} else if ((size_t)len >= size) {
			/* If we come here more than once, vsnprintf() returned garbage */
			assert(buffer == buf);
			size = (size_t)len + 1;
		} else {
			break;
		}
		buffer = malloc(buffer, size);
	}

	obstack_grow(obst, buffer, len);
	if (buffer != buf)
		free(buffer);

	return len;
}
