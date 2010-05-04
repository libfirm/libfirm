#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "obstack.h"

#ifdef _WIN32
#define vsnprintf _vsnprintf
#endif

int obstack_printf(struct obstack *obst, const char *fmt, ...)
{
	char  buf[128];
	char *buffer = buf;
	va_list ap;
	int len;

	va_start(ap, fmt);
	len = vsnprintf(buffer, sizeof(buffer), fmt, ap);
	va_end(ap);
	if (len < 0 || len >= (int) sizeof(buffer)) {
		size_t size   = len >= 0 ? (size_t) len : sizeof(buffer) * 2;
		char  *buffer = malloc(size);
		do {
			if (buffer == NULL)
				return -1;

			va_start(ap, fmt);
			len = vsnprintf(buffer, size, fmt, ap);
			va_end(ap);

			/* snprintf should return -1 only in the error case, but older
			 * glibcs and probably other systems are buggy in this respect and
			 * return -1 if the buffer was too small. We only abort for LARGE
			 * unrealistic buffer sizes here */
			if (len < 0) {
				if (size > 65536)
					return -1;
				size *= 2;
				buffer = realloc(buffer, size);
			} else if (len >= (int) size) {
				/* this should not happen if snprintf works correctly */
				abort();
			}
		} while (len < 0);
		free(buffer);
	}
	obstack_grow(obst, buffer, len);

	return len;
}
