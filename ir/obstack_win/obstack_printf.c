#include <stdarg.h>
#include <stdio.h>
#include "obstack.h"

#ifdef _WIN32
#define vsnprintf _vsnprintf
#endif

int obstack_printf(struct obstack *obst, const char *fmt, ...)
{
  char buf[1024];
  va_list ap;
  int len;

  va_start(ap, fmt);
  len = vsnprintf(buf, sizeof(buf), fmt, ap);
  obstack_grow(obst, buf, len);
  va_end(ap);

  return len;
}
