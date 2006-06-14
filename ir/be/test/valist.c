#ifdef __GNUC__
#define va_start(v,l)	__builtin_va_start(v,l)
#define va_end(v)	__builtin_va_end(v)
#define va_arg(v,l)	__builtin_va_arg(v,l)
#define va_copy(d,s)	__builtin_va_copy(d,s)
#else
#include <stdarg.h>
#endif

char * foo(char *fmt, ...) {
     va_list ap;
     char *s;

     va_start(ap, fmt);
     s = va_arg(ap, char *);
     va_end(ap);
     return s;
}

int main()
{
  printf("<%s>\n", foo("bla", "blup"));
}
