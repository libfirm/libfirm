/* fmt and all further parameters must be passed on the stack even for regparams */
#include <stdarg.h>

static void f(const char* fmt, ...)
{
  va_list va;
  va_start(va, fmt);
  vprintf(fmt, va);
  va_end(va);
}


int main(void)
{
  f("Hallo, %s!\n", "Welt");
  return 0;
}
