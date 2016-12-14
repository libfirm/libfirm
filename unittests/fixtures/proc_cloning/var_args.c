#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>

int f(bool b, size_t count, ...);
int f(bool b, size_t count, ...)
{
	// Make bool important
	if (!b) return 0;

	va_list ap;
	va_start(ap, count);

	int s = 0;
	for (size_t i = 0; i < count; i++) {
		s += va_arg(ap, int);
	}
	return s;
}

int main(void)
{
	return f(true, 2, 0, 1) + f(true, 3, 0, 1, 2) + f(true, 4, 0, 1, 2, 3);
}
