#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>

int f(bool b, size_t count, ...);
int f(bool b, size_t count, ...)
{
	// Make b and count important
	if (!b || !count) return 0;

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
	return f(true, 0) + f(true, 1, 0) + f(true, 1, 0) + f(true, 2, 0, 1);
}
