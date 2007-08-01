/*$ -fno-inline $*/
/* 64 bit problems in beabi (should be worked around by now) */
#include <assert.h>

extern int func1(int version, const char *path, unsigned long long *ptr)
{
	(void) version;
	(void) path;
	(void) ptr;
	return 42;
}

extern inline int func2(const char *path, unsigned long long dev)
{
	return func1(1, path, &dev);
}

int main()
{
	int res = func2("bla", 1);
	assert(res == 42);
	return 0;
}
