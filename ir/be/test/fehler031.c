#include <stdlib.h>
#include <stdio.h>

static unsigned hash_ptr(const void *ptr)
{
	unsigned ptr_int = ((const char*) ptr - (const char*) NULL);
	return ptr_int >> 3;
}

void *p = (void*) 0xdeadbeef;

int main()
{
	printf("0x%x (shoulde be 0x1bd5b7dd)\n", hash_ptr(p));

	return 0;
}
