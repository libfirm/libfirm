#include <assert.h>

char* ptr(void)
{
	return (char*) 123;
}

int main()
{
	printf("Int: %d\n", (int) ptr());
	return 0;
}
