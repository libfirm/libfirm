#include <stdlib.h>
#include <math.h>

int i = 0;
int *p1 = &i;

int main()
{
	*p1 = 1066;
	if(*p1 != 1066)
		abort();

	return 0;
}
