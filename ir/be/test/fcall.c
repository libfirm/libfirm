#include <math.h>

float a;

int main()
{
	/* tests for problems in x87 simulator when results of a call are not
	 * used */
	float b = a;
	sqrt(a);
	printf("%f\n", b);
  	return 0;
}
