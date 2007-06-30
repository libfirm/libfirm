#include <stdio.h>
#include <math.h>

int main(int argc, char ** argv) {
	float a = 0;

	while(argc--) {
		a += 1;
	}

	printf("%f\n", a);

	return 0;
}
