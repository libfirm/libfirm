#include <stdio.h>

double a;

double test(double d) {
	float x = d;
	double d1 = x;
	float x1 = d1;
	return x1;
}

int main() {
	printf("%f\n", test(a));
	return 0;
}
