#include <stdio.h>
int test(double f) {
	return f;
}

int arraycopy(double *p, int *q) {
	double sum = 0.0;
	int i;

	for (i = 0; i < 10; ++i) {
		double v = p[i];
		q[i] = v;
		sum += v;
	}
	return sum;
}

static double data[10] = {
	-1.0, +1.0, 1/100.0, -1/100.0, 439023402304342343.0, 30303.0, 0.0
};

int main() {
	int res[10], i;

	arraycopy(data, res);
	for (i = 0; i < 10; ++i) {
		printf("%f -> %d\n", data[i], res[i]);
	}

	printf("%d\n", test(45.0));
	return 0;
}
