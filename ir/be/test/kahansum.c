#include <stdio.h>

float kahanSum(const float A[], int n) {
	float sum = 0.0, C = 0.0, Y, T;
	int i;

	for (i = 0; i < n; ++i) {
		Y = A[i] - C;
		T = sum + Y;
		C = T - sum - Y;
		sum = T;
	}
	return sum;
}

int main()
{
    float test[] = { 1,2,3,4,5,6,7,8,9};
    printf("Result: %f\n", kahanSum(test, sizeof(test)/sizeof(float)));

    return 0;
}
