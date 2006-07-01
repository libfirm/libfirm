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