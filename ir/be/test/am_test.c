void am_test_func(int a, int b) {
	int ar[10];
	int i;

	for (i = 0; i < 10; i++) {
		ar[i] = i;
	}

	i = ar[1];

	ar[1] = a * b + i;
}
