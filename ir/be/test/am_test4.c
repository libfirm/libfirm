char c;

int f(int x, int y) {
	return x + y * 8;
}

int f2(int x, int y) {
	return x * 2 + y * 8;
}

int f3(int x) {
	return x * 2;
}

char f4(char *p, int k) {
	return p[k];
}

void dest_am(int *arr, int from, int to) {
	int i;

	for(i = from + 1; i < to; ++i) {
		arr[i] += arr[i-1];
	}
}

void dest_am2(int *arr, int from, int to) {
	int i;

	for(i = from + 1; i < to; ++i) {
		arr[i] = -arr[i];
	}
}

int main(void) {
	int arr[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
	int i;

	dest_am(arr, 0, 10);
	for(i = 0; i < 10; ++i) {
		printf("%d ", arr[i]);
	}
	printf("\n");
	dest_am2(arr, 0, 10);
	for(i = 0; i < 10; ++i) {
		printf("%d ", arr[i]);
	}
	printf("\n");

	return 0;
}
