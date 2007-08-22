#include <stdio.h>

int arr[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

int sum(int from, int to) {
	int i, res = 0;
	int len = to - from;

	for(i = 0; i < len; ++i) {
		res += arr[from + i];
	}

	return res;
}

int main(int argc, char **argv) {
	int from = 0;
	int to = 10;
	if(argc > 1)
		to = atoi(argv[1]);

	printf("Sum: %d\n", sum(from, to));
	return 0;
}
