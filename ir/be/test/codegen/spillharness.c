int max = 1000000;

int printf(const char *str, ...);

int main(int argc, char **argv) {
	int i;
	int i2;
	int val;

	for(i = 0; i < max; ++i) {
		val = rand();
	}

	rand();
	for(i2 = 0; i2 < 100; ++i2) {
	for(i = 0; i < max; ++i) {
		int i1 = rand();
		int i2 = rand();
		int i3 = rand();
		int i4 = rand();
		int i5 = rand();
		int i6 = rand();
		int i7 = rand();

		i += i1 +i2 +i3+i4+i5+i6+i7;
		printf("%d\n", i);
	}
	}

	printf("end\n");
	return val;
}
