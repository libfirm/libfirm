int max = 1000000;

void mark(void);
int foo(void);

int main(int argc, char **argv) {
	int i;
	int val;

	for(i = 0; i < max; ++i) {
		val = rand();
	}

	mark();
	for(i = 0; i < max; ++i) {
		int i1 = foo();
		int i2 = foo();
		int i3 = foo();
		int i4 = foo();
		int i5 = foo();
		int i6 = foo();
		int i7 = foo();

		i += i1 +i2 +i3+i4+i5+i6+i7;
		printf("%d\n", i);
	}

	printf("end\n");
	return val;
}
