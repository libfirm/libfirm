/*$ -fno-inline -fno-cond-eval $*/

int x;

int destroy_flags(void) {
	rand();
	return 0;
}

int f(void) {
	int a = (x < 5);
	int t = 1;
	destroy_flags();
	if(a) {
		t = 42;
	}
	return t;
}

int main(void) {
	x = 2;
	printf("Res: %d (expected 42)\n", f());
	x = 10;
	printf("Res: %d (expected 1)\n", f());

	return 0;
}
