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

#define T(name, OP, OP2) \
void dest_am_##name(int *arr, int from, int to) {  \
	int i;                                  \
                                            \
	for(i = from; i < to; ++i) {            \
		arr[i] = OP arr[i] OP2;             \
	}                                       \
}

T(neg, -,)
T(not, ~,)
T(add, 3 +,)
T(sub, , - 42)
T(and, 0x12345 &,)
T(or, 0x12345 |,)
T(xor, 0x12345 ^,)
T(inc, 1 + ,)
T(dec, , - 1)
T(shl, , << 3)
T(shr, , >> 3)

int main(void) {
	int arr[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
	int i;

#define C(name)   dest_am_##name(arr, 0, 10); \
	for(i = 0; i < 10; ++i) {                 \
		printf("%d ", arr[i]);                \
	}                                         \
	printf("\n");

	C(neg);
	C(not);
	C(add);
	C(sub);
	C(and);
	C(or);
	C(inc);
	C(dec);
	C(xor);

	return 0;
}
