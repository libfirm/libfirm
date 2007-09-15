#include <stdio.h>

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

#define TTYPE(name, type, OP, OP2) \
void dest_am_##name##type(type *arr, int from, int to) {  \
	int i;                                  \
                                            \
	for(i = from; i < to; ++i) {            \
		arr[i] = OP arr[i] OP2;             \
	}                                       \
}

#define T(name, OP, OP2)     \
	TTYPE(name,int,OP,OP2)   \
	TTYPE(name,short,OP,OP2) \
	TTYPE(name,char,OP,OP2)

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
	int   arrint[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
	short arrshort[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
	char  arrchar[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

	int i;

#define CTYPE(type,name)   dest_am_##name##type(arr##type, 0, 10); \
	for(i = 0; i < 10; ++i) {                 \
		printf("%d ", arr##type[i]);          \
	}                                         \
	printf("\n");

#define C(name) \
	CTYPE(int,name) \
	CTYPE(short,name) \
	CTYPE(char,name)

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
