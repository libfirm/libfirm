#include <stdio.h>

int bla(char a, char b, char c, char d, short e, short f, short g, int h, int i, float j) {
	return a + b + c + d + e + f + g + h + i + (int)j;
}

int convtest_func(char c, short s, int i, float f, double d) {
	char sc = c + s;
	char ic = c + i;
	char fc = c + f;
	char dc = c + d;
	short is = s + i;
	short fs = s + f;
	short ds = s + d;
    //int ti = i + s;
	int fi = i + f;
	int di = i + d;
	float df = d + f;

	return bla(sc, ic, fc, dc, is, fs, ds, fi, di, df);
}

int main() {
	printf("%d\n", convtest_func('a', 42, 2444, 7.753f, 8.222));
	return 0;
}
