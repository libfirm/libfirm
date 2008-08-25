#ifndef CHAR_BIT
#define CHAR_BIT 8
#endif

#define ROL(a,b) (((a) << (b)) | ((a) >> ((sizeof (a) * CHAR_BIT) - (b))))
#define ROR(a,b) (((a) >> (b)) | ((a) << ((sizeof (a) * CHAR_BIT) - (b))))

unsigned long long testL(unsigned long long a, int cnt) {
	return cnt == 0 ? a : ROL(a, cnt);
}

unsigned long long testR(unsigned long long a, int cnt) {
	return cnt == 0 ? a : ROR(a, cnt);
}

int main() {
	int printf(const char *fmt, ...);
	int i;

	for (i = 0; i < 64; ++i) {
		printf("%lld %lld\n", testL(1, i), testR(1, i));
	}
	return 0;
}
