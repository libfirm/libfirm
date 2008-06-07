#ifndef CHAR_BIT
#define CHAR_BIT 8
#endif

#define ROL(a,b) (((a) << (b)) | ((a) >> ((sizeof (a) * CHAR_BIT) - (b))))
#define ROL(a,b) (((a) << (b)) | ((a) >> ((sizeof (a) * CHAR_BIT) - (b))))

/* This is NOT folded into a ROL, but demonstrates an error in old lowering */
long long testLL(long long a) {
	return ROL(a,3);
}

int main() {
	int printf(const char *fmt, ...);

	printf("%lld\n", testLL(1));
	return 0;
}
