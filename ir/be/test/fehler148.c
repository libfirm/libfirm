#define NE 6

/* Number of 16 bit words in internal format */
#define NI (NE+3)

/* Array offset to exponent */
#define E 1

/* Array offset to high guard word */
#define M 2


static void
eshdn1 (x)
	register unsigned short *x;
{
	register unsigned short bits;
	int i;

	x += M;                       /* point to significand area */

	bits = 0;
	for (i = M; i < NI; i++)
	{
		if (*x & 1)
			bits |= 1;
		*x >>= 1;
		if (bits & 2)
			*x |= 0x8000;
		bits <<= 1;
		++x;
	}

}

static int
edivm (den, num)
	     unsigned short den[], num[];
{
	eshdn1 (num);
}

int main(int argc, char *argv[]) {
	unsigned short den[NI], num[NI];

	edivm(den, num);
	return 0;
}
