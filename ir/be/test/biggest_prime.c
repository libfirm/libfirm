#include <stdio.h>

static int m = 754974721;
static int N;
static int t[1 << 22];
static int a;
static int *p;
static int i;
//static int e = 1 << 22;
static int e = 1 << 10;
static int j;
static int s;
static int b;
static int c;
static int U;

void f(int d)
{
	for (s = 1 << 23; s; s /= 2, d = d * 1L * d % m) {
		if (s >= N) continue;
		for (p = t; p < t + N; p += s) {
			for (i = s, c = 1; i; i--) {
				b = *p + p[s], p[s] = (m + *p - p[s]) *
					1L * c % m, *p++ = b % m, c = c * 1L * d % m;
			}
		}
	}

	for (j = 0; i < N - 1;)
	{
		for (s = N / 2; !((j ^= s) & s); s /= 2)
		{}

		if (++i < j)
			a = t[i], t[i] = t[j], t[j] = a;
	}
}

int main ()
{
	*t = 2;
	U = N = 1;

	while (e /= 2) {
		N *= 2;
		U = U * 1L * (m + 1) / 2 % m;
		f(362);
		for (p = t; p < t + N;)
			*p++ = (*p * 1L * *p % m) * U % m;

		f(415027540);
		for (a = 0, p = t; p < t + N;) {
			a += (0x6A64B1 & e ? 2 : 1) * *p;
			*p++ = a % 10;
			a /= 10;
		}
	}

	while (!*--p)
		;

	t[0]--;
	{
		int qs = 0;
		while (p >= t)
			qs += *p--;
		printf ("Checksumme = %d\n", qs);
	}
	return 0;
}
