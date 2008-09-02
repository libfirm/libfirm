int x;

void f(int a, int b, char* p)
{
	int y = x;
	do {
		*p++ = 0;
	} while (++a != b);
	rand();
	x = y;
}
