#if 1
int f(int x)
{
	if (x < 23) x = 23;
	if (x > 42) x = 42;
	return x;
}
#endif


#if 1
static int g(int x)
{
	return x == 42;
}

extern void y(void);
extern void z(void);

void h(int x)
{
	if (g(x)) {
		y();
	} else {
		z();
	}
}
#endif


#if 1
int a(void);

void i(void)
{
	int finish = 0;
	int x;
	int y;
	int z;

	for (x = 0; x < 10 && !finish; x++) {
		for (y = 0; y < 10 && !finish; y++) {
			for (z = 0; z < 10 && !finish; z++) {
				if (a()) finish = 1;
			}
		}
	}
}
#endif


#if 1
int a(void);

void j(void)
{
	int finish = 0;
	int x;
	int y;
	int z;

	for (x = 0; !finish; x++) {
		for (y = 0; !finish; y++) {
			for (z = 0; !finish; z++) {
				if (a()) finish = 1;
			}
		}
	}
}
#endif
