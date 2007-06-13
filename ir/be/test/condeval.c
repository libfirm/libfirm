#if 1
int f(int x)
{
	if (x < 23) x = 23;
	if (x > 42) x = 42;
	return x;
}
#endif


#if 1
static __inline int g(int x)
{
	return x == 42;
}

void h(int x)
{
	if (g(x)) {
		puts("1");
	} else {
		puts("2");
	}
}
#endif


#if 1
int rand(void);

void i(void)
{
	int finish = 0;
	int x;
	int y;
	int z;

	for (x = 0; x < 10 && !finish; x++) {
		for (y = 0; y < 10 && !finish; y++) {
			for (z = 0; z < 10 && !finish; z++) {
				if (rand())
					finish = 1;
				//a();
			}
		}
	}
}
#endif


#if 1
int rand(void);

void j(void)
{
	int finish = 0;
	int x;
	int y;
	int z;

	for (x = 0; !finish; x++) {
		for (y = 0; !finish; y++) {
			for (z = 0; !finish; z++) {
				if (rand()) finish = 1;
			}
		}
	}
}
#endif


#if 1
static __inline int k(int x)
{
	if (x < 23) x = 23;
	if (x > 42) x = 42;
	return x;
}

int l(int x)
{
	return k(x) == 23;
}
#endif

int main() {
	return 0;
}
