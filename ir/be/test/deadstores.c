int a;
static int b;
struct { int a, b; } c;
static struct { int a, b; } d;
static int e;
int *ptr = &e;

int main(void) {
	union { int a; char arr[4]; } u;

	a = 1;
	b = 0x42;
	c.a = 2;
	d.a = 0x42;
	e = 3;
	u.a = 0x42;

	return 0;
}
