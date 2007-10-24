typedef struct s {
	char a;
	char b;
} s;

s a[129];
s* b = a;

void f(unsigned char i)
{
	printf("%d %d (should be 23 42)\n", b[i].a, b[i].b);
}


int main(void)
{
	b[128].a = 23;
	b[128].b = 42;
	f(128);
}
