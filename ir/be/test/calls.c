#if 0

int test(int a, int b, int c, int d, int e, int f, int g, int h, int i)
{
  x(a);
  x(a, b);
  x(a, b, c);
  x(a, b, c, d);
  x(a, b, c, d, e);
  x(a, b, c, d, e, f);
  x(a, b, c, d, e, f, g);
  x(a, b, c, d, e, f, g, h);
  x(a, b, c, d, e, f, g, h, i);
}

#endif


int test(int a, int b, int c)
{
  int d, e;

  d = a/b;
//  e = b/c;

  return d;
}

int main()
{
	printf("Result: %d\n", test(1,2,3));
	return 0;
}
