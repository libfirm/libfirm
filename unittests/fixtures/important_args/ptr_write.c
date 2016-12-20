// assert_local_important_args("f", 1);

void f(int *p, int x);

void f(int *p, int x)
{
	*p = x;
}
