// // assert_local_important_args("f", 1);

typedef void (*proc)(int x);

void f(proc g, int x);

void f(proc g, int x)
{
	g(x);
}
