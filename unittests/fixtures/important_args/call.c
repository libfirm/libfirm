// FIRM_IMPORTANT_ARGS: 1

typedef void (*proc)(void);

void f(proc g);

void f(proc g)
{
	g();
}
