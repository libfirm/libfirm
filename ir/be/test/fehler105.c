/* frontend crashs on missing return in compound returning function */
struct A {
	int a, b;
};

struct A func(void)
{
}

int main(void)
{
	return 0;
}
