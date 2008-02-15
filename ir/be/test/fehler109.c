int puts(const char *str);

void p(void)
{
	puts("p");
}

void q(void)
{
	puts("q");
}

void f(int x, long long y) {
	if (x) {
		if (y)
			p();
		if(!y)
			q();
	} else {
		if (y)
			q();
		if(!y)
			p();
	}
}

int main(void)
{
	f(40, 4);
	f(0, 1);
	f(0, 0);
	f(41, 0);
	return 0;
}
