/*$ -march=i686 $*/

double __attribute__((__cdecl__)) atof (const char *__nptr);

int a;
char *str;

void GIB_Range_f (void)
{
	double i, inc = 9, limit = 42;
	if (a == 4) {
		inc = atof(str);
		if(inc == 0) return;
	}
	for (i = atof (str); inc < 0 ? i >= limit : i <= limit; i += inc) {
	}
}

int main(void)
{
	return 0;
}
