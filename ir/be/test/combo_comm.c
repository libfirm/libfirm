/*$ -fcombo -fno-inline $*/
int test(int a, int b) {
	int c, d;

	c = a + b;

	if (0)
		d = 0;
	else
		d = b;

	d = d + a;

	return c + d;
}

int main(int argc, char *argv[]) {
	return test(2,3) != 10;
}
