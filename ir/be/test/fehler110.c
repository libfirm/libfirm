struct A {
	int a, b, c;
};

struct A globa = { 1, 2, 3 };

struct A funk(void) {
	struct A res;

	res.a = globa.c + 20;
	res.b = globa.b + 20;
	res.c = globa.a + 20;

	return res;
}

int main(void) {
	globa = funk();
	printf("%d %d %d\n", globa.a, globa.b, globa.c);
	return 0;
}
