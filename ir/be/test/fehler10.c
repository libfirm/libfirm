void a(void);
void b(void);

int main(void) {
	a();
	printf("\n");
	return 0;
}

void a(void) {
	goto a;

a:
	printf("x");
	b();
	return;

b:
	printf("y");
	b();
	return;
}

void b(void) {
	goto b;

a:
	printf("k");
	return;

b:
	printf("l");
	return;
}
