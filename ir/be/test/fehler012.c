void xyz (void);

int t = -1;

int main(void) {
	printf("%d\n", t);
	xyz();
	printf("%d\n", t);

	return 0;
}


void xyz (void) {
	t++;
}
