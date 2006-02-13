void xyz (void);

int t = -1;

int main(void) {
	printf("%d\n", t);
	xyz();
	printf("%d\n", t);
}


void xyz (void) {
	t++;
}
