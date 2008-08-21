char glob = -1;

int main(void) {
	char x = 126;
	while (x > 0) {
		x++;
		printf("%d\n", x);
		if (x == -125) break;
	}
	return 0;
}
