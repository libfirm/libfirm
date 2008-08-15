/*$ -fcombo $*/

int test() {
	int i, j;

	for (i = 0, j = 0; i < 5; ++i, ++j);

	while (i == j) {
		/* endless */
	}
	return i;
}

int main() {
	return 0;
}
