/*$ -fgvn-pre -fno-gcse $*/
char arr[256];
char X;

int test(int x, int p) {
	int y = 0;
	if (x) {
		y = 1;
	}
	do {
		++y;
		X = arr[y];
	} while (p);
}

int main() {
	return 0;
}
