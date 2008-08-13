/*$ -fno-inline $*/

int __attribute__((fastcall)) test(int a, int b, int c) {
	return a+b - c;
}

int main() {
	return test(4, -4, 0);
}
