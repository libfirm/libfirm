int A;

int test(int a) {
	if (a == 23)
		goto end;
	while (A != 0) {
		A = A * a;
	}
end:
	/* return should be padded by using ret $0 */
	return a;
}

int main(int argc, char *argv[]) {
	return 23 != test(23);
}
