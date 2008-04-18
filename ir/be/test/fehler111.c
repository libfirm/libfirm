int x = 0;

int main(int argc, char *argv[]) {
	int y;
	char *p = &x;

	*p = 23;
	y = x;
	x = 35;
	return y;
}
