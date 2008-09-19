static const int arr[] = { 1, 2, 3 };

static int func1(void) {
	return arr[1];
}

static const struct X { int a, b[2]; } data[] = { {1,{2, 3}}, {4,{5, 6}} };

static int func2(void) {
	return *data[1].b;
}

static const int darr[][2] = { {1,2}, {2,3} };

static int func3(void) {
	return darr[1][1];
}

int main(int argc, char *argv[]) {
	return func1() + func2() + func3() != 10;
}
