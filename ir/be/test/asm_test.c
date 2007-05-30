double test(double angle) {
	double result;
	asm ("fsinx %1,%0" : "=f" (result) : "f" (angle));
	return result;
}

int main(int argc, char *argv[]) {
	printf("%f\n", test(0.5));
}
