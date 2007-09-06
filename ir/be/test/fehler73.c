
float f(float a, float b) {
	return a*a + b*b;
}

int fi(int a, int b) {
	return a*a + b*b;
}

int main(void) {
	float a = 3;
	float b = 4;
	float c = 5;
	printf("%.30f %.30f %.30f %.30f %.30f\n", a, b, c, f(a, b), c*c);
	printf("%d\n", fi(3, 4));
	return 0;
}
