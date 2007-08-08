int randn = -1271796327;
double value = 4294967295;

int main(void) {
	float res = (double) (randn % (unsigned int) value);
	printf("Res: %f\n", res);
	return 0;
}
