#define INT_TO_FLOAT(I)                ((2.0F * (I) + 1.0F) * (1.0F/4294967294.0F))

float A, B, C;

void f(float r, float g, float b) {
	A = r; B = g; C = b;
}

void bla(int r, int g, int b) {
	f(INT_TO_FLOAT(r), INT_TO_FLOAT(g), INT_TO_FLOAT(b));
}

int main(int argc, char *argv[]) {
	return 0;
}
