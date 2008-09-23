struct {
	int x;
	int lets[];
} glob;

int x = 42;

int main(void) {
	int y = glob.lets[5] + glob.lets[x * 3];
	return y;
}
