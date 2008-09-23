struct {
	int x;
	int lets[3008];
} glob;

int x = 42;

int main(void) {
	int y = glob.lets[5] + glob.lets[x * 3];
	return y;
}
