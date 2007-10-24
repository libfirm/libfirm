/*$ -fno-inline $*/
#include <stdio.h>

struct decompostition {
	int a, b;
};

static struct decompostition do_something(void)
{
	struct decompostition c;
	rand();
	return c;
}

int main(void) {
	int arr[5];
	struct decompostition dc;

	arr[0] = 123;
	arr[1] = 245;
	dc = do_something();
	printf("%d %d\n", arr[0], arr[1]);

	return 0;
}
