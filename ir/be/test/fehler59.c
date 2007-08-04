#include <alloca.h>

int main()
{
	char *b1 = alloca(13);
	int i;

	for(i = 0; i < 5; ++i) {
		char *bfs = alloca(24);
		printf("Offset: %d\n", bfs - b1);
	}

	return 0;
}
