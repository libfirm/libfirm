#include <alloca.h>
#include <assert.h>

int main()
{
	char *b1 = alloca(13);
	int i;
	int lastoffs = 0;

	for(i = 0; i < 5; ++i) {
		char *bfs    = alloca(24);
		int   offset = b1 - bfs;
		memset(bfs, 0, 24);
		assert(offset > lastoffs);
		lastoffs = offset;
	}

	return 0;
}
