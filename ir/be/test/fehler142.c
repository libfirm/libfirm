#include <assert.h>

void foo(float zNear, float zFar)
{
	float v;
	float depth = zFar - zNear;

	v = -2 * zFar * zNear / depth;
	assert(v < 0);
}

int main(void) {
	foo(4, 2048);
	return 0;
}
