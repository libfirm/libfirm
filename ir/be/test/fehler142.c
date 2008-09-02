float foo(float zNear, float zFar)
{
	float depth = zFar - zNear;

	return -2 * zFar * zNear / depth;
}

int main(void) {
	return foo(4, 2048) >= 0;
}
