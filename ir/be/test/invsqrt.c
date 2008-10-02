/**
 * magical invsqrt function from Quake III code
 * see: http://www.codemaestro.com/reviews/9
 */

float InvSqrt(float x)
{
	float xhalf = 0.5f*x;
	int i = *(int*)&x;
	i = 0x5f3759df - (i>>1);
	x = *(float*)&i;
	x = x*(1.5f-xhalf*x*x);
	return x;
}

int main(void) {
	int result = InvSqrt(0.00056);
	printf("Result: %d (should be 42)", result);
	return result != 42;
}
