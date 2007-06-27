/* The tarval module fails to negate the constant and returns TV_OVERFLOW_BAD
 * when optimising the comparison (-x < 0  ->  x > 0). It is not expected that
 * the negation fails and an assertion while constructing the negated constant
 * is triggered */

int f(double x)
{
	return -x < 0;
}


int main(void)
{
	return 0;
}
