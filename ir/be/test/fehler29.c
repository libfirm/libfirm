/* Crash in ia32 floating point code emitter */

int f(float x)
{
	return x != -1;
}


int main(void)
{
	return 0;
}
