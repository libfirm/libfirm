#include <assert.h>

#define SIZE 268435456
char too_long_for_firm[SIZE];

int main(int argc, char **argv)
{
	too_long_for_firm[SIZE-1] = '\0';
	return 0;
}
