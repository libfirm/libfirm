#include <stdio.h>
#include <assert.h>

unsigned int p0 = 0;
unsigned int p1 = 42;

int simpler(void)
{
	if (p0 != p1)
		if (p0  == 0)
			return -1;
		else
			return 1;
	return 0;
}

int compare_string(char *CompValue, char         *ValuePtr)
{
    int              i           = 0;
    unsigned char   *p0          = (unsigned char *)CompValue;
    unsigned char   *p1          = (unsigned char *)ValuePtr;

    for (i = 0; i == 0 && *p0 != '\0' && *p1 != '\0'; p0++, p1++)
    {
        if (*p0 != *p1)
            if (*p0  < *p1)
                i      = -1;
            else
                i      = 1;
    }

    if (i == 0)
        if (*p0  != *p1)
            if (*p0 == '\0')
                i      = -1;
            else
                i      = 1;

    return(i);
}

int main()
{
#define test(a,b,shouldbe)   { int res = compare_string(a, b); printf("Compare %s, %s -> %d (should be %d)\n", a, b, res, shouldbe); assert(res == shouldbe); }
	test("a", "b", -1);
	test("", "", 0);
	test("Rothe", "Rother", -1);
	test("hallo", "hallo", 0);
	test("hallo", "welt", -1);
	test("welt", "hallo", 1);

	printf("Simpler: %d\n", simpler());
	return 0;
}
