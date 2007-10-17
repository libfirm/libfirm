#include <stdio.h>

unsigned MediumSize = 4096;
unsigned ObjSize = 136;
unsigned RgnEntrys = 30;
unsigned AllocEntrys = 26000;
//unsigned RgnEntrys = 30;
//unsigned AllocRgns = 866;

int main(void)
{
	unsigned RgnEntrys = MediumSize / ObjSize;
	unsigned AllocRgns = AllocEntrys / RgnEntrys;

	printf("Red. %u %u\n", RgnEntrys, AllocRgns);
	return 0;
}
