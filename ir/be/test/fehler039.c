/* register allocator fails to resolve IMul Constraints correctly */
#include <stdlib.h>
#include <stdio.h>

typedef struct
{
	short sX;
	short sY;
	int iLightID;
} EXPLOSIONTYPE;

static void GenerateExplosionFromExplosionPointer(EXPLOSIONTYPE* pExplosion)
{
	short sX = pExplosion->sX;
	short sY = pExplosion->sY;

	if (pExplosion->iLightID = rand())
	{
		printf("Blup: %d %d %d\n", pExplosion->iLightID, sX / 10, sY / 10);
	}
}


void f(void)
{
	GenerateExplosionFromExplosionPointer(0);
}


int main(void)
{
	return 0;
}
