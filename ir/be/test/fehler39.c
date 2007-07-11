/* register allocator fails to resolve IMul Constraints correctly */

#ifdef __GNUC__
#define NO_INLINE __attribute__((noinline))
#else
#define NO_INLINE __declspec(noinline)
#endif

int LightSpriteCreate()
{
	return 42;
}

void LightSpritePosition(int x, int y, int z)
{
	(void) x;
	(void) y;
	(void) z;
}

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

	if (pExplosion->iLightID = LightSpriteCreate())
	{
		LightSpritePosition(pExplosion->iLightID, sX / 10, sY / 10);
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
