typedef   signed short  INT16;
typedef   signed int    INT32;


typedef struct
{
	INT16 sX;
	INT16 sY;
	INT32 iLightID;
} EXPLOSIONTYPE;


static void GenerateExplosionFromExplosionPointer(EXPLOSIONTYPE* pExplosion)
{
	INT16 sX = pExplosion->sX;
	INT16 sY = pExplosion->sY;

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
