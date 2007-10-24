#include <assert.h>
#include <stdio.h>

#define SF_SMOKE_EFFECTS_TEMP_FILE_EXISTS       0x00100000    //Temp File starts with sm_
#define SF_LIGHTING_EFFECTS_TEMP_FILE_EXISTS    0x00200000    //Temp File starts with l_

#define SF_REVEALED_STATUS_TEMP_FILE_EXISTS     0x01000000    //Temp File starts with v_
#define SF_DOOR_STATUS_TEMP_FILE_EXISTS         0x02000000    //Temp File starts with ds_
#define SF_ENEMY_PRESERVED_TEMP_FILE_EXISTS     0x04000000    //Temp File starts with e_
#define SF_CIV_PRESERVED_TEMP_FILE_EXISTS       0x08000000    //Temp File starts with c_
#define SF_ITEM_TEMP_FILE_EXISTS                0x10000000    //Temp File starts with i_
#define SF_ROTTING_CORPSE_TEMP_FILE_EXISTS      0x20000000    //Temp File starts with r_
#define SF_MAP_MODIFICATIONS_TEMP_FILE_EXISTS   0x40000000    //Temp File starts with m_
#define SF_DOOR_TABLE_TEMP_FILES_EXISTS         0x80000000    //Temp File starts with d_

#define MAPS_DIR "maps"

void f(unsigned int uiType)
{
	//Convert the current sector location into a file name
	const char* zTempName = "blub";
	char pMapName[512];

	switch (uiType)
	{
		case SF_ITEM_TEMP_FILE_EXISTS:
			sprintf( pMapName, "%s/i_%s", MAPS_DIR, zTempName);
			break;

		case SF_ROTTING_CORPSE_TEMP_FILE_EXISTS:
			sprintf( pMapName, "%s/r_%s", MAPS_DIR, zTempName);
			break;

		case SF_MAP_MODIFICATIONS_TEMP_FILE_EXISTS:
			sprintf( pMapName, "%s/m_%s", MAPS_DIR, zTempName);
			break;

		case SF_DOOR_TABLE_TEMP_FILES_EXISTS:
			sprintf( pMapName, "%s/d_%s", MAPS_DIR, zTempName);
			break;

		case SF_REVEALED_STATUS_TEMP_FILE_EXISTS:
			sprintf( pMapName, "%s/v_%s", MAPS_DIR, zTempName);
			break;

		case SF_DOOR_STATUS_TEMP_FILE_EXISTS:
			sprintf( pMapName, "%s/ds_%s", MAPS_DIR, zTempName);
			break;

		case SF_ENEMY_PRESERVED_TEMP_FILE_EXISTS:
			sprintf( pMapName, "%s/e_%s", MAPS_DIR, zTempName);
			break;

		case SF_CIV_PRESERVED_TEMP_FILE_EXISTS:
			sprintf( pMapName, "%s/cc_%s", MAPS_DIR, zTempName);
			break;

		case SF_SMOKE_EFFECTS_TEMP_FILE_EXISTS:
			sprintf( pMapName, "%s/sm_%s", MAPS_DIR, zTempName);
			break;

		case SF_LIGHTING_EFFECTS_TEMP_FILE_EXISTS:
			sprintf( pMapName, "%s/l_%s", MAPS_DIR, zTempName);
			break;

		default:
			assert(0);
			break;
	}
}

int main()
{
	return 0;
}
