#include <string.h>


#define ANI_TOPMOST_LEVEL               7
#define CELL_X_SIZE                    10
#define CELL_Y_SIZE                    10
#define EXPLOSION_FLAG_USEABSPOS      0x00000001
#define EXPLOSION_FLAG_DISPLAYONLY    0x00000002
#define HIGHVOLUME                    127
#define MIN_AMB_LEVEL_FOR_MERC_LIGHTS   9
#define ROOF_LEVEL_HEIGHT              50
#define TRUE                            1
#define WALL_HEIGHT                    50


#define ANITILE_FORWARD											0x00000040
#define ANITILE_CACHEDTILE									0x00008000
#define ANITILE_NOZBLITTER									0x00040000
#define ANITILE_ALWAYS_TRANSLUCENT					0x00100000
#define ANITILE_EXPLOSION										0x20000000


typedef unsigned char  BOOLEAN;
typedef          char  CHAR8;
typedef   signed char   INT8;
typedef unsigned char  UINT8;
typedef   signed short  INT16;
typedef unsigned short UINT16;
typedef   signed int    INT32;
typedef unsigned int   UINT32;


typedef struct LEVELNODE LEVELNODE;


typedef struct TAG_anitile_params
{
	UINT32											uiFlags;							// flags struct
	UINT8												ubLevelID;						// Level ID for rendering layer
	INT16											  sStartFrame;					// Start frame
	INT16												sDelay;								// Delay time
	UINT16											usTileType;						// Tile databse type ( optional )
	UINT16											usTileIndex;					// Tile database index ( optional )
	INT16												sX;										// World X ( optional )
	INT16												sY;										// World Y ( optional )
	INT16												sZ;										// World Z ( optional )
	INT16												sGridNo;							// World GridNo

	LEVELNODE										*pGivenLevelNode;			// Levelnode for existing tile ( optional )
	const char* zCachedFile; // Filename for cached tile name ( optional )

	UINT8												ubOwner;							// UBID for the owner
	UINT8												ubKeyFrame1;					// Key frame 1
	UINT32											uiKeyFrame1Code;			// Key frame code
	UINT8												ubKeyFrame2;					// Key frame 2
	UINT32											uiKeyFrame2Code;			// Key frame code

	UINT32											uiUserData;
	UINT8												ubUserData2;
	UINT32											uiUserData3;

} ANITILE_PARAMS;


// Explosion Data
typedef struct
{
	UINT32											uiFlags;

	UINT8												ubOwner;
	UINT8												ubTypeID;

	UINT16											usItem;

	INT16												sX;										// World X ( optional )
	INT16												sY;										// World Y ( optional )
	INT16												sZ;										// World Z ( optional )
	INT16												sGridNo;							// World GridNo
	BOOLEAN											fLocate;
	INT8												bLevel;								// World level
	UINT8												ubUnsed[1];
} EXPLOSION_PARAMS;


typedef struct
{
	EXPLOSION_PARAMS		Params;
	BOOLEAN							fAllocated;
	INT16								sCurrentFrame;
	INT32								iID;
	INT32								iLightID;
	UINT8								ubUnsed[2];
} EXPLOSIONTYPE;


enum
{
	ANI_KEYFRAME_NO_CODE,
	ANI_KEYFRAME_BEGIN_TRANSLUCENCY,
	ANI_KEYFRAME_BEGIN_DAMAGE,
	ANI_KEYFRAME_CHAIN_WATER_EXPLOSION,
  ANI_KEYFRAME_DO_SOUND,
} KeyFrameEnums;


// SOUNDS ENUMERATION
enum SoundDefines
{
	MISS_1 = 0,
	MISS_2,
	MISS_3,
	MISS_4,
	MISS_5,
	MISS_6,
	MISS_7,
	MISS_8,
	MISS_G1,
	MISS_G2,
	MISS_KNIFE,
	FALL_1,
	FALL_2,
	FALL_TO_GROUND_1,
	FALL_TO_GROUND_2,
	FALL_TO_GROUND_3,
	HEAVY_FALL_1,
	BODY_SPLAT_1,
	GLASS_SHATTER1,
	GLASS_SHATTER2,
	DROPEN_1,
	DROPEN_2,
	DROPEN_3,
	DRCLOSE_1,
	DRCLOSE_2,
	UNLOCK_DOOR_1,
	KICKIN_DOOR,
	BREAK_LOCK,
	PICKING_LOCK,

	GARAGE_DOOR_OPEN,
	GARAGE_DOOR_CLOSE,
	ELEVATOR_DOOR_OPEN,
	ELEVATOR_DOOR_CLOSE,
	HITECH_DOOR_OPEN,
	HITECH_DOOR_CLOSE,
	CURTAINS_OPEN,
	CURTAINS_CLOSE,
	METAL_DOOR_OPEN,
	METAL_DOOR_CLOSE,

	WALK_LEFT_OUT,
	WALK_RIGHT_OUT,
	WALK_LEFT_OUT2,
	WALK_RIGHT_OUT2,
	WALK_LEFT_IN,
	WALK_RIGHT_IN,
	WALK_LEFT_IN2,
	WALK_RIGHT_IN2,
	WALK_LEFT_ROAD,
	WALK_RIGHT_ROAD,
	WALK_LEFT_ROAD2,
	WALK_RIGHT_ROAD2,
	CRAWL_1,
	CRAWL_2,
	CRAWL_3,
	CRAWL_4,
	TARG_REFINE_BEEP,
	ENDTURN_1,
	HEADCR_1,
	DOORCR_1,
	HEADSPLAT_1,
	BODY_EXPLODE_1,
	EXPLOSION_1,
	CROW_EXPLODE_1,
	SMALL_EXPLODE_1,
	HELI_1,
	BULLET_IMPACT_1,
	BULLET_IMPACT_2,
	BULLET_IMPACT_3,
	CREATURE_BATTLECRY_1,
	ENTER_WATER_1,
	ENTER_DEEP_WATER_1,

	COW_HIT_SND,
	COW_DIE_SND,

	// ROCKET GUN COMPUTER VOICE...
	RG_ID_IMPRINTED,
	RG_ID_INVALID,
	RG_TARGET_SELECTED,

	// CAVE COLLAPSE
	CAVE_COLLAPSE,

	// AIR RAID SOUNDS...
	S_RAID_WHISTLE,
	S_RAID_AMBIENT,
	S_RAID_DIVE,
	S_RAID_TB_DIVE,
	S_RAID_TB_BOMB,

	// VEHICLE SOUNDS
	S_VECH1_MOVE,
	S_VECH1_ON,
	S_VECH1_OFF,
	S_VECH1_INTO,

	S_DRYFIRE1,

	// IMPACT SOUNDS
	S_WOOD_IMPACT1,
	S_WOOD_IMPACT2,
	S_WOOD_IMPACT3,
	S_PORCELAIN_IMPACT1,
	S_RUBBER_IMPACT1,
	S_STONE_IMPACT1,
	S_WATER_IMPACT1,
	S_VEG_IMPACT1,
	S_METAL_IMPACT1,
	S_METAL_IMPACT2,
	S_METAL_IMPACT3,

	S_SLAP_IMPACT,

	// WEAPON RELOAD
	S_RELOAD_REVOLVER,
	S_RELOAD_PISTOL,
	S_RELOAD_SMG,
	S_RELOAD_RIFLE,
	S_RELOAD_SHOTGUN,
	S_RELOAD_LMG,

	// WEAPON LOCKNLOAD
	S_LNL_REVOLVER,
	S_LNL_PISTOL,
	S_LNL_SMG,
	S_LNL_RIFLE,
	S_LNL_SHOTGUN,
	S_LNL_LMG,

	//WEAPON SHOT SOUNDS
	S_SMALL_ROCKET_LAUNCHER,
	S_GLAUNCHER,
	S_UNDER_GLAUNCHER,
	S_ROCKET_LAUNCHER,
	S_MORTAR_SHOT,
	S_GLOCK17,
	S_GLOCK18,
	S_BERETTA92,
	S_BERETTA93,
	S_SWSPECIAL,
	S_BARRACUDA,
	S_DESERTEAGLE,
	S_M1911,
	S_MP5K,
	S_MAC10,
	S_THOMPSON,
	S_COMMANDO,
	S_MP53,
	S_AKSU74,
	S_P90,
	S_TYPE85,
	S_SKS,
	S_DRAGUNOV,
	S_M24,
	S_AUG,
	S_G41,
	S_RUGERMINI,
	S_C7,
	S_FAMAS,
	S_AK74,
	S_AKM,
	S_M14,
	S_FNFAL,
	S_G3A3,
	S_G11,
	S_M870,
	S_SPAS,
	S_CAWS,
	S_FNMINI,
	S_RPK74,
	S_21E,
	S_THROWKNIFE,
	S_TANK_CANNON,
	S_BURSTTYPE1,
  S_AUTOMAG,

	S_SILENCER_1,
	S_SILENCER_2,

	// SWOOSHES.
	SWOOSH_1,
	SWOOSH_2,
	SWOOSH_3,
	SWOOSH_4,
	SWOOSH_5,
	SWOOSH_6,

	// CREATURE SOUNDS....
	ACR_FALL_1,
	ACR_STEP_1,
	ACR_STEP_2,
	ACR_SWIPE,
	ACR_EATFLESH,
	ACR_CRIPPLED,
	ACR_DIE_PART1,
	ACR_DIE_PART2,
	ACR_LUNGE,
	ACR_SMELL_THREAT,
	ACR_SMEEL_PREY,
	ACR_SPIT,
	//BABY
	BCR_DYING,
	BCR_DRAGGING,
	BCR_SHRIEK,
	BCR_SPITTING,
	// LARVAE
	LCR_MOVEMENT,
	LCR_RUPTURE,
	// QUEEN
	LQ_SHRIEK,
	LQ_DYING,
	LQ_ENRAGED_ATTACK,
	LQ_RUPTURING,
	LQ_CRIPPLED,
	LQ_SMELLS_THREAT,
	LQ_WHIP_ATTACK,

	THROW_IMPACT_1,
	THROW_IMPACT_2,

	IDLE_SCRATCH,
	IDLE_ARMPIT,
	IDLE_BACKCRACK,

	AUTORESOLVE_FINISHFX,

	//Interface buttons, misc.
	EMAIL_ALERT,
	ENTERING_TEXT,
	REMOVING_TEXT,
	COMPUTER_BEEP2_IN,
	COMPUTER_BEEP2_OUT,
	COMPUTER_SWITCH1_IN,
	COMPUTER_SWITCH1_OUT,
	VSM_SWITCH1_IN,
	VSM_SWITCH1_OUT,
	VSM_SWITCH2_IN,
	VSM_SWITCH2_OUT,
	SM_SWITCH1_IN,
	SM_SWITCH1_OUT,
	SM_SWITCH2_IN,
	SM_SWITCH2_OUT,
	SM_SWITCH3_IN,
	SM_SWITCH3_OUT,
	BIG_SWITCH3_IN,
	BIG_SWITCH3_OUT,
	KLAXON_ALARM,
	BOXING_BELL,
	HELI_CRASH,
	ATTACH_TO_GUN,
	ATTACH_CERAMIC_PLATES,
	ATTACH_DETONATOR,

	GRAB_ROOF,
	LAND_ON_ROOF,

	UNSTEALTHY_OUTSIDE_1,
	UNSTEALTHY_OUTSIDE_2,
	UNSTEALTHY_INSIDE_1,

	OPEN_DEFAULT_OPENABLE,
	CLOSE_DEFAULT_OPENABLE,

	FIRE_ON_MERC,
	GLASS_CRACK,
	SPIT_RICOCHET,

	BLOODCAT_HIT_1,
	BLOODCAT_DIE_1,
	SLAP_1,

	ROBOT_BEEP,
	DOOR_ELECTRICITY,
	SWIM_1,
	SWIM_2,
	KEY_FAILURE,
	TARGET_OUT_OF_RANGE,
  OPEN_STATUE,
  USE_STATUE_REMOTE,
  USE_WIRE_CUTTERS,
  DRINK_CANTEEN_FEMALE,
  BLOODCAT_ATTACK,
  BLOODCAT_ROAR,
  ROBOT_GREETING,
  ROBOT_DEATH,
  GAS_EXPLODE_1,
  AIR_ESCAPING_1,

  OPEN_DRAWER,
  CLOSE_DRAWER,
  OPEN_LOCKER,
  CLOSE_LOCKER,
  OPEN_WOODEN_BOX,
  CLOSE_WOODEN_BOX,
  ROBOT_STOP,

	WATER_WALK1_IN,
	WATER_WALK1_OUT,
	WATER_WALK2_IN,
	WATER_WALK2_OUT,

  PRONE_UP_SOUND,
  PRONE_DOWN_SOUND,
  KNEEL_UP_SOUND,
  KNEEL_DOWN_SOUND,
  PICKING_SOMETHING_UP,

  COW_FALL,

  BLOODCAT_GROWL_1,
  BLOODCAT_GROWL_2,
  BLOODCAT_GROWL_3,
  BLOODCAT_GROWL_4,
  CREATURE_GAS_NOISE,
  CREATURE_FALL_PART_2,
  CREATURE_DISSOLVE_1,
  QUEEN_AMBIENT_NOISE,
  CREATURE_FALL,
  CROW_PECKING_AT_FLESH,
  CROW_FLYING_AWAY,
  SLAP_2,
  MORTAR_START,
  MORTAR_WHISTLE,
  MORTAR_LOAD,

  TURRET_MOVE,
  TURRET_STOP,
  COW_FALL_2,
  KNIFE_IMPACT,
  EXPLOSION_ALT_BLAST_1,
  EXPLOSION_BLAST_2,
  DRINK_CANTEEN_MALE,
  USE_X_RAY_MACHINE,
  CATCH_OBJECT,
  FENCE_OPEN,

	NUM_SAMPLES
};


// TERRAIN ID VALUES.
enum
{
	NO_TERRAIN,
	FLAT_GROUND,
	FLAT_FLOOR,
	PAVED_ROAD,
	DIRT_ROAD,
	LOW_GRASS,
	HIGH_GRASS,
	TRAIN_TRACKS,
	LOW_WATER,
	MED_WATER,
	DEEP_WATER,
	NUM_TERRAIN_TYPES
};


enum
{
	NO_BLAST,
	BLAST_1,
	BLAST_2,
	BLAST_3,
	STUN_BLAST,
	WATER_BLAST,
  TARGAS_EXP,
  SMOKE_EXP,
  MUSTARD_EXP,

	NUM_EXP_TYPES
};


CHAR8 sBlastSpeeds[] =
{
	0,
	80,
	80,
	80,
	20,
	80,
	80,
	80,
	80,
};


UINT8 ubDamageKeyFrame[NUM_EXP_TYPES] =
{
	0,
	3,
	5,
	5,
	5,
	18,
	18,
	18,
	18,
};


UINT32 uiExplosionSoundID[NUM_EXP_TYPES] =
{
	EXPLOSION_1,
	EXPLOSION_1,
	EXPLOSION_BLAST_2,  //LARGE
	EXPLOSION_BLAST_2,
	EXPLOSION_1,
	AIR_ESCAPING_1,
	AIR_ESCAPING_1,
	AIR_ESCAPING_1,
	AIR_ESCAPING_1,
};


UINT8 ubTransKeyFrame[NUM_EXP_TYPES] =
{
	0,
	17,
	28,
	24,
	1,
	1,
	1,
	1,
	1,
};


CHAR8	zBlastFilenames[][70] =
{
	"",
	"TILECACHE/zgrav_d.sti",
	"TILECACHE/zgrav_c.sti",
	"TILECACHE/zgrav_b.sti",
	"TILECACHE/shckwave.sti",
	"TILECACHE/wat_exp.sti",
	"TILECACHE/tear_exp.sti",
	"TILECACHE/tear_exp.sti",
	"TILECACHE/must_exp.sti",
};


UINT8 ubAmbientLightLevel;


static void GenerateExplosionFromExplosionPointer(EXPLOSIONTYPE* pExplosion)
{
	UINT32		uiFlags;
	UINT8			ubOwner;
	UINT8			ubTypeID;
	INT16			sX;
	INT16			sY;
	INT16			sZ;
	INT16			sGridNo;
	UINT16		usItem;
	UINT8			ubTerrainType;
	INT8			bLevel;
  UINT32    uiSoundID;

	ANITILE_PARAMS	AniParams;

	// Assign param values
	uiFlags				= pExplosion->Params.uiFlags;
	ubOwner				= pExplosion->Params.ubOwner;
	ubTypeID			= pExplosion->Params.ubTypeID;
	sX						= pExplosion->Params.sX;
	sY						= pExplosion->Params.sY;
	sZ						= pExplosion->Params.sZ;
	sGridNo				= pExplosion->Params.sGridNo;
	usItem				= pExplosion->Params.usItem;
	bLevel				= pExplosion->Params.bLevel;

  // If Z value given is 0 and bLevel > 0, make z heigher
  if ( sZ == 0 && bLevel > 0 )
  {
    sZ = ROOF_LEVEL_HEIGHT;
  }

	pExplosion->iLightID = -1;

	// OK, if we are over water.... use water explosion...
	ubTerrainType = GetTerrainType( sGridNo );

	// Setup explosion!
	memset( &AniParams, 0, sizeof( ANITILE_PARAMS ) );

	AniParams.sGridNo							= sGridNo;
	AniParams.ubLevelID						= ANI_TOPMOST_LEVEL;
	AniParams.sDelay							= sBlastSpeeds[ ubTypeID ];
	AniParams.sStartFrame					= pExplosion->sCurrentFrame;
	AniParams.uiFlags							= ANITILE_CACHEDTILE | ANITILE_FORWARD | ANITILE_EXPLOSION;

	if ( ubTerrainType == LOW_WATER || ubTerrainType == MED_WATER || ubTerrainType == DEEP_WATER )
	{
		// Change type to water explosion...
		ubTypeID = WATER_BLAST;
		AniParams.uiFlags						|= ANITILE_ALWAYS_TRANSLUCENT;
	}


	if ( sZ < WALL_HEIGHT )
	{
		AniParams.uiFlags |= ANITILE_NOZBLITTER;
	}

	if ( uiFlags & EXPLOSION_FLAG_USEABSPOS )
	{
		AniParams.sX									= sX;
		AniParams.sY									= sY;
		AniParams.sZ									= sZ;

		//AniParams.uiFlags							|= ANITILE_USEABSOLUTEPOS;
	}

	AniParams.ubKeyFrame1					= ubTransKeyFrame[ ubTypeID ];
	AniParams.uiKeyFrame1Code			= ANI_KEYFRAME_BEGIN_TRANSLUCENCY;

	if ( !( uiFlags & EXPLOSION_FLAG_DISPLAYONLY ) )
	{
		AniParams.ubKeyFrame2					= ubDamageKeyFrame[ ubTypeID ];
		AniParams.uiKeyFrame2Code			= ANI_KEYFRAME_BEGIN_DAMAGE;
	}
	AniParams.uiUserData					= usItem;
	AniParams.ubUserData2					= ubOwner;
	AniParams.uiUserData3					= pExplosion->iID;
	AniParams.zCachedFile = zBlastFilenames[ubTypeID];
	CreateAnimationTile( &AniParams );

	//  set light source....
	if ( pExplosion->iLightID == -1 )
	{
		// DO ONLY IF WE'RE AT A GOOD LEVEL
		if ( ubAmbientLightLevel >= MIN_AMB_LEVEL_FOR_MERC_LIGHTS )
		{
			if( ( pExplosion->iLightID = LightSpriteCreate("L-R04.LHT", 0 ) ) != (-1) )
			{
				LightSpritePower( pExplosion->iLightID, TRUE );

				LightSpritePosition( pExplosion->iLightID, (INT16)(sX/CELL_X_SIZE), (INT16)(sY/CELL_Y_SIZE) );
			}
		}
	}

  uiSoundID = uiExplosionSoundID[ ubTypeID ];

  if ( uiSoundID == EXPLOSION_1 )
  {
      // Randomize
     if ( Random( 2 ) == 0 )
     {
      uiSoundID = EXPLOSION_ALT_BLAST_1;
     }
  }

	PlayJA2Sample(uiSoundID, SoundVolume(HIGHVOLUME, sGridNo), 1, SoundDir(sGridNo));
}


void f(void)
{
	GenerateExplosionFromExplosionPointer(0);
}


int main(void)
{
	return 0;
}
