/* Wrong stabs */

#define MAX_FILENAME_LEN 48

typedef unsigned char BOOLEAN;
typedef unsigned int  UINT32;
typedef unsigned char UINT8;
typedef void*         HVOBJECT;


typedef struct
{
	char ubFilename[MAX_FILENAME_LEN];
	BOOLEAN   fLoaded;
	UINT32    uiIndex;
	UINT8     ubFlags;
	UINT8     ubNumberOfFrames;
	HVOBJECT  hVObject;
} CursorFileData;


CursorFileData blub;


int main(void)
{
	return 0;
}
