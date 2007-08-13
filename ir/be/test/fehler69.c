typedef unsigned char  UINT8;
typedef   signed short  INT16;
typedef unsigned short UINT16;
typedef unsigned int   UINT32;

#define FROMRGB(r, g ,b)  ((UINT32) (((UINT8) (r) | ((UINT16) (g) << 8)) | (((UINT32) (UINT8) (b)) << 16)))
#define SGPGetRValue(rgb)   ((UINT8) (rgb))
#define SGPGetBValue(rgb)   ((UINT8) ((rgb) >> 16))
#define SGPGetGValue(rgb)   ((UINT8) (((UINT16) (rgb)) >> 8))
#define BLACK_SUBSTITUTE 0x0001


float guiShadePercent;
INT16 gusRedShift;
INT16 gusGreenShift;
INT16 gusBlueShift;
INT16 gusRedMask;
INT16 gusGreenMask;
INT16 gusBlueMask;


UINT16 Get16BPPColor(UINT32 RGBValue)
{
  UINT8 r = SGPGetRValue(RGBValue);
  UINT8 g = SGPGetGValue(RGBValue);
  UINT8 b = SGPGetBValue(RGBValue);

  UINT16 r16 = (gusRedShift   < 0 ? r >> -gusRedShift   : r << gusRedShift);
  UINT16 g16 = (gusGreenShift < 0 ? g >> -gusGreenShift : g << gusGreenShift);
  UINT16 b16 = (gusBlueShift  < 0 ? b >> -gusBlueShift  : b << gusBlueShift);

  UINT16 usColor = (r16 & gusRedMask) | (g16 & gusGreenMask) | (b16 & gusBlueMask);

  if (usColor == 0 && RGBValue != 0) usColor = BLACK_SUBSTITUTE;

  return usColor;
}


void BuildShadeTable(void)
{
	UINT16 red;
	UINT16 green;
	UINT16 blue;

  for (red = 0; red < 256; red += 4)
  {
    for (green = 0; green < 256; green += 4)
    {
      for (blue = 0; blue < 256; blue += 4)
      {
        Get16BPPColor(42);
        Get16BPPColor(FROMRGB(red * guiShadePercent, green * guiShadePercent, blue * guiShadePercent));
      }
    }
  }
}


int main(void)
{
	BuildShadeTable();
	return 0;
}
