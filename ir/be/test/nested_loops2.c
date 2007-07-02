#include <stdio.h>

int k = 0;

static void
send_via_write() {
  int r2 = k;
  if ( r2 <= 2147483647 )
  {
  }
  else
  {
    for (;;)
    {
      for (;;)
      {
        r2 = printf( "%d\n", r2 );
//	if ( r2 < 0 )
	{
	  continue;
	}
	if ( r2 != 235 )
	  return;
      }
    }
  }
}

int main()
{
  send_via_write();
  return 0;
}
