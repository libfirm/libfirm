/* The compound initialisation code asserts when a wide char array in a struct
 * gets initialised with a wide string literal which is - not counting the \0 -
 * as long as the array */

#include <wchar.h>

struct s {
	wchar_t x[30];
	short y;
};


struct s x[][21] = {
	{ L"Exactly    30    chars    long", 1 }
};


int main(void)
{
	return 0;
}
