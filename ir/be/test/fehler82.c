/*$ -fno-inline -bra-chordal-co-algo=heur4 -blistsched-select=heur */

/* demonstrates copyheur4 violating register constraints of FucomFnstsw */
#include <stdio.h>

void f(float g, float g2) {
	if(g < 1.23) {
		if(g2 < 20)
			printf("good");
	} else {
		if(g2 < 20)
			printf("bad");
	}
}

int main(void)
{
	f(0.5, 10);
	return 0;
}
