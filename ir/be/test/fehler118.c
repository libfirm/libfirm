/*$ -fcombo $*/
#include <stdlib.h>

char *test(char *name) {
	char *p, *trunc = NULL;
	int plen;
	int min_part = 3;
	do {
	        p = name;
	        while (*p) {
	            	plen = 3;
	                p += plen;
	            	if (plen > min_part) trunc = p-1;
	        }
	} while (trunc == NULL && --min_part != 0);
	return p;
}

int main() {
	return 0;
}
