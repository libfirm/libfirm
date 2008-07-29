#include <stdlib.h>

char *test(char *name) {
	char *p, *trunc = NULL;
	int plen;
	int min_part = 3;
	do {
	        p = strrchr(name, '.');
	        p = p ? p+1 : name;
	        while (*p) {
	            	plen = strcspn(p, '.');
	                p += plen;
	            	if (plen > min_part) trunc = p-1;
                	if (*p) p++;
	        }
	} while (trunc == NULL && --min_part != 0);
	return p;
}

int main() {
	return 0;
}
