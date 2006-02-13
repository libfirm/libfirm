#include <stdio.h>
#include <stdlib.h>


static int id(int i) {
    return(i);
}

int main(int argc, char*argv[]) {
    int i, s;
    int count;
    int size;

    printf("Thilo.c\n");

    s = 0;
    if(argc > 1)
      size = atoi(argv[1]);
    else
      size = 1000;
    for(count = 0; count < 10000; count++) {
	s = 0;
	for(i = 1; i <= size; i++) {
	    s += id(i);
	}
    }
    printf(" %d\n", s);

    return 0;
}
