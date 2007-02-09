#include <stdio.h>
#include <stdlib.h>


#define true	1

static int loop = 2;
static int quiet = 1;

static void nop(void) {}

static int me1(int num) {
    int i = 0;

    if (num == 1) {
	printf("Going into endless loop 1 \n .... \n ");
	while(7 > 2) {
            if(!quiet)
                printf("%d\n", i++);
	}
    } else {
	num ++;
    }
    return num;
}

static void me2(int num) {
    int i = 0;

    if (num != 2) {
	nop();
    } else {
	printf("Going into endless loop 2 \n .... \n ");
	while(true) {
            if(!quiet)
                printf("%d\n", i++);
	}
    }
}

static void me3(int num) {
    int i = 0;

    printf("Going into endless loop 3 \n .... \n ");
    while(true) {
        if(!quiet)
            printf("%d\n", i++);
    }
}

int main(int argc, char *argv[]) {
    printf("XXEndless.c\n");
    if (argc != 2) {
		printf("\nUsage: Endless n, where n determines the loop.\n");
		printf("Continuing with default input.\n");
		return 0;
    } else {
		loop = atoi(argv[1]);
        quiet = 0;
    }
    me1(loop);
    me2(loop);
    me3(loop);
}
