/*$  -fgvn-pre -fno-gcse $*/

int something(int n);

int test() {
	int i;
	char inUse16[16];

	i = 0;
   	do {
    	if (something(1) == 1)
        	inUse16[i] = 1;
        else
        	inUse16[i] = 0;
        ++i;
    } while (i < 16);

    return inUse16[0];
}
