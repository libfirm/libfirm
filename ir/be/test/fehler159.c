/*$ -fcond-eval -fgvn-pre -fno-gcse $*/

int something(int n);

int test() {
	int i;
	char inUse16[16];

   	for (i = 0; i < 16; i++)
    	if (something(1) == 1)
        	inUse16[i] = 1;
        else
        	inUse16[i] = 0;

    return inUse16[0];
}
