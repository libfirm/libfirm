/*$ -fconfirm -fcond-eval $*/

int test(int lnWrd, int *ptExp) {
	int idx = 0;

	while ( lnWrd > 0 ) {
        	if ( *ptExp ) {
			*ptExp = 0;
			lnWrd--;
	 	}
		idx++;
		ptExp++;
	}
	return idx;
}

int arr[] = { 1, 0, 0, 0, 2, 3, 4};

int main(int argc, char *argv[]) {
	int x = test(3, arr);
	printf("%d\n", x);
	return 0;
}
