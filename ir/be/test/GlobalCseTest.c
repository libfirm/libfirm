//
// GCC-firm Project
//
// $Id$
//
// Testprogram to test GCC-firm : GlobalCseTest.c

int m(int a) {
    int b;
    if ( a == 1) {
	/* Global cse should move the Subtraction before or after this if. */
	if( a == 0) b = a - 2; else  b = a - 2;
    } else {
	b = 4;
    }
    return b;

}

int main (int argc, char *argv[]) {
    printf("GlobalCseTest.c\n");

    m(4);

    return 0;
}
