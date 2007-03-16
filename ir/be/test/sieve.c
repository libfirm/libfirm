/* Hey Emacs, this is a -*- C -*- file */
/*
 * Project:     STAC
 * File name:   test/stantard/sieve.c
 * Purpose:     test case:  Eratostenes sieve
 * Author:      Eratostenes
 * Modified by: Boris Boesler
 * Created:     17.7.2006
 * Copyright:   (c) 2006 Dresden Silicon GmbH
 * Licence:
 * CVS-ID:      $Id$
 */

/* switch to STA code generation */
#pragma codegenerator STA

/* Eratostenes Sieve Prime Number Program in C */
#define TRUE 1
#define FALSE 0
//#define SIZE	8190
#define SIZE	100
#define ITERATIONS	1

/* result as local variable to check */
/*
 * sieve(10) = 0x08
 * sieve(20) = 0x0d
 * sieve(100) = 0x2d
 */
short res;

/* the sieve */
short flags[SIZE + 1];

void task_start()
{
  register short i, prime, k, count, iter;
  count = 0;

  for (iter = 1; iter <= ITERATIONS; iter++) {
    count = 0;

    /* init solid sieve */
    for(i = 0; i <= SIZE; i++) {
      flags[i] = TRUE;
    }

    /* make holes in sieve */
    for(i = 0; i <= SIZE; i++) {
      if(flags[i]) {
	prime = i + i + 3;
	for (k = i + prime; k <= SIZE; k += prime) {
	  flags[k] = FALSE;
	}
	count++;
      }
    }
  }
  res = count ;
}

/* switch back to Control Flow Processor code generation */
#pragma codegenerator CFP


#ifdef REFERENCE_TESTS
int main(int argc, char **argv)
{
  task_start();
  printf("sieve(%d) = 0x%x\n", SIZE, res);
}
#endif
