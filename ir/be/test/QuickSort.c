/*
 * Project:     GCC-firm
 * File name:   test/Quicksort.c
 * Purpose:     sorting with quicksort
 * Author:
 * Modified by: Michael Beck (for GCC-firm)
 * Created:     XX.11.2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001 Universitaet Karlsruhe
 * Licence:
 * URL:         http://www-info1.informatik.uni-wuerzburg.de/staff/wolf/teaching/pi1_ws98/java/QuickSort.java
 */
#include <stdio.h>
#include <stdlib.h>

#define COUNTACTIONS

#ifdef COUNTACTIONS
// Variablen, in denen die Bewegungen und Vergleiche gespeichert werden
static int bewegungen = 0;
static int vergleiche = 0;
#endif


//--------------------
// quicksort-Funktion
//--------------------
static void quicksort(int *fld, int l, int r ) {
	// Wenn der zu sortierende Teil eine Laenge <= 1 hat -> fertig
	if( l >= r )
		return;

	int pivot = fld[r];
	int i = l-1, j = r;
	int v;

	// mit dem Pivotelement sortieren
	while( 1 ) {
		while( fld[++i] < pivot ) {
#ifdef COUNTACTIONS
			vergleiche++;
#endif
		}
#ifdef COUNTACTIONS
		vergleiche++;
#endif

		while( j > l && fld[--j] > pivot ) {
#ifdef COUNTACTIONS
			vergleiche++;
#endif
		}

#ifdef COUNTACTIONS
		vergleiche++;
#endif
		// Wenn j <= i ist, wurde der zu sortierende Teil des Feldes
		// durchlaufen -> fertig
		if( j <= i )
			break;

		// Elemente tauschen
		v = fld[i];
		fld[i] = fld[j];
		fld[j] = v;
		// ein Tausch zweier Feldelemente wird als eine Bewegung gerechnet
#ifdef COUNTACTIONS
		bewegungen++;
#endif
	}

	// Pivotelement in die Mitte tauschen
	fld[r] = fld[i];
	fld[i] = pivot;

#ifdef COUNTACTIONS
	bewegungen++;
#endif

	// Die zwei Teilfolgen rekursiv mit quicksort sortieren
	quicksort( fld, l, i-1 );
	quicksort( fld, i+1, r );
}

int verify(int* fld, int count) {
    int i;
    int last = fld[0];
    for(i = 1; i < count; ++i) {
        if(fld[i] < last)
            return 0;
        last = fld[i];
    }

    return 1;
}

//------------------------------
// Hauptfunktion des Programmes
//------------------------------
int main(int argc, char *argv[]) {
  int i, *fld;
  int count, seed;

  printf("QuickSort.c\n");

  if(argc > 1)
    count = atoi(argv[1]);
  else
    count = 10000;

  if(argc > 2)
    seed = atoi(argv[2]);
  else
    seed = 123456;

  srand(seed);

  fld = (void*) malloc(sizeof(*fld) * count);
  for(i = 0; i < count; ++i)
      fld[i] = rand();

  printf("Sorting %d random numbers (seed %d)\n",
          count, seed);
  // Sortieren
  quicksort(fld, 0, count-1);

  // Ausgabe
#ifdef COUNTACTIONS
  printf("Sorted. (needed %d comparisons and %d moves.\n", vergleiche, bewegungen);
#else
  printf("Sorted.\n");
#endif

  if(verify(fld, count))
      printf("Verify succeeded.\n");
  else
      printf("Verify failed.\n");

  return 0;
}
