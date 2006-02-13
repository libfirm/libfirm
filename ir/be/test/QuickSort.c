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

#include <stdlib.h>

// Variablen, in denen die Bewegungen und Vergleiche gespeichert werden
static int bewegungen;
static int vergleiche;


//--------------------
// quicksort-Funktion
//--------------------
static void quicksort(int *fld, int l, int r ) {
  // Wenn der zu sortierende Teil eine Laenge <= 1 hat -> fertig
  if( l < r ) {
    int pivot = fld[r];
    int i = l-1, j = r;
    int v;

    // mit dem Pivotelement sortieren
    while( 1 ) {
      while( fld[++i] < pivot )
	vergleiche++;
      vergleiche++;

      while( j > l && fld[--j] > pivot )
	vergleiche++;
      vergleiche++;
      // Wenn j <= i ist, wurde der zu sortierende Teil des Feldes
      // durchlaufen -> fertig
      if( j <= i )
	break;

      // Elemente tauschen
      v = fld[i];
      fld[i] = fld[j];
      fld[j] = v;
      // ein Tausch zweier Feldelemente wird als eine Bewegung gerechnet
      bewegungen++;
    }

    // Pivotelement in die Mitte tauschen
    fld[r] = fld[i];
    fld[i] = pivot;

    bewegungen++;

    // Die zwei Teilfolgen rekursiv mit quicksort sortieren
    quicksort( fld, l, i-1 );
    quicksort( fld, i+1, r );
  }
}

//------------------------------
// Hauptfunktion des Programmes
//------------------------------
int main(int argc, char *argv[]) {
  int i, *fld;
  int len = argc - 1;

  printf("QuickSort.c\n");

  if (len <= 0) {
      printf("Usage: QuickSort <list of numbers>\n");
      printf("Continuing with default input.\n");
      // fld = new int[] {3, 18, 5, 99, 104, 2};
      fld = (void *)malloc(sizeof(*fld) * (len = 6));
      fld[0] = 3; fld[1] = 18; fld[2] = 5; fld[3] = 99; fld[4] = 104; fld[5] = 2;
  }
  else {
    // Speicher fuer fld erzeugen
    fld = (void *)malloc(sizeof(*fld) * len);

    // Feldelemente belegen
    for( i = 0; i < len; i++ )
      fld[i] = atoi(argv[i + 1]);
  }

  printf("array:  ");
  for(i = 0; i < len; i++) {
    printf("%d, ", fld[i]);
  }

  // Sortieren
  bewegungen = 0;
  vergleiche = 0;
  quicksort( fld, 0, len - 1 );

  // Ausgabe
  printf( "\nSorted: " );

  for( i = 0; i < len; i++ ) {
    printf("%d, ", fld[i]);
  }

  printf("\nNeeded %d comparisons and %d moves.\n", vergleiche, bewegungen);

  return 0;
}
