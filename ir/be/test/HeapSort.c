/*
 * Project:     GCC-firm
 * File name:   test/HeapSort.java
 * Purpose:     sorting with heapsort
 * Author:
 * Modified by: Michael Beck (for GCC-firm)
 * Created:     XX.11.2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universitaet Karlsruhe
 * Licence:
 * URL:         http://www-info1.informatik.uni-wuerzburg.de/staff/wolf/teaching/pi1_ws98/java/Heapsort.java
 * Bugs:        Fails for input 5,3,4,7,99
 */

#include <stdlib.h>
#include <stdio.h>

/**
 * Heapsort-Algorithmus laut Vorlesung.
 */
/** Array mit Heap */
int *a;
/** Größe des Heaps */
int N;

/**
 * Tauscht die Elemente a[i] und a[j].
 */
void exchange(int i,int j) {
  int v;
  v = a[i];
  a[i] = a[j];
  a[j] = v;
}

/**
 * Läßt a[k] im Feld aufsteigen.
 */
void upheap(int k) {
  while ((k > 1) && (a[k] < a[k / 2])) {
    exchange(k, k / 2);
    k = k / 2;
  }
}

/**
 * Fügt das neue Element v in den Heap der Größe N
 * ein und erhöht N um 1
 */
void insert(int v) {
  N++;
  a[N] = v;
  upheap(N);
}

/**
* Läßt a[k] im Feld versickern.
*/
void downheap(int k) {
  int j = 2 * k;
  if (j < N) {    // a[k] hat linken Sohn a[j]
    if (j + 1 < N)   // a[k] hat auch rechten Sohn a[j+1]
      if (a[j] > a[j + 1])
	j++;        // Jetzt ist a[j] der kleinere Sohn von a[k]
    if (a[k] > a[j]) {
      exchange(k, j);
      downheap(j);
    }
  }
}

/**
 * Liefert als Resultat das Heap-Element a[k], entfernt a[k]
 * aus dem Heap und stellt die Heap-Eigenschaft wieder her.
 */
int removeh(int k) {
  int v = a[k];
  a[k] = a[N];
  N--;
  if ((k > 1) && (a[k] < a[k / 2]))
    upheap(k);
  else
    downheap(k);
  return v;
}

/**
 * Aufbau des Heaps durch downheap.
 */
void heapaufbau1(void) {
  int k;

  for (k = N / 2; k >= 1; k--)
    downheap(k);
}

/**
 * Aufbau des Heaps durch insert.
 */
void heapaufbau2(void) {
  int k, m = N; N = 0;
  for (k = 1;k <= m; k++)
    insert(a[k]);
}

/**
 * Sortiert das gegebene Feld b mit den oben angegebenen
 * Heap-Methoden und gibt das sortierte Feld zurück.
 */
int *heapsort(int *b, int len) {
  int i, k;
  int *c;

  // Globale Variablen für die Heap-Methoden setzen
  a = (void *)malloc(sizeof(*a) * (len + 1));
  N = len;

  // Die Heap-Methoden arbeiten auf dem Bereich a[1],...,a[N],
  // normale Java-Array belegen aber b[0],...,b[b.length-1].
  for (i = 0; i < len; i++) {
    a[i + 1] = b[i];
  }
  heapaufbau2();
  // Ergebnis in c kopieren
  c = (void *)malloc(sizeof(*c) * len);
  for (k = 0; k < len; k++)
    c[k] = removeh(1);
  return c;
}

/**
 * Ein einfaches Beispielprogramm. Alle Argumente des Programms müssen
 * Zahlen sein.
 * Bsp:<pre>
 * java Heapsort 6 13 17 42 9 3 5
 * array={6,13,17,42,9,3,5}
 * sorted array={3,5,6,9,13,17,42}
 * </pre>
 */
int main (int argc, char *argv[]) {
  // Umwandeln der Argumente in Zahlen
  int *b;
  int i, len;

  printf("Heap.c\n");

  len = argc - 1;
  b = (void *)malloc(len * sizeof(*b));

  for(i = 0; i < argc-1; i++) {
    b[i] = atoi(argv[i+1]);
    printf(" %d\n", b[i]);
  }

  if (len < 1) {
      printf("Usage: HeapSort <list of numbers>\n");
      printf("Continuing with default input.\n");
      // b = new int[] {3, 18, 5, 99, 104, 2};
      b = (void *)malloc(sizeof(*b) * (len = 6));
      b[0] = 3; b[1] = 18; b[2] = 5; b[3] = 99; b[4] = 104; b[5] = 2;
  }

  // Ausgabe
  //System.out.print("array={");
  // printf("length of array to sort: %d -- first element b[0]: %d\n", len, b[0]);
  printf(" array = ");
  for(i = 0; i < len - 1; i++) {
    printf(" %d,", b[i]);
  }
  if (len > 0) {
    printf(" %d\n", b[len - 1]);
  }
  // Sortieren
  b = heapsort(b, len);

  // Ausgabe
  printf(" sorted array = ");
  for(i = 0; i < len - 1; i++) {
    printf(" %d,", b[i]);
  }
  if (len > 0) {
    printf(" %d\n", b[len - 1]);
  }
  return 0;
}
