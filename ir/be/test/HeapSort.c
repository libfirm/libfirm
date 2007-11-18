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
/** Größe des Heaps */
static int N;

/**
 * Tauscht die Elemente a[i] und a[j].
 */
static void exchange(int *a, int i, int j) {
  	int v;
	v = a[i];
	a[i] = a[j];
	a[j] = v;
}

/**
 * Läßt a[k] im Feld aufsteigen.
 */
static void upheap(int *a, int k) {
	while ((k > 0) && (a[k] < a[k / 2])) {
		exchange(a, k, k / 2);
		k = k / 2;
	}
}

/**
 * Fügt das neue Element v in den Heap der Größe N
 * ein und erhöht N um 1
 */
static void insert(int *a, int v) {
	a[N] = v;
	upheap(a, N);
	N++;
}

/**
* Läßt a[k] im Feld versickern.
*/
static void downheap(int *a, int k) {
	int j = 2 * k;
	if (j < N) {    // a[k] hat linken Sohn a[j]
		if (j + 1 < N)   // a[k] hat auch rechten Sohn a[j+1]
			if (a[j] > a[j + 1])
				j++;        // Jetzt ist a[j] der kleinere Sohn von a[k]
		if (a[k] > a[j]) {
			exchange(a, k, j);
			downheap(a, j);
		}
	}
}

/**
 * Liefert als Resultat das Heap-Element a[k], entfernt a[k]
 * aus dem Heap und stellt die Heap-Eigenschaft wieder her.
 */
static int removeh(int *a, int k) {
	int v = a[k];
	a[k] = a[--N];
	if ((k > 0) && (a[k] < a[k / 2]))
		upheap(a, k);
	else
		downheap(a, k);

	return v;
}

/**
 * Aufbau des Heaps durch downheap.
 */
static void heapaufbau1(int *a) {
	int k;

	for (k = N / 2; k >= 1; k--)
		downheap(a, k);
}

/**
 * Aufbau des Heaps durch insert.
 */
static void heapaufbau2(int *a, int *b, int len) {
	int k;
	N = 0;
	for (k = 0; k < len; ++k)
		insert(a, b[k]);
}

/**
 * Sortiert das gegebene Feld b mit den oben angegebenen
 * Heap-Methoden und gibt das sortierte Feld zurück.
 */
static int *heapsort_(int *b, int len) {
	int k;
	int *c;
	int *a;

	// Globale Variablen für die Heap-Methoden setzen
	a = malloc(sizeof(a[0]) * len);
	heapaufbau2(a, b, len);

	// Ergebnis in c kopieren
	c = malloc(sizeof(c[0]) * len);
	for (k = 0; k < len; k++)
		c[k] = removeh(a, 0);

	return c;
}

static int verify(int* fld, int count) {
    int i;
    int last = fld[0];
    for(i = 1; i < count; ++i) {
        if(fld[i] < last)
            return 0;
        last = fld[i];
    }

    return 1;
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
	int i, count, seed;

	printf("Heap.c\n");

	if(argc > 1)
		count = atoi(argv[1]);
	else
		count = 10000;

	if(argc > 2)
		seed = atoi(argv[2]);
	else
		seed = 123456;

	srand(seed);

	b = (void*) malloc(sizeof(b[0]) * count);
	for(i = 0; i < count; ++i)
		b[i] = rand();

	printf("Sorting %d random numbers (seed %d)\n",
          count, seed);

	// Sortieren
	b = heapsort_(b, count);

	printf("Sorted.\n");

	if(verify(b, count))
		printf("Verify succeeded.\n");
	else
		printf("Verify failed.\n");

	return 0;
}
