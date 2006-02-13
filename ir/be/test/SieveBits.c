//
// GCC-firm Project
//
// $Id$
//

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

typedef int boolean;

typedef struct BitSet
{
    int *P;
    int K;
} BitSet;

static void ctorBitSet(BitSet *pThis, int k)
{
    pThis->P = (void *)malloc(sizeof(*(pThis->P)) * (k / 32 + 1));
    pThis->K = k;
}

static void set (BitSet *pThis, int i)
{
    if (i < 0)
	return;
    pThis->P[i >> 5] |= (1 << (i & 0x0000001F));
}
static void clear (BitSet *pThis, int i)
{
    pThis->P[i >> 5] &= ~(1 << (i & 0x0000001F));
}

static boolean get (BitSet *pThis, int i)
{
    return (pThis->P[i >> 5] & (1 << (i & 0x0000001F))) != 0;
}

static int size (BitSet *pThis)
{
    return pThis->K;
}


/* Sieve.c
Das Primzahlsieb fuer C.
*/

static BitSet prime;
// Enthaelt Flags fuer die ungeraden Zahlen:
// 3,5,7,...
// D.h., 2*i+3 ist prim, wenn prime[i] war ist

static void sieve (void) {
// das Sieb
    int k = size(&prime), i, j, p, l;
    // Zuerst alle Zahlen auf prim setzen
    for (i = 0; i < k; i++)
	set(&prime, i);
    // Dann Vielfache von Primzahlen aussieben
    for (i = 0; i < k; i++) {
	if (get(&prime, i)) {
	    // 2*i+3 ist prim
	    p = 2 * i + 3;
	    l = (p * p - 3) / 2;
	    if (l > k)
		break;
	    for (j = l; j < k; j += p)
		clear(&prime, j); // streicht p*p,p*(p+2), etc.
	}
    }
}

int main (int argc, char *argv[]) {
    // Hauptprogramm
    int n, i, k, count;

    if (argc <= 1)
	n = 10000000;
    // falls keine Argumente in der Kommandozeile
    else
	n = atoi(argv[1]);
    // lies Anzahl aus der Kommandozeile
    k = (n - 3) / 2;
    printf("Counting primes up to %d.\n", (2 * k + 3));

    ctorBitSet(&prime, k);
    sieve(); // das Sieb

    // Zaehle gefundene Primzahlen:
    count = 1;
    for (i = 0; i < k; i++)
    	if (get(&prime, i))
		count++;
    // Ausgabe:
    printf("%d primes found.\n", count);

    // list(); // nur fuer Testzwecke
    return 0;
}
