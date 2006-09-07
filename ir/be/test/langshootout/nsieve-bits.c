/*
 * The Great Computer Language Shootout
 * http://shootout.alioth.debian.org/
 *
 * Written by Dima Dorfman, 2004
 * Compile: gcc -std=c99 -O2 -o nsieve_bits_gcc nsieve_bits.c
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef uint_fast8_t bits;
#define NBITS   (CHAR_BIT * sizeof(bits))

static uintmax_t
nsieve(uintmax_t m)
{
        uintmax_t count, i, j;
        bits a[m / NBITS];

        memset(a, (1 << CHAR_BIT) - 1, sizeof(a));
        count = 0;
        for (i = 2; i < m; ++i)
                if (a[i / NBITS] & (1 << i % NBITS)) {
                        for (j = i + i; j < m; j += i)
                                a[j / NBITS] &= ~(1 << j % NBITS);
                        ++count;
                }
        return (count);
}

static void
test(unsigned long n)
{
        uintmax_t count, m;

        m = (1 << n) * 10000;
        count = nsieve(m);
        printf("Primes up to %8ju %8ju\n", m, count);
}

int
main(int ac, char **av)
{
        unsigned long n;
        char *cp;

        if (ac < 2) {
usage:          fprintf(stderr, "usage: nsieve N\n");
                exit(2);
        }
        n = strtoul(av[1], &cp, 10);
        if (*av[1] == '\0' || *cp != '\0' || n == ULONG_MAX)
                goto usage;
        test(n);
        if (n >= 1)
                test(n - 1);
        if (n >= 2)
                test(n - 2);
        exit(0);
}
