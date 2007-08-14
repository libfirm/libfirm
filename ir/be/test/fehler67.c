/*****************************************************************************
 * Program:  sad.c
 * Function: New implementation of the intel application note
 *			 AP-940: "Block matching in Motion estimation Algorithms
 *			 using Streaming SIMD Extensions 3"
 *           We changed:
 *			 -	We used local arrays instead of pointer arithmetic
 *				because of the limited capability of the memory disambiguator
 *			 -	Used if/else instead of abs function since we can't
 *				use function calls in specification
 *			 -	Unrolled the inner loop manually since our loop
 *				unroller does not work so well.
 *           Used as a test for the simd optimization.
 * TODO:	 -	Maybe use the "restrict" keyword to implement pointer
 *				arithmetic
 * Author:   Andreas Schoesser
 * Date:     2007-08-06
 *****************************************************************************/

#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#include <time.h>

unsigned int sad(int test_blockx, int test_blocky, int *best_block_x, int *best_block_y, int iterations);

main()
{
	int best_block_x, best_block_y;
	unsigned int min_diff;
	int			 iterations = 2;

	printf("PSADBW Example\n--------------\n\n");

	printf("Executing 'motion estimation' %d times...\n\n", iterations);
	min_diff = sad(0, 0, &best_block_x, &best_block_y, iterations);

	printf("MinDiff: %u\nBest X: %d\nBest Y: %d\n", min_diff, best_block_x, best_block_y);
}

unsigned int sad(int test_blockx, int test_blocky, int *best_block_x, int *best_block_y, int iterations)
{
	unsigned char b[256][256];

	unsigned char a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15;
	unsigned char b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15;

	int i, x, y, blocky;
	unsigned tmp_diff, min_diff = 0xFFFFFFFF; // MAX_UINT

	// Fill in some random values to compare
	for(x = 0; x < 256; x++)
		for(y = 0; y < 256; y++)
			b[y][x] = (unsigned char) rand() % 255;

	// Execute Block matching 100 times
	for(i = 0; i < iterations; i++)
	{
		// Iterate over whole frame, x,y=coords of current block
		for(x = 1; x < 256 - 16; x++)
			for(y = 0; y < 256 - 16; y++)
			{
				tmp_diff = 0;

				// Compare current Block with reference block
				for(blocky = 0; blocky < 16; blocky++)
				{
					// Vektor Loads
					a0 = b[blocky][0]; a1 = b[blocky][1]; a2 = b[blocky][2]; a3 = b[blocky][3]; a4 = b[blocky][4]; a5 = b[blocky][5]; a6 = b[blocky][6]; a7 = b[blocky][7]; a8 = b[blocky][8]; a9 = b[blocky][9]; a10 = b[blocky][10]; a11 = b[blocky][11]; a12 = b[blocky][12]; a13 = b[blocky][13]; a14 = b[blocky][14]; a15 = b[blocky][15];
					b0 = b[blocky + y][x + 0]; b1 = b[blocky + y][x + 1]; b2 = b[blocky + y][x + 2]; b3 = b[blocky + y][x + 3]; b4 = b[blocky + y][x + 4]; b5 = b[blocky + y][x + 5]; b6 = b[blocky + y][x + 6]; b7 = b[blocky + y][x + 7]; b8 = b[blocky + y][x + 8]; b9 = b[blocky + y][x + 9]; b10 = b[blocky + y][x + 10]; b11 = b[blocky + y][x + 11]; b12 = b[blocky + y][x + 12]; b13 = b[blocky + y][x + 13]; b14 = b[blocky + y][x + 14]; b15 = b[blocky + y][x + 15];

					// psadpw, would be nice if this could be done by loop unrolling
					tmp_diff += ((a0 > b0) ? (a0 - b0) : (b0 - a0))  +
						((a1 > b1) ? (a1 - b1) : (b1 - a1)) +
						((a2 > b2) ? (a2 - b2) : (b2 - a2)) +
						((a3 > b3) ? (a3 - b3) : (b3 - a3)) +
						((a4 > b4) ? (a4 - b4) : (b4 - a4)) +
						((a5 > b5) ? (a5 - b5) : (b5 - a5)) +
						((a6 > b6) ? (a6 - b6) : (b6 - a6)) +
						((a7 > b7) ? (a7 - b7) : (b7 - a7)) +
						((a8 > b8) ? (a8 - b8) : (b8 - a8)) +
						((a9 > b9) ? (a9 - b9) : (b9 - a9)) +
						((a10 > b10) ? (a10 - b10) : (b10 - a10)) +
						((a11 > b11) ? (a11 - b11) : (b11 - a11)) +
						((a12 > b12) ? (a12 - b12) : (b12 - a12)) +
						((a13 > b13) ? (a13 - b13) : (b13 - a13)) +
						((a14 > b14) ? (a14 - b14) : (b14 - a14)) +
						((a15 > b15) ? (a15 - b15) : (b15 - a15));
				}

			// Check if the current block is least different
			if(min_diff > tmp_diff)
			{
				min_diff = tmp_diff;
				*best_block_x = x;
				*best_block_y = y;
			}
		}
	}

	return(min_diff);
}
