#include <stdio.h>
#include <stdlib.h>

void merge (float *, int, int, int);

/* sort the (sub)array v from start to end */

void merge_sort (float *v, int start, int end) {
  int middle;   /* the middle of the subarray */

  /* no elements to sort */
  if (start == end) return;

  /* one element; already sorted! */
  if (start == end - 1) return;

  /* find the middle of the array, splitting it into two subarrays */
  middle = (start + end) / 2;

  /* sort the subarray from start..middle */
  merge_sort (v, start, middle);

  /* sort the subarray from middle..end */
  merge_sort (v, middle, end);

  /* merge the two sorted halves */
  merge (v, start, middle, end);
}

/* merge the subarray v[start..middle] with v[middle..end], placing the
 * result back into v.
 */
void merge (float *v, int start, int middle, int end) {
  int v1_n, v2_n, v1_index, v2_index, i;

  float *v1 = malloc((middle - start) * sizeof(*v1));
  float *v2 = malloc((end - middle) * sizeof(*v2));

  /* number of elements in first subarray */
  v1_n = middle - start;

  /* number of elements in second subarray */
  v2_n = end - middle;

  /* fill v1 and v2 with the elements of the first and second
   * subarrays, respectively
   */
  for (i=0; i<v1_n; i++) v1[i] = v[start + i];
  for (i=0; i<v2_n; i++) v2[i] = v[middle + i];

  /* v1_index and v2_index will index into v1 and v2, respectively... */
  v1_index = 0;
  v2_index = 0;

  /* ... as we pick elements from one or the other to place back
   * into v
   */
  for (i=0; (v1_index < v1_n) && (v2_index < v2_n); i++) {

    /* current v1 element less than current v2 element? */
    if (v1[v1_index] <= v2[v2_index])

      /* if so, this element belong as next in v */
      v[start + i] = v1[v1_index++];
    else
      /* otherwise, the element from v2 belongs there */
      v[start + i] = v2[v2_index++];
  }
  /* clean up; either v1 or v2 may have stuff left in it */

  for (; v1_index < v1_n; i++) v[start + i] = v1[v1_index++];
  for (; v2_index < v2_n; i++) v[start + i] = v2[v2_index++];

  free(v1); free(v2);
}

int main(int argc, char **argv) {
  float *f;
  int i, len;

  printf("MergeSort.c\n");

  len = argc - 1;
  f = malloc(len * sizeof(*f));

  for(i = 0; i < argc - 1; i++) {
    if (!sscanf(argv[i + 1], "%f", f + i)) { printf("Argument %d invalid, float expected instead of '%s'!\n", i, argv[i + 1]); return 1; }
    printf(" %f\n", f[i]);
  }

  if (len < 1) {
      printf("Usage: MergeSort <list of numbers>\n");
      printf("Continuing with default input.\n");
      f = (void *)malloc(sizeof(*f) * (len = 6));
      f[0] = 3.3; f[1] = 18.18; f[2] = 5.5; f[3] = 99.99; f[4] = 104.104; f[5] = 2.2;
  }

  // Ausgabe
  printf(" array = ");
  for(i = 0; i < len - 1; i++) {
    printf(" %f,", f[i]);
  }
  if (len > 0) {
    printf(" %f\n", f[len - 1]);
  }
  // Sortieren
  merge_sort(f, 0, len);

  // Ausgabe
  printf(" sorted array = ");
  for(i = 0; i < len - 1; i++) {
    printf(" %f,", f[i]);
  }
  if (len > 0) {
    printf(" %f\n", f[len - 1]);
  }

  return 0;
}
