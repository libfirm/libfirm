/*$ -fgvn-pre -fno-gcse $*/
int bl_count[256];

int test(int max_length, int overflow) {
    int bits;

    do {
            bits = max_length-1;
            while (bl_count[bits] == 0) bits--;
            bl_count[bits]--;      /* move one leaf down the tree */
            bl_count[bits+1] += 2; /* move one overflow item as its brother */
    } while (overflow > 0);
    return 0;
}

int main() {
    return 0;
}
