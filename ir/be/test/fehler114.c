unsigned x = 0xAABBCCDD;
unsigned y = 15;
unsigned z = 12;

int main(void) {
    unsigned t = x;
    unsigned t2 = y;
    unsigned l = z;
    printf("%X\n", (t << t2) | (t >> (32 - l)));
    return 0;
}
