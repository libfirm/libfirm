int x = 0x5FFFFFFF;

int main(void)
{
    double a = x;
    float  b = a;
    double c = b;
    x = c;
    printf("%f %f %f %d\n", a, b, c, x);
    return 0;
}
