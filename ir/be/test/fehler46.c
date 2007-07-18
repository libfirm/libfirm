/* tarval rounding wrong with fp-strict */

int main()
{
        float cost = 74.739288330078125;
        double square = cost * cost;
        //C2 = square;
        printf("cost: %.20f\n", cost);
        printf("square: %.20f\n", square);
        return 0;
}
