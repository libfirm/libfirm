/* different interpretation of strict conv */

float C1 = 15;
float C2 = 1;
float C3 = 0.0099999997764825821;
//double C4 = 0.12999999523162842;

int main()
{
	float C4 = 0.12999999523162842;
	C4 += C1 * C2 * C3;
	printf("%.30f\n", C4);
	return 0;
}
