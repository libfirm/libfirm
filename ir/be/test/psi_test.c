int func (int a, int b) {
	return a == b;
}

double func2(double a, double b) {
   return a != b ? a : b;
}

int func3(double a, double b) {
  return a != b;
}

int func4(double a, double b, int c, int d) {
  return a > b ? c : d;
}

double func5(double a, double b, int c, int d) {
  return c < d ? a : b;
}

int func6(double a, double b) {
	return a == 0.0 && b + 1.0 < 10.0;
}

int main()
{
    return 0;
}
