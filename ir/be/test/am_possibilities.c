/*
 B  - base
 IN - index
 IS - index (shifted)
 C  - const
 SC - symconst

    #   |   B   |   I   |  IS   |   C   |  SC   |             note
 -------+-------+-------+-------+-------+-------+-------------------------------
     00 |       |       |       |       |       | senseless
     01 |       |       |       |       |   x   |
     02 |       |       |       |   x   |       | senseless ?
     03 |       |       |       |   x   |   x   |
     04 |       |       |   x   |       |       | missing base ?
     05 |       |       |   x   |       |   x   | missing base ?
     06 |       |       |   x   |   x   |       | missing base ?
     07 |       |       |   x   |   x   |   x   | missing base ?
     08 |       |   x   |       |       |       | missing base
     09 |       |   x   |       |       |   x   | missing base
     10 |       |   x   |       |   x   |       | missing base
     11 |       |   x   |       |   x   |   x   | missing base
     12 |       |   x   |   x   |       |       | impossible
     13 |       |   x   |   x   |       |   x   | impossible
     14 |       |   x   |   x   |   x   |       | impossible
     15 |       |   x   |   x   |   x   |   x   | impossible
     16 |   x   |       |       |       |       |
     17 |   x   |       |       |       |   x   |
     18 |   x   |       |       |   x   |       |
     19 |   x   |       |       |   x   |   x   |
     20 |   x   |       |   x   |       |       |
     21 |   x   |       |   x   |       |   x   |
     22 |   x   |       |   x   |   x   |       |
     23 |   x   |       |   x   |   x   |   x   |
     24 |   x   |   x   |       |       |       |
     25 |   x   |   x   |       |       |   x   |
     26 |   x   |   x   |       |   x   |       |
     27 |   x   |   x   |       |   x   |   x   |
     28 |   x   |   x   |   x   |       |       | impossible
     29 |   x   |   x   |   x   |       |   x   | impossible
     30 |   x   |   x   |   x   |   x   |       | impossible
     31 |   x   |   x   |   x   |   x   |   x   | impossible
 */

struct s {
	char m[100];
} a;

char c;

int main(int argc, char **argv)
{
	return 0;
}

void load_01(void)
{
	c = a.m[0];
}

void load_03(void)
{
	c = a.m[1];
}

void load_16(char* base)
{
	c = base[0];
}

void load_17(int base)
{
	c = a.m[base];
}

void load_18(char* base)
{
	c = base[1];
}

void load_19(int base)
{
	c = a.m[base + 1];
}

void load_20_add(char* base, int index)
{
	c = base[2 * index];
}

void load_20_shift(char* base, int index)
{
	c = base[4 * index];
}

void load_21_add(int index)
{
	c = a.m[2 * index];
}

void load_21_shift(int index)
{
	c = a.m[4 * index];
}

void load_22_add(char* base, int index)
{
	c = base[2 * index + 1];
}

void load_22_shift(char* base, int index)
{
	c = base[4 * index + 1];
}

void load_23_add(int index)
{
	c = a.m[2 * index + 1];
}

void load_23_shift(int index)
{
	c = a.m[4 * index + 1];
}

void load_24(char* base, int index)
{
	c = base[index];
}

void load_25(int base, int index)
{
	c = a.m[base + index];
}

void load_26(char* base, int index)
{
	c = base[index + 1];
}

void load_27(int base, int index)
{
	c = a.m[base + index + 1];
}
