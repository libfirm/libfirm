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

char sc[100];
char* sc_pointers[100];
char c;

int main(int argc, char **argv)
{
	return 0;
}

/* Source address modes */

void load_01(void)
{
	c = sc[0];
}

void load_03(void)
{
	c = sc[1];
}

void load_16(char* base)
{
	c = base[0];
}

void load_17(int base)
{
	c = sc[base];
}

void load_18(char* base)
{
	c = base[1];
}

void load_19(int base)
{
	c = sc[base + 1];
}

void load_20_add(char* base, int index)
{
	c = base[2 * index];
}

void load_20_shift(char* base, int index)
{
	c = base[4 * index];
}

void load_21_add(int base, int index)
{
	c = sc[base + 2 * index];
}

void load_21_shift(int base, int index)
{
	c = sc[base + 4 * index];
}

void load_22_add(char* base, int index)
{
	c = base[2 * index + 1];
}

void load_22_shift(char* base, int index)
{
	c = base[4 * index + 1];
}

void load_23_add(int base, int index)
{
	c = sc[base + 2 * index + 1];
}

void load_23_shift(int base, int index)
{
	c = sc[base + 4 * index + 1];
}

void load_24(char* base, int index)
{
	c = base[index];
}

void load_25(int base, int index)
{
	c = sc[base + index];
}

void load_26(char* base, int index)
{
	c = base[index + 1];
}

void load_27(int base, int index)
{
	c = sc[base + index + 1];
}

/* Destination address modes */

void store_immediate_01(void)
{
	sc_pointers[0] = sc + 42;
}

void store_immediate_03(void)
{
	sc_pointers[1] = sc + 42;
}

void store_immediate_16(char** base)
{
	base[0] = sc + 42;
}

void store_immediate_17(int base)
{
	sc_pointers[base] = sc + 42;
}

void store_immediate_18(char** base)
{
	base[1] = sc + 42;
}

void store_immediate_19(int base)
{
	sc_pointers[base + 1] = sc + 42;
}

void store_immediate_20_add(char** base, int index)
{
	base[2 * index] = sc + 42;
}

void store_immediate_20_shift(char** base, int index)
{
	base[4 * index] = sc + 42;
}

void store_immediate_21_add(int base, int index)
{
	sc_pointers[base + 2 * index] = sc + 42;
}

void store_immediate_21_shift(int base, int index)
{
	sc_pointers[base + 4 * index] = sc + 42;
}

void store_immediate_22_add(char** base, int index)
{
	base[2 * index + 1] = sc + 42;
}

void store_immediate_22_shift(char** base, int index)
{
	base[4 * index + 1] = sc + 42;
}

void store_immediate_23_add(int base, int index)
{
	sc_pointers[base + 2 * index + 1] = sc + 42;
}

void store_immediate_23_shift(int base, int index)
{
	sc_pointers[base + 4 * index + 1] = sc + 42;
}

void store_immediate_24(char** base, int index)
{
	base[index] = sc + 42;
}

void store_immediate_25(int base, int index)
{
	sc_pointers[base + index] = sc + 42;
}

void store_immediate_26(char** base, int index)
{
	base[index + 1] = sc + 42;
}

void store_immediate_27(int base, int index)
{
	sc_pointers[base + index + 1] = sc + 42;
}
