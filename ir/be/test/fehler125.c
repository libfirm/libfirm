asm(
	"bla:\n"
	".long 0"
);
extern int bla;
int main()
{
	return bla;
}
