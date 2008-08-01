int x;

int
main ()
{
	if (x) {
		struct s { int j; };
		struct s *b; b->j = 5;
	}

	return 0;
}
