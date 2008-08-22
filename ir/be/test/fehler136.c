struct X
{
	int x;
};

extern const struct X bla;

struct X const* const blub[] = { &bla };

struct X const bla = { 23 };
