// assert_local_important_args("incr", 1);

typedef struct Integer {
	int val;
} Integer;

void incr(Integer *n);
void incr(Integer *n)
{
	n->val++;
}
