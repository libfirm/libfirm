#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

typedef struct linked {
	struct linked *next;
} linked;

linked* t(linked *val) {
	printf("start\n");

	if(val == NULL)
		return NULL;

	void *res = t(val->next);
	if(res)
		return res;
	return val;
}

int main()
{
	linked a, b, c;
	a.next = &b;
	b.next = &c;
	c.next = NULL;
	t(&a);
	return 0;
}
