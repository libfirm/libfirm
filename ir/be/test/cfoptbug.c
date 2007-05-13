#include <stdio.h>
#include <assert.h>

struct list {
	struct list *next;
	int val;
};

struct list l = { NULL, 5 };
struct list *sptr = &l;
struct list l2 = { NULL, 42 };
struct list *sptr2 = &l2;

struct list *test(int n)
{
	struct list *ptr1;
	struct list *ptr2;
	int sum = 0;

	if(n == 0) {
		ptr1 = sptr;
	} else {
		ptr1 = sptr2;
	}

	for(ptr2 = ptr1; ptr2 != NULL; ptr2 = ptr2->next) {
		sum += ptr2->val;
	}
	printf("Sum: %d\n", sum);

	return ptr1;
}

int main(int argc, char **argv)
{
	struct list *ptr1 = test(0);
	struct list *ptr2 = test(1);

	assert(ptr1 == sptr);
	assert(ptr2 == sptr2);

	return 0;
}
