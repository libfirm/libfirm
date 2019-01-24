#include "pdeq.h"

char arr[5000];

static void test_basic_rr(void)
{
	deq_t deq;
	deq_init(&deq);
	assert(deq_empty(&deq));

	deq_push_pointer_right(&deq, arr);
	assert(!deq_empty(&deq));
	char *v = deq_pop_pointer_right(char, &deq);
	assert(v == arr);
	assert(deq_empty(&deq));

	deq_free(&deq);
}

static void test_basic_lr(void)
{
	deq_t deq;
	deq_init(&deq);
	assert(deq_empty(&deq));

	deq_push_pointer_left(&deq, arr);
	assert(!deq_empty(&deq));
	char *v = deq_pop_pointer_right(char, &deq);
	assert(v == arr);
	assert(deq_empty(&deq));

	deq_free(&deq);
}

static void test_basic_ll(void)
{
	deq_t deq;
	deq_init(&deq);
	assert(deq_empty(&deq));

	deq_push_pointer_left(&deq, arr);
	assert(!deq_empty(&deq));
	char *v = deq_pop_pointer_left(char, &deq);
	assert(v == arr);
	assert(deq_empty(&deq));

	deq_free(&deq);
}

static void test_basic_rl(void)
{
	deq_t deq;
	deq_init(&deq);
	assert(deq_empty(&deq));

	deq_push_pointer_right(&deq, arr);
	assert(!deq_empty(&deq));
	char *v = deq_pop_pointer_left(char, &deq);
	assert(v == arr);
	assert(deq_empty(&deq));

	deq_free(&deq);
}

static void push_left(deq_t *deq, unsigned n, char *refp)
{
	for (unsigned i = 0; i < n; ++i) {
		deq_push_pointer_left(deq, refp+n-i-1);
		assert(!deq_empty(deq));
	}
}

static void push_right(deq_t *deq, unsigned n, char *refp)
{
	for (unsigned i = 0; i < n; ++i) {
		deq_push_pointer_right(deq, refp+i);
		assert(!deq_empty(deq));
	}
}

static void pop_left(deq_t *deq, unsigned n, const char *refp)
{
	for (unsigned i = 0; i < n; ++i) {
		assert(!deq_empty(deq));
		char *p = deq_pop_pointer_left(char, deq);
		assert(refp == NULL || p == refp + i);
	}
}

static void pop_right(deq_t *deq, unsigned n, const char *refp)
{
	for (unsigned i = 0; i < n; ++i) {
		assert(!deq_empty(deq));
		char *p = deq_pop_pointer_right(char, deq);
		assert(refp == NULL || p == refp + n - i - 1);
	}
}

static void verify(deq_t const *const deq, unsigned const expected_n,
				   char const *const min_ptr, char const *const max_ptr) {
	unsigned n = 0;
	char const *last = NULL;
	deq_foreach_pointer(deq, char, p) {
		assert(p >= last);
		assert(min_ptr <= p && p <= max_ptr);
		last = p;
		++n;
	}
	assert(n == expected_n);
}

static void test_many(void)
{
	deq_t deq;
	deq_init(&deq);
	assert(deq_empty(&deq));

	push_right(&deq, 5000, arr);
	verify(&deq, 5000, arr, arr+5000);
	pop_left(&deq, 5000, arr);
	assert(deq_empty(&deq));

	push_left(&deq, 5000, arr);
	verify(&deq, 5000, arr, arr+5000);
	pop_left(&deq, 5000, arr);
	assert(deq_empty(&deq));

	push_right(&deq, 5000, arr);
	verify(&deq, 5000, arr, arr+5000);
	pop_right(&deq, 5000, arr);
	assert(deq_empty(&deq));

	push_left(&deq, 5000, arr);
	verify(&deq, 5000, arr, arr+5000);
	pop_right(&deq, 5000, arr);
	assert(deq_empty(&deq));

	deq_free(&deq);
}

static void test_mixed(void)
{
	deq_t deq;
	deq_init(&deq);

	for (unsigned a = 0; a < 1000; ++a) {
		push_right(&deq, 1000-a, arr);
		pop_right(&deq, a+1, NULL);
	}
	assert(deq_empty(&deq));

	for (unsigned a = 0; a < 1000; ++a) {
		push_right(&deq, 1000-a, arr);
		pop_left(&deq, a+1, NULL);
	}
	assert(deq_empty(&deq));

	for (unsigned a = 0; a < 1000; ++a) {
		push_left(&deq, 1000-a, arr);
		pop_right(&deq, a+1, NULL);
	}
	assert(deq_empty(&deq));

	for (unsigned a = 0; a < 1000; ++a) {
		push_left(&deq, 1000-a, arr);
		pop_left(&deq, a+1, NULL);
	}
	assert(deq_empty(&deq));

	deq_free(&deq);
}

typedef void (*push_func)(deq_t *deq, unsigned n, char *refp);
typedef void (*pop_func)(deq_t *deq, unsigned n, char *refp);
static void test_mixed_4(push_func push0, pop_func pop0,
                         push_func push1, pop_func pop1)
{
	deq_t deq;
	deq_init(&deq);

	for (unsigned a = 0; a < 1000; ++a) {
		push0(&deq, 1000-a, arr);
		pop0(&deq, a+1, NULL);
		push1(&deq, 1000-a, arr);
		pop1(&deq, a+1, NULL);
	}
	assert(deq_empty(&deq));

	deq_free(&deq);
}

int main(void)
{
	test_basic_ll();
	test_basic_lr();
	test_basic_rl();
	test_basic_rr();

	test_many();
	test_mixed();

	test_mixed_4(push_left,  pop_left,  push_left,  pop_left);
	test_mixed_4(push_left,  pop_left,  push_left,  pop_right);
	test_mixed_4(push_left,  pop_left,  push_right, pop_left);
	test_mixed_4(push_left,  pop_left,  push_right, pop_left);
	test_mixed_4(push_left,  pop_right, push_left,  pop_left);
	test_mixed_4(push_left,  pop_right, push_left,  pop_right);
	test_mixed_4(push_left,  pop_right, push_right, pop_left);
	test_mixed_4(push_left,  pop_right, push_right, pop_left);
	test_mixed_4(push_right, pop_left,  push_left,  pop_left);
	test_mixed_4(push_right, pop_left,  push_left,  pop_right);
	test_mixed_4(push_right, pop_left,  push_right, pop_left);
	test_mixed_4(push_right, pop_left,  push_right, pop_left);
	test_mixed_4(push_right, pop_right, push_left,  pop_left);
	test_mixed_4(push_right, pop_right, push_left,  pop_right);
	test_mixed_4(push_right, pop_right, push_right, pop_left);
	test_mixed_4(push_right, pop_right, push_right, pop_left);

	return 0;
}
