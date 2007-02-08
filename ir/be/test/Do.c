
/* $Id$ */
/* Program builds a simple tree and walks the tree.
   Tree nodes are separated: one part contains the tree information,
   the other contains the data of the node. */

#include <stdlib.h>
#include <stdio.h>

#define NULL	((void *)0)

typedef struct Node Node;

typedef struct Data {
	const char *name;
	const char *addr;
	int    account1;
	int    account2;
	int    account3;
	Node   *found;
} Data;

struct Node {
	Data mydata;
	int  mykey;
	Node *son1;
	Node *son2;
	Node *son3;
	Node *son4;
};

static int Node_count;

static Node *new_node(int depth) {
	Node *res;

	res = (void *)malloc(sizeof(*res));
	res->mykey = Node_count++;       /* @@@ Want Random number */

	if (depth > 1) {
		res->son1 = new_node(depth-1);
		res->son2 = new_node(depth-1);
		res->son3 = new_node(depth-1);
		res->son4 = new_node(depth-1);
	} else {
		res->son1 = NULL;
		res->son2 = NULL;
		res->son3 = NULL;
		res->son4 = NULL;
	}
	return res;
}

static int find_max(Node *n) {
	if (n->son1 == NULL) {
		return n->mykey;
	} else {
		int max = find_max(n->son1);
		int max2 = find_max(n->son2);
		if (max2 > max) max = max2;
		/*max2 = find_max(n->son3);
		  if (max2 > max) max = max2;
		  max2 = find_max(n->son4);
		  if (max2 > max) max = max2;*/
		return max;
	}
}



static Node *root;     /* root of the tree to search */

static void alloc (int depth) {
	root = new_node(depth);
}
static void search (void) {
	printf(" Max = %d\n", find_max(root));
}

int main(int argc, char *argv[]) {
	int depth;

	printf("Do.c:\n");
	if (argc <= 1) {
		printf("Usage: Do n\nGive search tree depth!\n");
		printf("10 is a good value, 12 too much.\n");
		printf("Continuing with default value 9.\n");
		depth = 9;
	} else {
		depth = atoi(argv[1]);
	}
	alloc(depth);
	search();

	return 0;
}
