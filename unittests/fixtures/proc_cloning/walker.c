#include <stddef.h>

typedef struct node_t node_t;
struct node_t {
	node_t *left;
	node_t *right;
};

typedef void (*node_proc)(node_t *);

extern node_t *root;

void walkTree(node_t *tree, node_proc pre, node_proc post);
void walkTree(node_t *tree, node_proc pre, node_proc post)
{
	if (tree == NULL) return;
	if (pre != NULL) pre(tree);
	walkTree(tree->left, pre, post);
	walkTree(tree->right, pre, post);
	if (post != NULL) post(tree);
}

int main(void)
{
	walkTree(root, NULL, NULL);
}
